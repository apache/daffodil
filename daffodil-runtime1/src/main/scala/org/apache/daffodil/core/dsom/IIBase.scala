/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.dsom

import java.net.URI
import java.net.URISyntaxException
import scala.collection.immutable.ListMap
import scala.xml.Node

import org.apache.daffodil.core.dsom.IIUtils._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.DaffodilSchemaSource
import org.apache.daffodil.lib.iapi.URISchemaSource
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.util.Delay
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.XMLUtils

/**
 * This file along with DFDLSchemaFile are the implementation of import and include
 * for daffodil.
 *
 * There are mixins for the namespace and import/include aspects of the
 * schema component classes. There are also the classes associated
 * with the include and import components themselves.
 *
 * From the perspective of DSOM, there are 3 new kinds of SchemaComponents.
 *
 * A DFDLSchemaFile represents the naming/identifcation of a file, but not its
 * contents, which are represented by SchemaDocument. We need a file object
 * to represent the reference to a file which may or may not exist, but also
 * to encapsulate the mechanism by means of which URIs for namespaces are
 * resolved to find those files, and the loading of those files to get SchemaDocuments.
 *
 * There are of course other ways that SchemaDocuments come into existence
 * which do not necessarily involve DFDLSchemaFile objects. (E.g., test rigs, passed
 * by API, etc.)
 *
 * The Include and Import objects represent those elements of a DFDL Schema. Much
 * functionality is shared on an abstract base called IIBase. These essentially
 * manage the namespace inheritance and checking issues, the identification of the
 * appropriate namespace, and most importantly duplicate removal and circular reference
 * detection.
 *
 * Circular references are NOT an error. Two schema documents can mutually import
 * each other just fine. They simply must be detected to avoid an obvious
 * infinte loop/stack overflow kind of situation.
 *
 * Each included DFDLSchemaFile is identified by a "map pair", which is a tuple of
 * (NS, URL), and is used as the key to a Map which is used to keep track of
 * which we've seen. Because an import can include the same file into multiple
 * namespaces one needs both pieces to tell if you have already included that file
 * into that namespace.
 *
 */

/**
 * Maps an optional namespace and optional schemaLocation to an Include or Import object.
 *
 * As we include/import schemas, we append to one of these, and before we
 * include/import we check to see if it is already here.
 *
 * About use of Delay[T]:
 *
 * This is fairly deep function programming stuff, but it let's us have
 * our cake and eat it too for one thing. In processing of import
 * statements like this <xs:include schemaLocation="..."/>, the chicken/egg
 * problem arises about namespaces. We have to read the file just in order
 * to know the namespace in order to be able to decide if we have seen
 * this (NS, URL) pair before, and therefore don't need to load the
 * file....
 *
 * So we maintain this growing map of (NS, URL) => file called an IIMap.
 *
 * We use delay on this, because it lets us construct the DFDLSchemaFile,
 * construct the XMLSchemaDocument object, both of which require that
 * we pass in the IIMap. Then we can ask the XMLSchemaDocument for the
 * targetNamespace of the file, which will cause the file to be read.
 * But none of this needs the IIMap argument yet.
 *
 * We then look at this new (tns, url) pair, and see if it is already
 * in the map. If not, we extend the IIMap,... and by the magic of
 * Delayed evaluation, that map is the one being passed to the
 * DFDLSchemaFile and XMLSchemaDocument above.
 *
 * Seems cyclical, but it isn't. We can call the constructors, passing
 * them a promise (aka Delayed IIMap) to deliver the IIMap when it is
 * needed. Turns out it isn't needed for the constructed object to
 * answer the question "what is the targetNamespace". But that target
 * namespace information IS needed to determine the IIMap which will
 * be supplied when demanded.
 *
 * From an ObjectOriented programing perspective, we don't pass an IIMap,
 * we pass an IIMap factory (a delayed IIMap is effectively that). That
 * factory isn't being called yet, and by the way it has pointers back
 * to data structures that will be filled in later, so it can't be called
 * yet. You wouldn't write an OO program this way usually.
 *
 * Note that we must use a map that maintains insertion order, of which
 * ListMap is one of them.
 */
object IIUtils {
  type IIMap = Delay[ListMap[(NS, DaffodilSchemaSource), IIBase]]
  private val empty = new ListMap[(NS, DaffodilSchemaSource), IIBase]()
  val emptyIIMap = Delay(Symbol("IIMapEmpty"), this, empty).force
}

/**
 * Include/Import = "II" for short
 */
abstract class IIBase(
  final override val xml: Node,
  xsdArg: XMLSchemaDocument,
  val seenBefore: IIMap
) extends SchemaComponent
  with NestingLexicalMixin {
  final override def optLexicalParent = Option(xsdArg)

  /**
   * An import/include requires only that we can access the
   * schema file, recursively any of its includes/imports,
   * and that all the resulting are validated by the validating loader.
   *
   * It is important to point out that this intentionally references
   * iiSchemaFileMaybe rather than iiSchemaFile because the former only loads a
   * schema if it hasn't been seen before. If we instead required the
   * evaluation of iiSchemaFile, it would force loading of schemas that have
   * already been seen and result in duplicate effort and slower schema
   * compilation.
   */

  protected final lazy val notSeenThisBefore = LV(Symbol("notSeenThisBefore")) {
    val mp = mapPair
    val res = seenBefore.value.get(mp) match {
      case Some(_) => false
      case None => true
    }
    res
  }.value

  /**
   * (ns, file) that we've seen before.
   *
   * Includes this file we're currently on (if we haven't seen it)
   * so that if our own includes/imports cycle back to us
   * we will detect it.
   */
  protected final lazy val seenBeforeThisFile: IIMap = LV(Symbol("seenBeforeThisFile")) {
    val res = Delay(
      Symbol("seenBeforeThisFile"),
      this, {
        val v =
          if (notSeenThisBefore) seenBefore.value + mapTuple
          else seenBefore.value
        v
      }
    )
    res
  }.value

  final lazy val seenAfter: IIMap = LV(Symbol("seenAfter")) {
    val res = iiSchemaFileMaybe.map { _.seenAfter }.getOrElse(seenBefore)
    res
  }.value

  final lazy val schemaLocationProperty = getAttributeOption("schemaLocation")

  protected final def isValidURI(uri: String): Boolean = {
    try { new URI(uri) }
    catch { case ex: URISyntaxException => return false }
    true
  }

  /**
   * Both include and import have schemaLocation. For import it is optional.
   * If supplied we resolve it via the classpath, relative
   * to the location of the including/importing file, etc.
   */

  protected final lazy val resolvedSchemaLocation: Option[DaffodilSchemaSource] =
    LV(Symbol("resolvedSchemaLocation")) {
      val res = schemaLocationProperty.flatMap { slText =>
        val enclosingSchemaSource = schemaFile.map { sf =>
          sf.schemaSource
        }
        val optURISchemaSource =
          XMLUtils.resolveSchemaLocation(slText, enclosingSchemaSource)
        val optSource = optURISchemaSource.map { case (uriSchemaSource, relToAbs) =>
          schemaDefinitionWarningWhen(
            WarnID.DeprecatedRelativeSchemaLocation,
            relToAbs,
            s"Resolving relative schemaLocations absolutely is deprecated. Did you mean /$slText"
          )
          // if isBootStrapSD is true, we assume we are using the fakeXMLSchemaDocument, which means
          // we will be passing in and receiving back an absolute diagnosticFilepath from resolveSchemaLocation.
          // In just this case, we want to ignore that absolute filepath and use the diagnosticFilepath
          // from main, which is the XMLSchemaDocument diagnosticFilepath
          val finalUriSchemaSource = if (xmlSchemaDocument.isBootStrapSD) {
            Assert.invariant(enclosingSchemaSource.isEmpty)
            URISchemaSource(xmlSchemaDocument.diagnosticFile, uriSchemaSource.uri)
          } else {
            uriSchemaSource
          }
          finalUriSchemaSource
        }
        optSource
      }
      res
    }.value

  protected def mapPair: (NS, DaffodilSchemaSource)

  protected final lazy val mapTuple = LV(Symbol("mapTuple")) {
    val tuple = (mapPair, this)
    tuple
  }.value

  /**
   * Holds the location of the schema, whether that is from
   * the XML Catalog (import), or classpath (import or include).
   */
  protected def resolvedLocation: DaffodilSchemaSource

  /**
   * Get the schema file if it isn't one we have seen before.
   *
   * If it new to us, then get it. This can cause a failure if the
   * file doesn't exist but we need it to determine the namespace.
   * That is, the Option here does NOT return None
   * for file-not-found. It's only about whether we've already
   * got this file in the (ns, url) map we're maintaining.
   *
   * This can succeed even if the file will not be found later
   * when someone asks for the schema document, or if at that
   * point its namespace is not acceptable given the import
   * statement.
   */
  final lazy val iiSchemaFileMaybe: Option[DFDLSchemaFile] = LV(Symbol("iiSchemaFileMaybe")) {
    val res = if (notSeenThisBefore) {
      Some(iiSchemaFile)
    } else None
    res
  }.value

  /**
   * Unconditionally, get the schema file object.
   */
  final lazy val iiSchemaFile: DFDLSchemaFile = LV(Symbol("iiSchemaFile")) {
    val res = new DFDLSchemaFile(schemaSet, resolvedLocation, this, seenBeforeThisFile)
    res.node // force access to the data of the file.
    res
  }.value

  /**
   * For error message if we don't find a file/resource.
   */
  private def classPathLines = classPath.mkString("\n")

  private def classPath = Misc.classPath

  protected final lazy val whereSearched =
    if (classPath.length == 0) " Classpath was empty."
    else " Searched these classpath locations: \n" + classPathLines + "\n"
}
