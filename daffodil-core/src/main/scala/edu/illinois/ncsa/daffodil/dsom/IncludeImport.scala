package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import edu.illinois.ncsa.daffodil.util.Misc
import java.net.URL
import scala.xml.Node
import scala.xml.Elem
import scala.xml.Attribute
import scala.xml.Null
import org.apache.xerces.util.XMLResourceIdentifierImpl
import java.io.IOException
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.Info
import IIUtils._
import java.io.File
import java.net.URI
import scala.xml.NodeSeq
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.DaffodilCatalogResolver
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG
import edu.illinois.ncsa.daffodil.util.Delay
import java.net.URLDecoder
import java.net.URLEncoder

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
 */
object IIUtils {
  type IIMap = Delay[Map[(NS, URI), IIBase]]
}

/**
 * Mixin for all SchemaComponents
 */

trait SchemaComponentIncludesAndImportsMixin { self: SchemaComponent =>

  lazy val targetNamespace: NS = targetNamespace_.value
  private val targetNamespace_ = LV('targetNamespace) {
    val res = xmlSchemaDocument.targetNamespace
    res
  }

  lazy val targetNamespacePrefix = xml.scope.getPrefix(targetNamespace.toString)

  val orElseURL: String = "file:???"

  /**
   * Used in diagnostic messages; hence, valueOrElse to avoid
   * problems when this can't get a value due to an error.
   */
  lazy val fileName: String = fileName_.valueOrElse(orElseURL)
  private val fileName_ = LV('fileName) {
    xmlSchemaDocument.fileName
  }

}

/**
 * Mixin for SchemaSet
 */

trait SchemaSetIncludesAndImportsMixin { self: SchemaSet =>

  /**
   * Let's take the list of file names given, and make a fake schema
   * document with import statements for them. Then the algorithms
   * are all isolated to just the SchemaDocument class and the Include
   * and Import classes.
   */
  lazy val fakeXMLSchemaDocument = {
    val xsd = XMLUtils.XSD_NAMESPACE.toString

    // Any time we synthesize xml we have to grab the namespace definitions and 
    // make sure we drag them along onto the new structures.
    val fakeImportStatementsXML = schemaFiles.map { fn =>
      <import schemaLocation={ fn.toURI.toURL.toString } xmlns={ xsd }/>
    }

    val fakeSchemaDocXML =
      <schema xmlns={ xsd }>{ fakeImportStatementsXML }</schema>

    val initialEmptyIIMap: IIMap = Delay(Map.empty)

    val fakeSD = new XMLSchemaDocument(fakeSchemaDocXML, self, None, None, initialEmptyIIMap) {
      // this flag lets us import into this document
      // even though it does not have a namespace
      override val isBootStrapSD = true
    }
    fakeSD
  }

  lazy val allSchemaDocuments = allSchemaDocuments_.value
  private val allSchemaDocuments_ = LV('allSchemaDocuments) {
    allSchemaFiles.map { _.iiSchemaDocument }
  }

  lazy val allSchemaFiles = allSchemaFiles_.value
  private val allSchemaFiles_ = LV('allSchemaFiles) {
    val fd = fakeXMLSchemaDocument //bootstrap
    val sa = fd.seenAfter
    val sfl = sa.value.flatMap {
      case (_, ii) => {
        val sf = ii.iiSchemaFileMaybe // maybe not if we've already seen this file for the same namespace.
        sf
      }
    }.toList
    sfl
  }

}

/**
 * Mixin for SchemaDocument
 */
trait SchemaDocIncludesAndImportsMixin { self: XMLSchemaDocument =>

  /**
   * For include, if the included schema doesn't have a
   * targetNamespace, then we will take on the namespace
   * of whatever we are included into.
   *
   * This is the chameleon namespace concept, and it works
   * inductively. I.e., the included schema could include more
   * schemas, all of them ultimately getting the targetNamespace
   * from the schema enclosing that outermost include.
   *
   * If an included schema DOES have a targetNamespace, it must match what we're
   * included into.
   */
  lazy val sdTNSAttrib = this.getAttributeOption("targetNamespace").map { NS(_) }
  lazy val sdTargetNS = sdTNSAttrib.getOrElse(NoNamespace)

  override lazy val targetNamespace: NS = targetNamespace_.value
  private val targetNamespace_ = LV('targetNamespace) {
    val checkedNS =
      ii.map {
        _ match {
          case inc: Include => {
            sdTNSAttrib.map { tns =>
              schemaDefinitionUnless(inc.targetNamespace == tns,
                "Included schema does not have the same namespace as the file %s including it.",
                fileName)
              tns
            }.getOrElse(inc.targetNamespace)
          }
          case imp: Import => {
            val xmlSchemaDocContainingTheImportStatement = imp.xmlSchemaDocument
            val res = checkImportCompatibleNS(
              imp.importElementNS, sdTargetNS,
              xmlSchemaDocContainingTheImportStatement)
            res
          }
        }
      }
    val resultNS = checkedNS.getOrElse {
      ii.map { _.targetNamespace }.getOrElse(NoNamespace)
    }
    resultNS
  }

  // There is one distinguished top level SchemaDocument
  // that we use to start the ball rolling by importing all the 
  // files that the user supplies via the API/command line.
  // For all other SchemaDocuments they are not the bootstrap.
  //
  val isBootStrapSD = false

  def checkImportCompatibleNS(
    importElementNS: Option[NS],
    schemaDocsNS: NS,
    schemaDocContainingTheImportStatement: XMLSchemaDocument) = {
    (importElementNS, schemaDocsNS, schemaDocContainingTheImportStatement.targetNamespace) match {
      case (None, NoNamespace, NoNamespace) =>
        if (schemaDocContainingTheImportStatement.isBootStrapSD) NoNamespace
        else schemaDefinitionError("Namespaces of importing and imported schemas cannot both be no namespace.")
      case (None, NoNamespace, _) => NoNamespace
      case (None, importedSchemaNS, _) =>
        if (schemaDocContainingTheImportStatement.isBootStrapSD) importedSchemaNS
        else schemaDefinitionError("Import element specifies no namespace, but the imported schema has namespace %s.", importedSchemaNS)
      case (Some(importElementNS), importedSchemaNS, _) if (importElementNS != importedSchemaNS) =>
        schemaDefinitionError("Import element specifies namespace %s but namespace %s of imported schema does not match.", importElementNS, importedSchemaNS)
      case (Some(importElementNS), _, importingSchemaNS) if (importElementNS == importingSchemaNS) =>
        schemaDefinitionError("Importing schema namespace %s and imported schema namespace must be different.", importingSchemaNS)
      case (Some(importElementNS), _, _) => importElementNS
    }
  }

  override lazy val fileName = {
    this.fileNameFromAttribute().getOrElse("file:unknown")
  }

  def seenBefore: IIMap

  // val iiXML: Node = xml // override in SchemaSet

  lazy val impNodes = {
    val i = (xml \ "import")
    log(LogLevel.Debug, "There are %s imports", i.length)
    i
  }
  lazy val incNodes = (xml \ "include")

  val mtList: List[IIBase] = Nil

  /**
   * This is a bit complex. It's folding the list of includes (or imports)
   * and threading the accumulating map of included/imported things we've
   * already seen, and also building up a shorter list of just the local
   * children.
   */
  def getImportsOrIncludes(
    seenStartArg: IIMap,
    nodes: NodeSeq,
    factory: (Node, XMLSchemaDocument, IIMap) => IIBase): (IIMap, List[IIBase]) = {
    lazy val seenStart = seenStartArg
    val res = nodes.foldLeft((seenStart, mtList)) {
      case ((seen, localList), iNode) =>
        OOLAG.keepGoing((seen, localList)) {
          val i = factory(iNode, this, seen)
          val sa = i.seenAfter
          val locals = i +: localList
          (sa, locals)
        }
    }
    res
  }

  lazy val (importStatementsMap, localImports) = ismli_.value
  private val ismli_ = LV('importStatementsMap_localImports) {
    val res = getImportsOrIncludes(seenBefore, impNodes, new Import(_, _, _))
    res
  }

  lazy val (seenAfter, localIncludes) = sali_.value
  private val sali_ = LV('seenAfter_localIncludes) {
    val res = getImportsOrIncludes(importStatementsMap, incNodes, new Include(_, _, _))
    res
  }

}

/**
 * Include/Import = "II" for short
 */
abstract class IIBase(xml: Node, xsdArg: XMLSchemaDocument, val seenBefore: IIMap)
  extends SchemaComponent(xml, xsdArg) {

  /**
   * An import/include requires only that we can access the
   * schema file, recursively any of its includes/imports,
   * and that all the resulting are validated by the validating loader.
   */
  requiredEvaluations(iiSchemaFile, iiSchemaFile.iiXMLSchemaDocument)

  lazy val notSeenThisBefore = notSeenThisBefore_.value
  private val notSeenThisBefore_ = LV('notSeenThisBefore) {
    val mp = mapPair
    val res = seenBefore.value.get(mp) match {
      case Some(_) => false
      case None => true
    }
    res
  }

  /**
   * (ns, file) that we've seen before.
   *
   * Includes this file we're currently on (if we haven't seen it)
   * so that if our own includes/imports cycle back to us
   * we will detect it.
   */
  lazy val seenBeforeThisFile: IIMap = seenBeforeThisFile_.value
  private val seenBeforeThisFile_ = LV('seenBeforeThisFile) {
    val res = Delay {
      val v = if (notSeenThisBefore) Delay(seenBefore.value + mapTuple)
      else seenBefore
      v.value
    }
    res
  }

  lazy val seenAfter: IIMap = seenAfter_.value
  private val seenAfter_ = LV('seenAfter) {
    val res = iiSchemaFileMaybe.map { _.seenAfter }.getOrElse(seenBefore)
    res
  }

  lazy val schemaLocationProperty = getAttributeOption("schemaLocation")

  def isValidURI(uri: String): Boolean = {
    try { val res = new URI(uri) } catch { case ex: Exception => return false }
    true
  }

  /**
   * Both include and import have schemaLocation. For import it is optional.
   * If supplied we resolve it via the classpath, current working dir, relative
   * to the location of the including/importing file, etc.
   */

  final lazy val resolvedSchemaLocation: Option[URI] = resolvedSchemaLocation_.value
  private val resolvedSchemaLocation_ = LV('resolvedSchemaLocation) {
    val res = schemaLocationProperty.flatMap { slText =>
      // We need to determine if the URI is valid, if it's not we should attempt to encode it
      // to make it valid (takes care of spaces in directories). If it fails after this, oh well!
      val encodedSLText = if (!isValidURI(slText)) URLEncoder.encode(slText, "UTF-8") else slText
      val fileURI = URI.create(encodedSLText)
      val optURI =
        if (fileURI.isAbsolute() &&
          (new File(fileURI)).exists) Some(fileURI)
        else {
          // file is relative
          // So we try to resolve it a few different ways
          //
          // Removed this first case intentionally. Looking in the CWD 
          // would be a security risk/issue. So if a user wants the CWD
          // they should add "." to their classpath to get this behavior.
          //
          ///////////////////////////////////////////////////////////////////
          // (1) First try the current working directory, or however
          // the JVM completes a relative File when you call getAbsolutePath()
          //
          //          val relPathString = fileURI.getPath()
          //          val relFile = new File(relPathString)
          //          val absPath = relFile.getAbsolutePath()
          //          val absURI = new URI(absPath)
          //          val absFile = new File(absURI)
          //          if (absFile.exists()) {
          //            // found in CWD
          //            Some(absURI.toURL)
          //          } else {
          ///////////////////////////////////////////////////////////////////
          //
          // (2) Try self-relative, that is, relative to wherever this schema
          // doing the import/include is in the file system (or jar) resources.
          //
          val enclosingSchemaAbsURI: Option[URI] = schemaFile.map { _.uri }
          val selfRelative = enclosingSchemaAbsURI match {
            case None => None
            case Some(enclosingAbsURI) => {
              val absURI = (new URL(enclosingAbsURI.toURL, fileURI.toString)).toURI
              val absFile = new File(absURI)
              if (absFile.exists())
                // Win. found one relative to file doing the include/import
                Some(absURI)
              else
                // Nope. Not found relative to the file.
                None
            }
          }
          if (selfRelative.isDefined) selfRelative
          else {
            //
            // (2) Try classpath
            //
            val (optURI, _) = Misc.getResourceOption(slText) // searches classpath directories and classpath in jars.
            if (optURI.isDefined) {
              // found on classpath
              optURI
            } else {
              None
            }
          }
        }
      optURI
    }
    res
  }

  def mapPair: (NS, URI)

  final lazy val mapTuple = mapTuple_.value
  private val mapTuple_ = LV('mapTuple) {
    val tuple = (mapPair, this)
    tuple
  }

  /**
   * Holds the location of the schema, whether that is from
   * the XML Catalog (import), or classpath (import or include).
   */
  def resolvedLocation: URI

  //  lazy val super_iiSchemaFile = super_iiSchemaFile_.value
  //  private val super_iiSchemaFile_ = LV('super_iiSchemaFile) {
  //    val f = new DFDLSchemaFile(sset, resolvedLocation, this, seenBeforeThisFile)
  //    f
  //  }

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
  lazy val iiSchemaFileMaybe: Option[DFDLSchemaFile] = iiSchemaFileMaybe_.value
  private val iiSchemaFileMaybe_ = LV('iiSchemaFileMaybe) {
    val res = if (notSeenThisBefore) {
      Some(iiSchemaFile)
    } else None
    res
  }

  /**
   * Unconditionally, get the schema file object.
   */
  lazy val iiSchemaFile: DFDLSchemaFile = iiSchemaFile_.value
  private val iiSchemaFile_ = LV('iiSchemaFile) {
    val res = new DFDLSchemaFile(schemaSet, resolvedLocation, this, seenBeforeThisFile)
    res
  }

  /**
   * For error message if we don't find a file/resource.
   */
  lazy val classPathLines = classPathNotJars.mkString("\n")

  lazy val classPathNotJars = Misc.classPath.filterNot { _.endsWith(".jar") }

  lazy val whereSearched =
    if (classPathNotJars.length == 0) " Classpath was empty."
    else " Searched these classpath locations: \n" + classPathLines + "\n"
}
/**
 * enclosingGoalNS is None if this include
 * is being included (by one include hop, or several) into a schema having
 * 'no namespace'
 *
 * enclosingGoalNS is Some(str) if this include
 * is being included (by one include hop, or several) into a schema having
 * a targetNamespace.
 */
class Include(xml: Node, xsd: XMLSchemaDocument, seenArg: IIMap)
  extends IIBase(xml, xsd, seenArg) {

  final lazy val mapPair = mapPair_.value
  private val mapPair_ = LV('mapPair) {
    // for an include, the targetNamespace of the schema document that contained us is right.
    val mp = (targetNamespace, resolvedLocation)
    mp
  }

  lazy val slText = schemaLocationProperty.get // include always has a schemaLocation property

  lazy val resolvedNamespaceURI = None // include doesn't have a namespace.

  // include always has a schemaLocation
  lazy val resolvedLocation = resolvedLocation_.value
  private val resolvedLocation_ = LV('resolvedLocation) {
    resolvedSchemaLocation match {
      case Some(rsl) => {
        val ns = OOLAG.keepGoing(
          schemaDefinitionError("Unable to determine target namespace.")) {
            xsd.targetNamespace
          }
        log(Info("Included schema from %s into namespace %s.", rsl, ns))
        rsl
      }
      case None => schemaDefinitionError("Included schema not found at location %s." + whereSearched, slText)
    }
  }

}

/**
 * An import statement.
 *
 * The enclosingGoalNamespace argument is Some(noNamespace) for a topLevel schema file
 * that has no targetNamespace attribute.
 *
 * Now consider that we could be an import which is inside an included schema which includes another
 * included, etc. A nest of included schemas the innermost of which then contains an import.
 * We have to verify that the ultimate goal namespace at the start of that chain of includes
 * is different from this imported schema's goalNamespace.
 */
class Import(importNode: Node, xsd: XMLSchemaDocument, seenArg: IIMap)
  extends IIBase(importNode, xsd, seenArg) {

  final lazy val mapPair = mapPair_.value
  private val mapPair_ = LV('mapPair) {
    val mpOpt = importElementNS.map { ieNS => (ieNS, resolvedLocation) }
    val mp = mpOpt.getOrElse {
      //
      // we didn't have a namespace property in the import component
      // So we have to load the file to find out what the namespace
      // is, and then we might decide to use it, or not use it.
      //
      // This means we need to unconditionally load the schemaDocument
      // and not do checks nor use the incoming set of "seenBefore" 
      // schemas - as we need to open this schema file simply to see 
      // its namespace.

      // FIXME: if you have an import like this:
      // <import schemaLocation="..."/> 
      // This code will read the file TWICE. Once just to peek at the
      // namespace. 
      // 
      // This should be cached. i.e., cache the loaded schema file 
      // object by way of a factory without the final map parameter
      // (similarly, for DFDLSchemaDocument, the Import/Exports etc.)
      //
      val sf = iiSchemaFile // new DFDLSchemaFile(sset, resolvedLocation, this, Map.empty)
      val xsd = sf.iiXMLSchemaDocument
      val xsdtns = xsd.targetNamespace
      (xsdtns, resolvedLocation)
    }
    mp
  }

  lazy val importElementNS = getAttributeOption("namespace").map { NS(_) }

  //  override def iiSchemaDocument = iiSchemaDocument_.value
  //  private val iiSchemaDocument_ = LV('iiSchemaDocument) {
  //    checkedNS
  //    log(Info("Imported Schema Namespace: %s from location %s.", checkedNS, resolvedLocation))
  //    super_iiSchemaDocument
  //  }

  override lazy val targetNamespace: NS = targetNamespace_.value
  private val targetNamespace_ = LV('targetNamespace) {
    val tns = importElementNS match {
      case Some(ns) => ns // don't load it just to check compatibility.
      case None => iiSchemaFile.iiSchemaDocument.targetNamespace // load it because we have to have it.
    }
    tns
  }

  /**
   * Only import has a namespace URI.
   *
   * This will be Some(URL) for reading an imported schema,
   * if we resolved the namespace URI via the XML Catalog.
   */
  lazy val resolvedNamespaceURI: Option[URI] = resolvedNamespaceURI_.value
  private val resolvedNamespaceURI_ = LV('resolvedNamespaceURI) {
    importElementNS match {
      case None => {
        schemaDefinitionUnless(schemaLocationProperty != None, "When there is no namespace specified, there must be a schemaLocation specified.")
        None
      }
      case Some(ns) => {
        val uri = resolver.resolveURI(ns.toString)
        if (uri == null) None
        else {
          val res = URI.create(uri)
          Some(res)
        }
      }
    }
  }

  lazy val resolver = DaffodilCatalogResolver.resolver // iiSchemaFileMaybe.map { _.resolver }
  lazy val catFiles = resolver.catalogFiles.mkString(", ")

  /**
   * XML Catalog is tried first, then classpath
   */
  lazy val resolvedLocation = resolvedLocation_.value
  private val resolvedLocation_ = LV('resolvedLocation) {
    val rl = (importElementNS, resolvedNamespaceURI, schemaLocationProperty, resolvedSchemaLocation) match {
      case (None, _, Some(sl), None) =>
        schemaDefinitionError("Unable to import a no-namespace schema from schema location %s." + whereSearched, sl)
      case (Some(_), Some(rnURI), _, _) => rnURI // found it in the catalog based on namespace attribute
      case (Some(ns), None, Some(sl), None) =>
        schemaDefinitionError("Unable to import namespace %s from XML catalog(s) %s or schema location %s." + whereSearched, ns, catFiles, sl)
      case (_, None, Some(sl), Some(rsl)) => rsl // found it by way of the schemaLocation
      case (Some(ns), None, None, None) => {

        schemaDefinitionError("Unable to import namespace %s using XML Catalog(s) %s", ns, catFiles)
      }
      case _ => Assert.invariantFailed("illegal combination of namespace and schemaLocation")
    }
    rl
  }

}
