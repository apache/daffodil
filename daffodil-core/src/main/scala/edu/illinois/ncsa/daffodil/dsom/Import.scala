/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.util.Misc
import java.net.URL
import scala.xml.Node
import scala.xml.Elem
import scala.xml.Attribute
import scala.xml.Null
import scala.collection.immutable.ListMap
import org.apache.xerces.util.XMLResourceIdentifierImpl
import java.io.IOException
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.api.DaffodilSchemaSource
import edu.illinois.ncsa.daffodil.api.URISchemaSource
import edu.illinois.ncsa.daffodil.util._
import IIUtils._
import java.io.File
import java.net.URI
import scala.xml.NodeSeq
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG
import edu.illinois.ncsa.daffodil.util.Delay
import java.net.URLDecoder
import java.net.URLEncoder

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
final class Import(importNode: Node, xsd: XMLSchemaDocument, seenArg: IIMap)
    extends IIBase(importNode, xsd, seenArg) {

  final def mapPair = LV('mapPair) {
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
      val sf = iiSchemaFile
      val xsd = sf.iiXMLSchemaDocument
      val xsdtns = xsd.targetNamespace
      (xsdtns, resolvedLocation)
    }
    mp
  }.value

  lazy val importElementNS = getAttributeOption("namespace").map { NS(_) }

  override lazy val targetNamespace: NS = LV('targetNamespace) {
    val tns = importElementNS match {
      case Some(ns) => ns // don't load it just to check compatibility.
      case None => iiSchemaFile.iiSchemaDocument.targetNamespace // load it because we have to have it.
    }
    tns
  }.value

  /**
   * Only import has a namespace URI.
   *
   * This will be Some(URL) for reading an imported schema,
   * if we resolved the namespace URI via the XML Catalog.
   */
  lazy val resolvedNamespaceURI: Option[DaffodilSchemaSource] = LV('resolvedNamespaceURI) {
    importElementNS match {
      case None => {
        schemaDefinitionUnless(schemaLocationProperty != None, "When there is no namespace specified, there must be a schemaLocation specified.")
        None
      }
      case Some(ns) => {
        val uri = resolver.resolveURI(ns.toString)
        if (uri == null) None
        else {
          val res = URISchemaSource(URI.create(uri))
          Some(res)
        }
      }
    }
  }.value

  private lazy val resolver = xsd.schemaSet.resolver // iiSchemaFileMaybe.map { _.resolver }
  private lazy val catFiles = resolver.catalogFiles.mkString(", ")

  /**
   * XML Catalog is tried first, then classpath
   */
  lazy val resolvedLocation: DaffodilSchemaSource = LV('resolvedLocation) {

    log(LogLevel.Resolver, "Computing resolvedLocation")
    log(LogLevel.Resolver, "\nimportElementNS='%s'\nresolvedNamespaceURI='%s'\nschemaLocationProperty='%s'\nresolvedSchemaLocation='%s'".format(
      importElementNS, resolvedNamespaceURI, schemaLocationProperty, resolvedSchemaLocation))

    val rl = (importElementNS, resolvedNamespaceURI, schemaLocationProperty, resolvedSchemaLocation) match {
      case (None, _, Some(sl), None) => {
        if (xsd.isBootStrapSD) {
          //
          // special case - one of the user-supplied files (that we wrap in a
          // fake import statement and a fake surrounding document
          // doesn't exist.
          // We don't want the message to discuss those fake things.
          //
          schemaDefinitionError("No schema document at location %s.", sl)
        } else {
          schemaDefinitionError("Unable to import a no-namespace schema from schema location %s. %s", importNode, whereSearched)
        }
      }
      case (Some(_), Some(rnURI), _, _) => rnURI // found it in the catalog based on namespace attribute
      case (Some(ns), None, Some(sl), None) =>
        schemaDefinitionError("Unable to import namespace %s from XML catalog(s) %s or schema location %s. %s", ns, catFiles, importNode, whereSearched)
      case (_, None, Some(sl), Some(rsl)) => rsl // found it by way of the schemaLocation
      case (Some(ns), None, None, None) => {
        schemaDefinitionError("Unable to import namespace %s using XML Catalog(s) %s", ns, catFiles)
      }
      case _ => Assert.invariantFailed("illegal combination of namespace and schemaLocation")
    }
    rl
  }.value

}
