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
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.Info
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
                uriString)
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

  override lazy val uriString = {
    this.uriStringFromAttribute.getOrElse("file:unknown")
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

