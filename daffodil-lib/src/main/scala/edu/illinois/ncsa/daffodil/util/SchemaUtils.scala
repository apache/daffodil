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

package edu.illinois.ncsa.daffodil.util

import scala.xml._
import edu.illinois.ncsa.daffodil.xml._
import java.io.File
import java.io.FileNotFoundException
import edu.illinois.ncsa.daffodil.Implicits._

/*
 * This is not a file of tests.
 *
 * These are utilities to support unit testing schemas
 */
object SchemaUtils {

  /**
   * utility to create test schemas without having to repeat all the namespace definitions,
   * and the appinfo boilerplate. This makes tests more uniform.
   *
   * Defines standard named formats that can be re-used by tests. This helps with the issue
   * of having to add properties everywhere when a new property starts being expected.
   */

  /**
   * Constructs a DFDL schema more conveniently than having to specify all those xmlns attributes.
   */

  def dfdlTestSchemaUnqualified(topLevelAnnotations: Seq[Node], contentElements: Seq[Node], fileName: String = ""): Elem =
    dfdlTestSchema(topLevelAnnotations, contentElements, fileName = fileName, elementFormDefault = "unqualified")

  def dfdlTestSchemaWithTarget(
    topLevelAnnotations: Seq[Node],
    contentElements: Seq[Node],
    theTargetNS: String,
    elementFormDefault: String = "qualified"): Elem = {
    val tns = if (theTargetNS == "" || theTargetNS == null) NoNamespace else NS(theTargetNS)
    val sch =
      dfdlTestSchema(topLevelAnnotations, contentElements,
        targetNamespace = tns, defaultNamespace = tns, elementFormDefault = elementFormDefault)
    sch
  }

  /**
   * Use to create test schemas more conveniently.
   *
   * Now has optional args to allow control of elementFormDefault, whether
   * there is a targetNamespace, and whether there is a default namespace.
   *
   * Fixed (2014-10-01) to avoid proliferation of xmlns namespace bindings
   * as those were aggravating a xerces bug that we might as well avoid.
   * This also makes these much more readable.
   */
  def dfdlTestSchema(
    topLevelAnnotations: Seq[Node],
    contentElements: Seq[Node],
    schemaScope: NamespaceBinding = TopScope, // from the defineSchema node
    fileName: String = "",
    targetNamespace: NS = XMLUtils.targetNS,
    defaultNamespace: NS = XMLUtils.targetNS,
    elementFormDefault: String = "qualified"): Elem = {
    val fileAttrib =
      if (fileName == "") Null
      else Attribute(XMLUtils.INT_PREFIX, "file", Text(fileName), Null)
    val targetNamespaceAttrib =
      if (targetNamespace == NoNamespace) Null
      else Attribute(None, "targetNamespace", Text(targetNamespace.uri.toString), Null)
    var scope =
      if (schemaScope != TopScope) schemaScope
      else {
        import XMLUtils._
        <ignore xmlns:xsd={ xsdURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI } xmlns:fn={ fnURI } xmlns:dafint={ dafintURI }/>.scope
      }
    scope = XMLUtils.combineScopes("xs", XMLUtils.xsdURI, scope) // always need this one
    scope = XMLUtils.combineScopes(null, defaultNamespace, scope)
    scope = XMLUtils.combineScopes("tns", targetNamespace, scope)
    scope = XMLUtils.combineScopes("ex", targetNamespace, scope)

    val schemaNode =
      <xs:schema elementFormDefault={ elementFormDefault } attributeFormDefault="unqualified">
        <xs:include schemaLocation="xsd/built-in-formats.xsd"/>
        <xs:annotation>
          <xs:appinfo source={ XMLUtils.dfdlAppinfoSource }>
            { topLevelAnnotations }
          </xs:appinfo>
        </xs:annotation>
        {
          contentElements.map {
            case e: Elem => {
              val combined = XMLUtils.combineScopes(e.scope, scope)
              val res = e.copy(scope = combined)
              res
            }
          }
        }
      </xs:schema>.copy(scope = scope) % targetNamespaceAttrib % fileAttrib
    //
    // Note: no longer needs to write out and re-load to get namespaces
    // right. The namespaces are explicitly attached by copying the nodes and putting
    // specific scopes in place.
    //
    val res = XMLUtils.collapseScopes(schemaNode, TopScope).asInstanceOf[Elem]
    res
  }

}
