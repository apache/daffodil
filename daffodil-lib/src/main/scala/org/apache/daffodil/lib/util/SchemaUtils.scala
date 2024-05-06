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

package org.apache.daffodil.lib.util

import scala.xml._

import org.apache.daffodil.lib.xml._

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

  def dfdlTestSchemaUnqualified(
    includeImports: Seq[Node],
    topLevelAnnotations: Seq[Node],
    contentElements: Seq[Node],
    fileName: String = ""
  ): Elem =
    dfdlTestSchema(
      includeImports,
      topLevelAnnotations,
      contentElements,
      fileName = fileName,
      elementFormDefault = "unqualified"
    )

  def dfdlTestSchemaWithTarget(
    includeImports: Seq[Node],
    topLevelAnnotations: Seq[Node],
    contentElements: Seq[Node],
    theTargetNS: String,
    elementFormDefault: String = "qualified",
    hasDefaultNamespace: Boolean = true
  ): Elem = {
    val tns = if (theTargetNS == "" || theTargetNS == null) NoNamespace else NS(theTargetNS)
    val sch =
      dfdlTestSchema(
        includeImports,
        topLevelAnnotations,
        contentElements,
        targetNamespace = tns,
        defaultNamespace = tns,
        elementFormDefault = elementFormDefault,
        useDefaultNamespace = hasDefaultNamespace
      )
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
    includeImports: Seq[Node],
    topLevelAnnotations: Seq[Node],
    contentElements: Seq[Node],
    schemaScope: NamespaceBinding = TopScope, // from the defineSchema node
    fileName: String = "",
    targetNamespace: NS = XMLUtils.targetNS,
    defaultNamespace: NS = XMLUtils.targetNS,
    elementFormDefault: String = "qualified",
    useDefaultNamespace: Boolean = true,
    useTNS: Boolean = true
  ): Elem = {
    val fileAttrib =
      if (fileName == "") Null
      else Attribute(XMLUtils.INT_PREFIX, XMLUtils.FILE_ATTRIBUTE_NAME, Text(fileName), Null)
    val targetNamespaceAttrib =
      if (targetNamespace == NoNamespace) Null
      else Attribute(None, "targetNamespace", Text(targetNamespace.uri.toString), Null)
    var scope =
      if (schemaScope != TopScope) schemaScope
      else {
        import XMLUtils._
        <ignore xmlns:xsd={xsdURI} xmlns:dfdl={dfdlURI} xmlns:xsi={xsiURI} xmlns:fn={
          fnURI
        } xmlns:math={mathURI}/>.scope
      }
    scope = XMLUtils.combineScopes("xs", XMLUtils.xsdURI, scope) // always need this one
    if (useDefaultNamespace) {
      scope = XMLUtils.combineScopes(null, defaultNamespace, scope)
    }
    if (useTNS)
      scope = XMLUtils.combineScopes("tns", targetNamespace, scope)
    scope = XMLUtils.combineScopes("ex", targetNamespace, scope)
    scope = XMLUtils.combineScopes("dfdlx", XMLUtils.DFDLX_NAMESPACE, scope)
    scope = XMLUtils.combineScopes(XMLUtils.INT_PREFIX, XMLUtils.INT_NS, scope)

    val schemaNode =
      <xs:schema elementFormDefault={elementFormDefault}>
        {
        includeImports
      }
        <xs:annotation>
          <xs:appinfo source={XMLUtils.dfdlAppinfoSource}>
            {topLevelAnnotations}
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
