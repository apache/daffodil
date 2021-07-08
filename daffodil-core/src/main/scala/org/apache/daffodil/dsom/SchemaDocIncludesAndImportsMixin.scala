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

package org.apache.daffodil.dsom

import scala.xml.Node
import scala.xml.NodeSeq

import org.apache.daffodil.dsom.IIUtils._
import org.apache.daffodil.equality._
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.NoNamespace

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

  /**
   * A schema document gets its target namespace from the targetNamespace attribute
   * of the xs:schema root.
   *
   * However, if this schema document is being included in another document, then
   * the target namespace (if none is specified) is of the document into which this is being included.
   * If this schema document DOES have a target namespace, then it must match the target
   * namespace of the schema into which this is being included.
   *
   * And, if this schema document is being imported, then the target
   * namespace (if found) on the xs:schema of this schema document must match
   * that of the import statement. If a namespace is specified there.
   */
  override lazy val targetNamespace: NS = LV('targetNamespace) {
    val checkedNS =
      ii.map {
        _ match {
          case inc: Include => {
            sdTNSAttrib.map { tns =>
              schemaDefinitionUnless(inc.targetNamespace =:= tns,
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
  }.value

  // There is one distinguished top level SchemaDocument
  // that we use to start the ball rolling by importing all the
  // files that the user supplies via the API/command line.
  // For all other SchemaDocuments they are not the bootstrap.
  //
  def isBootStrapSD: Boolean

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

  protected def seenBefore: IIMap

  // val iiXML: Node = xml // override in SchemaSet

  lazy val impNodes = (xml \ "import")
  lazy val incNodes = (xml \ "include")

  val mtList: List[IIBase] = Nil

  /**
   * This is a bit complex. It's folding the list of includes (or imports)
   * and threading the accumulating map of included/imported things we've
   * already seen, and also building up a shorter list of just the local
   * children.
   */
  def getImportsOrIncludes(
    seenStart: IIMap,
    nodes: NodeSeq,
    factory: (Node, XMLSchemaDocument, IIMap) => IIBase): (IIMap, List[IIBase]) = {
    val res = nodes.foldLeft((seenStart, mtList)) {
      case ((seen, localList), iNode) =>
        {
          val i = factory(iNode, this, seen)
          val sa = i.seenAfter
          val locals = i +: localList
          (sa, locals)
        }
    }
    res
  }

  def importStatementsMap = ismli_._1
  def localImports = ismli_._2
  private lazy val ismli_ = LV('importStatementsMap_localImports) {
    val res = getImportsOrIncludes(seenBefore, impNodes, new Import(_, _, _))
    res
  }.value

  def seenAfter = sali_._1
  def localIncludes = sali_._2
  private lazy val sali_ = LV('seenAfter_localIncludes) {
    val res = getImportsOrIncludes(importStatementsMap, incNodes, new Include(_, _, _))
    res
  }.value

}
