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
import org.apache.daffodil.xml._
import org.apache.daffodil.dpath.NodeInfo

/**
 * There are 3 first-class concrete children of ElementBase.
 * Root, LocalElementDecl, and ElementRef
 */
final class ElementRef(xmlArg: Node, lexicalParent: GroupDefLike, position: Int)
  extends AbstractElementRef(xmlArg, lexicalParent, position)

abstract class AbstractElementRef(
  xmlArg: Node,
  parentArg: SchemaComponent,
  positionArg: Int)
  extends ElementBase
  with HasRefMixin
  with NamedMixin
  with NestingLexicalMixin {

  override lazy val xml = xmlArg
  final override lazy val optLexicalParent = Option(parentArg)
  final override lazy val position = positionArg

  def complexType: ComplexTypeBase = this.referencedElement.complexType
  def defaultValueAsString: String = this.referencedElement.defaultValueAsString
  def fixedValueAsString: String = this.referencedElement.fixedValueAsString
  def hasDefaultValue: Boolean = this.referencedElement.hasDefaultValue
  def hasFixedValue: Boolean = this.referencedElement.hasFixedValue
  def isComplexType: Boolean = this.referencedElement.isComplexType
  def isNillable: Boolean = this.referencedElement.isNillable
  def isSimpleType: Boolean = this.referencedElement.isSimpleType
  def simpleType: SimpleTypeBase = this.referencedElement.simpleType
  def primType: NodeInfo.PrimType = this.referencedElement.primType
  def optSimpleType = this.referencedElement.optSimpleType
  def optComplexType = this.referencedElement.optComplexType

  override lazy val optReferredToComponent = Some(referencedElement)

  /**
   * Note: since the namedQName might not exist, we cannot use
   * this in diagnostic messages. So any method/lazyval that
   * invokes namedQName because it's a named thing, must be overridden
   * and use refQName instead.
   */
  override def namedQName: NamedQName = LV('namedQName) {
    referencedElement.namedQName
  }.value

  override lazy val name = refQName.local

  override lazy val prefix = refQName.prefix.getOrElse(null)

  // Need to go get the Element we are referencing
  lazy val referencedElement: GlobalElementDecl = LV('referencedElement) {
    val ged = this.schemaSet.getGlobalElementDecl(refQName)
    val res = ged match {
      case None => {
        //
        // this element ref refers to something not found.
        //
        // That might be because the QName namespace prefix is no good, or
        // because there is no element with that global name.
        //
        // Can't use namedQName because that's the resolved one
        // must use the refQName
        //
        SDE("Referenced element not found: %s.", this.refQName)
      }
      case Some(x) => x
    }
    res
  }.value

  override lazy val namespace = refQName.namespace

  override lazy val diagnosticDebugName = "element reference " + refQName

  override def typeDef = referencedElement.typeDef

}
