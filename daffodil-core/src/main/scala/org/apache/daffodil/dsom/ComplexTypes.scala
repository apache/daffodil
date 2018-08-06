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
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.xml.QName

abstract class ComplexTypeBase(xmlArg: Node, parentArg: SchemaComponent)
  extends SchemaComponentImpl(xmlArg, parentArg)
  with TypeBase
  with NonPrimTypeMixin {

  final override def optRestriction = None
  final override def optUnion = None
  final override def typeNode = NodeInfo.Complex

  requiredEvaluations(modelGroup)

  override def elementDecl: ElementDeclMixin

  protected final lazy val <complexType>{ xmlChildren @ _* }</complexType> = xml

  final def group = modelGroup

  /**
   * Convenience methods for unit testing. Just makes tests a bit more compact and clearer.
   */
  final def sequence = group.asInstanceOf[Sequence]
  final def choice = group.asInstanceOf[Choice]

  final lazy val Seq(modelGroup) = {
    val s = smg
    schemaDefinitionUnless(s.length == 1, "A complex type must have exactly one model-group element child which is a sequence, choice, or group reference.")
    s
  }

  private def smg = LV('smg) {
    xmlChildren.flatMap {
      xmlChild =>
        {
          val g = ModelGroupFactory(xmlChild, this, 1, false) // discards unwanted text nodes also.
          g
        }
    }
  }.value

  // provides needed polymorphism across unannotated complex types, and
  // the annotated objects.
  lazy val localAndFormatRefProperties: Map[String, String] = {
    Map.empty[String, String]
  }

  //  lazy val isScannable: Boolean = {
  //    val selfOK = modelGroup.group.isScannable
  //    if (!selfOK) false
  //    else {
  //      val parentElem: ElementBase = enclosingComponent.get.asInstanceOf[ElementBase]
  //      val unScannableChildren = modelGroup.group.groupMembers.filterNot { child =>
  //        (child.knownEncodingCharset == parentElem.knownEncodingCharset) && child.isScannable
  //      }
  //      unScannableChildren.length == 0
  //    }
  //  }

}

final class GlobalComplexTypeDefFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SchemaComponentFactory(xmlArg, schemaDocumentArg)
  with GlobalNonElementComponentMixin {

  def forElement(elementDecl: ElementDeclMixin) = new GlobalComplexTypeDef(xml, schemaDocument, elementDecl)

  override lazy val namedQName = QName.createGlobal(name, targetNamespace, xml.scope)
}

/**
 * For unit testing purposes, the element argument might be supplied as null.
 */
final class GlobalComplexTypeDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, override val elementDecl: ElementDeclMixin)
  extends ComplexTypeBase(xmlArg, schemaDocumentArg)
  with GlobalNonElementComponentMixin
  with NestingTraversesToReferenceMixin {

  override lazy val referringComponent = Option(elementDecl)

}

final class LocalComplexTypeDef(xmlArg: Node, override val elementDecl: ElementDeclMixin)
  extends ComplexTypeBase(xmlArg, elementDecl)
  with LocalNonElementComponentMixin
  with NestingLexicalMixin {
  // nothing
}
