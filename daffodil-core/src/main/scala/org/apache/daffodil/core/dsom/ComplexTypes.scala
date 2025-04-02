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

import scala.xml.Comment
import scala.xml.Elem
import scala.xml.Node
import scala.xml.Text

import org.apache.daffodil.core.dsom.walker.ComplexTypeView
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.runtime1.dpath.NodeInfo

sealed abstract class ComplexTypeBase(xmlArg: Node, parentArg: SchemaComponent)
  extends SchemaComponentImpl(xmlArg, parentArg)
  with TypeBase
  with NonPrimTypeMixin
  with ComplexTypeView {

  final override def optRestriction = None
  final override def optUnion = None
  final override def typeNode = NodeInfo.Complex

  override final def group: ModelGroup = modelGroup

  /**
   * Convenience methods for unit testing. Just makes tests a bit more compact and clearer.
   */
  final def sequence = group.asInstanceOf[LocalSequence]
  final def choice = group.asInstanceOf[Choice]

  private lazy val Elem(_, "complexType", _, _, xmlChildren @ _*) = xml

  final lazy val Seq(modelGroup) = {
    val s = smg
    schemaDefinitionUnless(
      s.length == 1,
      "A complex type must have exactly one model-group element child which is a sequence, choice, or group reference."
    )
    s.head match {
      case sgtb: SequenceGroupTermBase =>
        sgtb.schemaDefinitionWhen(
          sgtb.isHidden,
          "A complex type cannot have a sequence with a hiddenGroupRef as its model group. " +
            "Wrap the hiddenGroupRef sequence in an empty sequence instead."
        )
      case _ => // do nothing
    }
    s
  }

  private lazy val smg = {
    childrenForTerms.map { xmlChild =>
      ModelGroupFactory(xmlChild, this, 1, isHidden = false)
    }
  }

  private lazy val childrenForTerms = {
    xmlChildren.flatMap { xmlChild =>
      {
        xmlChild match {
          case Elem(_, "annotation", _, _, annotationChildren @ _*) => {
            val dais = annotationChildren.find { ai =>
              ai.attribute("source") match {
                case Some(n) => n.text.contains("ogf") && n.text.contains("dfdl")
                case _ => false
              }
            }
            if (dais != None) {
              this.SDW(
                WarnID.InvalidAnnotationPoint,
                "complexType is not a valid annotation point. Annotation ignored."
              )
            }
            None
          }
          case textNode: Text => None
          case _: Comment => None
          case _ => Some(xmlChild)
        }
      }
    }
  }
}

object GlobalComplexTypeDef {
  def apply(xmlArg: Node, schemaDocumentArg: SchemaDocument) = {
    val gctd = new GlobalComplexTypeDef(xmlArg, schemaDocumentArg)
    gctd.initialize()
    gctd
  }
}

final class GlobalComplexTypeDef private (xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends ComplexTypeBase(xmlArg, schemaDocumentArg)
  with GlobalNonElementComponentMixin
  with NestingLexicalMixin {
  // Nothing needed here. The base class and mixins are providing all the functionality needed.
}

final class LocalComplexTypeDef(xmlArg: Node, val elementDecl: ElementDeclMixin)
  extends ComplexTypeBase(xmlArg, elementDecl)
  with LocalNonElementComponentMixin
  with NestingLexicalMixin {
  // Nothing needed here. The base class and mixins are providing all the functionality needed.
}
