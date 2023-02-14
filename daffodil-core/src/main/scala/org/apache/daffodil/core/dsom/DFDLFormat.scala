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

import scala.xml.Node

object DFDLFormat {
  def apply(node: Node, sd: SchemaDocument) = {
    val df = new DFDLFormat(node, sd)
    df.initialize()
    df
  }
}

final class DFDLFormat private (node: Node, sd: SchemaDocument)
  extends DFDLFormatAnnotation(node, sd)
// leave the below comments here for a while. (In case we have to reproduce
// this list of mixins on a schema component somewhere)
//  with Format_AnnotationMixin
//  with NillableMixin
//  with SeparatorSuppressionPolicyMixin
//  with RawElementRuntimeValuedPropertiesMixin
//  with RawSequenceRuntimeValuedPropertiesMixin

abstract class DFDLNonDefaultFormatAnnotation(node: Node, sc: AnnotatedSchemaComponent)
  extends DFDLFormatAnnotation(node, sc)

final class DFDLElement(node: Node, decl: ElementLikeMixin)
  extends DFDLNonDefaultFormatAnnotation(node, decl)

final class DFDLGroup(node: Node, decl: ModelGroup)
  extends DFDLNonDefaultFormatAnnotation(node, decl)

abstract class DFDLModelGroup(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLNonDefaultFormatAnnotation(node, decl)

final class DFDLSequence(node: Node, decl: SequenceDefMixin) extends DFDLModelGroup(node, decl)

final class DFDLChoice(node: Node, decl: ChoiceDefMixin) extends DFDLModelGroup(node, decl)

final class DFDLSimpleType(node: Node, decl: SimpleTypeDefBase)
  extends DFDLNonDefaultFormatAnnotation(node, decl) {}

final class DFDLEnumerationFactory(node: Node, decl: EnumerationDef)
  extends DFDLFormatAnnotation(node, decl)

// Leave the below comments in place. These are not reproduced (currently)
// on SimpleTypeDefBase. It appears these properties are only accessed
// indirectly by way of an ElementDecl that uses this type.
//
//  with SimpleType_AnnotationMixin
//  with TextNumberFormatMixin
//  with StringTextMixin
//  with NumberTextMixin
//  with CalendarTextMixin
//  with BooleanTextMixin
//  with RawSimpleTypeRuntimeValuedPropertiesMixin
