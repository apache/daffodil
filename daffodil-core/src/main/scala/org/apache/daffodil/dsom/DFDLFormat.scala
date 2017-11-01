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

package org.apache.daffodil.dsom

import scala.xml.Node

final class DFDLFormat(node: Node, sd: SchemaDocument)
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

final class DFDLElement(node: Node, decl: ElementBase)
  extends DFDLNonDefaultFormatAnnotation(node, decl)

final class DFDLGroup(node: Node, decl: GroupBase)
  extends DFDLNonDefaultFormatAnnotation(node, decl)

abstract class DFDLModelGroup(node: Node, decl: ModelGroup)
  extends DFDLNonDefaultFormatAnnotation(node, decl)

final class DFDLSequence(node: Node, decl: Sequence)
  extends DFDLModelGroup(node, decl)

final class DFDLChoice(node: Node, decl: Choice)
  extends DFDLModelGroup(node, decl)

final class DFDLSimpleType(node: Node, decl: SimpleTypeDefBase)
  extends DFDLNonDefaultFormatAnnotation(node, decl) {
}
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
