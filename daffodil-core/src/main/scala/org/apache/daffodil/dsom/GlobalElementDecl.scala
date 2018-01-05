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

final class GlobalElementDecl(
  xmlArg: Node,
  schemaDocument: SchemaDocument,
  elementRefArg: => AbstractElementRef)
  extends AnnotatedSchemaComponentImpl(xmlArg, schemaDocument)
  with GlobalElementComponentMixin
  with ElementDeclMixin
  with NestingTraversesToReferenceMixin {
  //   global elements combined with element references referring to them can
  //   be multiple occurring (aka arrays) hence, we have to have things
  //   that take root and referenced situation into account.

  lazy val elementRef = elementRefArg
  override lazy val dpathCompileInfo = elementRef.dpathElementCompileInfo

  requiredEvaluations(validateChoiceBranchKey)

  override lazy val referringComponent: Option[SchemaComponent] = Some(elementRef) // optElementRef

  def validateChoiceBranchKey(): Unit = {
    // Ensure that the global element decl does not have choiceBranchKey set.
    // We must use findPropertyOptionThisComponentOnly rather than
    // findPropertyOption, since the later will also inspect the element ref.
    // The element ref is allowed to have the dfdl:choiceBranchKey option, so
    // we must not inspect it.
    val found = findPropertyOptionThisComponentOnly("choiceBranchKey")
    if (found.isDefined) {
      SDE("dfdl:choiceBranchKey cannot be specified on a global element declaration")
    }
  }
}
