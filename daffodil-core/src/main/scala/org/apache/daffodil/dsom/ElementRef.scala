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
import org.apache.daffodil.grammar._
import org.apache.daffodil.xml._
import org.apache.daffodil.grammar.ElementReferenceGrammarMixin
import org.apache.daffodil.dpath.NodeInfo

/**
 * There are 3 first-class concrete children of ElementBase.
 * Root, LocalElementDecl, and ElementRef
 */
final class ElementRef(xmlArg: Node, parent: GroupDefLike, position: Int)
  extends AbstractElementRef(xmlArg, parent, position)

abstract class AbstractElementRef(xmlArg: Node,
  parentArg: SchemaComponent,
  positionArg: Int)
  extends ElementBase
  with ElementReferenceGrammarMixin
  with HasRefMixin
  with NamedMixin
  with NestingLexicalMixin {

  override lazy val xml = xmlArg
  final override lazy val parent = parentArg
  final override lazy val position = positionArg

  requiredEvaluations(referencedElement)

  def complexType: ComplexTypeBase = this.referencedElement.complexType
  def defaultValueAsString: String = this.referencedElement.defaultValueAsString
  def hasDefaultValue: Boolean = this.referencedElement.hasDefaultValue
  def isComplexType: Boolean = this.referencedElement.isComplexType
  def isNillable: Boolean = this.referencedElement.isNillable
  def isSimpleType: Boolean = this.referencedElement.isSimpleType
  def simpleType: SimpleTypeBase = this.referencedElement.simpleType
  def primType: NodeInfo.PrimType = this.referencedElement.primType

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
      case Some(x) => x.forElementRef(this)
    }
    res
  }.value

  override lazy val namespace = refQName.namespace

  override lazy val diagnosticDebugName = "element reference " + refQName

  override def typeDef = referencedElement.typeDef

}
