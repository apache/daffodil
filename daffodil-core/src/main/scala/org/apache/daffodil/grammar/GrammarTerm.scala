/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.grammar

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.grammar.primitives.Nada
import org.apache.daffodil.dsom.SchemaComponent
import org.apache.daffodil.oolag.OOLAG.OOLAGHost
import org.apache.daffodil.compiler.ParserOrUnparser
import org.apache.daffodil.util.Misc
import org.apache.daffodil.compiler.BothParserAndUnparser

trait HasNoUnparser {
  final lazy val unparser: Unparser = hasNoUnparser
  private def hasNoUnparser = Assert.invariantFailed("no unparser for " + Misc.getNameFromClass(this))
}

/**
 * Gram - short for "Grammar Term"
 *
 * These are the objects in the grammar. The grammar is supposed to be
 * roughly the grammar in the DFDL specification, but some differences are expected
 * because this one has to actually be operationalized.
 */
abstract class Gram(contextArg: SchemaComponent)
  extends OOLAGHost(contextArg) {

  final def SDE(str: String, args: Any*): Nothing = context.SDE(str, args: _*)
  final def SDW(str: String, args: Any*): Unit = context.SDW(str, args: _*)

  val forWhat: ParserOrUnparser = BothParserAndUnparser

  final val context: SchemaComponent = contextArg

  /**
   * Some grammar terms (productions specifically) have an indirection
   * to the grammar term that is their definition.
   *
   * This would be protected, except it is used in unit testing.
   */
  private[grammar] def gram: Gram = this

  private[grammar] def deref = this // override in Prod

  def name: String = diagnosticDebugName

  override lazy val path = context.path + "%" + diagnosticDebugName

  def isEmpty = false // they are by default not empty. Overridden in the cases where they could be.

  /**
   * Sequential composition operator in the grammar.
   *
   * Note: This should not evaluate the argument unless it has to.
   */
  def ~(qq: => Gram) = {
    lazy val q = qq.deref
    val self = this.deref
    //
    // The Nada terminal also behaves like empty for sequential composition
    // It is not empty for alternative composition though.
    //
    val res =
      if (self.isEmpty || self.isInstanceOf[Nada]) {
        if (q.isEmpty || q.isInstanceOf[Nada]) // Nada might get through to this point. Let's optimize it out.
          EmptyGram
        else q
      } else if (q.isEmpty || q.isInstanceOf[Nada]) self
      else {
        Assert.invariant(!self.isInstanceOf[Nada])
        Assert.invariant(!self.isEmpty)
        Assert.invariant(!q.isInstanceOf[Nada])
        Assert.invariant(!q.isEmpty)
        SeqComp(context, self, q)
      }
    res
  }

  def |(qq: => Gram) = {
    lazy val q = qq.deref
    val self = this.deref
    if (self.isEmpty)
      if (q.isEmpty) EmptyGram
      else q
    else if (q.isEmpty) self
    else
      AltComp(context, self, q)
  }

  /**
   * This || operator means one of the operands ONLY. I.e., statically
   * they are supposed to be mutually exclusive, so only one (or none) of them should
   * ever survive.
   */
  def ||(qq: => Gram) = {
    lazy val q = qq.deref
    val self = this.deref
    if (self.isEmpty)
      if (q.isEmpty) EmptyGram
      else q
    else if (q.isEmpty) self
    else
      Assert.invariantFailed("More than one alternative for || survived in the grammar")
  }

  /**
   * Parser - a Gram can provide a parser, which... parses what the Gram describes
   */
  def parser: Parser

  protected final def hasNoParser: Parser = Assert.invariantFailed("Has no parser.")
  protected final def hasNoUnparser: Unparser = Assert.invariantFailed("Has no unparser.")

  def unparser: Unparser // = DummyUnparser(Misc.getNameFromClass(this)) // context.runtimeData

}
