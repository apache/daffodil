package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.util.{ Debug, LogLevel, Logging, Info }
import edu.illinois.ncsa.daffodil.processors.xpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.grammar.Gram
import scala.collection.mutable.Stack
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar.UnaryGram
import edu.illinois.ncsa.daffodil.processors.parsers.ComplexTypeParser
import edu.illinois.ncsa.daffodil.processors.parsers.SequenceCombinatorParser
import edu.illinois.ncsa.daffodil.processors.parsers.ArrayCombinatorParser


case class ComplexTypeCombinator(ct: ComplexTypeBase, body: Gram) extends Terminal(ct.element, !body.isEmpty) {

  def parser: DaffodilParser = new ComplexTypeParser(ct, body)

  def unparser: Unparser = new Unparser(ct.element) {
    override def toString = "ComplexType"

    val bodyUnparser = body.unparser

    def unparse(start: UState): UState = {
      //start.mpstate.childIndexStack.push(1L)
      //val parseState = bodyUnparser.unparse(start)
      //start.mpstate.childIndexStack.pop()
      start
    }
  }
}

case class SequenceCombinator(sq: Sequence, body: Gram) extends Terminal(sq, !body.isEmpty) {

  def parser: DaffodilParser = new SequenceCombinatorParser(body, sq)
  
  def unparser: Unparser = new Unparser(sq) {
    override def toString = "Sequence"

    def unparse(start: UState): UState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }
}

case class ArrayCombinator(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {

  def parser: DaffodilParser = new ArrayCombinatorParser(body, e)

  def unparser: Unparser = new Unparser(e) {
    override def toString = "Array"

    val bodyUnparser = body.unparser

    def unparse(start: UState): UState = {
      val preState = start.withArrayIndexStack(1L :: start.arrayIndexStack)

      val unparseState = bodyUnparser.unparse(preState)
      if (unparseState.status != Success) return unparseState

      val postState = unparseState.withArrayIndexStack(unparseState.arrayIndexStack.tail)
      postState
    }
  }
}


