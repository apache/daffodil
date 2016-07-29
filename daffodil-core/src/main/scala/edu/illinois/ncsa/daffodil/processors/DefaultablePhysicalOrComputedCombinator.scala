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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.Found
import edu.illinois.ncsa.daffodil.dsom.NotFound

case class DefaultablePhysicalOrComputed(ctxt: ElementBase,
  scalarDefaultablePhysical: Gram,
  inputValueCalcElement: Gram,
  outputValueCalcElement: Gram,
  defaultableElement: Gram)
  extends Terminal(ctxt, true) {
  //Assert.invariant(!scalarDefaultablePhysical.isEmpty)
  //Assert.invariant(!inputValueCalcElement.isEmpty)
  //Assert.invariant(!outputValueCalcElement.isEmpty)
  //Assert.invariant(!defaultableElement.isEmpty) // NYI? So this will be empty?

  lazy val scalarDefaultablePhysicalParser = scalarDefaultablePhysical.parser
  lazy val inputValueCalcElementParser = inputValueCalcElement.parser

  lazy val inputValueCalcElementUnparser =
    inputValueCalcElement.unparser
  lazy val scalarDefaultablePhysicalUnparser = scalarDefaultablePhysical.unparser
  lazy val outputValueCalcElementUnparser = outputValueCalcElement.unparser

  override lazy val parser = {
    (ctxt.inputValueCalcOption, ctxt.outputValueCalcOption) match {
      case (_: NotFound, _: Found) => scalarDefaultablePhysicalParser // outputValueCalc element is just a regular physical element for parser
      case (_: Found, _: NotFound) => inputValueCalcElementParser
      case _ => Assert.impossibleCase()
    }
  }

  override lazy val unparser = {
    val ivcOpt = ctxt.inputValueCalcOption
    val ovcOpt = ctxt.outputValueCalcOption
    (ivcOpt, ovcOpt) match {
      case (_: NotFound, _: Found) => outputValueCalcElementUnparser
      // when unparsing, inputValueCalc elements don't contribute to the data.
      // They may get referenced from outputValueCalc or other expressions so their
      // element values may need to be in the infoset
      case (_: Found, _: NotFound) =>
        inputValueCalcElementUnparser
      case _ => scalarDefaultablePhysicalUnparser
    }
  }

}
