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

package edu.illinois.ncsa.daffodil.grammar.primitives

import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.parsers.SimpleNilOrEmptyOrValueParser
import edu.illinois.ncsa.daffodil.processors.unparsers.SimpleNilOrEmptyOrValueUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.SimpleNilOrValueParser
import edu.illinois.ncsa.daffodil.processors.unparsers.SimpleNilOrValueUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.SimpleEmptyOrValueParser
import edu.illinois.ncsa.daffodil.processors.unparsers.SimpleEmptyOrValueUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.ComplexNilOrContentParser
import edu.illinois.ncsa.daffodil.processors.unparsers.ComplexNilOrContentUnparser
import edu.illinois.ncsa.daffodil.grammar.Terminal

case class SimpleNilOrEmptyOrValue(ctxt: ElementBase, nilGram: Gram, emptyGram: Gram, valueGram: Gram) extends Terminal(ctxt, true) {
  Assert.invariant(!nilGram.isEmpty)
  Assert.invariant(!emptyGram.isEmpty)
  Assert.invariant(!valueGram.isEmpty)

  lazy val nilParser = nilGram.parser
  lazy val emptyParser = emptyGram.parser
  lazy val valueParser = valueGram.parser

  lazy val nilUnparser = nilGram.unparser
  lazy val emptyUnparser = emptyGram.unparser
  lazy val valueUnparser = valueGram.unparser

  override lazy val parser = SimpleNilOrEmptyOrValueParser(ctxt.erd, nilParser, emptyParser, valueParser)

  override lazy val unparser = SimpleNilOrEmptyOrValueUnparser(ctxt.erd, nilUnparser, emptyUnparser, valueUnparser)

}

case class SimpleNilOrValue(ctxt: ElementBase, nilGram: Gram, valueGram: Gram) extends Terminal(ctxt, true) {
  Assert.invariant(!nilGram.isEmpty)
  Assert.invariant(!valueGram.isEmpty)

  lazy val nilParser = nilGram.parser
  lazy val valueParser = valueGram.parser

  lazy val nilUnparser = nilGram.unparser
  lazy val valueUnparser = valueGram.unparser

  override lazy val parser = SimpleNilOrValueParser(ctxt.erd, nilParser, valueParser)

  override lazy val unparser = SimpleNilOrValueUnparser(ctxt.erd, nilUnparser, valueUnparser)

}

case class SimpleEmptyOrValue(ctxt: ElementBase, emptyGram: Gram, valueGram: Gram) extends Terminal(ctxt, true) {
  Assert.invariant(!emptyGram.isEmpty)
  Assert.invariant(!valueGram.isEmpty)

  lazy val emptyParser = emptyGram.parser
  lazy val valueParser = valueGram.parser

  lazy val emptyUnparser = emptyGram.unparser
  lazy val valueUnparser = valueGram.unparser

  override lazy val parser = SimpleEmptyOrValueParser(ctxt.erd, emptyParser, valueParser)

  override lazy val unparser = SimpleEmptyOrValueUnparser(ctxt.erd, emptyUnparser, valueUnparser)

}

case class ComplexNilOrContent(ctxt: ElementBase, nilGram: Gram, contentGram: Gram) extends Terminal(ctxt, true) {
  Assert.invariant(!nilGram.isEmpty)
  Assert.invariant(!contentGram.isEmpty)

  lazy val nilParser = nilGram.parser
  lazy val contentParser = contentGram.parser

  lazy val nilUnparser = nilGram.unparser
  lazy val contentUnparser = contentGram.unparser

  override lazy val parser = ComplexNilOrContentParser(ctxt.erd, nilParser, contentParser)

  override lazy val unparser = ComplexNilOrContentUnparser(ctxt.erd, nilUnparser, contentUnparser)

}
