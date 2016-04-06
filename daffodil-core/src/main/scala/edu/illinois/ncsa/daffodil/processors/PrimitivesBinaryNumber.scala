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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.parsers.BinaryIntegerRuntimeLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.BinaryIntegerKnownLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.BinaryDecimalRuntimeLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.BinaryDecimalKnownLengthParser
import edu.illinois.ncsa.daffodil.processors.parsers.BinaryFloatParser
import edu.illinois.ncsa.daffodil.processors.parsers.BinaryDoubleParser
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.processors.unparsers.DummyUnparser
import edu.illinois.ncsa.daffodil.util.Misc

class BinaryIntegerRuntimeLength(val e: ElementBase, signed: Boolean) extends Terminal(e, true) {
  override lazy val parser = new BinaryIntegerRuntimeLengthParser(e.elementRuntimeData, signed, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = DummyUnparser(Misc.getNameFromClass(this))
}

class BinaryIntegerKnownLength(val e: ElementBase, signed: Boolean, lengthInBits: Long) extends Terminal(e, true) {
  override lazy val parser = new BinaryIntegerKnownLengthParser(e.elementRuntimeData, signed, lengthInBits.toInt)

  override lazy val unparser: Unparser = DummyUnparser(Misc.getNameFromClass(this))
}

class BinaryDecimalRuntimeLength(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new BinaryDecimalRuntimeLengthParser(e.elementRuntimeData, e.decimalSigned, e.binaryDecimalVirtualPoint, e.lengthEv, e.lengthUnits)

  override lazy val unparser: Unparser = DummyUnparser(Misc.getNameFromClass(this))
}

class BinaryDecimalKnownLength(val e: ElementBase, lengthInBits: Long) extends Terminal(e, true) {
  override lazy val parser = new BinaryDecimalKnownLengthParser(e.elementRuntimeData, e.decimalSigned, e.binaryDecimalVirtualPoint, lengthInBits.toInt)

  override lazy val unparser: Unparser = DummyUnparser(Misc.getNameFromClass(this))
}

class BinaryFloat(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new BinaryFloatParser(e.elementRuntimeData)

  override lazy val unparser: Unparser = DummyUnparser(Misc.getNameFromClass(this))
}

class BinaryDouble(val e: ElementBase) extends Terminal(e, true) {
  override lazy val parser = new BinaryDoubleParser(e.elementRuntimeData)

  override lazy val unparser: Unparser = DummyUnparser(Misc.getNameFromClass(this))
}
