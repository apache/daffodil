/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.Evaluatable
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo
import java.lang.{ Long => JLong, Number => JNumber, Double => JDouble, Float => JFloat }
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState

class BinaryFloatParser(e: ElementRuntimeData)
  extends PrimParserObject(e) {

  def parse(start: PState): Unit = {
    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(32)) {
      PE(start, "Insufficient bits in data. Needed %d bit(s) but found only %d available.", 32, dis.remainingBits.get)
      return
    }

    val f: JFloat = dis.getBinaryFloat()
    start.simpleElement.overwriteDataValue(f)
  }
}

class BinaryDoubleParser(e: ElementRuntimeData)
  extends PrimParserObject(e) {

  def parse(start: PState): Unit = {
    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(64)) {
      PE(start, "Insufficient bits in data. Needed %d bit(s) but found only %d available.", 64, dis.remainingBits.get)
      return
    }

    val d: JDouble = dis.getBinaryDouble()
    start.simpleElement.overwriteDataValue(d)
  }
}

class BinaryDecimalKnownLengthParser(e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int, val lengthInBits: Int)
  extends BinaryDecimalParserBase(e, signed, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {
}

class BinaryDecimalRuntimeLengthParser(val e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int, val lengthEv: Evaluatable[JLong], val lUnits: LengthUnits)
  extends BinaryDecimalParserBase(e, signed, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {
}

abstract class BinaryDecimalParserBase(e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int)
  extends PrimParserObject(e) {

  protected def getBitLength(s: ParseOrUnparseState): Int

  def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(nBits)) {
      PE(start, "Insufficient bits in data. Needed %d bit(s) but found only %d available.", nBits, dis.remainingBits.get)
      return
    }

    val bigInt: BigInt =
      if (signed == YesNo.Yes) { dis.getSignedBigInt(nBits) }
      else { dis.getUnsignedBigInt(nBits) }

    val bigDec = BigDecimal(bigInt, binaryDecimalVirtualPoint)
    start.simpleElement.overwriteDataValue(bigDec)
  }
}

class BinaryIntegerRuntimeLengthParser(val e: ElementRuntimeData, signed: Boolean, val lengthEv: Evaluatable[JLong], val lUnits: LengthUnits)
  extends BinaryIntegerBaseParser(e, signed)
  with HasRuntimeExplicitLength {
}

class BinaryIntegerKnownLengthParser(e: ElementRuntimeData, signed: Boolean, val lengthInBits: Int)
  extends BinaryIntegerBaseParser(e, signed)
  with HasKnownLengthInBits {
}

abstract class BinaryIntegerBaseParser(e: ElementRuntimeData, signed: Boolean)
  extends PrimParserObject(e) {

  protected def getBitLength(s: ParseOrUnparseState): Int

  def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    if (nBits == 0) return // zero length is used for outputValueCalc often.
    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(nBits)) {
      PE(start, "Insufficient bits in data. Needed %d bit(s) but found only %d available.", nBits, dis.remainingBits.get)
      return
    }

    val int: JNumber =
      if (signed) {
        if (nBits > 64) { dis.getSignedBigInt(nBits) }
        else { dis.getSignedLong(nBits) }
      } else {
        if (nBits >= 64) { dis.getUnsignedBigInt(nBits) }
        else { dis.getUnsignedLong(nBits).toLong }
      }

    start.simpleElement.overwriteDataValue(int)
  }
}
