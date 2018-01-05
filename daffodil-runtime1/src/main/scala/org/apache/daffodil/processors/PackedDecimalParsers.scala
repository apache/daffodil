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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.util.{ DecimalUtils, PackedSignCodes }
import java.lang.{ Long => JLong }
import java.math.{ BigInteger => JBigInteger, BigDecimal => JBigDecimal }

class PackedDecimalKnownLengthParser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  val lengthInBits: Int)
  extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.packedToBigInteger(num, packedSignCodes)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.packedToBigDecimal(num, scale, packedSignCodes)

}

class PackedDecimalRuntimeLengthParser(
  val e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes,
  val lengthEv: Evaluatable[JLong],
  val lUnits: LengthUnits)
  extends PackedBinaryDecimalBaseParser(e, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.packedToBigInteger(num, packedSignCodes)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.packedToBigDecimal(num, scale, packedSignCodes)

}

class PackedIntegerRuntimeLengthParser(
  val e: ElementRuntimeData,
  signed: Boolean,
  packedSignCodes: PackedSignCodes,
  val lengthEv: Evaluatable[JLong],
  val lUnits: LengthUnits)
  extends PackedBinaryIntegerBaseParser(e, signed)
  with HasRuntimeExplicitLength {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.packedToBigInteger(num, packedSignCodes)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.packedToBigDecimal(num, scale, packedSignCodes)

}

class PackedIntegerKnownLengthParser(
  e: ElementRuntimeData,
  signed: Boolean,
  packedSignCodes: PackedSignCodes,
  val lengthInBits: Int)
  extends PackedBinaryIntegerBaseParser(e, signed)
  with HasKnownLengthInBits {

  override def toBigInteger(num: Array[Byte]): JBigInteger = DecimalUtils.packedToBigInteger(num, packedSignCodes)
  override def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal = DecimalUtils.packedToBigDecimal(num, scale, packedSignCodes)

}
