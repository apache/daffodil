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

package edu.illinois.ncsa.daffodil.dpath

import AsIntConverters._
import java.lang.{ Number => JNumber }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }

case object PlusDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = { asBigDecimal(v1).add( asBigDecimal(v2)) }
}
case object MinusDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = { asBigDecimal(v1).subtract( asBigDecimal(v2)) }
}
case object TimesDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = { asBigDecimal(v1).multiply( asBigDecimal(v2)) }
}
case object DivDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = { asBigDecimal(v1).divide( asBigDecimal(v2)) }
}
case object IDivDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = { asBigDecimal(v1).divide( asBigDecimal(v2)) }
}
case object ModDecimal extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigDecimal = { asBigDecimal(v1).remainder( asBigDecimal(v2)) }
}

case object PlusInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).add( asBigInt(v2)) }
}
case object MinusInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).subtract( asBigInt(v2)) }
}
case object TimesInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).multiply( asBigInt(v2)) }
}
case object DivInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).divide( asBigInt(v2)) }
}
case object IDivInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).divide( asBigInt(v2)) }
}
case object ModInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).mod(asBigInt(v2))}
}

case object PlusNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).add( asBigInt(v2)) }
}
case object MinusNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).subtract(asBigInt(v2)) }
}
case object TimesNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).multiply( asBigInt(v2)) }
}
case object DivNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).divide( asBigInt(v2)) }
}
case object IDivNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).divide( asBigInt(v2)) }
}
case object ModNonNegativeInteger extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).mod(asBigInt(v2)) }
}

case object PlusUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).add(asBigInt(v2)) }
}
case object MinusUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).subtract( asBigInt(v2)) }
}
case object TimesUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).multiply( asBigInt(v2)) }
}
case object DivUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).divide( asBigInt(v2)) }
}
case object IDivUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).divide( asBigInt(v2)) }
}
case object ModUnsignedLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JBigInt = { asBigInt(v1).mod(asBigInt(v2)) }
}

case object PlusLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) + asLong(v2) }
}
case object MinusLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) - asLong(v2) }
}
case object TimesLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) * asLong(v2) }
}
case object DivLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) / asLong(v2) }
}
case object IDivLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) / asLong(v2) }
}
case object ModLong extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) % asLong(v2) }
}

case object PlusUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) + asLong(v2) }
}
case object MinusUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) - asLong(v2) }
}
case object TimesUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) * asLong(v2) }
}
case object DivUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) / asLong(v2) }
}
case object IDivUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) / asLong(v2) }
}
case object ModUnsignedInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asLong(v1) % asLong(v2) }
}

case object PlusInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) + asInt(v2) }
}
case object MinusInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) - asInt(v2) }
}
case object TimesInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) * asInt(v2) }
}
case object DivInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) / asInt(v2) }
}
case object IDivInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) / asInt(v2) }
}
case object ModInt extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) % asInt(v2) }
}

case object PlusUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) + asInt(v2) }
}
case object MinusUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) - asInt(v2) }
}
case object TimesUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) * asInt(v2) }
}
case object DivUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) / asInt(v2) }
}
case object IDivUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) / asInt(v2) }
}
case object ModUnsignedShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asInt(v1) % asInt(v2) }
}

case object PlusShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) + asShort(v2) }
}
case object MinusShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) - asShort(v2) }
}
case object TimesShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) * asShort(v2) }
}
case object DivShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) / asShort(v2) }
}
case object IDivShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) / asShort(v2) }
}
case object ModShort extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) % asShort(v2) }
}

case object PlusUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) + asShort(v2) }
}
case object MinusUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) - asShort(v2) }
}
case object TimesUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) * asShort(v2) }
}
case object DivUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) / asShort(v2) }
}
case object IDivUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) / asShort(v2) }
}
case object ModUnsignedByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asShort(v1) % asShort(v2) }
}

case object PlusByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(v1) + asByte(v2) }
}
case object MinusByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(v1) - asByte(v2) }
}
case object TimesByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(v1) * asByte(v2) }
}
case object DivByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(v1) / asByte(v2) }
}
case object IDivByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(v1) / asByte(v2) }
}
case object ModByte extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asByte(v1) % asByte(v2) }
}

case object PlusFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) + asFloat(v2) }
}
case object MinusFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) - asFloat(v2) }
}
case object TimesFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) * asFloat(v2) }
}
case object DivFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) / asFloat(v2) }
}
case object IDivFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) / asFloat(v2) }
}
case object ModFloat extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asFloat(v1) % asFloat(v2) }
}

case object PlusDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) + asDouble(v2) }
}
case object MinusDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) - asDouble(v2) }
}
case object TimesDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) * asDouble(v2) }
}
case object DivDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) / asDouble(v2) }
}
case object IDivDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) / asDouble(v2) }
}
case object ModDouble extends NumericOp {
  def operate(v1: JNumber, v2: JNumber): JNumber = { asDouble(v1) % asDouble(v2) }
}
