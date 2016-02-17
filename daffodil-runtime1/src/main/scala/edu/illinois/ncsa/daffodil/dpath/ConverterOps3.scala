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

import scala.math.BigDecimal.long2bigDecimal
import scala.math.BigInt.long2bigInt

import AsIntConverters.asBigDecimal
import AsIntConverters.asBigInt
import AsIntConverters.asInt
import AsIntConverters.asLong
import edu.illinois.ncsa.daffodil.util.Misc
import java.lang.{ Byte => JByte, Short => JShort, Integer => JInt, Long => JLong, Float => JFloat, Double => JDouble, Boolean => JBoolean }
import AsIntConverters._

case object NumericToString extends ToString
case object DateTimeToString extends ToString
case object HexBinaryToString extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val bytes = a.asInstanceOf[Array[Byte]]
    val hex = Misc.bytes2Hex(bytes)
    hex
  }
}
case object HexStringToLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val res =
      try {
        val str = a.asInstanceOf[String]
        java.lang.Long.parseLong(str, 16)
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type long: " + nfe.getMessage())
          throw e
        }
      }
    asAnyRef(res)
  }
}
case object HexStringToUnsignedLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val res =
      try {
        val str = a.asInstanceOf[String]
        BigInt(str, 16)
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type unsignedLong: " + nfe.getMessage())
          throw e
        }
      }
    res
  }
}
case object BigIntToLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val res = asBigInt(a)
    if (res < Long.MinValue || res > Long.MaxValue) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    asLong(res)
  }
}
case object IntToLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = asLong(a)
}
case object UnsignedLongToLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val res = asBigInt(a)
    if (res < Long.MinValue || res > Long.MaxValue) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    asLong(res)
  }
}
case object UnsignedIntToLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    // Assert.invariant(a.isInstanceOf[Long])
    asLong(a)
  }
}
case object ArrayIndexToLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    asLong(a)
  }
}
case object ShortToLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = asLong(a)
}
case object UnsignedShortToLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = asLong(a)
}
case object ByteToLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = asLong(a)
}
case object UnsignedByteToLong extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = asLong(a)
}
