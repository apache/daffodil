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

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc
import java.lang.{ Number => JNumber, Byte => JByte, Short => JShort, Integer => JInt, Long => JLong, Float => JFloat, Double => JDouble, Boolean => JBoolean }
import java.math.{ BigInteger => JBigInt, BigDecimal => JBigDecimal }

object AsIntConverters {

  /**
   * Parsers don't always insert the smallest numeric type into the infoset.
   * Sometimes we get a BigInt when an Int would have sufficed, but the
   * parsers don't always do that. This is a workaround. Really the parsers
   * should be inserting the *right thing* into the infoset.
   */
  def asInt(n: AnyRef): JInt = {
    val value = n match {
      case b: JByte => b.toInt
      case s: JShort => s.toInt
      case i: JInt => return i
      case l: JLong => l.toInt
      case bi: BigInt => bi.toInt
      case bd: BigDecimal => bd.toInt
      case bi: JBigInt => bi.intValue()
      case bd: JBigDecimal => bd.intValue()
      case _ => Assert.invariantFailed("Unsupported conversion to Int. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    new JInt(value)
  }
  def asByte(n: AnyRef): JByte = {
    val value = n match {
      case b: JByte => return b
      case s: JShort => s.toByte
      case i: JInt => i.toByte
      case l: JLong => l.toByte
      case bi: BigInt => bi.toByte
      case bd: BigDecimal => bd.toByte
      case bi: JBigInt => bi.byteValue()
      case bd: JBigDecimal => bd.byteValue()
      case _ => Assert.invariantFailed("Unsupported conversion to Byte. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    new JByte(value)
  }
  def asShort(n: AnyRef): JShort = {
    val value = n match {
      case b: JByte => b.toShort
      case s: JShort => return s
      case i: JInt => i.toShort
      case l: JLong => l.toShort
      case bi: BigInt => bi.toShort
      case bd: BigDecimal => bd.toShort
      case bi: JBigInt => bi.shortValue()
      case bd: JBigDecimal => bd.shortValue()
      case _ => Assert.invariantFailed("Unsupported conversion to Short. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    new JShort(value)
  }

  def asLong(n: AnyRef): JLong = {
    val value = n match {
      case b: JByte => b.toLong
      case s: JShort => s.toLong
      case i: JInt => i.toLong
      case d: JDouble => d.toLong
      case f: JFloat => f.toLong
      case l: JLong => return l
      case bi: BigInt => bi.toLong
      case bd: BigDecimal => bd.toLong
      case bi: JBigInt => bi.longValue()
      case bd: JBigDecimal => bd.longValue()
      case _ => Assert.invariantFailed("Unsupported conversion to Long. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    new JLong(value)
  }

  def asBigInt(n: AnyRef): BigInt = {
    val value: BigInt = n match {
      case b: JBigInt => BigInt(b)
      case bd: JBigDecimal => BigInt(bd.toBigInteger())
      case d: JDouble => BigDecimal(d).toBigInt
      case f: JFloat => BigDecimal(f.toDouble).toBigInt
      case bi: BigInt => bi
      case bd: BigDecimal => bd.toBigInt
      // the rest of the JNumbers are integers long or smaller.
      case jn: JNumber => BigInt(jn.longValue())
      case _ => Assert.invariantFailed("Unsupported conversion to BigInt. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asJBigInt(n: AnyRef): JBigInt = {
    val value: JBigInt = n match {
      case b: JBigInt => b
      case bd: JBigDecimal => bd.toBigInteger()
      case d: JDouble => BigDecimal(d).toBigInt.bigInteger
      case f: JFloat => BigDecimal(f.toDouble).toBigInt.bigInteger
      case bi: BigInt => bi.bigInteger
      case bd: BigDecimal => bd.toBigInt.bigInteger
      // the rest of the JNumbers are integers long or smaller.
      case jn: JNumber => JBigInt.valueOf(jn.longValue())
      case _ => Assert.invariantFailed("Unsupported conversion to BigInt. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asFloat(n: AnyRef): JFloat = {
    val value = n match {
      case f: JFloat => return f
      case d: JDouble => d.toFloat
      case b: JByte => b.toFloat
      case s: JShort => s.toFloat
      case i: JInt => i.toFloat
      case l: JLong => l.toFloat
      case bi: BigInt => bi.toFloat
      case bd: BigDecimal => bd.toFloat
      case bi: JBigInt => bi.floatValue()
      case bd: JBigDecimal => bd.floatValue()
      case _ => Assert.invariantFailed("Unsupported conversion to Float. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    new JFloat(value)
  }

  def asDouble(n: AnyRef): JDouble = {
    val value = n match {
      case f: JFloat => f.toDouble
      case d: JDouble => return d
      case b: JByte => b.toDouble
      case s: JShort => s.toDouble
      case i: JInt => i.toDouble
      case l: JLong => l.toDouble
      case bi: BigInt => bi.toDouble
      case bd: BigDecimal => bd.toDouble
      case bi: JBigInt => bi.doubleValue()
      case bd: JBigDecimal => bd.doubleValue()
      case _ => Assert.invariantFailed("Unsupported conversion to Double. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    new JDouble(value)
  }

  def asBigDecimal(n: AnyRef): BigDecimal = {
    val value = n match {
      //
      // Not converting Float to string first causes precision issues
      // that round-half-to-even doesn't resolve correctly.  BigDecimal.valueOf(3.455) turns into 3.454999.
      // HALF_EVEN rounding mode would round this to 3.45 rather than the desired 3.46.
      case f: JFloat => BigDecimal(f.toString())
      case d: JDouble => BigDecimal.valueOf(d)
      case bi: BigInt => BigDecimal(bi)
      case bd: BigDecimal => bd
      case bi: JBigInt => BigDecimal(bi)
      case bd: JBigDecimal => BigDecimal(bd)
      // The rest of the cases are integers long or smaller
      case jn: JNumber => BigDecimal.valueOf(jn.longValue())
      case _ => Assert.invariantFailed("Unsupported conversion to BigDecimal. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asJBigDecimal(n: AnyRef): JBigDecimal = {
    val value: JBigDecimal = n match {
      //
      // Not converting Float to string first causes precision issues
      // that round-half-to-even doesn't resolve correctly.  BigDecimal.valueOf(3.455) turns into 3.454999.
      // HALF_EVEN rounding mode would round this to 3.45 rather than the desired 3.46.
      case f: JFloat => new JBigDecimal(f.toString)
      case d: JDouble => BigDecimal.valueOf(d).bigDecimal
      case bi: BigInt => BigDecimal(bi).bigDecimal
      case bd: BigDecimal => bd.bigDecimal
      case bi: JBigInt => new JBigDecimal(bi)
      case bd: JBigDecimal => bd
      // The rest of the cases are integers long or smaller
      case jn: JNumber => JBigDecimal.valueOf(jn.longValue())
      case _ => Assert.invariantFailed("Unsupported conversion to BigDecimal. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asBoolean(n: Any): JBoolean = {
    n match {
      case bool: JBoolean => return bool
      case b: Boolean => new JBoolean(b)
      case _ => Assert.invariantFailed("Unsupported conversion to Boolean. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
  }

  def asNumber(n: Any): JNumber = {
    n match {
      case b: Byte => new JByte(b)
      case s: Short => new JShort(s)
      case i: Int => new JInt(i)
      case l: Long => new JLong(l)
      case f: Float => new JFloat(f)
      case d: Double => new JDouble(d)
      // case bi: BigInt => bi.bigInteger
      // case bd: BigDecimal => bd.bigDecimal
      case jn: JNumber => jn
      case _ => Assert.invariantFailed("Unsupported conversion to Number. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
  }

  def asAnyRef(n: Any): AnyRef = {
    n match {
      // case bi: BigInt => bi.bigInteger
      // case bd: BigDecimal => bd.bigDecimal
      case ar: AnyRef => ar
      case b: Boolean => new JBoolean(b)
      case _ => asNumber(n)
    }
  }
}
