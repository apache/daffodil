/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.lib.util

import java.lang.{ Boolean => JBoolean }
import java.lang.{ Byte => JByte }
import java.lang.{ Double => JDouble }
import java.lang.{ Float => JFloat }
import java.lang.{ Integer => JInt }
import java.lang.{ Long => JLong }
import java.lang.{ Number => JNumber }
import java.lang.{ Short => JShort }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.lib.exceptions.Assert

object Numbers extends Numbers {

  override protected def errorThrower(message: String): Nothing =
    Assert.invariantFailed(message)

  def isValidInt(n: Number): Boolean = {
    val res = n match {
      case j: JInt => true
      case j: JShort => true
      case j: JByte => true
      case j: JLong if (j.longValue == j.intValue()) => true
      case _ => {
        val bd = asBigDecimal(n)
        try {
          bd.intValueExact()
          true
        } catch {
          case e: java.lang.ArithmeticException => false
        }
      }
    }
    res
  }

  def isValidLong(n: Number): Boolean = {
    val res = n match {
      case j: JLong => true
      case j: JInt => true
      case j: JShort => true
      case j: JByte => true
      case _ => {
        val bd = asBigDecimal(n)
        try {
          bd.longValueExact()
          true
        } catch {
          case e: java.lang.ArithmeticException => false
        }
      }
    }
    res
  }

  /**
   * This is true only if converting to a double and back results
   * in an equal JBigDecimal.
   *
   * The scale matters. E.g., if you do:
   * {{{
   * val foo = new JBigDecimal("0.2")
   * val bar = new JBigDecimal("0.200000000000")
   * isDecimalDouble(foo) // true
   * isDecimalDouble(bar) // false
   * }}}
   */
  def isDecimalDouble(bd: JBigDecimal): Boolean = {
    val df = bd.doubleValue()
    if (df.isInfinity) false
    else {
      val d = JBigDecimal.valueOf(df)
      d.equals(bd)
    }
  }
}

trait Numbers {

  protected def errorThrower(message: String): Nothing

  def asInt(n: AnyRef): JInt = {
    val value = n match {
      case f: JFloat => f.toInt
      case d: JDouble => d.toInt
      case b: JByte => b.toInt
      case s: JShort => s.toInt
      case i: JInt => return i
      case l: JLong => l.toInt
      case bi: JBigInt => bi.intValue()
      case bd: JBigDecimal => bd.intValue()
      case _ =>
        errorThrower(
          "Unsupported conversion to Int. %s of type %s".format(n, Misc.getNameFromClass(n))
        )
    }
    JInt.valueOf(value)
  }
  def asByte(n: AnyRef): JByte = {
    val value = n match {
      case f: JFloat => f.toByte
      case d: JDouble => d.toByte
      case b: JByte => return b
      case s: JShort => s.toByte
      case i: JInt => i.toByte
      case l: JLong => l.toByte
      case bi: JBigInt => bi.byteValue()
      case bd: JBigDecimal => bd.byteValue()
      case _ =>
        errorThrower(
          "Unsupported conversion to Byte. %s of type %s".format(n, Misc.getNameFromClass(n))
        )
    }
    JByte.valueOf(value)
  }
  def asShort(n: AnyRef): JShort = {
    val value = n match {
      case f: JFloat => f.toShort
      case d: JDouble => d.toShort
      case b: JByte => b.toShort
      case s: JShort => return s
      case i: JInt => i.toShort
      case l: JLong => l.toShort
      case bi: JBigInt => bi.shortValue()
      case bd: JBigDecimal => bd.shortValue()
      case _ =>
        errorThrower(
          "Unsupported conversion to Short. %s of type %s".format(n, Misc.getNameFromClass(n))
        )
    }
    JShort.valueOf(value)
  }

  def asLong(n: AnyRef): JLong = {
    val value = n match {
      case b: JByte => b.toLong
      case s: JShort => s.toLong
      case i: JInt => i.toLong
      case d: JDouble => d.toLong
      case f: JFloat => f.toLong
      case l: JLong => return l
      case bi: JBigInt => bi.longValue()
      case bd: JBigDecimal => bd.longValue()
      case _ =>
        errorThrower(
          "Unsupported conversion to Long. %s of type %s".format(n, Misc.getNameFromClass(n))
        )
    }
    JLong.valueOf(value)
  }

  /*
   * We no longer support the use of scala's BigInt/BigDecimal types
   * as it leads to precision issues within the ICU library.  By
   * forcing the use of Java's types we get the proper handling of
   * precision in ICU.
   * */
  def asBigInt(n: AnyRef): JBigInt = {
    val value: JBigInt = n match {
      case b: JBigInt => b
      case bd: JBigDecimal => bd.toBigInteger()
      case d: JDouble => new JBigDecimal(d).toBigInteger()
      case f: JFloat => new JBigDecimal(f.toDouble).toBigInteger()
      // the rest of the JNumbers are integers long or smaller.
      case jn: JNumber => new JBigInt(jn.toString())
      case _ =>
        errorThrower(
          "Unsupported conversion to BigInt. %s of type %s".format(n, Misc.getNameFromClass(n))
        )
    }
    value
  }

  /*
   * We no longer support the use of scala's BigInt/BigDecimal types
   * as it leads to precision issues within the ICU library.  By
   * forcing the use of Java's types we get the proper handling of
   * precision in ICU.
   * */
  def asJBigInt(n: AnyRef): JBigInt = {
    val value: JBigInt = n match {
      case b: JBigInt => b
      case bd: JBigDecimal => bd.toBigInteger()
      case d: JDouble => JBigDecimal.valueOf(d).toBigInteger()
      case f: JFloat => new JBigDecimal(f.toString()).toBigInteger()
      // the rest of the JNumbers are integers long or smaller.
      case jn: JNumber => JBigInt.valueOf(jn.longValue())
      case _ =>
        errorThrower(
          "Unsupported conversion to BigInt. %s of type %s".format(n, Misc.getNameFromClass(n))
        )
    }
    value
  }

  /*
   * We no longer support the use of scala's BigInt/BigDecimal types
   * as it leads to precision issues within the ICU library.  By
   * forcing the use of Java's types we get the proper handling of
   * precision in ICU.
   * */
  def asFloat(n: AnyRef): JFloat = {
    val value = n match {
      case f: JFloat => return f
      case d: JDouble => d.toFloat
      case b: JByte => b.toFloat
      case s: JShort => s.toFloat
      case i: JInt => i.toFloat
      case l: JLong => l.toFloat
      case bi: JBigInt => bi.floatValue()
      case bd: JBigDecimal => bd.floatValue()
      case _ =>
        errorThrower(
          "Unsupported conversion to Float. %s of type %s".format(n, Misc.getNameFromClass(n))
        )
    }
    JFloat.valueOf(value)
  }

  /*
   * We no longer support the use of scala's BigInt/BigDecimal types
   * as it leads to precision issues within the ICU library.  By
   * forcing the use of Java's types we get the proper handling of
   * precision in ICU.
   * */
  def asDouble(n: AnyRef): JDouble = {
    val value = n match {
      case f: JFloat => f.toDouble
      case d: JDouble => return d
      case b: JByte => b.toDouble
      case s: JShort => s.toDouble
      case i: JInt => i.toDouble
      case l: JLong => l.toDouble
      case bi: JBigInt => bi.doubleValue()
      case bd: JBigDecimal => bd.doubleValue()
      case _ =>
        errorThrower(
          "Unsupported conversion to Double. %s of type %s".format(n, Misc.getNameFromClass(n))
        )
    }
    JDouble.valueOf(value)
  }

  /*
   * We no longer support the use of scala's BigInt/BigDecimal types
   * as it leads to precision issues within the ICU library.  By
   * forcing the use of Java's types we get the proper handling of
   * precision in ICU.
   * */
  def asBigDecimal(n: AnyRef): JBigDecimal = {
    val value: JBigDecimal = n match {
      //
      // Not converting Float to string first causes precision issues
      // that round-half-to-even doesn't resolve correctly.  BigDecimal.valueOf(3.455) turns into 3.454999.
      // HALF_EVEN rounding mode would round this to 3.45 rather than the desired 3.46.
      case f: JFloat => new java.math.BigDecimal(f.toString)
      case d: JDouble => java.math.BigDecimal.valueOf(d)
      case bi: JBigInt => new java.math.BigDecimal(bi.toString())
      case bd: JBigDecimal => bd
      // The rest of the cases are integers long or smaller
      case jl: JLong => java.math.BigDecimal.valueOf(jl)
      case i: JInt => java.math.BigDecimal.valueOf(i.toLong)
      case s: JShort => java.math.BigDecimal.valueOf(s.toLong)
      case b: JByte => java.math.BigDecimal.valueOf(b.toLong)
      case jn: JNumber => new java.math.BigDecimal(jn.toString())
      case _ =>
        errorThrower(
          "Unsupported conversion to BigDecimal. %s of type %s".format(
            n,
            Misc.getNameFromClass(n)
          )
        )
    }
    value
  }

  /*
   * We no longer support the use of scala's BigInt/BigDecimal types
   * as it leads to precision issues within the ICU library.  By
   * forcing the use of Java's types we get the proper handling of
   * precision in ICU.
   * */
  def asJBigDecimal(n: AnyRef): JBigDecimal = {
    val value: JBigDecimal = n match {
      //
      // Not converting Float to string first causes precision issues
      // that round-half-to-even doesn't resolve correctly.  BigDecimal.valueOf(3.455) turns into 3.454999.
      // HALF_EVEN rounding mode would round this to 3.45 rather than the desired 3.46.
      case f: JFloat => new JBigDecimal(f.toString)
      case d: JDouble => JBigDecimal.valueOf(d)
      case bi: JBigInt => new JBigDecimal(bi)
      case bd: JBigDecimal => bd
      // The rest of the cases are integers long or smaller
      case jn: JNumber => JBigDecimal.valueOf(jn.longValue())
      case _ =>
        errorThrower(
          "Unsupported conversion to BigDecimal. %s of type %s".format(
            n,
            Misc.getNameFromClass(n)
          )
        )
    }
    value
  }

  def asBoolean(n: Any): JBoolean = {
    n match {
      case bool: JBoolean => return bool
      case b: Boolean => JBoolean.valueOf(b)
      case _ =>
        errorThrower(
          "Unsupported conversion to Boolean. %s of type %s".format(n, Misc.getNameFromClass(n))
        )
    }
  }

  def asNumber(n: Any): JNumber = {
    n match {
      case b: Byte => JByte.valueOf(b)
      case s: Short => JShort.valueOf(s)
      case i: Int => JInt.valueOf(i)
      case l: Long => JLong.valueOf(l)
      case f: Float => JFloat.valueOf(f)
      case d: Double => JDouble.valueOf(d)
      case jn: JNumber => jn
      case _ =>
        errorThrower(
          "Unsupported conversion to Number. %s of type %s".format(n, Misc.getNameFromClass(n))
        )
    }
  }

  @inline
  def asAnyRef(n: Any): AnyRef = {
    n.asInstanceOf[AnyRef]
  }

  /** For any standard predefined subtype of JNumber, is the value numerically zero. */
  def isZero(n1: JNumber): Boolean = {
    n1 match {
      case d: JDouble => d.doubleValue() == 0.0
      case f: JFloat => f.floatValue() == 0.0
      case bd: JBigDecimal => bd.signum() == 0
      case bi: JBigInt => bi.signum() == 0
      case l: JLong => l.longValue == 0
      case i: JInt => i.intValue == 0
      case s: JShort => s.shortValue == 0
      case b: JByte => b.byteValue == 0
      // $COVERAGE-OFF$
      case _ => errorThrower(s"Unknown JNumber type: ${n1.getClass.getName}")
      // $COVERAGE-ON$
    }
  }

}
