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

package edu.illinois.ncsa.daffodil.dsom

import java.math.BigInteger
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.GregorianCalendar
import com.ibm.icu.util.TimeZone
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

/////////////////////////////////////////////////////////////////
// Type System
/////////////////////////////////////////////////////////////////

trait TypeBase

trait TypeChecks {
  protected def dateToBigDecimal(date: String, format: String, dateType: String, context: ThrowsSDE): java.math.BigDecimal = {
    val df = new SimpleDateFormat(format)
    df.setCalendar(new GregorianCalendar())
    df.setTimeZone(TimeZone.GMT_ZONE)
    val bd = try {
      val dt = df.parse(date)
      new java.math.BigDecimal(dt.getTime())
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case u: UnsuppressableException => throw u
      case e1: Exception => {
        try {
          // Could already be a BigDecimal
          new java.math.BigDecimal(date)
        } catch {
          case s: scala.util.control.ControlThrowable => throw s
          case u: UnsuppressableException => throw u
          case e2: Exception => context.SDE("Failed to parse (%s) to %s (%s) due to %s (after %s).", date, dateType, format, e2.getMessage(), e1.getMessage())
        }
      }
    }
    bd
  }

  private def convertStringToBigDecimal(value: String, primitiveType: PrimType, context: ThrowsSDE): java.math.BigDecimal = {
    primitiveType match {
      case PrimType.DateTime => dateToBigDecimal(value, "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx", PrimType.DateTime.toString(), context)
      case PrimType.Date => dateToBigDecimal(value, "uuuu-MM-ddxxx", PrimType.Date.toString(), context)
      case PrimType.Time => dateToBigDecimal(value, "HH:mm:ss.SSSSSSxxx", PrimType.Time.toString(), context)
      case _ => new java.math.BigDecimal(value)
    }
  }

  def checkRangeReturnsValue(value: String, primitiveType: PrimType, theContext: ThrowsSDE): (Boolean, Option[BigDecimal]) = {
    // EmptyString is only valid for hexBinary and String
    if ((value == null | value.length() == 0)) {
      return primitiveType match {
        case PrimType.HexBinary | PrimType.String => (true, None)
        case _ => (false, None)
      }
    }

    // Don't need to range check String or HexBinary
    // no point attempting a conversion to BigDecimal so
    // return early here.
    primitiveType match {
      case PrimType.String | PrimType.HexBinary => return (true, None)
      case _ => /* Continue on below */
    }

    // Check Boolean, and the Numeric types.
    (value.toLowerCase(), primitiveType) match {
      case ("true", PrimType.Boolean) => (true, Some(BigDecimal(1)))
      case ("false", PrimType.Boolean) => (true, Some(BigDecimal(0)))
      case (x, PrimType.Boolean) => theContext.SDE("%s is not a valid Boolean value. Expected 'true' or 'false'.", x)
      case (_, _) => {
        // Perform conversions once
        val theValue = convertStringToBigDecimal(value, primitiveType, theContext)

        // Here we're just doing range checking for the
        // specified primitive type
        val res: Boolean = primitiveType match {
          case PrimType.Int => isInIntRange(theValue)
          case PrimType.Byte => isInByteRange(theValue)
          case PrimType.Short => isInShortRange(theValue)
          case PrimType.Long => isInLongRange(theValue)
          case PrimType.Integer => true // Unbounded Integer
          case PrimType.UnsignedInt => isInUnsignedIntRange(theValue)
          case PrimType.UnsignedByte => isInUnsignedByteRange(theValue)
          case PrimType.UnsignedShort => isInUnsignedShortRange(theValue)
          case PrimType.UnsignedLong => isInUnsignedLongRange(theValue)
          case PrimType.Double => isInDoubleRange(theValue)
          case PrimType.Float => isInFloatRange(theValue)
          case PrimType.DateTime => true
          case PrimType.Date => true
          case PrimType.Time => true
          case PrimType.Boolean => Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.Decimal => true // Unbounded Decimal
          case PrimType.HexBinary => Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.String => Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.NonNegativeInteger => isInNonNegativeIntegerRange(theValue)
        }
        val isValueWhole = {
          val IsWholeRegex = """^[^.]*(\.0*)?$""".r
          value match {
            case IsWholeRegex(_) => true
            case _ => false
          }
        }
        primitiveType match {
          case PrimType.Int | PrimType.Byte | PrimType.Short | PrimType.Long |
            PrimType.Integer | PrimType.UnsignedInt | PrimType.UnsignedByte | PrimType.UnsignedShort |
            PrimType.UnsignedLong => if (!isValueWhole) theContext.SDE("checkRange - Value (%s) must be a whole number.", value)
          case _ => // OK
        }
        (res, Some(theValue))
      }
    }
  }

  def checkRange(value: String, primitiveType: PrimType, theContext: ThrowsSDE): Boolean = {
    val (boolResult, _) = checkRangeReturnsValue(value, primitiveType, theContext)
    boolResult
  }

  protected def isNumInRange(num: java.math.BigDecimal, min: java.math.BigDecimal,
    max: java.math.BigDecimal): Boolean = {
    val checkMin = num.compareTo(min)
    if (checkMin < 0) { return false } // num less than min
    val checkMax = num.compareTo(max)
    if (checkMax > 0) { return false } // num greater than max
    true
  }
  protected def isInByteRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Byte.MinValue.toLong.toString())
    val max = new java.math.BigDecimal(Byte.MaxValue.toLong.toString())
    isNumInRange(value, min, max)
  }
  protected def isInShortRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Short.MinValue.toLong.toString())
    val max = new java.math.BigDecimal(Short.MaxValue.toLong.toString())
    isNumInRange(value, min, max)
  }
  protected def isInIntRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    val max = new java.math.BigDecimal(Int.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInIntegerRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    // Unbounded Integer
    val checkMin = value.compareTo(min)
    if (checkMin < 0) { return false } // num less than min
    true
  }
  protected def isInLongRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Long.MinValue.toString())
    val max = new java.math.BigDecimal(Long.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInDoubleRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Double.MinValue.toString())
    val max = new java.math.BigDecimal(Double.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInFloatRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Float.MinValue.toString())
    val max = new java.math.BigDecimal(Float.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInDecimalRange(value: java.math.BigDecimal): Boolean = {
    // BigDecimal is unbounded? So nothing outside of its range?
    true
  }
  protected def isInNegativeIntegerRange(value: java.math.BigDecimal, context: ThrowsSDE): Boolean = {
    // TODO: NegativeInteger not supported in DFDL v1.0
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    // val max = new java.math.BigDecimal(Int.MaxValue.toString())
    val isNegative = value.signum == -1
    if (!isNegative) context.SDE("Expected a negative integer for this value.")
    val checkMin = value.compareTo(min)
    if (checkMin < 0) context.SDE("Value (%s) was found to be more negative than allowed by Int.MinValue.", value.intValue())
    true
  }
  protected def isInNonNegativeIntegerRange(value: java.math.BigDecimal): Boolean = {
    // Should be treated as unsigned Integer (unbounded)
    // val min = java.math.BigDecimal.ZERO
    val isNegative = value.signum == -1
    if (isNegative) return false
    true
  }
  protected def isInUnsignedXXXRange(value: java.math.BigDecimal, numBits: Int, typeName: String): Boolean = {
    Assert.usage(numBits <= 64, "isInUnsignedXXXRange: numBits must be <= 64.")
    // val min = java.math.BigDecimal.ZERO
    val max = new java.math.BigDecimal(BigInteger.ONE.shiftLeft(numBits)).subtract(new java.math.BigDecimal(1))
    val isNegative = value.signum == -1
    if (isNegative) return false
    val checkMax = value.compareTo(max)
    if (checkMax > 0) return false
    true
  }
  protected def isInUnsignedLongRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 64, "ulong")

  protected def isInUnsignedIntRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 32, "uint")

  protected def isInUnsignedShortRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 16, "ushort")

  protected def isInUnsignedByteRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 8, "ubyte")
}
