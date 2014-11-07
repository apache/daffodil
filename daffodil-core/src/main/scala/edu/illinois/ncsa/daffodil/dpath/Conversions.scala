package edu.illinois.ncsa.daffodil.dpath

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.util.Misc
import java.lang.NumberFormatException
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import com.ibm.icu.util.DFDLCalendar
import java.text.ParsePosition
import com.ibm.icu.util.Calendar
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.DFDLDateTime
import com.ibm.icu.util.DFDLTime
import com.ibm.icu.util.DFDLDate
import java.text.ParseException

/*
 * Casting chart taken from http://www.w3.org/TR/xpath-functions/#casting, with
 * types DFDL does not allow removed.
 *
 *  S\T   bool dat dbl dec dT  flt hxB int str tim 
 *  bool   Y    N   Y   Y   N   Y   N   Y   Y   N  
 *  dat    N    Y   N   N   Y   N   N   N   Y   N  
 *  dbl    Y    N   Y   M   N   Y   N   M   Y   N  
 *  dec    Y    N   Y   Y   N   Y   N   Y   Y   N  
 *  dT     N    Y   N   N   Y   N   N   N   Y   Y  
 *  flt    Y    N   Y   M   N   Y   N   M   Y   N  
 *  hxB    N    N   N   N   N   N   Y   N   Y   N  
 *  int    Y    N   Y   Y   N   Y   N   Y   Y   N  
 *  str    M    M   M   M   M   M   M   M   Y   M  
 *  tim    N    N   N   N   N   N   N   N   Y   Y  
 */

object Conversion {

  /**
   * Computes additional recipe ops to be added to the end of an argument Recipe
   * to convert the type. ST is the start type, the type the recipe has as its
   * output type. TT is the target type desired.
   */
  def conversionOps(st: NodeInfo.Kind, tt: NodeInfo.Kind, context: ThrowsSDE): List[RecipeOp] = {
    import NodeInfo._
    val ops: List[RecipeOp] = (st, tt) match {
      case (_, Array) => Nil
      case (x, y) if (x == y) => Nil
      case (x, Nillable) => {
        // we don't have enough information here to check whether the 
        // item under scrutiny is nillable or not.
        Nil
      }
      case (Nothing, x) => Nil
      case (x, AnyType) => Nil
      case (x: AnyAtomic.Kind, AnyAtomic) => Nil
      case (x: Numeric.Kind, String) => List(NumericToString)

      case (x: AnyDateTime.Kind, String) => List(DateTimeToString)
      case (HexBinary, String) => List(HexBinaryToString)
      case (String, NonEmptyString) => List(StringToNonEmptyString)
      case (x, NonEmptyString) => conversionOps(st, String, context) :+ StringToNonEmptyString
      case (x, ArrayIndex) => conversionOps(st, Long, context) ++ List(LongToArrayIndex)
      case (Boolean, Double) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Decimal) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Float) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Integer) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, NonNegativeInteger) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Long) => List(BooleanToLong)
      case (Boolean, UnsignedLong) => List(BooleanToLong) // no need to range check.
      case (Boolean, Int) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, UnsignedInt) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Short) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, UnsignedShort) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Byte) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, UnsignedByte) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, String) => List(BooleanToString)
      case (Date, DateTime) => List(DateToDateTime)
      case (Double, Boolean) => DoubleToLong +: conversionOps(Long, tt, context)
      case (Double, Decimal) => List(DoubleToDecimal)
      case (Double, Float) => List(DoubleToFloat)
      case (Double, Integer) => DoubleToDecimal +: conversionOps(Decimal, tt, context)
      case (Double, NonNegativeInteger) => DoubleToDecimal +: conversionOps(Decimal, tt, context)
      case (Double, Long) => List(DoubleToLong)
      case (Double, UnsignedLong) => List(DoubleToUnsignedLong)
      case (Double, i: Int.Kind) => DoubleToLong +: conversionOps(Long, tt, context)
      case (Double, ui: UnsignedInt.Kind) => DoubleToUnsignedLong +: conversionOps(UnsignedLong, tt, context)
      case (Float, Boolean) => FloatToDouble +: conversionOps(Double, tt, context)
      case (Float, n: Numeric.Kind) => FloatToDouble +: conversionOps(Double, tt, context)
      case (Decimal, Boolean) => DecimalToLong +: conversionOps(Long, tt, context)
      case (Decimal, Integer) => List(DecimalToInteger)
      case (Decimal, NonNegativeInteger) => List(DecimalToNonNegativeInteger)
      case (Decimal, Double) => List(DecimalToDouble)
      case (Decimal, Float) => DecimalToDouble +: conversionOps(Double, tt, context)
      case (Decimal, Long) => List(DecimalToLong)
      case (Decimal, UnsignedLong) => List(DecimalToUnsignedLong)
      case (Decimal, Int) => List(DecimalToLong, LongToInt)
      case (Decimal, UnsignedInt) => List(DecimalToLong, LongToUnsignedInt)
      case (Decimal, Short) => List(DecimalToLong, LongToShort)
      case (Decimal, UnsignedShort) => List(DecimalToLong, LongToUnsignedShort)
      case (Decimal, Byte) => List(DecimalToLong, LongToByte)
      case (Decimal, UnsignedByte) => List(DecimalToLong, LongToUnsignedByte)
      case (Integer, n: Numeric.Kind) => IntegerToDecimal +: conversionOps(Decimal, tt, context)
      case (NonNegativeInteger, n: Numeric.Kind) => IntegerToDecimal +: conversionOps(Decimal, tt, context)
      // Commented these out because these are illegal conversions for XPath functions.
      // I cannot pass a DateTime as a Date argument to an XPath function.
      //
      //case (DateTime, Date) => List(DateTimeToDate)
      //case (DateTime, Time) => List(DateTimeToTime)
      case (Integer, Boolean) => IntegerToDecimal +: conversionOps(Decimal, tt, context)
      case (String, Double) => List(StringToDouble)
      case (String, Decimal) => List(StringToDecimal)
      case (String, Float) => List(StringToDouble, DoubleToFloat)
      case (String, Integer) => List(StringToDecimal, DecimalToInteger)
      case (String, NonNegativeInteger) => List(StringToDecimal, DecimalToNonNegativeInteger)
      case (String, Long) => List(StringToLong)
      case (String, UnsignedLong) => List(StringToUnsignedLong)
      case (String, Int) => List(StringToLong, LongToInt)
      case (String, UnsignedInt) => List(StringToLong, LongToUnsignedInt)
      case (String, Short) => List(StringToLong, LongToShort)
      case (String, UnsignedShort) => List(StringToLong, LongToUnsignedShort)
      case (String, Byte) => List(StringToLong, LongToByte)
      case (String, UnsignedByte) => List(StringToLong, LongToUnsignedByte)
      case (String, HexBinary) => List(XSHexBinary.asInstanceOf[RecipeOp]) // TODO: figure out why I had to put this cast in place.
      case (String, Date) => List(StringToDate)
      case (String, Time) => List(StringToTime)
      case (String, DateTime) => List(StringToDateTime)
      case (String, Boolean) =>
        List(StringToBoolean)
      case (Time, DateTime) => List(TimeToDateTime)
      case (Byte, Long) => List(ByteToLong)

      case (Long, Boolean) => List(LongToBoolean)
      case (x: Long.Kind, Boolean) => conversionOps(st, Long, context) ++ conversionOps(Long, tt, context)
      case (Long, Integer) => List(LongToInteger)
      case (Long, NonNegativeInteger) => List(LongToNonNegativeInteger)
      case (Long, Decimal) => List(LongToDecimal)
      case (Long, UnsignedLong) => List(LongToUnsignedLong)
      case (Long, Int) => List(LongToInt)
      case (Long, UnsignedInt) => List(LongToUnsignedInt)
      case (Long, Short) => List(LongToShort)
      case (Long, UnsignedShort) => List(LongToUnsignedShort)
      case (Long, Byte) => List(LongToByte)
      case (Long, UnsignedByte) => List(LongToUnsignedByte)
      case (Long, Double) => List(LongToDouble)
      case (Long, Float) => LongToDouble +: conversionOps(Double, tt, context)

      case (Int, Double) => IntToLong +: conversionOps(Long, tt, context)
      case (Int, Decimal) => IntToLong +: conversionOps(Long, tt, context)
      case (Int, Float) => IntToLong +: conversionOps(Long, tt, context)
      case (Int, Integer) => IntToLong +: conversionOps(Long, tt, context)
      case (Int, NonNegativeInteger) => IntToLong +: conversionOps(Long, tt, context)
      case (Int, Long) => List(IntToLong)
      case (Int, UnsignedLong) => IntToLong +: conversionOps(Long, tt, context)
      case (Int, UnsignedInt) => IntToLong +: conversionOps(Long, tt, context)
      case (Int, Short) => IntToLong +: conversionOps(Long, tt, context)
      case (Int, UnsignedShort) => IntToLong +: conversionOps(Long, tt, context)
      case (Int, Byte) => IntToLong +: conversionOps(Long, tt, context)
      case (Int, UnsignedByte) => IntToLong +: conversionOps(Long, tt, context)

      case (UnsignedInt, Double) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Decimal) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Float) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Integer) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, NonNegativeInteger) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Long) => List(UnsignedIntToLong)
      case (UnsignedInt, UnsignedLong) => List(UnsignedIntToLong) // no need to range check.
      case (UnsignedInt, Int) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Short) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, UnsignedShort) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Byte) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, UnsignedByte) => UnsignedIntToLong +: conversionOps(Long, tt, context)

      case (UnsignedLong, Double) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Decimal) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Float) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Integer) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, NonNegativeInteger) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Long) => List(UnsignedLongToLong)
      case (UnsignedLong, Int) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Short) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, UnsignedShort) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Byte) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, UnsignedByte) => UnsignedLongToLong +: conversionOps(Long, tt, context)

      case (ArrayIndex, Double) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Decimal) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Float) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Integer) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, NonNegativeInteger) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Long) => List(ArrayIndexToLong)
      case (ArrayIndex, UnsignedLong) => List(ArrayIndexToLong) // no need to range check.
      case (ArrayIndex, Int) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, UnsignedInt) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Short) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, UnsignedShort) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Byte) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, UnsignedByte) => ArrayIndexToLong +: conversionOps(Long, tt, context)

      case (Short, Double) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, Decimal) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, Float) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, Integer) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, NonNegativeInteger) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, Long) => List(ShortToLong)
      case (Short, UnsignedLong) => List(ShortToLong) // no need to range check.
      case (Short, Int) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, UnsignedInt) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, UnsignedShort) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, Byte) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, UnsignedByte) => ShortToLong +: conversionOps(Long, tt, context)

      case (UnsignedShort, Double) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Decimal) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Float) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Integer) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, NonNegativeInteger) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Long) => List(UnsignedShortToLong)
      case (UnsignedShort, UnsignedLong) => List(UnsignedShortToLong) // no need to range check.
      case (UnsignedShort, Int) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, UnsignedInt) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Short) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Byte) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, UnsignedByte) => UnsignedShortToLong +: conversionOps(Long, tt, context)

      case (Byte, Double) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, Decimal) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, Float) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, Integer) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, NonNegativeInteger) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, UnsignedLong) => List(ByteToLong) // no need to range check.
      case (Byte, Int) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, UnsignedInt) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, Short) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, UnsignedShort) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, Byte) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, UnsignedByte) => ByteToLong +: conversionOps(Long, tt, context)

      case (UnsignedByte, Double) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Decimal) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Float) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Integer) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, NonNegativeInteger) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Long) => List(UnsignedByteToLong)
      case (UnsignedByte, UnsignedLong) => List(UnsignedByteToLong) // no need to range check.
      case (UnsignedByte, Int) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, UnsignedInt) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Short) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, UnsignedShort) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Byte) => UnsignedByteToLong +: conversionOps(Long, tt, context)

      // Note: These are explicitly not allowed
      //      case (AnyAtomic, Time) => AnyAtomicToString +: conversionOps(String, tt, context)
      //      case (AnyAtomic, Date) => AnyAtomicToString +: conversionOps(String, tt, context)
      //      case (AnyAtomic, DateTime) => AnyAtomicToString +: conversionOps(String, tt, context)

      case (_, Exists) => Nil
      case (_, other) => context.SDE("The type %s cannot be converted to %s.", st.name, tt.name)
    }
    ops
  }

  protected def constructCalendar(value: String, inFormat: SimpleDateFormat, fncName: String, toType: String): Calendar = {
    val isLenient = inFormat.isLenient()

    try {
      inFormat.setLenient(false)
      inFormat.parse(value)
      true
    } catch {
      case e: ParseException => {
        inFormat.setLenient(isLenient)
        throw new java.lang.IllegalArgumentException(String.format("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s",
          fncName, value.toString, toType, "Failed to match format."))
      }
    }

    val calendar = inFormat.getCalendar()
    val pos = new ParsePosition(0)

    inFormat.parse(value.toString, calendar, pos)

    try {
      calendar.getTime()
    } catch {
      case ex: java.lang.IllegalArgumentException => {
        inFormat.setLenient(isLenient)
        throw new java.lang.IllegalArgumentException(String.format("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s",
          fncName, value.toString, toType, ex.getMessage()))
      }
    }

    inFormat.setLenient(isLenient)

    if (pos.getIndex() == 0 || pos.getErrorIndex() != -1) {
      throw new java.lang.IllegalArgumentException(String.format("Conversion Error: %s failed to convert \"%s\" to %s due to a parse error.",
        fncName, value.toString, toType))
    }
    if (pos.getIndex() != (value.length())) {
      throw new java.lang.IllegalArgumentException(String.format("Conversion Error: %s failed to convert \"%s\" to %s. Failed to use up all characters in the parse.  Stopped at %s.",
        fncName, value.toString, toType, pos.getIndex().toString))
    }
    calendar
  }

  def stringToDFDLDateTime(value: String, inFormat: SimpleDateFormat, fncName: String, toType: String): DFDLDateTime = {
    val calendar = constructCalendar(value, inFormat, fncName, toType)
    new DFDLDateTime(calendar)
  }

  def stringToDFDLDate(value: String, inFormat: SimpleDateFormat, fncName: String, toType: String): DFDLDate = {
    val calendar = constructCalendar(value, inFormat, fncName, toType)
    new DFDLDate(calendar)
  }

  def stringToDFDLTime(value: String, inFormat: SimpleDateFormat, fncName: String, toType: String): DFDLTime = {
    val calendar = constructCalendar(value, inFormat, fncName, toType)
    new DFDLTime(calendar)
  }

  /**
   * Compute types the args should be converted to, and the resulting type
   * from the operation on them.
   */
  def numericBinaryOpTargetTypes(op: String, inherent1: NodeInfo.Numeric.Kind, inherent2: NodeInfo.Numeric.Kind): (NodeInfo.Numeric.Kind, NodeInfo.Numeric.Kind) = {
    import NodeInfo._
    /*
     * Adjust for the Decimal result type when div is used on any integer types.
     */
    def divResult(t: NodeInfo.Numeric.Kind) =
      if (op == "div") Decimal else t
    val (argType: Numeric.Kind, resultType: Numeric.Kind) = (inherent1, inherent2) match {
      case (_, Decimal) => (Decimal, Decimal)
      case (Decimal, _) => (Decimal, Decimal)
      case (_, Double) => (Double, Double)
      case (Double, _) => (Double, Double)
      case (_, Float) => (Double, Double)
      case (Float, _) => (Double, Double)
      case (x, Integer) => (Integer, divResult(Integer))
      case (Integer, x) => (Integer, divResult(Integer))
      case (x, NonNegativeInteger) => (NonNegativeInteger, divResult(Integer))
      case (NonNegativeInteger, x) => (NonNegativeInteger, divResult(Integer))
      case (x, UnsignedLong) => (UnsignedLong, divResult(Integer))
      case (UnsignedLong, x) => (UnsignedLong, divResult(Integer))
      case (x, ArrayIndex) => (ArrayIndex, ArrayIndex)
      case (ArrayIndex, x) => (ArrayIndex, ArrayIndex)
      case (x, Long) => (Long, divResult(Long))
      case (Long, x) => (Long, divResult(Long))
      case (x, UnsignedInt) => (UnsignedInt, divResult(Long))
      case (UnsignedInt, x) => (UnsignedInt, divResult(Long))
      case (x, Int) => (Int, divResult(Int))
      case (Int, x) => (Int, divResult(Int))
      case (x, UnsignedShort) => (UnsignedShort, divResult(Int))
      case (UnsignedShort, x) => (UnsignedShort, divResult(Int))
      case (x, Short) => (Short, divResult(Int))
      case (Short, x) => (Short, divResult(Int))
      case (x, UnsignedByte) => (UnsignedByte, divResult(Int))
      case (UnsignedByte, x) => (UnsignedByte, divResult(Int))
      case (x, Byte) => (Byte, divResult(Int))
      case (Byte, x) => (Byte, divResult(Int))
      case _ => Assert.usageError(
        "Unsupported types for op '%s' were %s and %s.".format(op, inherent1, inherent2))
    }
    (argType, resultType)
  }

}
