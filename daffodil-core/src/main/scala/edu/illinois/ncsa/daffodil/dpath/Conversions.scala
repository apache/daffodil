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
    val ops = (st, tt) match {
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
      case (String, HexBinary) => List(XSHexBinary)
      case (String, Date) => List(XSDate)
      case (String, Time) => List(XSTime)
      case (String, DateTime) => List(XSDateTime)
      case (String, Boolean) =>
        List(StringToBoolean)
      case (Time, DateTime) => List(TimeToDateTime)
      case (Byte, Long) => List(ByteToLong)
      case (UnsignedByte, Long) => List(UnsignedByteToLong)

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

      case (a: AnyAtomic.Kind, AnyAtomic) => Nil

      case (AnyAtomic, Time) => AnyAtomicToString +: conversionOps(String, tt, context)
      case (AnyAtomic, Date) => AnyAtomicToString +: conversionOps(String, tt, context)
      case (AnyAtomic, DateTime) => AnyAtomicToString +: conversionOps(String, tt, context)

      case (_, other) => context.SDE("The type %s cannot be converted to %s.", st.name, tt.name)
    }
    ops
  }

  def stringToDFDLCalendar(value: String, inFormat: SimpleDateFormat, outFormat: SimpleDateFormat, fncName: String, toType: String): Either[String, DFDLCalendar] = {
    val calendar = inFormat.getCalendar()
    val pos = new ParsePosition(0)

    inFormat.setLenient(false)
    inFormat.parse(value.toString, calendar, pos)

    var formattedString: String = null
    try {
      calendar.getTime()
      formattedString = outFormat.format(calendar)
    } catch {
      case ex: java.lang.IllegalArgumentException => {
        //dstate.pstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, value.toString, toType, ex.getMessage())
        return Left(String.format("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, value.toString, toType, ex.getMessage()))
      }
      case ex: Exception => {
        // dstate.pstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, value.toString, toType, ex.getMessage())
        return Left(String.format("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, value.toString, toType, ex.getMessage()))
      }
    }

    if (pos.getIndex() == 0) {
      //dstate.pstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s.", fncName, value.toString, toType) 
      return Left(String.format("Conversion Error: %s failed to convert \"%s\" to %s due to a parse error.", fncName, value.toString, toType))
    }
    if (pos.getIndex() != (value.length())) {
      //dstate.pstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s.", fncName, value.toString, toType) 
      return Left(String.format("Conversion Error: %s failed to convert \"%s\" to %s. Failed to use up all characters in the parse.  Stopped at %s.",
        fncName, value.toString, toType, pos.getIndex().toString))
    }

    val cal = new DFDLCalendar(inFormat.getCalendar(), formattedString)
    Right(cal)
  }

  def calendarToDFDLCalendar(calendar: Calendar, formatString: String, dstate: DState, fncName: String, toType: String): DFDLCalendar = {
    val format = new SimpleDateFormat(formatString)
    format.setCalendar(calendar)

    var formattedString: String = null
    try {
      formattedString = format.format(calendar)
    } catch {
      case ex: java.lang.IllegalArgumentException => dstate.pstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, calendar.toString, toType, ex.getMessage())
      case ex: Exception => dstate.pstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, calendar.toString, toType, ex.getMessage())
    }

    val cal = new DFDLCalendar(format.getCalendar(), formattedString)
    cal
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

  //  def convertTo(st: NodeInfo.Kind, tt: NodeInfo.Kind, sv: Any): Any = {
  //    val res = tt match {
  //      case NodeInfo.String => convertToString(st, sv)
  //      case NodeInfo.Float => convertToFloat(st, sv)
  //      case NodeInfo.Double => convertToDouble(st, sv)
  //      case NodeInfo.Decimal => convertToDecimal(st, sv)
  //      case NodeInfo.DateTime => ???
  //      case NodeInfo.Time => ???
  //      case NodeInfo.Date => ???
  //      case NodeInfo.Boolean => convertToBoolean(st, sv)
  //      case NodeInfo.HexBinary => convertToHexBinary(st, sv)
  //      case NodeInfo.Integer => convertToInteger(st, sv)
  //      case NodeInfo.Int => convertToInt(st, sv)
  //      case NodeInfo.Short => convertToShort(st, sv)
  //      case NodeInfo.Long => convertToLong(st, sv)
  //      case NodeInfo.Byte => convertToByte(st, sv)
  //      case NodeInfo.UnsignedByte => convertToUnsignedByte(st, sv)
  //      case NodeInfo.UnsignedInt => convertToUnsignedInt(st, sv)
  //      case NodeInfo.UnsignedLong => convertToUnsignedLong(st, sv)
  //      case NodeInfo.UnsignedShort => convertToUnsignedShort(st, sv)
  //      case NodeInfo.NonNegativeInteger => convertToNonNegativeInteger(st, sv)
  //      case _ => Assert.usageError("convertTo called on unsupported type %s.".format(tt))
  //    }
  //    res
  //  }
  //
  //  def convertToFloat(st: NodeInfo.Kind, sv: Any): Float = {
  //    val res = st match {
  //      case NodeInfo.Float => sv.asInstanceOf[Float]
  //      case NodeInfo.Double => sv.asInstanceOf[Double] match {
  //        case Double.PositiveInfinity => Float.PositiveInfinity
  //        case Double.NegativeInfinity => Float.NegativeInfinity
  //        case Double.NaN => Float.NaN
  //        case d => d.toFloat
  //      }
  //      case NodeInfo.Boolean => sv.asInstanceOf[Boolean] match {
  //        case true => 1.0E0f
  //        case false => 0.0E0f
  //      }
  //      case NodeInfo.Decimal | NodeInfo.Integer | NodeInfo.Long | NodeInfo.Int | NodeInfo.Short | NodeInfo.Byte | NodeInfo.UnsignedByte | NodeInfo.UnsignedInt | NodeInfo.UnsignedLong | NodeInfo.UnsignedShort => {
  //        val str = convertToString(st, sv)
  //        convertToFloat(NodeInfo.String, str)
  //      }
  //      case NodeInfo.String => sv.asInstanceOf[String].toFloat
  //      case _ => ??? // TODO: Error
  //    }
  //    res
  //  }
  //
  //  def convertToDouble(st: NodeInfo.Kind, sv: Any): Double = {
  //    val res = st match {
  //      case NodeInfo.Double => sv.asInstanceOf[Double]
  //      case NodeInfo.Float => sv.asInstanceOf[Float] match {
  //        case Float.PositiveInfinity => Double.PositiveInfinity
  //        case Float.NegativeInfinity => Double.NegativeInfinity
  //        case Float.NaN => Double.NaN
  //        case f => f.toDouble
  //      }
  //      case NodeInfo.Boolean => sv.asInstanceOf[Boolean] match {
  //        case true => 1.0E0
  //        case false => 0.0E0
  //      }
  //      case NodeInfo.Decimal | NodeInfo.Integer | NodeInfo.Long | NodeInfo.Int | NodeInfo.Short | NodeInfo.Byte | NodeInfo.UnsignedByte | NodeInfo.UnsignedInt | NodeInfo.UnsignedLong | NodeInfo.UnsignedShort => {
  //        val str = convertToString(st, sv)
  //        convertToDouble(NodeInfo.String, str)
  //      }
  //      case NodeInfo.String => sv.asInstanceOf[String].toDouble
  //      case _ => ??? // TODO: Error
  //    }
  //    res
  //  }
  //
  //  def convertToDecimal(st: NodeInfo.Kind, sv: Any): BigDecimal = {
  //    val res = st match {
  //      case NodeInfo.Decimal => sv.asInstanceOf[BigDecimal]
  //      case NodeInfo.Integer => BigDecimal(sv.asInstanceOf[BigInt])
  //      case NodeInfo.Int => BigDecimal(sv.asInstanceOf[Int])
  //      case NodeInfo.UnsignedInt => BigDecimal(sv.asInstanceOf[Long])
  //      case NodeInfo.Short => BigDecimal(sv.asInstanceOf[Short])
  //      case NodeInfo.UnsignedShort => BigDecimal(sv.asInstanceOf[Int])
  //      case NodeInfo.Byte => BigDecimal(sv.asInstanceOf[Byte])
  //      case NodeInfo.UnsignedByte => BigDecimal(sv.asInstanceOf[Short])
  //      case NodeInfo.Long => BigDecimal(sv.asInstanceOf[Long])
  //      case NodeInfo.UnsignedLong => BigDecimal(sv.asInstanceOf[BigInt])
  //      case NodeInfo.Float => sv.asInstanceOf[Float] match {
  //        case Float.PositiveInfinity => ??? // TODO: Error
  //        case Float.NegativeInfinity => ??? // TODO: Error
  //        case Float.NaN => ??? // TODO: Error
  //        case f: Float => BigDecimal(f)
  //      }
  //      case NodeInfo.Double => sv.asInstanceOf[Double] match {
  //        case Double.PositiveInfinity => ??? // TODO: Error
  //        case Double.NegativeInfinity => ??? // TODO: Error
  //        case Double.NaN => ??? // TODO: Error
  //        case d: Double => BigDecimal(d)
  //      }
  //      case NodeInfo.Boolean => sv.asInstanceOf[Boolean] match {
  //        case true => BigDecimal(1.0)
  //        case false => BigDecimal(0.0)
  //      }
  //      case NodeInfo.String => BigDecimal(sv.asInstanceOf[String])
  //      case _ => ??? // TODO: Error
  //    }
  //    res
  //  }
  //
  //  def convertToInteger(st: NodeInfo.Kind, sv: Any): BigInt = {
  //    val res = st match {
  //      case NodeInfo.Integer => sv.asInstanceOf[BigInt]
  //      case NodeInfo.Int => BigInt(sv.asInstanceOf[Int])
  //      case NodeInfo.UnsignedInt => BigInt(sv.asInstanceOf[Long])
  //      case NodeInfo.Short => BigInt(sv.asInstanceOf[Short])
  //      case NodeInfo.UnsignedShort => BigInt(sv.asInstanceOf[Int])
  //      case NodeInfo.Byte => BigInt(sv.asInstanceOf[Byte])
  //      case NodeInfo.UnsignedByte => BigInt(sv.asInstanceOf[Short])
  //      case NodeInfo.Long => BigInt(sv.asInstanceOf[Long])
  //      case NodeInfo.UnsignedLong => sv.asInstanceOf[BigInt]
  //      case NodeInfo.Decimal => sv.asInstanceOf[BigDecimal].toBigInt
  //      case NodeInfo.Float => sv.asInstanceOf[Float] match {
  //        case Float.PositiveInfinity => ??? // TODO: Error
  //        case Float.NegativeInfinity => ??? // TODO: Error
  //        case Float.NaN => ??? // TODO: Error
  //        case f: Float => BigInt(f.longValue)
  //      }
  //      case NodeInfo.Double => sv.asInstanceOf[Double] match {
  //        case Double.PositiveInfinity => ??? // TODO: Error
  //        case Double.NegativeInfinity => ??? // TODO: Error
  //        case Double.NaN => ??? // TODO: Error
  //        case d: Double => BigInt(d.longValue)
  //      }
  //      case NodeInfo.Boolean => sv.asInstanceOf[Boolean] match {
  //        case true => BigInt(1)
  //        case false => BigInt(0)
  //      }
  //      case NodeInfo.String => BigInt(sv.asInstanceOf[String])
  //      case _ => ??? // TODO: Error
  //    }
  //    res
  //  }
  //
  //  def convertToLong(st: NodeInfo.Kind, sv: Any): Long = {
  //    val res = st match {
  //      case NodeInfo.Long => sv.asInstanceOf[Long]
  //      case NodeInfo.UnsignedLong => sv.asInstanceOf[BigInt].toLong
  //      case NodeInfo.Integer => sv.asInstanceOf[BigInt].toLong
  //      case NodeInfo.Int => sv.asInstanceOf[Int].toLong
  //      case NodeInfo.UnsignedInt => sv.asInstanceOf[Long].toLong
  //      case NodeInfo.Short => sv.asInstanceOf[Short].toLong
  //      case NodeInfo.UnsignedShort => sv.asInstanceOf[Int].toLong
  //      case NodeInfo.Byte => sv.asInstanceOf[Byte].toLong
  //      case NodeInfo.UnsignedByte => sv.asInstanceOf[Short].toLong
  //      case NodeInfo.Decimal => sv.asInstanceOf[BigDecimal].toLong
  //      case NodeInfo.Float => sv.asInstanceOf[Float] match {
  //        case Float.PositiveInfinity => ??? // TODO: Error
  //        case Float.NegativeInfinity => ??? // TODO: Error
  //        case Float.NaN => ??? // TODO: Error
  //        case f: Float => f.longValue
  //      }
  //      case NodeInfo.Double => sv.asInstanceOf[Double] match {
  //        case Double.PositiveInfinity => ??? // TODO: Error
  //        case Double.NegativeInfinity => ??? // TODO: Error
  //        case Double.NaN => ??? // TODO: Error
  //        case d: Double => d.longValue
  //      }
  //      case NodeInfo.Boolean => sv.asInstanceOf[Boolean] match {
  //        case true => 1l
  //        case false => 0l
  //      }
  //      case NodeInfo.String => sv.asInstanceOf[String].toLong
  //      case _ => ??? // TODO: Error
  //    }
  //    res
  //  }
  //
  //  def convertToInt(st: NodeInfo.Kind, sv: Any): Int = {
  //    val res = st match {
  //      case NodeInfo.Int => sv.asInstanceOf[Int]
  //      case NodeInfo.UnsignedInt => sv.asInstanceOf[Long].toInt
  //      case NodeInfo.Integer => sv.asInstanceOf[BigInt].toInt
  //      case NodeInfo.Long => sv.asInstanceOf[Long].toInt
  //      case NodeInfo.UnsignedLong => sv.asInstanceOf[BigInt].toInt
  //      case NodeInfo.Short => sv.asInstanceOf[Short].toInt
  //      case NodeInfo.UnsignedShort => sv.asInstanceOf[Int]
  //      case NodeInfo.Byte => sv.asInstanceOf[Byte].toInt
  //      case NodeInfo.UnsignedByte => sv.asInstanceOf[Short].toInt
  //      case NodeInfo.Decimal => sv.asInstanceOf[BigDecimal].toInt
  //      case NodeInfo.Float => sv.asInstanceOf[Float] match {
  //        case Float.PositiveInfinity => ??? // TODO: Error
  //        case Float.NegativeInfinity => ??? // TODO: Error
  //        case Float.NaN => ??? // TODO: Error
  //        case f: Float => f.intValue
  //      }
  //      case NodeInfo.Double => sv.asInstanceOf[Double] match {
  //        case Double.PositiveInfinity => ??? // TODO: Error
  //        case Double.NegativeInfinity => ??? // TODO: Error
  //        case Double.NaN => ??? // TODO: Error
  //        case d: Double => d.intValue
  //      }
  //      case NodeInfo.Boolean => sv.asInstanceOf[Boolean] match {
  //        case true => 1
  //        case false => 0
  //      }
  //      case NodeInfo.String => sv.asInstanceOf[String].toInt
  //      case _ => ??? // TODO: Error
  //    }
  //    res
  //  }
  //
  //  def convertToByte(st: NodeInfo.Kind, sv: Any): Int = {
  //    val res = st match {
  //      case NodeInfo.Byte => sv.asInstanceOf[Byte]
  //      case NodeInfo.Int => sv.asInstanceOf[Int].toByte
  //      case NodeInfo.UnsignedInt => sv.asInstanceOf[Long].toByte
  //      case NodeInfo.Integer => sv.asInstanceOf[BigInt].toByte
  //      case NodeInfo.Long => sv.asInstanceOf[Long].toByte
  //      case NodeInfo.UnsignedLong => sv.asInstanceOf[BigInt].toByte
  //      case NodeInfo.Short => sv.asInstanceOf[Short].toByte
  //      case NodeInfo.UnsignedShort => sv.asInstanceOf[Int].toByte
  //      case NodeInfo.UnsignedByte => sv.asInstanceOf[Short].toByte
  //      case NodeInfo.Decimal => sv.asInstanceOf[BigDecimal].toByte
  //      case NodeInfo.Float => sv.asInstanceOf[Float] match {
  //        case Float.PositiveInfinity => ??? // TODO: Error
  //        case Float.NegativeInfinity => ??? // TODO: Error
  //        case Float.NaN => ??? // TODO: Error
  //        case f: Float => f.byteValue
  //      }
  //      case NodeInfo.Double => sv.asInstanceOf[Double] match {
  //        case Double.PositiveInfinity => ??? // TODO: Error
  //        case Double.NegativeInfinity => ??? // TODO: Error
  //        case Double.NaN => ??? // TODO: Error
  //        case d: Double => d.byteValue
  //      }
  //      case NodeInfo.Boolean => sv.asInstanceOf[Boolean] match {
  //        case true => 1.byteValue
  //        case false => 0.byteValue
  //      }
  //      case NodeInfo.String => sv.asInstanceOf[String].toByte
  //      case _ => ??? // TODO: Error
  //    }
  //    res
  //  }
  //
  //  def convertToUnsignedByte(st: NodeInfo.Kind, sv: Any): Short = {
  //    val s = convertToShort(st, sv)
  //    if (s < 0 || s > (1 << 8) - 1) {
  //      throw new NumberFormatException("out of range")
  //    }
  //    s
  //  }
  //
  //  def convertToUnsignedShort(st: NodeInfo.Kind, sv: Any): Int = {
  //    val i = convertToInt(st, sv)
  //    if (i < 0 || i > (1 << 16) - 1) {
  //      throw new NumberFormatException("out of range")
  //    }
  //    i
  //  }
  //
  //  def convertToUnsignedInt(st: NodeInfo.Kind, sv: Any): Long = {
  //    val l = convertToLong(st, sv)
  //    if (l < 0 || l > (1l << 32) - 1) {
  //      throw new NumberFormatException("out of range")
  //    }
  //    l
  //  }
  //
  //  def convertToUnsignedLong(st: NodeInfo.Kind, sv: Any): BigInt = {
  //    val bi = convertToInteger(st, sv)
  //    if (bi < 0 || bi > (BigInt(1) << 64) - 1) {
  //      throw new NumberFormatException("out of range")
  //    }
  //    bi
  //  }
  //
  //  def convertToNonNegativeInteger(st: NodeInfo.Kind, sv: Any): BigInt = {
  //    val bi = convertToInteger(st, sv)
  //    if (bi < 0) {
  //      throw new NumberFormatException("out of range")
  //    }
  //    bi
  //  }
  //
  //  def convertToShort(st: NodeInfo.Kind, sv: Any): Short = {
  //    val res = st match {
  //      case NodeInfo.Short => sv.asInstanceOf[Short]
  //      case NodeInfo.UnsignedShort => sv.asInstanceOf[Int].toShort
  //      case NodeInfo.Int => sv.asInstanceOf[Int].toShort
  //      case NodeInfo.UnsignedInt => sv.asInstanceOf[Long].toShort
  //      case NodeInfo.Byte => sv.asInstanceOf[Byte].toShort
  //      case NodeInfo.UnsignedByte => sv.asInstanceOf[Short]
  //      case NodeInfo.Integer => sv.asInstanceOf[BigInt].toShort
  //      case NodeInfo.Long => sv.asInstanceOf[Long].toShort
  //      case NodeInfo.UnsignedLong => sv.asInstanceOf[BigInt].toShort
  //      case NodeInfo.Decimal => sv.asInstanceOf[BigDecimal].toShort
  //      case NodeInfo.Float => sv.asInstanceOf[Float] match {
  //        case Float.PositiveInfinity => ??? // TODO: Error
  //        case Float.NegativeInfinity => ??? // TODO: Error
  //        case Float.NaN => ??? // TODO: Error
  //        case f: Float => f.shortValue
  //      }
  //      case NodeInfo.Double => sv.asInstanceOf[Double] match {
  //        case Double.PositiveInfinity => ??? // TODO: Error
  //        case Double.NegativeInfinity => ??? // TODO: Error
  //        case Double.NaN => ??? // TODO: Error
  //        case d: Double => d.shortValue
  //      }
  //      case NodeInfo.Boolean => sv.asInstanceOf[Boolean] match {
  //        case true => 1.toShort
  //        case false => 0.toShort
  //      }
  //      case NodeInfo.String => sv.asInstanceOf[String].toShort
  //      case _ => ??? // TODO: Error
  //    }
  //    res
  //  }
  //
  //  def convertToBoolean(st: NodeInfo.Kind, sv: Any): Boolean = {
  //    val res = st match {
  //      case NodeInfo.Boolean => sv.asInstanceOf[Boolean]
  //      case NodeInfo.Integer => {
  //        val bi = sv.asInstanceOf[BigInt]
  //        if (bi == 0) false else true
  //      }
  //      case NodeInfo.Int => {
  //        val i = sv.asInstanceOf[Int]
  //        if (i == 0) false else true
  //      }
  //      case NodeInfo.UnsignedInt => {
  //        val ui = sv.asInstanceOf[Long]
  //        if (ui == 0) false else true
  //      }
  //      case NodeInfo.Byte => {
  //        val b = sv.asInstanceOf[Byte]
  //        if (b == 0) false else true
  //      }
  //      case NodeInfo.UnsignedByte => {
  //        val ub = sv.asInstanceOf[Short]
  //        if (ub == 0) false else true
  //      }
  //      case NodeInfo.Short => {
  //        val s = sv.asInstanceOf[Short]
  //        if (s == 0) false else true
  //      }
  //      case NodeInfo.UnsignedShort => {
  //        val s = sv.asInstanceOf[Int]
  //        if (s == 0) false else true
  //      }
  //      case NodeInfo.Long => {
  //        val l = sv.asInstanceOf[Long]
  //        if (l == 0l) false else true
  //      }
  //      case NodeInfo.UnsignedLong => {
  //        val ul = sv.asInstanceOf[BigInt]
  //        if (ul == 0) false else true
  //      }
  //      case NodeInfo.Decimal => {
  //        val bd = sv.asInstanceOf[BigDecimal]
  //        if (bd == 0.0E0) false else true
  //      }
  //      case NodeInfo.Float => sv.asInstanceOf[Float] match {
  //        case Float.NaN => false
  //        case 0.0E0 => false
  //        case -0.0E0 => false
  //        case _ => true
  //      }
  //      case NodeInfo.Double => sv.asInstanceOf[Double] match {
  //        case Double.NaN => false
  //        case 0.0E0 => false
  //        case -0.0E0 => false
  //        case _ => true
  //      }
  //      case NodeInfo.String => sv.asInstanceOf[String].toBoolean
  //      case _ => ??? // TODO: Error
  //    }
  //    res
  //  }
  //
  //  def convertToString(st: NodeInfo.Kind, sv: Any): String = {
  //    val res = st match {
  //      case NodeInfo.String => sv.asInstanceOf[String]
  //      case NodeInfo.Integer => sv.asInstanceOf[BigInt].toString
  //      case NodeInfo.Short => sv.asInstanceOf[Short].toString
  //      case NodeInfo.Long => sv.asInstanceOf[Long].toString
  //      case NodeInfo.Byte => sv.asInstanceOf[Byte].toString
  //      case NodeInfo.Int => sv.asInstanceOf[Int].toString
  //      case NodeInfo.UnsignedByte => sv.asInstanceOf[Short].toString
  //      case NodeInfo.UnsignedLong => sv.asInstanceOf[BigInt].toString
  //      case NodeInfo.UnsignedInt => sv.asInstanceOf[Long].toString
  //      case NodeInfo.UnsignedShort => sv.asInstanceOf[Int].toString
  //      case NodeInfo.Decimal => sv.asInstanceOf[BigDecimal] match {
  //        case d if d.isWhole => convertToInteger(NodeInfo.Decimal, d).toString
  //        case d => d.toString()
  //      }
  //      case NodeInfo.Float => sv.asInstanceOf[Float].toString
  //      case NodeInfo.Double => sv.asInstanceOf[Double].toString
  //      case NodeInfo.HexBinary => {
  //        val bytes = sv.asInstanceOf[Array[Byte]]
  //        Misc.bytes2Hex(bytes)
  //      }
  //      case _ => Assert.invariantFailed("unsupported conversion")
  //    }
  //    res
  //  }
  //
  //  def convertToHexBinary(st: NodeInfo.Kind, sv: Any): Array[Byte] = {
  //    val res = st match {
  //      case NodeInfo.HexBinary => sv.asInstanceOf[Array[Byte]]
  //      case NodeInfo.String => {
  //        val str = sv.asInstanceOf[String]
  //        if (str.length % 2 != 0) {
  //          throw new NumberFormatException("A hexBinary value must contain an even number of characters")
  //        }
  //        val hex = Misc.hex2Bytes(str)
  //        hex
  //      }
  //      case _ => throw new NumberFormatException("Cannot convert from %s to %s".format(st, NodeInfo.HexBinary))
  //    }
  //    res
  //  }

}
