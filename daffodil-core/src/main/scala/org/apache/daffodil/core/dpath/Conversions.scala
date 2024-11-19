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

package org.apache.daffodil.core.dpath

import org.apache.daffodil.runtime1.dpath._

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
  def conversionOps(
    st: NodeInfo.Kind,
    tt: NodeInfo.Kind,
    context: Expression
  ): List[RecipeOp] = {
    import NodeInfo._
    val ops: List[RecipeOp] = (st, tt) match {
      case (_, Array) => Nil
      case (x, y) if (x == y) => Nil

      case (Nothing, x) => Nil
      case (x, AnyType) => Nil
      case (x: AnyAtomic.Kind, AnyAtomic) => Nil
      case (x: Numeric.Kind, String) => List(NumericToString)

      case (_: Numeric.Kind, Double) => List(NumericToDouble)
      case (Numeric, _) => NumericToString +: conversionOps(String, tt, context)
      //
      // Because of round-half-to-even, we need to know what the type of the original
      // argument is. Doing these auto-conversions to Decimal hide this.
      //
      // Valid Numeric types are xs:float, xs:double, xs:integer and any sub-types.
      //
      //      case (Float, n: Numeric.Kind) => FloatToDouble +: conversionOps(Double, tt, context)
      //      case (Integer, n: Numeric.Kind) => IntegerToDecimal +: conversionOps(Decimal, tt, context)
      //      case (Double, Numeric) => List(DoubleToDecimal)
      case (n: NodeInfo.Numeric.Kind, Numeric) => Nil

      case (x: AnyDateTime.Kind, String) => List(DateTimeToString)
      case (HexBinary, String) => List(HexBinaryToString)
      case (String, NonEmptyString) => Nil
      case (x, NonEmptyString) => conversionOps(st, String, context)
      case (x: AnyAtomic.Kind, String) => List(AnyAtomicToString)
      case (x, ArrayIndex) => conversionOps(st, Long, context) ++ List(LongToArrayIndex)

      case (Integer, Boolean) => IntegerToDecimal +: conversionOps(Decimal, tt, context)
      case (Decimal, Boolean) => List(DecimalToBoolean)
      case (Double, Boolean) => List(DoubleToBoolean)
      case (Float, Boolean) => FloatToDouble +: conversionOps(Double, tt, context)
      case (n: Numeric.Kind, Boolean) => conversionOps(n, Long, context) ++ List(LongToBoolean)

      case (Boolean, Double) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Decimal) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Float) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Integer) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, NonNegativeInteger) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Long) => List(BooleanToLong)
      case (Boolean, UnsignedLong) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Int) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, UnsignedInt) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Short) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, UnsignedShort) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, Byte) => BooleanToLong +: conversionOps(Long, tt, context)
      case (Boolean, UnsignedByte) => BooleanToLong +: conversionOps(Long, tt, context)

      case (Date, DateTime) => List(DateToDateTime)
      case (Double, Decimal) => List(DoubleToDecimal)
      case (Double, Float) => List(DoubleToFloat)
      case (Double, Integer) => DoubleToDecimal +: conversionOps(Decimal, tt, context)
      case (Double, NonNegativeInteger) =>
        DoubleToDecimal +: conversionOps(Decimal, tt, context)
      case (Double, Long) => List(DoubleToLong)
      case (Double, UnsignedLong) => List(DoubleToUnsignedLong)
      case (Double, i: Int.Kind) => DoubleToLong +: conversionOps(Long, tt, context)
      case (Double, ui: UnsignedInt.Kind) =>
        DoubleToUnsignedLong +: UnsignedLongToLong +: conversionOps(Long, tt, context)

      case (Float, n: Numeric.Kind) => FloatToDouble +: conversionOps(Double, tt, context)

      case (Decimal, Integer) => List(DecimalToInteger)
      case (Decimal, NonNegativeInteger) => List(DecimalToNonNegativeInteger)
      case (Decimal, Float) => DecimalToDouble +: conversionOps(Double, tt, context)
      case (Decimal, Long) => List(DecimalToLong)
      case (Decimal, UnsignedLong) => List(DecimalToUnsignedLong)
      case (Decimal, Int) => List(DecimalToLong, LongToInt)
      case (Decimal, UnsignedInt) => List(DecimalToLong, LongToUnsignedInt)
      case (Decimal, Short) => List(DecimalToLong, LongToShort)
      case (Decimal, UnsignedShort) => List(DecimalToLong, LongToUnsignedShort)
      case (Decimal, Byte) => List(DecimalToLong, LongToByte)
      case (Decimal, UnsignedByte) => List(DecimalToLong, LongToUnsignedByte)

      case (Integer, n: NodeInfo.Numeric.Kind) =>
        IntegerToDecimal +: conversionOps(Decimal, tt, context)

      case (NonNegativeInteger, n: Numeric.Kind) =>
        IntegerToDecimal +: conversionOps(Decimal, tt, context)
      // Commented these out because these are illegal conversions for XPath functions.
      // I cannot pass a DateTime as a Date argument to an XPath function.
      //
      // case (DateTime, Date) => List(DateTimeToDate)
      // case (DateTime, Time) => List(DateTimeToTime)
      // case (Time, DateTime) => List(TimeToDateTime)
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
      case (String, HexBinary) => List(StringToHexBinary)
      case (String, Date) => List(StringToDate)
      case (String, Time) => List(StringToTime)
      case (String, DateTime) => List(StringToDateTime)
      case (String, Boolean) => List(StringToBoolean)
      case (Byte, Long) => List(ByteToLong)

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
      case (Long, Float) => LongToDouble +: conversionOps(Double, tt, context)

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

      case (UnsignedInt, Decimal) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Float) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Integer) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, NonNegativeInteger) =>
        UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Long) => List(UnsignedIntToLong)
      case (UnsignedInt, UnsignedLong) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Int) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Short) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, UnsignedShort) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, Byte) => UnsignedIntToLong +: conversionOps(Long, tt, context)
      case (UnsignedInt, UnsignedByte) => UnsignedIntToLong +: conversionOps(Long, tt, context)

      case (UnsignedLong, Decimal) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Float) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Integer) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, NonNegativeInteger) =>
        UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Long) => List(UnsignedLongToLong)
      case (UnsignedLong, Int) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Short) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, UnsignedShort) =>
        UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, Byte) => UnsignedLongToLong +: conversionOps(Long, tt, context)
      case (UnsignedLong, UnsignedByte) =>
        UnsignedLongToLong +: conversionOps(Long, tt, context)

      case (ArrayIndex, Decimal) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Float) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Integer) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, NonNegativeInteger) =>
        ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Long) => List(ArrayIndexToLong)
      case (ArrayIndex, UnsignedLong) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Int) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, UnsignedInt) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Short) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, UnsignedShort) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, Byte) => ArrayIndexToLong +: conversionOps(Long, tt, context)
      case (ArrayIndex, UnsignedByte) => ArrayIndexToLong +: conversionOps(Long, tt, context)

      case (Short, Decimal) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, Float) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, Integer) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, NonNegativeInteger) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, Long) => List(ShortToLong)
      case (Short, UnsignedLong) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, Int) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, UnsignedInt) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, UnsignedShort) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, Byte) => ShortToLong +: conversionOps(Long, tt, context)
      case (Short, UnsignedByte) => ShortToLong +: conversionOps(Long, tt, context)

      case (UnsignedShort, Decimal) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Float) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Integer) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, NonNegativeInteger) =>
        UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Long) => List(UnsignedShortToLong)
      case (UnsignedShort, UnsignedLong) =>
        UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Int) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, UnsignedInt) =>
        UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Short) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, Byte) => UnsignedShortToLong +: conversionOps(Long, tt, context)
      case (UnsignedShort, UnsignedByte) =>
        UnsignedShortToLong +: conversionOps(Long, tt, context)

      case (Byte, Decimal) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, Float) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, Integer) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, NonNegativeInteger) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, UnsignedLong) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, Int) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, UnsignedInt) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, Short) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, UnsignedShort) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, Byte) => ByteToLong +: conversionOps(Long, tt, context)
      case (Byte, UnsignedByte) => ByteToLong +: conversionOps(Long, tt, context)

      case (UnsignedByte, Decimal) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Float) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Integer) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, NonNegativeInteger) =>
        UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Long) => List(UnsignedByteToLong)
      case (UnsignedByte, UnsignedLong) =>
        UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Int) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, UnsignedInt) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Short) => UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, UnsignedShort) =>
        UnsignedByteToLong +: conversionOps(Long, tt, context)
      case (UnsignedByte, Byte) => UnsignedByteToLong +: conversionOps(Long, tt, context)

      // Note: These are explicitly not allowed
      //      case (AnyAtomic, Time) => AnyAtomicToString +: conversionOps(String, tt, context)
      //      case (AnyAtomic, Date) => AnyAtomicToString +: conversionOps(String, tt, context)
      //      case (AnyAtomic, DateTime) => AnyAtomicToString +: conversionOps(String, tt, context)

      // All of the 'types' should fall under AnyAtomic and there
      // is no need to convert to an AnyAtomic.  So anything converted to AnyAtomic
      // should be itself.
      //
      case (_, AnyAtomic) => Nil

      case (_, Exists) => Nil
      case (_, other) => {
        context.SDE(
          "In expression %s, the type %s cannot be converted to %s.",
          context.wholeExpressionText,
          st.globalQName.toQNameString,
          tt.globalQName.toQNameString
        )
      }
    }
    ops
  }

}
