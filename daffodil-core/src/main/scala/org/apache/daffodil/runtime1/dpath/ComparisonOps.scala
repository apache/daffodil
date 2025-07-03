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

package org.apache.daffodil.runtime1.dpath

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Numbers._
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBool
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive

/**
 * Case class used for ordering the return of the appropriate comparison operations for each primitive kind
 * @param eq represents the equality comparison
 * @param ne represents the unequality comparison
 * @param lt represents the less than comparison
 * @param le represents the less than or equal to comparison
 * @param gt represents the greater than comparison
 * @param ge represents the greater than or equal to comparison
 */
case class ComparisonOp(
  eq: CompareOpBase,
  ne: CompareOpBase,
  lt: CompareOpBase,
  le: CompareOpBase,
  gt: CompareOpBase,
  ge: CompareOpBase
)

/**
 * ComparisonOps.forType represents a map with key of NodeInfo.Kind and the value of the above ComparisonOp case class, which
 * is a 6 param object containing the appropriate comparison object for the NodeInfo.Kind
 *
 * To use, you can call the forType map using an argument targetType, which queries the Map using the key targetType
 * then use the returned object to select the ComparisonOpBase of interest
 *
 * @example {{{
 *  val compOps = ComparisonOps.forType(targetType)
 *  val res = compOps.lt.operate(x, y)
 *  res
 * }}}
 *
 */
object ComparisonOps {
  lazy val forType: Map[NodeInfo.Kind, ComparisonOp] = {
    Map(
      PrimType.Boolean -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_Boolean,
        LE_Boolean,
        GT_Boolean,
        GE_Boolean
      ),
      PrimType.Integer -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_Integer,
        LE_Integer,
        GT_Integer,
        GE_Integer
      ),
      PrimType.Date -> ComparisonOp(EQ_Compare, NE_Compare, LT_Date, LE_Date, GT_Date, GE_Date),
      PrimType.Time -> ComparisonOp(EQ_Compare, NE_Compare, LT_Time, LE_Time, GT_Time, GE_Time),
      PrimType.DateTime -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_DateTime,
        LE_DateTime,
        GT_DateTime,
        GE_DateTime
      ),
      PrimType.String -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_String,
        LE_String,
        GT_String,
        GE_String
      ),
      PrimType.Decimal -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_Decimal,
        LE_Decimal,
        GT_Decimal,
        GE_Decimal
      ),
      PrimType.NonNegativeInteger -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_NonNegativeInteger,
        LE_NonNegativeInteger,
        GT_NonNegativeInteger,
        GE_NonNegativeInteger
      ),
      PrimType.UnsignedLong -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_UnsignedLong,
        LE_UnsignedLong,
        GT_UnsignedLong,
        GE_UnsignedLong
      ),
      PrimType.Long -> ComparisonOp(EQ_Compare, NE_Compare, LT_Long, LE_Long, GT_Long, GE_Long),
      PrimType.UnsignedInt -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_UnsignedInt,
        LE_UnsignedInt,
        GT_UnsignedInt,
        GE_UnsignedInt
      ),
      NodeInfo.ArrayIndex -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_UnsignedInt,
        LE_UnsignedInt,
        GT_UnsignedInt,
        GE_UnsignedInt
      ),
      PrimType.Int -> ComparisonOp(EQ_Compare, NE_Compare, LT_Int, LE_Int, GT_Int, GE_Int),
      PrimType.UnsignedShort -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_UnsignedShort,
        LE_UnsignedShort,
        GT_UnsignedShort,
        GE_UnsignedShort
      ),
      PrimType.Short -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_Short,
        LE_Short,
        GT_Short,
        GE_Short
      ),
      PrimType.UnsignedByte -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_UnsignedByte,
        LE_UnsignedByte,
        GT_UnsignedByte,
        GE_UnsignedByte
      ),
      PrimType.Byte -> ComparisonOp(EQ_Compare, NE_Compare, LT_Byte, LE_Byte, GT_Byte, GE_Byte),
      PrimType.Float -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_Float,
        LE_Float,
        GT_Float,
        GE_Float
      ),
      PrimType.Double -> ComparisonOp(
        EQ_Compare,
        NE_Compare,
        LT_Double,
        LE_Double,
        GT_Double,
        GE_Double
      ),
      PrimType.HexBinary -> ComparisonOp(
        EQ_CompareByteArray,
        NE_CompareByteArray,
        LT_ByteArray,
        LE_ByteArray,
        GT_ByteArray,
        GE_ByteArray
      )
    )
  }
}

case object EQ_Compare extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    val res = v1 == v2
    asBoolean(res)
  }
}

case object EQ_CompareByteArray extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    val res = v1.getByteArray.sameElements(v2.getByteArray)
    asBoolean(res)
  }
}

case object LT_ByteArray extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    Assert.usageError("Unsupported operation LT on Byte Array")
  }
}

case object LE_ByteArray extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    Assert.usageError("Unsupported operation LE on Byte Array")
  }
}

case object GT_ByteArray extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    Assert.usageError("Unsupported operation GT on Byte Array")
  }
}

case object GE_ByteArray extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    Assert.usageError("Unsupported operation GE on Byte Array")
  }
}

case object NE_Compare extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    val res = v1 != v2
    asBoolean(res)
  }
}

case object NE_CompareByteArray extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    val res = !v1.getByteArray.sameElements(v2.getByteArray)
    asBoolean(res)
  }
}

case object LT_Boolean extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    val b1 = asBoolean(v1.getAnyRef)
    val b2 = asBoolean(v2.getAnyRef)

    val res = (!b1 && b2)
    asBoolean(res)
  }
}
case object GT_Boolean extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    val b1 = asBoolean(v1.getAnyRef)
    val b2 = asBoolean(v2.getAnyRef)

    val res = (b1 && !b2)
    asBoolean(res)
  }
}
case object LE_Boolean extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    !GT_Boolean.operate(v1, v2).getBoolean
  }
}
case object GE_Boolean extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    !LT_Boolean.operate(v1, v2).getBoolean
  }
}

case object LT_Date extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getDate < v2.getDate
  }
}
case object GT_Date extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getDate > v2.getDate
  }
}
case object LE_Date extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getDate <= v2.getDate
  }
}
case object GE_Date extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getDate >= v2.getDate
  }
}
case object LT_Time extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getTime < v2.getTime
  }
}
case object GT_Time extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getTime > v2.getTime
  }
}
case object LE_Time extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getTime <= v2.getTime
  }
}
case object GE_Time extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getTime >= v2.getTime
  }
}
case object LT_DateTime extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getDateTime < v2.getDateTime
  }
}
case object GT_DateTime extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getDateTime > v2.getDateTime
  }
}
case object LE_DateTime extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getDateTime <= v2.getDateTime
  }
}
case object GE_DateTime extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    v1.getDateTime >= v2.getDateTime
  }
}

case object LT_String extends StringCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    val res = compare(v1, v2) < 0
    res
  }
}
case object GT_String extends StringCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    val res = compare(v1, v2) > 0
    res
  }
}
case object LE_String extends StringCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    val res = compare(v1, v2) <= 0
    res
  }
}
case object GE_String extends StringCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    val res = compare(v1, v2) >= 0
    res
  }
}
case object LT_Decimal extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigDecimal(v1.getAnyRef).compareTo(asBigDecimal(v2.getAnyRef)) == -1
  }
}
case object GT_Decimal extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigDecimal(v1.getAnyRef).compareTo(asBigDecimal(v2.getAnyRef)) == 1
  }
}
case object LE_Decimal extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigDecimal(v1.getAnyRef).compareTo(asBigDecimal(v2.getAnyRef)) <= 0
  }
}
case object GE_Decimal extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigDecimal(v1.getAnyRef).compareTo(asBigDecimal(v2.getAnyRef)) >= 0
  }
}
case object LT_Integer extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == -1
  }
}
case object GT_Integer extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == 1
  }
}
case object LE_Integer extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) <= 0
  }
}
case object GE_Integer extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) >= 0
  }
}
case object LT_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == -1
  }
}
case object GT_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == 1
  }
}
case object LE_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) <= 0
  }
}
case object GE_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) >= 0
  }
}
case object LT_UnsignedLong extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == -1
  }
}
case object GT_UnsignedLong extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == 1
  }
}
case object LE_UnsignedLong extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) <= 0
  }
}
case object GE_UnsignedLong extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) >= 0
  }
}
case object LT_Long extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asLong(v1.getAnyRef) < asLong(v2.getAnyRef)
  }
}
case object GT_Long extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asLong(v1.getAnyRef) > asLong(v2.getAnyRef)
  }
}
case object LE_Long extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asLong(v1.getAnyRef) <= asLong(v2.getAnyRef)
  }
}
case object GE_Long extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asLong(v1.getAnyRef) >= asLong(v2.getAnyRef)
  }
}
case object LT_UnsignedInt extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asLong(v1.getAnyRef) < asLong(v2.getAnyRef)
  }
}
case object GT_UnsignedInt extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asLong(v1.getAnyRef) > asLong(v2.getAnyRef)
  }
}
case object LE_UnsignedInt extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asLong(v1.getAnyRef) <= asLong(v2.getAnyRef)
  }
}
case object GE_UnsignedInt extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asLong(v1.getAnyRef) >= asLong(v2.getAnyRef)
  }
}
case object LT_Int extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asInt(v1.getAnyRef) < asInt(v2.getAnyRef)
  }
}
case object GT_Int extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asInt(v1.getAnyRef) > asInt(v2.getAnyRef)
  }
}
case object LE_Int extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asInt(v1.getAnyRef) <= asInt(v2.getAnyRef)
  }
}
case object GE_Int extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asInt(v1.getAnyRef) >= asInt(v2.getAnyRef)
  }
}
case object LT_UnsignedShort extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asInt(v1.getAnyRef) < asInt(v2.getAnyRef)
  }
}
case object GT_UnsignedShort extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asInt(v1.getAnyRef) > asInt(v2.getAnyRef)
  }
}
case object LE_UnsignedShort extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asInt(v1.getAnyRef) <= asInt(v2.getAnyRef)
  }
}
case object GE_UnsignedShort extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asInt(v1.getAnyRef) >= asInt(v2.getAnyRef)
  }
}
case object LT_Short extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asShort(v1.getAnyRef) < asShort(v2.getAnyRef)
  }
}
case object GT_Short extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asShort(v1.getAnyRef) > asShort(v2.getAnyRef)
  }
}
case object LE_Short extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asShort(v1.getAnyRef) <= asShort(v2.getAnyRef)
  }
}
case object GE_Short extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asShort(v1.getAnyRef) >= asShort(v2.getAnyRef)
  }
}
case object LT_UnsignedByte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asShort(v1.getAnyRef) < asShort(v2.getAnyRef)
  }
}
case object GT_UnsignedByte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asShort(v1.getAnyRef) > asShort(v2.getAnyRef)
  }
}
case object LE_UnsignedByte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asShort(v1.getAnyRef) <= asShort(v2.getAnyRef)
  }
}
case object GE_UnsignedByte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asShort(v1.getAnyRef) >= asShort(v2.getAnyRef)
  }
}
case object LT_Byte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asByte(v1.getAnyRef) < asByte(v2.getAnyRef)
  }
}
case object GT_Byte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asByte(v1.getAnyRef) > asByte(v2.getAnyRef)
  }
}
case object LE_Byte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asByte(v1.getAnyRef) <= asByte(v2.getAnyRef)
  }
}
case object GE_Byte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asByte(v1.getAnyRef) >= asByte(v2.getAnyRef)
  }
}
case object LT_Float extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asFloat(v1.getAnyRef) < asFloat(v2.getAnyRef)
  }
}
case object GT_Float extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asFloat(v1.getAnyRef) > asFloat(v2.getAnyRef)
  }
}
case object LE_Float extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asFloat(v1.getAnyRef) <= asFloat(v2.getAnyRef)
  }
}
case object GE_Float extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asFloat(v1.getAnyRef) >= asFloat(v2.getAnyRef)
  }
}
case object LT_Double extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asDouble(v1.getAnyRef) < asDouble(v2.getAnyRef)
  }
}
case object GT_Double extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asDouble(v1.getAnyRef) > asDouble(v2.getAnyRef)
  }
}
case object LE_Double extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asDouble(v1.getAnyRef) <= asDouble(v2.getAnyRef)
  }
}
case object GE_Double extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = {
    asDouble(v1.getAnyRef) >= asDouble(v2.getAnyRef)
  }
}
