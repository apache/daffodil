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

package org.apache.daffodil.dpath

import org.apache.daffodil.util.Numbers._
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValueBool

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
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { !GT_Boolean.operate(v1, v2).getBoolean }
}
case object GE_Boolean extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { !LT_Boolean.operate(v1, v2).getBoolean }
}

case object LT_Date extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getDate < v2.getDate }
}
case object GT_Date extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getDate > v2.getDate }
}
case object LE_Date extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getDate <= v2.getDate }
}
case object GE_Date extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getDate >= v2.getDate }
}
case object LT_Time extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getTime < v2.getTime }
}
case object GT_Time extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getTime > v2.getTime }
}
case object LE_Time extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getTime <= v2.getTime }
}
case object GE_Time extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getTime >= v2.getTime }
}
case object LT_DateTime extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getDateTime < v2.getDateTime }
}
case object GT_DateTime extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getDateTime > v2.getDateTime }
}
case object LE_DateTime extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getDateTime <= v2.getDateTime }
}
case object GE_DateTime extends CompareOpBase {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { v1.getDateTime >= v2.getDateTime }
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
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigDecimal(v1.getAnyRef).compareTo(asBigDecimal(v2.getAnyRef)) == -1 }
}
case object GT_Decimal extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigDecimal(v1.getAnyRef).compareTo(asBigDecimal(v2.getAnyRef)) == 1 }
}
case object LE_Decimal extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigDecimal(v1.getAnyRef).compareTo(asBigDecimal(v2.getAnyRef)) <= 0 }
}
case object GE_Decimal extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigDecimal(v1.getAnyRef).compareTo(asBigDecimal(v2.getAnyRef)) >= 0 }
}
case object LT_Integer extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == -1 }
}
case object GT_Integer extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == 1 }
}
case object LE_Integer extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) <= 0 }
}
case object GE_Integer extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) >= 0 }
}
case object LT_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == -1 }
}
case object GT_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == 1 }
}
case object LE_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) <= 0 }
}
case object GE_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) >= 0 }
}
case object LT_UnsignedLong extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == -1 }
}
case object GT_UnsignedLong extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) == 1 }
}
case object LE_UnsignedLong extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) <= 0 }
}
case object GE_UnsignedLong extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asBigInt(v1.getAnyRef).compareTo(asBigInt(v2.getAnyRef)) >= 0 }
}
case object LT_Long extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asLong(v1.getAnyRef) < asLong(v2.getAnyRef) }
}
case object GT_Long extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asLong(v1.getAnyRef) > asLong(v2.getAnyRef) }
}
case object LE_Long extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asLong(v1.getAnyRef) <= asLong(v2.getAnyRef) }
}
case object GE_Long extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asLong(v1.getAnyRef) >= asLong(v2.getAnyRef) }
}
case object LT_UnsignedInt extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asLong(v1.getAnyRef) < asLong(v2.getAnyRef) }
}
case object GT_UnsignedInt extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asLong(v1.getAnyRef) > asLong(v2.getAnyRef) }
}
case object LE_UnsignedInt extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asLong(v1.getAnyRef) <= asLong(v2.getAnyRef) }
}
case object GE_UnsignedInt extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asLong(v1.getAnyRef) >= asLong(v2.getAnyRef) }
}
case object LT_Int extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asInt(v1.getAnyRef) < asInt(v2.getAnyRef) }
}
case object GT_Int extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asInt(v1.getAnyRef) > asInt(v2.getAnyRef) }
}
case object LE_Int extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asInt(v1.getAnyRef) <= asInt(v2.getAnyRef) }
}
case object GE_Int extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asInt(v1.getAnyRef) >= asInt(v2.getAnyRef) }
}
case object LT_UnsignedShort extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asInt(v1.getAnyRef) < asInt(v2.getAnyRef) }
}
case object GT_UnsignedShort extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asInt(v1.getAnyRef) > asInt(v2.getAnyRef) }
}
case object LE_UnsignedShort extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asInt(v1.getAnyRef) <= asInt(v2.getAnyRef) }
}
case object GE_UnsignedShort extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asInt(v1.getAnyRef) >= asInt(v2.getAnyRef) }
}
case object LT_Short extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asShort(v1.getAnyRef) < asShort(v2.getAnyRef) }
}
case object GT_Short extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asShort(v1.getAnyRef) > asShort(v2.getAnyRef) }
}
case object LE_Short extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asShort(v1.getAnyRef) <= asShort(v2.getAnyRef) }
}
case object GE_Short extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asShort(v1.getAnyRef) >= asShort(v2.getAnyRef) }
}
case object LT_UnsignedByte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asShort(v1.getAnyRef) < asShort(v2.getAnyRef) }
}
case object GT_UnsignedByte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asShort(v1.getAnyRef) > asShort(v2.getAnyRef) }
}
case object LE_UnsignedByte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asShort(v1.getAnyRef) <= asShort(v2.getAnyRef) }
}
case object GE_UnsignedByte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asShort(v1.getAnyRef) >= asShort(v2.getAnyRef) }
}
case object LT_Byte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asByte(v1.getAnyRef) < asByte(v2.getAnyRef) }
}
case object GT_Byte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asByte(v1.getAnyRef) > asByte(v2.getAnyRef) }
}
case object LE_Byte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asByte(v1.getAnyRef) <= asByte(v2.getAnyRef) }
}
case object GE_Byte extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asByte(v1.getAnyRef) >= asByte(v2.getAnyRef) }
}
case object LT_Float extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asFloat(v1.getAnyRef) < asFloat(v2.getAnyRef) }
}
case object GT_Float extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asFloat(v1.getAnyRef) > asFloat(v2.getAnyRef) }
}
case object LE_Float extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asFloat(v1.getAnyRef) <= asFloat(v2.getAnyRef) }
}
case object GE_Float extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asFloat(v1.getAnyRef) >= asFloat(v2.getAnyRef) }
}
case object LT_Double extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asDouble(v1.getAnyRef) < asDouble(v2.getAnyRef) }
}
case object GT_Double extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asDouble(v1.getAnyRef) > asDouble(v2.getAnyRef) }
}
case object LE_Double extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asDouble(v1.getAnyRef) <= asDouble(v2.getAnyRef) }
}
case object GE_Double extends NumberCompareOp {
  def operate(v1: DataValuePrimitive, v2: DataValuePrimitive): DataValueBool = { asDouble(v1.getAnyRef) >= asDouble(v2.getAnyRef) }
}
