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

import java.lang.{ Boolean => JBoolean }
import org.apache.daffodil.util.Numbers._
import org.apache.daffodil.calendar.DFDLDateTime
import org.apache.daffodil.calendar.DFDLTime
import org.apache.daffodil.calendar.DFDLDate

case object EQ_Compare extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = {
    val res = v1 == v2
    asBoolean(res)
  }
}

case object EQ_CompareByteArray extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = {
    val res = v1.asInstanceOf[Array[Byte]].sameElements(v2.asInstanceOf[Array[Byte]])
    asBoolean(res)
  }
}

case object NE_Compare extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = {
    val res = v1 != v2
    asBoolean(res)
  }
}

case object NE_CompareByteArray extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = {
    val res = !v1.asInstanceOf[Array[Byte]].sameElements(v2.asInstanceOf[Array[Byte]])
    asBoolean(res)
  }
}

case object LT_Boolean extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = {
    val b1 = asBoolean(v1)
    val b2 = asBoolean(v2)

    val res = (!b1 && b2)
    asBoolean(res)
  }
}
case object GT_Boolean extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = {
    val b1 = asBoolean(v1)
    val b2 = asBoolean(v2)

    val res = (b1 && !b2)
    asBoolean(res)
  }
}
case object LE_Boolean extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { !GT_Boolean.operate(v1, v2) }
}
case object GE_Boolean extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { !LT_Boolean.operate(v1, v2) }
}

case object LT_Date extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLDate] < v2.asInstanceOf[DFDLDate] }
}
case object GT_Date extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLDate] > v2.asInstanceOf[DFDLDate] }
}
case object LE_Date extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLDate] <= v2.asInstanceOf[DFDLDate] }
}
case object GE_Date extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLDate] >= v2.asInstanceOf[DFDLDate] }
}
case object LT_Time extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLTime] < v2.asInstanceOf[DFDLTime] }
}
case object GT_Time extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLTime] > v2.asInstanceOf[DFDLTime] }
}
case object LE_Time extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLTime] <= v2.asInstanceOf[DFDLTime] }
}
case object GE_Time extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLTime] >= v2.asInstanceOf[DFDLTime] }
}
case object LT_DateTime extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLDateTime] < v2.asInstanceOf[DFDLDateTime] }
}
case object GT_DateTime extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLDateTime] > v2.asInstanceOf[DFDLDateTime] }
}
case object LE_DateTime extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLDateTime] <= v2.asInstanceOf[DFDLDateTime] }
}
case object GE_DateTime extends CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { v1.asInstanceOf[DFDLDateTime] >= v2.asInstanceOf[DFDLDateTime] }
}

case object LT_String extends StringCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = {
    val res = compare(v1, v2) < 0
    res
  }
}
case object GT_String extends StringCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = {
    val res = compare(v1, v2) > 0
    res
  }
}
case object LE_String extends StringCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = {
    val res = compare(v1, v2) <= 0
    res
  }
}
case object GE_String extends StringCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = {
    val res = compare(v1, v2) >= 0
    res
  }
}
case object LT_Decimal extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigDecimal(v1).compareTo(asBigDecimal(v2)) == -1 }
}
case object GT_Decimal extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigDecimal(v1).compareTo(asBigDecimal(v2)) == 1 }
}
case object LE_Decimal extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigDecimal(v1).compareTo(asBigDecimal(v2)) <= 0 }
}
case object GE_Decimal extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigDecimal(v1).compareTo(asBigDecimal(v2)) >= 0 }
}
case object LT_Integer extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) == -1 }
}
case object GT_Integer extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) == 1 }
}
case object LE_Integer extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) <= 0 }
}
case object GE_Integer extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) >= 0 }
}
case object LT_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) == -1 }
}
case object GT_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) == 1 }
}
case object LE_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) <= 0 }
}
case object GE_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) >= 0 }
}
case object LT_UnsignedLong extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) == -1 }
}
case object GT_UnsignedLong extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) == 1 }
}
case object LE_UnsignedLong extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) <= 0 }
}
case object GE_UnsignedLong extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asBigInt(v1).compareTo(asBigInt(v2)) >= 0 }
}
case object LT_Long extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asLong(v1) < asLong(v2) }
}
case object GT_Long extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asLong(v1) > asLong(v2) }
}
case object LE_Long extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asLong(v1) <= asLong(v2) }
}
case object GE_Long extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asLong(v1) >= asLong(v2) }
}
case object LT_UnsignedInt extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asLong(v1) < asLong(v2) }
}
case object GT_UnsignedInt extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asLong(v1) > asLong(v2) }
}
case object LE_UnsignedInt extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asLong(v1) <= asLong(v2) }
}
case object GE_UnsignedInt extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asLong(v1) >= asLong(v2) }
}
case object LT_Int extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asInt(v1) < asInt(v2) }
}
case object GT_Int extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asInt(v1) > asInt(v2) }
}
case object LE_Int extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asInt(v1) <= asInt(v2) }
}
case object GE_Int extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asInt(v1) >= asInt(v2) }
}
case object LT_UnsignedShort extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asInt(v1) < asInt(v2) }
}
case object GT_UnsignedShort extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asInt(v1) > asInt(v2) }
}
case object LE_UnsignedShort extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asInt(v1) <= asInt(v2) }
}
case object GE_UnsignedShort extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asInt(v1) >= asInt(v2) }
}
case object LT_Short extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asShort(v1) < asShort(v2) }
}
case object GT_Short extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asShort(v1) > asShort(v2) }
}
case object LE_Short extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asShort(v1) <= asShort(v2) }
}
case object GE_Short extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asShort(v1) >= asShort(v2) }
}
case object LT_UnsignedByte extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asShort(v1) < asShort(v2) }
}
case object GT_UnsignedByte extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asShort(v1) > asShort(v2) }
}
case object LE_UnsignedByte extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asShort(v1) <= asShort(v2) }
}
case object GE_UnsignedByte extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asShort(v1) >= asShort(v2) }
}
case object LT_Byte extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asByte(v1) < asByte(v2) }
}
case object GT_Byte extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asByte(v1) > asByte(v2) }
}
case object LE_Byte extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asByte(v1) <= asByte(v2) }
}
case object GE_Byte extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asByte(v1) >= asByte(v2) }
}
case object LT_Float extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asFloat(v1) < asFloat(v2) }
}
case object GT_Float extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asFloat(v1) > asFloat(v2) }
}
case object LE_Float extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asFloat(v1) <= asFloat(v2) }
}
case object GE_Float extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asFloat(v1) >= asFloat(v2) }
}
case object LT_Double extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asDouble(v1) < asDouble(v2) }
}
case object GT_Double extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asDouble(v1) > asDouble(v2) }
}
case object LE_Double extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asDouble(v1) <= asDouble(v2) }
}
case object GE_Double extends NumberCompareOp {
  def operate(v1: AnyRef, v2: AnyRef): JBoolean = { asDouble(v1) >= asDouble(v2) }
}
