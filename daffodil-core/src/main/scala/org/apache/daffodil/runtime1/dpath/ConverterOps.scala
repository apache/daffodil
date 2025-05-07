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

import java.lang.{ Boolean => JBoolean }
import java.lang.{ Byte => JByte }
import java.lang.{ Double => JDouble }
import java.lang.{ Float => JFloat }
import java.lang.{ Integer => JInt }
import java.lang.{ Long => JLong }
import java.lang.{ Short => JShort }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.lib.calendar.DFDLDate
import org.apache.daffodil.lib.calendar.DFDLDateTime
import org.apache.daffodil.lib.util.Numbers.asBigInt
import org.apache.daffodil.lib.util.Numbers.asBoolean
import org.apache.daffodil.lib.util.Numbers.asDouble
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBigDecimal
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBigInt
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBool
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueByte
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDate
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDateTime
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDouble
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueFloat
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueInt
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueLong
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueShort
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueString
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueTime

case object BooleanToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong =
    JLong.valueOf(if (asBoolean(a.getAnyRef) == true) 1L else 0L)
}

case object BooleanToString extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueString =
    if (asBoolean(a.getAnyRef) == true) "true" else "false"
}

case object DateTimeToDate extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDate = {
    a.getAnyRef match {
      case dt: DFDLDateTime => dt.toDate()
      case _ =>
        throw new NumberFormatException(
          "xs:dateTime expected but an invalid type was received."
        )
    }
  }
}
case object DateTimeToTime extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueTime = {
    a.getAnyRef match {
      case dt: DFDLDateTime => dt.toTime()
      case _ =>
        throw new NumberFormatException(
          "xs:dateTime expected but an invalid type was received."
        )
    }
  }
}
case object DateToDateTime extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDateTime = {
    a.getAnyRef match {
      case d: DFDLDate => d.toDateTime()
      case _ =>
        throw new NumberFormatException("xs:date expected but an invalid type was received.")
    }
  }
}
case object DecimalToInteger extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt =
    PrimType.Integer.fromNumber(a.getBigDecimal).getBigInt
}
case object DecimalToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong =
    PrimType.Long.fromNumber(a.getBigDecimal).getLong
}
case object DecimalToDouble extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDouble =
    PrimType.Double.fromNumber(a.getBigDecimal).getDouble
}
case object DecimalToNonNegativeInteger extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt =
    PrimType.NonNegativeInteger.fromNumber(a.getBigDecimal).getBigInt
}
case object DecimalToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt =
    PrimType.UnsignedLong.fromNumber(a.getBigDecimal).getBigInt
}
case object DecimalToBoolean extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBool = {
    val d = a.getBigDecimal
    val comp = d.compareTo(JBigDecimal.ZERO)
    val b =
      if (comp == 0) false
      else true
    asBoolean(b)
  }
}
case object DoubleToDecimal extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigDecimal =
    PrimType.Decimal.fromNumber(a.getDouble).getBigDecimal
}
case object DoubleToFloat extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueFloat =
    PrimType.Float.fromNumber(a.getDouble).getFloat
}
case object DoubleToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong =
    PrimType.Long.fromNumber(a.getDouble).getLong
}
case object DoubleToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt =
    asBigInt(PrimType.UnsignedLong.fromNumber(a.getDouble).getAnyRef)
}
case object DoubleToBoolean extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBool = {
    val d = a.getDouble
    val b =
      if (d == 0.0) false
      else if (d.isNaN()) false
      else true
    asBoolean(b)
  }
}
case object FloatToDouble extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDouble =
    PrimType.Double.fromNumber(a.getFloat).getDouble
}
case object IntegerToDecimal extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigDecimal =
    PrimType.Decimal.fromNumber(a.getBigInt).getBigDecimal
}
case object IntegerToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt =
    PrimType.UnsignedLong.fromNumber(a.getBigInt).getBigInt
}
case object LongToBoolean extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBool =
    asBoolean(if (a.getLong == 0) false else true)
}
case object LongToByte extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueByte =
    PrimType.Byte.fromNumber(a.getLong).getByte
}
case object LongToDecimal extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigDecimal =
    PrimType.Decimal.fromNumber(a.getLong).getBigDecimal
}
case object LongToDouble extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDouble =
    PrimType.Double.fromNumber(a.getLong).getDouble
}
case object LongToFloat extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueFloat =
    PrimType.Float.fromNumber(a.getLong).getFloat
}
case object LongToInt extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueInt =
    PrimType.Int.fromNumber(a.getLong).getInt
}

case object LongToInteger extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt =
    PrimType.Integer.fromNumber(a.getLong).getBigInt
}

case object LongToShort extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueShort =
    PrimType.Short.fromNumber(a.getLong).getShort
}

case object LongToArrayIndex extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong =
    PrimType.Long.fromNumber(a.getLong).getLong
}
case object LongToUnsignedByte extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueShort =
    PrimType.UnsignedByte.fromNumber(a.getLong).getShort
}
case object LongToUnsignedInt extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong =
    PrimType.UnsignedInt.fromNumber(a.getLong).getLong
}
case object LongToUnsignedShort extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueInt =
    PrimType.UnsignedShort.fromNumber(a.getLong).getInt
}

case object LongToNonNegativeInteger extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt =
    PrimType.NonNegativeInteger.fromNumber(a.getLong).getBigInt
}

case object LongToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt =
    PrimType.UnsignedLong.fromNumber(a.getLong).getBigInt
}

case object NumericToDouble extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDouble =
    asDouble(a.getAnyRef)
}

case object StringToBoolean extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBool =
    PrimType.Boolean.fromXMLString(a.getString).getBoolean
}
case object StringToDecimal extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigDecimal =
    PrimType.Decimal.fromXMLString(a.getString).getBigDecimal
}
case object StringToDouble extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDouble =
    PrimType.Double.fromXMLString(a.getString).getDouble
}
case object StringToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong =
    PrimType.Long.fromXMLString(a.getString).getLong
}
case object StringToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt =
    PrimType.UnsignedLong.fromXMLString(a.getString).getBigInt
}

/**
 * Summary: Computes the effective boolean value of the sequence \$arg.
 *
 * If \$arg is the empty sequence, fn:boolean returns false.
 *
 * If \$arg is a sequence whose first item is a node, fn:boolean returns true.
 *
 * If \$arg is a singleton value of type xs:boolean or a derived from
 * xs:boolean, fn:boolean returns \$arg.
 *
 * If \$arg is a singleton value of type xs:string or a type derived from
 * xs:string, xs:anyURI or a type derived from xs:anyURI or xs:untypedAtomic,
 * fn:boolean returns false if the operand value has zero length; otherwise
 * it returns true.
 *
 * If \$arg is a singleton value of any numeric type or a type derived
 * from a numeric type, fn:boolean returns false if the operand value
 * is NaN or is numerically equal to zero; otherwise it returns true.
 *
 * In all other cases, fn:boolean raises a type error [err:FORG0006].
 */
case object FNToBoolean extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBool = {
    val res = a.getAnyRef match {
      case b: JBoolean => b.booleanValue()
      case s: String => if (s.length == 0) false else true
      case d: JDouble => if (d.isNaN() || d == 0) false else true
      case f: JFloat => if (f.isNaN() || f == 0) false else true
      //
      // BigDecimal does not have a representation for NaN or Infinite
      case bd: JBigDecimal => if (bd.compareTo(JBigDecimal.ZERO) == 0) false else true
      case b: JByte => if (b == 0) false else true
      case s: JShort => if (s == 0) false else true
      case i: JInt => if (i == 0) false else true
      case l: JLong => if (l == 0) false else true
      case bi: JBigInt => if (bi.compareTo(JBigInt.ZERO) == 0) false else true
      // TODO: Once sequences are supported, fill in these case statements
      // case s: LocalSequence if s.length == 0 => false
      // case s: LocalSequence if s(0) == Node => true
      case _ => throw new NumberFormatException("Invalid argument type.")
    }
    res
  }
}
