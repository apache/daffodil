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
import java.lang.{ Byte => JByte }
import java.lang.{ Double => JDouble }
import java.lang.{ Float => JFloat }
import java.lang.{ Integer => JInt }
import java.lang.{ Long => JLong }
import java.lang.{ Short => JShort }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.calendar.DFDLDate
import org.apache.daffodil.calendar.DFDLDateTime
import org.apache.daffodil.infoset.DataValue.DataValueBigDecimal
import org.apache.daffodil.infoset.DataValue.DataValueBigInt
import org.apache.daffodil.infoset.DataValue.DataValueBool
import org.apache.daffodil.infoset.DataValue.DataValueByte
import org.apache.daffodil.infoset.DataValue.DataValueDate
import org.apache.daffodil.infoset.DataValue.DataValueDateTime
import org.apache.daffodil.infoset.DataValue.DataValueDouble
import org.apache.daffodil.infoset.DataValue.DataValueFloat
import org.apache.daffodil.infoset.DataValue.DataValueInt
import org.apache.daffodil.infoset.DataValue.DataValueLong
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValueShort
import org.apache.daffodil.infoset.DataValue.DataValueString
import org.apache.daffodil.infoset.DataValue.DataValueTime
import org.apache.daffodil.util.Numbers.asBigDecimal
import org.apache.daffodil.util.Numbers.asBigInt
import org.apache.daffodil.util.Numbers.asBoolean
import org.apache.daffodil.util.Numbers.asByte
import org.apache.daffodil.util.Numbers.asDouble
import org.apache.daffodil.util.Numbers.asFloat
import org.apache.daffodil.util.Numbers.asInt
import org.apache.daffodil.util.Numbers.asLong
import org.apache.daffodil.util.Numbers.asShort
import org.apache.daffodil.xml.XMLUtils

case object BooleanToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = JLong.valueOf(if (asBoolean(a.getAnyRef) == true) 1L else 0L)
}

case object BooleanToString extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueString = if (asBoolean(a.getAnyRef) == true) "true" else "false"
}

case object DateTimeToDate extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDate = {
    a.getAnyRef match {
      case dt: DFDLDateTime => dt.toDate
      case _ => throw new NumberFormatException("xs:dateTime expected but an invalid type was received.")
    }
  }
}
case object DateTimeToTime extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueTime = {
    a.getAnyRef match {
      case dt: DFDLDateTime => dt.toTime
      case _ => throw new NumberFormatException("xs:dateTime expected but an invalid type was received.")
    }
  }
}
case object DateToDateTime extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDateTime = {
    a.getAnyRef match {
      case d: DFDLDate => d.toDateTime
      case _ => throw new NumberFormatException("xs:date expected but an invalid type was received.")
    }
  }
}
case object DecimalToInteger extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = asBigDecimal(a.getAnyRef).toBigInteger()
}
case object DecimalToLong extends Converter {
  val MAX_VALUE = JBigDecimal.valueOf(Long.MaxValue)
  val MIN_VALUE = JBigDecimal.valueOf(Long.MinValue)

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = {
    val res = asBigDecimal(a.getAnyRef)
    if (res.compareTo(MIN_VALUE) == -1 || res.compareTo(MAX_VALUE) == 1) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    asLong(res)
  }
}
case object DecimalToDouble extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDouble = asDouble(a.getAnyRef)
}
case object DecimalToNonNegativeInteger extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = {
    val res = asBigDecimal(a.getAnyRef)
    if (res.compareTo(JBigDecimal.ZERO) == -1) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))
    res.toBigInteger()
  }
}
case object DecimalToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = {
    val res = asBigDecimal(a.getAnyRef).toBigInteger()
    if (res.compareTo(JBigInt.ZERO) == -1) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))

    if (res.compareTo(NodeInfo.UnsignedLong.Max) == 1) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}
case object DecimalToBoolean extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBool = {
    val d = asBigDecimal(a.getAnyRef)
    val comp = d.compareTo(JBigDecimal.ZERO)
    val b =
      if (comp == 0) false
      else true
    asBoolean(b)
  }
}
case object DoubleToDecimal extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigDecimal = JBigDecimal.valueOf(asDouble(a.getAnyRef))
}
case object DoubleToFloat extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueFloat = {
    val f = asFloat(a.getAnyRef)
    f
  }
}
case object DoubleToLong extends Converter {
  val MAX_VALUE = JBigDecimal.valueOf(Long.MaxValue)
  val MIN_VALUE = JBigDecimal.valueOf(Long.MinValue)
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = {
    val res = asBigDecimal(a.getAnyRef)
    if (res.compareTo(MIN_VALUE) == -1 || res.compareTo(MAX_VALUE) == 1) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    asLong(a.getAnyRef)
  }
}
case object DoubleToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = {
    val res = asBigInt(a.getAnyRef)
    if (res.compareTo(JBigInt.ZERO) == -1) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned long.".format(res))
    if (res.compareTo(NodeInfo.UnsignedLong.Max) == 1) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}
case object DoubleToBoolean extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBool = {
    val d = asDouble(a.getAnyRef).doubleValue()
    val b =
      if (d == 0.0) false
      else if (d.isNaN()) false
      else true
    asBoolean(b)
  }
}
case object FloatToDouble extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDouble = asDouble(a.getAnyRef)
}
case object IntegerToDecimal extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigDecimal = new JBigDecimal(asBigInt(a.getAnyRef))
}
case object IntegerToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = {
    val res = asBigInt(a.getAnyRef)
    if (res.compareTo(JBigInt.ZERO) == -1) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned long.".format(res))
    if (res.compareTo(NodeInfo.UnsignedLong.Max) == 1) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}
case object LongToBoolean extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBool = asBoolean(if (asLong(a.getAnyRef) == 0) false else true)
}
case object LongToByte extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueByte = {
    val l = a.getLong
    if (l > Byte.MaxValue || l < Byte.MinValue) throw new NumberFormatException("Value %s out of range for Byte type.".format(l))
    asByte(l)
  }
}
case object LongToDecimal extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigDecimal = JBigDecimal.valueOf(asLong(a.getAnyRef))
}
case object LongToDouble extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDouble = asDouble(a.getAnyRef)
}
case object LongToFloat extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueFloat = asFloat(a.getAnyRef)
}
case object LongToInt extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueInt = {
    val l = asLong(a.getAnyRef).longValue()
    if (l > Int.MaxValue || l < Int.MinValue) throw new NumberFormatException("Value %s out of range for Int type.".format(l))
    asInt(a.getAnyRef)
  }
}

case object LongToInteger extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = JBigInt.valueOf(asLong(a.getAnyRef))
}

case object LongToShort extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueShort = {
    val l = asLong(a.getAnyRef).longValue()
    if (l > Short.MaxValue || l < Short.MinValue) throw new NumberFormatException("Value %s out of range for Short type.".format(l))
    asShort(a.getAnyRef)
  }
}

case object LongToArrayIndex extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = {
    val res = asLong(a.getAnyRef)
    res
  }
}
case object LongToUnsignedByte extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueShort = {
    val res = asLong(a.getAnyRef).longValue()
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned byte.".format(res))
    if (res > 255) throw new NumberFormatException("Value %s out of range for unsigned byte.".format(res))
    asShort(a.getAnyRef)
  }
}
case object LongToUnsignedInt extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = {
    val r = asLong(a.getAnyRef)
    val res = r.longValue()
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned int.".format(res))
    if (res > 0xFFFFFFFFL) throw new NumberFormatException("Value %s out of range for unsigned int.".format(res))
    r
  }
}
case object LongToUnsignedShort extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueInt = {
    val res = asLong(a.getAnyRef).longValue()
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned short.".format(res))
    if (res > 65535) throw new NumberFormatException("Value %s out of range for unsigned short.".format(res))
    asInt(a.getAnyRef)
  }
}

case object LongToNonNegativeInteger extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = {
    val res = JBigInt.valueOf(asLong(a.getAnyRef))
    if (res.compareTo(JBigInt.ZERO) == -1) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))
    res
  }
}

case object LongToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = {
    val res = JBigInt.valueOf(asLong(a.getAnyRef))
    if (res.compareTo(JBigInt.ZERO) == -1) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))
    else res
  }
}

case object NumericToDouble extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDouble = asDouble(a.getAnyRef)
}

case object StringToBoolean extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBool = {
    val str = a.getString
    val res =
      if (str == "true" || str == "1") true
      else if (str == "false" || str == "0") false
      else throw new NumberFormatException("Value '%s' is not a valid boolean value {true, false, 1, 0}.".format(str))
    asBoolean(res)
  }
}
case object StringToDecimal extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigDecimal = new JBigDecimal(a.getString)
}
case object StringToDouble extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDouble = {
    val str = a.getString
    val d =
      if (str == XMLUtils.PositiveInfinityString) JDouble.POSITIVE_INFINITY
      else if (str == XMLUtils.NegativeInfinityString) JDouble.NEGATIVE_INFINITY
      else if (str == XMLUtils.NaNString) JDouble.NaN
      else str.toDouble
    d
  }
}
case object StringToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = {
    val res =
      try {
        a.getString.toLong
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type long: " + nfe.getMessage())
          throw e
        }
      }
    res
  }
}
case object StringToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = {
    val res = new JBigInt(a.getString)
    if (res.compareTo(JBigInt.ZERO) == -1) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned long.".format(res))
    if (res.compareTo(NodeInfo.UnsignedLong.Max) == 1) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
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
      //case s: Sequence if s.length == 0 => false
      //case s: Sequence if s(0) == Node => true
      case _ => throw new NumberFormatException("Invalid argument type.")
    }
    res
  }
}
