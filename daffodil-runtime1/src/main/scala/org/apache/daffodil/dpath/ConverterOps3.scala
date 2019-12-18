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

import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.infoset.DataValue.DataValueBigInt
import org.apache.daffodil.infoset.DataValue.DataValueLong
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValueString
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.Numbers.asBigInt
import org.apache.daffodil.util.Numbers.asLong

case object NumericToString extends ToString
case object DateTimeToString extends ToString
case object HexBinaryToString extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueString = {
    val bytes = a.getByteArray
    val hex = Misc.bytes2Hex(bytes)
    hex
  }
}
case object HexStringToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = {
    val res =
      try {
        val str = a.getString
        java.lang.Long.parseLong(str, 16)
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type long: " + nfe.getMessage())
          throw e
        }
      }
    res
  }
}
case object HexStringToUnsignedLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = {
    val res: DataValueBigInt =
      try {
        val str = a.getString
        new JBigInt(str, 16)
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type unsignedLong: " + nfe.getMessage())
          throw e
        }
      }
    res
  }
}
case object BigIntToLong extends Converter {
  val MAX_VALUE = JBigInt.valueOf(Long.MaxValue)
  val MIN_VALUE = JBigInt.valueOf(Long.MinValue)
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = {
    val res = asBigInt(a.getAnyRef)
    if (res.compareTo(MIN_VALUE) == -1 || res.compareTo(MAX_VALUE) == 1) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    asLong(res)
  }
}
case object IntToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = asLong(a.getAnyRef)
}
case object UnsignedLongToLong extends Converter {
  val MAX_VALUE = JBigInt.valueOf(Long.MaxValue)
  val MIN_VALUE = JBigInt.valueOf(Long.MinValue)
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = {
    val res = asBigInt(a.getAnyRef)
    if (res.compareTo(MIN_VALUE) == -1 || res.compareTo(MAX_VALUE) == 1) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    asLong(res)
  }
}
case object UnsignedIntToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = {
    asLong(a.getAnyRef)
  }
}
case object ArrayIndexToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = {
    asLong(a.getAnyRef)
  }
}
case object ShortToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = asLong(a.getAnyRef)
}
case object UnsignedShortToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = asLong(a.getAnyRef)
}
case object ByteToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = asLong(a.getAnyRef)
}
case object UnsignedByteToLong extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueLong = asLong(a.getAnyRef)
}
