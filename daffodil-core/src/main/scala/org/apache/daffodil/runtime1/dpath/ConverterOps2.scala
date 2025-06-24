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

import org.apache.daffodil.lib.calendar.DFDLDate
import org.apache.daffodil.lib.calendar.DFDLDateConversion
import org.apache.daffodil.lib.calendar.DFDLDateTime
import org.apache.daffodil.lib.calendar.DFDLDateTimeConversion
import org.apache.daffodil.lib.calendar.DFDLTime
import org.apache.daffodil.lib.calendar.DFDLTimeConversion
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueByteArray
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDate
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDateTime
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueTime

case object AnyAtomicToString extends ToString {
  val name = "AnyAtomicToString"
}

case object StringToDate extends Converter {
  val name = "StringToDate"

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDate = {
    val result = a.getAnyRef match {
      case cal: DFDLDateTime => cal.toDate()
      case cal: DFDLDate => cal
      case str: String => DFDLDateConversion.fromXMLString(str)
      case _ =>
        throw new NumberFormatException(
          "xs:date only accepts String, Date or DateTime objects."
        )
    }
    result
  }
}

case object StringToDateTime extends Converter {
  val name = "StringToDateTime"

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueDateTime = {
    val result = a.getAnyRef match {
      case cal: DFDLDateTime => cal
      case cal: DFDLDate => cal.toDateTime()
      case str: String => DFDLDateTimeConversion.fromXMLString(str)
      case _ =>
        throw new NumberFormatException(
          "xs:dateTime only accepts String, Date or DateTime objects."
        )
    }
    result
  }
}

case object StringToTime extends Converter {
  val name = "StringToTime"

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueTime = {
    val result = a.getAnyRef match {
      case cal: DFDLDateTime => cal.toTime()
      case cal: DFDLTime => cal
      case str: String => DFDLTimeConversion.fromXMLString(str)
      case _ =>
        throw new NumberFormatException(
          "xs:time only accepts String, DateTime or Time objects."
        )
    }
    result
  }
}

case object StringToHexBinary extends Converter with HexBinaryKind {
  val name = "StringToHexBinary"

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueByteArray = {
    val result = a.getAnyRef match {
      case s: String => Misc.hex2Bytes(s)
      case hb: Array[Byte] => hb
      case x =>
        throw new NumberFormatException(
          "%s cannot be cast to dfdl:hexBinary\ndfdl:hexBinary received an unrecognized type! Must be String or HexBinary."
            .format(x.toString)
        )
    }
    result
  }
}
