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

import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import org.apache.daffodil.calendar.DFDLDateTime
import org.apache.daffodil.calendar.DFDLCalendar
import org.apache.daffodil.calendar.DFDLTime
import org.apache.daffodil.calendar.DFDLDate

case object AnyAtomicToString extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    a match {
      case c: DFDLCalendar => c.toString
      case _ => a.asInstanceOf[String]
    }
  }
}

trait XSDateTimeKind extends Serializable {
  val timeZoneID = "UTC"
  @transient lazy val calendar = new ThreadLocal[Calendar] {
    override def initialValue = {
      val cal = Calendar.getInstance()
      cal.clear()
      cal.setLenient(false)
      //
      // We don't want to set a TimeZone here.  It should be 'parsed' if it's
      // there.  If it's not, the TimeZone will be set to TimeZone.UNKNOWN_ZONE
      // which will operate just like GMT/UTC
      //
      cal
    }
  }

  def defaultFormatter: ThreadLocal[DFDLDateFormat]
  def acceptableFormats: Seq[DFDLDateFormat]

  def getNewCalendar: Calendar = calendar.get.clone().asInstanceOf[Calendar]

  protected def createCalendar(str: String, inFormat: SimpleDateFormat, expectsTZ: Boolean,
    fncName: String, toType: String): DFDLCalendar

  def matchFormat(str: String, fncName: String, toType: String): DFDLCalendar = {

    acceptableFormats.foreach(f => {
      val inFormat = f.getFormat
      inFormat.setCalendar(getNewCalendar)
      try {
        val cal = createCalendar(str, inFormat, f.expectsTimeZone, fncName, toType)
        // Here we've successfully created a calendar using the expected format
        // denoted by 'inFormat'. Return the calendar.
        return cal
      } catch {
        case e: IllegalArgumentException => /* Format failed, continue trying to match other formats */
      }

    })
    // All acceptable formats failed
    throw new NumberFormatException("Failed to convert \"%s\" to %s.".format(str, toType))
  }
}

case object StringToDate extends Converter with XSDateTimeKind with DateFormatters {
  val name = "StringToDate"

  /**
   * A list of acceptable formats as specified by: http://www.w3.org/TR/NOTE-datetime
   *
   * Order matters here as we are also trying to determine if a time zone was parsed
   */
  def acceptableFormats = Seq(defaultFormatter.get, withoutTimezoneFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat, expectsTZ: Boolean, fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLDate(str, inFormat, expectsTZ, fncName, toType)
  }

  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val result = a match {
      case cal: DFDLDateTime => cal.toDate
      case cal: DFDLDate => cal
      case str: String => matchFormat(str, name, "xs:date")
      case _ => throw new NumberFormatException("xs:date only accepts String, Date or DateTime objects.")
    }
    result
  }
}
case object StringToDateTime extends Converter
  with XSDateTimeKind with DateTimeFormatters {
  val name = "StringToDateTime"

  /**
   * A list of acceptable formats as specified by: http://www.w3.org/TR/NOTE-datetime
   *
   * Order matters here as we are also trying to determine if a time zone was parsed
   */
  def acceptableFormats = Seq(defaultFormatter.get, withFractNoTimeZoneFormatter.get,
    withTimeZoneNoFractFormatter.get, noTimeZoneNoFractFormatter.get,
    withTimeZoneFormatter.get, dateOnlyFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat,
    expectsTZ: Boolean, fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLDateTime(str, inFormat, expectsTZ, fncName, toType)
  }

  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val result = a match {
      case cal: DFDLDateTime => cal
      case cal: DFDLDate => cal.toDateTime
      case str: String => matchFormat(str, name, "xs:dateTime")
      case _ => throw new NumberFormatException("xs:dateTime only accepts String, Date or DateTime objects.")
    }
    result
  }

}

case object StringToTime extends Converter with XSDateTimeKind with TimeFormatters {
  val name = "StringToTime"

  /**
   * A list of acceptable formats as specified by: http://www.w3.org/TR/NOTE-datetime
   *
   * Order matters here as we are also trying to determine if a time zone was parsed
   */
  def acceptableFormats = Seq(defaultFormatter.get, noTimeZoneFormatter.get,
    withTimeZoneNoFractFormatter.get, noTimeZoneNoFractFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat,
    expectsTZ: Boolean, fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLTime(str, inFormat, expectsTZ, fncName, toType)
  }

  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    val result = a match {
      case cal: DFDLDateTime => cal.toTime
      case cal: DFDLTime => cal
      case str: String => matchFormat(str, name, "xs:time")
      case _ => throw new NumberFormatException("xs:time only accepts String, DateTime or Time objects.")
    }
    result
  }

}
