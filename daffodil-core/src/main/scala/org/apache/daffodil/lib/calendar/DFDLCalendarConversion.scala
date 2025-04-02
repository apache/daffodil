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

package org.apache.daffodil.lib.calendar

import java.lang.Integer

import com.ibm.icu.util.Calendar
import com.ibm.icu.util.SimpleTimeZone
import com.ibm.icu.util.TimeZone

object DFDLCalendarConversion {

  @inline
  private def pad2(i: Int) = {
    val istr = i.toString
    if (i >= 10) istr
    else "0" + istr
  }

  @inline
  private def pad3(i: Int) = {
    val istr = i.toString
    if (i >= 100) istr
    else if (i >= 10) "0" + istr
    else "00" + istr
  }

  @inline
  private def pad4(i: Int) = {
    val istr = i.toString
    if (i >= 1000) istr
    else if (i >= 100) "0" + istr
    else if (i >= 10) "00" + istr
    else "000" + istr
  }

  def datePartToXMLString(dfdlcal: DFDLCalendar): String = {
    val calendar = dfdlcal.calendar
    val y = calendar.get(Calendar.EXTENDED_YEAR)
    val m = calendar.get(Calendar.MONTH) + 1
    val d = calendar.get(Calendar.DAY_OF_MONTH)

    val ysign = if (y >= 0) "" else "-"

    ysign + pad4(Math.abs(y)) + "-" + pad2(m) + "-" + pad2(d)
  }

  /**
   * Parses a string that begins with the pattern "uuuu-MM-dd" and sets the
   * appropriate values in the calendar. The year part may be 1 or more digits
   * (including an optional sign) and must be a valid postive or negative
   * integer. The month and day parts must be zero padded digits.
   *
   * If the pattern is not followed, an IllegalArgumentException is thrown.
   *
   * @return if the date part was succesfully parsed, returns a substring of
   *         the remaining characters
   */
  def datePartFromXMLString(string: String, calendar: Calendar): String = {
    @inline
    def invalidValue = throw new IllegalArgumentException(
      "Invalid date string: %s".format(string)
    )

    if (string.length == 0) invalidValue

    val endYear =
      if (string.charAt(0) == '-') {
        string.indexOf('-', 1) // skip negative sign in negative years
      } else {
        string.indexOf('-')
      }

    if (endYear == -1) invalidValue
    if (string.length < endYear + 6) invalidValue
    if (string.charAt(endYear + 3) != '-') invalidValue

    val y = string.substring(0, endYear)
    val m = string.substring(endYear + 1, endYear + 3)
    val d = string.substring(endYear + 4, endYear + 6)

    try {
      calendar.set(Calendar.EXTENDED_YEAR, Integer.parseInt(y))
      calendar.set(Calendar.MONTH, Integer.parseInt(m) - 1)
      calendar.set(Calendar.DAY_OF_MONTH, Integer.parseInt(d))
    } catch {
      case _: NumberFormatException => invalidValue
    }

    string.substring(endYear + 6)
  }

  def timePartToXMLString(dfdlcal: DFDLCalendar): String = {
    val calendar = dfdlcal.calendar
    val h = calendar.get(Calendar.HOUR_OF_DAY)
    val m = calendar.get(Calendar.MINUTE)
    val s = calendar.get(Calendar.SECOND)
    val u = calendar.get(Calendar.MILLISECOND)

    pad2(h) + ":" + pad2(m) + ":" + pad2(s) + (if (u != 0) "." + pad3(u) + "000" else "")
  }

  /**
   * Parses a string that begins with the pattern "HH:mm:ss.SSSSSS" (where the
   * .SSSSSS is optional) and sets the appropriate values in the calendar. The
   * hour, minute, and second parts are zero padded two digits. The
   * milliseconds is everything up to the time zone or end of string if there
   * is no time zon
   *
   * If the pattern is not followed, an IllegalArgumentException is thrown.
   *
   * @return if the time part was succesfully parsed, returns a substring of
   *         the remaining characters
   */
  def timePartFromXMLString(string: String, calendar: Calendar): String = {
    @inline
    def invalidValue = throw new IllegalArgumentException(
      "Invalid time string: %s".format(string)
    )

    if (string.length < 8) invalidValue
    if (string.charAt(2) != ':') invalidValue
    if (string.charAt(5) != ':') invalidValue

    val h = string.substring(0, 2)
    val m = string.substring(3, 5)
    val s = string.substring(6, 8)

    val (ms, endTime) =
      if (string.length > 8) {
        // must have milliseconds or a time zone
        if (string.charAt(8) != '.') {
          // must be a time zone and no milliseconds
          ("0", 8)
        } else {
          // must have milliseconds, maybe a time zone
          val tzStart = string.indexWhere(c => c == '-' || c == '+' || c == 'Z', 9)
          if (tzStart == -1) {
            // no timezone, the rest of string is milliseconds
            (string.substring(9), string.length)
          } else {
            // has timezone, just consume up that
            (string.substring(9, tzStart), tzStart)
          }
        }
      } else {
        // no milliseconds or time zone
        ("0", 8)
      }

    try {
      calendar.set(Calendar.HOUR_OF_DAY, Integer.parseInt(h))
      calendar.set(Calendar.MINUTE, Integer.parseInt(m))
      calendar.set(Calendar.SECOND, Integer.parseInt(s))
      // ICU only supports integer milliseconds precision, which means we can
      // only support at most 3 digits from the milliseconds field
      val msDigits = Math.min(ms.length, 3)
      val msUnscaled = Integer.parseInt(ms.substring(0, msDigits))
      val msScaled =
        if (msDigits == 1) msUnscaled * 100
        else if (msDigits == 2) msUnscaled * 10
        else msUnscaled
      calendar.set(Calendar.MILLISECOND, msScaled)
    } catch {
      case _: NumberFormatException => invalidValue
    }

    string.substring(endTime)
  }

  def timeZonePartToXMLString(dfdlcal: DFDLCalendar): String = {
    if (!dfdlcal.hasTimeZone) {
      ""
    } else {
      // This will return +00:00 for GMT. We may want to consider returning 'Z'
      // instead when offsetInMils is zero. The DFDL specification is unclear
      // on whether +00:00 or Z is expected. Note that the specification is
      // clear that the dfdl:timeZoneFrom* functions (which are implemented
      // using this function) should return +00:00 and not Z So if this
      // function is changed to return a 'Z', it must be made configurable.
      val tz = dfdlcal.calendar.getTimeZone
      val offsetInMils = tz.getRawOffset
      val offsetInMins = Math.abs(offsetInMils / (1000 * 60))
      val h = offsetInMins / 60
      val m = offsetInMins % 60
      val s = if (offsetInMils >= 0) "+" else "-"

      s + pad2(h) + ":" + pad2(m)
    }
  }

  /**
   * Parses a string that of the pattern [+-]hh:mm(:ss)? and sets the timezone
   * in the calendar. The hour, minute, and second parts are zero padded two
   * digits.
   *
   * If the pattern is not followed, an IllegalArgumentException is thrown.
   *
   * @return if the time part was succesfully parsed, returns a substring of
   *         the remaining characters
   */
  def timeZonePartFromXMLString(string: String, calendar: Calendar): String = {
    @inline
    def invalidValue = throw new IllegalArgumentException(
      "Invalid time zone string: %s".format(string)
    )

    if (string == "") {
      // no timezone
      string
    } else {
      val firstChar = string.charAt(0)
      val (timezone, endTimeZone) =
        if (firstChar == 'Z') {
          (TimeZone.GMT_ZONE, 1)
        } else {
          val sign =
            if (firstChar == '+') 1
            else if (firstChar == '-') -1
            else invalidValue

          if (string.length < 6) invalidValue
          if (string.charAt(3) != ':') invalidValue

          val h = string.substring(1, 3)
          val m = string.substring(4, 6)
          val s =
            if (string.length > 6) {
              if (string.charAt(6) != ':') invalidValue
              string.substring(7, 9)
            } else {
              "00"
            }

          val offsetInMillis =
            try {
              val hi = Integer.parseInt(h)
              val mi = Integer.parseInt(m)
              val si = Integer.parseInt(s)
              if (hi < 0 || hi >= 24) invalidValue
              if (mi < 0 || mi >= 60) invalidValue
              if (si < 0 || si >= 60) invalidValue
              sign * (hi * 60 * 60 + mi * 60 + si) * 1000
            } catch {
              case _: NumberFormatException => invalidValue
            }

          val tz =
            if (offsetInMillis == 0) TimeZone.GMT_ZONE
            else new SimpleTimeZone(offsetInMillis, string)
          val consumed = if (string.length > 6) 9 else 6

          (tz, consumed)
        }

      calendar.setTimeZone(timezone)
      string.substring(endTimeZone)
    }
  }
}

trait DFDLCalendarConversion {
  val calendarType: String

  @inline
  final protected def invalidCalendar(string: String): Nothing = {
    throw new IllegalArgumentException(
      "Failed to parse %s from string: %s".format(calendarType, string)
    )
  }

  protected val emptyCalendar = {
    val c = Calendar.getInstance(TimeZone.UNKNOWN_ZONE)
    c.clear()
    c.setLenient(false)
    c
  }
}

object DFDLDateTimeConversion extends DFDLCalendarConversion {

  val calendarType = "xs:dateTime"

  /**
   * Supported patterns:
   *   uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx
   *   uuuu-MM-dd'T'HH:mm:ss.SSSSSS
   *   uuuu-MM-dd'T'HH:mm:ssxxxxx
   *   uuuu-MM-dd'T'HH:mm:ss
   *   uuuu-MM-ddxxxxx
   *   uuuu-MM-dd
   */
  def fromXMLString(string: String): DFDLDateTime = {
    val calendar = emptyCalendar.clone().asInstanceOf[Calendar]

    try {
      val rem1 = DFDLCalendarConversion.datePartFromXMLString(string, calendar)
      val rem2 =
        if (rem1.length > 0 && rem1(0) == 'T') {
          DFDLCalendarConversion.timePartFromXMLString(rem1.substring(1), calendar)
        } else {
          rem1
        }
      val rem3 = DFDLCalendarConversion.timeZonePartFromXMLString(rem2, calendar)
      if (rem3.length > 0) invalidCalendar(string)
      val hasTimeZone = rem2.length > 0

      // this causes validation of the fields
      calendar.getTimeInMillis()

      DFDLDateTime(calendar, hasTimeZone)
    } catch {
      // thrown by us if a string doesn't match a pattern, or ICU if fields are invalid
      case _: IllegalArgumentException => invalidCalendar(string)
    }
  }

  def toXMLString(dt: DFDLDateTime): String = {
    DFDLCalendarConversion.datePartToXMLString(dt) +
      "T" +
      DFDLCalendarConversion.timePartToXMLString(dt) +
      DFDLCalendarConversion.timeZonePartToXMLString(dt)
  }
}

object DFDLDateConversion extends DFDLCalendarConversion {

  val calendarType = "xs:date"

  /**
   * Supported patterns:
   *   uuuu-MM-ddxxxxx
   *   uuuu-MM-dd
   */
  def fromXMLString(string: String): DFDLDate = {
    val calendar = emptyCalendar.clone().asInstanceOf[Calendar]

    try {
      val rem1 = DFDLCalendarConversion.datePartFromXMLString(string, calendar)
      val rem2 = DFDLCalendarConversion.timeZonePartFromXMLString(rem1, calendar)
      if (rem2.length > 0) invalidCalendar(string)
      val hasTimeZone = rem1.length > 0

      // this causes validation of the fields
      calendar.getTimeInMillis()

      DFDLDate(calendar, hasTimeZone)
    } catch {
      // thrown by us if a string doesn't match a pattern, or ICU if fields are invalid
      case _: IllegalArgumentException => invalidCalendar(string)
    }
  }

  def toXMLString(d: DFDLDate): String = {
    DFDLCalendarConversion.datePartToXMLString(d) +
      DFDLCalendarConversion.timeZonePartToXMLString(d)
  }
}

object DFDLTimeConversion extends DFDLCalendarConversion {

  val calendarType = "xs:time"

  /**
   * Supported patterns:
   *   HH:mm:ss.SSSSSSxxxxx
   *   HH:mm:ss.SSSSSS
   *   HH:mm:ssxxxxx
   *   HH:mm:ss
   */
  def fromXMLString(string: String): DFDLTime = {
    val calendar = emptyCalendar.clone().asInstanceOf[Calendar]

    try {
      val rem1 = DFDLCalendarConversion.timePartFromXMLString(string, calendar)
      val rem2 = DFDLCalendarConversion.timeZonePartFromXMLString(rem1, calendar)
      if (rem2.length > 0) invalidCalendar(string)
      val hasTimeZone = rem1.length > 0

      // this causes validation of the fields
      calendar.getTimeInMillis()

      DFDLTime(calendar, hasTimeZone)
    } catch {
      // thrown by us if a string doesn't match a pattern, or ICU if fields are invalid
      case _: IllegalArgumentException => invalidCalendar(string)
    }
  }

  def toXMLString(t: DFDLTime): String = {
    DFDLCalendarConversion.timePartToXMLString(t) +
      DFDLCalendarConversion.timeZonePartToXMLString(t)
  }
}
