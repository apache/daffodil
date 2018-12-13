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

package org.apache.daffodil.calendar

import com.ibm.icu.util.Calendar
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.TimeZone

import java.text.ParseException
import java.text.ParsePosition

/**
 * Wrapper arounda SimpleDateFormat and a flag that determines if it expects to
 * parse a timezone or not
 */
case class DFDLCalendarFormat private (format: SimpleDateFormat, expectsTimezone: Boolean)

object DFDLCalendarFormat {
  def apply(pattern: String, expectsTimezone: Boolean): DFDLCalendarFormat = {
    new DFDLCalendarFormat(InfosetSimpleDateFormat(pattern), expectsTimezone)
  }
}

object InfosetSimpleDateFormat {
  def apply(pattern: String): SimpleDateFormat = {
    val sdf = new SimpleDateFormat(pattern)
    sdf.setLenient(false)
    sdf
  }
}

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

  def timePartToXMLString(dfdlcal: DFDLCalendar): String = {
    val calendar = dfdlcal.calendar
    val h = calendar.get(Calendar.HOUR_OF_DAY)
    val m = calendar.get(Calendar.MINUTE)
    val s = calendar.get(Calendar.SECOND)
    val u = calendar.get(Calendar.MILLISECOND)

    pad2(h) + ":" + pad2(m) + ":" + pad2(s) + (if (u != 0) "." + pad3(u) + "000" else "")
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
}

trait DFDLCalendarConversion {

  val calendarType: String

  protected def fromXMLFormats = new ThreadLocal[Seq[DFDLCalendarFormat]]

  /**
   * Attempts to parse a given date/time string with multiple allowable
   * formats. If the string is completely parsed with one of the formats, it
   * returns the resulting Calendar of the parse and the DFDLCalendarFormat
   * that led to the successful match.
   */
  protected def parseFromXMLString(string: String): (Calendar, DFDLCalendarFormat) = {
    // Create strict calendar, initialize with no time zone to ensure timezone
    // information only comes from the data if it exists
    val calendar = Calendar.getInstance(TimeZone.UNKNOWN_ZONE)
    calendar.setLenient(false)

    val successfulFormat = fromXMLFormats.get.find { calendarFormat =>

      val pos = new ParsePosition(0)
      calendar.clear()
      calendarFormat.format.parse(string, calendar, pos)

      try {
        calendar.getTime()
       
        if (pos.getIndex() == 0 || pos.getErrorIndex() != -1 || pos.getIndex() != string.length()) {
          false
        } else {
          true
        }
      } catch {
        case _: IllegalArgumentException => {
          // thrown by getTime() when parsed data is not strictly correct
          false
        }
      }
    }

    if (successfulFormat.isEmpty) {
      throw new IllegalArgumentException(
        """Failed to parse "%s" to an %s""".format(string, calendarType))
    }

    (calendar, successfulFormat.get)
  }
}

object DFDLDateTimeConversion extends DFDLCalendarConversion {

  val calendarType = "xs:dateTime"

  @transient
  override protected lazy val fromXMLFormats = new ThreadLocal[Seq[DFDLCalendarFormat]] {
    override def initialValue = {
      Seq(
        DFDLCalendarFormat("uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx", true),
        DFDLCalendarFormat("uuuu-MM-dd'T'HH:mm:ss.SSSSSS", false),
        DFDLCalendarFormat("uuuu-MM-dd'T'HH:mm:ssxxxxx", true),
        DFDLCalendarFormat("uuuu-MM-dd'T'HH:mm:ss", false),
        DFDLCalendarFormat("uuuu-MM-ddxxxxx", true),
        DFDLCalendarFormat("uuuu-MM-dd", false)
      )
    }
  }

  def fromXMLString(string: String): DFDLDateTime = {
    val (calendar, format) = parseFromXMLString(string)
    DFDLDateTime(calendar, format.expectsTimezone)
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

  @transient
  override protected lazy val fromXMLFormats = new ThreadLocal[Seq[DFDLCalendarFormat]] {
    override def initialValue = {
      Seq(
        DFDLCalendarFormat("uuuu-MM-ddxxxxx", true),
        DFDLCalendarFormat("uuuu-MM-dd", false)
      )
    }
  }

  def fromXMLString(string: String): DFDLDate = {
    val (calendar, format) = parseFromXMLString(string)
    DFDLDate(calendar, format.expectsTimezone)
  }

  def toXMLString(d: DFDLDate): String = {
    DFDLCalendarConversion.datePartToXMLString(d) +
      DFDLCalendarConversion.timeZonePartToXMLString(d)
  }
}

object DFDLTimeConversion extends DFDLCalendarConversion {

  val calendarType = "xs:time"

  @transient
  override protected lazy val fromXMLFormats = new ThreadLocal[Seq[DFDLCalendarFormat]] {
    override def initialValue = {
      Seq(
        DFDLCalendarFormat("HH:mm:ss.SSSSSSxxxxx", true),
        DFDLCalendarFormat("HH:mm:ss.SSSSSS", false),
        DFDLCalendarFormat("HH:mm:ssxxxxx", true),
        DFDLCalendarFormat("HH:mm:ss", false)
      )
    }
  }

  def fromXMLString(string: String): DFDLTime = {
    val (calendar, format) = parseFromXMLString(string)
    DFDLTime(calendar, format.expectsTimezone)
  }


  def toXMLString(t: DFDLTime): String = {
    DFDLCalendarConversion.timePartToXMLString(t) +
      DFDLCalendarConversion.timeZonePartToXMLString(t)
  }
}
