package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import java.text.ParsePosition

import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import com.ibm.icu.util.DFDLDate
import com.ibm.icu.util.DFDLDateTime
import com.ibm.icu.util.DFDLTime
import com.ibm.icu.util.GregorianCalendar
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.ULocale

import edu.illinois.ncsa.daffodil.exceptions.Assert

case class ConvertTextCalendarParser(erd: ElementRuntimeData,
  xsdType: String,
  prettyType: String,
  pattern: String,
  hasTZ: Boolean,
  locale: ULocale,
  infosetPattern: String,
  firstDay: Int,
  calendarDaysInFirstWeek: Int,
  calendarCheckPolicy: Boolean,
  calendarTz: Option[TimeZone],
  tz: TimeZone)
  extends PrimParser(erd) {

  // Used to configure the dataFormatter
  lazy val calendar: Calendar = {
    val cal = Calendar.getInstance(locale)
    cal.setFirstDayOfWeek(firstDay)
    cal.setMinimalDaysInFirstWeek(calendarDaysInFirstWeek)
    cal.setLenient(calendarCheckPolicy)
    val tz = {
      // If 'no time zone', then use UNKNOWN_ZONE
      //
      // UNKNOWN_ZONE behaves just like GMT/UTC and will
      // preserve the Date/Time values.
      //
      calendarTz.getOrElse(TimeZone.UNKNOWN_ZONE)
    }
    cal.setTimeZone(tz)
    cal.clear
    cal
  }

  // As per ICU4J documentation, "Date formats are not synchronized. If
  // multiple threads access a format concurrently, it must be synchronized
  // externally."
  lazy val tlDataFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val formatter = new SimpleDateFormat(pattern, locale)
      formatter.setCalendar(calendar)
      formatter.setLenient(true)
      formatter
    }
  }

  lazy val tlInfosetFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val formatter = new SimpleDateFormat(infosetPattern)
      val cal = new GregorianCalendar()
      cal.clear
      formatter.setCalendar(cal)
      formatter.setTimeZone(tz)
      formatter.setLenient(true)
      formatter
    }
  }

  def parse(start: PState): PState = {
    val node: InfosetSimpleElement = start.simpleElement
    var str = node.dataValueAsString

    Assert.invariant(str != null)

    val pos = new ParsePosition(0)
    val cal = tlDataFormatter.get.getCalendar.clone.asInstanceOf[Calendar]
    tlDataFormatter.get.parse(str, cal, pos);

    // Verify that what was parsed was what was passed exactly in byte count
    // Use pos to verify all characters consumed & check for errors
    if (pos.getIndex != str.length || pos.getErrorIndex >= 0) {
      val errIndex = if (pos.getErrorIndex >= 0) pos.getErrorIndex else pos.getIndex
      return PE(start, "Convert to %s (for xs:%s): Failed to parse '%s' at character %d.", prettyType, xsdType, str, errIndex + 1)
    }

    // Unfortunately, there is no publicly available method for validating
    // Calendar values are correct with respect to leniency. So instead, just
    // try to calculate the time, which forces validation. This causes an
    // exception to be thrown if a Calendar is not valid.
    try {
      cal.getTime
    } catch {
      case e: IllegalArgumentException => {
        return PE(start, "Convert to %s (for xs:%s): Failed to parse '%s': %s.", prettyType, xsdType, str, e.getMessage)
      }
    }

    val newCal = xsdType.toLowerCase() match {
      case "time" => new DFDLTime(cal, hasTZ)
      case "date" => new DFDLDate(cal, hasTZ)
      case "datetime" => new DFDLDateTime(cal, hasTZ)
      case _ => Assert.impossibleCase
    }

    node.setDataValue(newCal)

    start
  }
}

object TextCalendarConstants {
  final val maxFractionalSeconds = 9
}
