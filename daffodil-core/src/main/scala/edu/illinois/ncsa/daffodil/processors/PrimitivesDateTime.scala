/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import com.ibm.icu.util.Calendar
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.ULocale
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.Converter
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.CalendarCheckPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.CalendarFirstDayOfWeek
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.CalendarPatternKind
import edu.illinois.ncsa.daffodil.processors.unparsers.ConvertTextCalendarUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.DummyUnparser
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import java.text.ParsePosition
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import com.ibm.icu.util.GregorianCalendar
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.ULocale
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.calendar.DFDLDateTime
import edu.illinois.ncsa.daffodil.calendar.DFDLTime
import edu.illinois.ncsa.daffodil.calendar.DFDLDate

abstract class ConvertTextCalendarPrimBase(e: ElementBase, guard: Boolean)
  extends Terminal(e, guard) {

  protected val xsdType = "dateTime"
  protected val prettyType = "DateTime"

  override def toString = "to(xs:" + xsdType + ")"

  protected def infosetPattern: String
  protected def implicitPattern: String
  protected def validFormatCharacters: Seq[Char]

  lazy val pattern: String = {
    val p = e.calendarPatternKind match {
      case CalendarPatternKind.Explicit => e.calendarPattern
      case CalendarPatternKind.Implicit => implicitPattern
    }

    val escapedText = "(''|'[^']+'|[^a-zA-Z])".r
    val patternNoEscapes = escapedText.replaceAllIn(p, "")
    patternNoEscapes.toSeq.foreach(char =>
      if (!validFormatCharacters.contains(char)) {
        SDE("Character '%s' not allowed in dfdl:calendarPattern for xs:%s".format(char, xsdType))
      })

    if (patternNoEscapes.indexOf("S" * (TextCalendarConstants.maxFractionalSeconds + 1)) >= 0) {
      SDE("More than %d fractional seconds unsupported in dfdl:calendarPattern for xs:%s".format(TextCalendarConstants.maxFractionalSeconds, xsdType))
    }

    p
  }

  val firstDay = e.calendarFirstDayOfWeek match {
    case CalendarFirstDayOfWeek.Sunday => Calendar.SUNDAY
    case CalendarFirstDayOfWeek.Monday => Calendar.MONDAY
    case CalendarFirstDayOfWeek.Tuesday => Calendar.TUESDAY
    case CalendarFirstDayOfWeek.Wednesday => Calendar.WEDNESDAY
    case CalendarFirstDayOfWeek.Thursday => Calendar.THURSDAY
    case CalendarFirstDayOfWeek.Friday => Calendar.FRIDAY
    case CalendarFirstDayOfWeek.Saturday => Calendar.SATURDAY
  }

  val calendarDaysInFirstWeek = e.calendarDaysInFirstWeek.toInt

  val calendarCheckPolicy = e.calendarCheckPolicy match {
    case CalendarCheckPolicy.Strict => false
    case CalendarCheckPolicy.Lax => true
  }

  val TimeZoneRegex = """(UTC)?([+\-])?([01]\d|\d)(:?([0-5]\d))?""".r
  val tzStr = e.calendarTimeZone match {
    case TimeZoneRegex(_, plusOrMinus, hour, _, minute) => {
      val pomStr = if (plusOrMinus == null) "+" else plusOrMinus
      val minStr = if (minute == null) "" else minute
      "GMT%s%s%s".format(pomStr, hour, minStr)
    }
    case _ => e.calendarTimeZone
  }

  val calendarTz: Option[TimeZone] = {
    if (tzStr.length == 0) None // Empty String, 'no time zone'
    else {
      val tz = TimeZone.getTimeZone(tzStr)
      if (tz == TimeZone.UNKNOWN_ZONE) {
        e.schemaDefinitionErrorDueToPropertyValue(
          "calendarTimeZone", e.calendarTimeZone, e.calendarTimeZone_location, e,
          "Unknown time zone '%s'", e.calendarTimeZone)
      }
      Some(tz) // Valid time zone
    }
  }

  lazy val hasTZ: Boolean = {
    val tzPattern = "(?<!')((z{1,4}|Z{1,4})|(O{1}|O{4})|(v{1}|v{4})|(V{1,4})|(x{1,3}|X{1,3}))".r

    val res = tzPattern.findFirstIn(pattern) match {
      case None => {
        // Check the calendar time zone property.
        //
        // If there should be 'no time zone' then false
        // else true.
        //
        calendarTz match {
          case None => false
          case Some(_) => true
        }
      }
      case Some(_) => true
    }
    res
  }

  private lazy val localeEv = new CalendarLanguageEv(e.calendarLanguage, e.erd) // will get compiled by CalendarEv which uses it.

  private lazy val calendarEv = {
    val cev = new CalendarEv(localeEv, calendarTz, firstDay, calendarDaysInFirstWeek, calendarCheckPolicy, e.erd)
    cev.compile()
    cev
  }

  override lazy val parser = new ConvertTextCalendarParser(
    e.elementRuntimeData,
    xsdType,
    prettyType,
    pattern,
    hasTZ,
    localeEv,
    calendarEv,
    infosetPattern,
    firstDay,
    calendarDaysInFirstWeek,
    calendarCheckPolicy,
    calendarTz,
    TimeZone.GMT_ZONE)

  override lazy val unparser = new ConvertTextCalendarUnparser(
    e.elementRuntimeData,
    xsdType,
    prettyType,
    pattern,
    hasTZ,
    localeEv,
    calendarEv,
    infosetPattern,
    firstDay,
    calendarDaysInFirstWeek,
    calendarCheckPolicy,
    calendarTz,
    TimeZone.GMT_ZONE)
}

case class ConvertTextDatePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val xsdType = "date"
  protected override val prettyType = "Date"
  protected override val infosetPattern = "uuuu-MM-ddxxx"
  protected override val implicitPattern = "uuuu-MM-dd"
  protected override val validFormatCharacters = "dDeEFGMuwWyXxYzZ".toSeq
}

case class ConvertTextTimePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val xsdType = "time"
  protected override val prettyType = "Time"
  protected override val infosetPattern = "HH:mm:ss.SSSSSSxxx"
  protected override val implicitPattern = "HH:mm:ssZ"
  protected override val validFormatCharacters = "ahHkKmsSvVzXxZ".toSeq
}

case class ConvertTextDateTimePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val xsdType = "dateTime"
  protected override val prettyType = "DateTime"
  protected override val infosetPattern = "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx"
  protected override val implicitPattern = "uuuu-MM-dd'T'HH:mm:ss"
  protected override val validFormatCharacters = "adDeEFGhHkKmMsSuwWvVyXxYzZ".toSeq
}
