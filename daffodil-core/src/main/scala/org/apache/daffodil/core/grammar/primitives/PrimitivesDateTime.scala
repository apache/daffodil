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

package org.apache.daffodil.core.grammar.primitives

import java.text.ParsePosition
import scala.Boolean

import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.grammar.Terminal
import org.apache.daffodil.lib.calendar.TextCalendarConstants
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BinaryCalendarRep
import org.apache.daffodil.lib.schema.annotation.props.gen.CalendarCheckPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.CalendarFirstDayOfWeek
import org.apache.daffodil.lib.schema.annotation.props.gen.CalendarPatternKind
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.runtime1.processors.CalendarEv
import org.apache.daffodil.runtime1.processors.CalendarLanguageEv
import org.apache.daffodil.runtime1.processors.DateTimeFormatterEv
import org.apache.daffodil.runtime1.processors.parsers.ConvertBinaryCalendarSecMilliParser
import org.apache.daffodil.runtime1.processors.parsers.ConvertTextCalendarParser
import org.apache.daffodil.unparsers.runtime1.ConvertBinaryCalendarSecMilliUnparser
import org.apache.daffodil.unparsers.runtime1.ConvertTextCalendarUnparser

import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.ULocale

abstract class ConvertCalendarPrimBase(e: ElementBase, guard: Boolean)
  extends Terminal(e, guard) {

  protected val xsdType = "dateTime"
  protected val prettyType = "DateTime"

  override def toString = "to(xs:" + xsdType + ")"
}

abstract class ConvertTextCalendarPrimBase(e: ElementBase, guard: Boolean)
  extends ConvertCalendarPrimBase(e, guard) {

  protected def infosetPattern: String
  protected def implicitPattern: String
  protected def validFormatCharacters: Seq[Char]

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

  // Binary calendars with a BinaryCalendarRep of 'bcd' or 'ibm4690Packed' should ignore the calendarTimeZone option
  val tzStr = if (
    e.representation == Representation.Binary && e.binaryCalendarRep != BinaryCalendarRep.Packed
  ) {
    ""
  } else {
    e.calendarTimeZone match {
      case TimeZoneRegex(_, plusOrMinus, hour, _, minute) => {
        val pomStr = if (plusOrMinus == null) "+" else plusOrMinus
        val minStr = if (minute == null) "" else minute
        "GMT%s%s%s".format(pomStr, hour, minStr)
      }
      case _ => e.calendarTimeZone
    }
  }

  val calendarTz: Option[TimeZone] = {
    if (tzStr.length == 0) None // Empty String, 'no time zone'
    else {
      val tz = TimeZone.getTimeZone(tzStr)
      if (tz == TimeZone.UNKNOWN_ZONE) {
        e.schemaDefinitionErrorDueToPropertyValue(
          "calendarTimeZone",
          e.calendarTimeZone,
          e.calendarTimeZone_location,
          e,
          "Unknown time zone '%s'",
          e.calendarTimeZone
        )
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

  private lazy val localeEv = {
    val ev = new CalendarLanguageEv(e.calendarLanguage, e.eci)
    ev.compile(e.tunable)
    ev
  }

  private lazy val calendarEv = {
    val cev = new CalendarEv(
      localeEv,
      calendarTz,
      firstDay,
      calendarDaysInFirstWeek,
      calendarCheckPolicy,
      e.eci
    )
    cev.compile(e.tunable)
    cev
  }

  lazy val pattern: String = {
    val p = e.calendarPatternKind match {
      case CalendarPatternKind.Explicit => e.calendarPattern
      case CalendarPatternKind.Implicit =>
        e.representation match {
          case Representation.Binary => Assert.impossibleCase
          case _ => implicitPattern
        }
    }

    val patternToCheck: String = if (e.representation == Representation.Text) {
      val escapedText = "(''|'[^']+'|[^a-zA-Z])".r
      escapedText.replaceAllIn(p, "")
    } else {
      p
    }

    schemaDefinitionWhen(
      patternToCheck.length == 0,
      "dfdl:calendarPatttern contains no pattern letters"
    )

    patternToCheck.toSeq.foreach(char =>
      if (!validFormatCharacters.contains(char)) {
        if (e.representation == Representation.Binary)
          SDE(
            "Character '%s' not allowed in dfdl:calendarPattern for xs:%s with a binaryCalendarRep of '%s'"
              .format(char, xsdType, e.binaryCalendarRep)
          )
        else
          SDE(
            "Character '%s' not allowed in dfdl:calendarPattern for xs:%s".format(char, xsdType)
          )
      }
    )

    if (e.representation == Representation.Binary) {
      // For binary calendars, calendarPattern can contain only characters that always result in digits,
      //   so more than 2 'e' or 'M' in a row aren't valid as they result in text
      if (patternToCheck.contains("eee") || patternToCheck.contains("MMM")) {
        SDE(
          "dfdl:calendarPattern must only contain characters that result in the presentation of digits for xs:%s with a binaryCalendarRep of '%s'"
            .format(xsdType, e.binaryCalendarRep)
        )
      }
    }
    if (patternToCheck.indexOf("S" * (TextCalendarConstants.maxFractionalSeconds + 1)) >= 0) {
      SDE(
        "More than %d fractional seconds unsupported in dfdl:calendarPattern for xs:%s".format(
          TextCalendarConstants.maxFractionalSeconds,
          xsdType
        )
      )
    }

    p
  }

  private lazy val dateTimeFormatterEv = {
    val ev = new DateTimeFormatterEv(
      calendarEv,
      localeEv,
      pattern,
      e.eci
    )
    ev.compile(e.tunable)
    ev
  }

  override lazy val parser = new ConvertTextCalendarParser(
    e.elementRuntimeData,
    xsdType,
    prettyType,
    pattern,
    hasTZ,
    calendarEv,
    dateTimeFormatterEv
  )

  override lazy val unparser =
    new ConvertTextCalendarUnparser(
      e.elementRuntimeData,
      pattern,
      calendarEv,
      dateTimeFormatterEv
    )
}

case class ConvertTextDatePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val xsdType = "date"
  protected override val prettyType = "Date"
  protected override val infosetPattern = "uuuu-MM-ddxxx"
  protected override val implicitPattern = "uuuu-MM-dd"
  protected override val validFormatCharacters =
    if (e.representation == Representation.Binary) {
      "dDeFMuwWyY".toSeq
    } else {
      "dDeEFGMuwWyXxYzZ".toSeq
    }
}

case class ConvertTextTimePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val xsdType = "time"
  protected override val prettyType = "Time"
  protected override val infosetPattern = "HH:mm:ss.SSSSSSxxx"
  protected override val implicitPattern = "HH:mm:ssZ"
  protected override val validFormatCharacters =
    if (e.representation == Representation.Binary) {
      "hHkKmsS".toSeq
    } else {
      "ahHkKmsSvVzXxZ".toSeq
    }
}

case class ConvertTextDateTimePrim(e: ElementBase)
  extends ConvertTextCalendarPrimBase(e, true) {
  protected override val xsdType = "dateTime"
  protected override val prettyType = "DateTime"
  protected override val infosetPattern = "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx"
  protected override val implicitPattern = "uuuu-MM-dd'T'HH:mm:ss"
  protected override val validFormatCharacters =
    if (e.representation == Representation.Binary) {
      "dDeFhHkKmMsSuwWyY".toSeq
    } else {
      "adDeEFGhHkKmMsSuwWvVyXxYzZ".toSeq
    }
}

case class ConvertBinaryDateTimeSecMilliPrim(e: ElementBase, lengthInBits: Long)
  extends ConvertCalendarPrimBase(e, true) {
  protected override val xsdType = "dateTime"
  protected override val prettyType = "DateTime"

  lazy val epochCalendar: Calendar = {
    val cal = Calendar.getInstance
    cal.clear()
    cal.setLenient(false)

    val sdfWithTZ = new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ssZZZZ", ULocale.ENGLISH)
    var pos = new ParsePosition(0)
    sdfWithTZ.parse(e.binaryCalendarEpoch, cal, pos)

    if (pos.getIndex != e.binaryCalendarEpoch.length || pos.getErrorIndex >= 0) {
      // binaryCalendarEpoch didn't match the first format with timezone, so try without
      cal.clear()
      pos = new ParsePosition(0)
      val sdf = new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss", ULocale.ENGLISH)
      cal.setTimeZone(TimeZone.UNKNOWN_ZONE)
      sdf.parse(e.binaryCalendarEpoch, cal, pos)

      if (pos.getIndex != e.binaryCalendarEpoch.length || pos.getErrorIndex >= 0) {
        SDE(
          "Failed to parse binaryCalendarEpoch - Format must match the pattern 'uuuu-MM-dd'T'HH:mm:ss' or 'uuuu-MM-dd'T'HH:mm:ssZZZZ'"
        )
      }
    }

    try {
      cal.getTime
    } catch {
      case e: IllegalArgumentException => {
        SDE("Failed to parse binaryCalendarEpoch: %s.", e.getMessage())
      }
    }

    cal
  }

  override lazy val parser = new ConvertBinaryCalendarSecMilliParser(
    e.elementRuntimeData,
    !epochCalendar.getTimeZone.equals(TimeZone.UNKNOWN_ZONE),
    e.binaryCalendarRep,
    epochCalendar,
    lengthInBits.toInt
  )

  override lazy val unparser =
    new ConvertBinaryCalendarSecMilliUnparser(
      e.elementRuntimeData,
      e.binaryCalendarRep,
      epochCalendar.getTimeInMillis,
      lengthInBits.toInt,
      !epochCalendar.getTimeZone.equals(TimeZone.UNKNOWN_ZONE)
    )
}
