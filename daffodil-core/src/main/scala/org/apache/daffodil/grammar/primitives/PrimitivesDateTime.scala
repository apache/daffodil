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

package org.apache.daffodil.grammar.primitives

import java.text.ParsePosition
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.ULocale
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.schema.annotation.props.gen.CalendarCheckPolicy
import org.apache.daffodil.schema.annotation.props.gen.CalendarFirstDayOfWeek
import org.apache.daffodil.schema.annotation.props.gen.CalendarPatternKind
import org.apache.daffodil.processors.unparsers.BinaryCalendarBCDRuntimeLengthUnparser
import org.apache.daffodil.processors.unparsers.BinaryCalendarBCDKnownLengthUnparser
import org.apache.daffodil.processors.unparsers.BinaryCalendarBCDDelimitedLengthUnparser
import org.apache.daffodil.processors.unparsers.ConvertBinaryCalendarSecMilliUnparser
import org.apache.daffodil.processors.unparsers.ConvertTextCalendarUnparser
import com.ibm.icu.util.Calendar
import com.ibm.icu.util.TimeZone
import org.apache.daffodil.processors.CalendarEv
import org.apache.daffodil.processors.CalendarLanguageEv
import org.apache.daffodil.processors.parsers.BinaryCalendarBCDDelimitedLengthParser
import org.apache.daffodil.processors.parsers.BinaryCalendarBCDKnownLengthParser
import org.apache.daffodil.processors.parsers.BinaryCalendarBCDRuntimeLengthParser
import org.apache.daffodil.processors.parsers.ConvertBinaryCalendarSecMilliParser
import org.apache.daffodil.processors.parsers.ConvertTextCalendarParser
import org.apache.daffodil.processors.parsers.TextCalendarConstants
import scala.Boolean

abstract class ConvertCalendarPrimBase(e: ElementBase, guard: Boolean)
  extends Terminal(e, guard) {

  protected val xsdType = "dateTime"
  protected val prettyType = "DateTime"

  override def toString = "to(xs:" + xsdType + ")"
}

trait CalendarPrimBase {
  def e: ElementBase

  protected def infosetPattern: String
  protected def validFormatCharacters: Seq[Char]
  protected def pattern: String

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

  protected lazy val localeEv = {
    val ev = new CalendarLanguageEv(e.calendarLanguage, e.erd)
    ev.compile()
    ev
  }

  protected lazy val calendarEv = {
    val cev = new CalendarEv(localeEv, calendarTz, firstDay, calendarDaysInFirstWeek, calendarCheckPolicy, e.erd)
    cev.compile()
    cev
  }
}

abstract class ConvertTextCalendarPrimBase(e: ElementBase, guard: Boolean)
  extends ConvertCalendarPrimBase(e, guard)
  with CalendarPrimBase {

  protected def implicitPattern: String

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

  override lazy val parser = new ConvertTextCalendarParser(
    e.elementRuntimeData,
    xsdType,
    prettyType,
    pattern,
    hasTZ,
    localeEv,
    calendarEv)

  override lazy val unparser = new ConvertTextCalendarUnparser(
    e.elementRuntimeData,
    pattern,
    localeEv,
    calendarEv)
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

abstract class BinaryPackedDecimalCalendarPrimBase(e: ElementBase, guard: Boolean)
  extends ConvertCalendarPrimBase(e, guard)
  with CalendarPrimBase {

  lazy val pattern: String = {
    val p = e.calendarPatternKind match {
      case CalendarPatternKind.Explicit => e.calendarPattern
      case _ => Assert.impossibleCase
    }

    p.toSeq.foreach(char =>
      if (!validFormatCharacters.contains(char)) {
        SDE("Character '%s' not allowed in dfdl:calendarPattern for xs:%s with a binaryCalendarRep of '%s'".format(char, xsdType, e.binaryCalendarRep))
      })

    if (p.indexOf("S" * (TextCalendarConstants.maxFractionalSeconds + 1)) >= 0) {
      SDE("More than %d fractional seconds unsupported in dfdl:calendarPattern for xs:%s".format(TextCalendarConstants.maxFractionalSeconds, xsdType))
    }

    p
  }
}

case class ConvertBinaryDateTimeSecMilliPrim(e: ElementBase, lengthInBits: Long) extends ConvertCalendarPrimBase(e, true) {
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
        SDE("Failed to parse binaryCalendarEpoch - Format must match the pattern 'uuuu-MM-dd'T'HH:mm:ss' or 'uuuu-MM-dd'T'HH:mm:ssZZZZ'")
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
    lengthInBits.toInt)

  override lazy val unparser =
    new ConvertBinaryCalendarSecMilliUnparser(
    e.elementRuntimeData,
    e.binaryCalendarRep,
    epochCalendar.getTimeInMillis,
    lengthInBits.toInt,
    !epochCalendar.getTimeZone.equals(TimeZone.UNKNOWN_ZONE))
}

abstract class BCDRuntimeLength(e: ElementBase) extends BinaryPackedDecimalCalendarPrimBase(e, true) {

  override lazy val parser = new BinaryCalendarBCDRuntimeLengthParser(
    e.elementRuntimeData,
    false,
    pattern,
    localeEv,
    calendarEv,
    xsdType,
    prettyType,
    e.lengthEv,
    e.lengthUnits)

  override lazy val unparser =
    new BinaryCalendarBCDRuntimeLengthUnparser(
    e.elementRuntimeData,
    pattern,
    localeEv,
    calendarEv,
    e.lengthEv,
    e.lengthUnits)
}

abstract class BCDKnownLength(e: ElementBase, lengthInBits: Long) extends BinaryPackedDecimalCalendarPrimBase(e, true) {

  override lazy val parser = new BinaryCalendarBCDKnownLengthParser(
    e.elementRuntimeData,
    false,
    lengthInBits.toInt,
    pattern,
    localeEv,
    calendarEv,
    xsdType,
    prettyType)

  override lazy val unparser =
    new BinaryCalendarBCDKnownLengthUnparser(
    e.elementRuntimeData,
    lengthInBits.toInt,
    pattern,
    localeEv,
    calendarEv)
}

abstract class BCDDelimitedLength(e: ElementBase, xsdType: String, prettyType: String)
  extends StringDelimited(e)
  with CalendarPrimBase {

  lazy val pattern: String = {
    val p = e.calendarPatternKind match {
      case CalendarPatternKind.Explicit => e.calendarPattern
      case _ => Assert.impossibleCase
    }

    p.toSeq.foreach(char =>
      if (!validFormatCharacters.contains(char)) {
        SDE("Character '%s' not allowed in dfdl:calendarPattern for xs:%s with a binaryCalendarRep of '%s'".format(char, xsdType, e.binaryCalendarRep))
      })

    if (p.indexOf("S" * (TextCalendarConstants.maxFractionalSeconds + 1)) >= 0) {
      SDE("More than %d fractional seconds unsupported in dfdl:calendarPattern for xs:%s".format(TextCalendarConstants.maxFractionalSeconds, xsdType))
    }

    p
  }

  val isDelimRequired: Boolean = false

  override lazy val parser = new BinaryCalendarBCDDelimitedLengthParser(
    e.elementRuntimeData,
    false,
    pattern,
    localeEv,
    calendarEv,
    xsdType,
    prettyType,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired)

  override lazy val unparser =
    new BinaryCalendarBCDDelimitedLengthUnparser(
    e.elementRuntimeData,
    pattern,
    localeEv,
    calendarEv)

}

case class BCDDateKnownLengthPrim(e: ElementBase, lengthInBits: Long)
    extends BCDKnownLength(e, lengthInBits) {
  protected override val xsdType = "date"
  protected override val prettyType = "Date"
  protected override val infosetPattern = "uuuu-MM-ddxxx"
  protected override val validFormatCharacters = "dDeEFGMuwWyXxYzZ".toSeq
}

case class BCDDateRuntimeLengthPrim(e: ElementBase) extends BCDRuntimeLength(e) {
  protected override val xsdType = "date"
  protected override val prettyType = "Date"
  protected override val infosetPattern = "uuuu-MM-ddxxx"
  protected override val validFormatCharacters = "dDeEFGMuwWyXxYzZ".toSeq
}

case class BCDDateDelimitedLengthPrim(e: ElementBase)
    extends BCDDelimitedLength(e, "date", "Date") {
  protected override val infosetPattern = "uuuu-MM-ddxxx"
  protected override val validFormatCharacters = "dDeEFGMuwWyXxYzZ".toSeq
}

case class BCDTimeKnownLengthPrim(e: ElementBase, lengthInBits: Long)
    extends BCDKnownLength(e, lengthInBits) {
  protected override val xsdType = "time"
  protected override val prettyType = "Time"
  protected override val infosetPattern = "HH:mm:ss.SSSSSSxxx"
  protected override val validFormatCharacters = "ahHkKmsSvVzXxZ".toSeq
}

case class BCDTimeRuntimeLengthPrim(e: ElementBase) extends BCDRuntimeLength(e) {
  protected override val xsdType = "time"
  protected override val prettyType = "Time"
  protected override val infosetPattern = "HH:mm:ss.SSSSSSxxx"
  protected override val validFormatCharacters = "ahHkKmsSvVzXxZ".toSeq
}

case class BCDTimeDelimitedLengthPrim(e: ElementBase)
    extends BCDDelimitedLength(e, "time", "Time") {
  protected override val infosetPattern = "HH:mm:ss.SSSSSSxxx"
  protected override val validFormatCharacters = "ahHkKmsSvVzXxZ".toSeq
}

case class BCDDateTimeKnownLengthPrim(e: ElementBase, lengthInBits: Long)
    extends BCDKnownLength(e, lengthInBits) {
  protected override val xsdType = "dateTime"
  protected override val prettyType = "DateTime"
  protected override val infosetPattern = "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx"
  protected override val validFormatCharacters = "adDeEFGhHkKmMsSuwWvVyXxYzZ".toSeq
}

case class BCDDateTimeRuntimeLengthPrim(e: ElementBase) extends BCDRuntimeLength(e) {
  protected override val xsdType = "dateTime"
  protected override val prettyType = "DateTime"
  protected override val infosetPattern = "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx"
  protected override val validFormatCharacters = "adDeEFGhHkKmMsSuwWvVyXxYzZ".toSeq
}

case class BCDDateTimeDelimitedLengthPrim(e: ElementBase)
    extends BCDDelimitedLength(e, "dateTime", "DateTime") {
  protected override val infosetPattern = "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx"
  protected override val validFormatCharacters = "adDeEFGhHkKmMsSuwWvVyXxYzZ".toSeq
}
