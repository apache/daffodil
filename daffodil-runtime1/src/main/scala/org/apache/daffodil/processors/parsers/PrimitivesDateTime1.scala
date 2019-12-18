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

package org.apache.daffodil.processors.parsers

import java.text.ParsePosition
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import com.ibm.icu.util.ULocale
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.calendar.DFDLDateTime
import org.apache.daffodil.calendar.DFDLTime
import org.apache.daffodil.calendar.DFDLDate
import org.apache.daffodil.processors.CalendarEv
import org.apache.daffodil.processors.CalendarLanguageEv
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Processor
import org.apache.daffodil.schema.annotation.props.gen.BinaryCalendarRep
import org.apache.daffodil.dsom.TunableLimitExceededError

abstract class ConvertTextCalendarProcessorBase(
  override val context: ElementRuntimeData,
  pattern: String) extends Processor {
  // The dfdl:calendarLanguage property can be a runtime-valued expression.
  // Hence, locale and calendar, derived from it, can also be runtime-valued.
  //
  // To parse we need a SimpleDateFormat, and these are
  // (1) not thread safe - so need to be a thread local
  // (2) must be initialized passing the runtime-valued locale and calendar.
  //
  // Now, fact is, while this could be runtime-valued, even if it is, it is very unlikely
  // to be changing a lot. So it's a shame to endlessly allocate SimpleDateFormat objects
  //
  // So we have a 1 slot cache here. We memorize the locale and calendar and associated
  // thread local, and return that thread local if the locale and calendar are the same.
  //
  // Conservatively, they need to be the exact same object, not just equivalent in the "==" sense.
  //
  // This won't be great if some format really is switching among locales and calendars
  // so that more than one of them really ought to be cached, but that's sufficiently unlikely
  // that this trivial scheme will do for now.
  //
  private final class Cache(var cache: (ULocale, Calendar, SimpleDateFormat))

  private object tlCache extends ThreadLocal[Cache] {
    override def initialValue = new Cache(null)
  }

  /**
   * tlDataFormatter can be runtime valued, as it depends on both locale and calendar
   * each of which can be runtime valued.
   */
  final protected def tlDataFormatter(locale: ULocale, calendar: Calendar) = {
    val tl = tlCache.get
    val cache = tl.cache
    if ((cache ne null) && (locale eq cache._1) && (calendar eq cache._2)) {
      // cache hit. Same formatter will do
      cache._3
    } else {
      // As per ICU4J documentation, "Date formats are not synchronized. If
      // multiple threads access a format concurrently, it must be synchronized
      // externally."
      val formatter = new SimpleDateFormat(pattern, locale)
      formatter.setCalendar(calendar)
      formatter.setLenient(true)
      tl.cache = (locale, calendar, formatter)
      formatter
    }
  }
}

case class ConvertTextCalendarParser(
  erd: ElementRuntimeData,
  xsdType: String,
  prettyType: String,
  pattern: String,
  hasTZ: Boolean,
  localeEv: CalendarLanguageEv,
  calendarEv: CalendarEv)
  extends ConvertTextCalendarProcessorBase(erd, pattern)
  with TextPrimParser {

  override lazy val runtimeDependencies = Vector(localeEv, calendarEv)

  def parse(start: PState): Unit = {
    val node = start.simpleElement
    val str = node.dataValueAsString

    Assert.invariant(str != null)

    val pos = new ParsePosition(0)

    val locale: ULocale = localeEv.evaluate(start)

    val calendarOrig: Calendar = calendarEv.evaluate(start)

    // The clear here actually shouldn't be necessary since we call clear()
    // when we create the calendar in CalendarEv, and nothing ever modifies
    // that calendar, we only modify clones. However, it looks like the act of
    // deserializing a Calendar object causes values to be initialized again.
    // So if someone uses a saved parser this calendar will have garbage in it
    // that can affect the results. So clear it here to make sure that's not
    // the case.
    calendarOrig.clear()

    // It's important here to use the calendarOrig that results from
    // calendarEv.evaluate() since the tlDataFormatter is a cache the uses
    // reference equality. For everything else we want to use a clone for
    // reasons described below.
    val df = tlDataFormatter(locale, calendarOrig)

    // When we evaluate calendarEV, if it is a constant we will always get back
    // the same Calendar object. Because of this it is important here to clone
    // this calendar and always use the clone below for two reasons:
    //
    // 1) The below code will modify modify the calendar object based on the
    //    value of the parsed string. Any changes to the object will persist
    //    and could affect future parses. By cloning, we ensure we do not
    //    modify the original calendar object.
    //
    // 2) Multiple threads would have access to the same Calendar object, and
    //    so the below could modify the same object at the same time. By
    //    cloning, we ensure that they modify different objects.
    val calendar = calendarOrig.clone().asInstanceOf[Calendar]

    df.setCalendar(calendar)
    df.parse(str, calendar, pos);

    // Verify that what was parsed was what was passed exactly in byte count
    // Use pos to verify all characters consumed & check for errors
    if (pos.getIndex != str.length || pos.getErrorIndex >= 0) {
      val errIndex = if (pos.getErrorIndex >= 0) pos.getErrorIndex else pos.getIndex
      PE(start, "Unable to parse xs:%s from text: %s", xsdType, str)
      return
    }

    // Unfortunately, there is no publicly available method for validating
    // Calendar values are correct with respect to leniency. So instead, just
    // try to calculate the time, which forces validation. This causes an
    // exception to be thrown if a Calendar is not valid.
    try {
      calendar.getTime
      if ((calendar.get(Calendar.YEAR) > start.tunable.maxValidYear) || (calendar.get(Calendar.YEAR) < start.tunable.minValidYear))
        throw new TunableLimitExceededError(erd.schemaFileLocation,
          "Year value of %s is not within the limits of the tunables minValidYear (%s) and maxValidYear (%s)",
          calendar.get(Calendar.YEAR), start.tunable.minValidYear, start.tunable.maxValidYear)
    } catch {
      case e: IllegalArgumentException => {
        PE(start, "Unable to parse xs:%s from text: %s. %s", xsdType, str, e.getMessage())
        return
      }
    }

    val infosetCalendar = xsdType.toLowerCase() match {
      case "time" => DFDLTime(calendar, hasTZ)
      case "date" => DFDLDate(calendar, hasTZ)
      case "datetime" => DFDLDateTime(calendar, hasTZ)
      case _ => Assert.impossibleCase
    }

    node.overwriteDataValue(infosetCalendar)

  }
}

case class ConvertBinaryCalendarSecMilliParser(
  override val context: ElementRuntimeData,
  hasTZ: Boolean,
  binCalRep: BinaryCalendarRep,
  epochCal: Calendar,
  lengthInBits: Int)
  extends PrimParser {

  override lazy val runtimeDependencies = Vector()

  def parse(start: PState): Unit = {

    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(lengthInBits)) {
      PENotEnoughBits(start, lengthInBits, dis.remainingBits)
      return
    }

    val slong: Long = dis.getSignedLong(lengthInBits, start)
    val cal = epochCal.clone.asInstanceOf[Calendar]

    val millisToAdd: Long = binCalRep match {
      case BinaryCalendarRep.BinarySeconds => slong * 1000
      case BinaryCalendarRep.BinaryMilliseconds => slong
      case _ => Assert.impossibleCase
    }

    val newTime = cal.getTimeInMillis + millisToAdd
    try {
      cal.setTimeInMillis(newTime)
      if ((cal.get(Calendar.YEAR) > start.tunable.maxValidYear) || (cal.get(Calendar.YEAR) < start.tunable.minValidYear))
        throw new TunableLimitExceededError(context.schemaFileLocation,
          "Year value of %s is not within the limits of the tunables minValidYear (%s) and maxValidYear (%s)",
          cal.get(Calendar.YEAR), start.tunable.minValidYear, start.tunable.maxValidYear)
    } catch {
      case e: IllegalArgumentException => {
        PE(start, "%s milliseconds from the binaryCalendarEpoch is out of range of valid values: %s.",
          millisToAdd, e.getMessage())
        return
      }
    }

    val newCal = DFDLDateTime(cal, hasTZ)
    start.simpleElement.overwriteDataValue(newCal)
  }
}
