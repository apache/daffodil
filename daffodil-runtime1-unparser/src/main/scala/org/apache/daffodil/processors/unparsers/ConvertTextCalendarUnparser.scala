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

package org.apache.daffodil.processors.unparsers

import com.ibm.icu.util.Calendar
import com.ibm.icu.util.ULocale

import org.apache.daffodil.calendar.DFDLCalendar
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.CalendarEv
import org.apache.daffodil.processors.CalendarLanguageEv
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.util.Misc
import org.apache.daffodil.processors.parsers.ConvertTextCalendarProcessorBase

case class ConvertTextCalendarUnparser(
  erd: ElementRuntimeData,
  pattern: String,
  localeEv: CalendarLanguageEv,
  calendarEv: CalendarEv)
  extends ConvertTextCalendarProcessorBase(erd, pattern)
  with TextPrimUnparser {

  /**
   * Primitive unparsers must override runtimeDependencies
   */
  override lazy val runtimeDependencies = Vector(localeEv, calendarEv)

  def unparse(state: UState): Unit = {
    val locale: ULocale = localeEv.evaluate(state)
    val calendar: Calendar = calendarEv.evaluate(state)

    val node = state.currentInfosetNode.asSimple

    val dc = node.dataValue

    val calValue = node.dataValue match {
      case dc: DFDLCalendar => dc.calendar
      case x => Assert.invariantFailed("ConvertTextCalendar received unsupported type. %s of type %s.".format(x, Misc.getNameFromClass(x)))
    }

    // This initialization is needed because the calendar object may have been
    // persisted, and that computes/completes fields that are not yet
    // completed, such as the Julian day, which freezes the year to 1970. We
    // want a fresh start on all the fields that are filled in from a parse.
    // Also, the tlDataFormatter below uses a cache based on the value of the
    // calendar, so we want to ensure we always get the same data formatter if
    // the calendar is the same
    calendar.clear()

    val df = tlDataFormatter(locale, calendar)
    df.setCalendar(calendar)

    // At this point we should be able to just do "df.format(calValue)".
    // However, when we do that ICU actually uses some fields in calValue (e.g.
    // minimamDaysInFirstWeek, firstDayInWeek) rather than using the fields in
    // "calendar" set in setCalendar above. Those fields in calValue are
    // specific to the locale used to parse the infoset value, which means the
    // locale can effect the unparsed value. So instead of calling
    // format(calValue), copy all the date/time fields into "calendar" (which
    // has the appropriate settings based on DFDL properties) and then unparse
    // that. This ensures there are no locale specific issues related to
    // unparsing calendars.
    calendar.set(
      calValue.get(Calendar.EXTENDED_YEAR),
      calValue.get(Calendar.MONTH),
      calValue.get(Calendar.DAY_OF_MONTH),
      calValue.get(Calendar.HOUR_OF_DAY),
      calValue.get(Calendar.MINUTE),
      calValue.get(Calendar.SECOND)
    )
    calendar.set(Calendar.MILLISECOND,   calValue.get(Calendar.MILLISECOND))
    calendar.setTimeZone(calValue.getTimeZone)

    val str = df.format(calendar)

    node.overwriteDataValue(str)
  }
}
