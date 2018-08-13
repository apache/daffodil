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

    // This initialization is needed because the calendar object may have
    // been persisted, and that computes/completes fields that are not yet completed,
    // such as the Julian day, which freezes the year to 1970.
    // We want a fresh start on all the fields that are filled in from a parse.

    calendar.clear()

    val df = tlDataFormatter(locale, calendar)
    df.setCalendar(calendar)

    val node = state.currentInfosetNode.asSimple

    val calValue = node.dataValue match {
      case dc: DFDLCalendar => dc.calendar
      case x => Assert.invariantFailed("ConvertTextCalendar received unsupported type. %s of type %s.".format(x, Misc.getNameFromClass(x)))
    }

    val str = df.format(calValue)

    node.overwriteDataValue(str)
  }
}
