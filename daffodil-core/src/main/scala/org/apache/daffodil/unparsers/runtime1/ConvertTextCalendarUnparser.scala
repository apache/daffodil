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

package org.apache.daffodil.unparsers.runtime1

import org.apache.daffodil.lib.calendar.DFDLCalendar
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.processors.CalendarEv
import org.apache.daffodil.runtime1.processors.DateTimeFormatterEv
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers._

import com.ibm.icu.util.Calendar

case class ConvertTextCalendarUnparser(
  override val context: ElementRuntimeData,
  pattern: String,
  calendarEv: CalendarEv,
  dateTimeFormatterEv: DateTimeFormatterEv
) extends TextPrimUnparser {

  /**
   * Primitive unparsers must override runtimeDependencies
   */
  override def runtimeDependencies = Vector(calendarEv, dateTimeFormatterEv)

  def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple

    val dc = node.dataValue

    val infosetCalendar = node.dataValue.getAnyRef match {
      case dc: DFDLCalendar => dc.calendar
      case x =>
        Assert.invariantFailed(
          "ConvertTextCalendar received unsupported type. %s of type %s.".format(
            x,
            Misc.getNameFromClass(x)
          )
        )
    }

    val calendarOrig: Calendar = calendarEv.evaluate(state)

    // The clear() here actually shouldn't be necessary since we call clear()
    // when we create the calendar in CalendarEv, and nothing ever modifies
    // that calendar--we only modify clones. However, it looks like the act of
    // deserializing a Calendar object causes values to be initialized again.
    // So if someone uses a saved parser this calendar will have garbage in it
    // that can affect the results. So clear it here to make sure that's not
    // the case.
    calendarOrig.clear()

    val df = dateTimeFormatterEv.evaluate(state).get

    // When we evaluate calendarEV, if it is a constant we will always get back
    // the same Calendar object. Because of this it is important here to clone
    // this calendar and always use the clone below for two reasons:
    //
    // 1) The below code will modify the calendar object based on the dateTime
    //    in the infoset. Any changes to the object would persist and affect
    //    future unparses. By cloning, we ensure we do not modify the original
    //    calendar object that other unparses will use.
    //
    // 2) Multiple threads would have access to the same Calendar object, and
    //    so the below code could modify the same object at the same time
    //    across threads. By cloning, we ensure that they modify different
    //    objects.
    val calendar = calendarOrig.clone().asInstanceOf[Calendar]

    // At this point we should be able to just do "df.format(infosetCalendar)".
    // However, when we do that ICU actually uses some fields in infosetCalendar (e.g.
    // minimamDaysInFirstWeek, firstDayInWeek) rather than using the fields in
    // "calendar" set in setCalendar above. Those fields in infosetCalendar are
    // specific to the locale used to parse the infoset value, which means the
    // locale can effect the unparsed value. So instead of calling
    // format(infosetCalendar), copy all the date/time fields into "calendar" (which
    // has the appropriate settings based on DFDL properties) and then unparse
    // that. This ensures there are no locale specific issues related to
    // unparsing calendars.
    calendar.set(
      infosetCalendar.get(Calendar.EXTENDED_YEAR),
      infosetCalendar.get(Calendar.MONTH),
      infosetCalendar.get(Calendar.DAY_OF_MONTH),
      infosetCalendar.get(Calendar.HOUR_OF_DAY),
      infosetCalendar.get(Calendar.MINUTE),
      infosetCalendar.get(Calendar.SECOND)
    )
    calendar.set(Calendar.MILLISECOND, infosetCalendar.get(Calendar.MILLISECOND))
    calendar.setTimeZone(infosetCalendar.getTimeZone)

    df.setCalendar(calendar)
    val str = df.format(calendar)

    node.overwriteDataValue(str)
  }
}
