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

package org.apache.daffodil.runtime1.processors.parsers

import java.text.ParsePosition

import org.apache.daffodil.lib.calendar.DFDLDate
import org.apache.daffodil.lib.calendar.DFDLDateTime
import org.apache.daffodil.lib.calendar.DFDLTime
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BinaryCalendarRep
import org.apache.daffodil.runtime1.dsom.TunableLimitExceededError
import org.apache.daffodil.runtime1.processors.CalendarEv
import org.apache.daffodil.runtime1.processors.DateTimeFormatterEv
import org.apache.daffodil.runtime1.processors.ElementRuntimeData

import com.ibm.icu.util.Calendar

case class ConvertTextCalendarParser(
  override val context: ElementRuntimeData,
  xsdType: String,
  prettyType: String,
  pattern: String,
  hasTZ: Boolean,
  calendarEv: CalendarEv,
  dateTimeFormatterEv: DateTimeFormatterEv
) extends TextPrimParser {

  override def runtimeDependencies = Vector(calendarEv, dateTimeFormatterEv)

  def parse(start: PState): Unit = {
    val node = start.simpleElement
    val str = node.dataValueAsString

    Assert.invariant(str != null)

    val pos = new ParsePosition(0)

    val calendarOrig: Calendar = calendarEv.evaluate(start)

    // The clear here actually shouldn't be necessary since we call clear()
    // when we create the calendar in CalendarEv, and nothing ever modifies
    // that calendar, we only modify clones. However, it looks like the act of
    // deserializing a Calendar object causes values to be initialized again.
    // So if someone uses a saved parser this calendar will have garbage in it
    // that can affect the results. So clear it here to make sure that's not
    // the case.
    calendarOrig.clear()

    val df = dateTimeFormatterEv.evaluate(start).get

    // When we evaluate calendarEV, if it is a constant we will always get back
    // the same Calendar object. Because of this it is important here to clone
    // this calendar and always use the clone below for two reasons:
    //
    // 1) The below code will modify the calendar object based on the
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

    // Verify that we did not fail to parse and that we consumed the entire string. Note that
    // getErrorIndex is never set and is always -1. Only a getIndex value of zero means there
    // was an error
    if (pos.getIndex == 0 || pos.getIndex != str.length) {
      PE(start, "Unable to parse xs:%s from text: %s", xsdType, str)
      return
    }

    // Unfortunately, there is no publicly available method for validating
    // Calendar values are correct with respect to leniency. So instead, just
    // try to calculate the time, which forces validation. This causes an
    // exception to be thrown if a Calendar is not valid.
    try {
      calendar.getTime
      if (
        (calendar.get(Calendar.YEAR) > start.tunable.maxValidYear) || (calendar.get(
          Calendar.YEAR
        ) < start.tunable.minValidYear)
      )
        throw new TunableLimitExceededError(
          context.schemaFileLocation,
          "Year value of %s is not within the limits of the tunables minValidYear (%s) and maxValidYear (%s)",
          calendar.get(Calendar.YEAR),
          start.tunable.minValidYear,
          start.tunable.maxValidYear
        )
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
  lengthInBits: Int
) extends PrimParser {

  override def runtimeDependencies = Vector()

  def parse(start: PState): Unit = {

    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(lengthInBits)) {
      PENotEnoughBits(start, lengthInBits, dis)
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
      if (
        (cal.get(Calendar.YEAR) > start.tunable.maxValidYear) || (cal.get(
          Calendar.YEAR
        ) < start.tunable.minValidYear)
      )
        throw new TunableLimitExceededError(
          context.schemaFileLocation,
          "Year value of %s is not within the limits of the tunables minValidYear (%s) and maxValidYear (%s)",
          cal.get(Calendar.YEAR),
          start.tunable.minValidYear,
          start.tunable.maxValidYear
        )
    } catch {
      case e: IllegalArgumentException => {
        PE(
          start,
          "%s milliseconds from the binaryCalendarEpoch is out of range of valid values: %s.",
          millisToAdd,
          e.getMessage()
        )
        return
      }
    }

    val newCal = DFDLDateTime(cal, hasTZ)
    start.simpleElement.overwriteDataValue(newCal)
  }
}
