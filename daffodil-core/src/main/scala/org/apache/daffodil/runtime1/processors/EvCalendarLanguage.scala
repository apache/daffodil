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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.lib.cookers.Converter
import org.apache.daffodil.lib.exceptions._
import org.apache.daffodil.runtime1.dsom._

import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.ULocale

object LocaleConverter extends Converter[String, ULocale] {

  val regex = "([A-Za-z]{1,8}([-_][A-Za-z0-9]{1,8})*)"
  val localePattern = java.util.regex.Pattern.compile(regex)

  protected def convert(b: String, context: ThrowsSDE, forUnparse: Boolean): ULocale = {
    val m = localePattern.matcher(b)
    if (m.matches()) {
      val canonicalCalLang = ULocale.canonicalize(b)
      Assert.invariant(canonicalCalLang ne null)
      val l = new ULocale(canonicalCalLang)
      l
    } else {
      context.schemaDefinitionError(
        "dfdl:calendarLanguage property syntax error. Must match '%s' (ex: 'en_us' or 'de_1996'), but was '%s'.",
        regex,
        b
      )
    }
  }
}

class CalendarLanguageEv(
  calendarLanguageExpr: CompiledExpression[String],
  eci: DPathElementCompileInfo
) extends EvaluatableConvertedExpression[String, ULocale](
    calendarLanguageExpr,
    LocaleConverter,
    eci
  )
  with InfosetCachedEvaluatable[ULocale] {
  override def runtimeDependencies = Vector()
}

class CalendarEv(
  localeEv: CalendarLanguageEv,
  calendarTz: Option[TimeZone],
  firstDay: Int,
  calendarDaysInFirstWeek: Int,
  calendarCheckPolicy: Boolean,
  eci: DPathElementCompileInfo
) extends Evaluatable[Calendar](eci)
  with InfosetCachedEvaluatable[Calendar] {

  override def runtimeDependencies = Seq(localeEv)

  override def compute(state: ParseOrUnparseState) = {
    // Used to configure the dataFormatter
    val locale = localeEv.evaluate(state)
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
      if (calendarTz.isDefined) calendarTz.get else TimeZone.UNKNOWN_ZONE
    }
    cal.setTimeZone(tz)

    // The Calendar is initialized with time values based on the current system
    // which aren't always overwritten during a parse. We don't want that, so
    // clear out those values.
    cal.clear()

    cal
  }
}

class DateTimeFormatterEv(
  calendarEv: CalendarEv,
  localeEv: CalendarLanguageEv,
  pattern: String,
  eci: DPathElementCompileInfo
) extends Evaluatable[ThreadLocal[SimpleDateFormat]](eci)
  with InfosetCachedEvaluatable[ThreadLocal[SimpleDateFormat]] {

  override def runtimeDependencies = Seq(localeEv)

  override def compute(state: ParseOrUnparseState) = {
    val calendar = calendarEv.evaluate(state)
    val locale = localeEv.evaluate(state)

    // As per ICU4J documentation, "Date formats are not synchronized. If multiple threads
    // access a format concurrently, it must be synchronized externally." Rather than
    // synchronzing, we create a ThreadLocal so each thread gets their own copy of the
    // SimpleDateFormat
    val dateFormatTL = new ThreadLocal[SimpleDateFormat] with Serializable {
      override def initialValue = {
        val formatter = new SimpleDateFormat(pattern, locale)
        formatter.setCalendar(calendar)
        formatter.setLenient(calendar.isLenient)
        formatter
      }
    }
    dateFormatTL
  }
}
