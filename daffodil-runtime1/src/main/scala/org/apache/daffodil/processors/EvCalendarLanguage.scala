/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.processors

import org.apache.daffodil.dsom._
import org.apache.daffodil.exceptions._
import com.ibm.icu.util.Calendar
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.ULocale
import org.apache.daffodil.cookers.Converter


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
      context.schemaDefinitionError("dfdl:calendarLanguage property syntax error. Must match '%s' (ex: 'en_us' or 'de_1996'), but was '%s'.", regex, b)
    }
  }
}

class CalendarLanguageEv(calendarLanguageExpr: CompiledExpression[String], erd: ElementRuntimeData)
  extends EvaluatableConvertedExpression[String, ULocale](
    calendarLanguageExpr,
    LocaleConverter,
    erd)
  with InfosetCachedEvaluatable[ULocale] {
  override lazy val runtimeDependencies = Nil
}

class CalendarEv(localeEv: CalendarLanguageEv,
  calendarTz: Option[TimeZone],
  firstDay: Int,
  calendarDaysInFirstWeek: Int,
  calendarCheckPolicy: Boolean,
  erd: ElementRuntimeData)
  extends Evaluatable[Calendar](erd)
  with InfosetCachedEvaluatable[Calendar] {

  override lazy val runtimeDependencies = Seq(localeEv)

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
    cal
  }
}
