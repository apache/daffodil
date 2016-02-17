package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.exceptions._
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
      context.schemaDefinitionError("dfdl:calendarLanguage property syntax error. Must match '%s' (ex: 'en_us' or 'de_1996'), but was '%s'.", regex, b)
    }
  }
}

class CalendarLanguageEv(calendarLanguageExpr: CompiledExpression[String], erd: ElementRuntimeData)
  extends EvaluatableConvertedExpression[String, ULocale](
    calendarLanguageExpr,
    LocaleConverter,
    erd) {
  override def runtimeDependencies = Nil
}

class CalendarEv(localeEv: CalendarLanguageEv,
  calendarTz: Option[TimeZone],
  firstDay: Int,
  calendarDaysInFirstWeek: Int,
  calendarCheckPolicy: Boolean,
  erd: ElementRuntimeData) extends Evaluatable[Calendar](erd) {

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
