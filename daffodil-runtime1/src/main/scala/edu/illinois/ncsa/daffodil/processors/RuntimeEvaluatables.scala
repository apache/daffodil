//package edu.illinois.ncsa.daffodil.processors
//
//import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
//import edu.illinois.ncsa.daffodil.dsom._
//import edu.illinois.ncsa.daffodil.exceptions._
//import edu.illinois.ncsa.daffodil.util.Maybe
//import edu.illinois.ncsa.daffodil.equality._
//import com.ibm.icu.text.SimpleDateFormat
//import com.ibm.icu.util.Calendar
//import com.ibm.icu.util.GregorianCalendar
//import com.ibm.icu.util.TimeZone
//import com.ibm.icu.util.ULocale
//
///**
// * Runtime valued properties that are enums would all work like ByteOrder here.
// */
//class ByteOrderEv(expr: CompiledExpression[String], erd: ElementRuntimeData)
//  extends EvaluatableConvertedExpression[String, ByteOrder](
//    erd.qNameForProperty("byteOrder"),
//    expr,
//    ByteOrder,
//    erd) {
//  override def runtimeDependencies = Nil
//
//}
//
///**
// * Encoding is a string, so there is no converter.
// */
//class EncodingEv(expr: CompiledExpression[String], trd: TermRuntimeData)
//  extends EvaluatableExpression[String](
//    trd.qNameForProperty("encoding"),
//    expr,
//    trd) {
//  override def runtimeDependencies = Nil
//}
//
///**
// * Singleton Ok is for returning from checks and such which will either throw SDE
// * or succeed. It means "check succeeded"
// */
//class Ok private () {
//  override def toString = "Ok"
//}
//object Ok extends Ok()
//
//class CheckByteAndBitOrderEv(t: TermRuntimeData, bitOrder: BitOrder, maybeByteOrder: Maybe[ByteOrderEv])
//  extends Evaluatable[Ok](t) { // can't use unit here, not <: AnyRef
//
//  override lazy val runtimeDependencies =
//    if (maybeByteOrder.isEmpty) Nil
//    else maybeByteOrder.value +: maybeByteOrder.value.runtimeDependencies
//
//  final protected def compute(state: ParseOrUnparseState): Ok = {
//    if (maybeByteOrder.isEmpty) return Ok
//    val byteOrderEv = maybeByteOrder.get
//    val byteOrder = byteOrderEv.evaluate(state)
//    bitOrder match {
//      case BitOrder.MostSignificantBitFirst => // ok
//      case BitOrder.LeastSignificantBitFirst =>
//        if (byteOrder =:= ByteOrder.BigEndian) {
//          t.schemaDefinitionError("Bit order 'leastSignificantBitFirst' requires byte order 'littleEndian', but byte order was '%s'.", byteOrder)
//        }
//    }
//    Ok
//  }
//}
//
//object LocaleConverter extends Converter[String, ULocale] {
//
//  val regex = "([A-Za-z]{1,8}([-_][A-Za-z0-9]{1,8})*)"
//  val localePattern = java.util.regex.Pattern.compile(regex)
//
//  protected def convert(b: String, context: ThrowsSDE, forUnparse: Boolean): ULocale = {
//    val m = localePattern.matcher(b)
//    if (m.matches()) {
//      val canonicalCalLang = ULocale.canonicalize(b)
//      Assert.invariant(canonicalCalLang ne null)
//      val l = new ULocale(canonicalCalLang)
//      l
//    } else {
//      context.schemaDefinitionError("dfdl:calendarLanguage property syntax error. Must match '%s' (ex: 'en_us' or 'de_1996'), but was '%s'.", regex, b)
//    }
//  }
//}
//
//class CalendarLanguageEv(calendarLanguageExpr: CompiledExpression[String], erd: ElementRuntimeData)
//  extends EvaluatableConvertedExpression[String, ULocale](
//    erd.qNameForProperty("calendarLanguage"),
//    calendarLanguageExpr,
//    LocaleConverter,
//    erd) {
//  override def runtimeDependencies = Nil
//}
//
//class CalendarEv(localeEv: CalendarLanguageEv,
//  calendarTz: Option[TimeZone],
//  firstDay: Int,
//  calendarDaysInFirstWeek: Int,
//  calendarCheckPolicy: Boolean,
//  erd: ElementRuntimeData) extends Evaluatable[Calendar](erd) {
//
//  override lazy val runtimeDependencies = Seq(localeEv)
//
//  override def compute(state: ParseOrUnparseState) = {
//    // Used to configure the dataFormatter
//    val locale = localeEv.evaluate(state)
//    val cal = Calendar.getInstance(locale)
//    cal.setFirstDayOfWeek(firstDay)
//    cal.setMinimalDaysInFirstWeek(calendarDaysInFirstWeek)
//    cal.setLenient(calendarCheckPolicy)
//    val tz = {
//      // If 'no time zone', then use UNKNOWN_ZONE
//      //
//      // UNKNOWN_ZONE behaves just like GMT/UTC and will
//      // preserve the Date/Time values.
//      //
//      if (calendarTz.isDefined) calendarTz.get else TimeZone.UNKNOWN_ZONE
//    }
//    cal.setTimeZone(tz)
//    cal
//  }
//}
