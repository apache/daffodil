package edu.illinois.ncsa.daffodil.processors.unparsers

import com.ibm.icu.util.Calendar
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.ULocale
import edu.illinois.ncsa.daffodil.calendar._
import edu.illinois.ncsa.daffodil.processors._

case class ConvertTextCalendarUnparser(erd: ElementRuntimeData,
  xsdType: String,
  prettyType: String,
  pattern: String,
  hasTZ: Boolean,
  localeEv: Evaluatable[ULocale],
  calendarEv: Evaluatable[Calendar],
  infosetPattern: String,
  firstDay: Int,
  calendarDaysInFirstWeek: Int,
  calendarCheckPolicy: Boolean,
  calendarTz: Option[TimeZone],
  tz: TimeZone)
  extends ConvertTextCalendarProcessorBase(erd,
    xsdType, prettyType, pattern, hasTZ, localeEv, calendarEv, infosetPattern, firstDay, calendarDaysInFirstWeek,
    calendarCheckPolicy, calendarTz, tz)
  with PrimUnparser {

  /**
   * Primitive unparsers must override runtimeDependencies
   */
  override def runtimeDependencies = Seq(localeEv, calendarEv)

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

    val dfdlCal = node.dataValue.asInstanceOf[DFDLCalendar]

    val str = df.format(dfdlCal.calendar)

    node.overwriteDataValue(str)
  }
}
