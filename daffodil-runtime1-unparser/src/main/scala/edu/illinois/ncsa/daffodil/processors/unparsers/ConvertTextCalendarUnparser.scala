package edu.illinois.ncsa.daffodil.processors.unparsers

import com.ibm.icu.util.Calendar
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.ULocale
import edu.illinois.ncsa.daffodil.calendar._
import edu.illinois.ncsa.daffodil.processors._
import com.ibm.icu.util.GregorianCalendar

case class ConvertTextCalendarUnparser(erd: ElementRuntimeData,
  xsdType: String,
  prettyType: String,
  pattern: String,
  hasTZ: Boolean,
  localeEv: CalendarLanguageEv,
  calendarEv: CalendarEv,
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
  override lazy val runtimeDependencies = Seq(localeEv, calendarEv)

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
      case gc: GregorianCalendar => gc
      case dt: java.util.Date => dt
      // TODO: either always put GregorianCalendars in the infoset, or never.
      // This match/case should be unnecessary. See JIRA DFDL-1504
    }

    val str = df.format(calValue)

    node.overwriteDataValue(str)
  }
}
