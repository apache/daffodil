package daffodil.processors

import java.text.ParsePosition
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.{ Calendar, TimeZone, GregorianCalendar, ULocale }

import daffodil.dsom._
import daffodil.exceptions.Assert
import daffodil.exceptions.UnsuppressableException
import daffodil.grammar.Terminal
import daffodil.grammar.Gram
import daffodil.schema.annotation.props.gen.CalendarPatternKind
import daffodil.schema.annotation.props.gen.CalendarFirstDayOfWeek
import daffodil.schema.annotation.props.gen.CalendarCheckPolicy

case class ConvertTextCalendarParser(gram: Gram, e: ElementBase, dataFormatter: SimpleDateFormat, infosetFormatter: SimpleDateFormat) extends PrimParser(gram, e) {

  protected val GramName = e.primType.name
  protected val GramDescription = { GramName(0).toUpper + GramName.substring(1) }

  override def toString = "to(xs:" + GramName + ")"

  def parse(start: PState): PState = {

    val node = start.parentElement
    var str = node.dataValue

    Assert.invariant(str != null)

    val resultState = try {
      if (str == "") {
        return PE(start, "Convert to %s (for xs:%s): Cannot parse calendar from empty string", GramDescription, GramName)
      }

      val pos = new ParsePosition(0)
      val datetime = try {
        dataFormatter.parse(str, pos);
      } catch {
        case u: UnsuppressableException => throw u
        case e: Exception =>
          return PE(start, "Convert to %s (for xs:%s): Parse of '%s' threw exception %s", GramDescription, GramName, str, e)
      }

      // Verify that what was parsed was what was passed exactly in byte count
      // Use pos to verify all characters consumed & check for errors!
      if (pos.getIndex != str.length) {
        return PE(start, "Convert to %s (for xs:%s): Failed to parse '%s' at character %d .", GramDescription, GramName, str, pos.getErrorIndex)
      }

      // Convert to the infoset format
      val result = infosetFormatter.format(datetime)
      node.setDataValue(result)

      start
    }

    resultState
  }
}

abstract class ConvertTextCalendarPrimBase(e: ElementBase, guard: Boolean)
  extends Terminal(e, guard) {

  protected val GramName = e.primType.name
  protected val GramDescription = { GramName(0).toUpper + GramName.substring(1) }

  override def toString = "to(xs:" + GramName + ")"

  protected def infosetPattern: String
  protected def implicitPattern: String
  protected def validFormatCharacters: Seq[Char]

  lazy val pattern: String = {
    val p = e.calendarPatternKind match {
      case CalendarPatternKind.Explicit => e.calendarPattern
      case CalendarPatternKind.Implicit => implicitPattern
    }

    val escapedText = "(''|'[^']+'|[^a-zA-Z])".r
    val patternNoEscapes = escapedText.replaceAllIn(p, "")
    patternNoEscapes.toSeq.foreach(char =>
      if (!validFormatCharacters.contains(char)) {
        SDE("Character '%s' not allowed in dfdl:calendarPattern for %s".format(char,GramName))
      }
    )

    p
  }

  lazy val locale: ULocale = {
    val locales = ULocale.getAvailableLocales()
    val l = locales.find(l => l.getName == e.calendarLanguage) match {
      case Some(l) => l
      case None => SDE("Unknown language specified for dfdl:calendarLanguage: %s", e.calendarLanguage)
    }

    l
  }

  // Used to configure the dataFormatter
  lazy val calendar: Calendar = {
    val cal = Calendar.getInstance(locale)

    val firstDay = e.calendarFirstDayOfWeek match {
      case CalendarFirstDayOfWeek.Sunday    => Calendar.SUNDAY
      case CalendarFirstDayOfWeek.Monday    => Calendar.MONDAY
      case CalendarFirstDayOfWeek.Tuesday   => Calendar.TUESDAY
      case CalendarFirstDayOfWeek.Wednesday => Calendar.WEDNESDAY
      case CalendarFirstDayOfWeek.Thursday  => Calendar.THURSDAY
      case CalendarFirstDayOfWeek.Friday    => Calendar.FRIDAY
      case CalendarFirstDayOfWeek.Saturday  => Calendar.SATURDAY
    }
    cal.setFirstDayOfWeek(firstDay)

    cal.setMinimalDaysInFirstWeek(e.calendarDaysInFirstWeek.toInt)

    val lax = e.calendarCheckPolicy match {
      case CalendarCheckPolicy.Strict => false
      case CalendarCheckPolicy.Lax => true
    }
    cal.setLenient(lax)

    val tz = TimeZone.getTimeZone(e.calendarTimeZone)
    if (tz == TimeZone.UNKNOWN_ZONE) {
      SDE("Unknown timezone specified for dfdl:calendarTimeZone: %s", e.calendarTimeZone)
    }
    cal.setTimeZone(tz)

    cal
  }

  // Used for parsing/unparsing
  lazy val dataFormatter: SimpleDateFormat = {
    val formatter = new SimpleDateFormat(pattern, locale)
    formatter.setCalendar(calendar)
    formatter
  }

  // Used for writing to/reading from the infoset
  lazy val infosetFormatter: SimpleDateFormat = {
    val formatter = new SimpleDateFormat(infosetPattern)
    // infoset is always Gregorian/UTC
    formatter.setCalendar(new GregorianCalendar())
    formatter.setTimeZone(TimeZone.GMT_ZONE)
    formatter
  }

  def parser: Parser = new ConvertTextCalendarParser(this, e, dataFormatter, infosetFormatter)

  def unparser: Unparser = DummyUnparser(e)
}

case class ConvertTextDatePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val infosetPattern = "yyyy-MM-dd"
  protected override val implicitPattern = "yyyy-MM-dd"
  protected override val validFormatCharacters = "dDeEFGMuwWyYzZ".toSeq
}

case class ConvertTextTimePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val infosetPattern = "HH:mm:ssZZZZZ"
  protected override val implicitPattern = "HH:mm:ssZZZ"
  protected override val validFormatCharacters = "ahHkKmsSzZ".toSeq
}

case class ConvertTextDateTimePrim(e: ElementBase) extends ConvertTextCalendarPrimBase(e, true) {
  protected override val infosetPattern = "yyyy-MM-dd'T'HH:mm:ssZZZZZ"
  protected override val implicitPattern = "yyyy-MM-dd'T'HH:mm:ss"
  protected override val validFormatCharacters = "adDeEFGhHkKmMsSuwWyYzZ".toSeq
}
