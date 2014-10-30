package com.ibm.icu.util

import com.ibm.icu.text.SimpleDateFormat

trait ToDateTimeMixin { self: DFDLCalendar =>
  def toDateTime(): DFDLDateTime = new DFDLDateTime(calendar.clone().asInstanceOf[Calendar])
}

trait ToTimeMixin { self: DFDLCalendar =>
  def toTime(): DFDLTime = new DFDLTime(calendar.clone().asInstanceOf[Calendar])
}

trait ToDateMixin { self: DFDLCalendar =>
  def toDate(): DFDLDate = new DFDLDate(calendar.clone().asInstanceOf[Calendar])
}

case class DFDLDate(calendar: Calendar)
  extends DFDLCalendar
  with ToDateTimeMixin with ToDateMixin {
  lazy val formattedStr: String = createFormatString(dateFormat)
}

case class DFDLTime(calendar: Calendar)
  extends DFDLCalendar
  with ToTimeMixin {
  lazy val formattedStr: String = createFormatString(timeFormat)
}

case class DFDLDateTime(calendar: Calendar)
  extends DFDLCalendar
  with ToDateTimeMixin with ToDateMixin with ToTimeMixin {
  lazy val formattedStr: String = createFormatString(dateTimeFormat)
}

abstract class DFDLCalendar
  extends Calendar {
  final val dateTimeFormat: String = "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx"
  final val dateFormat: String = "uuuu-MM-ddxxxxx"
  final val timeFormat: String = "HH:mm:ss.SSSSSSxxxxx"

  def calendar: Calendar
  def formattedStr: String

  protected def handleComputeMonthStart(x$1: Int, x$2: Int, x$3: Boolean): Int = calendar.handleComputeMonthStart(x$1, x$2, x$3)
  protected def handleGetExtendedYear(): Int = calendar.handleGetExtendedYear()
  protected def handleGetLimit(x$1: Int, x$2: Int): Int = calendar.handleGetLimit(x$1, x$2)
  def getField(fieldIndex: Int): Int = calendar.get(fieldIndex)

  def getCalendar() = calendar
  override def toString(): String = formattedStr

  protected def createFormatString(formatString: String): String = {
    val format = new SimpleDateFormat(formatString)
    format.setCalendar(calendar)

    var formattedString: String = null
    try {
      formattedString = format.format(calendar)
    } catch {
      case ex: java.lang.IllegalArgumentException =>
        throw new java.lang.IllegalArgumentException("Calendar content failed to match the format '%s' due to %s".format(formatString, ex.getMessage()))
    }
    formattedString
  }

}

