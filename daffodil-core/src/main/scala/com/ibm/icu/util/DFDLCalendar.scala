package com.ibm.icu.util

import com.ibm.icu.text.SimpleDateFormat

class DFDLCalendar(calendar: Calendar, formattedStr: String) extends Calendar {
  final val dateTimeFormat: String = "yyyy-MM-dd'T'HH:mm:ss.SSSSSSxxxxx"
  final val dateFormat: String = "yyyy-MM-ddxxxxx"
  final val timeFormat: String = "HH:mm:ss.SSSSSSxxxxx"

  protected def handleComputeMonthStart(x$1: Int, x$2: Int, x$3: Boolean): Int = calendar.handleComputeMonthStart(x$1, x$2, x$3)
  protected def handleGetExtendedYear(): Int = calendar.handleGetExtendedYear()
  protected def handleGetLimit(x$1: Int, x$2: Int): Int = calendar.handleGetLimit(x$1, x$2)
  def getField(fieldIndex: Int): Int = calendar.get(fieldIndex)
  //  def clear(): Unit = calendar.clear()
  //  def clear(field: Int) = calendar.clear(field)
  //  def getTime() = calendar.getTime()
  //  def getTimeInMillis() = calendar.getTimeInMillis()
  //  def isLenient() = calendar.isLenient()
  //  def isSet(field: Int) = calendar.isSet(field)

  def getCalendar() = calendar
  override def toString(): String = formattedStr
  
  private def convertTo(formatString: String): DFDLCalendar = {
    val cal: Calendar = calendar.clone().asInstanceOf[Calendar]
    val format = new SimpleDateFormat(formatString)
    var formattedString: String = null
    try {
      formattedString = format.format(cal)
    } catch {
      // TODO: What to do about erroring here?
      case ex: java.lang.IllegalArgumentException => //dstate.pstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, value.toString, toType, ex.getMessage())
      case ex: Exception => //dstate.pstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, value.toString, toType, ex.getMessage())
    }
    val newCal = new DFDLCalendar(cal, formattedString)
    newCal
  }
  
  def toDate(): DFDLCalendar = convertTo(dateFormat)
  def toTime(): DFDLCalendar = convertTo(timeFormat)
  def toDateTime(): DFDLCalendar = convertTo(dateTimeFormat)
}

