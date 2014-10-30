package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.processors._
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.OnStack
import edu.illinois.ncsa.daffodil.util.PreSerialization
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import scala.math.BigDecimal.RoundingMode
import edu.illinois.ncsa.daffodil.util.Bits
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import java.text.ParsePosition
import com.ibm.icu.util.DFDLCalendar
import com.ibm.icu.util.SimpleTimeZone
import com.ibm.icu.util.TimeZone
import java.nio.ByteBuffer
import com.ibm.icu.util.DFDLDateTime
import com.ibm.icu.util.DFDLDate
import com.ibm.icu.util.DFDLTime
import AsIntConverters._

case object AnyAtomicToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case c: DFDLCalendar => c.toString
      case _ => a.asInstanceOf[String]
    }
  }
}

case object TimeToDateTime extends Converter {
  override def computeValue(a: Any, dstate: DState) = ??? // a.asInstanceOf[Time].toDateTime
}
trait XSDateTimeKind {
  val timeZoneID = "UTC"
  lazy val calendar = new ThreadLocal[Calendar] {
    override def initialValue = {
      val cal = Calendar.getInstance()
      cal.clear()
      cal.setLenient(false)
      cal.setTimeZone(new SimpleTimeZone(0, timeZoneID))
      cal
    }
  }

  def defaultFormatter: ThreadLocal[SimpleDateFormat]
  def acceptableFormats: Seq[SimpleDateFormat]

  def getNewCalendar: Calendar = calendar.get.clone().asInstanceOf[Calendar]

  protected def createCalendar(str: String, inFormat: SimpleDateFormat,
    fncName: String, toType: String): DFDLCalendar

  def matchFormat(str: String, fncName: String, toType: String): DFDLCalendar = {

    acceptableFormats.foreach(f => {
      val inFormat = f
      inFormat.setCalendar(getNewCalendar)
      try {
        val cal = createCalendar(str, inFormat, fncName, toType)
        // Here we've successfully created a calendar using the expected format
        // denoted by 'inFormat'. Return the calendar.
        return cal
      } catch {
        case e: IllegalArgumentException => /* Format failed, continue trying to match other formats */
      }

    })
    // All acceptable formats failed
    throw new NumberFormatException("Failed to convert \"%s\" to %s.".format(str, toType))
  }
}

trait DateTimeFormatters {
  lazy val defaultFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx")
      format
    }
  }

  lazy val withFractNoTimeZoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss.SSSSSS")
      format
    }
  }

  lazy val withTimeZoneNoFractFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ssxxxxx")
      format
    }
  }

  lazy val noTimeZoneNoFractFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss")
      format
    }
  }

  lazy val withTimeZoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-ddxxxxx")
      format
    }
  }

  lazy val dateOnlyFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd")
      format
    }
  }
}

case object StringToDate extends Converter with XSDateTimeKind with DateFormatters {
  val name = "StringToDate"

  def acceptableFormats = Seq(defaultFormatter.get, withoutTimezoneFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat, fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLDate(str, inFormat, fncName, toType)
  }

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case cal: DFDLDateTime => cal.toDate
      case cal: DFDLDate => cal
      case str: String => matchFormat(str, name, "xs:date")
      case _ => throw new NumberFormatException("xs:date only accepts String, Date or DateTime objects.")
    }
    result
  }
}
case object StringToDateTime extends Converter
  with XSDateTimeKind with DateTimeFormatters {
  val name = "StringToDateTime"

  def acceptableFormats = Seq(defaultFormatter.get, withFractNoTimeZoneFormatter.get,
    noTimeZoneNoFractFormatter.get, withTimeZoneFormatter.get,
    withTimeZoneNoFractFormatter.get, dateOnlyFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat,
    fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLDateTime(str, inFormat, fncName, toType)
  }

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case cal: DFDLDateTime => cal
      case cal: DFDLDate => cal.toDateTime
      case str: String => matchFormat(str, name, "xs:dateTime")
      case _ => throw new NumberFormatException("xs:dateTime only accepts String, Date or DateTime objects.")
    }
    result
  }

}

case object StringToTime extends Converter with XSDateTimeKind with TimeFormatters {
  val name = "StringToTime"

  def acceptableFormats = Seq(defaultFormatter.get, noTimeZoneFormatter.get,
    noTimeZoneNoFractFormatter.get, withTimeZoneNoFractFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat,
    fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLTime(str, inFormat, fncName, toType)
  }

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case cal: DFDLDateTime => cal.toTime
      case cal: DFDLTime => cal
      case str: String => matchFormat(str, name, "xs:time")
      case _ => throw new NumberFormatException("xs:time only accepts String, DateTime or Time objects.")
    }
    result
  }

}