/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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
import com.ibm.icu.util.SimpleTimeZone
import com.ibm.icu.util.TimeZone
import java.nio.ByteBuffer
import AsIntConverters._
import edu.illinois.ncsa.daffodil.calendar.DFDLDateTime
import edu.illinois.ncsa.daffodil.calendar.DFDLCalendar
import edu.illinois.ncsa.daffodil.calendar.DFDLTime
import edu.illinois.ncsa.daffodil.calendar.DFDLDate

case object AnyAtomicToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case c: DFDLCalendar => c.toString
      case _ => a.asInstanceOf[String]
    }
  }
}

trait XSDateTimeKind {
  val timeZoneID = "UTC"
  lazy val calendar = new ThreadLocal[Calendar] {
    override def initialValue = {
      val cal = Calendar.getInstance()
      cal.clear()
      cal.setLenient(false)
      //
      // We don't want to set a TimeZone here.  It should be 'parsed' if it's
      // there.  If it's not, the TimeZone will be set to TimeZone.UNKNOWN_ZONE
      // which will operate just like GMT/UTC
      //
      cal
    }
  }

  def defaultFormatter: ThreadLocal[DFDLDateFormat]
  def acceptableFormats: Seq[DFDLDateFormat]

  def getNewCalendar: Calendar = calendar.get.clone().asInstanceOf[Calendar]

  protected def createCalendar(str: String, inFormat: SimpleDateFormat, expectsTZ: Boolean,
    fncName: String, toType: String): DFDLCalendar

  def matchFormat(str: String, fncName: String, toType: String): DFDLCalendar = {

    acceptableFormats.foreach(f => {
      val inFormat = f.getFormat
      inFormat.setCalendar(getNewCalendar)
      try {
        val cal = createCalendar(str, inFormat, f.expectsTimeZone, fncName, toType)
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

case object StringToDate extends Converter with XSDateTimeKind with DateFormatters {
  val name = "StringToDate"

  /**
   * A list of acceptable formats as specified by: http://www.w3.org/TR/NOTE-datetime
   *
   * Order matters here as we are also trying to determine if a time zone was parsed
   */
  def acceptableFormats = Seq(defaultFormatter.get, withoutTimezoneFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat, expectsTZ: Boolean, fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLDate(str, inFormat, expectsTZ, fncName, toType)
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

  /**
   * A list of acceptable formats as specified by: http://www.w3.org/TR/NOTE-datetime
   *
   * Order matters here as we are also trying to determine if a time zone was parsed
   */
  def acceptableFormats = Seq(defaultFormatter.get, withFractNoTimeZoneFormatter.get,
    withTimeZoneNoFractFormatter.get, noTimeZoneNoFractFormatter.get,
    withTimeZoneFormatter.get, dateOnlyFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat,
    expectsTZ: Boolean, fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLDateTime(str, inFormat, expectsTZ, fncName, toType)
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

  /**
   * A list of acceptable formats as specified by: http://www.w3.org/TR/NOTE-datetime
   *
   * Order matters here as we are also trying to determine if a time zone was parsed
   */
  def acceptableFormats = Seq(defaultFormatter.get, noTimeZoneFormatter.get,
    withTimeZoneNoFractFormatter.get, noTimeZoneNoFractFormatter.get)

  protected def createCalendar(str: String, inFormat: SimpleDateFormat,
    expectsTZ: Boolean, fncName: String, toType: String): DFDLCalendar = {
    Conversion.stringToDFDLTime(str, inFormat, expectsTZ, fncName, toType)
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