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

package edu.illinois.ncsa.daffodil.calendar

import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar;
import com.ibm.icu.util.TimeZone;

import edu.illinois.ncsa.daffodil.exceptions.Assert
import com.ibm.icu.impl.OlsonTimeZone
import java.text.ParsePosition
import scala.collection.mutable.ArraySeq
import edu.illinois.ncsa.daffodil.processors.TextCalendarConstants

object DFDLCalendarOrder extends Enumeration {
  type DFDLCalendarOrder = Value
  val P_LESS_THAN_Q, P_GREATER_THAN_Q, P_EQUAL_Q, P_NOT_EQUAL_Q = Value
}

trait OrderedCalendar { self: DFDLCalendar =>
  protected lazy val fields = ArraySeq(Calendar.EXTENDED_YEAR, Calendar.MONTH,
    Calendar.DAY_OF_MONTH, Calendar.HOUR_OF_DAY, Calendar.MINUTE,
    Calendar.SECOND)

  import DFDLCalendarOrder._

  /**
   * From Section 3.2.7.4 of http://www.w3.org/TR/xmlschema-2/#dateTime
   *
   *    A. Normalize P and Q. That is, if there is a timezone present, but it
   *       is not Z, convert it to Z using the addition operation defined in:
   *       http://www.w3.org/TR/xmlschema-2/#adding-durations-to-dateTimes
   *
   *       Thus 2000-03-04T23:00:00+03:00 normalizes to 2000-03-04T20:00:00Z
   *
   *    B. If P and Q either both have a time zone or both do not have a
   *       time zone, compare P and Q field by field from the year field
   *       down to the second field, and return a result as soon as it
   *       can be determined. That is:
   *
   *         1. For each i in {year, month, day, hour, minute, second}
   *         		a. If P[i] and Q[i] are both not specified, continue
   *                 to the next i
   *              b. If P[i] is not specified and Q[i] is, or vice versa,
   *                 stop and return P <> Q
   *              c. If P[i] < Q[i], stop and return P < Q
   *              d. If P[i] > Q[i], stop and return P > Q
   *         2. Stop and return P = Q
   *
   *    C. Otherwise, if P contains a time zone and Q does not,
   *       compare as follows:
   *
   *         1. P < Q if P < (Q with time zone +14:00)
   *         2. P > Q if P > (Q with time zone -14:00)
   *         3. P <> Q otherwise, that is,
   *            if (Q with time zone +14:00) < P < (Q with time zone -14:00)
   *
   *    D. Otherwise, if P does not contain a time zone and Q does,
   *       compare as follows:
   *
   *         1. P < Q if (P with time zone -14:00) < Q.
   *         2. P > Q if (P with time zone +14:00) > Q.
   *         3. P <> Q otherwise, that is,
   *            if (P with time zone +14:00) < Q < (P with time zone -14:00)
   */
  def order(p: DFDLDateTime, q: DFDLDateTime): DFDLCalendarOrder = {

    val pHasTZ = p.hasTimeZone
    val qHasTZ = q.hasTimeZone

    // Step 1 of the algorithm is to normalize the dates to Z time zone.
    //
    val pPrime = p.getNormalizedCalendar
    val qPrime = q.getNormalizedCalendar

    val res: DFDLCalendarOrder = {
      if ((pHasTZ && qHasTZ) || (!pHasTZ && !qHasTZ)) { orderIgnoreTimeZone(pPrime, qPrime) }
      else if (pHasTZ && !qHasTZ) {
        val qPlus = qPrime.getDateTimePlusFourteenHours

        if (orderIgnoreTimeZone(pPrime, qPlus) == DFDLCalendarOrder.P_LESS_THAN_Q) { DFDLCalendarOrder.P_LESS_THAN_Q }
        else {
          val qMinus = q.getDateTimeMinusFourteenHours
          if (orderIgnoreTimeZone(pPrime, qMinus) == DFDLCalendarOrder.P_GREATER_THAN_Q) { DFDLCalendarOrder.P_GREATER_THAN_Q }
          else { DFDLCalendarOrder.P_NOT_EQUAL_Q }
        }
      } else if (!pHasTZ && qHasTZ) {
        val pMinus = pPrime.getDateTimeMinusFourteenHours

        if (orderIgnoreTimeZone(pMinus, qPrime) == DFDLCalendarOrder.P_LESS_THAN_Q) { DFDLCalendarOrder.P_LESS_THAN_Q }
        else {
          val pPlus = pPrime.getDateTimePlusFourteenHours
          if (orderIgnoreTimeZone(pPlus, qPrime) == DFDLCalendarOrder.P_GREATER_THAN_Q) { DFDLCalendarOrder.P_GREATER_THAN_Q }
          else { DFDLCalendarOrder.P_NOT_EQUAL_Q }
        }
      } else { Assert.impossibleCase }
    }

    res
  }

  /**
   * If P and Q either both have a time zone or both do not have a
   * time zone, compare P and Q field by field from the year field
   * down to the second field, and return a result as soon as it
   * can be determined.
   */
  private def orderIgnoreTimeZone(p: DFDLDateTime, q: DFDLDateTime): DFDLCalendarOrder = {
    Assert.invariant((p.hasTimeZone && q.hasTimeZone) || (!p.hasTimeZone && !q.hasTimeZone))

    val length = fields.length
    for (i <- 0 to length) {
      if (i == length) return DFDLCalendarOrder.P_EQUAL_Q

      val field = fields(i)

      val hasPSubI = p.isFieldSet(field)
      val hasQSubI = q.isFieldSet(field)

      if (!hasPSubI && !hasQSubI) { /* continue */ }
      else if (hasPSubI ^ hasQSubI) return DFDLCalendarOrder.P_NOT_EQUAL_Q
      else {
        val pSubI = p.getField(field)
        val qSubI = q.getField(field)

        if (pSubI < qSubI) { return DFDLCalendarOrder.P_LESS_THAN_Q }
        else if (pSubI > qSubI) { return DFDLCalendarOrder.P_GREATER_THAN_Q }
        else { /* continue */ }
      }
    }
    DFDLCalendarOrder.P_EQUAL_Q
  }
}

trait ToDateTimeMixin { self: DFDLCalendar =>
  def toDateTime(): DFDLDateTime = new DFDLDateTime(calendar.clone().asInstanceOf[Calendar], self.hasTimeZone)
}

trait ToTimeMixin { self: DFDLCalendar =>
  def toTime(): DFDLTime = new DFDLTime(calendar.clone().asInstanceOf[Calendar], self.hasTimeZone)
}

trait ToDateMixin { self: DFDLCalendar =>
  def toDate(): DFDLDate = new DFDLDate(calendar.clone().asInstanceOf[Calendar], self.hasTimeZone)
}

case class DFDLDate(calendar: Calendar, parsedTZ: Boolean)
  extends DFDLCalendar(parsedTZ: Boolean)
  with ToDateTimeMixin with ToDateMixin {

  /**
   * This apply method supplies a way to just specify 'no time zone' when
   * one was not expected.  This just so happens to be the same as GMT.
   */
  def apply(cal: Calendar, expectsTZ: Boolean) = {
    if (!expectsTZ) cal.setTimeZone(TimeZone.UNKNOWN_ZONE)
    new DFDLDate(cal, expectsTZ)
  }

  @transient override lazy val tlFormatter = if (this.hasTimeZone) TextCalendarConstants.tlDateInfosetFormatter else TextCalendarConstants.tlDateNoTZInfosetFormatter

  override def equals(other: Any): Boolean = other match {
    case that: DFDLDate => this.toDateTimeWithReference equals that.toDateTimeWithReference
    case _ => Assert.invariantFailed("xs:date can only ever be compared with an xs:date.")
  }

  // Operators == and != taken care of by overriding of equals method
  //
  def >(that: DFDLDate): Boolean = this.toDateTimeWithReference > that.toDateTimeWithReference
  def <(that: DFDLDate): Boolean = this.toDateTimeWithReference < that.toDateTimeWithReference
  def >=(that: DFDLDate): Boolean = this.toDateTimeWithReference >= that.toDateTimeWithReference
  def <=(that: DFDLDate): Boolean = this.toDateTimeWithReference <= that.toDateTimeWithReference

  /**
   * Used by xs:date comparison ops.
   *
   * Because we are converting a Date to a DateTime, we
   * need to zero out the Hour, Minute, Second.
   */
  private def toDateTimeWithReference: DFDLDateTime = {
    val dateCal = calendar.clone().asInstanceOf[Calendar]
    // Rather than setting these to the reference time, it was
    // pointed out in the review that we could just 'clear' these
    // values and achieve the same effect, but optimized
    //
    dateCal.clear(Calendar.HOUR_OF_DAY)
    dateCal.clear(Calendar.MINUTE)
    dateCal.clear(Calendar.SECOND)
    new DFDLDateTime(dateCal, parsedTZ)
  }
}

case class DFDLTime(calendar: Calendar, parsedTZ: Boolean)
  extends DFDLCalendar(parsedTZ)
  with ToTimeMixin {

  /**
   * This apply method supplies a way to just specify 'no time zone' when
   * one was not expected.  This just so happens to be the same as GMT.
   */
  def apply(cal: Calendar, expectsTZ: Boolean) = {
    if (!expectsTZ) cal.setTimeZone(TimeZone.UNKNOWN_ZONE)
    new DFDLTime(cal, expectsTZ)
  }

  @transient override lazy val tlFormatter = if (this.hasTimeZone) TextCalendarConstants.tlTimeInfosetFormatter else TextCalendarConstants.tlTimeNoTZInfosetFormatter

  override def equals(other: Any): Boolean = other match {
    case that: DFDLTime => this.toDateTimeWithReference equals that.toDateTimeWithReference
    case _ => Assert.invariantFailed("xs:time can only ever be compared with xs:time")
  }

  // Operators == and != taken care of by overriding of equals method
  //
  def >(that: DFDLTime): Boolean = this.toDateTimeWithReference > that.toDateTimeWithReference
  def <(that: DFDLTime): Boolean = this.toDateTimeWithReference < that.toDateTimeWithReference
  def >=(that: DFDLTime): Boolean = this.toDateTimeWithReference >= that.toDateTimeWithReference
  def <=(that: DFDLTime): Boolean = this.toDateTimeWithReference <= that.toDateTimeWithReference

  /**
   * Used by xs:time comparison ops.
   *
   * Because we are converting a Time to a DateTime,
   * we need to provide a dummy/reference date.
   *
   * The spec uses 12-31-1972 as the reference date in its examples.
   */
  private def toDateTimeWithReference: DFDLDateTime = {
    val timeCal = calendar.clone().asInstanceOf[Calendar]
    // Rather than setting these to the reference date, it was
    // pointed out in the review that we could just 'clear' these
    // values and achieve the same effect, but optimized
    //
    timeCal.clear(Calendar.YEAR)
    timeCal.clear(Calendar.EXTENDED_YEAR)
    timeCal.clear(Calendar.MONTH)
    timeCal.clear(Calendar.DAY_OF_MONTH)
    new DFDLDateTime(timeCal, parsedTZ)
  }

}

case class DFDLDateTime(calendar: Calendar, parsedTZ: Boolean)
  extends DFDLCalendar(parsedTZ)
  with ToDateTimeMixin with ToDateMixin with ToTimeMixin {

  /**
   * This apply method supplies a way to just specify 'no time zone' when
   * one was not expected.  This just so happens to be the same as GMT.
   */
  def apply(cal: Calendar, expectsTZ: Boolean) = {
    if (!expectsTZ) cal.setTimeZone(TimeZone.UNKNOWN_ZONE)
    new DFDLDateTime(cal, expectsTZ)
  }

  @transient override lazy val tlFormatter = if (this.hasTimeZone) TextCalendarConstants.tlDateTimeInfosetFormatter else TextCalendarConstants.tlDateTimeNoTZInfosetFormatter

  override def equals(other: Any) = other match {
    case that: DFDLDateTime => dateTimeEqual(this, that)
    case _ => Assert.invariantFailed("xs:dateTime can only ever be compared with xs:dateTime")
  }

  // Operators == and != taken care of by overriding of equals method
  //
  def >(that: DFDLDateTime): Boolean = dateTimeGreaterThan(this, that)
  def <(that: DFDLDateTime): Boolean = dateTimeLessThan(this, that)
  def >=(that: DFDLDateTime): Boolean = !dateTimeLessThan(this, that)
  def <=(that: DFDLDateTime): Boolean = !dateTimeGreaterThan(this, that)

  protected lazy val normalizedCalendar = normalizeCalendar(calendar)

  def getDateTimePlusFourteenHours: DFDLDateTime = {
    val adjustedCal = adjustTimeZone(normalizedCalendar, 14, 0)
    val dt = new DFDLDateTime(adjustedCal, parsedTZ)
    dt
  }

  def getDateTimeMinusFourteenHours: DFDLDateTime = {
    val adjustedCal = adjustTimeZone(normalizedCalendar, -14, 0)
    val dt = new DFDLDateTime(adjustedCal, parsedTZ)
    dt
  }

  /**
   * Normalize the calendar to GMT
   */
  def normalizeCalendar(cal: Calendar): Calendar = {
    val newCal = cal.clone().asInstanceOf[Calendar]

    // TimeZone.UNKNOWN_ZONE behaves like GMT/UTC
    //
    if (cal.getTimeZone() == TimeZone.GMT_ZONE ||
      cal.getTimeZone() == TimeZone.UNKNOWN_ZONE) { return newCal }

    // Need to multiply the offset by -1 to get the right
    // sign to 'add' to the millisecond field
    //
    val offset = newCal.get(Calendar.ZONE_OFFSET) * -1

    newCal.clear(Calendar.ZONE_OFFSET)
    newCal.setTimeZone(TimeZone.GMT_ZONE)
    newCal.add(Calendar.MILLISECOND, offset)

    val date = newCal.getTime()

    newCal
  }

  def adjustTimeZone(cal: Calendar, hours: Int, minutes: Int): Calendar = {
    val newCal = cal.clone().asInstanceOf[Calendar]

    newCal.add(Calendar.HOUR_OF_DAY, hours)
    newCal.add(Calendar.MINUTE, minutes)
    newCal.getTime()
    newCal
  }

  def getNormalizedCalendar(): DFDLDateTime = {
    new DFDLDateTime(normalizedCalendar.clone().asInstanceOf[Calendar], parsedTZ)
  }

  /**
   * Returns true if and only if the value of $v1 is equal to the value
   * of $v2 according to the algorithm defined in section 3.2.7.4 of
   * [XML Schema Part 2: Datatypes Second Edition] "Order relation on dateTime"
   *  for xs:dateTime values with timezones. Returns false otherwise.
   *
   * This function backs up the "eq", "ne", "le" and "ge" operators on
   * xs:dateTime values.
   */
  private def dateTimeEqual(v1: Any, v2: Any): Boolean = {
    val p = v1.asInstanceOf[DFDLDateTime]
    val q = v2.asInstanceOf[DFDLDateTime]

    order(p, q) == DFDLCalendarOrder.P_EQUAL_Q
  }

  /**
   * Returns true if and only if the value of $v1 is less than the value
   * of $v2 according to the algorithm defined in section 3.2.7.4
   * of [XML Schema Part 2: Datatypes Second Edition]
   * "Order relation on dateTime" for xs:dateTime values with timezones.
   * Returns false otherwise.
   *
   * This function backs up the "lt" and "le" operators on xs:dateTime values.
   */
  private def dateTimeLessThan(v1: Any, v2: Any): Boolean = {
    val p = v1.asInstanceOf[DFDLDateTime]
    val q = v2.asInstanceOf[DFDLDateTime]

    order(p, q) == DFDLCalendarOrder.P_LESS_THAN_Q
  }

  /**
   * Returns true if and only if the value of $v1 is greater than the
   * value of $v2 according to the algorithm defined in section 3.2.7.4 of
   * [XML Schema Part 2: Datatypes Second Edition] "Order relation on dateTime"
   * for xs:dateTime values with timezones. Returns false otherwise.
   *
   * This function backs up the "gt" and "ge" operators on xs:dateTime values.
   */
  private def dateTimeGreaterThan(v1: Any, v2: Any): Boolean = {
    val p = v1.asInstanceOf[DFDLDateTime]
    val q = v2.asInstanceOf[DFDLDateTime]

    order(p, q) == DFDLCalendarOrder.P_GREATER_THAN_Q
  }
}

abstract class DFDLCalendar(containsTZ: Boolean)
  extends OrderedCalendar with Serializable {

  /* Print formats */
  final val dateTimeFormatNoTZ: String = "uuuu-MM-dd'T'HH:mm:ss.SSSSSS"
  final val dateFormatNoTZ: String = "uuuu-MM-dd"
  final val timeFormatNoTZ: String = "HH:mm:ss.SSSSSS"
  final val dateTimeFormat: String = "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx"
  final val dateFormat: String = "uuuu-MM-ddxxxxx"
  final val timeFormat: String = "HH:mm:ss.SSSSSSxxxxx"
  final val tzFormat: String = "xxx" // -08:00 The ISO8601 extended format with hours and minutes fields.

  def calendar: Calendar
  def tlFormatter: ThreadLocal[SimpleDateFormat]

  def getField(fieldIndex: Int): Int = calendar.get(fieldIndex)
  def isFieldSet(fieldIndex: Int): Boolean = calendar.isSet(fieldIndex)
  def hasTimeZone: Boolean = {
    // There does not appear to really be a concept of 'no time zone'
    // so we use a regex or other means of elimination to determine
    // if a time zone was expected or parsed.  This is passed as a flag
    // to the DFDLCalendar object.
    //
    // On a side note, it does appear that we can specify an 'unknown'
    // time zone using TimeZone.UNKNOWN_ZONE.  However, this behaves like
    // the GMT/UTC time zone.
    //
    // To be clear, when no time zone is present (or expected) we set
    // the calendar time zone to TimeZone.UNKNOWN_ZONE and carry around
    // this flag.  This avoids accessing calendar.getZone and comparing
    // against TimeZone.UNKNOWN_ZONE, but allows the calendar object
    // to somewhat represent 'no time zone'.
    //
    containsTZ
  }

  /**
   * Returns the TimeZone in the format of "+00:00"
   */
  def getTimeZoneString: String = {
    val formatter = TextCalendarConstants.tlTzInfosetFormatter.get
    val formattedString = try {
      formatter.format(calendar)
    } catch {
      case ex: java.lang.IllegalArgumentException =>
        throw new java.lang.IllegalArgumentException("Calendar content failed to match the format '%s' due to %s".format(formatter.toPattern, ex.getMessage()))
    }
    formattedString
  }

  /**
   * Returns the ICU Calendar object.
   *
   * We represent 'No time zone' by TimeZone.UNKNOWN_ZONE
   */
  def getCalendar() = calendar
  override def toString(): String = {
    val formatter = tlFormatter.get
    formatter.setCalendar(calendar)

    val formattedString = try {
      formatter.format(calendar)
    } catch {
      case ex: java.lang.IllegalArgumentException =>
        throw new java.lang.IllegalArgumentException("Calendar content failed to match the format '%s' due to %s".format(formatter.toPattern, ex.getMessage()))
    }
    formattedString
  }

}

