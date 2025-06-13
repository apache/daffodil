/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.lib.calendar

import java.math.{ BigDecimal => JBigDecimal }

import org.apache.daffodil.lib.exceptions.Assert

import com.ibm.icu.util.Calendar
import com.ibm.icu.util.TimeZone

object DFDLCalendarOrder extends Enumeration {
  type DFDLCalendarOrder = Value
  val P_LESS_THAN_Q, P_GREATER_THAN_Q, P_EQUAL_Q, P_NOT_EQUAL_Q = Value

  val fieldsForComparison = Array(
    Calendar.EXTENDED_YEAR,
    Calendar.MONTH,
    Calendar.DAY_OF_MONTH,
    Calendar.HOUR_OF_DAY,
    Calendar.MINUTE,
    Calendar.SECOND
  )
}

trait OrderedCalendar { self: DFDLCalendar =>
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
   *             a. If P[i] and Q[i] are both not specified, continue
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
    val pPrime = p.getNormalizedCalendar()
    val qPrime = q.getNormalizedCalendar()

    val res: DFDLCalendarOrder = {
      if ((pHasTZ && qHasTZ) || (!pHasTZ && !qHasTZ)) { orderCompareFields(pPrime, qPrime) }
      else if (pHasTZ && !qHasTZ) {
        val qPlus = qPrime.getDateTimePlusFourteenHours

        if (orderCompareFields(pPrime, qPlus) == DFDLCalendarOrder.P_LESS_THAN_Q)
          DFDLCalendarOrder.P_LESS_THAN_Q
        else {
          val qMinus = q.getDateTimeMinusFourteenHours
          if (orderCompareFields(pPrime, qMinus) == DFDLCalendarOrder.P_GREATER_THAN_Q)
            DFDLCalendarOrder.P_GREATER_THAN_Q
          else
            DFDLCalendarOrder.P_NOT_EQUAL_Q
        }
      } else if (!pHasTZ && qHasTZ) {
        val pMinus = pPrime.getDateTimeMinusFourteenHours

        if (orderCompareFields(pMinus, qPrime) == DFDLCalendarOrder.P_LESS_THAN_Q)
          DFDLCalendarOrder.P_LESS_THAN_Q
        else {
          val pPlus = pPrime.getDateTimePlusFourteenHours
          if (orderCompareFields(pPlus, qPrime) == DFDLCalendarOrder.P_GREATER_THAN_Q)
            DFDLCalendarOrder.P_GREATER_THAN_Q
          else
            DFDLCalendarOrder.P_NOT_EQUAL_Q
        }
      } else { Assert.impossibleCase }
    }

    res
  }

  /**
   * This method does the actual comparison of calendars that have been
   * normalized/modified according to the W3C specification in the order()
   * method above, by comparing the existence and values of various calendar
   * fields. Note that by the point this method is called the calendars should
   * either not have a timezone or have been normalized to a UTC timezone, so
   * the timezone field does not play a role in this comparison.
   */
  private def orderCompareFields(p: DFDLDateTime, q: DFDLDateTime): DFDLCalendarOrder = {
    Assert.invariant(!p.hasTimeZone || p.calendar.getTimeZone == TimeZone.GMT_ZONE)
    Assert.invariant(!q.hasTimeZone || q.calendar.getTimeZone == TimeZone.GMT_ZONE)

    val length = fieldsForComparison.length
    for (i <- 0 until length) {
      val field = fieldsForComparison(i)

      val hasPSubI = p.calendar.isSet(field)
      val hasQSubI = q.calendar.isSet(field)

      if (!hasPSubI && !hasQSubI) { /* continue */ }
      else if (hasPSubI ^ hasQSubI) return DFDLCalendarOrder.P_NOT_EQUAL_Q
      else {
        val pSubI = p.calendar.get(field)
        val qSubI = q.calendar.get(field)

        if (pSubI < qSubI) { return DFDLCalendarOrder.P_LESS_THAN_Q }
        else if (pSubI > qSubI) { return DFDLCalendarOrder.P_GREATER_THAN_Q }
        else { /* continue */ }
      }
    }
    DFDLCalendarOrder.P_EQUAL_Q
  }
}

trait ToDateTimeMixin { self: DFDLCalendar =>
  def toDateTime(): DFDLDateTime =
    DFDLDateTime(calendar.clone().asInstanceOf[Calendar], self.hasTimeZone)
}

trait ToTimeMixin { self: DFDLCalendar =>
  def toTime(): DFDLTime = DFDLTime(calendar.clone().asInstanceOf[Calendar], self.hasTimeZone)
}

trait ToDateMixin { self: DFDLCalendar =>
  def toDate(): DFDLDate = DFDLDate(calendar.clone().asInstanceOf[Calendar], self.hasTimeZone)
}

case class DFDLDate(calendar: Calendar, override val hasTimeZone: Boolean)
  extends DFDLCalendar
  with ToDateTimeMixin
  with ToDateMixin {

  override def toString(): String = DFDLDateConversion.toXMLString(this)

  override def equals(other: Any): Boolean = other match {
    case that: DFDLDate => this.toDateTimeWithReference.equals(that.toDateTimeWithReference)
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
    DFDLDateTime(dateCal, hasTimeZone)
  }

}

case class DFDLTime(calendar: Calendar, override val hasTimeZone: Boolean)
  extends DFDLCalendar
  with ToTimeMixin {

  override def toString(): String = DFDLTimeConversion.toXMLString(this)

  override def equals(other: Any): Boolean = other match {
    case that: DFDLTime => this.toDateTimeWithReference.equals(that.toDateTimeWithReference)
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
    DFDLDateTime(timeCal, hasTimeZone)
  }

}

case class DFDLDateTime(calendar: Calendar, override val hasTimeZone: Boolean)
  extends DFDLCalendar
  with ToDateTimeMixin
  with ToDateMixin
  with ToTimeMixin {

  override def toString(): String = DFDLDateTimeConversion.toXMLString(this)

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
    Assert.invariant(!hasTimeZone)
    val adjustedCal = adjustTimeZone(normalizedCalendar, 14, 0)
    val dt = DFDLDateTime(adjustedCal, hasTimeZone)
    dt
  }

  def getDateTimeMinusFourteenHours: DFDLDateTime = {
    Assert.invariant(!hasTimeZone)
    val adjustedCal = adjustTimeZone(normalizedCalendar, -14, 0)
    val dt = DFDLDateTime(adjustedCal, hasTimeZone)
    dt
  }

  /**
   * Normalize the calendar to GMT
   */
  def normalizeCalendar(cal: Calendar): Calendar = {
    val newCal = cal.clone().asInstanceOf[Calendar]

    // TimeZone.UNKNOWN_ZONE behaves like GMT/UTC
    //
    if (
      cal.getTimeZone() == TimeZone.GMT_ZONE ||
      cal.getTimeZone() == TimeZone.UNKNOWN_ZONE
    ) { return newCal }

    // Need to multiply the offset by -1 to get the right
    // sign to 'add' to the millisecond field
    //
    val offset = newCal.get(Calendar.ZONE_OFFSET) * -1

    newCal.clear(Calendar.ZONE_OFFSET)
    newCal.setTimeZone(TimeZone.GMT_ZONE)
    newCal.add(Calendar.MILLISECOND, offset)

    newCal.getTime()

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
    DFDLDateTime(normalizedCalendar.clone().asInstanceOf[Calendar], hasTimeZone)
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

/**
 * Abstract class for DFDL Date/Time/DateTime. Wrapper around ICU calendar.
 *
 * The main reason this is neeed  is there does not appear to really be a
 * concept of 'no time zone', so a hasTimeZone function must be implemented by
 * implementors specifying whether or not the calendar has a timezone.
 *
 * On a related note, it does appear that we can specify an 'unknown' time zone
 * using TimeZone.UNKNOWN_ZONE, since this behaves like the GMT/UTC time zone.
 *
 * To be clear, when no time zone is present (or expected) we set the calendar
 * time zone to TimeZone.UNKNOWN_ZONE and carry around the hasTimeZOne flag.
 * This avoids accessing calendar.getZone and comparing against
 * TimeZone.UNKNOWN_ZONE, but allows the calendar object to somewhat represent
 * 'no time zone'.
 */
abstract class DFDLCalendar extends OrderedCalendar with Serializable {

  // Ensure the calendar does not have a timezone set if the hasTimeZone flag
  // has not been set
  if (!hasTimeZone) calendar.setTimeZone(TimeZone.UNKNOWN_ZONE)

  def calendar: Calendar

  def hasTimeZone: Boolean

  final def toJBigDecimal: JBigDecimal = new JBigDecimal(calendar.getTimeInMillis())
}
