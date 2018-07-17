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

package org.apache.daffodil.processors.unparsers

import java.lang.{ Long => JLong }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInteger }

import org.apache.daffodil.calendar.DFDLCalendar
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.processors.CalendarEv
import org.apache.daffodil.processors.CalendarLanguageEv
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.parsers.ConvertTextCalendarProcessorBase
import org.apache.daffodil.processors.parsers.HasKnownLengthInBits
import org.apache.daffodil.processors.parsers.HasRuntimeExplicitLength
import org.apache.daffodil.schema.annotation.props.gen.BinaryCalendarRep
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.util.DecimalUtils
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.Misc

import com.ibm.icu.util.Calendar
import com.ibm.icu.util.ULocale

case class ConvertBinaryCalendarSecMilliUnparser(
  override val context: ElementRuntimeData,
  binCalRep: BinaryCalendarRep,
  epochTimeMillis: Long,
  lengthInBits: Int,
  hasTZ: Boolean)
  extends PrimUnparser {

  /**
   * Primitive unparsers must override runtimeDependencies
   */
  override lazy val runtimeDependencies = Nil

  protected def putNumber(dos: DataOutputStream, value: Long, nBits: Int, finfo: FormatInfo): Boolean = {
    dos.putLong(value, nBits, finfo)
  }

  def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple

    val calValue = node.dataValue match {
      case dc: DFDLCalendar => dc.calendar
      case x => Assert.invariantFailed("ConvertBinaryCalendar received unsupported type. %s of type %s.".format(x, Misc.getNameFromClass(x)))
    }

    // Adjust the time based on time zone - if a time zone wasn't specified, Calendar will assume the default
    // time zone for the user instead of TimeZone.UNKNOWN_ZONE so we need to adjust to get the correct time
    // Note that setting the correct time zone for the calendar will not adjust the time.
    val epochTime = if (!hasTZ) {
      val tz = calValue.getTimeZone
      val gmtOffset = calValue.get(Calendar.ZONE_OFFSET)
      val dstOffset = if (tz.inDaylightTime(calValue.getTime)) tz.getDSTSavings else 0
      epochTimeMillis - (gmtOffset + dstOffset)
    } else {
      epochTimeMillis
    }

    val diff: Long = binCalRep match {
      case BinaryCalendarRep.BinarySeconds => (calValue.getTimeInMillis - epochTime) / 1000
      case BinaryCalendarRep.BinaryMilliseconds => (calValue.getTimeInMillis - epochTime)
      case _ => Assert.impossibleCase
    }

    val dos = state.dataOutputStream
    val res = putNumber(dos, diff, lengthInBits, state)

    if (!res) {
      Assert.invariant(dos.maybeRelBitLimit0b.isDefined)
      UnparseError(One(state.schemaFileLocation), One(state.currentLocation), "Insufficient space to unparse element %s, required %s bits, but only %s were available.",
        context.dpathElementCompileInfo.namedQName.toPrettyString, lengthInBits, dos.maybeRelBitLimit0b.get)
    }
  }
}

abstract class BinaryCalendarBCDUnparser (
  override val context: ElementRuntimeData,
  pattern: String,
  localeEv: CalendarLanguageEv,
  calendarEv: CalendarEv)
  extends ConvertTextCalendarProcessorBase {

  protected def fromBigInteger(bigInt: JBigInteger, nBits: Int): Array[Byte] = DecimalUtils.bcdFromBigInteger(bigInt, nBits)

  protected def putNumber(dos: DataOutputStream, bigDec: JBigDecimal, nBits: Int, finfo: FormatInfo): Boolean = {
    val packedNum = fromBigInteger(bigDec.unscaledValue, nBits)
    dos.putByteArray(packedNum, packedNum.length * 8, finfo)
  }

  def doUnparse(state: UState, bitLength: Int): Unit = {

    val locale: ULocale = localeEv.evaluate(state)
    val calendar: Calendar = calendarEv.evaluate(state)

    calendar.clear()

    val df = tlDataFormatter(locale, calendar)
    df.setCalendar(calendar)

    val node = state.currentInfosetNode.asSimple

    val calValue = node.dataValue match {
      case dc: DFDLCalendar => dc.calendar
      case x => Assert.invariantFailed("ConvertBinaryCalendar received unsupported type. %s of type %s.".format(x, Misc.getNameFromClass(x)))
    }

    val str: String = df.format(calValue)

    val strAsBigDec: JBigDecimal = DecimalUtils.bcdToBigDecimal(DecimalUtils.bcdFromBigInteger(new JBigInteger(str), 0), 0)

    val dos = state.dataOutputStream
    val res = putNumber(dos, strAsBigDec, bitLength, state)

    if (!res) {
      Assert.invariant(dos.maybeRelBitLimit0b.isDefined)
      UnparseError(One(state.schemaFileLocation), One(state.currentLocation), "Insufficient space to unparse element %s, required %s bits, but only %s were available.",
        context.dpathElementCompileInfo.namedQName.toPrettyString, bitLength, dos.maybeRelBitLimit0b.get)
    }
  }
}

case class BinaryCalendarBCDKnownLengthUnparser(
  override val context: ElementRuntimeData,
  lengthInBits: Int,
  pattern: String,
  localeEv: CalendarLanguageEv,
  calendarEv: CalendarEv)
  extends BinaryCalendarBCDUnparser(context, pattern, localeEv, calendarEv)
  with PrimUnparser
  with HasKnownLengthInBits {

  /**
   * Primitive unparsers must override runtimeDependencies
   */
  override lazy val runtimeDependencies = Seq(localeEv, calendarEv)

  def unparse(state: UState): Unit = {
    doUnparse(state, lengthInBits)
  }
}

case class BinaryCalendarBCDRuntimeLengthUnparser(
  override val e: ElementRuntimeData,
  pattern: String,
  localeEv: CalendarLanguageEv,
  calendarEv: CalendarEv,
  val lengthEv: Evaluatable[JLong],
  val lUnits: LengthUnits)
  extends BinaryCalendarBCDUnparser(e, pattern, localeEv, calendarEv)
  with PrimUnparser
  with HasRuntimeExplicitLength {

  /**
   * Primitive unparsers must override runtimeDependencies
   */
  override lazy val runtimeDependencies = Seq(localeEv, calendarEv, lengthEv)

  def unparse(state: UState): Unit = {
    doUnparse(state, getBitLength(state))
  }

}

case class BinaryCalendarBCDDelimitedLengthUnparser(
  val e: ElementRuntimeData,
  pattern: String,
  localeEv: CalendarLanguageEv,
  calendarEv: CalendarEv)
  extends BinaryCalendarBCDUnparser(e, pattern, localeEv, calendarEv)
  with PrimUnparser {

  /**
   * Primitive unparsers must override runtimeDependencies
   */
  override lazy val runtimeDependencies = Seq(localeEv, calendarEv)

  def unparse(state: UState): Unit = {
    doUnparse(state, 0)
  }

}
