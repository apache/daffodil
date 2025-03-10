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

package org.apache.daffodil.unparsers.runtime1

import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.lib.calendar.DFDLCalendar
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BinaryCalendarRep
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers._

import com.ibm.icu.util.Calendar

case class ConvertBinaryCalendarSecMilliUnparser(
  override val context: ElementRuntimeData,
  binCalRep: BinaryCalendarRep,
  epochTimeMillis: Long,
  lengthInBits: Int,
  hasTZ: Boolean
) extends PrimUnparser {

  /**
   * Primitive unparsers must override runtimeDependencies
   */
  override def runtimeDependencies = Vector()

  protected def putNumber(
    dos: DataOutputStream,
    value: Long,
    nBits: Int,
    finfo: FormatInfo
  ): Boolean = {
    dos.putLong(value, nBits, finfo)
  }

  def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple

    val calValue = node.dataValue.getAnyRef match {
      case dc: DFDLCalendar => dc.calendar
      case x =>
        Assert.invariantFailed(
          "ConvertBinaryCalendar received unsupported type. %s of type %s.".format(
            x,
            Misc.getNameFromClass(x)
          )
        )
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

    val dos = state.getDataOutputStream
    val res = putNumber(dos, diff, lengthInBits, state)

    if (!res) {
      Assert.invariant(dos.maybeRelBitLimit0b.isDefined)
      UnparseError(
        One(state.schemaFileLocation),
        One(state.currentLocation),
        "Insufficient space to unparse element %s, required %s bits, but only %s were available.",
        context.dpathElementCompileInfo.namedQName.toPrettyString,
        lengthInBits,
        dos.maybeRelBitLimit0b.get
      )
    }
  }
}
