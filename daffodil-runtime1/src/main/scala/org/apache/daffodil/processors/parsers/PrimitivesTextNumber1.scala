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

package org.apache.daffodil.processors.parsers


import com.ibm.icu.math.{ BigDecimal => ICUBigDecimal }
import com.ibm.icu.text.DecimalFormat

import java.text.ParsePosition

import scala.util.matching.Regex

import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.DataValue.DataValueNumber
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.TextNumberFormatEv

case class ConvertTextCombinatorParser(
  rd: TermRuntimeData,
  valueParser: Parser,
  converterParser: Parser)
  extends CombinatorParser(rd) {

  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector(valueParser, converterParser)

  def parse(start: PState): Unit = {
    valueParser.parse1(start)
    if (start.processorStatus ne Success) {
      return
    }
    converterParser.parse1(start)
  }
}

case class ConvertTextNumberParser(
  textNumberFormatEv: TextNumberFormatEv,
  zeroRepsRegex: List[Regex],
  override val context: ElementRuntimeData)
  extends TextPrimParser {

  override lazy val runtimeDependencies = Vector(textNumberFormatEv)

  private val primNumeric = context.optPrimType.get.asInstanceOf[NodeInfo.PrimType.PrimNumeric]

  def parse(start: PState): Unit = {
    val node: DISimple = start.simpleElement
    val str = node.dataValueAsString

    Assert.invariant(str != null) // worst case it should be empty string. But not null.
    if (str == "") {
      PE(start, "Unable to parse %s from empty string", context.optPrimType.get.globalQName)
      return
    }

    // because of the way the zero rep regular expressions are generated, they
    // will match either all or none of 'str', never part of it. Thus,
    // findFirstIn() either matches and it's a zero rep, or it doesn't and it's
    // not a zero
    val numValue = zeroRepsRegex.find { _.findFirstIn(str).isDefined } match {
      case Some(_) => primNumeric.fromNumber(0)
      case None => {
        val df = textNumberFormatEv.evaluate(start).get
        val strCheckPolicy = if (df.isParseStrict) str else str.trim
        val pos = new ParsePosition(0)
        val icuNum = df.parse(strCheckPolicy, pos)

        // sometimes ICU will return their own custom BigDecimal, even if the
        // value could be represented as a BigInteger. We only want Java types,
        // so detect this and convert it to the appropriate type
        val num = icuNum match {
          case bd: ICUBigDecimal => {
            if (bd.scale == 0) bd.unscaledValue
            else bd.toBigDecimal
          }
          case _ => icuNum
        }

        // Verify that what was parsed was what was passed exactly in byte count.
        // Use pos to verify all characters consumed & check for errors!
        if (num == null) {
          PE(start, "Unable to parse %s from text: %s",
            context.optPrimType.get.globalQName, str)
          return
        }
        if (pos.getIndex != strCheckPolicy.length) {
          val isValid =
            if (df.getPadPosition == DecimalFormat.PAD_AFTER_SUFFIX) {
              // If the DecimalFormat pad position is PAD_AFTER_SUFFIX, ICU
              // does not update the parse position to be a the end of the
              // padding, but instead sets the position to the end of the
              // suffix. So we need to manually check to see if all characters
              // after the parse position are the pad character
              val padChar = df.getPadCharacter
              val afterPosition = str.substring(pos.getIndex)
              afterPosition.forall(_ == padChar)
            } else {
              // For all other padPositions, the parse position must be at the
              // end of the string. That's not the case here, so it's not valid
              false
            }
          if (!isValid) {
            PE(start, "Unable to parse %s from text: %s",
              context.optPrimType.get.globalQName, str)
            return
          }
        }

        val numValue: DataValueNumber = try {
          primNumeric.fromNumber(num)
        } catch {
          case e: InvalidPrimitiveDataException => {
            PE(start, "%s", e.getMessage)
            return
          }
        }
        numValue
      }
    }

    node.overwriteDataValue(numValue)

  }
}
