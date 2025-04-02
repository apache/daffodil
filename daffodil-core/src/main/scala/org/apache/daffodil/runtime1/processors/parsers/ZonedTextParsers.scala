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

package org.apache.daffodil.runtime1.processors.parsers

import java.text.ParsePosition

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.TextZonedSignStyle
import org.apache.daffodil.lib.util.DecimalUtils
import org.apache.daffodil.lib.util.DecimalUtils.OverpunchLocation
import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Success
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.TextNumberFormatEv

case class ConvertZonedCombinatorParser(
  rd: TermRuntimeData,
  valueParser: Parser,
  converterParser: Parser
) extends CombinatorParser(rd) {

  override def runtimeDependencies = Vector()

  override def childProcessors = Vector(valueParser, converterParser)

  def parse(start: PState): Unit = {
    valueParser.parse1(start)
    if (start.processorStatus eq Success) {
      converterParser.parse1(start)
    }
  }
}

case class ConvertZonedNumberParser(
  opl: OverpunchLocation.Value,
  textNumberFormatEv: TextNumberFormatEv,
  optZonedSignStyle: Option[TextZonedSignStyle],
  override val context: ElementRuntimeData,
  override val textDecimalVirtualPoint: Int
) extends TextPrimParser
  with TextDecimalVirtualPointMixin {

  override def runtimeDependencies = Vector(textNumberFormatEv)

  private val primNumeric = context.optPrimType.get.asInstanceOf[NodeInfo.PrimType.PrimNumeric]

  def parse(start: PState): Unit = {
    val node: DISimple = start.simpleElement
    val str = node.dataValueAsString

    Assert.invariant(str != null) // worst case it should be empty string. But not null.
    if (str == "") {
      PE(
        start,
        "Unable to parse zoned %s from empty string",
        context.optPrimType.get.globalQName
      )
      return
    }

    var checkLength = str.length
    val numValue = {
      val df = textNumberFormatEv.evaluate(start)
      val pos = new ParsePosition(0)

      val decodedNum =
        try {
          DecimalUtils.zonedToNumber(str, optZonedSignStyle, opl)
        } catch {
          case e: NumberFormatException => {
            PE(
              start,
              "Unable to parse zoned %s from text: %s. %s",
              context.optPrimType.get.globalQName,
              str,
              e.getMessage
            )
            return
          }
        }
      if (decodedNum(0) == '-')
        checkLength = checkLength + 1

      val num1 = df.parse(decodedNum, pos)

      // Verify that what was parsed was what was passed exactly in byte count.
      // Use pos to verify all characters consumed & check for errors!
      if (num1 == null || pos.getIndex != checkLength) {
        PE(
          start,
          "Unable to parse zoned %s from text: %s.",
          context.optPrimType.get.globalQName,
          str
        )
        return
      }

      val num2 = applyTextDecimalVirtualPointForParse(num1)

      val numValue =
        try {
          primNumeric.fromNumber(num2)
        } catch {
          case e: InvalidPrimitiveDataException => {
            PE(start, "%s", e.getMessage)
            return
          }
        }
      numValue
    }

    node.overwriteDataValue(numValue)

  }
}
