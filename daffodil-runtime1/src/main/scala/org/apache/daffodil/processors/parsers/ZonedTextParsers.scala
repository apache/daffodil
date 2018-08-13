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

import org.apache.daffodil.schema.annotation.props.gen.TextZonedSignStyle
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.UnsuppressableException
import org.apache.daffodil.util.DecimalUtils
import org.apache.daffodil.util.DecimalUtils.OverpunchLocation
import java.text.ParsePosition
import java.lang.{ Number => JNumber }
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.TermRuntimeData

case class ConvertZonedCombinatorParser(
  rd: TermRuntimeData,
  valueParser: Parser,
  converterParser: Parser)
  extends CombinatorParser(rd) {

  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector(valueParser, converterParser)

  def parse(start: PState): Unit = {
    valueParser.parse1(start)
    if (start.processorStatus eq Success) {
      converterParser.parse1(start)
    }
  }
}

case class ConvertZonedNumberParser[S](
  helper: ConvertTextNumberParserUnparserHelperBase[S],
  opl: OverpunchLocation.Value,
  nff: NumberFormatFactoryBase[S],
  zonedSignStyle: TextZonedSignStyle,
  override val context: ElementRuntimeData) extends TextPrimParser {
  override lazy val runtimeDependencies = Vector()

  override def toString = "to(xs:" + helper.xsdType + ")"

  def parse(start: PState): Unit = {
    val node: DISimple = start.simpleElement
    val str = node.dataValueAsString

    Assert.invariant(str != null) // worst case it should be empty string. But not null.
    if (str == "") {
      PE(start, "Convert to %s (for xs:%s): Cannot parse number from empty string", helper.prettyType, helper.xsdType)
      return
    }

    var checkLength = str.length
    val numValue = {
      val df = nff.getNumFormat(start)
      val pos = new ParsePosition(0)
      val num = try {
        val decodedNum = DecimalUtils.zonedToNumber(str, zonedSignStyle, opl)
        if (decodedNum(0) == '-')
          checkLength = checkLength + 1
        df.get.parse(decodedNum, pos)
      } catch {
        case s: scala.util.control.ControlThrowable => throw s
        case u: UnsuppressableException => throw u
        case e: Exception => {
          PE(start, "Convert to %s (for xs:%s): Parse of '%s' threw exception %s",
            helper.prettyType, helper.xsdType, str, e)
          return
        }
      }

      // Verify that what was parsed was what was passed exactly in byte count.
      // Use pos to verify all characters consumed & check for errors!
      if (num == null || pos.getIndex != checkLength) {
        PE(start, "Convert to %s (for xs:%s): Unable to parse '%s' (using up all characters).",
          helper.prettyType, helper.xsdType, str)
        return
      }

      val numValue = {
        if (helper.isInvalidRange(num)) {
          PE(start, "Convert to %s (for xs:%s): Out of Range: '%s' converted to %s, is not in range for the type.",
            helper.prettyType, helper.xsdType, str, num)
          return
        }

        // convert to proper type
        val asNumber = helper.getNum(num)
        Assert.invariant(!asNumber.isInstanceOf[String])

        asNumber
      }
      numValue
    }

    node.overwriteDataValue(numValue.asInstanceOf[JNumber])

  }
}
