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

import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.TextJustificationType

abstract class LiteralNilOfSpecifiedLengthParserBase(erd: ElementRuntimeData)
  extends TextPrimParser
  with StringOfSpecifiedLengthMixin
  with NilMatcherMixin {

  private val eName = erd.name

  override def runtimeDependencies = Vector(erd.encInfo.charsetEv)
  override val context = erd

  override val charsetEv = erd.encInfo.charsetEv

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + eName + " nilValue='" + cookedNilValuesForParse + "'/>"
  }

  def isFieldNilLit(field: String): Boolean

  override def parse(start: PState): Unit = {
    if (erd.isComplexType) {
      // nillable complex types must have a nilValue of %ES;. For a literal nil specified length
      // complex to be nilled, that means either there must be a specified length that is zero
      // or there isn't a specified length and we have reached the end of the data. If neither
      // of these conditions are true, then there is non-empty data for this complex element and
      // it cannot be nilled.
      val bitLimit0b = start.bitLimit0b
      val hasSpecifiedLength = bitLimit0b.isDefined
      if (
        (hasSpecifiedLength && (bitLimit0b.get - start.bitPos0b) > 0) ||
        (!hasSpecifiedLength && start.dataInputStream.hasData)
      ) {
        // Fail!
        PE(start, "%s - Does not contain a nil literal", eName)
      } else {
        // Valid! Success ParseResult indicates nilled
      }
    } else {
      // Simple element, read a string up to the bitLimit and see if it matches the nilValue
      val field = parseString(start)

      val isFieldEmpty = field.length() == 0

      if (isFieldEmpty && isEmptyAllowed) {
        // Valid! Success ParseResult indicates nilled
      } else if (isFieldEmpty && !isEmptyAllowed) {
        // Fail!
        PE(start, "%s - Empty field found but not allowed", eName)
      } else if (isFieldNilLit(field)) {
        // Contains a nilValue, Success ParseResult indicates nilled
      } else {
        // Fail!
        PE(start, "%s - Does not contain a nil literal", eName)
      }
    }
  }

}

/**
 * Specifically designed to be used inside one of the SpecifiedLength parsers.
 *
 * This grabs a string as long as it can get, depending on the SpecifiedLength context
 * to constrain how much it can get.
 */
final class LiteralValueNilOfSpecifiedLengthParser(
  override val cookedNilValuesForParse: List[String],
  override val parsingPadChar: MaybeChar,
  override val justificationTrim: TextJustificationType.Type,
  override val ignoreCase: Boolean,
  erd: ElementRuntimeData
) extends LiteralNilOfSpecifiedLengthParserBase(erd) {

  private val eName = erd.name

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + eName + " nilValue='" + cookedNilValuesForParse + "'/>"
  }

  override def isFieldNilLit(field: String): Boolean = isFieldNilLiteralValue(field)

}

/**
 * Specifically designed to be used inside one of the SpecifiedLength parsers.
 *
 * This grabs a string as long as it can get, depending on the SpecifiedLength context
 * to constrain how much it can get.
 */
final class LiteralCharacterNilOfSpecifiedLengthParser(
  override val cookedNilValuesForParse: List[String],
  override val parsingPadChar: MaybeChar,
  override val justificationTrim: TextJustificationType.Type,
  override val ignoreCase: Boolean,
  erd: ElementRuntimeData
) extends LiteralNilOfSpecifiedLengthParserBase(erd) {

  private val eName = erd.name

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + eName + " nilValue='" + cookedNilValuesForParse + "'/>"
  }

  override def isFieldNilLit(field: String): Boolean = isFieldNilLiteralCharacter(field)

}
