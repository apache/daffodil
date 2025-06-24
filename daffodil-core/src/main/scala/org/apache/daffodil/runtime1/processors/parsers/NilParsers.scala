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

    val field = parseString(start)

    val isFieldEmpty = field.length() == 0

    if (isFieldEmpty && isEmptyAllowed) {
      // Valid! Success ParseResult indicates nilled
    } else if (isFieldEmpty && !isEmptyAllowed) {
      // Fail!
      PE(start, "%s - Empty field found but not allowed!", eName)
    } else if (isFieldNilLit(field)) {
      // Contains a nilValue, Success ParseResult indicates nilled
    } else {
      // Fail!
      PE(start, "%s - Does not contain a nil literal!", eName)
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
