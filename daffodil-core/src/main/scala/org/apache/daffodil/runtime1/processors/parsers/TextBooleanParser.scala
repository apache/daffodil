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

import java.lang.{ Boolean => JBoolean }

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.TextBooleanFalseRepEv
import org.apache.daffodil.runtime1.processors.TextBooleanTrueRepEv

case class ConvertTextBooleanParser(
  override val context: ElementRuntimeData,
  textBooleanTrueRepEv: TextBooleanTrueRepEv,
  textBooleanFalseRepEv: TextBooleanFalseRepEv,
  ignoreCase: Boolean
) extends TextPrimParser {

  override def runtimeDependencies = Vector(textBooleanTrueRepEv, textBooleanFalseRepEv)

  private def matches(str1: String, str2: String): Boolean = {
    if (ignoreCase) str1.equalsIgnoreCase(str2) else str1 == str2
  }

  override def parse(start: PState): Unit = {
    val node = start.simpleElement
    val str = node.dataValueAsString

    Assert.invariant(str != null)

    val textBooleanTrueReps: List[String] = textBooleanTrueRepEv.evaluate(start)
    val textBooleanFalseReps: List[String] = textBooleanFalseRepEv.evaluate(start)

    Assert.invariant(textBooleanTrueReps.length >= 1)
    Assert.invariant(textBooleanFalseReps.length >= 1)

    val newBool: JBoolean =
      if (textBooleanTrueReps.find(matches(_, str)).isDefined) true
      else if (textBooleanFalseReps.find(matches(_, str)).isDefined) false
      else {
        PE(start, "Unable to parse xs:boolean from text: %s", str)
        return
      }

    node.overwriteDataValue(newBool)
  }
}
