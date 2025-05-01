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

import java.lang.Boolean.{ FALSE => JFalse }
import java.lang.Boolean.{ TRUE => JTrue }

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.processors._
import org.apache.daffodil.runtime1.processors.unparsers._

case class ConvertTextBooleanUnparser(
  erd: ElementRuntimeData,
  textBooleanTrueRepEv: TextBooleanTrueRepEv,
  textBooleanFalseRepEv: TextBooleanFalseRepEv
) extends TextPrimUnparser {

  override def context = erd

  /**
   * Primitive unparsers must override runtimeDependencies
   */
  override def runtimeDependencies = Vector(textBooleanTrueRepEv, textBooleanFalseRepEv)

  def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple

    val boolValue = node.dataValue.getAnyRef match {
      case JTrue => {
        val textBooleanTrueReps: List[String] = textBooleanTrueRepEv.evaluate(state)
        Assert.invariant(textBooleanTrueReps.length >= 1)
        textBooleanTrueReps(0)
      }
      case JFalse => {
        val textBooleanFalseReps: List[String] = textBooleanFalseRepEv.evaluate(state)
        Assert.invariant(textBooleanFalseReps.length >= 1)
        textBooleanFalseReps(0)
      }
    }

    node.overwriteDataValue(boolValue)
  }
}
