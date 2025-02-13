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

package org.apache.daffodil.runtime1.processors.dfa

import org.apache.daffodil.runtime1.processors.Delimiter
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.parsers.DelimiterTextType

object CreatePaddingDFA {

  /**
   * Constructs a DFADelimiter object that specifically
   * looks for padChar.
   */
  def apply(padChar: Char, rd: TermRuntimeData): DFADelimiter = {
    // TODO: In the future we will need to change this because the padChar isn't necessarily a char.
    // One can use it to specify a numeric byte to be used to pad as well.

    val allStates: Array[State] = new Array[State](1)

    val startState = new StartStatePadding(allStates, padChar)

    allStates(0) = startState

    new DFADelimiterImpl(
      DelimiterTextType.Other,
      allStates,
      padChar.toString(),
      rd.schemaFileLocation
    )
  }

  /**
   * Constructs a DFADelimiter object that specifically
   * looks for padChar.
   */
  def apply(padChar: Char, outputNewLine: String, rd: TermRuntimeData): DFADelimiter = {
    // TODO: In the future we will need to change this because the padChar isn't necessarily a char.
    // One can use it to specify a numeric byte to be used to pad as well.

    val allStates: Array[State] = new Array[State](1)

    val startState = new StartStatePadding(allStates, padChar)

    allStates(0) = startState

    val d = new Delimiter()
    d.compileDelimiter(padChar.toString, false)

    val unparseValue = d.delimBuf.map { _.unparseValue("") }.mkString

    new DFADelimiterImplUnparse(
      DelimiterTextType.Other,
      allStates,
      padChar.toString(),
      unparseValue,
      rd.schemaFileLocation
    )
  }
}
