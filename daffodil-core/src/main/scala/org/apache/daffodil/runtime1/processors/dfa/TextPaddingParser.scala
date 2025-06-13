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

import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.io.DataInputStream
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.runtime1.processors.DelimiterIterator
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.parsers.PState

class TextPaddingParser(val padChar: Char, override val context: TermRuntimeData)
  extends DFAParser {

  override lazy val name: String = "TextPaddingParser"
  override lazy val info: String = "padChar='" + padChar + "'"

  val paddingDFA = CreatePaddingDFA(padChar, context)

  def parse(
    state: PState,
    input: DataInputStream,
    delimIter: DelimiterIterator
  ): Maybe[ParseResult] = {

    val paddingReg: Registers = state.dfaRegistersPool.getFromPool("TextPaddingParser1")

    paddingReg.reset(state, input, delimIter)

    paddingDFA.run(paddingReg) // Will always succeed.

    val paddingValue = One(paddingReg.resultString.toString)

    state.dfaRegistersPool.returnToPool(paddingReg)
    state.dfaRegistersPool.finalCheck()

    One(new ParseResult(paddingValue, Nope, ArrayBuffer()))
  }

}
