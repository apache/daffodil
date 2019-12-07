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

package org.apache.daffodil.processors

import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.dfa.DFAField
import org.apache.daffodil.processors.dfa.CreateFieldDFA
import org.apache.daffodil.dsom.DPathCompileInfo

class FieldDFAParseEv(val escapeSchemeEv: Maybe[EscapeSchemeParseEv], rd: DPathCompileInfo)
  extends Evaluatable[DFAField](rd)
  with InfosetCachedEvaluatable[DFAField] {

  override lazy val runtimeDependencies = escapeSchemeEv.toList

  def compute(state: ParseOrUnparseState) = {

    val fieldDFA =
      if (escapeSchemeEv.isDefined) {
        val es = escapeSchemeEv.get.evaluate(state)
        es match {
          case s: EscapeSchemeBlockParserHelper => CreateFieldDFA()
          case s: EscapeSchemeCharParserHelper => CreateFieldDFA(s.ec, s.eec)
        }
      } else {
        CreateFieldDFA()
      }
    fieldDFA
  }
}
