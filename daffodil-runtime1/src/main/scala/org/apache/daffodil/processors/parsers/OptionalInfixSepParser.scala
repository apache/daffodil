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

import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.exceptions.Assert

class OptionalInfixSepParser(ctxt: TermRuntimeData, sepParser: Parser)
  extends CombinatorParser(ctxt) {

  Assert.invariant(!sepParser.isEmpty)

  override lazy val nom = "OptionalInfixSep"
  override lazy val childProcessors = Seq(sepParser)
  override lazy val runtimeDependencies = Nil

  override def parse(start: PState): Unit = {
    if (start.mpstate.arrayPos > 1) sepParser.parse1(start)
    else if (start.mpstate.groupPos > 1) sepParser.parse1(start)
    else {
      //ok
    }
  }

}
