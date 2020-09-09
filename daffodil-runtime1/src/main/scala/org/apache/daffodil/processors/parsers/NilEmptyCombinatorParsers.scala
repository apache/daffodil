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
import org.apache.daffodil.processors.Success

abstract class NilOrValueParser(ctxt: TermRuntimeData, nilParser: Parser, valueParser: Parser)
  extends CombinatorParser(ctxt) {

  override lazy val childProcessors = Vector(nilParser, valueParser)
  override lazy val runtimeDependencies = Vector()

  def parse(pstate: PState): Unit = {

    // This isn't technically a point of uncertainty. Nothing in the nilParser
    // can discriminate this PoU. We are really just using this as a technique
    // to be able to reset back to this point if the nil parser fails.
    pstate.withPointOfUncertainty("NilOrValueParser", ctxt) { pou =>

      nilParser.parse1(pstate)

      if (pstate.processorStatus ne Success) {
        // did not find a nil. reset back to the pou and try to parse the value
        pstate.resetToPointOfUncertainty(pou)
        valueParser.parse1(pstate)
      } else {
        // no-op. We found nil, withPointOfUncertainty will discard the pou
      }
    }

  }
}


case class SimpleNilOrValueParser(ctxt: TermRuntimeData, nilParser: Parser, valueParser: Parser)
  extends NilOrValueParser(ctxt, nilParser, valueParser)

case class ComplexNilOrContentParser(ctxt: TermRuntimeData, emptyParser: Parser, contentParser: Parser)
  extends NilOrValueParser(ctxt, emptyParser, contentParser)
