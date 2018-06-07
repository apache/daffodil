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
package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.processors.{ SequenceRuntimeData, TermRuntimeData }
import org.apache.daffodil.processors.ElementRuntimeData

trait Unseparated { self: SequenceChildUnparser =>

  val childProcessors = Seq(childUnparser)
}

class ScalarOrderedRequiredUnseparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData)
  extends SequenceChildUnparser(childUnparser, srd, trd) with Unseparated {

  override protected def unparse(state: UState) = childUnparser.unparse1(state)
}

abstract class RepUnseparatedUnparser(
  childUnparser: Unparser,
  val minRepeats: Long,
  val maxRepeats: Long,
  srd: SequenceRuntimeData,
  val erd: ElementRuntimeData,
  val baseName: String)
  extends SequenceChildUnparser(childUnparser, srd, erd)
  with Unseparated with RepUnparser {
  // nothing
}

class RepOrderedExactlyNUnseparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  repeatCount: Long,
  baseName: String = "ExactlyN")
  extends RepUnseparatedUnparser(childUnparser, 0, repeatCount, srd, erd, baseName)
  with RepUnparser {
  // nothing
}

class RepOrderedExactlyTotalOccursCountUnseparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData)
  extends RepOrderedExactlyNUnseparatedSequenceChildUnparser(childUnparser,
    srd, erd,
    { val ignored = 0; ignored },
    "ExactlyTotalOccursCount") {
  // nothing
}

class RepOrderedWithMinMaxUnseparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  baseName: String,
  min: Long = -1,
  max: Long = -1) // pass -2 to force unbounded behavior
  extends RepUnseparatedUnparser(
    childUnparser,
    (if (min == -1) erd.minOccurs else min),
    max match {
      case -1 if (erd.maxOccurs == -1) => Long.MaxValue
      case -1 => erd.maxOccurs
      case -2 => Long.MaxValue
      case _ => max
    },
    srd, erd, baseName) {
  // nothing
}

class OrderedUnseparatedSequenceUnparser(rd: SequenceRuntimeData, childUnparsers: Seq[SequenceChildUnparser])
  extends OrderedSequenceUnparserBase(rd, childUnparsers) {

  /**
   * Unparses one iteration of an array/optional element
   */
  protected def unparseOne(
    unparser: SequenceChildUnparser,
    trd: TermRuntimeData,
    state: UState): Unit = {

    unparser.unparse1(state)
  }

}
