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

import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.TermRuntimeData

trait Separated { self: SequenceChildUnparser =>

  def sep: Unparser
  def spos: SeparatorPosition
  def ssp: SeparatorSuppressionPolicy

  val childProcessors = Seq(childUnparser, sep)
}

class ScalarOrderedRequiredSeparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData,
  val sep: Unparser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy)
  extends SequenceChildUnparser(childUnparser, srd, trd)
  with Separated {

  override def unparse(state: UState) = childUnparser.unparse1(state)
}

class RepOrderedExactlyNSeparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  val erd: ElementRuntimeData,
  repeatCount: Long,
  val sep: Unparser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy,
  val baseName: String = "ExactlyN")
  extends SequenceChildUnparser(childUnparser, srd, erd)
  with Separated
  with RepUnparser {
  val minRepeats = 0L
  val maxRepeats = repeatCount
}

class RepOrderedExactlyTotalOccursCountSeparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  sep: Unparser,
  spos: SeparatorPosition,
  ssp: SeparatorSuppressionPolicy)
  extends RepOrderedExactlyNSeparatedSequenceChildUnparser(childUnparser,
    srd, erd,
    { val ignored = 0; ignored },
    sep, spos, ssp,
    "ExactlyTotalOccursCount") {
  // nothing
}

/**
 * Pass nothing, or -1 for min/max to mean use erd.minOccurs/maxOccurs
 * Pass -2 for max to force unbounded behavior.
 */
class RepOrderedWithMinMaxSeparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  val erd: ElementRuntimeData,
  val baseName: String,
  val sep: Unparser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy,
  min: Long = -1,
  max: Long = -1) // pass -2 to force unbounded behavior
  extends SequenceChildUnparser(childUnparser, srd, erd)
  with Separated
  with RepUnparser {
  val minRepeats = if (min == -1) erd.minOccurs else min
  val maxRepeats = max match {
    case -1 if (erd.maxOccurs == -1) => Long.MaxValue
    case -1 => erd.maxOccurs
    case -2 => Long.MaxValue
    case _ => max
  }
}

class OrderedSeparatedSequenceUnparser(rd: SequenceRuntimeData,
  ssp: SeparatorSuppressionPolicy,
  spos: SeparatorPosition,
  sep: Unparser,
  childrenArg: Seq[SequenceChildUnparser])
  extends OrderedSequenceUnparserBase(rd, childrenArg) {

  import SequenceChildUnparser._

  /**
   * Unparses one iteration of an array/optional element
   */
  protected def unparseOne(
    parserArg: SequenceChildUnparser,
    trd: TermRuntimeData,
    state: UState): Unit = {

    val unparser = parserArg.asInstanceOf[SeparatedChildUnparser]

    if ((spos eq SeparatorPosition.Prefix) && trd.isRepresented) {
      sep.unparse1(state)
    }

    if ((spos eq SeparatorPosition.Infix) && state.childPos > 1 && trd.isRepresented) {
      sep.unparse1(state)
    }

    unparser.unparse1(state)

    if ((spos eq SeparatorPosition.Postfix) && trd.isRepresented) {
      sep.unparse1(state)
    }
  }

}
