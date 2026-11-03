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

import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.processors.RuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.TextProcessor
import org.apache.daffodil.runtime1.processors.unparsers.*

class SkipRegionUnparser(skipInBits: Int, override val context: TermRuntimeData)
  extends AlignmentPrimUnparser {

  override val runtimeDependencies = Array()

  override def unparse(state: UState) = {
    val dos = state.getDataOutputStream
    if (!dos.skip(skipInBits, state)) UE(state, "Unable to skip %s(bits).", skipInBits)
  }
}

trait AlignmentFillMixin {

  def alignmentInBits: Int

  def UE(ustate: UState, s: String, args: Any*): Nothing

  def alignmentTest(ustate: UState): Boolean = {
    val dos = ustate.getDataOutputStream
    if (dos.maybeAbsBitPos0b.isEmpty) {
      Logger.log.debug(
        s"${this} ${ustate} Unable to align to ${alignmentInBits} bits because there is no absolute bit position."
      )
    }
    dos.maybeAbsBitPos0b.isDefined
  }

  def alignmentContinuation(state: UState): Unit = {
    val dos = state.getDataOutputStream
    val b4 = dos.relBitPos0b
    if (!dos.align(alignmentInBits, state))
      UE(state, "Unable to align to %s(bits).", alignmentInBits)
    val aft = dos.relBitPos0b
    val delta = aft - b4
    if (delta == 0)
      Logger.log.debug(s"${this} did nothing.")
    else
      Logger.log.debug(s"${this} moved ${delta} bits to align to ${alignmentInBits}(bits).")
  }
}

class AlignmentFillUnparser(
  override val alignmentInBits: Int,
  override val context: TermRuntimeData
) extends AlignmentPrimUnparser
  with DelegatedSuspendableUnparser
  with AlignmentFillMixin {

  override val runtimeDependencies = Array()

  override def rd: RuntimeData = context

  override def suspensionTest(ustate: UState): Boolean = alignmentTest(ustate)

  override def suspensionContinuation(ustate: UState): Unit = alignmentContinuation(ustate)
}

class MandatoryTextAlignmentUnparser(alignmentInBits: Int, e: TermRuntimeData)
  extends AlignmentFillUnparser(alignmentInBits, e)
  with TextProcessor
