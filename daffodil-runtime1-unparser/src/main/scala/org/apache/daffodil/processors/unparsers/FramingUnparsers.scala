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

import org.apache.daffodil.processors.SuspendableOperation
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.processors.TextProcessor
import org.apache.daffodil.processors.TermRuntimeData

class SkipRegionUnparser(
  skipInBits: Int,
  override val context: TermRuntimeData)
  extends AlignmentPrimUnparser {

  override def runtimeDependencies = Vector()

  override def unparse(state: UState) = {
    val dos = state.dataOutputStream
    if (!dos.skip(skipInBits, state)) UE(state, "Unable to skip %s(bits).", skipInBits)
  }
}

class AlignmentFillUnparserSuspendableOperation(
  alignmentInBits: Int,
  override val rd: TermRuntimeData)
  extends SuspendableOperation {

  override def test(ustate: UState) = {
    val dos = ustate.dataOutputStream
    if (dos.maybeAbsBitPos0b.isEmpty) {
      log(LogLevel.Debug, "%s %s Unable to align to %s bits because there is no absolute bit position.", this, ustate, alignmentInBits)
    }
    dos.maybeAbsBitPos0b.isDefined
  }

  override def continuation(state: UState): Unit = {
    val dos = state.dataOutputStream
    val b4 = dos.relBitPos0b
    if (!dos.align(alignmentInBits, state))
      UE(state, "Unable to align to %s(bits).", alignmentInBits)
    val aft = dos.relBitPos0b
    val delta = aft - b4
    if (delta == 0)
      log(LogLevel.Debug, "%s did nothing.", this)
    else
      log(LogLevel.Debug, "%s moved %s bits to align to %s(bits).", this, delta, alignmentInBits)
  }
}

class AlignmentFillUnparser(
  alignmentInBits: Int,
  override val context: TermRuntimeData)
  extends AlignmentPrimUnparser
  with SuspendableUnparser {

  override def runtimeDependencies = Vector()

  override def suspendableOperation =
    new AlignmentFillUnparserSuspendableOperation(
      alignmentInBits, context)
}

class MandatoryTextAlignmentUnparser(
  alignmentInBits: Int,
  e: TermRuntimeData)
  extends AlignmentFillUnparser(alignmentInBits, e)
  with TextProcessor
