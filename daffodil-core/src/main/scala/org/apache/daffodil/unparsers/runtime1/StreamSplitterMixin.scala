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

import scala.collection.mutable.Buffer

import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.ZeroLengthStatus
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.processors.Processor
import org.apache.daffodil.runtime1.processors.RuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers.*

trait StreamSplitter {

  /**
   * Given two DataOutputStream, determine the set of data output streams starting from the first,
   * and in chain until (and including) we reach the second.
   *
   * If the two are the same DataOutputStream, we get back a sequence of just the one DOS.
   */
  def getDOSFromAtoB(
    beforeDOS: DataOutputStream,
    afterDOS: DataOutputStream
  ): Seq[DataOutputStream] = {
    val buf = Buffer[DataOutputStream]()
    var maybeNext = Maybe(beforeDOS)
    while (maybeNext.isDefined && (maybeNext.get ne afterDOS)) {
      val thisOne = maybeNext.get
      buf += thisOne
      maybeNext = thisOne.maybeNextInChain
    }
    Assert.invariant(maybeNext.isDefined) // we MUST find a path to the afterDOS.
    Assert.invariant(maybeNext.get eq afterDOS)
    // we include the final afterDOS data output stream in the list
    buf += afterDOS
    val res = buf.toSeq
    res
  }
}

/**
 * Tracks whether a region of unparsed data, isolated by splitting the data
 * output stream, is zero length or not.
 */
trait ZeroLengthRegionMixin extends StreamSplitter {

  private var zlStatus_ : ZeroLengthStatus = ZeroLengthStatus.Unknown

  final def zlStatus: ZeroLengthStatus = zlStatus_

  /**
   * Splits the data output stream so the data unparsed after this point is
   * isolated, and returns the data output stream that ends the region before
   * the split.
   */
  protected final def splitRegion(
    splitter: RegionSplitUnparser,
    state: UState
  ): DataOutputStream = {
    val op = splitter.suspendableOperation
    splitter.unparseWithOperation(state, op)
    op.savedUstate.getDataOutputStream
  }

  /**
   * Updates the status from the streams of the region. Any non-zero stream
   * makes the region non-zero, and it is zero only when all streams are known
   * to be zero. Otherwise it remains unknown until the streams are further
   * along. The streams are only computed while the status is unknown.
   */
  protected final def updateZLStatus(dosToCheck: => Seq[DataOutputStream]): ZeroLengthStatus = {
    if (zlStatus_ eq ZeroLengthStatus.Unknown) {
      val doss = dosToCheck
      if (doss.exists { _.zeroLengthStatus eq ZeroLengthStatus.NonZero }) {
        zlStatus_ = ZeroLengthStatus.NonZero
      } else if (doss.forall { _.zeroLengthStatus eq ZeroLengthStatus.Zero }) {
        zlStatus_ = ZeroLengthStatus.Zero
      }
    }
    zlStatus_
  }
}

/**
 * We need to isolate the regions that we have to test for ZL/notZL
 * for separator suppression. So we want these regions to not share
 * data output streams with anything outside them.
 *
 * To achieve that we split the data output stream using its buffering
 * capabilities. Some of the splits are natural. They are one side of
 * the (potentially suppressed) separator itself. So creating that
 * suspendable unparser for the separator gives us a split.
 *
 * The other side of these regions is however, an artificial split.
 *
 * To avoid duplication of complex code paths, we create this
 * suspendable unparser that exists purely to split the underlying DOS
 * but maintain all the invariants of the underlying DOS code, i.e.,
 * the streams still get finalized (and therefore collapsed) the regular
 * way.
 *
 * We are counting on the fact that these objects live on the garbage
 * collected heap, so have unbounded lifetimes, i.e., they aren't recycled
 * in a pool or anything.
 *
 * Performance Note: These do not need to capture state like a normal
 * suspension does (e.g., for dfdl:outputValueCalc), as they're not actually
 * suspending any actual unparsing behavior. Just forcing a boundary in the
 * data output streams so that we can use the data output streams to measure the
 * length (zero/non-zero) of unparsed data.
 */
class RegionSplitUnparser(override val context: TermRuntimeData)
  extends PrimUnparser
  with StatefulDelegatedSuspendableUnparser[RegionSplitState] {

  override def childProcessors: Vector[Processor] = Vector()

  override val runtimeDependencies = Array()

  override def rd: RuntimeData = context

  override def newSuspensionState(): RegionSplitState = new RegionSplitState

  /**
   * Suspends once, since test fails the first time.
   * When retried, the test succeeds.
   */
  override def suspensionTest(ustate: UState, state: RegionSplitState): Boolean = {
    if (state.secondTime) true
    else {
      state.secondTime = true
      false
    }
  }

  override def suspensionContinuation(ustate: UState, state: RegionSplitState): Unit = {
    // do nothing.
    //
    // The underlying suspension system will take care of
    // finishing the DOS so everything gets unblocked.
  }
}

final class RegionSplitState {
  var secondTime = false
}

object RegionSplitUnparser {
  def apply(trd: TermRuntimeData) = {
    val unp = new RegionSplitUnparser(trd)
    Processor.initialize(unp)
    unp
  }
}
