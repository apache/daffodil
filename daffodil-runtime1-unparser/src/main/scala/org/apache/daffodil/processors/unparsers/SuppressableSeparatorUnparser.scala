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
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.ZeroLengthStatus
import org.apache.daffodil.processors.Processor

/**
 * Performance Note: This can be a very special purpose suspension. Unlike the
 * suspensions for dfdl:outputValueCalc, a separator expression can only look backwards
 * to data that is already known. Hence, we can evaluate the separator, and just cache it,
 * and when we decide to unsuspend this, we either blat out that cached data or not.
 * But we need none of the state needed to unparse or evaluate expressions.
 */
final class SuppressableSeparatorUnparserSuspendableOperation(
  sepUnparser: Unparser,
  override val rd: TermRuntimeData)
  extends SuspendableOperation
  with StreamSplitter {

  private var zlStatus_ : ZeroLengthStatus = ZeroLengthStatus.Unknown

  private var maybeDOSAfterSeparatorRegion: Maybe[DataOutputStream] = Maybe.Nope
  private var maybeDOSForStartOfSeparatedRegionBeforePostfixSeparator: Maybe[DataOutputStream] = Maybe.Nope
  private var maybeDOSForEndOfSeparatedRegionBeforePostfixSeparator: Maybe[DataOutputStream] = Maybe.Nope

  def captureStateAtEndOfPotentiallyZeroLengthRegionFollowingTheSeparator(s: UState): Unit = {
    val splitter = RegionSplitUnparser(rd)
    splitter.unparse(s) // splits the DOS so all the potentially ZL stuff is isolated.
    maybeDOSAfterSeparatorRegion = Maybe(splitter.dataOutputStream)
  }

  /**
   * Needed for the corner case of suppressing a postfix separator.
   *
   * Here we need to know whether two regions of data are zero length
   * when dealing with trailingEmpty suppression. The data before the
   * postfix separator must be ZL, and the data to the end of sequence must
   * be ZL. So we have the ability to capture two ranges of buffered data
   * from unparsing.
   */
  def captureDOSForStartOfSeparatedRegionBeforePostfixSeparator(s: UState): Unit = {
    val splitter = RegionSplitUnparser(rd)
    splitter.unparse(s) // splits the DOS so all the potentially ZL stuff is isolated.
    maybeDOSForStartOfSeparatedRegionBeforePostfixSeparator = Maybe(splitter.dataOutputStream.maybeNextInChain.get)
  }

  def captureDOSForEndOfSeparatedRegionBeforePostfixSeparator(s: UState): Unit = {
    val splitter = RegionSplitUnparser(rd)
    splitter.unparse(s) // splits the DOS so all the potentially ZL stuff is isolated.
    maybeDOSForEndOfSeparatedRegionBeforePostfixSeparator = Maybe(splitter.dataOutputStream)
  }

  /**
   * Get list of DOSs that we need to check to see if we know
   * they are nonZL or not.
   */
  private lazy val dosToCheck_ = {
    Assert.usage(maybeDOSAfterSeparatorRegion.isDefined)
    val dosForStartOfSeparatedRegion = savedUstate.dataOutputStream.maybeNextInChain.get
    val dosForEndOfSeparatedRegion = maybeDOSAfterSeparatorRegion.get
    val primaryDOSList = getDOSFromAtoB(
      dosForStartOfSeparatedRegion,
      dosForEndOfSeparatedRegion)
    val secondaryDOSList =
      if (maybeDOSForStartOfSeparatedRegionBeforePostfixSeparator.isDefined) {
        Assert.usage(maybeDOSForEndOfSeparatedRegionBeforePostfixSeparator.isDefined)
        getDOSFromAtoB(
          maybeDOSForStartOfSeparatedRegionBeforePostfixSeparator.get,
          maybeDOSForEndOfSeparatedRegionBeforePostfixSeparator.get)
      } else
        Seq()
    val res = primaryDOSList ++ secondaryDOSList
    res
  }

  /**
   * Determine if the decision about whether to suppress or not can be taken.
   *
   * A trailing separator may be suppressed if the things after it are zero-length
   * to the end of the sequence.
   *
   * An anyEmpty separator may be suppressed if the separated thing is zero-length.
   *
   * This must be determined by examining the buffers of unparsed data. Such time as their
   * length is known to be greater than zero the test will return true. If they are
   * finished and length is still zero the test will also return true. Otherwise they
   * are possibly just temporarily of length zero, so we don't know so we return false
   * and the suspension will be retried later.
   */
  override def test(ustate: UState): Boolean = {
    if (zlStatus_ ne ZeroLengthStatus.Unknown)
      true
    else if (maybeDOSAfterSeparatorRegion.isEmpty)
      false
    else {
      Assert.invariant(maybeDOSAfterSeparatorRegion.isDefined)
      if (dosToCheck_.exists { dos =>
        val dosZLStatus = dos.zeroLengthStatus
        dosZLStatus eq ZeroLengthStatus.NonZero
      }) {
        zlStatus_ = ZeroLengthStatus.NonZero
        true
      } else if (dosToCheck_.forall { dos =>
        val dosZLStatus = dos.zeroLengthStatus
        dosZLStatus eq ZeroLengthStatus.Zero
      }) {
        zlStatus_ = ZeroLengthStatus.Zero
        true
      } else {
        Assert.invariant(zlStatus_ eq ZeroLengthStatus.Unknown)
        false
      }
    }
  }

  /**
   * Once we know whether the length is zero/non-zero, then we decide to unparse the
   * separator or not.
   *
   * If we're positional and potentially trailing, then this will only be Zero length
   * if we're considering unparsing a trailing separator for an empty, with nothing following.
   * So if ZL, no separator, otherwise we unparse the separator.
   */
  override def continuation(state: UState): Unit = {
    import ZeroLengthStatus._
    zlStatus_ match {
      case Zero => {
        // zero length, so we suppress the separator
        //
        // That means we do nothing. After the continuation, the underlying suspension
        // system finalizes the data output streams.
      }
      case NonZero => {
        // non-zero case. So we need the separator.
        sepUnparser.unparse1(savedUstate)
      }
      case Unknown =>
        Assert.invariantFailed("Should be known zero or non-zero by here.")
    }
  }
}

final class SuppressableSeparatorUnparser private (
  sepUnparser: Unparser,
  override val context: TermRuntimeData,
  override val suspendableOperation: SuspendableOperation)
  extends PrimUnparser
  with SuspendableUnparser {

  override val childProcessors: Vector[Processor] = Vector(sepUnparser)

  override def runtimeDependencies = Vector()
}

object SuppressableSeparatorUnparser {

  def apply(
    sepUnparser: Unparser,
    context: TermRuntimeData,
    suspendableOperation: SuspendableOperation) = {
    val res = new SuppressableSeparatorUnparser(sepUnparser, context, suspendableOperation)
    Processor.initialize(res)
    res
  }

}

