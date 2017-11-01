/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil.processors

import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.processors.unparsers.UStateMain
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.util.Logging
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeInt
import passera.unsigned.ULong
import org.apache.daffodil.processors.unparsers.UnparseError
import org.apache.daffodil.io.BitOrderChangeException

/**
 * The suspension object keeps track of the state of the task, i.e., whether it
 * is done, whether it is making forward progress when run or not.
 *
 * A suspension" may block, by which we mean it may set isDone to false, and return.
 *
 * Running the suspension again tries again and will either block or complete.
 *
 */
trait Suspension
  extends Serializable with Logging {

  val isReadOnly = false

  def UE(ustate: UState, s: String, args: Any*) = {
    UnparseError(One(rd.schemaFileLocation), One(ustate.currentLocation), s, args: _*)
  }

  private var savedUstate_ : UState = null

  final def savedUstate = {
    Assert.invariant(savedUstate_ ne null)
    // Assert above fails if no suspension state created yet. Can't ask for ustate.
    // means we are pre-evaluating to decide if we need to suspend.
    savedUstate_
  }

  def rd: RuntimeData

  protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = MaybeULong.Nope

  protected def doTask(ustate: UState): Unit

  /**
   * After calling this, call isDone and if that's false call isMakingProgress to
   * understand whether it is done, blocked on the exactly same situation, or blocked elsewhere.
   *
   * This status is needed to implement circular deadlock detection
   */
  final def runSuspension() {
    doTask(savedUstate)
    if (isDone && !isReadOnly) {
      try {
        savedUstate.dataOutputStream.setFinished()
      } catch {
        case boc: BitOrderChangeException =>
          savedUstate.SDE(boc)
      }
      log(LogLevel.Debug, "%s finished %s.", this, savedUstate)
    }
  }

  final def run(ustate: UState) {
    doTask(ustate)
    if (!isDone) {
      val mkl = maybeKnownLengthInBits(ustate)
      setup(ustate, mkl)
    }
  }

  final def explain() {
    val t = this
    Assert.invariant(t.isBlocked)
    log(LogLevel.Warning, "%s", t.blockedLocation)
  }

  private var priorNodeOrVar: Maybe[AnyRef] = Nope
  private var priorInfo: Maybe[AnyRef] = Nope
  private var priorIndex: MaybeInt = MaybeInt.Nope
  private var priorExc: Maybe[AnyRef] = Nope

  private var maybeNodeOrVar: Maybe[AnyRef] = Nope
  private var maybeInfo: Maybe[AnyRef] = Nope
  private var maybeIndex: MaybeInt = MaybeInt.Nope
  private var maybeExc: Maybe[AnyRef] = Nope

  private var done_ : Boolean = false
  private var isBlocked_ = false

  final def setDone {
    done_ = true
  }

  final def isDone = done_

  final def isBlocked = isBlocked_

  final def setUnblocked() {
    isBlocked_ = false
  }

  /**
   * False if the expression blocked at the same spot, i.e.,
   * didn't make any forward progress.
   */
  private var isMakingProgress_ : Boolean = true

  final def isMakingProgress = isMakingProgress_

  final def block(nodeOrVar: AnyRef, info: AnyRef, index: Long, exc: AnyRef) {
    log(LogLevel.Debug, "blocking %s due to %s", this, exc)

    Assert.usage(nodeOrVar ne null)
    Assert.usage(info ne null)
    Assert.usage(exc ne null)
    priorNodeOrVar = maybeNodeOrVar
    priorInfo = maybeInfo
    priorIndex = maybeIndex
    priorExc = maybeExc
    maybeNodeOrVar = One(nodeOrVar)
    maybeInfo = One(info)
    maybeIndex = MaybeInt(index.toInt)
    maybeExc = One(exc)
    done_ = false
    isBlocked_ = true

    if (isBlockedSameLocation) {
      isMakingProgress_ = false
    } else if (isBlockedFirstTime) {
      isMakingProgress_ = true
    } else {
      isMakingProgress_ = true
    }
  }

  final def blockedLocation = "BLOCKED\nexc=%s\nnode=%s\ninfo=%s\nindex=%s".format(maybeExc, maybeNodeOrVar, maybeInfo, maybeIndex)

  private def isBlockedFirstTime: Boolean = {
    isBlocked &&
      priorNodeOrVar.isEmpty
  }

  private def isBlockedSameLocation: Boolean = {
    val res = isBlocked &&
      {
        if (priorNodeOrVar.isEmpty) false
        else {
          Assert.invariant(maybeNodeOrVar.isDefined)
          val res =
            maybeNodeOrVar.get == priorNodeOrVar.get &&
              maybeInfo.get == priorInfo.get &&
              maybeIndex.get == priorIndex.get &&
              maybeExc.get == priorExc.get
          res
        }
      }
    res
  }

  final protected def setup(ustate: UState, maybeKnownLengthInBits: MaybeULong) {
    Assert.usage(ustate.currentInfosetNodeMaybe.isDefined)

    val original = ustate.dataOutputStream.asInstanceOf[DirectOrBufferedDataOutputStream]

    //    // if we know the length will be 0, no need to allocate a buffered DOS
    //    // but if we don't know the length or we know the length is non-zero
    //    // then we do need a buffered DOS
    //    //
    //    if (maybeKnownLengthInBits.isEmpty ||
    //      (maybeKnownLengthInBits.get != 0)) {

    val buffered = original.addBuffered

    if (maybeKnownLengthInBits.isDefined) {
      // since we know the length of the unparsed representation that we're skipping for now,
      // that means we know the absolute position of the bits in the buffer we're creating
      // and that means alignment operations don't have to suspend waiting for this knowledge
      if (original.maybeAbsBitPos0b.isDefined) {
        // direct streams always know this, but buffered streams may not.

        val originalAbsBitPos0b = original.maybeAbsBitPos0b.getULong

        // we are passed this length (in bits)
        // and can use it to initialize the absolute bit pos of the buffered output stream.
        //
        // This allows us to deal with alignment regions, that is, we can determine
        // their size since we know the absolute bit position.

        val mkl = maybeKnownLengthInBits.getULong
        Assert.invariant(mkl >= ULong(0))
        buffered.setAbsStartingBitPos0b(originalAbsBitPos0b + mkl)

      }
    } else {
      // log(LogLevel.Debug,
      log(LogLevel.Debug, "Buffered DOS created for %s without knowning absolute start bit pos: %s\n",
        ustate.currentInfosetNode.erd.diagnosticDebugName, buffered)
    }

    // the main-thread will carry on using the original ustate but unparsing
    // into this buffered stream.
    ustate.dataOutputStream = buffered

    // }

    //
    // clone the ustate for use when evaluating the expression
    //
    // TODO: Performance - copying this whole state, just for OVC is painful.
    // Some sort of copy-on-write scheme would be better.
    //
    val cloneUState = ustate.asInstanceOf[UStateMain].cloneForSuspension(original)
    if (isReadOnly) {
      try {
        original.setFinished()
      } catch {
        case boc: BitOrderChangeException => ustate.SDE(boc)
      }
    }

    savedUstate_ = cloneUState

    ustate.asInstanceOf[UStateMain].addSuspension(this)
  }
}

