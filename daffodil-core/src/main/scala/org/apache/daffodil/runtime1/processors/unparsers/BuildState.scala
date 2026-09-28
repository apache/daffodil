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

package org.apache.daffodil.runtime1.processors.unparsers

import java.io.ByteArrayOutputStream

import org.apache.daffodil.api
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.io.StringDataInputStreamForUnparse
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.LocalStack
import org.apache.daffodil.lib.util.MStackOfMaybe
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.runtime1.dpath.UnparserBlocking
import org.apache.daffodil.runtime1.infoset.DIDocument
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.InfosetAccessor
import org.apache.daffodil.runtime1.infoset.InfosetInputter
import org.apache.daffodil.runtime1.processors.DelimiterStackUnparseNode
import org.apache.daffodil.runtime1.processors.EscapeSchemeUnparserHelper
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.SuspensionTracker
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.VariableBox
import org.apache.daffodil.runtime1.processors.VariableMap
import org.apache.daffodil.runtime1.processors.dfa.DFADelimiter

/**
 * A `UState` subclass that consumes an actual `InfosetInputter` and provides
 * live Cursor/TRD/index-stack behavior; this is the "build" side of the
 * build/write unparse split. The write-only surface (delimiter stack,
 * escape scheme cache, and the scratch buffers used for measuring/escaping
 * text) is stubbed to error, since nothing build does should ever touch it;
 * build never writes content.
 *
 * `getDataOutputStream` is NOT stubbed: generic `UState` utility methods
 * (toString, currentLocation, bitPos0b) call into it unconditionally, so
 * `BuildState` constructs an actual DOS wrapping a no-op sink purely to
 * satisfy that.
 *
 * Used only when the `useBuildWritePrefetch` tunable is enabled (default
 * false); otherwise unused, and unparsing constructs `UStateMain`
 * exclusively as before.
 */
final class BuildState(
  private val inputter: InfosetInputter,
  sharedCtx: UnparseSharedContext,
  diagnosticsArg: Seq[api.Diagnostic],
  areDebugging: Boolean
) extends UState(
    // Build never reads or writes a variable, so an empty map is enough;
    // it's just here to satisfy UState's constructor.
    new VariableBox(VariableMap()),
    diagnosticsArg,
    Maybe(sharedCtx.dataProc),
    sharedCtx.tunable,
    areDebugging
  )
  with SuspensionCapableUState
  with TraversalIndexStacks {

  dState.setMode(UnparserBlocking)
  setSharedContext(sharedCtx)

  // Purely so generic UState utility methods (toString, currentLocation,
  // bitPos0b) have something non-null to call into; never actually
  // written to for real output.
  setDataOutputStream(
    DirectOrBufferedDataOutputStream(
      new java.io.OutputStream { override def write(b: Int): Unit = () },
      null,
      false,
      sharedCtx.tunable.outputStreamChunkSizeInBytes,
      sharedCtx.tunable.maxByteArrayOutputStreamBufferSizeInBytes,
      sharedCtx.tunable.tempFilePath
    )
  )

  // Build runs ahead of write, so freeing a node here would null out a
  // child reference write hasn't read yet; write still frees as normal.
  override def releaseUnneededInfoset: Boolean = false

  private def writeOnly =
    Assert.usageError("BuildState never writes content, so this write-only state doesn't exist")

  override def escapeSchemeEVCache: MStackOfMaybe[EscapeSchemeUnparserHelper] = writeOnly
  override def withUnparserDataInputStream: LocalStack[StringDataInputStreamForUnparse] =
    writeOnly
  override def withByteArrayOutputStream
    : LocalStack[(ByteArrayOutputStream, DirectOrBufferedDataOutputStream)] = writeOnly
  override def allTerminatingMarkup: List[DFADelimiter] = writeOnly
  override def localDelimiters: DelimiterStackUnparseNode = writeOnly
  override def pushDelimiters(node: DelimiterStackUnparseNode): Unit = writeOnly
  override def popDelimiters(): Unit = writeOnly

  override def advance: Boolean = inputter.advance
  override def advanceAccessor: InfosetAccessor = inputter.advanceAccessor
  override def inspect: Boolean = inputter.inspect
  override def inspectAccessor: InfosetAccessor = inputter.inspectAccessor
  override def fini(): Unit = Assert.usageError("Not to be used on UState")

  override def inspectOrError: InfosetAccessor = {
    if (inspect) {
      inspectAccessor
    } else {
      Assert.invariantFailed(
        "An InfosetEvent was required for building, but no InfosetEvent was available."
      )
    }
  }

  override def advanceOrError: InfosetAccessor = {
    if (advance) {
      advanceAccessor
    } else {
      Assert.invariantFailed(
        "An InfosetEvent was required for building, but no InfosetEvent was available."
      )
    }
  }

  override def isInspectArrayEnd: Boolean = {
    if (!inspect) {
      false
    } else {
      inspectAccessor match {
        case e if e.isEnd && e.isArray => true
        case _ => false
      }
    }
  }

  def currentInfosetNode: DINode = {
    if (currentInfosetNodeMaybe.isEmpty) {
      null
    } else {
      currentInfosetNodeMaybe.get
    }
  }

  def currentInfosetNodeMaybe: Maybe[DINode] = {
    if (currentInfosetNodeStack.isEmpty) {
      Nope
    } else {
      currentInfosetNodeStack.top
    }
  }

  override val currentInfosetNodeStack = new MStackOfMaybe[DINode]

  // Shared, not owned; one SuspensionTracker queue, both build and write
  // see the same one via sharedCtx.
  def suspensionTracker: SuspensionTracker = sharedCtx.suspensionTracker

  // Build never evaluates an expression or writes a suspendable child, so
  // nothing build does can ever suspend, and this is never called: only
  // Suspension.suspend calls it, and that always calls cloneForSuspension
  // first, which already throws.
  def addSuspension(se: Suspension): Unit = writeOnly

  /**
   * Uses evalBuildResolvableSuspensions: canResolveWithoutWriting is a
   * static, direction-blind heuristic, so a suspension it marks false
   * (usually a forward reference, occasionally an already-resolved
   * backward one) is skipped here rather than genuinely retried, and
   * left pending for a later, unfiltered sweep instead.
   */
  def evalSuspensions(isFinal: Boolean): Unit = {
    sharedCtx.suspensionTracker.evalBuildResolvableSuspensions()
    if (isFinal) sharedCtx.suspensionTracker.requireFinal()
  }
  def suspensions = sharedCtx.suspensionTracker.suspensions

  /**
   * Build never evaluates an expression or writes a suspendable child, so
   * nothing build does can ever suspend, and this is never called.
   */
  override def cloneForSuspension(suspendedDOS: DirectOrBufferedDataOutputStream): UState =
    writeOnly

  final override def pushTRD(trd: TermRuntimeData): Unit = inputter.pushTRD(trd)
  final override def maybeTopTRD(): Maybe[TermRuntimeData] = inputter.maybeTopTRD()
  final override def popTRD(trd: TermRuntimeData): TermRuntimeData = {
    val poppedTRD = inputter.popTRD()
    if (poppedTRD ne trd)
      Assert.invariantFailed("TRDs do not match. Expected: " + trd + " got " + poppedTRD)
    poppedTRD
  }

  final override def documentElement: DIDocument = inputter.documentElement
}
