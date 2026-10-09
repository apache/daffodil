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

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.util.MStackOfMaybe
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.runtime1.infoset.DIDocument
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.InfosetAccessor
import org.apache.daffodil.runtime1.infoset.InfosetInputter
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData

/**
 * The "build" side of the build and unparse split: it walks the infoset
 * events from an actual `InfosetInputter` and builds the infoset tree ahead
 * of the unparse. It needs only the tree state, so it is not a `UState`:
 * it has no output stream, variables or debugger state, and build never
 * writes content.
 *
 * Used only when the `infosetBuilderMode` tunable is buildAhead when unparsing;
 * otherwise only `UStateMain` is constructed.
 */
final class InfosetBuildState(
  private val inputter: InfosetInputter,
  override val tunable: DaffodilTunables
) extends InfosetTreeState
  with TraversalIndexStacks
  with InfosetFromEvents {

  // Build never frees a node, so finishing one only marks it final. A simple
  // node stays open for the unparse, which gives it its value.
  override def finishElement(cur: DINode, erd: ElementRuntimeData): Unit = {
    if (cur.isComplex) {
      val lastChild = cur.maybeLastChild
      if (lastChild.isDefined && lastChild.get.isArray) {
        lastChild.get.setFinal()
      }
      if (!withinHiddenNest || erd.isRepresented) {
        cur.setFinal()
      }
    }
    markDocumentFinalIfRootEnded()
  }

  override def finishOvcElement(cur: DINode): Unit = markDocumentFinalIfRootEnded()

  private val eventState: InfosetEventState = new InputterEventState(inputter, "building")

  override def advance: Boolean = eventState.advance
  override def advanceAccessor: InfosetAccessor = eventState.advanceAccessor
  override def inspect: Boolean = eventState.inspect
  override def inspectAccessor: InfosetAccessor = eventState.inspectAccessor
  override def fini(): Unit = Assert.usageError("Not to be used on InfosetBuildState")
  override def inspectOrError: InfosetAccessor = eventState.inspectOrError
  override def advanceOrError: InfosetAccessor = eventState.advanceOrError
  override def isInspectArrayEnd: Boolean = eventState.isInspectArrayEnd

  override def pushTRD(trd: TermRuntimeData): Unit = eventState.pushTRD(trd)
  override def maybeTopTRD(): Maybe[TermRuntimeData] = eventState.maybeTopTRD()
  override def popTRD(trd: TermRuntimeData): TermRuntimeData = eventState.popTRD(trd)

  override def documentElement: DIDocument = inputter.documentElement

  override val currentInfosetNodeStack = new MStackOfMaybe[DINode]

  override def currentInfosetNode: DINode = {
    if (currentInfosetNodeMaybe.isEmpty) {
      null
    } else {
      currentInfosetNodeMaybe.get
    }
  }

  override def currentInfosetNodeMaybe: Maybe[DINode] = {
    if (currentInfosetNodeStack.isEmpty) {
      Nope
    } else {
      currentInfosetNodeStack.top
    }
  }

  // Build tracks child position in its own frames, never in a stack.
  override def moveOverOneElementChildOnly(): Unit = ()

  private var hiddenDepth = 0
  override def incrementHiddenDef(): Unit = hiddenDepth += 1
  override def decrementHiddenDef(): Unit = hiddenDepth -= 1
  override def withinHiddenNest: Boolean = hiddenDepth > 0

  // Build runs ahead of the unparse, which frees each node once it is done
  // with it, so build leaves freeing to the unparse.
  override def freeChildIfNoLongerNeeded(parent: DINode, index: Int): Unit = ()

  // The lead is how far build is ahead of the unparse: the nodes build has
  // constructed that the unparse has not yet finished. Build counts a node when
  // it joins the tree, which keeps an ancestor's count ahead of its
  // descendants', and the unparse uncounts it when it finishes the node.
  private var lead: Long = 0

  override def attachElement(newElem: DIElement): Unit = {
    super.attachElement(newElem)
    lead += 1
  }

  def decrementLead(): Unit = {
    lead -= 1
    Assert.invariant(lead >= 0)
  }

  def currentLead: Long = lead

  // Build stops advancing once the lead exceeds the build ahead limit, which
  // bounds how far ahead of the unparse it may run.
  def leadExceedsBuildAheadLimit: Boolean = lead > tunable.unparseBuildAheadWindowNodes
}
