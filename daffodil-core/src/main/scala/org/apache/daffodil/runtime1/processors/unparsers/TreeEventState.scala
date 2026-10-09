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

import scala.annotation.tailrec

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.MStackOfInt
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.infoset.DIArray
import org.apache.daffodil.runtime1.infoset.DIDocument
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.Info
import org.apache.daffodil.runtime1.infoset.InfosetAccessor
import org.apache.daffodil.runtime1.infoset.InfosetBuildCursor
import org.apache.daffodil.runtime1.infoset.InfosetEventKind
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData

/**
 * The infoset events of the tree that build is constructing, so the
 * unparsers consume them exactly as they consume the events of an
 * InfosetInputter. A node build has not produced yet is waited for by
 * advancing the build cursor. Hidden nodes get no events, as
 * in the event stream the tree was built from.
 *
 * The walk is an explicit stack of the containers being visited, so it can
 * stop after any event and resume later.
 */
final class TreeEventState(
  cursor: InfosetBuildCursor,
  releaseUnneededInfoset: Boolean
) extends InfosetEventState {

  // The walk's stack of containers being visited, innermost last. It is held
  // in parallel arrays, so visiting a node allocates nothing.
  private var containerStack = new Array[DINode](16)

  // For the container at the same position of the stack, the index of the next
  // of its children to visit.
  private var nextChildIndexes = new Array[Int](16)

  // For the container at the same position of the stack, what kind of node it
  // is, found once when the container was first reached.
  private var containerKinds = new Array[NodeKind](16)

  // How many containers are on the stack.
  private var stackDepth = 0

  // The simple node whose start event was emitted and whose end event is next.
  // A simple node has no children to visit, so it never goes on the stack.
  private var pendingEnd: DINode = null

  // The document may not exist yet when this is constructed, so the walk
  // starts at the first event asked for.
  private var started = false

  // What a node is, found once when it is first reached, so the walk does not
  // test its type again for each of its events.
  private def kindOf(node: DINode): NodeKind = {
    node match {
      case _: DISimple => NodeKind.Simple
      case _: DIArray => NodeKind.Array
      case _: DIDocument => NodeKind.Document
      case _ => NodeKind.Complex
    }
  }

  private def pushContainer(node: DINode, kind: NodeKind): Unit = {
    if (stackDepth == containerStack.length) {
      containerStack = java.util.Arrays.copyOf(containerStack, stackDepth * 2)
      nextChildIndexes = java.util.Arrays.copyOf(nextChildIndexes, stackDepth * 2)
      containerKinds = java.util.Arrays.copyOf(containerKinds, stackDepth * 2)
    }
    containerStack(stackDepth) = node
    nextChildIndexes(stackDepth) = 0
    containerKinds(stackDepth) = kind
    stackDepth += 1
  }

  private def popContainer(): Unit = {
    stackDepth -= 1
    containerStack(stackDepth) = null
  }

  // Two accessors trade roles: one holds the event that was computed and not
  // yet consumed, the other holds the event consumed last. Consuming an event
  // swaps them instead of copying the event.
  private var inspected = InfosetAccessor()
  private var advanced = InfosetAccessor()
  private var hasInspectedEvent = false

  private val trdStack = new MStackOf[TermRuntimeData]()

  // Makes the next event the inspected one, if there is one.
  private def fill(): Boolean = {
    if (!hasInspectedEvent) {
      if (!started) {
        started = true
        pushContainer(cursor.buildState.documentElement, NodeKind.Document)
      }
      hasInspectedEvent = computeNext()
    }
    hasInspectedEvent
  }

  // Sets the inspected accessor to the next event. False if there are no more.
  @tailrec
  private def computeNext(): Boolean = {
    if (pendingEnd ne null) {
      val ended = pendingEnd
      pendingEnd = null
      setEvent(ended, NodeKind.Simple, isStart = false)
      freeEndedChild()
      true
    } else if (stackDepth == 0) {
      false
    } else {
      val innermost = stackDepth - 1
      val currentNode = containerStack(innermost)
      val currentKind = containerKinds(innermost)
      if (childExistsOrFinal(currentNode, nextChildIndexes(innermost))) {
        // A node ahead of this walk that is already freed was a hidden one,
        // which finished before the walk got here.
        val nextChild = currentNode.child(nextChildIndexes(innermost))
        nextChildIndexes(innermost) += 1
        if (nextChild eq null) {
          computeNext()
        } else {
          val nextKind = kindOf(nextChild)
          if (isHidden(nextChild, nextKind)) {
            computeNext()
          } else {
            setEvent(nextChild, nextKind, isStart = true)
            if (nextKind eq NodeKind.Simple) {
              pendingEnd = nextChild
            } else {
              pushContainer(nextChild, nextKind)
            }
            true
          }
        }
      } else if (currentKind eq NodeKind.Document) {
        popContainer()
        computeNext()
      } else {
        emitEnd(currentNode, currentKind)
        true
      }
    }
  }

  // Advances build until the child exists, or the parent is final with no
  // child there. The unparse only needs the child to exist, so it knows which
  // branch or occurrence it is on, not for it to have a value. Build having
  // finished with the child still missing is a stall.
  private def childExistsOrFinal(parent: DINode, index: Int): Boolean = {
    while (index >= parent.numChildren) {
      if (parent.isFinal) {
        return false
      }
      if (cursor.isFinished) {
        UnparseError(
          Nope,
          Nope,
          "Expected child %s of %s, but the build finished without it.",
          index + 1,
          parent.erd.namedQName.toExtendedSyntax
        )
      }
      cursor.advance()
    }
    true
  }

  /**
   * For each container the unparse is inside, how many of its children the
   * unparse has reached, so a debugger can show the infoset as an event-driven
   * unparse would have built it by now. A container the unparse is not inside
   * is fully reached or not reached at all, so it has no entry. A node whose
   * start event is computed but not yet consumed is left out.
   */
  def reachedChildCounts(): java.util.Map[DINode, Integer] = {
    val counts = new java.util.IdentityHashMap[DINode, Integer]
    var i = 0
    while (i < stackDepth) {
      counts.put(containerStack(i), nextChildIndexes(i))
      i += 1
    }
    if (hasInspectedEvent && inspected.isStart) {
      if (pendingEnd ne null) {
        // The started node is simple, so it is not on the stack.
        counts.put(containerStack(stackDepth - 1), nextChildIndexes(stackDepth - 1) - 1)
      } else if (stackDepth >= 2) {
        counts.remove(containerStack(stackDepth - 1))
        counts.put(containerStack(stackDepth - 2), nextChildIndexes(stackDepth - 2) - 1)
      }
    }
    counts
  }

  // The unparse is done with a node, so build is no longer ahead of it by one.
  def decrementLead(): Unit = cursor.buildState.decrementLead()

  // Once a node's end event is out, the node is no longer needed in its
  // parent, whose frame is now on top and whose next index is just past it.
  private def freeEndedChild(): Unit = {
    val parent = stackDepth - 1
    containerStack(parent).freeChildIfNoLongerNeeded(
      nextChildIndexes(parent) - 1,
      releaseUnneededInfoset
    )
  }

  private def isHidden(node: DINode, kind: NodeKind): Boolean = {
    if (kind eq NodeKind.Array) {
      node.numChildren > 0 && node.isHidden
    } else {
      node.isHidden
    }
  }

  private def emitEnd(endedNode: DINode, kind: NodeKind): Unit = {
    setEvent(endedNode, kind, isStart = false)
    popContainer()
    freeEndedChild()
  }

  private def setEvent(node: DINode, kind: NodeKind, isStart: Boolean): Unit = {
    if (kind eq NodeKind.Array) {
      inspected.kind = if (isStart) {
        InfosetEventKind.StartArray
      } else {
        InfosetEventKind.EndArray
      }
      inspected.info = Info(node.erd)
    } else {
      inspected.kind = if (isStart) {
        InfosetEventKind.StartElement
      } else {
        InfosetEventKind.EndElement
      }
      inspected.info = Info(node.asInstanceOf[DIElement])
    }
  }

  // The child index in each container where the search for its next hidden
  // child resumes. Hidden nodes are asked for in the order they appear in the tree.
  private lazy val hiddenCursors = new java.util.IdentityHashMap[DINode, Integer](8)

  // The container and index of each hidden node taken for unparsing and not yet freed.
  private lazy val handedOutContainers = new MStackOf[DINode](8)
  private lazy val handedOutIndexes = MStackOfInt(8)

  def takeExistingHiddenChild(parent: DINode, erd: ElementRuntimeData): DIElement = {
    val container = if (erd.isArray) {
      parent.child(findNextHiddenIndex(parent, erd, isArray = true))
    } else {
      parent
    }
    val index = findNextHiddenIndex(container, erd, isArray = false)
    handedOutContainers.push(container)
    handedOutIndexes.push(index)
    container.child(index).asInstanceOf[DIElement]
  }

  def freeExistingHiddenChild(): Unit = {
    val container = handedOutContainers.pop
    container.freeChildIfNoLongerNeeded(handedOutIndexes.pop(), releaseUnneededInfoset)
  }

  // The cursor stays on a hidden array while its occurrences are handed out one
  // at a time, and moves past the array when a different hidden node is asked for.
  private def findNextHiddenIndex(
    container: DINode,
    erd: ElementRuntimeData,
    isArray: Boolean
  ): Int = {
    val cursor = hiddenCursors.get(container)
    var index = if (cursor eq null) {
      0
    } else {
      cursor.intValue
    }
    var found = false
    while (!found) {
      Assert.invariant(childExistsOrFinal(container, index))
      // A hidden node is always ready; a node already freed is not hidden.
      val child = container.child(index)
      if ((child ne null) && isHidden(child, kindOf(child)) && (child.erd eq erd)) {
        found = true
      } else {
        index += 1
      }
    }
    val nextCursor = if (isArray) {
      index
    } else {
      index + 1
    }
    hiddenCursors.put(container, nextCursor)
    index
  }

  override def advance: Boolean = {
    if (fill()) {
      val consumed = inspected
      inspected = advanced
      advanced = consumed
      hasInspectedEvent = false
      true
    } else {
      false
    }
  }

  override def advanceAccessor: InfosetAccessor = advanced

  override def inspect: Boolean = fill()

  override def inspectAccessor: InfosetAccessor = inspected

  override def inspectOrError: InfosetAccessor = {
    if (inspect) {
      inspectAccessor
    } else {
      Assert.invariantFailed(
        "An InfosetEvent was required for unparsing, but no InfosetEvent was available."
      )
    }
  }

  override def advanceOrError: InfosetAccessor = {
    if (advance) {
      advanceAccessor
    } else {
      Assert.invariantFailed(
        "An InfosetEvent was required for unparsing, but no InfosetEvent was available."
      )
    }
  }

  override def isInspectArrayEnd: Boolean = inspect && inspected.isEnd && inspected.isArray

  override def pushTRD(trd: TermRuntimeData): Unit = trdStack.push(trd)

  override def maybeTopTRD(): Maybe[TermRuntimeData] = {
    if (trdStack.isEmpty) {
      Nope
    } else {
      One(trdStack.top)
    }
  }

  override def popTRD(trd: TermRuntimeData): TermRuntimeData = {
    val popped = trdStack.pop
    if (popped ne trd) {
      Assert.invariantFailed("TRDs do not match. Expected: " + trd + " got " + popped)
    }
    popped
  }
}

/**
 * What a node the walk visits is, which decides the events it gets. A complex
 * node is a complex element other than the document.
 */
private[unparsers] enum NodeKind {
  case Simple, Complex, Array, Document
}
