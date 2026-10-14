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

package org.apache.daffodil.runtime1.infoset

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers.InfosetBuildState
import org.apache.daffodil.runtime1.processors.unparsers.InfosetTreeState
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError
import org.apache.daffodil.unparsers.runtime1.ElementUnparserBase
import org.apache.daffodil.unparsers.runtime1.RepeatingChildUnparser
import org.apache.daffodil.unparsers.runtime1.SequenceChildUnparser

/**
 * A node in a much smaller, dedicated tree of InfosetBuilders that parallels the
 * full Unparser tree: only Grams that actually create or select infoset
 * content (elements, sequences, choices, hidden groups) contribute one, so
 * building the infoset never has to dispatch through the many wrapper
 * unparsers that build no infoset events (delimiters, escape schemes,
 * layers, padding, specified-length) that sit between them in the
 * Unparser tree.
 *
 * An InfosetBuilder is immutable compiled-schema state shared by every unparse. The
 * per-unparse position lives in the InfosetBuildFrame it creates, on an InfosetBuildCursor's
 * explicit stack, so building can stop after any step and continue later
 * without holding a thread or a JVM call stack.
 */
trait InfosetBuilder extends Serializable {
  def newFrame(): InfosetBuildFrame

  // True only for NadaInfosetBuilder, which contributes nothing to the tree.
  def isEmpty: Boolean = false

  // This builder if it builds anything, else the other one.
  final def orElse(other: InfosetBuilder): InfosetBuilder = if (!isEmpty) this else other
}

/**
 * One InfosetBuilder's in-progress state for one unparse. `step` performs one
 * transition and must either push exactly one child frame onto the cursor
 * (this frame is stepped again once that child pops) or pop itself from the
 * cursor to signal it is complete. A frame is therefore stepped more than once,
 * so it keeps a phase that tells it which step it is on.
 */
abstract class InfosetBuildFrame {
  def step(cursor: InfosetBuildCursor): Unit
}

/**
 * The explicit stack of InfosetBuildFrames that stands in for the call stack of a
 * recursive build. `advance` runs it until the lead window is full, so a
 * caller that needs more infoset tree can pull it forward directly. Driven
 * against an `InfosetBuildState`.
 */
final class InfosetBuildCursor(
  root: InfosetBuilder,
  val buildState: InfosetBuildState
) {
  def state: InfosetTreeState = buildState

  // Starts with the root's frame.
  private val stack = {
    val frames = new MStackOf[InfosetBuildFrame](64)
    frames.push(root.newFrame())
    frames
  }

  def push(frame: InfosetBuildFrame): Unit = stack.push(frame)

  def pop(): Unit = stack.pop

  def isFinished: Boolean = stack.isEmpty

  /**
   * Steps until the lead window is full or building completes, so it takes a
   * single step when the window is already full. With lastAdvance it ignores
   * the window and runs until building completes. A failure propagates and
   * ends the unparse, so the cursor is not used again.
   */
  def advance(lastAdvance: Boolean = false): Unit = {
    while (!stack.isEmpty) {
      stack.top.step(this)
      if (!lastAdvance && buildState.leadExceedsBuildAheadLimit) {
        return
      }
    }
  }
}

/**
 * Builder for a Gram that creates or selects no infoset content. A builder
 * with nothing to build collapses into this one, and a sequence drops its
 * children that have it, so a parent only holds one where it needs a value
 * in that slot, such as a choice branch with no content, which the branch map
 * still needs an entry for. A parent that holds one recognizes it by isEmpty
 * and never builds it.
 */
object NadaInfosetBuilder extends InfosetBuilder {
  override def isEmpty = true

  override def toString = "Nada"

  override def newFrame(): InfosetBuildFrame =
    Assert.abort("NadaInfosetBuilders are all supposed to optimize out!")
}

/**
 * Builds each of several sibling Grams' content in order. Used only where a
 * `~` composition has more than one child that actually builds infoset
 * content; the common case of at most one such child never needs this.
 */
final private class SeqCompInfosetBuilder(children: Array[InfosetBuilder])
  extends InfosetBuilder {
  override def newFrame(): InfosetBuildFrame = new InfosetBuildFrame {
    private var i = 0
    override def step(cursor: InfosetBuildCursor): Unit = {
      if (i < children.length) {
        val child = children(i)
        i += 1
        cursor.push(child.newFrame())
      } else {
        cursor.pop()
      }
    }
  }
}

object SeqCompInfosetBuilder {
  def apply(children: Array[InfosetBuilder]): InfosetBuilder = {
    if (children.isEmpty) {
      NadaInfosetBuilder
    } else if (children.length == 1) {
      children.head
    } else {
      new SeqCompInfosetBuilder(children)
    }
  }
}

/**
 * Builds one element's infoset node and, for complex types, its descendant
 * nodes through contentBuilder. The node is made with the element unparser's
 * unparseBegin and unparseEnd, the same code an unparse runs, so build and
 * unparse create a node the same way; only what the element contains is built
 * through the builder tree.
 */
final class ElementInfosetBuilder(
  erd: ElementRuntimeData,
  elementUnparser: ElementUnparserBase,
  contentBuilder: InfosetBuilder
) extends InfosetBuilder {

  override def newFrame(): InfosetBuildFrame = new InfosetBuildFrame {
    import ContentBuildPhase.*

    // Start begins the element and pushes the frame of its content, if it has
    // any. AfterContent ends the element.
    private var phase: ContentBuildPhase = Start

    override def step(cursor: InfosetBuildCursor): Unit = {
      val state = cursor.state
      if (phase eq Start) {
        elementUnparser.unparseBegin(state)
        if (erd.isComplexType) {
          state.pushTRD(erd.optComplexTypeModelGroupRuntimeData.get)
          if (!contentBuilder.isEmpty) {
            phase = AfterContent
            cursor.push(contentBuilder.newFrame())
            return
          }
        }
      }
      if (erd.isComplexType) {
        state.popTRD(erd.optComplexTypeModelGroupRuntimeData.get)
      }
      elementUnparser.unparseEnd(state)
      cursor.pop()
    }
  }
}

/**
 * Pairs a sequence child's occurs-count and array bookkeeping, which comes from
 * its unparser, with the child's own InfosetBuilder, which SequenceInfosetBuilder
 * builds in place of the child's full unparser.
 */
final case class SequenceChildInfosetBuildInfo(
  childUnparser: SequenceChildUnparser,
  childBuilder: InfosetBuilder
)

/**
 * Where a sequence's frame is in its walk over the children. Start is before
 * the first step. NextChild is between children. AfterScalar and
 * AfterOccurrence are right after a child's frame popped. InArray is an array
 * or optional loop between occurrences.
 */
private enum SequenceBuildPhase {
  case Start, NextChild, AfterScalar, InArray, AfterOccurrence
}

/**
 * Where a frame that builds one piece of content is in its steps. Start is
 * before the content's frame is pushed. AfterContent is once that frame has
 * popped, when this frame is stepped again to finish.
 */
private enum ContentBuildPhase {
  case Start, AfterContent
}

/**
 * Builds an entire sequence's children, scalar and array/optional alike,
 * each through its own InfosetBuilder.
 */
final private class SequenceInfosetBuilder(children: Array[SequenceChildInfosetBuildInfo])
  extends InfosetBuilder {

  override def newFrame(): InfosetBuildFrame = new SequenceInfosetBuildFrame

  private final class SequenceInfosetBuildFrame extends InfosetBuildFrame {
    import SequenceBuildPhase.*

    private var phase: SequenceBuildPhase = Start
    private var index = 0
    // children(index), read once per child and used by every later phase.
    private var current: SequenceChildInfosetBuildInfo = null
    private var rep: RepeatingChildUnparser = null
    private var numOccurrences = 0
    private var maxReps = 0L

    override def step(cursor: InfosetBuildCursor): Unit = {
      val state = cursor.state
      phase match {
        case Start => {
          state.groupIndexStack.push(1L)
          phase = NextChild
          nextChild(cursor, state)
        }
        case NextChild => nextChild(cursor, state)
        case AfterScalar => {
          current.childUnparser.trd match {
            case erd: ElementRuntimeData if !erd.isRepresented => // ok, skip group advance
            case _ => state.moveOverOneGroupIndexOnly()
          }
          finishChild(state)
        }
        case AfterOccurrence => {
          numOccurrences += 1
          state.moveOverOneArrayIterationIndexOnly()
          state.moveOverOneOccursIndexOnly()
          state.moveOverOneGroupIndexOnly()
          phase = InArray
        }
        case InArray => {
          if (rep.shouldDoUnparser(rep, state)) {
            phase = AfterOccurrence
            cursor.push(current.childBuilder.newFrame())
          } else {
            rep.checkFinalOccursCountBetweenMinAndMaxOccurs(
              state,
              rep,
              numOccurrences,
              maxReps,
              state.arrayIterationPos - 1
            )
            rep.consumeEndArrayEvent(rep.erd, state)
            finishRepeating(state)
          }
        }
      }
    }

    private def nextChild(cursor: InfosetBuildCursor, state: InfosetTreeState): Unit = {
      if (index == children.length) {
        state.groupIndexStack.pop()
        cursor.pop()
      } else {
        current = children(index)
        val cu = current.childUnparser
        state.pushTRD(cu.trd)
        cu match {
          case r: RepeatingChildUnparser => {
            rep = r
            state.arrayIterationIndexStack.push(1L)
            state.occursIndexStack.push(1L)
            numOccurrences = 0
            maxReps = r.maxRepeatsConst

            Assert.invariant(state.inspect, "No event for building.")
            val ev = state.inspectAccessor
            if (ev.erd eq r.erd) {
              r.startArrayOrOptional(state)
              phase = InArray
            } else {
              r.checkFinalOccursCountBetweenMinAndMaxOccurs(
                state,
                r,
                numOccurrences,
                maxReps,
                0
              )
              finishRepeating(state)
            }
          }
          case _ => {
            phase = AfterScalar
            cursor.push(current.childBuilder.newFrame())
          }
        }
      }
    }

    private def finishRepeating(state: InfosetTreeState): Unit = {
      state.arrayIterationIndexStack.pop()
      state.occursIndexStack.pop()
      rep = null
      finishChild(state)
    }

    private def finishChild(state: InfosetTreeState): Unit = {
      state.popTRD(children(index).childUnparser.trd)
      index += 1
      phase = NextChild
    }
  }
}

object SequenceInfosetBuilder {
  def apply(children: Array[SequenceChildInfosetBuildInfo]): InfosetBuilder = {
    val nonEmptyChildren = children.filterNot(_.childBuilder.isEmpty)
    if (nonEmptyChildren.isEmpty) {
      NadaInfosetBuilder
    } else {
      new SequenceInfosetBuilder(nonEmptyChildren)
    }
  }
}

/**
 * Builds just the one structurally-present branch of a choice, resolved
 * from the next infoset event, through that branch's own InfosetBuilder.
 */
final class ChoiceInfosetBuilder(
  mgrd: ModelGroupRuntimeData,
  branchMap: Map[NamedQName, (TermRuntimeData, InfosetBuilder)],
  defaultBranch: Maybe[(TermRuntimeData, InfosetBuilder)]
) extends InfosetBuilder {

  private def resolveBranch(state: InfosetTreeState): (TermRuntimeData, InfosetBuilder) = {
    if (state.withinHiddenNest) {
      defaultBranch.get
    } else {
      state.pushTRD(mgrd)
      val event = state.inspectOrError
      // An end event never starts a branch, so it always takes the default.
      val fromTable = if (event.isStart) {
        branchMap.get(event.erd.namedQName)
      } else {
        None
      }
      val resolved = if (fromTable.isDefined) {
        fromTable
      } else {
        defaultBranch.toOption
      }
      if (resolved.isEmpty) {
        UnparseError(
          One(mgrd.schemaFileLocation),
          Nope,
          "Found next element %s, but expected one of %s.",
          event.erd.namedQName.toExtendedSyntax,
          branchMap.keys.map { _.toExtendedSyntax }.mkString(", ")
        )
      }
      state.popTRD(mgrd)
      resolved.get
    }
  }

  override def newFrame(): InfosetBuildFrame = new InfosetBuildFrame {
    import ContentBuildPhase.*

    // Start resolves the branch and pushes the frame of its content, if it has
    // any. AfterContent leaves the branch, whose TRD is kept to pop.
    private var phase: ContentBuildPhase = Start
    private var branchTRD: TermRuntimeData = null

    override def step(cursor: InfosetBuildCursor): Unit = {
      val state = cursor.state
      if (phase eq Start) {
        val (trd, builder) = resolveBranch(state)
        phase = AfterContent
        branchTRD = trd
        state.pushTRD(trd)
        if (!builder.isEmpty) {
          cursor.push(builder.newFrame())
        }
      } else {
        state.popTRD(branchTRD)
        cursor.pop()
      }
    }
  }
}

/**
 * Builds the body of a hidden group. withinHiddenNest must be maintained during
 * build too: it is what makes the nodes of hidden elements get created, since
 * no event will ever exist for them.
 */
final private class HiddenGroupInfosetBuilder(bodyBuilder: InfosetBuilder)
  extends InfosetBuilder {
  override def newFrame(): InfosetBuildFrame = new InfosetBuildFrame {
    import ContentBuildPhase.*

    // Start enters the hidden group and pushes the frame of its body.
    // AfterContent leaves the group.
    private var phase: ContentBuildPhase = Start

    override def step(cursor: InfosetBuildCursor): Unit = {
      if (phase eq Start) {
        phase = AfterContent
        cursor.state.incrementHiddenDef()
        cursor.push(bodyBuilder.newFrame())
      } else {
        cursor.state.decrementHiddenDef()
        cursor.pop()
      }
    }
  }
}

object HiddenGroupInfosetBuilder {
  def apply(bodyBuilder: InfosetBuilder): InfosetBuilder = {
    if (bodyBuilder.isEmpty) {
      NadaInfosetBuilder
    } else {
      new HiddenGroupInfosetBuilder(bodyBuilder)
    }
  }
}

/**
 * A nilled complex element has no children to build; nilled-ness is only
 * known once the node exists, so this checks it at build time rather than
 * resolving statically to either branch.
 */
final private class NilOrContentInfosetBuilder(contentBuilder: InfosetBuilder)
  extends InfosetBuilder {
  override def newFrame(): InfosetBuildFrame = new InfosetBuildFrame {
    import ContentBuildPhase.*

    // Start pushes the frame of the content, unless the element is nilled.
    // AfterContent, or a nilled element, finishes the frame.
    private var phase: ContentBuildPhase = Start

    override def step(cursor: InfosetBuildCursor): Unit = {
      if ((phase eq Start) && !cursor.state.currentInfosetNode.asComplex.isNilled) {
        phase = AfterContent
        cursor.push(contentBuilder.newFrame())
      } else {
        cursor.pop()
      }
    }
  }
}

object NilOrContentInfosetBuilder {
  def apply(contentBuilder: InfosetBuilder): InfosetBuilder = {
    if (contentBuilder.isEmpty) {
      NadaInfosetBuilder
    } else {
      new NilOrContentInfosetBuilder(contentBuilder)
    }
  }
}
