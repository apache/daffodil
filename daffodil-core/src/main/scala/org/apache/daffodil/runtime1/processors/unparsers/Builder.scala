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
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.infoset.ChoiceBranchEndEvent
import org.apache.daffodil.runtime1.infoset.ChoiceBranchEvent
import org.apache.daffodil.runtime1.infoset.ChoiceBranchStartEvent
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.unparsers.runtime1.RepeatingChildUnparser
import org.apache.daffodil.unparsers.runtime1.SequenceChildUnparser

/**
 * A node in a much smaller, dedicated tree of Builders that parallels the
 * full Unparser tree: only Grams that actually create or select infoset
 * content (elements, sequences, choices, hidden groups) contribute one, so
 * building the infoset never has to dispatch through the many write-only
 * wrapper unparsers (delimiters, escape schemes, layers, padding,
 * specified-length) that sit between them in the Unparser tree. Driven
 * exclusively from `BuildState`, never from a write-side `UState`.
 */
trait Builder extends Serializable {
  def build(state: UState): Unit
}

/**
 * A no-op stand-in for a choice branch or sequence child whose content is
 * empty (e.g. an empty sequence), so its build-time presence can still be
 * recorded without anything actually needing to happen.
 */
object EmptyBuilder extends Builder {
  override def build(state: UState): Unit = ()
}

/**
 * Builds each of several sibling Grams' content in order. Used only where a
 * `~` composition has more than one child that actually builds infoset
 * content; the common case of at most one such child never needs this.
 */
final class SeqCompBuilder(children: Array[Builder]) extends Builder {
  override def build(state: UState): Unit = {
    var i = 0
    while (i < children.length) {
      children(i).build(state)
      i += 1
    }
  }
}

/**
 * Builds one element's infoset node and, for complex types, recurses into
 * contentBuilder to build descendant nodes. unparseBegin/unparseEnd are the
 * same element-kind-specific (plain/nillable/OVC/etc.) node-creation logic
 * unparse() itself uses, including the bounded-lookahead lead-counter
 * hookup and the deferred simple-value finalization; only the "what does
 * this element contain" recursion is redirected to the builder tree instead
 * of back into the unparser tree.
 */
final class ElementBuilder(
  erd: ElementRuntimeData,
  unparseBegin: UState => Unit,
  unparseEnd: UState => Unit,
  contentBuilder: Maybe[Builder]
) extends Builder {

  override def build(state: UState): Unit = {
    unparseBegin(state)

    if (erd.isComplexType) {
      state.pushTRD(erd.optComplexTypeModelGroupRuntimeData.get)
      if (contentBuilder.isDefined) { contentBuilder.get.build(state) }
      state.popTRD(erd.optComplexTypeModelGroupRuntimeData.get)
    }

    unparseEnd(state)
  }
}

/**
 * Pairs a sequence child's existing occurs-count/array bookkeeping (reused
 * as-is from the Unparser tree, since it is cheap, pure state bookkeeping
 * unrelated to the tree-walking overhead this Builder tree exists to avoid)
 * with that same child's own Builder, which SequenceBuilder recurses into
 * instead of the child's full Unparser.
 */
final case class SequenceChildBuildInfo(
  childUnparser: SequenceChildUnparser,
  childBuilder: Builder
)

/**
 * Builds an entire sequence's children, scalar and array/optional alike.
 * Mirrors OrderedSequenceUnparserBase's own build loop exactly, except each
 * child's recursive build call targets its Builder rather than its full,
 * write-only-wrapper-laden Unparser.
 */
final class SequenceBuilder(children: IndexedSeq[SequenceChildBuildInfo]) extends Builder {

  override def build(state: UState): Unit = {
    state.groupIndexStack.push(1L)

    var index = 0
    val limit = children.length
    while (index < limit) {
      val info = children(index)
      val cu = info.childUnparser
      val trd = cu.trd
      state.pushTRD(trd)
      cu match {
        case rep: RepeatingChildUnparser => {
          state.arrayIterationIndexStack.push(1L)
          state.occursIndexStack.push(1L)
          val erd = rep.erd
          var numOccurrences = 0
          val maxReps = rep.maxRepeats(state)

          Assert.invariant(state.inspect, "No event for building.")
          val ev = state.inspectAccessor
          if (ev.erd eq erd) {
            rep.startArrayOrOptional(state)
            while (rep.shouldDoUnparser(rep, state)) {
              info.childBuilder.build(state)
              numOccurrences += 1
              state.moveOverOneArrayIterationIndexOnly()
              state.moveOverOneOccursIndexOnly()
              state.moveOverOneGroupIndexOnly()
            }
            rep.checkFinalOccursCountBetweenMinAndMaxOccurs(
              state,
              rep,
              numOccurrences,
              maxReps,
              state.arrayIterationPos - 1
            )
            rep.endArrayOrOptional(erd, state)
          } else {
            rep.checkFinalOccursCountBetweenMinAndMaxOccurs(
              state,
              rep,
              numOccurrences,
              maxReps,
              0
            )
          }

          state.arrayIterationIndexStack.pop()
          state.occursIndexStack.pop()
        }
        case _ => {
          info.childBuilder.build(state)
          trd match {
            case erd: ElementRuntimeData if !erd.isRepresented => // ok, skip group advance
            case _ => state.moveOverOneGroupIndexOnly()
          }
        }
      }
      state.popTRD(trd)
      index += 1
    }

    state.groupIndexStack.pop()
  }
}

/**
 * Builds just the one structurally-present branch of a choice. Mirrors
 * ChoiceCombinatorUnparser's own branch resolution exactly, except the
 * resolved branch's recursive build call targets its Builder rather than
 * its full Unparser.
 */
final class ChoiceBuilder(
  mgrd: ModelGroupRuntimeData,
  branchMap: Map[ChoiceBranchEvent, (TermRuntimeData, Builder)],
  defaultBranch: Maybe[(TermRuntimeData, Builder)]
) extends Builder {

  private def buildBranch(state: UState, branch: (TermRuntimeData, Builder)): Unit = {
    val (trd, builder) = branch
    state.pushTRD(trd)
    builder.build(state)
    state.popTRD(trd)
  }

  override def build(state: UState): Unit = {
    if (state.withinHiddenNest) {
      buildBranch(state, defaultBranch.get)
    } else {
      state.pushTRD(mgrd)
      val event = state.inspectOrError
      val key: ChoiceBranchEvent = event match {
        case e if e.isStart && (e.isElement || e.isArray) =>
          ChoiceBranchStartEvent(e.erd.namedQName)
        case e if e.isEnd && (e.isElement || e.isArray) =>
          ChoiceBranchEndEvent(e.erd.namedQName)
      }
      val fromTable = branchMap.get(key)
      val resolved = if (fromTable.isDefined) {
        fromTable
      } else {
        defaultBranch.toOption
      }
      if (resolved.isEmpty) {
        UnparseError(
          One(mgrd.schemaFileLocation),
          One(state.currentLocation),
          "Found next element %s, but expected one of %s.",
          key.qname.toExtendedSyntax,
          branchMap.keys.map { _.qname.toExtendedSyntax }.mkString(", ")
        )
      }
      state.popTRD(mgrd)
      buildBranch(state, resolved.get)
    }
  }
}

/**
 * Builds the body of a hidden group. withinHiddenNest must stay maintained
 * during build too: it is what tells a hidden element's unparseBegin/
 * unparseEnd to manufacture a node instead of consuming an event that will
 * never exist.
 */
final class HiddenGroupBuilder(bodyBuilder: Builder) extends Builder {
  override def build(state: UState): Unit = {
    try {
      state.incrementHiddenDef()
      bodyBuilder.build(state)
    } finally {
      state.decrementHiddenDef()
    }
  }
}

/**
 * A nilled complex element has no children to build; nilled-ness is only
 * known once the node exists, so this checks it at build time rather than
 * resolving statically to either branch.
 */
final class NilOrContentBuilder(contentBuilder: Builder) extends Builder {
  override def build(state: UState): Unit = {
    val inode = state.currentInfosetNode.asComplex
    if (!inode.isNilled) { contentBuilder.build(state) }
  }
}
