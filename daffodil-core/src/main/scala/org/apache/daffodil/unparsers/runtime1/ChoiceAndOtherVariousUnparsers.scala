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

import scala.jdk.CollectionConverters.*

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.*
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.ProperlySerializableMap.*
import org.apache.daffodil.runtime1.infoset.*
import org.apache.daffodil.runtime1.processors.*
import org.apache.daffodil.runtime1.processors.unparsers.*

case class ChoiceBranchMap(
  lookupTable: ProperlySerializableMap[ChoiceBranchEvent, Unparser],
  unmappedDefault: Option[Unparser]
) extends Serializable {

  def get(cbe: ChoiceBranchEvent): Maybe[Unparser] = {
    val fromTable = lookupTable.get(cbe)
    val res =
      if (fromTable != null) One(fromTable)
      else {
        //
        // There must be an unmapped default in this case
        // because otherwise the map is incomplete.
        //
        if (unmappedDefault.isDefined)
          One(unmappedDefault.get)
        else
          Nope
      }
    res
  }

  def defaultUnparser = unmappedDefault

  def childProcessors = lookupTable.values.iterator.asScala.toVector ++ unmappedDefault

  def keys = lookupTable.keySet.asScala
}

/*
 * Sometimes choices have an empty branch (e.g. a sequence that just has an
 * assert in it) that optimizes to a NadaUnparser. NadaUnparsers should all
 * optimize out, but the ChoiceCombinatorUnparser still expects to have
 * something in these cases. So we have a special empty branch unparser that
 * does nothing, but gives the ChoiceCombinatorUnparser something that it can
 * use.
 */
class ChoiceBranchEmptyUnparser(val context: RuntimeData) extends PrimUnparserNoData {

  override val runtimeDependencies = Array()

  def unparse(state: UState): Unit = {
    // do nothing
  }
}

class ChoiceCombinatorUnparser(
  mgrd: ModelGroupRuntimeData,
  choiceBranchMap: ChoiceBranchMap,
  choiceLengthInBits: MaybeInt
) extends CombinatorUnparser(mgrd)
  with ToBriefXMLImpl
  with WriteUnparser {
  override def nom = "Choice"

  override val runtimeDependencies = Array()

  override def childProcessors = choiceBranchMap.childProcessors

  /**
   * Resolves which branch an already-built child belongs to by keying off
   * the child's element identity, blocking until that's known, then
   * recurses into the resolved branch. Self-manages advancing/freeing its
   * own resolved tree-child position, and applies the same choice-length
   * filling as the structural pass. Covers the visible-choice path only;
   * the hidden-choice default branch is handled separately below.
   */
  override def writeContent(containerNode: DINode, state: UState): Unit = {
    val sharedCtx = state.sharedContext.get
    val complex = containerNode.asComplex
    val childIndex = state.childIndexStack.top.toInt

    val (maybeChildUnparser, resolvedChildIndex): (Maybe[Unparser], Int) =
      if (state.withinHiddenNest) {
        // A hidden choice's branch is always the single deterministic
        // default one (DFDL requires its outcome to be schema-determined,
        // not data-driven), so no key/event peek is needed. The tree child at this position, if any, IS this default branch's own content, not something to skip past.
        val idx = if (sharedCtx.childExistsOrFinal(complex, childIndex)) {
          childIndex
        } else {
          -1
        }
        (Maybe.toMaybe(choiceBranchMap.defaultUnparser), idx)
      } else {
        // Hidden elements never produce infoset events, so the branch
        // lookup keys are built from the first represented child. Build
        // still materializes a branch's leading hidden group as actual
        // tree children, so skip past those first.
        var idx = childIndex
        while (sharedCtx.childExistsOrFinal(complex, idx) && complex.child(idx).isHidden) {
          idx += 1
        }
        if (idx >= complex.numChildren) {
          // Build is done and no child ever showed up: the choice resolved
          // to a branch with no infoset footprint at all (e.g. an empty
          // sequence, or an absent defaultable element); fall back to the
          // default/unmapped branch.
          (Maybe.toMaybe(choiceBranchMap.defaultUnparser), -1)
        } else {
          val child = sharedCtx.awaitChild(complex, idx)
          val key: ChoiceBranchEvent = ChoiceBranchStartEvent(child.erd.namedQName)
          val fromTable = choiceBranchMap.lookupTable.get(key)
          if (fromTable != null) {
            // An actual match; this tree position genuinely belongs to this
            // choice.
            (One(fromTable), idx)
          } else {
            // No branch key matches this child, so it must belong to a
            // sibling term after this choice: this choice resolved to a
            // branch with no infoset footprint here and consumes no
            // tree position.
            (Maybe.toMaybe(choiceBranchMap.defaultUnparser), -1)
          }
        }
      }
    if (maybeChildUnparser.isEmpty) {
      // A real UnparseError, not an internal assertion: a choice with no
      // default and no match for what's actually in the tree is
      // malformed input, not an invariant violation.
      UnparseError(
        One(mgrd.schemaFileLocation),
        One(state.currentLocation),
        "No matching or default choice branch found."
      )
    }
    val child = if (resolvedChildIndex >= 0) {
      complex.child(resolvedChildIndex)
    } else {
      // A resumable group or ChoiceBranchEmptyUnparser needs no tree child,
      // so null is fine, but the unmapped default can also be a bare
      // ElementUnparserBase, which DOES need one: writeContent(null, state)
      // on that would NPE deep inside it instead of failing clearly here.
      if (maybeChildUnparser.get.isInstanceOf[ElementUnparserBase]) {
        Assert.invariantFailed(
          "Choice resolved to a simple-element default branch with no resolved tree position."
        )
      }
      null
    }

    // True when the resolved branch is itself a resumable group, whose own
    // dispatch already advances/frees its tree positions; this choice must
    // not also advance/free then, or it double-advances past the branch's
    // last child, skipping the following sibling term.
    var innerSelfManagesPosition = false
    withChoiceLengthFiller(state) {
      maybeChildUnparser.get match {
        case elemUnp: ElementUnparserBase => elemUnp.writeContent(child, state)
        case wu: WriteUnparser =>
          innerSelfManagesPosition = true
          wu.writeContent(containerNode, state)
        case emptyUnp: ChoiceBranchEmptyUnparser =>
          // A branch that optimized to nothing (e.g. a sequence containing
          // only an assert); runs its (no-op) unparse, same as any other
          // synchronous branch.
          emptyUnp.unparse1(state)
        case other =>
          Assert.usageError(s"unhandled choice branch unparser type: $other")
      }
    }

    // Nothing to advance/free when the branch had no infoset footprint
    // (resolvedChildIndex == -1), nor when it's itself a resumable group (innerSelfManagesPosition), which already did so for its own positions.
    if (resolvedChildIndex >= 0 && !innerSelfManagesPosition) {
      state.moveOverOneElementChildOnly()
      complex.freeChildIfNoLongerNeeded(resolvedChildIndex, state.releaseUnneededInfoset)
    }
  }

  def unparse(state: UState): Unit = {
    if (state.withinHiddenNest) {
      val branchForUnparseIfHidden = choiceBranchMap.defaultUnparser
      branchForUnparseIfHidden.get.unparse1(state)
    } else {
      state.pushTRD(mgrd)
      val event: InfosetAccessor = state.inspectOrError
      val key: ChoiceBranchEvent = event match {
        //
        // The ChoiceBranchStartEvent(...) is not a case class constructor. It is a
        // hash-table lookup for a cached value. This avoids constructing these
        // objects over and over again.
        //
        case e if e.isStart && e.isElement => ChoiceBranchStartEvent(e.erd.namedQName)
        case e if e.isEnd && e.isElement => ChoiceBranchEndEvent(e.erd.namedQName)
        case e if e.isStart && e.isArray => ChoiceBranchStartEvent(e.erd.namedQName)
        case e if e.isEnd && e.isArray => ChoiceBranchEndEvent(e.erd.namedQName)
      }

      val maybeChildUnparser = choiceBranchMap.get(key)
      if (maybeChildUnparser.isEmpty) {
        UnparseError(
          One(mgrd.schemaFileLocation),
          One(state.currentLocation),
          "Found next element %s, but expected one of %s.",
          key.qname.toExtendedSyntax,
          choiceBranchMap.keys
            .map {
              _.qname.toExtendedSyntax
            }
            .mkString(", ")
        )
      }
      val childUnparser = maybeChildUnparser.get
      state.popTRD(mgrd)
      state.pushTRD(childUnparser.context.asInstanceOf[TermRuntimeData])
      withChoiceLengthFiller(state) {
        childUnparser.unparse1(state)
      }
      state.popTRD(childUnparser.context.asInstanceOf[TermRuntimeData])
    }
  }

  /**
   * Wraps runChosenBranch with the dfdl:choiceLength "unused region"
   * filler (no-op if choiceLengthInBits isn't set), capturing DOS
   * position before/after via ChoiceUnusedUnparser. The
   * state.setProcessor calls are redundant for unparse()'s call site but
   * required for writeContent's, which bypasses unparse1 (and its
   * equivalent setProcessor) entirely.
   */
  private def withChoiceLengthFiller(state: UState)(runChosenBranch: => Unit): Unit = {
    if (choiceLengthInBits.isEmpty) {
      runChosenBranch
    } else {
      val suspendableOp =
        new ChoiceUnusedUnparserSuspendableOperation(mgrd, choiceLengthInBits.get)
      val unusedUnparser = new ChoiceUnusedUnparser(mgrd, choiceLengthInBits.get, suspendableOp)
      state.setProcessor(ChoiceCombinatorUnparser.this)
      suspendableOp.captureDOSStartForChoiceUnused(state)
      runChosenBranch
      state.setProcessor(ChoiceCombinatorUnparser.this)
      suspendableOp.captureDOSEndForChoiceUnused(state)
      unusedUnparser.unparse(state)
    }
  }
}

class DelimiterStackUnparser(
  initiatorOpt: Maybe[InitiatorUnparseEv],
  separatorOpt: Maybe[SeparatorUnparseEv],
  terminatorOpt: Maybe[TerminatorUnparseEv],
  ctxt: TermRuntimeData,
  bodyUnparser: Unparser
) extends CombinatorUnparser(ctxt)
  with WriteUnparser {

  // Hoisted once per instance rather than passed as `pushDelimiterScope`/
  // `bodyUnparser.unparse1` at each call site: an eta-expansion of an
  // instance method (or one of its fields) closes over `this`, so it
  // allocates a fresh closure on every call otherwise, and both
  // writeContent and unparse run once per matching element in the infoset.
  private val funcPushDelimiterScope: UState => Unit = pushDelimiterScope
  private val funcBodyUnparserUnparse1: UState => Unit = bodyUnparser.unparse1

  /**
   * Pushes the delimiter scope, recurses into the body, and pops only
   * once the body's own writeContent (if any) returns, since a pending
   * pause still needs the stack for separator writing.
   */
  override def writeContent(containerNode: DINode, state: UState): Unit =
    writeWithPushPop(
      containerNode,
      bodyUnparser,
      state,
      setup = funcPushDelimiterScope,
      teardown = (state, _) => state.popDelimiters()
    )
  override def nom = "DelimiterStack"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else
      "<DelimiterStack initiator='" + initiatorOpt +
        "' separator='" + separatorOpt +
        "' terminator='" + terminatorOpt + "'>" +
        bodyUnparser.toBriefXML(depthLimit - 1) +
        "</DelimiterStack>"
  }

  override def childProcessors = Vector(bodyUnparser)

  override val runtimeDependencies =
    (initiatorOpt.toList ++ separatorOpt.toList ++ terminatorOpt.toList).toArray

  def unparse(state: UState): Unit =
    withPushPop(
      state,
      setup = funcPushDelimiterScope,
      dispatch = funcBodyUnparserUnparse1,
      teardown = (state, _) => state.popDelimiters()
    )

  private def pushDelimiterScope(state: UState): Unit = {
    val init =
      if (initiatorOpt.isDefined) initiatorOpt.get.evaluate(state)
      else EmptyDelimiterStackUnparseNode.empty
    val sep =
      if (separatorOpt.isDefined) separatorOpt.get.evaluate(state)
      else EmptyDelimiterStackUnparseNode.empty
    val term =
      if (terminatorOpt.isDefined) terminatorOpt.get.evaluate(state)
      else EmptyDelimiterStackUnparseNode.empty
    state.pushDelimiters(DelimiterStackUnparseNode(init, sep, term))
  }
}

class DynamicEscapeSchemeUnparser(
  escapeScheme: EscapeSchemeUnparseEv,
  ctxt: TermRuntimeData,
  bodyUnparser: Unparser
) extends CombinatorUnparser(ctxt)
  with WriteUnparser {
  override def nom = "EscapeSchemeStack"

  override def childProcessors = Vector(bodyUnparser)

  override val runtimeDependencies = Array(escapeScheme)

  // Hoisted once per instance rather than passed at each call site: an
  // eta-expansion of an instance method, or a lambda referencing an
  // instance field like `escapeScheme`, closes over `this`, so it
  // allocates a fresh closure on every call otherwise, and both
  // writeContent and unparse run once per matching element in the infoset.
  private val funcCacheEscapeScheme: UState => Unit = cacheEscapeScheme
  private val funcBodyUnparserUnparse1: UState => Unit = bodyUnparser.unparse1
  private val funcInvalidateCache: (UState, Unit) => Unit =
    (state, _) => escapeScheme.invalidateCache(state)

  /**
   * Caches the escape scheme, recurses into the body, and invalidates
   * the cache only once the body's own writeContent (if any) returns,
   * since a pending pause still needs the cache for delimiter writing.
   */
  override def writeContent(containerNode: DINode, state: UState): Unit =
    writeWithPushPop(
      containerNode,
      bodyUnparser,
      state,
      setup = funcCacheEscapeScheme,
      teardown = funcInvalidateCache
    )

  def unparse(state: UState): Unit =
    withPushPop(
      state,
      setup = funcCacheEscapeScheme,
      dispatch = funcBodyUnparserUnparse1,
      teardown = funcInvalidateCache
    )

  // Evaluates the dynamic escape scheme in the correct scope; the result is
  // cached in the Evaluatable (since it is manually cached), so future
  // unparsers/evaluatables that use this escape scheme reuse that cached
  // value.
  private def cacheEscapeScheme(state: UState): Unit = {
    escapeScheme.newCache(state)
    escapeScheme.evaluate(state)
  }
}
