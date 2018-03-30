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

import org.apache.daffodil.processors._
import org.apache.daffodil.infoset._
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.equality._

class ComplexTypeUnparser(rd: RuntimeData, bodyUnparser: Unparser)
  extends CombinatorUnparser(rd) {

  override lazy val runtimeDependencies = Nil

  override def nom = "ComplexType"

  override def isEmpty = bodyUnparser.isEmpty

  override lazy val childProcessors = Seq(bodyUnparser)

  def unparse(start: UState): Unit = {
    start.childIndexStack.push(1L) // one-based indexing
    bodyUnparser.unparse1(start)
    start.childIndexStack.pop()
  }
}

class SequenceCombinatorUnparser(ctxt: ModelGroupRuntimeData, childUnparsers: Array[Unparser])
  extends CombinatorUnparser(ctxt)
  with ToBriefXMLImpl {

  override lazy val runtimeDependencies = Nil

  override def nom = "Sequence"

  // Sequences of nothing (no initiator, no terminator, nothing at all) should
  // have been optimized away
  Assert.invariant(childUnparsers.length > 0)
  Assert.invariant(ctxt.groupMembers.length == childUnparsers.length)

  override lazy val childProcessors: Seq[Processor] = childUnparsers

  def unparse(start: UState): Unit = {

    var index = 0
    var doUnparser = false
    val limit = childUnparsers.length
    start.groupIndexStack.push(1L) // one-based indexing

    while (index < limit) {
      doUnparser = false
      val childUnparser = childUnparsers(index)
      val childRD = childUnparser.context

      childRD match {
        case erd: ElementRuntimeData if !erd.isRequired => {
          // it's not a required element, so we check to see if we have a matching
          // incoming infoset event
          if (start.inspect) {
            val ev = start.inspectAccessor
            if (ev.isStart) {
              val eventNQN = ev.node.namedQName
              if (eventNQN =:= erd.namedQName) {
                doUnparser = true
              }
            } else if (ev.isEnd && ev.isComplex) {
                // ok. Expected case. Do nothing.
            } else {
              Assert.invariantFailed("Not a start event: " + ev)
            }
          }
        }
        case _ => {
          // since only elements can be optional, anything else is non-optional
          doUnparser = true
        }
      }
      if (doUnparser) {
        childUnparser.unparse1(start)
      }
      index += 1
      //
      // Note: the invariant is that unparsers move over 1 within their group themselves
      // we do not do the moving over here as we are the caller of the unparser.
      //
    }
    start.groupIndexStack.pop()
    //
    // this is establishing the invariant that unparsers (in this case the sequence unparser)
    // moves over within its containing group. The caller of an unparser does not do this move.
    //
    start.moveOverOneGroupIndexOnly()
  }
}

class ChoiceCombinatorUnparser(mgrd: ModelGroupRuntimeData, eventUnparserMap: Map[ChoiceBranchEvent, Unparser])
  extends CombinatorUnparser(mgrd)
  with ToBriefXMLImpl {
  override def nom = "Choice"

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors: Seq[Processor] = eventUnparserMap.map { case (k, v) => v }.toSeq

  def unparse(state: UState): Unit = {

    val event: InfosetAccessor = state.inspectOrError
    val key: ChoiceBranchEvent = event match {
      //
      // The ChoiceBranchStartEvent(...) is not a case class constructor. It is a
      // hash-table lookup for a cached value. This avoids constructing these
      // objects over and over again.
      //
      case e if e.isStart && e.isElement => ChoiceBranchStartEvent(e.asElement.runtimeData.namedQName)
      case e if e.isEnd && e.isElement => ChoiceBranchEndEvent(e.asElement.runtimeData.namedQName)
      case e if e.isStart && e.isArray => ChoiceBranchStartEvent(e.asArray.erd.namedQName)
      case e if e.isEnd && e.isArray => ChoiceBranchEndEvent(e.asArray.erd.namedQName)
    }

    val childUnparser = eventUnparserMap.get(key).getOrElse {
      UnparseError(One(mgrd.schemaFileLocation), One(state.currentLocation), "Encountered event %s. Expected one of %s.",
        key, eventUnparserMap.keys.mkString(", "))
    }
    childUnparser.unparse1(state)
  }
}

// Choices inside a hidden group ref (i.e. Hidden Choices) are slightly
// different because we will never see events for any of the branches. Instead,
// we will just always pick the branch in which every thing is defaultble or
// OVC, so we can calculated exactly which branch to take in a hidden choice
// statically at compile time. That logic is in ChoiceCombinator, and the
// branch to take is passed into this HiddenChoiceCombinatorUnparser.
class HiddenChoiceCombinatorUnparser(mgrd: ModelGroupRuntimeData, branchUnparser: Unparser)
  extends CombinatorUnparser(mgrd)
  with ToBriefXMLImpl {
  override def nom = "HiddenChoice"
  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors: Seq[Processor] = Seq(branchUnparser)

  def unparse(state: UState): Unit = {
    branchUnparser.unparse1(state)
  }
}

class DelimiterStackUnparser(initiatorOpt: Maybe[InitiatorUnparseEv],
  separatorOpt: Maybe[SeparatorUnparseEv],
  terminatorOpt: Maybe[TerminatorUnparseEv],
  ctxt: TermRuntimeData,
  bodyUnparser: Unparser)
  extends CombinatorUnparser(ctxt) {
  override def nom = "DelimiterStack"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<DelimiterStack initiator='" + initiatorOpt +
        "' separator='" + separatorOpt +
        "' terminator='" + terminatorOpt + "'>" +
        bodyUnparser.toBriefXML(depthLimit - 1) +
        "</DelimiterStack>"
  }

  override lazy val childProcessors: Seq[Processor] = Seq(bodyUnparser)

  override lazy val runtimeDependencies = initiatorOpt.toList ++ separatorOpt.toList ++ terminatorOpt.toList

  def unparse(state: UState): Unit = {
    // Evaluate Delimiters
    val init = if (initiatorOpt.isDefined) Maybe.toMaybe(initiatorOpt.get.evaluate(state)) else Nope
    val sep = if (separatorOpt.isDefined) Maybe.toMaybe(separatorOpt.get.evaluate(state)) else Nope
    val term = if (terminatorOpt.isDefined) Maybe.toMaybe(terminatorOpt.get.evaluate(state)) else Nope

    val node = DelimiterStackUnparseNode(init, sep, term)

    state.pushDelimiters(node)

    bodyUnparser.unparse1(state)

    state.popDelimiters
  }
}

class DynamicEscapeSchemeUnparser(escapeScheme: EscapeSchemeUnparseEv, ctxt: TermRuntimeData, bodyUnparser: Unparser)
  extends CombinatorUnparser(ctxt) {
  override def nom = "EscapeSchemeStack"

  override lazy val childProcessors: Seq[Processor] = Seq(bodyUnparser)

  override lazy val runtimeDependencies = Seq(escapeScheme)

  def unparse(state: UState): Unit = {
    // evaluate the dynamic escape scheme in the correct scope. the resulting
    // value is cached in the Evaluatable (since it is manually cached) and
    // future parsers/evaluatables that use this escape scheme will use that
    // cached value.
    escapeScheme.newCache(state)
    escapeScheme.evaluate(state)

    // Unparse
    bodyUnparser.unparse1(state)

    // invalidate the escape scheme cache
    escapeScheme.invalidateCache(state)
  }
}

class ArrayCombinatorUnparser(erd: ElementRuntimeData, bodyUnparser: Unparser)
  extends CombinatorUnparser(erd) {
  override def nom = "Array"
  override lazy val runtimeDependencies = Nil
  override lazy val childProcessors = Seq(bodyUnparser)

  def unparse(state: UState) {
    state.arrayIndexStack.push(1L) // one-based indexing
    state.occursBoundsStack.push(state.tunable.maxOccursBounds)

    var event = state.advanceOrError
    Assert.invariant(event.isStart && event.node.isInstanceOf[DIArray])

    bodyUnparser.unparse1(state)

    event = state.advanceOrError
    if (!(event.isEnd && event.node.isInstanceOf[DIArray])) {
      UnparseError(One(erd.schemaFileLocation), One(state.currentLocation), "Expected array end event for %s, but received %s.", erd.namedQName, event)
    }

    val shouldValidate =
      (state.dataProc.isDefined) && state.dataProc.value.getValidationMode != ValidationMode.Off

    val actualOccurs = state.arrayIndexStack.pop()
    state.occursBoundsStack.pop()

    if (shouldValidate) {
      (erd.minOccurs, erd.maxOccurs) match {
        case (Some(minOccurs), Some(maxOccurs)) => {
          val isUnbounded = maxOccurs == -1
          val occurrence = actualOccurs - 1
          if (isUnbounded && occurrence < minOccurs)
            state.validationError("%s occurred '%s' times when it was expected to be a " +
              "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.diagnosticDebugName,
              occurrence, minOccurs)
          else if (!isUnbounded && (occurrence < minOccurs || occurrence > maxOccurs))
            state.validationError("%s occurred '%s' times when it was expected to be a " +
              "minimum of '%s' and a maximum of '%s' times.", erd.diagnosticDebugName,
              occurrence, minOccurs, maxOccurs)
        }
        case _ => // ok
      }
    }
  }
}

class OptionalCombinatorUnparser(erd: ElementRuntimeData, bodyUnparser: Unparser)
  extends CombinatorUnparser(erd) {
  override def nom = "Optional"
  override lazy val childProcessors = Seq(bodyUnparser)

  override lazy val runtimeDependencies = Nil

  def unparse(state: UState) {

    state.arrayIndexStack.push(1L) // one-based indexing
    state.occursBoundsStack.push(1L)

    val event = state.inspectOrError
    Assert.invariant(event.isStart && !event.node.isInstanceOf[DIArray])

    bodyUnparser.unparse1(state)

    state.arrayIndexStack.pop()
    state.occursBoundsStack.pop()
  }
}

