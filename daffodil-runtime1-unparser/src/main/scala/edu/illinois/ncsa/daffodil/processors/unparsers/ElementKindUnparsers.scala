/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.unparsers
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.equality._

class ComplexTypeUnparser(rd: RuntimeData, bodyUnparser: Unparser)
  extends UnparserObject(rd) {
  override def nom = "ComplexType"

  override lazy val childProcessors = Seq(bodyUnparser)

  def unparse(start: UState): Unit = {
    start.childIndexStack.push(1L) // one-based indexing
    bodyUnparser.unparse1(start, rd)
    start.childIndexStack.pop()
  }
}

class SequenceCombinatorUnparser(rdArg: ModelGroupRuntimeData, childUnparsers: Vector[Unparser])
  extends TermUnparser(rdArg)
  with ToBriefXMLImpl {
  override def nom = "Sequence"

  // Sequences of nothing (no initiator, no terminator, nothing at all) should
  // have been optimized away
  Assert.invariant(childUnparsers.length > 0)

  // Since some of the grammar terms might have folded away to EmptyGram,
  // the number of unparsers here may be different from the number of
  // children of the sequence group.
  Assert.invariant(rdArg.groupMembers.length >= childUnparsers.length)

  override lazy val childProcessors: Seq[Processor] = childUnparsers

  def unparse(start: UState): Unit = {

    start.groupIndexStack.push(1L) // one-based indexing

    var index = 0
    var doUnparser = false
    val limit = childUnparsers.length

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
              val c = ev.asComplex
              //ok. We've peeked ahead and found the end of the complex element
              //that this sequence is the model group of.
              val optParentRD = termRuntimeData.immediateEnclosingElementRuntimeData
              optParentRD match {
                case Some(e: ElementRuntimeData) =>
                  Assert.invariant(c.runtimeData.namedQName =:= e.namedQName)
                case _ =>
                  Assert.invariantFailed("Not end element for this sequence's containing element. Event %s, optParentRD %s.".format(
                    ev, optParentRD))
              }
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
        childUnparser.unparse1(start, childRD)
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
  extends TermUnparser(mgrd)
  with ToBriefXMLImpl {
  override def nom = "Choice"

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
    childUnparser.unparse1(state, mgrd)
  }
}

// Choices inside a hidden group ref (i.e. Hidden Choices) are slightly
// different because we will never see events for any of the branches. Instead,
// we will just always pick the branch in which every thing is defaultble or
// OVC, so we can calculated exactly which branch to take in a hidden choice
// statically at compile time. That logic is in ChoiceCombinator, and the
// branch to take is passed into this HiddenChoiceCombinatorUnparser.
class HiddenChoiceCombinatorUnparser(mgrd: ModelGroupRuntimeData, branchUnparser: Unparser)
  extends TermUnparser(mgrd)
  with ToBriefXMLImpl {
  override def nom = "HiddenChoice"

  override lazy val childProcessors: Seq[Processor] = Seq(branchUnparser)

  def unparse(state: UState): Unit = {
    branchUnparser.unparse1(state, mgrd)
  }
}

class DelimiterStackUnparser(initiatorOpt: Maybe[InitiatorUnparseEv],
  separatorOpt: Maybe[SeparatorUnparseEv],
  terminatorOpt: Maybe[TerminatorUnparseEv],
  override val context: RuntimeData,
  bodyUnparser: Unparser)
  extends Unparser {
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

    bodyUnparser.unparse1(state, context)

    state.popDelimiters
  }
}

class DynamicEscapeSchemeUnparser(escapeScheme: EscapeSchemeUnparseEv, override val context: RuntimeData, bodyUnparser: Unparser)
  extends Unparser {
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
    bodyUnparser.unparse1(state, context)

    // invalidate the escape scheme cache
    escapeScheme.invalidateCache(state)
  }
}

class ArrayCombinatorUnparser(erd: ElementRuntimeData, bodyUnparser: Unparser)
  extends TermUnparser(erd) {
  override def nom = "Array"
  override lazy val childProcessors = Seq(bodyUnparser)

  def unparse(state: UState) {
    state.arrayIndexStack.push(1L) // one-based indexing
    state.occursBoundsStack.push(DaffodilTunableParameters.maxOccursBounds)

    var event = state.advanceOrError
    Assert.invariant(event.isStart && event.node.isInstanceOf[DIArray])

    bodyUnparser.unparse1(state, erd)

    event = state.advanceOrError
    if (!(event.isEnd && event.node.isInstanceOf[DIArray])) {
      UnparseError(One(erd.schemaFileLocation), One(state.currentLocation), "Needed end of array, but found %s.", event)
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
              "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.prettyName,
              occurrence, minOccurs)
          else if (!isUnbounded && (occurrence < minOccurs || occurrence > maxOccurs))
            state.validationError("%s occurred '%s' times when it was expected to be a " +
              "minimum of '%s' and a maximum of '%s' times.", erd.prettyName,
              occurrence, minOccurs, maxOccurs)
        }
        case _ => // ok
      }
    }
  }
}

class OptionalCombinatorUnparser(erd: ElementRuntimeData, bodyUnparser: Unparser) extends UnparserObject(erd) {
  override def nom = "Optional"
  override lazy val childProcessors = Seq(bodyUnparser)

  def unparse(state: UState) {

    state.arrayIndexStack.push(1L) // one-based indexing
    state.occursBoundsStack.push(1L)

    val event = state.inspectOrError
    Assert.invariant(event.isStart && !event.node.isInstanceOf[DIArray])

    bodyUnparser.unparse1(state, erd)

    state.arrayIndexStack.pop()
    state.occursBoundsStack.pop()
  }
}

class FinalizeProcessingUnparser(val context: ElementRuntimeData) extends PrimUnparser {
  override val runtimeDependencies = Nil

  def unparse(state: UState) {
    val ev = state.advanceMaybe
    if (ev.isDefined) {
      UnparseError(Nope, One(state.currentLocation), "Expected no remaining events, but received: %s.", ev.get)
    }
  }
}
