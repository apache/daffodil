package edu.illinois.ncsa.daffodil.processors.unparsers
/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.Processor
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.DIComplex

// TODO: JIRA DFDL-1581 - rename classes to remove "Statement" prefix (and rename file)
abstract class StatementElementUnparserBase(
  rd: ElementRuntimeData,
  name: String,
  setVarUnparsers: Array[Unparser],
  eBeforeUnparser: Maybe[Unparser],
  eUnparser: Maybe[Unparser],
  eAfterUnparser: Maybe[Unparser])
  extends TermUnparser(rd) {

  override lazy val childProcessors: Seq[Processor] = setVarUnparsers ++ eBeforeUnparser.toSeq ++ eUnparser.toSeq ++ eAfterUnparser.toSeq

  def move(state: UState): Unit // implement for different kinds of "moving over to next thing"
  def unparseBegin(state: UState): Unit
  def unparseEnd(state: UState): Unit

  def doSetVars(state: UState) {
    var i: Int = 0
    while (i < setVarUnparsers.length) {
      val unp = setVarUnparsers(i)
      i += 1
      unp.unparse1(state, rd)
    }
  }

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Element name='" + name + "'>" +
        (if (eBeforeUnparser.isDefined) eBeforeUnparser.value.toBriefXML(depthLimit - 1) else "") +
        (if (eUnparser.isDefined) eUnparser.value.toBriefXML(depthLimit - 1) else "") +
        setVarUnparsers.map { _.toBriefXML(depthLimit - 1) }.mkString +
        (if (eAfterUnparser.isDefined) eAfterUnparser.value.toBriefXML(depthLimit - 1) else "") +
        "</Element>"
  }

  def validate(state: UState): Unit = {
    ??? // TODO: JIRA DFDL-1582 - Is the ticket for implementing the Unparser - validation feature

    //    val currentElement = state.thisElement
    //
    //    if (currentElement.valid.isDefined) { return }
    //
    //    val resultState = DFDLCheckConstraintsFunction.validate(state) match {
    //      case Right(boolVal) => {
    //        log(LogLevel.Debug, "Validation succeeded for %s", currentElement.toXML())
    //        currentElement.setValid(true)
    //      }
    //      case Left(failureMessage) => {
    //        log(LogLevel.Debug,
    //          "Validation failed for %s due to %s. The element value was %s.",
    //          context.toString, failureMessage, currentElement.toXML())
    //        state.reportValidationError("%s failed dfdl:checkConstraints due to %s",
    //          context.toString, failureMessage)
    //        currentElement.setValid(false)
    //      }
    //    }
  }

  def unparse(state: UState): Unit = {

    if (state.dataProc.isDefined) state.dataProc.value.startElement(state, this)
    unparseBegin(state)
    if (eBeforeUnparser.isDefined) {
      eBeforeUnparser.get.unparse1(state, rd)
    }

    Assert.invariant(state.hasInfoset)

    //
    // We used to try to capture the starts and ends of the content region
    // and value regions here in this unparse method.
    //
    // The reason we can't do this here, is that the data grammar has a variety
    // of places where a content region begins, a value region begins, a value
    // region ends, and a content region ends. We can litter the grammar with
    // "capture" unparsers that gather this information and that way this
    // combinator doesn't have to have 36 categories of argument unparsers
    // that it does before or after one or the other. For example, there's
    // content and value regions for literal nil values, as well as regular values,
    // and nils have their own delimiters. The grammar is just not set up for
    // those to share one element combinator. Such an element combinator would
    // have as arguments, every grammar term for nils, empties, and regular values
    // including all their delimiters. It's just not a useful factoring.
    //

    if (eUnparser.isDefined) {
      eUnparser.get.unparse1(state, rd)
    }

    doSetVars(state) // Even unparsing, setVars are always after the element so they can refer to "."

    if (eAfterUnparser.isDefined) {
      eAfterUnparser.get.unparse1(state, rd)
    }
    unparseEnd(state)
    if (state.dataProc.isDefined) state.dataProc.value.endElement(state, this)

  }
}

class StatementElementUnparser(
  erd: ElementRuntimeData,
  name: String,
  setVarUnparsers: Array[Unparser],
  eBeforeUnparser: Maybe[Unparser],
  eUnparser: Maybe[Unparser],
  eAfterUnparser: Maybe[Unparser])
  extends StatementElementUnparserBase(
    erd,
    name,
    setVarUnparsers,
    eBeforeUnparser,
    eUnparser,
    eAfterUnparser) {

  def move(start: UState) {
    val grIndex = start.groupIndexStack.pop()
    start.groupIndexStack.push(grIndex + 1)
    val childIndex = start.childIndexStack.pop()
    start.childIndexStack.push(childIndex + 1)
  }

  def unparseBegin(state: UState): Unit = {
    val elem =
      if (!erd.isHidden) {
        // Hidden elements are not in the infoset, so we will never get an event
        // for them. Only try to consume start events for non-hidden elements
        val event = state.advanceOrError
        if (!event.isStart) { //  || event.erd != erd) {
          UnparseError(Nope, One(state.currentLocation), "Expected element start event for %s, but received %s.", erd.namedQName, event)
        }
        event.asElement
      } else {
        // Since we never get events for hidden elements, their infoset elements
        // will have never been created. This means we need to manually create them
        val e = if (erd.isComplexType) new DIComplex(erd) else new DISimple(erd)
        state.currentInfosetNode.asComplex.addChild(e)
        e
      }

    // When the infoset events are being advanced, the currentInfosetNodeStack
    // is pushing and popping to match the events. This provides the proper
    // context for evaluation of expressions.
    val e = One(elem)
    state.currentInfosetNodeStack.push(e)
    state.aaa_currentNode = e
  }

  def unparseEnd(state: UState): Unit = {
    if (!erd.isHidden) {
      // Hidden elements are not in the infoset, so we will never get an event
      // for them. Only try to consume end events for non-hidden elements
      val event = state.advanceOrError
      if (!event.isEnd) { //  || event.erd != erd) {
        UnparseError(Nope, One(state.currentLocation), "Expected element end event for %s, but received %s.", erd.namedQName, event)
      }
    }

    state.currentInfosetNodeStack.pop
    state.aaa_currentNode =
      if (state.currentInfosetNodeStack.isEmpty)
        Nope
      else
        state.currentInfosetNodeStack.top

    move(state)
  }
}

/**
 * The unparser used for an element that has inputValueCalc.
 *
 * The only thing we do is move over one child element, because the
 * inputValueCalc element does take up one child element position.
 * However, not in the group - because that is what is used to decide
 * whether to place separators, and there should not be any separator
 * corresponding to an IVC element.
 */
class StatementElementUnparserNoRep(
  erd: ElementRuntimeData,
  name: String,
  setVarUnparsers: Array[Unparser])
  extends StatementElementUnparser(
    erd,
    name,
    setVarUnparsers,
    Nope,
    Nope,
    Nope) {

  /**
   * Move over in the element children, but not in the group.
   * This avoids separators for this IVC element.
   */
  override def move(state: UState) {
    val childIndex = state.childIndexStack.pop()
    state.childIndexStack.push(childIndex + 1)
  }
}

class StatementElementOutputValueCalcUnparser(
  erd: ElementRuntimeData,
  name: String,
  setVarUnparsers: Array[Unparser],
  eBeforeUnparser: Maybe[Unparser],
  eUnparser: Maybe[Unparser],
  eAfterUnparser: Maybe[Unparser])
  extends StatementElementUnparser(
    erd,
    name,
    setVarUnparsers,
    eBeforeUnparser,
    eUnparser,
    eAfterUnparser) {

  override def unparseBegin(state: UState) {
    val elem =
      if (!erd.isHidden) {
        // outputValueCalc elements are optional in the infoset. If the next event
        // is for this is for this OVC element, then consume the start/end events.
        // Otherwise, the next event is for a following element, and we do not want
        // to consume it. Don't even bother checking all this if it's hidden. It
        // definitely won't be in the infoset in that case.
        val eventMaybe = state.inspectMaybe
        if (eventMaybe.isDefined && eventMaybe.get.erd == erd) {
          // Event existed for this OVC element, should be a start and end events
          val startEv = state.advanceOrError // Consume the start event
          Assert.invariant(startEv.isStart && startEv.erd == erd)
          val endEv = state.advanceOrError // Consume the end event
          Assert.invariant(endEv.isEnd && endEv.erd == erd)

          val e = startEv.asSimple
          // Remove any state that was set by what created this event. Later
          // code asserts that OVC elements do not have a value
          e.resetValue
          e
        } else {
          // Event was optional and didn't exist, create a new InfosetElement and add it
          val e = new DISimple(erd)
          state.currentInfosetNode.asComplex.addChild(e)
          e
        }
      } else {
        // Event was hidden and will never exist, create a new InfosetElement and add it
        val e = new DISimple(erd)
        state.currentInfosetNode.asComplex.addChild(e)
        e
      }

    val e = One(elem)
    state.currentInfosetNodeStack.push(e)
    state.aaa_currentNode = e
  }

  override def unparseEnd(state: UState) {
    // if an OVC element existed, the start AND end events were consumed in
    // unparseBegin. No need to advance the cursor here.
    state.aaa_currentNode =
      if (state.currentInfosetNodeStack.isEmpty)
        Nope
      else
        state.currentInfosetNodeStack.top

    move(state)
  }
}
