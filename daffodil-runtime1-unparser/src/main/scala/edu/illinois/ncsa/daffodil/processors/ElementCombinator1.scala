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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.dpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.dpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.debugger._
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.unparsers.InfosetEvent
import edu.illinois.ncsa.daffodil.processors.unparsers.End
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.processors.unparsers.Start

abstract class StatementElementUnparserBase(
  rd: RuntimeData,
  name: String,
  setVarUnparser: Seq[Unparser],
  eUnparser: Option[Unparser],
  eAfterUnparser: Option[Unparser])
  extends Unparser(rd) {

  override lazy val childProcessors: Seq[Processor] = setVarUnparser ++ eUnparser ++ eAfterUnparser

  def move(state: UState): Unit // implement for different kinds of "moving over to next thing"
  def unparseBegin(state: UState): Unit
  def unparseEnd(state: UState): Unit

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Element name='" + name + "'>" +
        eUnparser.map { _.toBriefXML(depthLimit - 1) }.getOrElse("") +
        setVarUnparser.map { _.toBriefXML(depthLimit - 1) }.mkString +
        eAfterUnparser.map { _.toBriefXML(depthLimit - 1) }.getOrElse("") +
        "</Element name='" + name + "'>"
  }

  def validate(state: UState): Unit = {
    ???
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
    //        state.withValidationError("%s failed dfdl:checkConstraints due to %s",
    //          context.toString, failureMessage)
    //        currentElement.setValid(false)
    //      }
    //    }
  }

  def unparse(state: UState): Unit = {

    unparseBegin(state)
    // Debugger.startElement(state, this)
    eUnparser.map { eUnparser =>
      eUnparser.unparse1(state, rd)
    }
    setVarUnparser.foreach(d => {
      d.unparse1(state, rd)
    })
    eAfterUnparser.foreach { eAfterUnparser =>
      eAfterUnparser.unparse1(state, rd)
    }
    unparseEnd(state)
  }
}

class StatementElementUnparser(
  erd: ElementRuntimeData,
  name: String,
  setVar: Seq[Unparser],
  eUnparser: Option[Unparser],
  eAfterUnparser: Option[Unparser])
  extends StatementElementUnparserBase(
    erd,
    name,
    setVar,
    eUnparser,
    eAfterUnparser) {

  def move(start: UState) {
    val grIndex = start.groupIndexStack.pop()
    start.groupIndexStack.push(grIndex + 1)
    val childIndex = start.childIndexStack.pop()
    start.childIndexStack.push(childIndex + 1)
  }

  def unparseBegin(state: UState): Unit = {
    val event: InfosetEvent = state.infosetSource.next()
    println(this)
    println(event)
    event match {
      case Start(infoElement: DIElement) => {
        Assert.invariant(infoElement.runtimeData == erd)
        state.currentInfosetNode = One(infoElement)
      }
      case Start(infoArr: DIArray) => ??? // Invariant failed? Should be an array combinator, not this element ?
      case _ => UnparseError(Nope, One(state), "Expected element start event, but received: %s", event)
    }
  }

  def unparseEnd(state: UState): Unit = {
    val event: InfosetEvent = state.infosetSource.next()
    event match {
      case End(infoElement: DIElement) => {
        Assert.invariant(infoElement.runtimeData == erd)
        state.currentInfosetNode = One(infoElement)
      }
      case _ => UnparseError(Nope, One(state), "Expected element end event, but received: %s", event)
    }
    move(state)
  }
}

class StatementElementUnparserNoRep(
  erd: ElementRuntimeData,
  name: String,
  setVar: Seq[Unparser],
  eUnparser: Option[Unparser],
  eAfterUnparser: Option[Unparser])
  extends StatementElementUnparser(
    erd,
    name,
    setVar,
    eUnparser,
    eAfterUnparser) {

  override lazy val childProcessors = setVar ++ eUnparser ++ eAfterUnparser

  // if there is no rep (inputValueCalc), then we do create a new child so that index must advance,
  // but we don't create anything new as far as the group is concerned, and we don't want 
  // the group 'thinking' that there's a prior sibling inside the group and placing a 
  // separator after it. So in the case of NoRep, we don't advance group child, just element child.
  override def move(state: UState) {
    val childIndex = state.childIndexStack.pop()
    state.childIndexStack.push(childIndex + 1)
  }
}

