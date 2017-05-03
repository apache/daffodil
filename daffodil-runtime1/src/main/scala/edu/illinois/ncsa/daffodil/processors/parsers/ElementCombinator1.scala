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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.infoset._
import edu.illinois.ncsa.daffodil.dpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.dpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.Processor
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.Success

abstract class ElementParserBase(
  rd: RuntimeData,
  name: String,
  patDiscrimParser: Maybe[Parser],
  patAssertParser: Array[Parser],
  setVarParser: Array[Parser],
  testDiscrimParser: Maybe[Parser],
  testAssertParser: Array[Parser],
  eBeforeParser: Maybe[Parser],
  eParser: Maybe[Parser],
  eAfterParser: Maybe[Parser])
  extends ParserObject(rd) {

  def move(pstate: PState): Unit // implement for different kinds of "moving over to next thing"
  def parseBegin(pstate: PState): Unit
  def parseEnd(pstate: PState): Unit

  override lazy val childProcessors: Seq[Processor] = patDiscrimParser.toSeq ++
    patAssertParser ++
    eBeforeParser.toSeq ++
    eParser.toSeq ++
    setVarParser ++
    testDiscrimParser.toSeq ++
    testAssertParser ++
    eAfterParser.toSeq

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Element name='" + name + "'>" +
        childProcessors.map { _.toBriefXML(depthLimit - 1) }.mkString +
        "</Element>"
  }

  def validate(pstate: PState): PState = {
    val currentElement = pstate.thisElement

    if (currentElement.valid.isDefined) { return pstate }

    val resultState = {
      val ccfResult = DFDLCheckConstraintsFunction.validate(pstate)
      if (ccfResult.isOK) {
        log(LogLevel.Debug, "Validation succeeded for %s", currentElement.namedQName)
        currentElement.setValid(true)
        pstate // Success, do not mutate state.
      } else {
        val failureMessage = ccfResult.errMsg
        log(LogLevel.Debug,
          "Validation failed for %s due to %s. The element was %s.",
          context.toString, failureMessage, currentElement.namedQName)
        pstate.reportValidationError("%s failed facet checks due to: %s",
          context.toString, failureMessage)
        currentElement.setValid(false)
        pstate
      }
    }
    resultState
  }

  def parse(pstate: PState): Unit = {

    if (patDiscrimParser.isDefined) {
      val startingBitPos = pstate.dataInputStream.mark("ElementParserBase1")
      patDiscrimParser.get.parse1(pstate)
      // Pattern fails at the start of the Element
      if (pstate.status ne Success) {
        pstate.dataInputStream.discard(startingBitPos)
        return
      } else {
        pstate.dataInputStream.reset(startingBitPos)
      }
    } else if (patAssertParser.length > 0) {
      val startingBitPos = pstate.dataInputStream.mark("ElementParserBase2")
      var i: Int = 0
      val size = patAssertParser.size
      while (i < size) {
        val d = patAssertParser(i)
        d.parse1(pstate)
        // Pattern fails at the start of the Element
        if (pstate.status ne Success) {
          pstate.dataInputStream.discard(startingBitPos)
          return
        }
        i += 1
      }
      // backup again. If all pattern discriminators and/or asserts
      // have passed, now we parse the element. But we backup
      // as if the pattern matching had not advanced the state.
      pstate.dataInputStream.reset(startingBitPos)
    }

    parseBegin(pstate)

    try {
      // TODO: Performance/Maintainability - get rid of use of return statements.

      if (pstate.status ne Success) return // but finally at the bottom will run!

      if (eBeforeParser.isDefined)
        eBeforeParser.get.parse1(pstate)

      if (pstate.status ne Success) return

      // We just successfully created the element in the infoset. Notify the
      // debugger of this so it can do things like check for break points
      if (pstate.dataProc.isDefined) pstate.dataProc.value.startElement(pstate, this)

      if (eParser.isDefined)
        eParser.get.parse1(pstate)

      Assert.invariant(pstate.hasInfoset)

      var setVarFailureDiags: Seq[Diagnostic] = Nil

      if (pstate.status eq Success) {
        var i: Int = 0
        while (i < setVarParser.length) {
          val d = setVarParser(i)
          i += 1
          d.parse1(pstate)
          if (pstate.status ne Success) {
            setVarFailureDiags = pstate.diagnostics
            // a setVariable statement may fail. But we want to continue to try
            // more of the setVariable statements, as they may be necessary
            // to evaluate the test discriminator below, and some might
            // be successful even if one fails, allowing the discriminator to be true.
            //
            // So it's a bit odd, but we're going to just keep parsing using this
            // failed state as the input to the next setVariable parse step.
          }
        }
      }

      if (testDiscrimParser.isDefined) {
        testDiscrimParser.get.parse1(pstate)
        // Tests fail at the end of the Element
        if (pstate.status ne Success) { return }
      }

      //
      // We're done with the discriminator, so now we revisit the set variable statements.
      // If a failure occurred there, then now we can fail out right here.
      //
      if (!setVarFailureDiags.isEmpty) {
        pstate.setFailed(setVarFailureDiags.head)
        return
      }

      // Element evaluation failed, return
      if (pstate.status ne Success) { return }

      {
        var i = 0
        while (i < testAssertParser.length) {
          val d = testAssertParser(i)
          i += 1

          d.parse1(pstate)
          // Tests fail at the end of the Element
          if (pstate.status ne Success) { return }
        }
      }

      if (eAfterParser.isDefined)
        eAfterParser.get.parse1(pstate)

      if (pstate.status ne Success) return
    } finally {
      parseEnd(pstate)
      if (pstate.dataProc.isDefined) pstate.dataProc.value.endElement(pstate, this)
    }
  }
}

class ElementParser(
  erd: ElementRuntimeData,
  name: String,
  patDiscrim: Maybe[Parser],
  patAssert: Array[Parser],
  setVar: Array[Parser],
  testDiscrim: Maybe[Parser],
  testAssert: Array[Parser],
  eBeforeParser: Maybe[Parser],
  eParser: Maybe[Parser],
  eAfterParser: Maybe[Parser])
  extends ElementParserBase(
    erd,
    name,
    patDiscrim,
    patAssert,
    setVar,
    testDiscrim,
    testAssert,
    eBeforeParser,
    eParser,
    eAfterParser) {

  def move(start: PState) {
    start.mpstate.moveOverOneGroupIndexOnly
    start.mpstate.moveOverOneElementChildOnly
    ()
  }

  def parseBegin(pstate: PState): Unit = {
    val currentElement = Infoset.newElement(erd).asInstanceOf[DIElement]

    log(LogLevel.Debug, "currentElement = %s", currentElement)
    val priorElement = pstate.infoset
    priorElement match {
      case ct: DIComplex => ct.addChild(currentElement)
      case st: DISimple => {
        // don't add as a child. This corner case
        // is just about tests where the root node is
        // a simple element.
      }
    }
    log(LogLevel.Debug, "priorElement = %s", priorElement)
    pstate.setParent(currentElement)
  }

  def parseEnd(pstate: PState): Unit = {
    val currentElement = pstate.infoset
    val priorElement = currentElement.diParent

    if (pstate.status eq Success) {
      val shouldValidate =
        (pstate.dataProc.isDefined) && pstate.dataProc.value.getValidationMode != ValidationMode.Off
      if (shouldValidate && erd.isSimpleType) {
        // Execute checkConstraints
        validate(pstate)
      }
      if (priorElement ne null) pstate.setParent(priorElement)
      move(pstate)
    } else { // failure.
      if (priorElement ne null) {
        // We set the context back to the parent infoset element here
        // But we do not remove the child here. That's done at the
        // point of uncertainty when it restores the state of the
        // element after a failure.
        pstate.setParent(priorElement)
      }
    }
  }
}

class ElementParserNoRep(
  erd: ElementRuntimeData,
  name: String,
  patDiscrim: Maybe[Parser],
  patAssert: Array[Parser],
  setVar: Array[Parser],
  testDiscrim: Maybe[Parser],
  testAssert: Array[Parser],
  eBeforeParser: Maybe[Parser],
  eParser: Maybe[Parser],
  eAfterParser: Maybe[Parser])
  extends ElementParser(
    erd,
    name,
    patDiscrim,
    patAssert,
    setVar,
    testDiscrim,
    testAssert,
    eBeforeParser,
    eParser,
    eAfterParser) {

  // if there is no rep (inputValueCalc), then we do create a new child so that index must advance,
  // but we don't create anything new as far as the group is concerned, and we don't want
  // the group 'thinking' that there's a prior sibling inside the group and placing a
  // separator after it. So in the case of NoRep, we don't advance group child, just element child.
  override def move(state: PState) {
    state.mpstate.moveOverOneElementChildOnly
  }
}

class ChoiceElementParser(
  erd: ElementRuntimeData,
  name: String,
  patDiscrim: Maybe[Parser],
  patAssert: Array[Parser],
  setVar: Array[Parser],
  testDiscrim: Maybe[Parser],
  testAssert: Array[Parser],
  eBeforeParser: Maybe[Parser],
  eParser: Maybe[Parser],
  eAfterParser: Maybe[Parser])
  extends ElementParserBase(
    erd,
    name,
    patDiscrim,
    patAssert,
    setVar,
    testDiscrim,
    testAssert,
    eBeforeParser,
    eParser,
    eAfterParser) {

  def move(state: PState) = {}

  /**
   * ElementBegin just adds the element we are constructing to the infoset and changes
   * the state to be referring to this new element as what we're parsing data into.
   */
  def parseBegin(pstate: PState): Unit = {
    val currentElement = Infoset.newElement(erd)
    log(LogLevel.Debug, "currentElement = %s", currentElement)
  }

  // We don't want to modify the state here except
  // for validation.
  def parseEnd(pstate: PState): Unit = {
    val currentElement = pstate.thisElement

    val shouldValidate =
      (pstate.dataProc.isDefined) && pstate.dataProc.value.getValidationMode != ValidationMode.Off
    if (shouldValidate && erd.isSimpleType) {
      // Execute checkConstraints
      validate(pstate)
    }

    val priorElement = currentElement.parent
    // Note: interaction of unboxed Maybe[T] with pass by name args of log method
    // require us to call toScalaOption here.
    log(LogLevel.Debug, "priorElement = %s", Maybe.WithNulls.toScalaOption(priorElement))
    move(pstate)
  }
}
