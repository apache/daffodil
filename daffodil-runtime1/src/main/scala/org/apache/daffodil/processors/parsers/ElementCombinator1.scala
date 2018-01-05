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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.infoset._
import org.apache.daffodil.dpath.DFDLCheckConstraintsFunction
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.dpath.DFDLCheckConstraintsFunction
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Processor
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.TermRuntimeData

abstract class ElementParserBase(
  rd: TermRuntimeData,
  name: String,
  patDiscrimParser: Maybe[Parser],
  patAssertParser: Array[Parser],
  setVarParser: Array[Parser],
  testDiscrimParser: Maybe[Parser],
  testAssertParser: Array[Parser],
  eBeforeParser: Maybe[Parser],
  eParser: Maybe[Parser],
  eAfterParser: Maybe[Parser])
  extends CombinatorParser(rd) {

  override lazy val runtimeDependencies = Nil

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
        pstate.validationError("%s failed facet checks due to: %s",
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
      if (pstate.processorStatus ne Success) {
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
        if (pstate.processorStatus ne Success) {
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

      if (pstate.processorStatus ne Success) return // but finally at the bottom will run!

      if (eBeforeParser.isDefined)
        eBeforeParser.get.parse1(pstate)

      if (pstate.processorStatus ne Success) return

      // We just successfully created the element in the infoset. Notify the
      // debugger of this so it can do things like check for break points
      if (pstate.dataProc.isDefined) pstate.dataProc.value.startElement(pstate, this)

      if (eParser.isDefined)
        eParser.get.parse1(pstate)

      Assert.invariant(pstate.hasInfoset)

      var setVarFailureDiags: Seq[Diagnostic] = Nil

      if (pstate.processorStatus eq Success) {
        var i: Int = 0
        while (i < setVarParser.length) {
          val d = setVarParser(i)
          i += 1
          d.parse1(pstate)
          if (pstate.processorStatus ne Success) {
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
        if (pstate.processorStatus ne Success) { return }
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
      if (pstate.processorStatus ne Success) { return }

      {
        var i = 0
        while (i < testAssertParser.length) {
          val d = testAssertParser(i)
          i += 1

          d.parse1(pstate)
          // Tests fail at the end of the Element
          if (pstate.processorStatus ne Success) { return }
        }
      }

      if (eAfterParser.isDefined)
        eAfterParser.get.parse1(pstate)

      if (pstate.processorStatus ne Success) return
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
    val currentElement = Infoset.newElement(erd, pstate.tunable).asInstanceOf[DIElement]

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

    if (pstate.processorStatus eq Success) {
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
    val currentElement = Infoset.newElement(erd, pstate.tunable)
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
