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

package org.apache.daffodil.runtime1.processors.parsers

import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.iapi.ValidationMode
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.dpath.DFDLCheckConstraintsFunction
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Processor
import org.apache.daffodil.runtime1.processors.Success

abstract class ElementParserBase(
  erd: ElementRuntimeData,
  name: String,
  patDiscrimParser: Maybe[Parser],
  patAssertParser: Array[Parser],
  setVarParser: Array[Parser],
  testDiscrimParser: Maybe[Parser],
  testAssertParser: Array[Parser],
  eBeforeParser: Maybe[Parser],
  eParser: Maybe[Parser],
  eAfterParser: Maybe[Parser],
  eRepTypeParser: Maybe[Parser]
) extends CombinatorParser(erd) {

  override def runtimeDependencies = Vector()

  def move(pstate: PState): Unit // implement for different kinds of "moving over to next thing"
  def parseBegin(pstate: PState): Unit
  def parseEnd(pstate: PState): Unit

  override def childProcessors: Vector[Processor] = (patDiscrimParser.toSeq ++
    patAssertParser ++
    eBeforeParser.toSeq ++
    eParser.toSeq ++
    setVarParser ++
    testDiscrimParser.toSeq ++
    testAssertParser ++
    eAfterParser.toSeq ++
    eRepTypeParser.toSeq).toVector

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else
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
        Logger.log.debug(s"Validation succeeded for ${currentElement.namedQName}")
        currentElement.setValid(true)
        pstate // Success, do not mutate state.
      } else {
        val failureMessage = ccfResult.errMsg
        Logger.log.debug(
          "Validation failed for ${context.toString} due to ${failureMessage}. The element was ${currentElement.namedQName}."
        )
        pstate.validationError(
          "%s failed facet checks due to: %s",
          context.toString,
          failureMessage
        )
        currentElement.setValid(false)
        pstate
      }
    }
    resultState
  }

  def parse(pstate: PState): Unit = {

    // Note that pattern discriminators and asserts do not advance the position
    // in the input stream, so there is no need to mark/reset the inputstream
    if (patDiscrimParser.isDefined) {
      patDiscrimParser.get.parse1(pstate)
      if (pstate.processorStatus ne Success) {
        return
      }
    } else if (patAssertParser.length > 0) {
      var i: Int = 0
      val size = patAssertParser.size
      while (i < size) {
        val d = patAssertParser(i)
        d.parse1(pstate)
        if (pstate.processorStatus ne Success) {
          return
        }
        i += 1
      }
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

      if (eRepTypeParser.isDefined) {
        eRepTypeParser.get.parse1(pstate)
      } else if (eParser.isDefined) {
        eParser.get.parse1(pstate)
      }

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

      // if we should do limited validation via CheckConstraints
      val shouldValidate = erd.isSimpleType &&
        pstate.dataProc.isDefined &&
        pstate.dataProc.value.validationMode == ValidationMode.Limited

      if (shouldValidate) {
        validate(pstate)
      }

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
  eAfterParser: Maybe[Parser],
  eRepTypeParser: Maybe[Parser]
) extends ElementParserBase(
    erd,
    name,
    patDiscrim,
    patAssert,
    setVar,
    testDiscrim,
    testAssert,
    eBeforeParser,
    eParser,
    eAfterParser,
    eRepTypeParser
  ) {

  def move(start: PState): Unit = {
    start.mpstate.moveOverOneElementChildOnly()
    ()
  }

  def parseBegin(pstate: PState): Unit = {
    val currentElement = Infoset.newElement(erd).asInstanceOf[DIElement]
    if (pstate.withinHiddenNest)
      currentElement.setHidden()

    Logger.log.debug(s"currentElement = ${currentElement}")
    val priorElement = pstate.infoset
    priorElement match {
      case ct: DIComplex => ct.addChild(currentElement, pstate.tunable)
      case st: DISimple => {
        // don't add as a child. This corner case
        // comes up with QuasiElements, which do not actually get inserted into the infoset
        // The under such cases, we don't particuarly care about keeping the infoset clean (as the caller will revert changes anyway),
        // but we do need to set the QuasiElements parent to maintain the invariant.
        // It also comes up in tests where the root node is
        // a simple element.
        currentElement.setParent(st.parent)
      }
    }
    Logger.log.debug(s"priorElement = ${priorElement}")
    pstate.setInfoset(currentElement, Nope)
  }

  def parseEnd(pstate: PState): Unit = {
    val currentElement = pstate.infoset
    val priorElement = currentElement.diParent

    // set the context back to the parent infoset element if we have one. We might not have one
    // if we are the root element.
    if (priorElement ne null) pstate.setInfoset(priorElement, One(currentElement))

    if (pstate.processorStatus eq Success) {
      move(pstate)
    } else {
      // nothing to be done here. Note that even though we failed we do not need to remove this
      // new element that we created. That is done at the point of uncertainty when it restores
      // the state of the infoset after a failure.
    }
  }
}

class ElementParserInputValueCalc(
  erd: ElementRuntimeData,
  name: String,
  patDiscrim: Maybe[Parser],
  patAssert: Array[Parser],
  setVar: Array[Parser],
  testDiscrim: Maybe[Parser],
  testAssert: Array[Parser],
  eBeforeParser: Maybe[Parser],
  eParser: Maybe[Parser],
  eAfterParser: Maybe[Parser]
) extends ElementParser(
    erd,
    name,
    patDiscrim,
    patAssert,
    setVar,
    testDiscrim,
    testAssert,
    eBeforeParser,
    eParser,
    eAfterParser,
    Maybe.Nope
  ) {

  // if there is no rep (inputValueCalc), then we do create a new child so that index must advance,
  // but we don't create anything new as far as the group is concerned, and we don't want
  // the group 'thinking' that there's a prior sibling inside the group and placing a
  // separator after it. So in the case of InputValueCalc, we don't advance group child, just element child.
  override def move(state: PState): Unit = {
    state.mpstate.moveOverOneElementChildOnly()
  }
}
