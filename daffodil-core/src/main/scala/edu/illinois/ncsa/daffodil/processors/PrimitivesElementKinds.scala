package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.util.{ Debug, LogLevel, Logging, Info }
import edu.illinois.ncsa.daffodil.processors.xpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.grammar.Gram
import scala.collection.mutable.Stack
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar.UnaryGram

//case class ComplexElementEndPattern(e: ElementBase) extends Terminal(e, e.isComplexType == true && e.lengthKind == LengthKind.Pattern) {
//  // TODO: Should this be more generic; is there a way to detect state from the current element to tell us if it's time
//  //       to pop the input stack?
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//    override def toString = "</" + e.name + " dfdl:lengthKind='pattern'>"
//
//    /**
//     * ElementEnd just moves back to the parent element of the current one.
//     */
//    def parse(start: PState): PState = {
//      val currentElement = start.parentElement
//      log(LogLevel.Debug, "currentElement = %s", currentElement))
//      var priorElement = currentElement.parent
//      log(LogLevel.Debug, "priorElement = %s", priorElement))
//      val postState = start.withParent(priorElement).moveOverByOneElement.withLastInStream()
//      postState
//    }
//  }
//
//  def unparser: Unparser = new DummyUnparser(e)
//}

case class StartChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct.element, guard) {

  def parser: DaffodilParser = new PrimParser(this, ct.element) {
    override def toString = "StartChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }

  def unparser: Unparser = new Unparser(ct.element) {
    override def toString = "StartChildren"

    def unparse(start: UState): UState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }
}

class SequenceStartEnd(sq: Sequence, body: => Gram) extends Terminal(sq, !body.isEmpty) {

  def parser: DaffodilParser = new PrimParser(this, sq) {
    override def toString = "SequenceStartEnd"

    val bodyParser = body.parser

    override def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<SequenceStartEnd>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</SequenceStartEnd>"
    }

    def parse(start: PState): PState = {
      GroupIndexStack.get.push(1L)
      val parseState = bodyParser.parse1(start, context)
      GroupIndexStack.get.pop()
      GroupIndexStack.moveOverOneGroupIndexOnly()
      parseState
    }
  }

  def unparser: Unparser = new Unparser(sq) {
    override def toString = "SequenceStartEnd"

    def unparse(start: UState): UState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }
}

object GroupIndexStack {
  private val tl = new ThreadLocal[Stack[Long]] {
    override def initialValue = {
      val s = new Stack[Long]
      s.push(-1L)
      s
    }
  }
  
  def moveOverOneGroupIndexOnly() {
    val gis = GroupIndexStack.get
    Assert.usage(!gis.isEmpty)
    gis.push(gis.pop + 1)
  }

  def get = tl.get()
}

case class EndChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct.element, guard) {

  def parser: DaffodilParser = new PrimParser(this, ct.element) {
    override def toString = "EndChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }

  def unparser: Unparser = new Unparser(ct.element) {
    override def toString = "EndChildren"

    def unparse(start: UState): UState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }
}

//case class EndSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {
//
//  def parser: DaffodilParser = new PrimParser(this, sq) {
//    override def toString = "EndSequence"
//
//    def parse(start: PState): PState = {
//      // When we end a sequence group, we have created a group child in the parent
//      // so we advance that index. 
//      val postState = start.withGroupIndexStack(start.groupIndexStack.tail).moveOverOneGroupIndexOnly
//      postState
//    }
//  }
//
//  def unparser: Unparser = new Unparser(sq) {
//    override def toString = "EndSequence"
//
//    def unparse(start: UState): UState = {
//      val postState = start.withGroupIndexStack(start.groupIndexStack.tail).moveOverOneGroupIndexOnly
//      postState
//    }
//  }
//}

case class StartArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = "StartArray"

    def parse(start: PState): PState = {
      val postState1 = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      val postState2 = postState1.withOccursCountStack(DaffodilTunableParameters.occursCountMax :: postState1.occursCountStack)
      postState2
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "StartArray"

    def unparse(start: UState): UState = {
      val postState = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      postState
    }
  }
}

case class EndArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = "EndArray"

    def parse(start: PState): PState = {
      val shouldValidate = start.dataProc.getValidationMode != ValidationMode.Off
      val actualOccurs = start.arrayIndexStack.headOption
      val postState1 = start.withArrayIndexStack(start.arrayIndexStack.tail)
      val postState2 = postState1.withOccursCountStack(postState1.occursCountStack.tail)

      val finalState = {
        if (shouldValidate) {
          e match {
            case led: LocalElementDecl => {
              val expectedMinOccurs = led.minOccurs
              val expectedMaxOccurs = led.maxOccurs
              val isUnbounded = expectedMaxOccurs == -1
              val postValidationState = actualOccurs match {
                case Some(o) => {
                  val occurrence = o - 1
                  val result =
                    if (isUnbounded && occurrence < expectedMinOccurs)
                      start.withValidationError("%s occurred '%s' times when it was expected to be a " +
                        "minimum of '%s' and a maximum of 'UNBOUNDED' times.", e,
                        occurrence, expectedMinOccurs)
                    else if (!isUnbounded && (occurrence < expectedMinOccurs || occurrence > expectedMaxOccurs))
                      start.withValidationError("%s occurred '%s' times when it was expected to be a " +
                        "minimum of '%s' and a maximum of '%s' times.", e,
                        occurrence, expectedMinOccurs, expectedMaxOccurs)
                    else
                      postState2

                  result
                }
                case None => start.withValidationError("No occurrence found for %s when it was expected to be a " +
                  "minimum of '%s' times and a maximum of '%s' times.", e,
                  expectedMinOccurs, if (isUnbounded) "UNBOUNDED" else expectedMaxOccurs)
              }
              postValidationState
            }
            case _ => postState2
          }
        } else postState2
      }
      finalState
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "EndArray"

    def unparse(start: UState): UState = {
      val postState = start.withArrayIndexStack(start.arrayIndexStack.tail)
      postState
    }
  }
}

