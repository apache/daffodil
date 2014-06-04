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


case class ComplexTypeCombinator(ct: ComplexTypeBase, body: Gram) extends Terminal(ct.element, !body.isEmpty) {

  def parser: DaffodilParser = new PrimParser(this, ct.element) {
    override def toString = "ComplexType"
    
    val bodyParser = body.parser

    override def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<ComplexType>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</ComplexType>"
    }

    def parse(start: PState): PState = {
      ChildIndexStack.get.push(1L)
      val parseState = bodyParser.parse1(start, ct)
      ChildIndexStack.get.pop()
      parseState
    }
  }

  def unparser: Unparser = new Unparser(ct.element) {
    override def toString = "ComplexType"

    val bodyUnparser = body.unparser

    def unparse(start: UState): UState = {
      ChildIndexStack.get.push(1L)
      val parseState = bodyUnparser.unparse(start)
      ChildIndexStack.get.pop()
      parseState
    }
  }
}

object ChildIndexStack {
  private val tl = new ThreadLocal[Stack[Long]] {
    override def initialValue = {
      val s = new Stack[Long]
      s.push(-1L)
      s
    }
  }

  def moveOverOneElementChildOnly = {
    val cis = tl.get
    Assert.usage(!cis.isEmpty)
    cis.push(cis.pop + 1)
  }

  def setup() {
    tl.get.clear
    tl.get.push(-1L)
  }

  def get = tl.get()
}

case class SequenceCombinator(sq: Sequence, body: Gram) extends Terminal(sq, !body.isEmpty) {

  def parser: DaffodilParser = new PrimParser(this, sq) {
    override def toString = "Sequence"

    val bodyParser = body.parser

    override def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<Sequence>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</Sequence>"
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
    override def toString = "Sequence"

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

  def setup() {
    tl.get.clear
    tl.get.push(-1L)
  }

  def get = tl.get()
}

case class ArrayCombinator(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = "Array"

    val bodyParser = body.parser

    override def toBriefXML(depthLimit: Int = -1): String = {
      if (depthLimit == 0) "..." else
        "<Array>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</Array>"
    }

    def parse(start: PState): PState = {

      ArrayIndexStack.get.push(1L)
      OccursBoundsStack.get.push(DaffodilTunableParameters.maxOccursBounds)

      val parseState = bodyParser.parse1(start, e)
      if (parseState.status != Success) return parseState

      val shouldValidate = SchemaComponentRegistry.getDataProc.getValidationMode != ValidationMode.Off

      val actualOccurs = ArrayIndexStack.get.pop()
      OccursBoundsStack.get.pop()

      val finalState = {
        if (shouldValidate) {
          e match {
            case led: LocalElementDecl => {
              val expectedMinOccurs = led.minOccurs
              val expectedMaxOccurs = led.maxOccurs
              val isUnbounded = expectedMaxOccurs == -1
              val occurrence = actualOccurs - 1
              val postValidationState = {
                if (isUnbounded && occurrence < expectedMinOccurs)
                  parseState.withValidationError("%s occurred '%s' times when it was expected to be a " +
                    "minimum of '%s' and a maximum of 'UNBOUNDED' times.", e,
                    occurrence, expectedMinOccurs)
                else if (!isUnbounded && (occurrence < expectedMinOccurs || occurrence > expectedMaxOccurs))
                  parseState.withValidationError("%s occurred '%s' times when it was expected to be a " +
                    "minimum of '%s' and a maximum of '%s' times.", e,
                    occurrence, expectedMinOccurs, expectedMaxOccurs)
                else
                  parseState
              }
              postValidationState
            }
          case _ => parseState
          }
        } else {
          parseState
        }
      }
      finalState
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "Array"

    val bodyUnparser = body.unparser

    def unparse(start: UState): UState = {
      val preState = start.withArrayIndexStack(1L :: start.arrayIndexStack)

      val unparseState = bodyUnparser.unparse(preState)
      if (unparseState.status != Success) return unparseState

      val postState = unparseState.withArrayIndexStack(unparseState.arrayIndexStack.tail)
      postState
    }
  }
}

object ArrayIndexStack {
  private val tl = new ThreadLocal[Stack[Long]] {
    override def initialValue = {
      val s = new Stack[Long]
      s.push(-1L)
      s
    }
  }
  def moveOverOneArrayIndexOnly = {
    val ais = tl.get
    ais.push(ais.pop + 1)
  }

  def setup() {
    tl.get.clear
    tl.get.push(-1L)
  }

  def get = tl.get()
}

object OccursBoundsStack {
  private val tl = new ThreadLocal[Stack[Long]] {
    override def initialValue = {
      val s = new Stack[Long]
      s.push(-1L)
      s
    }
  }

  def updateHead(ob: Long) = {
    val obs = tl.get
    obs.pop()
    obs.push(ob)
  }

  def setup() {
    tl.get.clear
    tl.get.push(-1L)
  }

  def get = tl.get()
}

