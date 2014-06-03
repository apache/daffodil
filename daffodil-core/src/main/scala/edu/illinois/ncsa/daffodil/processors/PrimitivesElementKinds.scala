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



/**
 * *
 * ChoiceElementBegin allows us to get away without adding the additional
 * choice element required of the unordered sequence transformation.
 *
 * This prevents us from having to worry about navigating around additional
 * choice elements in the infoset.  We effectively don't add them if they
 * were constructed as a result of an unordered sequence.
 */
case class ChoiceElementBegin(e: ElementBase) extends Terminal(e, true) {

  val isHidden = e.isHidden

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<ChoiceElementBegin name='" + e.name + "'/>"
    }

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start: PState): PState = {
      val currentElement = Infoset.newElement(e, isHidden)

      log(LogLevel.Debug, "currentElement = %s", currentElement)
      start
    }
  }

  def unparser = new DummyUnparser(e)
}

case class ElementBegin(e: ElementBase) extends Terminal(e, true) {

  val isHidden = e.isHidden

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<ElementBegin name='" + e.name + "'/>"
    }

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start: PState): PState = {
      val currentElement = Infoset.newElement(e, isHidden)

      log(LogLevel.Debug, "currentElement = %s", currentElement)
      val priorElement = start.infoset
      priorElement.addElement(currentElement)
      log(LogLevel.Debug, "priorElement = %s", priorElement)
      val postState = start.withParent(currentElement)
      postState
    }
  }

  def unparser = new DummyUnparser(e)
  //  def unparser: Unparser = new Unparser(e) {
  //    override def toString = "<" + e.name + ">"
  //
  //    /**
  //     * Changes the state to refer to the next element in the infoset as the element to unparse.
  //     */
  //    def unparse(start: UState): UState = {
  //      val nextElement = {
  //        //
  //        // TODO FIXME: THis can't be correct. The elementBegin shouldn't be writing out element contents.
  //        // That should happen in content unparsers. This unparser should just set things up so content
  //        // unparsers see the correct element to take the contents of. Which really means just changing the 
  //        // parent pointer in the UState.
  //        //
  //        // TODO: should not try/catch - should return a failed UState on error
  //        try { //if content contains elements
  //          if (!start.childIndexStack.isEmpty) {
  //            if (start.childPos != 1) { //if not first child, write unparsed result of previous child to outputStream
  //              //              val encoder = e.knownEncodingEncoder
  //              //              start.outStream.setEncoder(encoder)
  //              start.outStream.write()
  //            }
  //            start.currentElement.getContent().get(start.childPos.asInstanceOf[Int] - 1).asInstanceOf[org.jdom2.Element]
  //          } else {
  //            //            val encoder = e.knownEncodingEncoder
  //            //            start.outStream.setEncoder(encoder)
  //            start.currentElement.getContent().get(0).asInstanceOf[org.jdom2.Element]
  //          }
  //        } catch {
  //          case u: UnsuppressableException => throw u
  //          case e: Exception => start.currentElement //if content is text
  //        }
  //      }
  //
  //      start.withCurrent(nextElement).moveOverByOneElement
  //    }
  //  }
}

abstract class ElementEndBase(e: ElementBase) extends Terminal(e, true) {
  def toPrettyString = "</" + e.name + prettyStringModifier + ">"
  def prettyStringModifier: String

  def move(pstate: PState): PState // implement for different kinds of "moving over to next thing"
  def kindString = "ElementEnd"

  def validate(pstate: PState): PState = {
    val currentElement = pstate.parentElement
    val resultState = DFDLCheckConstraintsFunction.validate(pstate) match {
      case Right(boolVal) => {
        log(LogLevel.Debug, "Validation succeeded for %s", currentElement.toBriefXML)
        pstate // Success, do not mutate state.
      }
      case Left(failureMessage) => {
        log(LogLevel.Debug,
          "Validation failed for %s due to %s. The element value was %s.",
          e.toString, failureMessage, currentElement.toBriefXML)
        pstate.withValidationError("%s failed dfdl:checkConstraints due to %s",
          e.toString, failureMessage)
      }
    }

    resultState
  }

  def localParse(start: PState): PState = {
    val currentElement = start.parentElement

    val shouldValidate = start.dataProc.getValidationMode != ValidationMode.Off
    val postValidate =
      if (shouldValidate && e.isSimpleType) {
        // Execute checkConstraints
        val resultState = validate(start)
        resultState
      } else start

    // Assert.invariant(currentElement.getName() != "_document_" )
    val priorElement = currentElement.parent
    log(LogLevel.Debug, "priorElement = %s", priorElement)
    val postState = move(postValidate.withParent(priorElement))
    postState
  }

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<" + kindString + " name='" + e.name + "'/>"
    }

    override def toString = toPrettyString

    /**
     * ElementEnd just moves back to the parent element of the current one.
     */
    def parse(start: PState): PState = localParse(start)
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "</" + e.name + ">"

    /**
     * Changes state to refer to parent element of the current one.
     */
    def unparse(start: UState): UState = {
      val postState = {
        if (start.currentElement.getName != start.rootName) {
          val parent = start.currentElement.getParentElement()
          val state = start.withCurrent(parent)
          state
        } else {
          start
        }
      }
      postState
    }
  }
}

/**
 * *
 * ChoiceElementEnd allows us to get away without adding the additional
 * choice element required of the unordered sequence transformation.
 *
 * This prevents us from having to worry about navigating around additional
 * choice elements in the infoset.  We effectively don't add them if they
 * were constructed as a result of an unordered sequence.
 */
case class ChoiceElementEnd(e: ElementBase) extends ElementEndBase(e) {
  def move(pstate: PState) = pstate
  def prettyStringModifier = ""
  override def kindString = "ChoiceElementEnd"

  // We don't want to modify the state here except
  // for validation.
  override def localParse(start: PState): PState = {
    val currentElement = start.parentElement

    val shouldValidate = start.dataProc.getValidationMode != ValidationMode.Off
    val postValidate =
      if (shouldValidate && e.isSimpleType) {
        // Execute checkConstraints
        val resultState = validate(start)
        resultState
      } else start

    val priorElement = currentElement.parent
    log(LogLevel.Debug, "priorElement = %s", priorElement)
    val postState = move(postValidate)
    postState
  }
}

case class ElementEnd(e: ElementBase) extends ElementEndBase(e) {
  def move(pstate: PState) = pstate.moveOverByOneElement
  def prettyStringModifier = ""
}

case class ElementEndNoRep(e: ElementBase) extends ElementEndBase(e) {
  // if there is no rep (inputValueCalc), then we do create a new child so that index must advance,
  // but we don't create anything new as far as the group is concerned, and we don't want 
  // the group 'thinking' that there's a prior sibling inside the group and placing a 
  // separator after it. So in the case of NoRep, we don't advance group child, just element child.
  def move(pstate: PState) = pstate.moveOverOneElementChildOnly
  def prettyStringModifier = "(NoRep)"
}

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

