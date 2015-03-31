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
import edu.illinois.ncsa.daffodil.dsom.EscapeSchemeObject
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.dsom.EscapeSchemeObject
import edu.illinois.ncsa.daffodil.xml.QNameBase
import edu.illinois.ncsa.daffodil.xml.QName

class ComplexTypeUnparser(rd: RuntimeData, bodyUnparser: Unparser)
  extends Unparser(rd) {
  override def nom = "ComplexType"

  override lazy val childProcessors = Seq(bodyUnparser)

  def unparse(start: UState): Unit = {
    start.currentInfosetNodeStack.push(Nope) // save
    start.childIndexStack.push(1L) // one-based indexing
    bodyUnparser.unparse1(start, rd)
    start.childIndexStack.pop()
    start.currentInfosetNodeStack.pop // restore
  }
}

class SequenceCombinatorUnparser(rd: RuntimeData, bodyUnparser: Unparser)
  extends Unparser(rd) {
  override def nom = "Sequence"

  override lazy val childProcessors: Seq[Processor] = Seq(bodyUnparser)

  def unparse(start: UState): Unit = {
    start.groupIndexStack.push(1L) // one-based indexing
    bodyUnparser.unparse1(start, rd)
    start.groupIndexStack.pop()
    start.moveOverOneGroupIndexOnly()
  }
}

class ChoiceCombinatorUnparser(rd: RuntimeData, qnameUnparserMap: Map[QNameBase, Unparser])
  extends Unparser(rd)
  with ToBriefXMLImpl {
  override def nom = "Choice"

  override lazy val childProcessors: Seq[Processor] = qnameUnparserMap.map { case (k, v) => v }.toSeq

  def unparse(start: UState): Unit = {
    val mgrd = rd.asInstanceOf[ModelGroupRuntimeData]

    val event: InfosetEvent = start.peek
    val eventQName = event match {
      case Start(simple: DISimple) => simple.runtimeData.namedQName
      case Start(complex: DIComplex) => complex.runtimeData.namedQName
      case Start(array: DIArray) => array.namedQName
      case _ => UnparseError(Nope, One(start), "Expected element start event, but received: %s", event)
    }

    val childUnparser = qnameUnparserMap(eventQName)
    childUnparser.unparse1(start, rd)
  }
}


class DelimiterStackUnparser(outputNewLine: CompiledExpression,
  initiatorOpt: Option[CompiledExpression],
  separatorOpt: Option[CompiledExpression],
  terminatorOpt: Option[CompiledExpression],
  initiatorLoc: (String, String),
  separatorLocOpt: Option[(String, String)],
  terminatorLoc: (String, String),
  isLengthKindDelimited: Boolean,
  rd: RuntimeData,
  bodyUnparser: Unparser)
  extends Unparser(rd) with EvaluatesStaticDynamicTextUnparser {
  override def nom = "DelimiterStack"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<DelimiterStack initiator='" + initiatorOpt +
        "' separator='" + separatorOpt +
        "' terminator='" + terminatorOpt + "'>" +
        bodyUnparser.toBriefXML(depthLimit - 1) +
        "</DelimiterStack>"
  }

  val (staticInits, dynamicInits) = getStaticAndDynamicText(initiatorOpt, outputNewLine, context)
  val (staticSeps, dynamicSeps) = getStaticAndDynamicText(separatorOpt, outputNewLine, context, isLengthKindDelimited)
  val (staticTerms, dynamicTerms) = getStaticAndDynamicText(terminatorOpt, outputNewLine, context, isLengthKindDelimited)

  override lazy val childProcessors: Seq[Processor] = Seq(bodyUnparser)

  def unparse(start: UState): Unit = {
    // Evaluate Delimiters
    val init = if (staticInits.isDefined) staticInits else evaluateDynamicText(dynamicInits, outputNewLine, start, context, false)
    val sep = if (staticSeps.isDefined) staticSeps else evaluateDynamicText(dynamicSeps, outputNewLine, start, context, isLengthKindDelimited)
    val term = if (staticTerms.isDefined) staticTerms else evaluateDynamicText(dynamicTerms, outputNewLine, start, context, isLengthKindDelimited)

    val node = DelimiterStackUnparseNode(init,
      sep,
      term,
      { if (!init.isDefined) Nope else One(initiatorLoc) },
      separatorLocOpt,
      { if (!term.isDefined) Nope else One(terminatorLoc) })

    start.pushDelimiters(node)

    bodyUnparser.unparse1(start, rd)

    start.popDelimiters
  }
}

class EscapeSchemeStackUnparser(escapeScheme: EscapeSchemeObject, rd: RuntimeData, bodyUnparser: Unparser)
  extends Unparser(rd) {
  override def nom = "EscapeSchemeStack"

  override lazy val childProcessors: Seq[Processor] = Seq(bodyUnparser)

  val scheme =
    {
      val isConstant = escapeScheme.escapeKind match {
        case EscapeKind.EscapeBlock => {
          (escapeScheme.optionEscapeEscapeCharacter.isEmpty ||
            escapeScheme.optionEscapeEscapeCharacter.get.isConstant)
        }
        case EscapeKind.EscapeCharacter => {
          (escapeScheme.optionEscapeCharacter.isEmpty ||
            escapeScheme.optionEscapeCharacter.get.isConstant) &&
            (escapeScheme.optionEscapeEscapeCharacter.isEmpty ||
              escapeScheme.optionEscapeEscapeCharacter.get.isConstant)
        }
      }
      val theScheme: EscapeSchemeFactoryBase = {
        if (isConstant) EscapeSchemeFactoryStatic(escapeScheme, rd)
        else EscapeSchemeFactoryDynamic(escapeScheme, rd)
      }
      theScheme
    }

  def unparse(start: UState): Unit = {
    // Evaluate
    val (afterEval, escScheme) = {
      val (afterDynamicEval, evaluatedScheme) = scheme.getEscapeSchemeUnparser(start)

      (afterDynamicEval, evaluatedScheme)
    }

    // Set Escape Scheme
    start.currentEscapeScheme = One(escScheme)

    // Unparse
    bodyUnparser.unparse1(start, rd)

    // Clear EscapeScheme
    start.currentEscapeScheme = Nope
  }
}

class EscapeSchemeNoneStackUnparser(
  rd: RuntimeData, bodyUnparser: Unparser)
  extends Unparser(rd) {

  override def nom = "EscapeSchemeStack"

  override lazy val childProcessors = Seq(bodyUnparser)

  def unparse(start: UState): Unit = {

    // Clear Escape Scheme
    start.currentEscapeScheme = Nope

    // Unparse
    bodyUnparser.unparse1(start, rd)

    // Clear EscapeScheme
    start.currentEscapeScheme = Nope

  }
}
class ArrayCombinatorUnparser(erd: ElementRuntimeData, bodyUnparser: Unparser) extends Unparser(erd) {
  override def nom = "Array"
  override lazy val childProcessors = Seq(bodyUnparser)

  def unparse(ustate: UState) {

    ustate.arrayIndexStack.push(1L) // one-based indexing
    ustate.occursBoundsStack.push(DaffodilTunableParameters.maxOccursBounds)

    if (!ustate.hasNext) UE(ustate, "Malformed event stream for array: No more events were found.")
    var ev = ustate.peek
    if (ev.isInstanceOf[Start] && ev.node.isInstanceOf[DIArray]) {
      ustate.next()
      bodyUnparser.unparse1(ustate, erd)
      if (!ustate.hasNext) UE(ustate, "Needed End Array Infoset Event, but no more events were found.")
      ev = ustate.next()
      if (!(ev.isInstanceOf[End] && ev.node.isInstanceOf[DIArray]))
        UE(ustate, "Needed End Array Infoset Event, but found %s.", ev)
    } else {
      // case of array with no elements at all; hence, no DIArray events
      // all we have done is peek ahead one event, so we haven't disturbed
      // the infosetSource
    }
    val shouldValidate = ustate.dataProc.getValidationMode != ValidationMode.Off

    val actualOccurs = ustate.arrayIndexStack.pop()
    ustate.occursBoundsStack.pop()

    (erd.minOccurs, erd.maxOccurs) match {
      case (Some(minOccurs), Some(maxOccurs)) if shouldValidate => {
        val isUnbounded = maxOccurs == -1
        val occurrence = actualOccurs - 1
        if (isUnbounded && occurrence < minOccurs)
          ustate.validationError("%s occurred '%s' times when it was expected to be a " +
            "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.prettyName,
            occurrence, minOccurs)
        else if (!isUnbounded && (occurrence < minOccurs || occurrence > maxOccurs))
          ustate.validationError("%s occurred '%s' times when it was expected to be a " +
            "minimum of '%s' and a maximum of '%s' times.", erd.prettyName,
            occurrence, minOccurs, maxOccurs)
      }
      case _ => // ok
    }
  }
}
