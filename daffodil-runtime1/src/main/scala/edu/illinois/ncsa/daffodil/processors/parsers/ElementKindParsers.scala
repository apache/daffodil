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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.Processor
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.dsom.EntityReplacer
import edu.illinois.ncsa.daffodil.processors.EvaluatesStaticDynamicTextParser
import edu.illinois.ncsa.daffodil.processors.DelimiterStackNode
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeParserHelper
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeFactoryDynamic
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeFactoryStatic
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.dsom.EscapeSchemeObject
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeCharParserHelper
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlockParserHelper
import edu.illinois.ncsa.daffodil.exceptions.Assert

class ComplexTypeParser(rd: RuntimeData, bodyParser: Parser)
  extends Parser(rd) {
  override def nom = "ComplexType"

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {
    start.mpstate.childIndexStack.push(1L) // one-based indexing
    bodyParser.parse1(start)
    start.mpstate.childIndexStack.pop()
    ()
  }
}

/**
 * This parser should only ever be called when delimiters exist.
 *
 * The purpose of this parser is to create/evaluate delimiter DFAs
 * and push them to the delimiter stack (bring them in scope) for
 * subsequent (internal/body) parse steps.  Then on the way out pop
 * the delimiter DFAs (bring them out of scope) after
 * the internal/body parser has completed.
 */
class DelimiterStackParser(initiatorOpt: Option[CompiledExpression],
  separatorOpt: Option[CompiledExpression],
  terminatorOpt: Option[CompiledExpression],
  initiatorLoc: (String, String),
  separatorLocOpt: Option[(String, String)],
  terminatorLoc: (String, String),
  isLengthKindDelimited: Boolean,
  rd: RuntimeData, bodyParser: Parser)
  extends PrimParser(rd)
  with EvaluatesStaticDynamicTextParser {

  private def unlessEmptyString(ce: Option[CompiledExpression]): Option[CompiledExpression] =
    ce.flatMap { ce => if (ce.isConstant && ce.constantAsString == "") None else Some(ce) }

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else {
      "<DelimiterStack" +
        unlessEmptyString(initiatorOpt).map { i => " initiator='" + i + "'" }.getOrElse("") +
        unlessEmptyString(separatorOpt).map { s => " separator='" + s + "'" }.getOrElse("") +
        unlessEmptyString(terminatorOpt).map { t => " terminator='" + t + "'" }.getOrElse("") + ">" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</DelimiterStack>"
    }
  }

  val (staticInits, dynamicInits) = getStaticAndDynamicText(initiatorOpt, context)
  val (staticSeps, dynamicSeps) = getStaticAndDynamicText(separatorOpt, context, isLengthKindDelimited)
  val (staticTerms, dynamicTerms) = getStaticAndDynamicText(terminatorOpt, context, isLengthKindDelimited)

  def parse(start: PState): Unit = {

    // Evaluate Delimiters
    val dynamicInitsSeq = evaluateDynamicText(dynamicInits, start, context, false)
    val dynamicSepsSeq = evaluateDynamicText(dynamicSeps, start, context, isLengthKindDelimited)
    val dynamicTermsSeq = evaluateDynamicText(dynamicTerms, start, context, isLengthKindDelimited)

    val allInits: Seq[DFADelimiter] = combineStaticAndDynamic(staticInits, dynamicInitsSeq)
    val allSeps: Seq[DFADelimiter] = combineStaticAndDynamic(staticSeps, dynamicSepsSeq)
    val allTerms: Seq[DFADelimiter] = combineStaticAndDynamic(staticTerms, dynamicTermsSeq)

    val node = DelimiterStackNode(allInits,
      allSeps,
      allTerms,
      { if (allInits.isEmpty) Nope else One(initiatorLoc) },
      separatorLocOpt,
      { if (allTerms.isEmpty) Nope else One(terminatorLoc) })

    // Push Delimiters
    start.mpstate.pushDelimiters(node)

    // Parse
    bodyParser.parse1(start)

    // Pop Delimiters
    start.mpstate.popDelimiters
    start.clearDelimitedText
    ()
  }
}

/**
 * *
 * This parser should only ever be called when an escape scheme exists.
 *
 * Evaluates the escape scheme and brings it in and out of scope.
 */
class EscapeSchemeStackParser(escapeScheme: EscapeSchemeObject,
  rd: RuntimeData, bodyParser: Parser)
  extends Parser(rd)
  with EvaluatesStaticDynamicTextParser {

  override lazy val childProcessors = Seq(bodyParser)

  val scheme = {
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
      val theScheme = {
        if (isConstant) EscapeSchemeFactoryStatic(escapeScheme, rd)
        else EscapeSchemeFactoryDynamic(escapeScheme, rd)
      }
      theScheme
    }
  }

  def parse(start: PState): Unit = {
    // Evaluate
    val escScheme = scheme.getEscapeSchemeParser(start)

    // Set Escape Scheme
    start.mpstate.currentEscapeScheme = One(escScheme)

    // Parse
    bodyParser.parse1(start)

    // Clear EscapeScheme
    start.mpstate.currentEscapeScheme = Nope
  }
}

class EscapeSchemeNoneStackParser(
  rd: RuntimeData, bodyParser: Parser)
  extends Parser(rd) {

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {

    // Clear EscapeScheme
    start.mpstate.currentEscapeScheme = Nope

    // Parse
    bodyParser.parse1(start)

    // Clear EscapeScheme
    start.mpstate.currentEscapeScheme = Nope
  }
}

class SequenceCombinatorParser(rd: RuntimeData, bodyParser: Parser)
  extends Parser(rd) {
  override def nom = "Sequence"

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {
    start.mpstate.groupIndexStack.push(1L) // one-based indexing

    bodyParser.parse1(start)

    start.mpstate.groupIndexStack.pop()
    start.mpstate.moveOverOneGroupIndexOnly()
    ()
  }
}

/**
 * This is essentially just a wrapper around the bodyParser, which is an
 * AltCompParser. This is only here to maintain symmetry with the unparse side,
 * which has a more complicated unparser that differs from an AltCompUnparser.
 */
class ChoiceCombinatorParser(rd: RuntimeData, bodyParser: Parser)
  extends Parser(rd) {
  override def nom = "Choice"

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {
    bodyParser.parse1(start)
  }
}

class ArrayCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser) extends Parser(erd) {
  override def nom = "Array"
  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {

    start.mpstate.arrayIndexStack.push(1L) // one-based indexing
    start.mpstate.occursBoundsStack.push(DaffodilTunableParameters.maxOccursBounds)

    bodyParser.parse1(start)
    if (start.status != Success) return

    val shouldValidate = start.dataProc.getValidationMode != ValidationMode.Off

    val actualOccurs = start.mpstate.arrayIndexStack.pop()
    start.mpstate.occursBoundsStack.pop()

    (erd.minOccurs, erd.maxOccurs) match {
      case (Some(minOccurs), Some(maxOccurs)) if shouldValidate => {
        val isUnbounded = maxOccurs == -1
        val occurrence = actualOccurs - 1

        if (isUnbounded && occurrence < minOccurs)
          start.withValidationError("%s occurred '%s' times when it was expected to be a " +
            "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.prettyName,
            occurrence, minOccurs)
        else if (!isUnbounded && (occurrence < minOccurs || occurrence > maxOccurs))
          start.withValidationError("%s occurred '%s' times when it was expected to be a " +
            "minimum of '%s' and a maximum of '%s' times.", erd.prettyName,
            occurrence, minOccurs, maxOccurs)
        else {
          //ok
        }
      }
      case _ => //ok
    }
  }
}

// This follows the same behavior as Arrays for parsing
class OptionalCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser) extends ArrayCombinatorParser(erd, bodyParser) {
  override def nom = "Optional"
}

