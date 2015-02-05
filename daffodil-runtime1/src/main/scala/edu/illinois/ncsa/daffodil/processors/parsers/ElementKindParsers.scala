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
import edu.illinois.ncsa.daffodil.processors.EvaluatesStaticDynamicText
import edu.illinois.ncsa.daffodil.processors.DelimiterStackNode
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.EscapeScheme
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeFactoryDynamic
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeFactoryStatic
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.dsom.EscapeSchemeObject
import edu.illinois.ncsa.daffodil.processors.EscapeKindNoneStackNode
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeStackNode
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeChar
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlock

class ComplexTypeParser(rd: RuntimeData, bodyParser: Parser)
  extends Parser(rd) {
  override def nom = "ComplexType"

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): PState = {
    start.mpstate.childIndexStack.push(1L) // one-based indexing
    val parseState = bodyParser.parse1(start, rd)
    start.mpstate.childIndexStack.pop()
    parseState
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
  with EvaluatesStaticDynamicText {

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<DelimiterStack initiator='" + initiatorOpt +
        "' separator='" + separatorOpt +
        "' terminator='" + terminatorOpt + "'>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</DelimiterStack>"
  }

  val (staticInits, dynamicInits) = getStaticAndDynamicText(initiatorOpt, context)
  val (staticSeps, dynamicSeps) = getStaticAndDynamicText(separatorOpt, context, isLengthKindDelimited)
  val (staticTerms, dynamicTerms) = getStaticAndDynamicText(terminatorOpt, context, isLengthKindDelimited)

  def parse(start: PState): PState = {

    // Evaluate Delimiters
    val (dynamicInitsSeq, afterInits) = evaluateDynamicText(dynamicInits, start, context, false)
    val (dynamicSepsSeq, afterSeps) = evaluateDynamicText(dynamicSeps, afterInits, context, isLengthKindDelimited)
    val (dynamicTermsSeq, afterTerms) = evaluateDynamicText(dynamicTerms, afterSeps, context, isLengthKindDelimited)
    val afterDynamicEval = afterTerms

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
    afterDynamicEval.mpstate.pushDelimiters(node)

    // Parse
    val parseState = bodyParser.parse1(start, rd)

    // Pop Delimiters
    parseState.mpstate.popDelimiters

    parseState
  }
}

/***
 * This parser should only ever be called when an escape scheme exists.
 * 
 * Evaluates the escape scheme and brings it in and out of scope.
 */
class EscapeSchemeStackParser(escapeScheme: EscapeSchemeObject,
  rd: RuntimeData, bodyParser: Parser)
  extends Parser(rd)
  with EvaluatesStaticDynamicText {
  
  override lazy val childProcessors = Seq(bodyParser)

  val scheme =  { 
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

  def parse(start: PState): PState = {
    // Evaluate
    val (afterEval, node) = {
      val (afterDynamicEval, evaluatedScheme) = scheme.getEscapeScheme(start)

      (afterDynamicEval, new EscapeSchemeStackNode(evaluatedScheme))
    }

    // Push Escape Scheme
    afterEval.mpstate.pushEscapeScheme(node)

    // Parse
    val parseState = bodyParser.parse1(start, rd)

    // Pop EscapeScheme
    parseState.mpstate.popEscapeScheme

    parseState
  }
}

class SequenceCombinatorParser(rd: RuntimeData, bodyParser: Parser)
  extends Parser(rd) {
  override def nom = "Sequence"

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): PState = {
    start.mpstate.groupIndexStack.push(1L) // one-based indexing

    val parseState = bodyParser.parse1(start, rd)

    parseState.mpstate.groupIndexStack.pop()
    parseState.mpstate.moveOverOneGroupIndexOnly()
    parseState
  }
}

class ArrayCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser) extends Parser(erd) {
  override def nom = "Array"
  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): PState = {

    start.mpstate.arrayIndexStack.push(1L) // one-based indexing
    start.mpstate.occursBoundsStack.push(DaffodilTunableParameters.maxOccursBounds)

    val parseState = bodyParser.parse1(start, erd)
    if (parseState.status != Success) return parseState

    val shouldValidate = start.dataProc.getValidationMode != ValidationMode.Off

    val actualOccurs = start.mpstate.arrayIndexStack.pop()
    start.mpstate.occursBoundsStack.pop()

    val finalState = {
      (erd.minOccurs, erd.maxOccurs) match {
        case (Some(minOccurs), Some(maxOccurs)) if shouldValidate =>
          val isUnbounded = maxOccurs == -1
          val occurrence = actualOccurs - 1
          val postValidationState = {
            if (isUnbounded && occurrence < minOccurs)
              parseState.withValidationError("%s occurred '%s' times when it was expected to be a " +
                "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.prettyName,
                occurrence, minOccurs)
            else if (!isUnbounded && (occurrence < minOccurs || occurrence > maxOccurs))
              parseState.withValidationError("%s occurred '%s' times when it was expected to be a " +
                "minimum of '%s' and a maximum of '%s' times.", erd.prettyName,
                occurrence, minOccurs, maxOccurs)
            else
              parseState
          }
          postValidationState
        case _ => parseState
      }
    }
    finalState
  }
}
