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

import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.{ Parser, ParserObject }
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.DelimiterStackNode
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.InitiatorParseEv
import edu.illinois.ncsa.daffodil.processors.SeparatorParseEv
import edu.illinois.ncsa.daffodil.processors.TerminatorParseEv
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeParseEv

class ComplexTypeParser(rd: RuntimeData, bodyParser: Parser)
  extends ParserObject(rd) {
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
class DelimiterStackParser(initiatorOpt: Maybe[InitiatorParseEv],
  separatorOpt: Maybe[SeparatorParseEv],
  terminatorOpt: Maybe[TerminatorParseEv],
  initiatorLoc: (String, String),
  separatorLocOpt: Maybe[(String, String)],
  terminatorLoc: (String, String),
  override val context: RuntimeData, bodyParser: Parser)
  extends Parser {

  override lazy val childProcessors = List(bodyParser)

  override lazy val runtimeDependencies = initiatorOpt.toList ++ separatorOpt.toList ++ terminatorOpt.toList

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else {
      "<DelimiterStack" +
        initiatorOpt.toScalaOption.map { i => " initiator='" + i + "'" }.getOrElse("") +
        separatorOpt.toScalaOption.map { s => " separator='" + s + "'" }.getOrElse("") +
        terminatorOpt.toScalaOption.map { t => " terminator='" + t + "'" }.getOrElse("") + ">" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</DelimiterStack>"
    }
  }

  def parse(start: PState): Unit = {

    val allInits: Array[DFADelimiter] = if (initiatorOpt.isDefined) initiatorOpt.get.evaluate(start) else Array()
    val allSeps: Array[DFADelimiter] = if (separatorOpt.isDefined) separatorOpt.get.evaluate(start) else Array()
    val allTerms: Array[DFADelimiter] = if (terminatorOpt.isDefined) terminatorOpt.get.evaluate(start) else Array()

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
class EscapeSchemeStackParser(escapeScheme: EscapeSchemeParseEv,
  override val context: RuntimeData, bodyParser: Parser)
  extends Parser {

  override lazy val childProcessors = Seq(bodyParser)

  override lazy val runtimeDependencies = List(escapeScheme)

  def parse(start: PState): Unit = {
    // Set Escape Scheme
    start.mpstate.currentEscapeScheme = One(escapeScheme.evaluate(start))

    // Parse
    bodyParser.parse1(start)

    // Clear EscapeScheme
    start.mpstate.currentEscapeScheme = Nope
  }
}

class EscapeSchemeNoneStackParser(
  rd: RuntimeData, bodyParser: Parser)
  extends ParserObject(rd) {

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
  extends ParserObject(rd) {
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
  extends ParserObject(rd) {
  override def nom = "Choice"

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {
    bodyParser.parse1(start)
  }
}

class ArrayCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser) extends ParserObject(erd) {
  override def nom = "Array"
  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {

    start.mpstate.arrayIndexStack.push(1L) // one-based indexing
    start.mpstate.occursBoundsStack.push(DaffodilTunableParameters.maxOccursBounds)

    bodyParser.parse1(start)
    if (start.status ne Success) return

    val shouldValidate =
      start.dataProc.isDefined && start.dataProc.value.getValidationMode != ValidationMode.Off

    val actualOccurs = start.mpstate.arrayIndexStack.pop()
    start.mpstate.occursBoundsStack.pop()

    if (shouldValidate && erd.minOccurs.isDefined && erd.maxOccurs.isDefined) {
      val minO = erd.minOccurs.get
      val maxO = erd.maxOccurs.get
      val isUnbounded = maxO == -1
      val occurrence = actualOccurs - 1

      if (isUnbounded && occurrence < minO)
        start.reportValidationError("%s occurred '%s' times when it was expected to be a " +
          "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.prettyName,
          occurrence, minO)
      else if (!isUnbounded && (occurrence < minO || occurrence > maxO))
        start.reportValidationError("%s occurred '%s' times when it was expected to be a " +
          "minimum of '%s' and a maximum of '%s' times.", erd.prettyName,
          occurrence, minO, maxO)
      else {
        //ok
      }
    }
  }
}

// This follows the same behavior as Arrays for parsing
class OptionalCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser) extends ArrayCombinatorParser(erd, bodyParser) {
  override def nom = "Optional"
}
