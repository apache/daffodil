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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }
import edu.illinois.ncsa.daffodil.util.{ Debug, LogLevel, Logging, Info }
import edu.illinois.ncsa.daffodil.dpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.grammar.Gram
import scala.collection.mutable.Stack
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar.UnaryGram
import edu.illinois.ncsa.daffodil.processors.parsers.ComplexTypeParser
import edu.illinois.ncsa.daffodil.processors.parsers.SequenceCombinatorParser
import edu.illinois.ncsa.daffodil.processors.parsers.ChoiceCombinatorParser
import edu.illinois.ncsa.daffodil.processors.parsers.ArrayCombinatorParser
import edu.illinois.ncsa.daffodil.processors.unparsers.ComplexTypeUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.SequenceCombinatorUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ChoiceCombinatorUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.DelimiterStackParser
import edu.illinois.ncsa.daffodil.processors.parsers.EscapeSchemeStackParser
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthKind
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.unparsers.EscapeSchemeStackUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ArrayCombinatorUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.DelimiterStackUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.EscapeSchemeNoneStackParser
import edu.illinois.ncsa.daffodil.processors.unparsers.EscapeSchemeNoneStackUnparser
import edu.illinois.ncsa.daffodil.xml.QNameBase
import edu.illinois.ncsa.daffodil.grammar.EmptyGram
import edu.illinois.ncsa.daffodil.equality._

case class DelimiterStackCombinatorSequence(sq: Sequence, body: Gram) extends Terminal(sq, !body.isEmpty) {
  val isLengthKindDelimited =
    if (sq.enclosingElement.isDefined) { sq.enclosingElement.get.lengthKind == LengthKind.Delimited }
    else false

  def parser: DaffodilParser = new DelimiterStackParser(Some(sq.initiator), Some(sq.separator), Some(sq.terminator),
    sq.initiatorLoc, Some(sq.separatorLoc), sq.terminatorLoc, isLengthKindDelimited, sq.runtimeData, body.parser)

  override def unparser: DaffodilUnparser = new DelimiterStackUnparser(sq.outputNewLine, Some(sq.initiator), Some(sq.separator), Some(sq.terminator),
    sq.initiatorLoc, Some(sq.separatorLoc), sq.terminatorLoc, isLengthKindDelimited, sq.runtimeData, body.unparser)
}

case class DelimiterStackCombinatorChoice(ch: Choice, body: Gram) extends Terminal(ch, !body.isEmpty) {
  val isLengthKindDelimited =
    if (ch.enclosingElement.isDefined) { ch.enclosingElement.get.lengthKind == LengthKind.Delimited }
    else false

  def parser: DaffodilParser = new DelimiterStackParser(Some(ch.initiator), None, Some(ch.terminator),
    ch.initiatorLoc, None, ch.terminatorLoc, isLengthKindDelimited, ch.runtimeData, body.parser)

  override def unparser: DaffodilUnparser = new DelimiterStackUnparser(ch.outputNewLine, Some(ch.initiator), None, Some(ch.terminator),
    ch.initiatorLoc, None, ch.terminatorLoc, isLengthKindDelimited, ch.runtimeData, body.unparser)
}

case class DelimiterStackCombinatorElement(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {
  val isLengthKindDelimited = e.lengthKind == LengthKind.Delimited

  def parser: DaffodilParser = new DelimiterStackParser(Some(e.initiator), None, Some(e.terminator),
    e.initiatorLoc, None, e.terminatorLoc, isLengthKindDelimited, e.runtimeData, body.parser)

  override def unparser: DaffodilUnparser = new DelimiterStackUnparser(e.outputNewLine, Some(e.initiator), None, Some(e.terminator),
    e.initiatorLoc, None, e.terminatorLoc, isLengthKindDelimited, e.runtimeData, body.unparser)
}

case class EscapeSchemeStackCombinatorElement(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {

  val schemeOpt = e.optionEscapeScheme.map { _.escapeScheme }

  def parser: DaffodilParser =
    if (schemeOpt.isDefined) new EscapeSchemeStackParser(schemeOpt.get, e.runtimeData, body.parser)
    else new EscapeSchemeNoneStackParser(e.runtimeData, body.parser)
  
  override def unparser: DaffodilUnparser =
    if (schemeOpt.isDefined) new EscapeSchemeStackUnparser(schemeOpt.get, e.runtimeData, body.unparser)
    else new EscapeSchemeNoneStackUnparser(e.runtimeData, body.unparser)
}

case class ComplexTypeCombinator(ct: ComplexTypeBase, body: Gram) extends Terminal(ct.element, !body.isEmpty) {

  def parser: DaffodilParser = new ComplexTypeParser(ct.runtimeData, body.parser)
  override def unparser: DaffodilUnparser =
    new ComplexTypeUnparser(ct.runtimeData, body.unparser)
}

case class SequenceCombinator(sq: Sequence, body: Gram) extends Terminal(sq, !body.isEmpty) {

  def parser: DaffodilParser = new SequenceCombinatorParser(sq.runtimeData, body.parser)
  override def unparser: DaffodilUnparser = new SequenceCombinatorUnparser(sq.runtimeData, body.unparser)
}

case class ArrayCombinator(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {

  def parser: DaffodilParser = new ArrayCombinatorParser(e.elementRuntimeData, body.parser)
  override def unparser: Unparser = new ArrayCombinatorUnparser(e.elementRuntimeData, body.unparser)

}

/*
 * The purpose of the ChoiceCombinator (and the parsers it creates) is to
 * determine which branch to go down. In the parser case, we just rely on the
 * AltCompParser behavior to handle the backtracking. In the unparser case, we
 * know which element we got from the infoset, but we need to determine which
 * branch of the infoset to take. This unparser uses a Map to make the
 * determination based on the element seen.
 */
case class ChoiceCombinator(ch: Choice, alternatives: Seq[Gram]) extends Terminal(ch, !alternatives.isEmpty) {
  def parser: DaffodilParser = {
    var folded = alternatives.map { gf => gf }.foldRight(EmptyGram.asInstanceOf[Gram]) { _ | _ }
    new ChoiceCombinatorParser(ch.runtimeData, folded.parser)
  }

  override def unparser: DaffodilUnparser = {
    val qnameRDMap = ch.choiceBranchMap
    val qnameUnparserMap = qnameRDMap.mapValues { v =>
      alternatives.find(_.context.runtimeData =:= v).get.unparser
    }.asInstanceOf[Map[QNameBase,DaffodilUnparser]]

    // The following line is required because mapValues() creates a "view" of
    // the map, which is not serializable. map()ing this "view" with the
    // identity forces evaluation of the "view", creating a map that is
    // serializable and can be safely passed to a parser. See SI-7005 for
    // discussions about this issue.
    val serializableMap = qnameUnparserMap.map(identity)

    new ChoiceCombinatorUnparser(ch.runtimeData, serializableMap)
  }
}

