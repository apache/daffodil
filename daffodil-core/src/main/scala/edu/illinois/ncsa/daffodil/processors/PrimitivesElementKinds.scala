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
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.parsers.ComplexTypeParser
import edu.illinois.ncsa.daffodil.processors.parsers.SequenceCombinatorParser
import edu.illinois.ncsa.daffodil.processors.parsers.ChoiceCombinatorParser
import edu.illinois.ncsa.daffodil.processors.parsers.ArrayCombinatorParser
import edu.illinois.ncsa.daffodil.processors.parsers.OptionalCombinatorParser
import edu.illinois.ncsa.daffodil.processors.unparsers.ComplexTypeUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.SequenceCombinatorUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ChoiceCombinatorUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.HiddenChoiceCombinatorUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.DelimiterStackParser
import edu.illinois.ncsa.daffodil.processors.parsers.DynamicEscapeSchemeParser
import edu.illinois.ncsa.daffodil.processors.unparsers.DynamicEscapeSchemeUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ArrayCombinatorUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.OptionalCombinatorUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.DelimiterStackUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.FinalizeProcessingUnparser
import edu.illinois.ncsa.daffodil.grammar.EmptyGram
import edu.illinois.ncsa.daffodil.equality._; object ENoWarn3 { EqualitySuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe._

case class DelimiterStackCombinatorSequence(sq: Sequence, body: Gram) extends Terminal(sq, !body.isEmpty) {
  lazy val pInit = if (sq.initiatorParseEv.isKnownNonEmpty)  One(sq.initiatorParseEv)  else Nope
  lazy val pSep  = if (sq.separatorParseEv.isKnownNonEmpty)  One(sq.separatorParseEv)  else Nope
  lazy val pTerm = if (sq.terminatorParseEv.isKnownNonEmpty) One(sq.terminatorParseEv) else Nope
  
  lazy val uInit = if (sq.initiatorParseEv.isKnownNonEmpty)  One(sq.initiatorUnparseEv)  else Nope
  lazy val uSep  = if (sq.separatorParseEv.isKnownNonEmpty)  One(sq.separatorUnparseEv)  else Nope
  lazy val uTerm = if (sq.terminatorParseEv.isKnownNonEmpty) One(sq.terminatorUnparseEv) else Nope

  lazy val parser: DaffodilParser = new DelimiterStackParser((pInit.toList ++ pSep.toList ++ pTerm.toList).toArray, sq.runtimeData, body.parser)

  override lazy val unparser: DaffodilUnparser = new DelimiterStackUnparser(uInit, uSep, uTerm, sq.runtimeData, body.unparser)
}

case class DelimiterStackCombinatorChoice(ch: Choice, body: Gram) extends Terminal(ch, !body.isEmpty) {
  lazy val pInit = if (ch.initiatorParseEv.isKnownNonEmpty)  One(ch.initiatorParseEv)  else Nope
  lazy val pTerm = if (ch.terminatorParseEv.isKnownNonEmpty) One(ch.terminatorParseEv) else Nope
  
  lazy val uInit = if (ch.initiatorParseEv.isKnownNonEmpty)  One(ch.initiatorUnparseEv)  else Nope
  lazy val uTerm = if (ch.terminatorParseEv.isKnownNonEmpty) One(ch.terminatorUnparseEv) else Nope

  lazy val parser: DaffodilParser = new DelimiterStackParser((pInit.toList ++ pTerm.toList).toArray, ch.runtimeData, body.parser)

  override lazy val unparser: DaffodilUnparser = new DelimiterStackUnparser(uInit, None, uTerm, ch.runtimeData, body.unparser)
}

case class DelimiterStackCombinatorElement(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {
  lazy val pInit = if (e.initiatorParseEv.isKnownNonEmpty)  One(e.initiatorParseEv)  else Nope
  lazy val pTerm = if (e.terminatorParseEv.isKnownNonEmpty) One(e.terminatorParseEv) else Nope
  
  lazy val uInit = if (e.initiatorParseEv.isKnownNonEmpty)  One(e.initiatorUnparseEv)  else Nope
  lazy val uTerm = if (e.terminatorParseEv.isKnownNonEmpty) One(e.terminatorUnparseEv) else Nope

  lazy val parser: DaffodilParser = new DelimiterStackParser((pInit.toList ++ pTerm.toList).toArray, e.runtimeData, body.parser)

  override lazy val unparser: DaffodilUnparser = new DelimiterStackUnparser(uInit, None, uTerm, e.runtimeData, body.unparser)
}

case class DynamicEscapeSchemeCombinatorElement(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {

  val schemeParseOpt = e.optionEscapeScheme.map { _.escapeSchemeParseEv }
  val schemeUnparseOpt = e.optionEscapeScheme.map { _.escapeSchemeUnparseEv }

  Assert.invariant(schemeParseOpt.isDefined && !schemeParseOpt.get.isConstant)
  Assert.invariant(schemeUnparseOpt.isDefined && !schemeUnparseOpt.get.isConstant)

  lazy val parser: DaffodilParser = new DynamicEscapeSchemeParser(schemeParseOpt.get, e.runtimeData, body.parser)

  override lazy val unparser: DaffodilUnparser = new DynamicEscapeSchemeUnparser(schemeUnparseOpt.get, e.runtimeData, body.unparser)
}

case class ComplexTypeCombinator(ct: ComplexTypeBase, body: Gram) extends Terminal(ct.element, !body.isEmpty) {

  lazy val parser: DaffodilParser = new ComplexTypeParser(ct.runtimeData, body.parser)
  override lazy val unparser: DaffodilUnparser =
    new ComplexTypeUnparser(ct.runtimeData, body.unparser)
}

case class SequenceCombinator(sq: Sequence, rawTerms: Seq[Gram])
  extends Terminal(sq, !rawTerms.filterNot { _.isEmpty }.isEmpty) {

  private val mt: Gram = EmptyGram
  lazy val body = rawTerms.foldRight(mt) { _ ~ _ }
  lazy val terms = rawTerms.filterNot { _.isEmpty }
  lazy val unparsers = terms.map { _.unparser }.toVector

  lazy val parser: DaffodilParser = new SequenceCombinatorParser(sq.termRuntimeData, body.parser)

  override lazy val unparser: DaffodilUnparser = {
    sq.checkHiddenSequenceIsDefaultableOrOVC
    new SequenceCombinatorUnparser(sq.modelGroupRuntimeData, unparsers)
  }
}

case class UnorderedSequenceCombinator(s: Sequence, terms: Seq[Gram])
  extends UnimplementedPrimitive(s, false) {
  // stub for now. These are not implemented currently.
}

case class ArrayCombinator(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {

  lazy val parser: DaffodilParser = new ArrayCombinatorParser(e.elementRuntimeData, body.parser)
  override lazy val unparser: Unparser = new ArrayCombinatorUnparser(e.elementRuntimeData, body.unparser)
}

case class OptionalCombinator(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {

  lazy val parser: DaffodilParser = new OptionalCombinatorParser(e.elementRuntimeData, body.parser)
  override lazy val unparser: Unparser = new OptionalCombinatorUnparser(e.elementRuntimeData, body.unparser)
}

/*
 * The purpose of the ChoiceCombinator (and the parsers it creates) is to
 * determine which branch to go down. In the parser case, we just rely on the
 * AltCompParser behavior to handle the backtracking. In the unparser case, we
 * know which element we got from the infoset, but we need to determine which
 * branch of the choice to take. This unparser uses a Map to make the
 * determination based on the element seen.
 */
case class ChoiceCombinator(ch: Choice, alternatives: Seq[Gram]) extends Terminal(ch, !alternatives.isEmpty) {
  lazy val parser: DaffodilParser = {
    val folded = alternatives.map { gf => gf }.foldRight(EmptyGram.asInstanceOf[Gram]) { _ | _ }
    new ChoiceCombinatorParser(ch.runtimeData, folded.parser)
  }

  override lazy val unparser: DaffodilUnparser = {
    if (!ch.isHidden) {
      val eventRDMap = ch.choiceBranchMap
      val eventUnparserMap = eventRDMap.mapValues { rd =>
        alternatives.find(_.context.runtimeData =:= rd).get.unparser
      }

      // The following line is required because mapValues() creates a "view" of
      // the map, which is not serializable. map()ing this "view" with the
      // identity forces evaluation of the "view", creating a map that is
      // serializable and can be safely passed to a parser. See SI-7005 for
      // discussions about this issue.
      val serializableMap = eventUnparserMap.map(identity)

      new ChoiceCombinatorUnparser(ch.modelGroupRuntimeData, serializableMap)
    } else {
      // Choices inside a hidden group ref are slightly different because we
      // will never see events for any of the branches. Instead, we will just
      // always pick the branch in which every thing is defaultble or OVC. It
      // is a warning if more than one of those branches exist. It is an SDE if
      // such a branch does not exist, which is detected elsewhere

      // this call is necessary since it will throw an SDE if no choice branch
      // was defaultable
      ch.childrenInHiddenGroupNotDefaultableOrOVC
      val defaultableBranches = ch.groupMembersNoRefs.filter { _.childrenInHiddenGroupNotDefaultableOrOVC.length == 0 }
      Assert.invariant(defaultableBranches.length > 0)
      if (defaultableBranches.length > 1) {
        SDW("xs:choice inside a hidden group has unparse ambiguity: multiple branches exist with all children either defaulable or have the dfdl:outputValueCalc property set. The first branch will be chosen during unparse. Defaultable branches are:\n%s",
          defaultableBranches.mkString("\n"))
      }
      val defaultableBranchRD = defaultableBranches(0).runtimeData
      val defaultableBranchUnparser = alternatives.find(_.context.runtimeData =:= defaultableBranchRD).get.unparser
      new HiddenChoiceCombinatorUnparser(ch.modelGroupRuntimeData, defaultableBranchUnparser)
    }
  }
}

case class FinalizeProcessing(e: GlobalElementDecl) extends Terminal(e, true) {
  lazy val parser: DaffodilParser = new NadaParser(e.runtimeData)

  override lazy val unparser: DaffodilUnparser = new FinalizeProcessingUnparser(e.elementRuntimeData)
}
