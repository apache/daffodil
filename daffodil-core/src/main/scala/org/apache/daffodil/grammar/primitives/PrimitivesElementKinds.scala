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

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.dsom._
import org.apache.daffodil.processors.parsers.{ Parser => DaffodilParser }
import org.apache.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.processors.parsers.ComplexTypeParser
import org.apache.daffodil.processors.parsers.SequenceCombinatorParser
import org.apache.daffodil.processors.parsers.ChoiceCombinatorParser
import org.apache.daffodil.processors.parsers.ChoiceDispatchCombinatorParser
import org.apache.daffodil.processors.parsers.ArrayCombinatorParser
import org.apache.daffodil.processors.parsers.OptionalCombinatorParser
import org.apache.daffodil.processors.unparsers.ComplexTypeUnparser
import org.apache.daffodil.processors.unparsers.SequenceCombinatorUnparser
import org.apache.daffodil.processors.unparsers.ChoiceCombinatorUnparser
import org.apache.daffodil.processors.unparsers.HiddenChoiceCombinatorUnparser
import org.apache.daffodil.processors.parsers.DelimiterStackParser
import org.apache.daffodil.processors.parsers.DynamicEscapeSchemeParser
import org.apache.daffodil.processors.unparsers.DynamicEscapeSchemeUnparser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.processors.unparsers.ArrayCombinatorUnparser
import org.apache.daffodil.processors.unparsers.OptionalCombinatorUnparser
import org.apache.daffodil.processors.unparsers.DelimiterStackUnparser
import org.apache.daffodil.grammar.EmptyGram
import org.apache.daffodil.equality._;
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.cookers.ChoiceBranchKeyCooker
import org.apache.daffodil.api.WarnID
import org.apache.daffodil.util.Misc
import org.apache.daffodil.xml.XMLUtils

object ENoWarn3 { EqualitySuppressUnusedImportWarning() }

case class DelimiterStackCombinatorSequence(sq: SequenceTermBase, body: Gram) extends Terminal(sq, !body.isEmpty) {
  lazy val pInit = if (sq.initiatorParseEv.isKnownNonEmpty) One(sq.initiatorParseEv) else Nope
  lazy val pSep = if (sq.separatorParseEv.isKnownNonEmpty) One(sq.separatorParseEv) else Nope
  lazy val pTerm = if (sq.terminatorParseEv.isKnownNonEmpty) One(sq.terminatorParseEv) else Nope

  lazy val uInit = if (sq.initiatorParseEv.isKnownNonEmpty) One(sq.initiatorUnparseEv) else Nope
  lazy val uSep = if (sq.separatorParseEv.isKnownNonEmpty) One(sq.separatorUnparseEv) else Nope
  lazy val uTerm = if (sq.terminatorParseEv.isKnownNonEmpty) One(sq.terminatorUnparseEv) else Nope

  lazy val parser: DaffodilParser = new DelimiterStackParser((pInit.toList ++ pSep.toList ++ pTerm.toList).toArray, sq.runtimeData, body.parser)

  override lazy val unparser: DaffodilUnparser = new DelimiterStackUnparser(uInit, uSep, uTerm, sq.termRuntimeData, body.unparser)
}

case class DelimiterStackCombinatorChoice(ch: ChoiceTermBase, body: Gram) extends Terminal(ch, !body.isEmpty) {
  lazy val pInit = if (ch.initiatorParseEv.isKnownNonEmpty) One(ch.initiatorParseEv) else Nope
  lazy val pTerm = if (ch.terminatorParseEv.isKnownNonEmpty) One(ch.terminatorParseEv) else Nope

  lazy val uInit = if (ch.initiatorParseEv.isKnownNonEmpty) One(ch.initiatorUnparseEv) else Nope
  lazy val uTerm = if (ch.terminatorParseEv.isKnownNonEmpty) One(ch.terminatorUnparseEv) else Nope

  lazy val parser: DaffodilParser = new DelimiterStackParser((pInit.toList ++ pTerm.toList).toArray, ch.runtimeData, body.parser)

  override lazy val unparser: DaffodilUnparser = new DelimiterStackUnparser(uInit, None, uTerm, ch.termRuntimeData, body.unparser)
}

case class DelimiterStackCombinatorElement(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {
  lazy val pInit = if (e.initiatorParseEv.isKnownNonEmpty) One(e.initiatorParseEv) else Nope
  lazy val pTerm = if (e.terminatorParseEv.isKnownNonEmpty) One(e.terminatorParseEv) else Nope

  lazy val uInit = if (e.initiatorParseEv.isKnownNonEmpty) One(e.initiatorUnparseEv) else Nope
  lazy val uTerm = if (e.terminatorParseEv.isKnownNonEmpty) One(e.terminatorUnparseEv) else Nope

  lazy val delims = (pInit.toList ++ pTerm.toList)

  override def toString() = {
    val delimAttrib = delims.map { _.toString }.map { XMLUtils.escape(_).toString() }.mkString(" ")
    "<" + Misc.getNameFromClass(this) + " delims='" + delimAttrib + "'>" +
      body.toString() +
      "</" + Misc.getNameFromClass(this) + ">"
  }
  lazy val parser: DaffodilParser = new DelimiterStackParser(delims.toArray, e.termRuntimeData, body.parser)

  override lazy val unparser: DaffodilUnparser = new DelimiterStackUnparser(uInit, None, uTerm, e.termRuntimeData, body.unparser)
}

case class DynamicEscapeSchemeCombinatorElement(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {

  val schemeParseOpt = e.optionEscapeScheme.map { _.escapeSchemeParseEv }
  val schemeUnparseOpt = e.optionEscapeScheme.map { _.escapeSchemeUnparseEv }

  Assert.invariant(schemeParseOpt.isDefined && !schemeParseOpt.get.isConstant)
  Assert.invariant(schemeUnparseOpt.isDefined && !schemeUnparseOpt.get.isConstant)

  lazy val parser: DaffodilParser = new DynamicEscapeSchemeParser(schemeParseOpt.get, e.termRuntimeData, body.parser)

  override lazy val unparser: DaffodilUnparser = new DynamicEscapeSchemeUnparser(schemeUnparseOpt.get, e.termRuntimeData, body.unparser)
}

case class ComplexTypeCombinator(ct: ComplexTypeBase, body: Gram) extends Terminal(ct.elementDecl, !body.isEmpty) {

  override def isEmpty = body.isEmpty

  override def toString() =
    "<" + Misc.getNameFromClass(this) + ">" +
      body.toString() +
      "</" + Misc.getNameFromClass(this) + ">"

  lazy val parser: DaffodilParser = new ComplexTypeParser(ct.runtimeData, body.parser)

  override lazy val unparser: DaffodilUnparser =
    new ComplexTypeUnparser(ct.runtimeData, body.unparser)
}

case class SequenceCombinator(sq: SequenceTermBase, rawTerms: Seq[Gram])
  extends Terminal(sq, true) {

  lazy val terms = rawTerms.filterNot { _.isEmpty }
  
  override lazy val isEmpty = terms.isEmpty
  override def toString() =
    "<" + Misc.getNameFromClass(this) + ">" +
      terms.map { _.toString() }.mkString +
      "</" + Misc.getNameFromClass(this) + ">"

  lazy val parsers = terms.map { term =>
    term.parser
  }.toVector

  lazy val unparsers = terms.map { term =>
    term.unparser
  }.toVector

  lazy val parser: DaffodilParser = new SequenceCombinatorParser(sq.termRuntimeData, sq.separatorSuppressionPolicy, parsers)

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
  override def toString() = "<Array>" + body.toString + "</Array>"

  lazy val parser: DaffodilParser = new ArrayCombinatorParser(e.elementRuntimeData, body.parser)
  override lazy val unparser: Unparser = new ArrayCombinatorUnparser(e.elementRuntimeData, body.unparser)
}

case class OptionalCombinator(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {

  override def toString() = "<Optional>" + body.toString + "</Optional>"
  lazy val parser: DaffodilParser = new OptionalCombinatorParser(e.elementRuntimeData, body.parser)
  override lazy val unparser: Unparser = new OptionalCombinatorUnparser(e.elementRuntimeData, body.unparser)
}

/*
 * The purpose of the ChoiceCombinator (and the parsers it creates) is to
 * determine which branch to go down. In the parser case, for non-direct
 * dispatch, we just rely on the AltCompParser behavior to handle the
 * backtracking. For direct dispatch, we create a disapatch-branch key map
 * which is used to determine which branch to parse at runtime.
 *
 * In the unparser case, we know which element we got from the infoset, but we
 * need to determine which branch of the choice to take at runtime. This
 * unparser uses a Map to make the determination based on the element seen.
 */
case class ChoiceCombinator(ch: ChoiceTermBase, alternatives: Seq[Gram]) extends Terminal(ch, !alternatives.isEmpty) {
  lazy val parser: DaffodilParser = {
    if (!ch.isDirectDispatch) {
      val folded = alternatives.map { gf => gf }.foldRight(EmptyGram.asInstanceOf[Gram]) { _ | _ }
      new ChoiceCombinatorParser(ch.termRuntimeData, folded.parser)
    } else {
      val dispatchBranchKeyValueTuples = alternatives.flatMap { alt =>
        val keyTerm = alt.context.asInstanceOf[Term]
        val cbk = keyTerm.choiceBranchKey
        // Note that this behaves differently than the specification, since
        // this accepts a space separated list of keys
        val cbks = ChoiceBranchKeyCooker.convertConstant(cbk, keyTerm.runtimeData, forUnparse = false)
        cbks.map { (_, alt) }
      }

      // check for duplicate dfdl:choiceBranchKeys
      val groupedByKey = dispatchBranchKeyValueTuples.groupBy(_._1)
      groupedByKey.foreach {
        case (k, kvs) =>
          if (kvs.length > 1) {
            SDE("dfdl:choiceBranchKey value (%s) is not unique across all branches of a direct dispatch choice. Offending branches are:\n%s",
              k, kvs.map(_._2.context.runtimeData).map(rd => rd.diagnosticDebugName + " " + rd.schemaFileLocation.locationDescription).mkString("- ", "\n- ", ""))
          }
      }

      val dispatchBranchKeyMap = dispatchBranchKeyValueTuples.toMap.mapValues(_.parser)
      val serializableMap = dispatchBranchKeyMap.map(identity)

      new ChoiceDispatchCombinatorParser(ch.termRuntimeData, ch.choiceDispatchKeyEv, serializableMap)
    }
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
      val defaultableBranches = ch.groupMembers.filter { _.childrenInHiddenGroupNotDefaultableOrOVC.length == 0 }
      Assert.invariant(defaultableBranches.length > 0)
      if (defaultableBranches.length > 1) {
        SDW(WarnID.ChoiceInsideHiddenGroup, "xs:choice inside a hidden group has unparse ambiguity: multiple branches exist with all children either defaulable or have the dfdl:outputValueCalc property set. The first branch will be chosen during unparse. Defaultable branches are:\n%s",
          defaultableBranches.mkString("\n"))
      }
      val defaultableBranchRD = defaultableBranches(0).runtimeData
      val defaultableBranchUnparser = alternatives.find(_.context.runtimeData =:= defaultableBranchRD).get.unparser
      new HiddenChoiceCombinatorUnparser(ch.modelGroupRuntimeData, defaultableBranchUnparser)
    }
  }
}

