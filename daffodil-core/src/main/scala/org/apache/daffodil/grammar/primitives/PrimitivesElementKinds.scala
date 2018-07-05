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
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.processors.unparsers._
import org.apache.daffodil.grammar.Gram
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
  lazy val parser: DaffodilParser = {
    val p = body.parser
    if (p.isEmpty) p
    else new DelimiterStackParser(delims.toArray, e.termRuntimeData, p)
  }

  override lazy val unparser: DaffodilUnparser = {
    val u = body.unparser
    if (u.isEmpty) u
    else new DelimiterStackUnparser(uInit, None, uTerm, e.termRuntimeData, u)
  }
}

case class DynamicEscapeSchemeCombinatorElement(e: ElementBase, body: Gram) extends Terminal(e, !body.isEmpty) {

  val schemeParseOpt = e.optionEscapeScheme.map { _.escapeSchemeParseEv }
  val schemeUnparseOpt = e.optionEscapeScheme.map { _.escapeSchemeUnparseEv }

  Assert.invariant(schemeParseOpt.isDefined && !schemeParseOpt.get.isConstant)
  Assert.invariant(schemeUnparseOpt.isDefined && !schemeUnparseOpt.get.isConstant)

  lazy val parser: DaffodilParser = {
    val p = body.parser
    if (p.isEmpty) p
    else new DynamicEscapeSchemeParser(schemeParseOpt.get, e.termRuntimeData, p)
  }

  override lazy val unparser: DaffodilUnparser = {
    val u = body.unparser
    if (u.isEmpty) u
    else new DynamicEscapeSchemeUnparser(schemeUnparseOpt.get, e.termRuntimeData, u)
  }
}

case class ComplexTypeCombinator(ct: ComplexTypeBase, body: Gram) extends Terminal(ct.elementDecl, !body.isEmpty) {

  override def isEmpty = body.isEmpty

  private lazy val p = body.parser
  private lazy val u = body.unparser

  override def toString() =
    "<" + Misc.getNameFromClass(this) + ">" +
      body.toString() +
      "</" + Misc.getNameFromClass(this) + ">"

  lazy val parser: DaffodilParser =
    if (p.isEmpty)
      p
    else
      new ComplexTypeParser(ct.runtimeData, p)

  override lazy val unparser: DaffodilUnparser =
    if (u.isEmpty)
      u
    else
      new ComplexTypeUnparser(ct.runtimeData, u)
}

sealed abstract class OrderedSequenceBase(sq: SequenceTermBase, rawTerms: Seq[Term])
  extends Terminal(sq, rawTerms.length > 0) {

  private lazy val terms = rawTerms.filterNot { _.termContentBody.isEmpty }

  // if all the body terms are empty, this causes this whole combinator to
  // optimize away.
  override final lazy val isEmpty = super.isEmpty || terms.isEmpty

  override def toString() =
    "<" + Misc.getNameFromClass(this) + ">" +
      rawTerms.map { _.toString() }.mkString +
      "</" + Misc.getNameFromClass(this) + ">"

  protected lazy val parserPairs = terms.flatMap { term =>
    val isNotRequired = !term.isRequired
    val gram = term match {
      case e: ElementBase if isNotRequired || term.isArray => e.recurrance
      case e: ElementBase => e.enclosedElement
      case _ => term.termContentBody
    }
    val p = gram.parser
    if (p.isEmpty) Nil
    else List((term.termRuntimeData, gram.parser))
  }.toVector

  protected lazy val unparserPairs = terms.flatMap { term =>
    val isNotRequired = !term.isRequired
    val gram = term match {
      case e: ElementBase if isNotRequired || term.isArray => e.recurrance
      case e: ElementBase => e.enclosedElement
      case _ => term.termContentBody
    }
    val u = gram.unparser
    if (u.isEmpty) Nil
    else List((term.termRuntimeData, gram.unparser))
  }.toVector

  protected lazy val parsers = parserPairs.map { _._2 }

  protected lazy val unparsers = unparserPairs.map { _._2 }
}

case class OrderedUnseparatedSequence(sq: SequenceTermBase, rawTerms: Seq[Term])
  extends OrderedSequenceBase(sq, rawTerms) {

  lazy val parser: DaffodilParser =
    new OrderedUnseparatedSequenceParser(sq.termRuntimeData, parsers)

  override lazy val unparser: DaffodilUnparser = {
    sq.checkHiddenSequenceIsDefaultableOrOVC
    new OrderedUnseparatedSequenceUnparser(sq.modelGroupRuntimeData, unparsers)
  }
}

case class OrderedSeparatedSequence(sq: SequenceTermBase, rawTerms: Seq[Term])
  extends OrderedSequenceBase(sq, rawTerms) {

  lazy val parser: DaffodilParser = {
    new OrderedSeparatedSequenceParser(sq.termRuntimeData, sq.separatorSuppressionPolicy,
      sq.separatorPosition, sq.sequenceSeparator.parser, parserPairs)
  }

  override lazy val unparser: DaffodilUnparser = {
    sq.checkHiddenSequenceIsDefaultableOrOVC
    new OrderedSeparatedSequenceUnparser(sq.modelGroupRuntimeData, sq.separatorPosition,
      sq.sequenceSeparator.unparser, unparserPairs)
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
 * dispatch, we just rely on backtracking here.
 *
 * For direct dispatch, we create a disapatch-branch key map
 * which is used to determine which branch to parse at runtime.
 *
 * In the unparser case, we know which element we got from the infoset, but we
 * need to determine which branch of the choice to take at runtime. This
 * unparser uses a Map to make the determination based on the element seen.
 */
case class ChoiceCombinator(ch: ChoiceTermBase, rawAlternatives: Seq[Gram]) extends Terminal(ch, !rawAlternatives.isEmpty) {

  private lazy val alternatives = rawAlternatives.filterNot(_.isEmpty)

  private lazy val parsers = alternatives.map { _.parser }.filterNot { _.isEmpty }

  override def isEmpty = super.isEmpty || alternatives.isEmpty

  lazy val parser: DaffodilParser = {
    if (!ch.isDirectDispatch) {
      new ChoiceParser(ch.termRuntimeData, parsers)
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
      val eventUnparserMap = eventRDMap.flatMap {
        case (cbe, rd) =>
          // if we don't find a matching RD for a term that's probably
          // because the term is an empty sequence or empty choice (which do happen
          // and we even have tests for them). Since those can never be chosen by
          // means of an element event, they don't appear in the map.
          val altGram = alternatives.find { alt =>
            val crd = alt.context.runtimeData
            val found = crd =:= rd
            found
          }
          altGram.map { ag => (cbe, ag.unparser) }
      }

      new ChoiceCombinatorUnparser(ch.modelGroupRuntimeData, eventUnparserMap)
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

