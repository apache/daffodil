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

package org.apache.daffodil.grammar
import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.dsom._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar.primitives._
import org.apache.daffodil.grammar.primitives.OrderedSequence

trait SequenceGrammarMixin extends GrammarMixin { self: SequenceTermBase =>

  final override lazy val groupContent = prod("groupContent") {
    if (isLayered) layerContent
    else sequenceContent
  }

  final lazy val sequenceContent = {
    import columnConstants._
    self.sequenceKind match {
      case Ordered__ => orderedSequence
      case Unordered => subsetError("Unordered sequences are not supported.") // unorderedSequenceContent
    }
  }

  private lazy val layerContent = {
    schemaDefinitionUnless(groupMembers.length == 1, "Layered sequence can have only 1 child term. %s were found: %s", groupMembers.length,
      groupMembers.mkString(", "))
    val term = groupMembers(0)
    schemaDefinitionWhen(term.isArray, "Layered sequence body cannot be an array.")
    LayeredSequence(this, new ScalarOrderedRequiredSequenceChild(this, term, 1))
  }

  private lazy val seqChildren = (groupMembers zip Stream.from(1)).map {
    case (gm, i) =>
      sequenceChild(gm, i)
  }

  private lazy val orderedSequence = {
    val res = new OrderedSequence(this, seqChildren)
    res
  }

  /**
   * Constants to make the lookup tables below more readable without using fragile whitespace
   */
  private object columnConstants {
    val UNB = -1 // UNBOUNDED
    val ZER = 0
    val ONE = 1

    val Sep__ = true
    val NoSep = false

    val Ordered__ = SequenceKind.Ordered
    val Unordered = SequenceKind.Unordered

    val Never______ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.Never
    val Trailing___ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.TrailingEmpty
    val TrailingStr: SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.TrailingEmptyStrict
    val Always_____ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.AnyEmpty

    val StopValue_ = OccursCountKind.StopValue
    val Implicit__ = OccursCountKind.Implicit
    val Parsed____ = OccursCountKind.Parsed
    val Fixed_____ = OccursCountKind.Fixed
    val Expression = OccursCountKind.Expression

    type EB = ElementBase
    type MG = ModelGroup
    type SG = SequenceTermBase
    type CG = ChoiceTermBase
  }

  /**
   * Produces the right kind of SequenceChild object for this particular child
   * for the role it must play within this sequence's behavior.
   *
   * A SequenceChild object is effectively generator for part of the Sequence's parse/unparse
   * algorithm. For arrays these SequenceChild objects enable processing exactly one array instance at
   * a time, orchestrated by the surrounding sequence's processor.
   */
  private def sequenceChild(child: Term, groupIndex: Int): SequenceChild = {
    import columnConstants._
    val (max, min, ock) = child match {
      case e: EB => (e.maxOccurs, e.minOccurs, e.occursCountKind)
      case _ => (1, 1, null)
    }
    val ssp = separatorSuppressionPolicy
    val res = (child, sequenceKind, hasSeparator, ssp, ock, min, max) match {
      case (e: EB, Ordered__, _____, ___________, _________, ONE, ONE) => new ScalarOrderedRequiredSequenceChild(this, e, groupIndex)
      case (e: EB, _________, _____, ___________, StopValue_, ___, __2) => e.subsetError("dfdl:occursCountKind 'stopValue' is not supported.")
      case (_____, Unordered, ____2, ___________, __________, ___, __2) => this.subsetError("Unordered sequences are not supported.")
      case (e: EB, Ordered__, _____, ___________, Fixed_____, ___, UNB) => e.SDE("occursCountKind='fixed' not allowed with unbounded maxOccurs")
      case (e: EB, Ordered__, _____, ___________, Fixed_____, min, `min`) => new RepOrderedExactlyNSequenceChild(this, e, groupIndex, min)
      case (e: EB, Ordered__, _____, ___________, Fixed_____, min, max) => { Assert.invariant(min != max); e.SDE("occursCountKind='fixed' requires minOccurs and maxOccurs to be equal (%d != %d)", min, max) }
      case (e: EB, Ordered__, _____, ___________, Expression, ___, __2) => new RepOrderedExactlyTotalOccursCountSequenceChild(this, e, groupIndex)
      case (e: EB, Ordered__, _____, Never______, Implicit__, ___, UNB) => e.SDE("separatorSuppressionPolicy='never' with occursCountKind='implicit' requires bounded maxOccurs.")
      case (e: EB, Ordered__, _____, Never______, Implicit__, ___, max) => new RepOrderedExactlyNSequenceChild(this, e, groupIndex, max)
      case (e: EB, Ordered__, _____, Never______, ock /****/ , ___, __2) => e.SDE("separatorSuppressionPolicy='never' not allowed in combination with occursCountKind='" + ock + "'.")
      case (e: EB, Ordered__, _____, Trailing___, Implicit__, ___, UNB) if (!e.isLastDeclaredRequiredElementOfSequence) => e.SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a sequence")
      case (e: EB, Ordered__, _____, Trailing___, Implicit__, min, max) => new RepOrderedWithMinMaxSequenceChild(this, e, groupIndex, "MinMaxTrailingLax")
      case (e: EB, Ordered__, _____, TrailingStr, Implicit__, ___, UNB) if (!e.isLastDeclaredRequiredElementOfSequence) => e.SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a sequence")
      case (e: EB, Ordered__, _____, TrailingStr, Implicit__, ___, max) => new RepOrderedWithMinMaxSequenceChild(this, e, groupIndex, "MinUnboundedTrailingStrict", min, -2)
      case (e: EB, Ordered__, _____, Always_____, Implicit__, ___, max) => new RepOrderedWithMinMaxSequenceChild(this, e, groupIndex, "MinMaxAlways")
      case (e: EB, Ordered__, _____, Always_____, Parsed____, ___, __2) => new RepOrderedWithMinMaxSequenceChild(this, e, groupIndex, "Unbounded")
      case (m: MG, Ordered__, _____, ___________, __________, ___, __2) => new ScalarOrderedRequiredSequenceChild(this, m, groupIndex)
      case (_____, _________, ____2, policy /**/ , ock /**/ , ___, __2) => child.SDE("separatorSuppressionPolicy='" + policy + "' not allowed with occursCountKind='" + ock + "'.")
    }
    res
  }

  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */
  final lazy val hasPrefixSep = sepExpr(SeparatorPosition.Prefix)

  final lazy val hasInfixSep = sepExpr(SeparatorPosition.Infix)

  final lazy val hasPostfixSep = sepExpr(SeparatorPosition.Postfix)

  // note use of pass by value. We don't want to even need the SeparatorPosition property unless there is a separator.
  private def sepExpr(pos: => SeparatorPosition): Boolean = {
    if (hasSeparator) if (separatorPosition eq pos) true else false
    else false
  }

  final lazy val hasSeparator = separatorParseEv.isKnownNonEmpty

  lazy val sequenceSeparator = prod("separator", hasSeparator) {
    //
    // TODO: (JIRA DFDL-1400) The separators may be in a different encoding than the terms
    // that they separate.
    //
    // So we must allow for a change of encoding (which may also imply a change
    // of bit order)
    //
    // However, this isn't the same as just plopping down a bitOrderChange ~ encodingChange, since
    // those examine prior peer, and what we want to scrutinize is the prior term being separated.
    //
    delimMTA ~ SequenceSeparator(this)
  }
}

