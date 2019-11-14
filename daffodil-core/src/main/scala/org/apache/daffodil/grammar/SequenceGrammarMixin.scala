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
import org.apache.daffodil.grammar.primitives.UnorderedSequence
import org.apache.daffodil.runtime1.SequenceTermRuntime1Mixin

trait SequenceGrammarMixin
  extends GrammarMixin
  with SequenceTermRuntime1Mixin { self: SequenceTermBase =>

  final override lazy val groupContent = prod("groupContent") {
    if (isLayered) layerContent
    else sequenceContent
  }

  private lazy val sequenceContent = {
    import columnConstants._
    self.sequenceKind match {
      case Ordered__ => orderedSequence
      case Unordered => unorderedSequence
    }
  }

  private lazy val layerContent = {
    schemaDefinitionUnless(groupMembers.length == 1, "Layered sequence can have only 1 child term. %s were found: %s", groupMembers.length,
      groupMembers.mkString(", "))
    val term = groupMembers(0)
    schemaDefinitionWhen(term.isArray, "Layered sequence body cannot be an array.")
    LayeredSequence(this, new ScalarOrderedSequenceChild(this, term, 1)) // We use 1-based indexing for children.
  }

  private lazy val seqChildren = LV('seqChildren) {
    (groupMembers zip Stream.from(1)).map {
      case (gm, i) =>
        sequenceChild(gm, i)
    }
  }.value

  private lazy val orderedSequence = {
    val res = new OrderedSequence(this, seqChildren)
    res
  }

  private lazy val unorderedSequence = {
    schemaDefinitionUnless(groupMembers.length > 0, "Unordered sequences must not be empty")
    val alternatives = groupMembers.map { _.termContentBody }
    val res = new UnorderedSequence(this, seqChildren, alternatives)
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

    val True_ = true

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
      case e: EB =>
        // don't require OCK unnecessarily
        (e.maxOccurs, e.minOccurs, if (e.isScalar) null else e.occursCountKind)
      case _ => (1, 1, null)
    }
    // don't require SSP unnecessarily
    val ssp =
      if (!hasSeparator)
        SeparatorSuppressionPolicy.AnyEmpty
      else
        separatorSuppressionPolicy

    val res = (child, sequenceKind, ssp, ock, min, max) match {
      case (e: EB, Ordered__, ___________, __________, ONE, ONE) => new ScalarOrderedSequenceChild(this, e, groupIndex)
      case (e: EB, _________, ___________, StopValue_, ___, __2) => e.subsetError("dfdl:occursCountKind 'stopValue' is not supported.")
      case (e: EB, Ordered__, ___________, Parsed____, ___, __2) => new RepOrderedWithMinMaxSequenceChild(this, e, groupIndex)
      case (e: EB, Ordered__, ___________, Fixed_____, ___, UNB) => e.SDE("occursCountKind='fixed' not allowed with unbounded maxOccurs")
      case (e: EB, Ordered__, Never______, Implicit__, ___, UNB) if !e.isLastDeclaredRepresentedInSequence => unboundedPositionalError(e)
      case (e: EB, Ordered__, Trailing___, Implicit__, ___, UNB) if !e.isLastDeclaredRepresentedInSequence => unboundedPositionalError(e)
      case (e: EB, Ordered__, TrailingStr, Implicit__, ___, UNB) if !e.isLastDeclaredRepresentedInSequence => unboundedPositionalError(e)
      case (e: EB, Ordered__, ___________, Fixed_____, ___, `min`) => new RepOrderedExactlyNSequenceChild(this, e, groupIndex, min)
      case (e: EB, Ordered__, ___________, Fixed_____, ___, max) => { Assert.invariant(min != max); e.SDE("occursCountKind='fixed' requires minOccurs and maxOccurs to be equal (%d != %d)", min, max) }
      case (e: EB, Ordered__, ___________, Expression, ___, __2) => new RepOrderedExactlyTotalOccursCountSequenceChild(this, e, groupIndex)
      case (e: EB, Ordered__, Never______, Implicit__, ___, UNB) => e.SDE("separatorSuppressionPolicy='never' with occursCountKind='implicit' requires bounded maxOccurs.")
      case (e: EB, Ordered__, Never______, Implicit__, ___, max) => new RepOrderedExactlyNSequenceChild(this, e, groupIndex, max)
      case (e: EB, Ordered__, Never______, ock /****/ , ___, __2) if (ock ne null) => e.SDE("separatorSuppressionPolicy='never' not allowed in combination with occursCountKind='" + ock + "'.")
      case (e: EB, Ordered__, Trailing___, Implicit__, ___, max) => new RepOrderedWithMinMaxSequenceChild(this, e, groupIndex)
      case (e: EB, Ordered__, TrailingStr, Implicit__, ___, UNB) => new RepOrderedWithMinMaxSequenceChild(this, e, groupIndex)
      case (e: EB, Ordered__, TrailingStr, Implicit__, ___, max) => new RepOrderedWithMinMaxSequenceChild(this, e, groupIndex)
      case (e: EB, Ordered__, Always_____, Implicit__, ___, max) => new RepOrderedWithMinMaxSequenceChild(this, e, groupIndex)
      case (e: EB, Unordered, ___________, __________, ___, ONE) => new ScalarOrderedSequenceChild(this, e, groupIndex)
      case (e: EB, Unordered, ___________, Parsed____, ___, max) => new RepOrderedWithMinMaxSequenceChild(this, e, groupIndex)
      case (e: EB, Unordered, ___________, __________, ___, __2) => e.SDE("When sequenceKind='unordered', occursCountKind must be 'parsed'")
      case (m: MG, Unordered, ___________, __________, ___, __2) => child.SDE("All memebers of an unordered sequence must be Element or ElemenntRef")
      case (m: MG, _________, ___________, __________, ___, __2) => new ScalarOrderedSequenceChild(this, m, groupIndex)
      case (_____, _________, policy /**/ , ock /**/ , ___, __2) => child.SDE("separatorSuppressionPolicy='" + policy + "' not allowed with occursCountKind='" + ock + "'.")
    }
    res
  }

  private def unboundedPositionalError(e: ElementBase) =
    e.SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a positional sequence")

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

  /**
   * True if the term has a separator expressed on it.
   *
   * Do not confuse with the concept of the delimiter being able to match or not match zero-length data.
   * Whether the representation of a term in the data stream "has a separator", as in a specific separator
   * occupies a non-zero number of bits, is an entirely different question.
   */
  lazy val hasSeparator = separatorParseEv.isKnownNonEmpty

  lazy val sequenceSeparator = prod("separator", hasSeparator) {
    delimMTA ~ SequenceSeparator(this)
  }
}
