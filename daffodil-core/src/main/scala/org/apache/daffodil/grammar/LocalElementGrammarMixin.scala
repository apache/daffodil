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
import org.apache.daffodil.schema.annotation.props._
import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.equality._; object ENoWarn2 { EqualitySuppressUnusedImportWarning() }
import org.apache.daffodil.grammar.primitives.StopValue
import org.apache.daffodil.grammar.primitives.NotStopValue
import org.apache.daffodil.grammar.primitives.RepUnbounded
import org.apache.daffodil.grammar.primitives.RepExactlyTotalOccursCount
import org.apache.daffodil.grammar.primitives.RepExactlyTotalN
import org.apache.daffodil.grammar.primitives.RepExactlyN
import org.apache.daffodil.grammar.primitives.RepAtMostTotalN
import org.apache.daffodil.grammar.primitives.RepAtMostOccursCount
import org.apache.daffodil.grammar.primitives.OccursCountExpression
import org.apache.daffodil.grammar.primitives.OptionalCombinator
import org.apache.daffodil.grammar.primitives.ArrayCombinator

trait LocalElementGrammarMixin extends GrammarMixin { self: ElementBase =>

  override lazy val termContentBody = prod("termContentBody") { // override in ElementRef
    (if (isScalar) enclosedElement else recurrance)
  }

  protected final lazy val allowedValue = prod("allowedValue") { notStopValue | value }

  private lazy val notStopValue = prod("notStopValue", hasStopValue) { NotStopValue(this) }

  private lazy val separatedEmpty = prod("separatedEmpty",
    emptyIsAnObservableConcept && !isScalar) {
      separatedForArrayPosition(empty)
    }

  private lazy val separatedRecurring = prod("separatedRecurring", !isScalar) {
    separatedForArrayPosition(enclosedElement)
  }

  //  private lazy val separatedRecurringNonDefault = prod("separatedRecurringNonDefault", !isScalar) {
  //    separatedForArrayPosition(enclosedElementNonDefault)
  //  }

  private lazy val nonSeparatedScalarDefaultable = prod("nonSeparatedScalarDefaultable", isScalar) { enclosedElement }

  private lazy val recurrance = prod("recurrance", !isScalar) {
    if (isOptional) {
      OptionalCombinator(this, arrayContents)
    } else {
      ArrayCombinator(this, arrayContents)
    }
  }

  final override lazy val asTermInChoice = prod("asTermInChoice") {
    nonSeparatedScalarDefaultable || recurrance
  }

  /**
   * speculate parsing forward until we get an error
   */
  private lazy val separatedContentWithMinUnboundedWithoutTrailingEmpties = prod("separatedContentWithMinUnboundedWithoutTrailingEmpties", !isScalar) {
    RepExactlyN(self, minOccurs, separatedRecurring) ~
      RepUnbounded(self, separatedRecurring) ~
      StopValue(this)
  }

  private lazy val separatedContentWithMinAndMaxWithoutTrailingEmpties = prod("separatedContentWithMinAndMaxWithoutTrailingEmpties", !isScalar) {
    RepExactlyN(self, minOccurs, separatedRecurring) ~
      RepAtMostTotalN(self, maxOccurs, separatedRecurring) ~
      StopValue(this)
  }

  private lazy val separatedContentWithMinUnbounded = prod("separatedContentWithMinUnbounded", !isScalar) {
    separatedContentWithMinUnboundedWithoutTrailingEmpties // These are for tolerating trailing empties. Let's not tolerate them for now.
  }

  private lazy val separatedContentWithMinAndMax = prod("separatedContentWithMinAndMax", !isScalar) {
    separatedContentWithMinAndMaxWithoutTrailingEmpties // These are for tolerating trailing empties. Let's not tolerate them for now.
  }

  private lazy val separatedContentZeroToUnbounded = prod("separatedContentZeroToUnbounded", !isScalar) {
    RepUnbounded(self, separatedRecurring) ~
      StopValue(this)
  }

  private lazy val separatedContentAtMostNWithoutTrailingEmpties = prod("separatedContentAtMostNWithoutTrailingEmpties", !isScalar) {
    RepExactlyN(self, minOccurs, separatedRecurring) ~
      RepAtMostTotalN(this, maxOccurs, separatedRecurring) ~
      StopValue(this)
  }

  // TODO: Do we have to adjust the count to take stopValue into account?
  // Answer: No because the counts are never used when there is a stopValue (at least in current
  // thinking about how occursCountKind='stopValue' works.)

  private lazy val separatedContentAtMostN = prod("separatedContentAtMostN") {
    (separatedContentAtMostNWithoutTrailingEmpties // FIXME: We don't know whether we can absorb trailing separators or not here.
    // We don't know if this repeating thing is in trailing position, or in the middle
    // of a sequence. There is also ambiguity if the enclosing sequence and this sequence
    // have the same separator.
    //      ~
    //      RepAtMostTotalN(self, maxOccurs, separatedEmpty) // absorb extra separators, if found.
    )
  }

  /**
   *  parse counted number of occurrences exactly.
   */
  private lazy val stopValueSize = if (hasStopValue) 1 else 0

  // TODO FIXME: We really want to have different productions for parsing and unparsing in these
  // complex cases where there is defaulting, etc. Unparsing has many fewer cases, and is just not
  // symmetric with parsing in these situations.
  private def separatedContentExactlyN(count: Long) = prod("separatedContentExactlyN") {
    if (minOccurs == maxOccurs) {
      // fixed length case. All are defaultable. Still might have a stop value tho.
      RepExactlyN(self, count, separatedRecurring) ~
        StopValue(this)
    } else {
      // variable length case. So some defaultable, some not.
      RepExactlyN(self, minOccurs, separatedRecurring) ~
        RepAtMostTotalN(self, count, separatedRecurring) ~
        StopValue(this) ~
        RepExactlyTotalN(self, maxOccurs + stopValueSize, separatedEmpty) // absorb remaining separators after stop value.
    }
  }

  private lazy val separatedContentExactlyNComputed = prod("separatedContentExactlyNComputed") {
    OccursCountExpression(this) ~
      RepAtMostOccursCount(this, minOccurs, separatedRecurring) ~
      RepExactlyTotalOccursCount(this, separatedRecurring)
  }

  // keep in mind that anything here that scans for a representation either knows the length it is going after, or knows what the terminating markup is, and
  // our invariant is, that it does NOT consume that markup ever. The parser consumes it with appropriate grammar terminals.

  private val UNB = -1 // UNBOUNDED
  private val ZERO = 0 // ZERO

  lazy val arrayContents = prod("arrayContents", !isScalar) {
    arrayContentsNoSeparators || arrayContentsWithSeparators
  }

  private lazy val contentUnbounded = prod("contentUnbounded") {
    RepUnbounded(self, separatedRecurring)
  }

  //
  // Silly constants to make the lookup tables below more readable without using fragile whitespace
  //
  private val Never______ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.Never
  private val Trailing___ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.TrailingEmpty
  private val TrailingStr: SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.TrailingEmptyStrict
  private val Always_____ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.AnyEmpty

  private val StopValue_ = OccursCountKind.StopValue
  private val Implicit__ = OccursCountKind.Implicit
  private val Parsed____ = OccursCountKind.Parsed
  private val Fixed_____ = OccursCountKind.Fixed
  private val Expression = OccursCountKind.Expression

  private lazy val arrayContentsNoSeparators = prod("arrayContentsNoSeparators", !isScalar && !hasSep) {
    val res = (occursCountKind, minOccurs, maxOccurs) match {
      case (Expression, ____, __2) => separatedContentExactlyNComputed
      case (Fixed_____, ____, UNB) => SDE("occursCountKind='fixed' not allowed with unbounded maxOccurs")
      case (Fixed_____, min_, max) if (min_ != max) => SDE("occursCountKind='fixed' requires minOccurs and maxOccurs to be equal (%d != %d)", min_, max)
      case (Fixed_____, ____, max) => separatedContentExactlyN(max)
      case (Implicit__, ZERO, UNB) => contentUnbounded // same as parsed
      case (Implicit__, min_, UNB) => RepExactlyN(self, min_, separatedRecurring) ~ contentUnbounded // respects minOccurs
      case (Implicit__, ____, __2) => separatedContentAtMostN // uses min and maxOccurs
      case (Parsed____, ____, __2) => contentUnbounded
      case (StopValue_, ____, __2) => contentUnbounded
    }
    res
  }

  /**
   * Matches the table about separator suppression policy.
   *
   * TODO: Update this table to match the final spec.
   */
  private lazy val arrayContentsWithSeparators = prod("arrayContentsWithSeparators", !isScalar && hasSep) {
    val triple = (separatorSuppressionPolicy, occursCountKind, maxOccurs, minOccurs)
    val res = triple match {
      case (___________, Expression, ___, __2) => separatedContentExactlyNComputed
      case (___________, Fixed_____, UNB, ___) => SDE("occursCountKind='fixed' not allowed with unbounded maxOccurs")
      case (___________, Fixed_____, max, min) if (max != min) => SDE("occursCountKind='fixed' requires minOccurs to equal maxOccurs (%d != %d)", minOccurs, max)
      case (___________, Fixed_____, max, ___) => separatedContentExactlyN(max)
      case (Never______, Implicit__, UNB, ___) => SDE("separatorSuppressionPolicy='never' with occursCountKind='implicit' required bounded maxOccurs.")
      case (Never______, Implicit__, max, ___) => separatedContentExactlyN(max)
      case (Never______, ock /****/ , ___, __2) => SDE("separatorSuppressionPolicy='never' not allowed in combination with occursCountKind='" + ock + "'.")
      case (Trailing___, Implicit__, UNB, ___) if (!isLastDeclaredRequiredElementOfSequence) => SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a sequence")
      case (Trailing___, Implicit__, UNB, min) => separatedContentWithMinUnbounded
      case (Trailing___, Implicit__, max, min) if min > 0 => separatedContentWithMinAndMax
      case (Trailing___, Implicit__, max, ___) => separatedContentAtMostN // FIXME: have to have all of them - not trailing position
      case (TrailingStr, Implicit__, UNB, ___) if (!isLastDeclaredRequiredElementOfSequence) => SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a sequence")
      case (TrailingStr, Implicit__, UNB, ___) => separatedContentWithMinUnboundedWithoutTrailingEmpties // we're depending on optionalEmptyPart failing on empty content.
      case (TrailingStr, Implicit__, max, ___) => separatedContentAtMostNWithoutTrailingEmpties
      case (Always_____, Implicit__, UNB, ___) => separatedContentWithMinUnbounded
      case (Always_____, Implicit__, max, ___) => separatedContentAtMostN
      case (Always_____, Parsed____, ___, __2) => separatedContentZeroToUnbounded
      case (Always_____, StopValue_, ___, __2) => separatedContentZeroToUnbounded
      case (policy /**/ , ock /****/ , max, __2) => SDE("separatorSuppressionPolicy='" + policy + "' not allowed with occursCountKind='" + ock + "'.")
    }
    res
  }
}
