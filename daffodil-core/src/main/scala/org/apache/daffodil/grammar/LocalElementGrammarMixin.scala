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

package edu.illinois.ncsa.daffodil.grammar
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.equality._; object ENoWarn2 { EqualitySuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.grammar.primitives.StopValue
import edu.illinois.ncsa.daffodil.grammar.primitives.NotStopValue
import edu.illinois.ncsa.daffodil.grammar.primitives.RepUnbounded
import edu.illinois.ncsa.daffodil.grammar.primitives.RepExactlyTotalOccursCount
import edu.illinois.ncsa.daffodil.grammar.primitives.RepExactlyTotalN
import edu.illinois.ncsa.daffodil.grammar.primitives.RepExactlyN
import edu.illinois.ncsa.daffodil.grammar.primitives.RepAtMostTotalN
import edu.illinois.ncsa.daffodil.grammar.primitives.RepAtMostOccursCount
import edu.illinois.ncsa.daffodil.grammar.primitives.OccursCountExpression
import edu.illinois.ncsa.daffodil.grammar.primitives.OptionalCombinator
import edu.illinois.ncsa.daffodil.grammar.primitives.ArrayCombinator

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

  private lazy val separatedRecurringDefaultable = prod("separatedRecurringDefaultable", !isScalar) {
    separatedForArrayPosition(enclosedElement)
  }

  private lazy val separatedRecurringNonDefault = prod("separatedRecurringNonDefault", !isScalar) {
    separatedForArrayPosition(enclosedElementNonDefault)
  }

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
    RepExactlyN(self, minOccurs, separatedRecurringDefaultable) ~
      RepUnbounded(self, separatedRecurringNonDefault) ~
      StopValue(this)
  }

  private lazy val separatedContentWithMinAndMaxWithoutTrailingEmpties = prod("separatedContentWithMinAndMaxWithoutTrailingEmpties", !isScalar) {
    RepExactlyN(self, minOccurs, separatedRecurringDefaultable) ~
      RepAtMostTotalN(self, maxOccurs, separatedRecurringNonDefault) ~
      StopValue(this)
  }

  private lazy val separatedContentWithMinUnbounded = prod("separatedContentWithMinUnbounded", !isScalar) {
    separatedContentWithMinUnboundedWithoutTrailingEmpties // These are for tolerating trailing empties. Let's not tolerate them for now.
  }

  private lazy val separatedContentWithMinAndMax = prod("separatedContentWithMinAndMax", !isScalar) {
    separatedContentWithMinAndMaxWithoutTrailingEmpties // These are for tolerating trailing empties. Let's not tolerate them for now.
  }

  private lazy val separatedContentZeroToUnbounded = prod("separatedContentZeroToUnbounded", !isScalar) {
    RepUnbounded(self, separatedRecurringNonDefault) ~
      StopValue(this)
  }

  private lazy val separatedContentAtMostNWithoutTrailingEmpties = prod("separatedContentAtMostNWithoutTrailingEmpties", !isScalar) {
    RepExactlyN(self, minOccurs, separatedRecurringDefaultable) ~
      RepAtMostTotalN(this, maxOccurs, separatedRecurringNonDefault) ~
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
      RepExactlyN(self, count, separatedRecurringDefaultable) ~
        StopValue(this)
    } else {
      // variable length case. So some defaultable, some not.
      RepExactlyN(self, minOccurs, separatedRecurringDefaultable) ~
        RepAtMostTotalN(self, count, separatedRecurringNonDefault) ~
        StopValue(this) ~
        RepExactlyTotalN(self, maxOccurs + stopValueSize, separatedEmpty) // absorb remaining separators after stop value.
    }
  }

  private lazy val separatedContentExactlyNComputed = prod("separatedContentExactlyNComputed") {
    OccursCountExpression(this) ~
      RepAtMostOccursCount(this, minOccurs, separatedRecurringDefaultable) ~
      RepExactlyTotalOccursCount(this, separatedRecurringNonDefault)
  }

  // keep in mind that anything here that scans for a representation either knows the length it is going after, or knows what the terminating markup is, and
  // our invariant is, that it does NOT consume that markup ever. The parser consumes it with appropriate grammar terminals.

  private val UNB = -1 // UNBOUNDED
  private val ZERO = 0 // ZERO

  lazy val arrayContents = prod("arrayContents", !isScalar) {
    arrayContentsNoSeparators || arrayContentsWithSeparators
  }

  private lazy val contentUnbounded = prod("contentUnbounded") {
    RepUnbounded(self, separatedRecurringDefaultable)
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
      case (Implicit__, min_, UNB) => RepExactlyN(self, min_, separatedRecurringDefaultable) ~ contentUnbounded // respects minOccurs
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
