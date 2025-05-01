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

package org.apache.daffodil.core.grammar

import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.dsom.ModelGroup
import org.apache.daffodil.core.dsom.QuasiElementDeclBase
import org.apache.daffodil.core.dsom.Root
import org.apache.daffodil.core.dsom.Term
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.AlignmentKind
import org.apache.daffodil.lib.schema.annotation.props.gen.AlignmentUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.util.Math

case class AlignmentMultipleOf(nBits: Long) {
  def *(that: AlignmentMultipleOf) = AlignmentMultipleOf(Math.gcd(nBits, that.nBits))
  def %(that: AlignmentMultipleOf) = nBits % that.nBits
  def +(that: LengthApprox) = AlignmentMultipleOf(Math.gcd(nBits, nBits + that.nBits))
}

trait LengthApprox {
  val nBits: Long
  def +(that: LengthApprox): LengthApprox = (this, that) match {
    case (l: LengthExact, r: LengthExact) => LengthExact(l.nBits + r.nBits)
    case (l, r) => LengthMultipleOf(Math.gcd(l.nBits, r.nBits))
  }
}
case class LengthExact(nBits: Long) extends LengthApprox
case class LengthMultipleOf(nBits: Long) extends LengthApprox

trait AlignedMixin extends GrammarMixin { self: Term =>

  requiredEvaluationsIfActivated(hasNoSkipRegions)

  /**
   * If "manual" this property disables all automatic alignment. The
   * schema author must use leadingSkip, trailingSkip, or just ensure
   * all the elements/terms are aligned based on their length.
   */
  lazy val alignmentKindDefaulted: AlignmentKind =
    optionAlignmentKind.getOrElse(AlignmentKind.Automatic)

  /**
   * true if we can statically determine that the start of this
   * will be properly aligned by where the prior thing left us positioned.
   * Hence we are guaranteed to be properly aligned.
   */
  final lazy val isKnownToBeAligned: Boolean = LV(Symbol("isKnownToBeAligned")) {
    if (!isRepresented || (alignmentKindDefaulted == AlignmentKind.Manual)) true
    else {
      val pa = priorAlignmentWithLeadingSkipApprox
      val aa = alignmentApprox
      val res = (pa % aa) == 0
      res
    }
  }.value

  /**
   * True if alignment for a text feature of this Term (e.g., an initiator)
   * is provably not needed, either because there is no requirement for such
   * alignment, or we can prove that the required alignment is already established.
   *
   * This goes further TermEncodingMixin.hasTextAlignment because it
   * considers the surrounding context meeting the alignment needs.
   */
  final lazy val isKnownToBeTextAligned: Boolean = LV(Symbol("isKnownToBeTextAligned")) {
    if (alignmentKindDefaulted == AlignmentKind.Manual) true // manual alignment
    else if (isKnownEncoding) {
      if (knownEncodingAlignmentInBits == 1)
        true
      else if (priorAlignmentWithLeadingSkipApprox.nBits % knownEncodingAlignmentInBits == 0)
        true
      else
        false
    } else if (schemaSet.root.isScannable)
      true
    else
      false
  }.value

  final lazy val isDelimiterKnownToBeTextAligned: Boolean = {
    if (alignmentKindDefaulted == AlignmentKind.Manual) true // manual alignment
    else if (isKnownEncoding) {
      if (knownEncodingAlignmentInBits == 1)
        true
      else if (endingAlignmentApprox.nBits % knownEncodingAlignmentInBits == 0)
        true
      else
        false
    } else if (schemaSet.root.isScannable)
      true
    else
      false
  }

  final lazy val hasNoSkipRegions = LV(Symbol("hasNoSkipRegions")) {
    leadingSkip == 0 && trailingSkip == 0
  }.value

  private lazy val alignmentApprox: AlignmentMultipleOf = {
    AlignmentMultipleOf(alignmentValueInBits.toLong)
  }

  private def alignmentSkipInBits(skipProp: Int) = alignmentUnits match {
    case AlignmentUnits.Bits => skipProp
    case AlignmentUnits.Bytes => skipProp * 8
  }

  lazy val leadingSkipInBits = alignmentSkipInBits(leadingSkip)
  lazy val trailingSkipInBits = alignmentSkipInBits(trailingSkip)

  private lazy val leadingSkipApprox: LengthApprox = {
    LengthExact(leadingSkipInBits)
  }

  protected lazy val trailingSkipApprox: LengthApprox = {
    LengthExact(trailingSkipInBits)
  }

  // FIXME: DAFFODIL-2295
  // Does not take into account that in a sequence, what may be prior may be a separator.
  // The separator is text in some encoding, might not be the same as this term's encoding, and
  // the alignment will be left where that text leaves it.
  //
  private lazy val priorAlignmentApprox: AlignmentMultipleOf =
    LV(Symbol("priorAlignmentApprox")) {
      if (this.isInstanceOf[Root] || this.isInstanceOf[QuasiElementDeclBase]) {
        AlignmentMultipleOf(
          0
        ) // root and quasi elements are aligned with anything // TODO: really? Why quasi-elements - they should have implicit alignment ?
      } else {
        val priorSibs = potentialPriorTerms
        val arraySelfAlignment =
          if (isArray) {
            val e = this.asInstanceOf[ElementBase]
            if (e.isComplexType && e.lengthKind == LengthKind.Implicit) {
              // Array of a complex type with implicit length. In this case, it
              // is not possible to calculate the approximate prior alignment of
              // the previous element. This is because the prior alignment could
              // come from a previous element of this same array. Since this
              // array element is implicit length, knowing where it ends requires
              // knowing where it starts and the approximate length of the
              // children. But we can't know where it starts without knowing
              // where the previous one array element ends. And we end up in a
              // loop.
              //
              // So there isn't much we can do regarding alignment. What we can
              // do is determine if this array element is byte aligned AND all of
              // its children are byte lengths/byte aligned, if that is the case
              // then we at least know this array and its elements are byte
              // aligned. If that isn't the case, there isn't much we can do.
              if (isKnownToBeByteAlignedAndByteLength) {
                Seq(AlignmentMultipleOf(8))
              } else {
                Seq(AlignmentMultipleOf(1))
              }
            } else {
              // If this is an array, it's prior alignment could be it's own alignment.
              // We really want to use this.endingAlignmentApprox since that
              // takes into account the previous alignments, lengths, and leading
              // skips. However, we cannot use that since that method ends up
              // calling this method, which results in a circular loop. So,
              // instead just use calculate this arrays ending alignment not
              // taking into account it's previous alignment.
              Seq(alignmentApprox + (elementSpecifiedLengthApprox + trailingSkipApprox))
            }
          } else {
            Seq()
          }

        val unorderedSequenceSelfAlignment =
          if (isEverInUnorderedSequence) {
            if (isKnownToBeByteAlignedAndByteLength) {
              Seq(AlignmentMultipleOf(8))
            } else {
              Seq(AlignmentMultipleOf(1))
            }
          } else {
            Seq()
          }

        val priorSibsAlignmentsApprox = priorSibs.map { ps =>
          val eaa = if (isEverInUnorderedSequence) {
            // Return 0 here, unordered alignment will be handled by unorderedSequenceSelfAlignment
            AlignmentMultipleOf(0)
          } else {
            ps.endingAlignmentApprox
          }
          eaa
        }

        val parentAlignmentApprox =
          if (priorSibs.isEmpty || isEverInUnorderedSequence) {
            // we only care about the parent alignment if we are the first child
            // in a model group, or if we are in an unordered sequence and we
            // could be first when parsing
            immediatelyEnclosingModelGroup.map { p =>
              val csa = p.contentStartAlignment
              csa
            }.toSeq
          } else {
            Seq()
          }

        val priorAlignmentsApprox =
          priorSibsAlignmentsApprox ++ parentAlignmentApprox ++ arraySelfAlignment ++ unorderedSequenceSelfAlignment
        if (priorAlignmentsApprox.isEmpty)
          alignmentApprox // it will be the containing context's responsibility to insure this IS where we start.
        else
          priorAlignmentsApprox.reduce(_ * _)
      }
    }.value

  private lazy val priorAlignmentWithLeadingSkipApprox: AlignmentMultipleOf = {
    priorAlignmentApprox + leadingSkipApprox
  }

  protected lazy val contentStartAlignment: AlignmentMultipleOf = {
    if (priorAlignmentWithLeadingSkipApprox % alignmentApprox == 0) {
      // alignment won't be needed, continue using prior alignment as start alignment
      priorAlignmentWithLeadingSkipApprox
    } else {
      // alignment will be needed, it will be forced to be aligned to alignmentApprox
      alignmentApprox
    }
  }

  protected lazy val endingAlignmentApprox: AlignmentMultipleOf = {
    this match {
      case eb: ElementBase => {
        if (eb.isComplexType && eb.lengthKind == LengthKind.Implicit) {
          eb.complexType.group.endingAlignmentApprox + trailingSkipApprox
        } else {
          // simple type or complex type with specified length
          contentStartAlignment + (elementSpecifiedLengthApprox + trailingSkipApprox)
        }
      }
      case mg: ModelGroup => {
        //
        // We're interested in how the model group ends. Whatever could be
        // the last term, each individual such possibility has an endingAlignmentApprox,
        // and we need to aggregate those to get the summary endingAlignmentApprox.
        //
        val (lastChildren, couldBeLast) = mg.potentialLastChildren
        val lastApproxesConsideringChildren: Seq[AlignmentMultipleOf] = lastChildren.map { lc =>
          //
          // for each possible last child, add its ending alignment
          // to our trailing skip to get where it would leave off were
          // it the actual last child.
          //
          val lceaa = lc.endingAlignmentApprox
          val res = lceaa + trailingSkipApprox
          res
        }
        val optApproxIfNoChildren =
          //
          // gather possibilities for this item itself
          //
          if (couldBeLast)
            //
            // if this model group could be last, then consider
            // if none of its content was present.
            // We'd just have the contentStart, nothing, and the trailing Skip.
            //
            Some(contentStartAlignment + trailingSkipApprox)
          else
            // can't be last, no possibilities to gather.
            None
        val lastApproxes = lastApproxesConsideringChildren ++ optApproxIfNoChildren
        Assert.invariant(lastApproxes.nonEmpty)
        val res = lastApproxes.reduce { _ * _ }
        res
      }
    }
  }

  protected lazy val elementSpecifiedLengthApprox: LengthApprox = {
    this match {
      case eb: ElementBase => {
        eb.lengthKind match {
          case LengthKind.Implicit => {
            // assert this is simple element base
            LengthExact(eb.elementLengthInBitsEv.optConstant.get.get)
          }
          case LengthKind.Explicit => {
            if (eb.elementLengthInBitsEv.optConstant.isDefined) {
              val maybeLength = eb.elementLengthInBitsEv.optConstant.get
              if (maybeLength.isDefined) {
                LengthExact(maybeLength.get)
              } else {
                // this only occurs when lengthUnits="characters", but the
                // charset does not have a fixed width. In that case, we know
                // it's not a non-byte size charset, so the length must be a
                // multiple of 8
                LengthMultipleOf(8)
              }
            } else
              eb.lengthUnits match {
                case LengthUnits.Bits => LengthMultipleOf(1)
                case LengthUnits.Bytes => LengthMultipleOf(8)
                case LengthUnits.Characters => encodingLengthApprox
              }
          }
          case LengthKind.Delimited => encodingLengthApprox
          case LengthKind.Pattern => encodingLengthApprox
          case LengthKind.EndOfParent => LengthMultipleOf(1) // NYI
          case LengthKind.Prefixed => LengthMultipleOf(1) // NYI
        }
      }
      case _: ModelGroup => Assert.usageError("Only for elements")
    }
  }

  private lazy val encodingLengthApprox: LengthApprox = {
    if (isKnownEncoding) {
      val dfdlCharset = charsetEv.optConstant.get
      val fixedWidth =
        if (dfdlCharset.maybeFixedWidth.isDefined) dfdlCharset.maybeFixedWidth.get else 8
      LengthMultipleOf(fixedWidth)
    } else {
      // runtime encoding, which must be a byte-length encoding
      LengthMultipleOf(8)
    }
  }

  protected lazy val isKnownToBeByteAlignedAndByteLength: Boolean = {
    if (isRepresented) {
      val isByteAligned = alignmentValueInBits == 8
      val isSkipRegionByteLength = (leadingSkipInBits % 8 == 0) && (trailingSkipInBits % 8 == 0)

      val isByteLength = this match {
        case mg: ModelGroup => mg.groupMembers.forall { _.isKnownToBeByteAlignedAndByteLength }
        case eb: ElementBase => {
          val isSelfByteSizeEncoding =
            eb.charsetEv.optConstant.exists(_.bitWidthOfACodeUnit == 8)
          val isSelfByteLength =
            if (eb.isComplexType && eb.lengthKind == LengthKind.Implicit) {
              eb.complexType.group.isKnownToBeByteAlignedAndByteLength
            } else {
              elementSpecifiedLengthApprox.nBits % 8 == 0
            }
          isSelfByteSizeEncoding && isSelfByteLength
        }
      }

      isByteAligned && isSkipRegionByteLength && isByteLength
    } else {
      true
    }
  }

}
