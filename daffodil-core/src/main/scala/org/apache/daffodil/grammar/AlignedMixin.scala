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

import org.apache.daffodil.dsom.Term
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.dsom.ModelGroup
import org.apache.daffodil.schema.annotation.props.gen.AlignmentUnits
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.util.Math
import org.apache.daffodil.dsom.Root

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

  /**
   * true if we can statically determine that the start of this
   * will be properly aligned by where the prior thing left us positioned.
   * Hence we are guaranteed to be properly aligned.
   */
  final def isKnownToBeAligned: Boolean = LV('isKnownToBeAligned) {
    if (!isRepresented) {
      true
    } else {
      val pa = priorAlignmentWithLeadingSkipApprox
      val aa = alignmentApprox
      val res = (pa % aa) == 0
      res
    }
  }.value

  final lazy val isKnownToBeTextAligned: Boolean = {
    if (self.encodingInfo.isKnownEncoding) {
      if (self.encodingInfo.knownEncodingAlignmentInBits == 1)
        true
      else if (priorAlignmentWithLeadingSkipApprox.nBits % self.encodingInfo.knownEncodingAlignmentInBits == 0)
        true
      else
        false
    } else if (this.rootElementRef.get.isScannable)
      true
    else
      false
  }

  final lazy val isDelimiterKnownToBeTextAligned: Boolean = {
    if (self.encodingInfo.isKnownEncoding) {
      if (self.encodingInfo.knownEncodingAlignmentInBits == 1)
        true
      else if (endingAlignmentApprox.nBits % self.encodingInfo.knownEncodingAlignmentInBits == 0)
        true
      else
        false
    } else if (this.rootElementRef.get.isScannable)
      true
    else
      false
  }

  final lazy val hasNoSkipRegions = leadingSkip == 0 && trailingSkip == 0

  private lazy val alignmentApprox: AlignmentMultipleOf = {
    AlignmentMultipleOf(alignmentValueInBits.toLong)
  }

  lazy val leadingSkipInBits = {
    alignmentUnits match {
      case AlignmentUnits.Bits => leadingSkip
      case AlignmentUnits.Bytes => leadingSkip * 8
    }
  }

  private lazy val leadingSkipApprox: LengthApprox = {
    LengthExact(leadingSkipInBits)
  }

  lazy val trailingSkipInBits = {
    alignmentUnits match {
      case AlignmentUnits.Bits => trailingSkip
      case AlignmentUnits.Bytes => trailingSkip * 8
    }
  }

  private lazy val trailingSkipApprox: LengthApprox = {
    LengthExact(trailingSkipInBits)
  }

  private lazy val priorAlignmentApprox: AlignmentMultipleOf = {
    if (this.isInstanceOf[Root]) {
      AlignmentMultipleOf(0) // root is aligned with anything
    } else {
      val (priorSibs, parent) = potentialPriorTerms
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
            // children. But we can't know whree it starts without knowing
            // where the previous one array element ends. And we end up in a
            // loop.
            //
            // So there isn't much we can do regarding alignement. What we can
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

      val priorAlignmentsApprox = priorSibs.map(_.endingAlignmentApprox) ++ parent.map(_.contentStartAlignment).toSeq ++ arraySelfAlignment
      priorAlignmentsApprox.reduce(_ * _)
    }
  }

  private lazy val priorAlignmentWithLeadingSkipApprox: AlignmentMultipleOf = {
    priorAlignmentApprox + leadingSkipApprox
  }

  protected lazy val contentStartAlignment: AlignmentMultipleOf = {
    if ((priorAlignmentWithLeadingSkipApprox) % alignmentApprox == 0) {
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
        val (lastChildren, couldBeLast) = mg.potentialLastChildren
        val lastApprox = lastChildren.map(_.endingAlignmentApprox + trailingSkipApprox) ++ (if (couldBeLast) Seq(contentStartAlignment + trailingSkipApprox) else Seq())
        lastApprox.reduce { _ * _ }
      }
    }
  }

  private lazy val elementSpecifiedLengthApprox: LengthApprox = {
    this match {
      case eb: ElementBase => {
        eb.lengthKind match {
          case LengthKind.Implicit => {
            // asssert this is simple element base
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
            } else eb.lengthUnits match {
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
    }
  }

  private lazy val encodingLengthApprox: LengthApprox = {
    if (isKnownEncoding) {
      val dfdlCharset = charsetEv.optConstant.get
      val fixedWidth = if (dfdlCharset.maybeFixedWidth.isDefined) dfdlCharset.maybeFixedWidth.get else 8
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
          val isSelfByteSizeEncoding = eb.charsetEv.optConstant.map {
            _.bitWidthOfACodeUnit == 8
          }.getOrElse(false)
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
