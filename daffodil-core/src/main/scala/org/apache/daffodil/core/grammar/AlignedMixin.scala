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
import org.apache.daffodil.core.dsom.SequenceTermBase
import org.apache.daffodil.core.dsom.Term
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.AlignmentKind
import org.apache.daffodil.lib.schema.annotation.props.gen.AlignmentUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind.Prefixed
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.lib.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.lib.util.Math

case class AlignmentMultipleOf(nBits: Long) {
  // To combine alignments that could all happen at the same point in data, use *,
  // To add new alignment to an existing approximate alignment, use +
  def *(that: AlignmentMultipleOf) = AlignmentMultipleOf(Math.gcd(nBits, that.nBits))
  def %(that: AlignmentMultipleOf) = nBits % that.nBits
  def +(that: AlignmentMultipleOf) =
    if (this.nBits % that.nBits == 0) this else that
  // To combine AlignmentMultipleOfs and lengths, use +
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
    val res =
      if (!isRepresented || (alignmentKindDefaulted == AlignmentKind.Manual)) true
      else {
        val pa = priorAlignmentWithLeadingSkipApprox
        val aa = alignmentApprox
        (pa % aa) == 0
      }
    res
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
    val res =
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
    res
  }.value

  final lazy val isDelimiterKnownToBeTextAligned: Boolean = {
    val res =
      if (alignmentKindDefaulted == AlignmentKind.Manual) true // manual alignment
      else if (isKnownEncoding) {
        if (knownEncodingAlignmentInBits == 1)
          true
        else if (contentEndAlignment.nBits % knownEncodingAlignmentInBits == 0)
          true
        else
          false
      } else if (schemaSet.root.isScannable)
        true
      else
        false
    res
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

  /**
   * The priorAlignmentApprox doesn't need to take into account the separator
   * alignment/length as that comes after the priorAlignmentApprox, but before
   * the leadingSkip.
   */
  private lazy val priorAlignmentApprox: AlignmentMultipleOf =
    LV(Symbol("priorAlignmentApprox")) {
      if (this.isInstanceOf[Root] || this.isInstanceOf[QuasiElementDeclBase]) {
        AlignmentMultipleOf(
          0
        ) // root and quasi elements are aligned with anything (quasi elements are always 1 aligned)
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
            ps.endingAlignmentWithRightFramingApprox
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
          alignmentApprox // it will be the containing context's responsibility to ensure this IS where we start.
        else
          priorAlignmentsApprox.reduce(_ * _)
      }
    }.value

  private lazy val separatorPrefixInfixMTAApprox =
    this.optLexicalParent match {
      case Some(s: SequenceTermBase) if s.hasSeparator =>
        import SeparatorPosition.*
        s.separatorPosition match {
          case Prefix | Infix => AlignmentMultipleOf(s.knownEncodingAlignmentInBits)
          case Postfix => AlignmentMultipleOf(1)
        }
      case _ => AlignmentMultipleOf(1)
    }

  private lazy val separatorPostfixMTAApprox =
    this.optLexicalParent match {
      case Some(s: SequenceTermBase) if s.hasSeparator =>
        import SeparatorPosition.*
        s.separatorPosition match {
          case Prefix | Infix => AlignmentMultipleOf(1)
          case Postfix => AlignmentMultipleOf(s.knownEncodingAlignmentInBits)
        }
      case _ => AlignmentMultipleOf(1)
    }

  private lazy val separatorLengthApprox = this.optLexicalParent match {
    case Some(s: SequenceTermBase) if s.hasSeparator =>
      s.encodingLengthApprox
    case _ => LengthExact(0)
  }

  private lazy val initiatorMTAApprox =
    if (this.hasInitiator) {
      AlignmentMultipleOf(this.knownEncodingAlignmentInBits)
    } else {
      AlignmentMultipleOf(1)
    }

  private lazy val initiatorLengthApprox = if (this.hasInitiator) {
    this.encodingLengthApprox
  } else {
    LengthExact(0)
  }

  private lazy val terminatorMTAApprox =
    if (this.hasTerminator) {
      AlignmentMultipleOf(this.knownEncodingAlignmentInBits)
    } else {
      AlignmentMultipleOf(1)
    }

  private lazy val terminatorLengthApprox = if (this.hasTerminator) {
    this.encodingLengthApprox
  } else {
    LengthExact(0)
  }

  /**
   * prior alignment with leading skip includes the prefix/infix separator MTA and length,
   * any leading skips
   */
  private lazy val priorAlignmentWithLeadingSkipApprox: AlignmentMultipleOf = {
    val priorAlignmentWithSeparatorApprox = priorAlignmentApprox
      + separatorPrefixInfixMTAApprox
      + separatorLengthApprox
    val leadingAlignmentApprox = (priorAlignmentWithSeparatorApprox
      + leadingSkipApprox)
    leadingAlignmentApprox
  }

  /**
   * The length of the prefix length quasi element.
   *
   * This is only relevant for quasi elements, or prefixed terms.
   */
  protected lazy val prefixLengthElementLength: LengthApprox = {
    this match {
      case eb: ElementBase => {
        if (eb.lengthKind == Prefixed) {
          val prefixTypeElem = eb.prefixedLengthElementDecl
          prefixTypeElem.elementSpecifiedLengthApprox
        } else {
          LengthExact(0)
        }
      }
      case _ => LengthExact(0)
    }
  }

  /**
   * The alignment of the term's value.
   *
   * This is only relevant for simple elements with textual representation.
   */
  private lazy val valueMTAApprox = {
    this match {
      case eb: ElementBase if eb.isSimpleType && eb.representation == Representation.Text => {
        AlignmentMultipleOf(eb.knownEncodingAlignmentInBits)
      }
      case _ => AlignmentMultipleOf(1)
    }
  }

  /**
   * This alignment is made up of the alignment of the term itself,
   * the initiator MTA and length, the prefix length quasi
   * element length, and the value MTA (we add it for optimization
   * but is extremely difficult to test, as other things such
   * as unparsers will provide MTA, even elementSpecifiedLengthApprox
   * considers the encoding also).
   */
  protected lazy val contentStartAlignment: AlignmentMultipleOf = {
    val leftFramingApprox = priorAlignmentWithLeadingSkipApprox
      + alignmentApprox
      + initiatorMTAApprox
      + initiatorLengthApprox
    val csa = (leftFramingApprox + prefixLengthElementLength) + valueMTAApprox
    csa
  }

  /**
   * Accounts for the start of the content start alignment and element length.
   * This is used to determine the alignment before the terminator and trailing skip
   */
  protected lazy val contentEndAlignment: AlignmentMultipleOf = {
    this match {
      case eb: ElementBase => {
        val res = if (eb.isComplexType && eb.lengthKind == LengthKind.Implicit) {
          eb.complexType.group.contentEndAlignment
        } else {
          // simple type or complex type with specified length
          contentStartAlignment + elementSpecifiedLengthApprox
        }
        res
      }
      case mg: ModelGroup => {
        //
        // We're interested in how the model group ends. Whatever could be
        // the last term, each individual such possibility has an endingAlignmentWithRightFramingApprox,
        // and we need to aggregate those to get the summary endingAlignmentWithRightFramingApprox.
        //
        val (lastChildren, couldBeLast) = mg.potentialLastChildren
        val lastApproxesConsideringChildren: Seq[AlignmentMultipleOf] = lastChildren.map { lc =>
          // for each possible last child, gather its endingAlignmentWithRightFramingApprox
          // as we want to account for the separators that are a part of the SequenceContent (see DFDL spec)
          val lceaa = lc.endingAlignmentWithRightFramingApprox
          lceaa
        }
        val optApproxIfNoChildren =
          //
          // gather possibilities for this item itself
          //
          if (couldBeLast) {
            //
            // if this model group could be last, then consider
            // if none of its content was present.
            // We'd just have the contentStart
            //
            Some(contentStartAlignment)
          } else
            // can't be last, no possibilities to gather.
            None
        val lastApproxes = lastApproxesConsideringChildren ++ optApproxIfNoChildren
        // take all the gathered possibilities to get where it would leave off were
        // each the actual last child.
        Assert.invariant(lastApproxes.nonEmpty)
        lastApproxes.reduce(_ * _)
      }
    }
  }

  /**
   * Add the ending alignment of the term
   * to terminator MTA/length and our trailing skip
   *
   * The postfix separator MTA/length needs to be added after the trailing skip
   */
  protected lazy val endingAlignmentWithRightFramingApprox: AlignmentMultipleOf = {
    val endingAlignmentApprox = this.contentEndAlignment
      + terminatorMTAApprox
      + terminatorLengthApprox
      + trailingSkipApprox
    val res = endingAlignmentApprox
      + separatorPostfixMTAApprox
      + separatorLengthApprox
    res
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
                case LengthUnits.Characters => eb.encodingLengthApprox
              }
          }
          case LengthKind.Delimited => encodingLengthApprox
          case LengthKind.Pattern => encodingLengthApprox
          case LengthKind.EndOfParent => LengthMultipleOf(1) // NYI
          // If an element is lengthKind="prefixed", the element's length is the length
          // of the value of the prefix element, which can't be known till runtime
          case LengthKind.Prefixed => LengthMultipleOf(1) // NYI (see DAFFODIL-3066)
        }
      }
      case _: ModelGroup => Assert.usageError("Only for elements")
    }
  }

  protected lazy val encodingLengthApprox: LengthApprox = {
    if (this.isKnownEncoding) {
      val dfdlCharset = this.charsetEv.optConstant.get
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
