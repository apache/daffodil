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

package org.apache.daffodil.core.grammar.primitives

import org.apache.daffodil.core.dsom._
import org.apache.daffodil.core.grammar._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.processors.parsers._
import org.apache.daffodil.unparsers.runtime1._

/**
 * A SequenceChild is exactly that, a child Term of a Sequence
 *
 * Methods/members of this class combine information about a child term
 * with that about the sequence itself. This class is really a part of the
 * state of the sequence object.
 *
 * This is the class primarily responsible for compilation of sequence
 * and term information into digested form for the runtime, such
 * that the runtime can properly implement complex DFDL behaviors like
 * separator suppression and emptyElementParsePolicy.
 *
 * These objects are part of the Gram object hierarchy.
 * They represent the use of a Term in a context. They are
 * objects that belong to (are owned by exactly one) the enclosing sequence, are part of it, and
 * so it is reasonable for a SequenceChild to have a backpointer to the
 * enclosing Sequence object in all cases, and this is passed to them on construction.
 * This particular backpointer is
 * not a challenge to sharing substructure as these objects cannot and are
 * not intended to be shared. They really are state of the enclosing sequence
 * broken out for convenience.
 *
 * This allows the child (of the Sequence) Term object (dsom object) that provides the
 * definition of the SequenceChild to be shared/reused, in principle without having
 * a backpointer to the enclosing Sequence. That allows sharing, and removes lots of
 * duplication/copying that is otherwise needed in the schema compiler data structures.
 *
 * Eventually things like alignment calculations should move from
 * Terms to these objects. That is, those calculations should not be done
 * on the DSOM objects, but on these SequenceChild objects in the Gram
 * objects.
 */
abstract class SequenceChild(protected val sq: SequenceTermBase, child: Term, groupIndex: Int)
  extends Terminal(child, true) {
  import SeparatedSequenceChildBehavior._
  import SeparatorSuppressionPolicy._

  private lazy val sgtb = sq.asInstanceOf[SequenceGroupTermBase]

  protected lazy val childParser = child.termContentBody.parser
  protected lazy val childUnparser = child.termContentBody.unparser

  final override lazy val parser = sequenceChildParser
  final override lazy val unparser = sequenceChildUnparser

  protected def sequenceChildParser: SequenceChildParser
  protected def sequenceChildUnparser: SequenceChildUnparser

  final lazy val optSequenceChildParser: Option[SequenceChildParser] =
    if (childParser.isEmpty) None else Some(parser)

  final lazy val optSequenceChildUnparser: Option[SequenceChildUnparser] =
    if (childUnparser.isEmpty) None else Some(unparser)

  /**
   * There's only parse result helpers here, so let's abbreviate
   */
  protected type SeparatedHelper = SeparatedSequenceChildParseResultHelper
  protected type UnseparatedHelper = UnseparatedSequenceChildParseResultHelper

  protected def separatedHelper: SeparatedHelper
  protected def unseparatedHelper: UnseparatedHelper

  protected lazy val sepMtaGram = sq.delimMTA

  protected lazy val sepGram = {
    sscb match {
      case _: PositionalLike => {
        sgtb.checkSeparatorTerminatorConflict
        sgtb.checkDelimiterEscapeConflict(child)
      }
      case _ => // ok
    }
    sq.sequenceSeparator
  }

  protected lazy val sepParser = (sepMtaGram ~ sepGram).parser
  protected lazy val sepUnparser = (sepMtaGram ~ sepGram).unparser

  final protected def srd = sq.sequenceRuntimeData
  final protected def trd = child.termRuntimeData
  final def termRuntimeData = trd
  final protected lazy val mgrd = child.asInstanceOf[ModelGroup].modelGroupRuntimeData
  final protected lazy val erd = child.asInstanceOf[ElementBase].elementRuntimeData

  private lazy val childElement = child.asInstanceOf[ElementBase]
  private lazy val childModelGroup = child.asInstanceOf[ModelGroup]

  protected final def ssp = sq.separatorSuppressionPolicy

  protected lazy val separatedSequenceChildBehavior: SeparatedSequenceChildBehavior = {
    import SeparatorSuppressionPolicy._
    import SeparatedSequenceChildBehavior._
    child match {
      case m: ModelGroup => {
        handleIfPotentiallyTrailingElseDefaultBehavior
      }
      case eb: ElementBase if (eb.isArray || eb.isOptional) => {
        import OccursCountKind._
        eb.occursCountKind match {
          case Fixed => Positional
          case Expression => Positional
          case Parsed => NonPositional
          case StopValue => NonPositional
          case Implicit => {
            val UNB = -1
            (
              eb.isPotentiallyTrailing,
              this.isDeclaredLast,
              sq.separatorSuppressionPolicy,
              eb.maxOccurs
            ) match {
              case (false, _, AnyEmpty, _) => NonPositional
              case (false, _, _, UNB) if !eb.isLastDeclaredRepresentedInSequence =>
                Assert.invariantFailed("Should be SDE found elsewhere.")
              case (false, _, _, _) => Positional
              case (true, _, Never, UNB) =>
                Assert.invariantFailed("Should be SDE found elsewhere.")
              case (true, false, AnyEmpty, UNB) => NonPositional
              case (true, false, TrailingEmpty, UNB)
                  if !eb.isLastDeclaredRepresentedInSequence =>
                Assert.invariantFailed("Should be SDE found elsewhere.")
              case (true, false, TrailingEmptyStrict, UNB)
                  if !eb.isLastDeclaredRepresentedInSequence =>
                Assert.invariantFailed("Should be SDE found elsewhere.")
              case (true, _, Never, _) => PositionalNever
              case (true, _, AnyEmpty, _) => NonPositional
              case (true, _, TrailingEmpty, _) => PositionalTrailingLax
              case (true, _, TrailingEmptyStrict, _) => PositionalTrailingStrict
            }
          }
        }
      }
      case eb: ElementBase if eb.isScalar => {
        handleIfPotentiallyTrailingElseDefaultBehavior
      }
      case _ => Positional // obscure cases like maxOccurs 0 with lengthKind 'implicit'
    }
  }

  protected def handleIfPotentiallyTrailingElseDefaultBehavior
    : SeparatedSequenceChildBehavior = {
    if (child.isPotentiallyTrailing) {
      ssp match {
        case AnyEmpty => NonPositional
        case TrailingEmpty => PositionalTrailingLax
        case TrailingEmptyStrict => PositionalTrailingStrict
        case Never => Positional
      }
    } else {
      Positional
    }
  }

  protected final def sscb = separatedSequenceChildBehavior

  final protected def isDeclaredLast: Boolean = {
    child.isLastDeclaredRepresentedInSequence
  }

  final protected lazy val isPositional: Boolean = {
    separatedSequenceChildBehavior match {
      case _: SeparatedSequenceChildBehavior.PositionalLike => true
      case _ => false
    }
  }

  /**
   * Combines static knowledge of whether the term can
   * be zero length, with issues like in order for trailing sep suppression to
   * apply, the term must be potentially trailing.
   *
   * This combines the static information about the child term with that of
   * the sequence child itself to answer, for this usage of the term, whether
   * we know for sure that we should NOT suppress the separator.
   *
   * True if we should never suppress separator i.e., always lay down
   * associated separator. False if we may/may-not suppress separator
   * depending on runtime characteristics like whether some thing(s) are zero length.
   */
  final protected lazy val isKnownStaticallyNotToSuppressSeparator: Boolean = {
    val neverSuppressSeparator = true
    val sometimesSuppressSeparator = false
    val res =
      if (zeroLengthDetector eq NeverZeroLengthDetector)
        neverSuppressSeparator // never zero length so we don't suppress separator
      else {
        ssp match {
          case TrailingEmpty | TrailingEmptyStrict =>
            if (child.isPotentiallyTrailing)
              sometimesSuppressSeparator
            else
              neverSuppressSeparator
          case AnyEmpty =>
            sometimesSuppressSeparator
          case Never =>
            neverSuppressSeparator
        }
      }
    res
  }

  /**
   * Combines static information about the model groupsequence child's definition
   * with properties of the sequence to tell us if a zero-length
   * after a parse attempt needs special treatment.
   *
   * This is the concept of "empty" that applies to model groups.
   * As they are not elements, nothing about default values or EVDP or nil reps applies
   * here.
   */
  final protected lazy val (isModelGroupRepPossiblyZeroLength, isModelGroupRepNonZeroLength)
    : (Boolean, Boolean) = {
    if (!childModelGroup.isRepresented) (false, false)
    else
      childModelGroup match {
        case mg if mg.isPotentiallyTrailing => (true, false)
        case mg => {
          val hasSyntax = mg.hasKnownRequiredSyntax
          //
          // This is coming out incorrect in one case
          // (a) the format has a terminator which is an expression
          // (b) the expression evaluates to say, %WSP*; or %ES; based on looking at other infoset information.
          // (c) that delimiter matches zero length
          // (d) the lengthKind is NOT delimited. So we're not scanning for this.
          // This comes up in mil-std-2045 and other formats which have an optional
          // final terminator after a string having lengthKind 'pattern'.
          // In that case, hasKnownRequiredSyntax is incorrect.
          //
          // Bug DAFFODIL-2132 is why this is incorrect in the above case.
          //
          (!hasSyntax, hasSyntax)
        }
      }
  }

  /**
   * Combines static information about the sequence child's definition
   * with properties of the sequence to tell us if a zero-length
   * after a parse attempt needs special treatment.
   *
   */
  final protected lazy val (isEmptyRepZeroLength, isEmptyRepNonZeroLength)
    : (Boolean, Boolean) = {
    val res: (Boolean, Boolean) = childElement match {
      case _ if !childElement.isRepresented => (false, false)
      case eb if eb.isComplexType => {
        val canBeZL = !eb.hasDelimiters &&
          !eb.complexType.modelGroup.hasKnownRequiredSyntax
        (canBeZL, false)
      }
      case eb if eb.isEmptyAnObservableConcept => {
        Assert.invariant(eb.isSimpleType)
        (eb.hasEmptyValueZLSyntax, !eb.hasEmptyValueZLSyntax)
      }
      case eb: ElementBase => {
        Assert.invariant(eb.isSimpleType)
        Assert.invariant(!eb.isEmptyAnObservableConcept)
        (false, false)
      }
    }
    res
  }

  /**
   * A zeroLengthDetector is a runtime device used by the unparser to
   * determine whether a term could unparse to zero length or not.
   *
   * Computed statically, because sometimes we know that it is not possible
   * for the representation to be zero length (e.g., non-nillable ahd non-defaultable
   * int always has to have at least one digit.)
   *
   * Used by unparsing algorithms that involve separator suppression for
   * zero-length data. The point of this is to avoid the overhead of unparser
   * uncertainty about a separator being needed or not. If you can examine the value
   * with a zero-length-detector and it gives you a positive answer one way or the
   * other, you can avoid a great deal of unparser overhead where it has to suspend
   * whether to emit a separator. If the ZL detector tells you the representation
   * will be greater than zero length, you can just emit a separator and move on.
   *
   * The detector algorithm is independent of the usage of the term.
   * That is, it doesn't take things like dfdl:separatorSuppressionPolicy (which
   * is a property of the surrounding sequence) into account.
   */
  final lazy val zeroLengthDetector: ZeroLengthDetector = {
    val result: ZeroLengthDetector = child match {
      case e: ElementBase => {
        import LengthKind._

        lazy val couldBeZLSimpleType =
          !e.hasDelimiters &&
            e.isSimpleType &&
            (e.lengthKind match {
              case Delimited | EndOfParent => true
              //
              // When parsing, the pattern might not match zero-length data, but
              // when unparsing we just output the data as it appears in the infoset.
              // We don't check that it satisfies the pattern. So a zero-length
              // string in the infoset will be zero-length when output.
              //
              case Pattern => true
              case Explicit | Implicit => e.hasFixedLengthOf(0)
              case Prefixed => false
            }) &&
            //
            // Alignment stuff - can't be any aligning possible or we'd have
            // to do a runtime check for whether things are aligned or not.
            // In the unparser, that is a suspendable operation, since we may
            // not know the bit position.
            //
            // Separated sequences where there's any alignment possibly needed
            // are an obscure corner case we don't need to handle.
            //
            e.hasNoSkipRegions &&
            (((e.impliedRepresentation eq Representation.Text) && e.hasTextAlignment) ||
              // binary data is allowed to be delimited... so long as the
              // separators are recognizably not confused with binary bytes.
              // The initiator is AFTER the alignmentFill region. So those are checked
              // elsewhere for compatibility.
              ((e.impliedRepresentation eq Representation.Binary) && e.isKnownToBeAligned))

        if (!e.isRepresented)
          NotRepresentedZeroLengthDetector
        else if (e.isArray && !e.isRequiredStreamingUnparserEvent)
          PossiblyZeroArrayOccurrencesDetector
        else {
          Assert.invariant(e.isRepresented)
          val hasNilZL = (e.isNillable && !e.hasNilValueRequiredSyntax)
          val hasEmptyZL = (e.isEmptyAnObservableConcept && e.hasEmptyValueZLSyntax)
          val hasStringZL =
            e.isSimpleType && (e.simpleType.primType eq NodeInfo.String) && (couldBeZLSimpleType || hasEmptyZL)
          val hasHexBinaryZL =
            e.isSimpleType && (e.simpleType.primType eq NodeInfo.HexBinary) && (couldBeZLSimpleType || hasEmptyZL)
          val zld =
            (hasNilZL, hasStringZL, hasHexBinaryZL) match {
              case (true, false, false) => new NillableZeroLengthDetector
              case (false, true, false) => new StringZeroLengthDetector
              case (false, false, true) => new HexBinaryZeroLengthDetector
              case (true, true, false) => new NillableStringZeroLengthDetector
              case (true, false, true) => new NillableHexBinaryZeroLengthDetector
              case (_, true, true) =>
                Assert.invariantFailed("Can't be both String and HexBinary type")
              case _ => NeverZeroLengthDetector
            }
          zld
        }
      }
      case m: ModelGroup => {
        if (m.hasKnownRequiredSyntax)
          NeverZeroLengthDetector
        else
          PossiblyZeroLengthModelGroupDetector
      }
    }
    result
  }

}

class ScalarOrderedSequenceChild(sq: SequenceTermBase, term: Term, groupIndex: Int)
  extends SequenceChild(sq, term, groupIndex) {
  import SeparatedSequenceChildBehavior._

  lazy val sequenceChildParser: SequenceChildParser = {
    val res = term match {
      case _ if !term.isRepresented =>
        new NonRepresentedSequenceChildParser(childParser, srd, trd)
      case (_: ElementBase) if sq.hasSeparator =>
        new ScalarOrderedElementSeparatedSequenceChildParser(
          childParser,
          srd,
          trd,
          sepParser,
          sq.separatorPosition,
          separatedHelper
        )
      case (_: ModelGroup) if sq.hasSeparator => {
        Assert.invariant(term.isInstanceOf[ModelGroup])
        new GroupSeparatedSequenceChildParser(
          childParser,
          srd,
          mgrd,
          sepParser,
          sq.separatorPosition,
          separatedHelper
        )
      }
      case _ if !sq.hasSeparator =>
        new ScalarOrderedUnseparatedSequenceChildParser(
          childParser,
          srd,
          trd,
          unseparatedHelper
        )
    }
    res
  }
  override lazy val sequenceChildUnparser: SequenceChildUnparser = {
    val res =
      if (sq.hasSeparator) {
        new ScalarOrderedSeparatedSequenceChildUnparser(
          childUnparser,
          srd,
          trd,
          sepUnparser,
          sq.separatorPosition,
          sq.separatorSuppressionPolicy,
          zeroLengthDetector,
          term.isPotentiallyTrailing,
          isKnownStaticallyNotToSuppressSeparator,
          isPositional,
          isDeclaredLast
        )
      } else {
        new ScalarOrderedUnseparatedSequenceChildUnparser(childUnparser, srd, trd)
      }
    res
  }

  override lazy val separatedHelper = {
    term match {
      case _: ModelGroup => sepGroupHelper
      case _: ElementBase => sepScalarElementHelper
    }
  }

  private lazy val sepGroupHelper: SeparatedHelper = {
    this.sscb match {
      case PositionalTrailingLax | PositionalTrailingStrict =>
        new PositionalTrailingGroupSeparatedSequenceChildParseResultHelper(
          mgrd,
          sscb,
          isModelGroupRepPossiblyZeroLength,
          isModelGroupRepNonZeroLength
        )
      case Positional | PositionalNever =>
        new PositionalGroupSeparatedSequenceChildParseResultHelper(
          mgrd,
          sscb,
          isModelGroupRepPossiblyZeroLength,
          isModelGroupRepNonZeroLength
        )
      case NonPositional =>
        new NonPositionalGroupSeparatedSequenceChildParseResultHelper(
          mgrd,
          sscb,
          isModelGroupRepPossiblyZeroLength,
          isModelGroupRepNonZeroLength
        )
    }
  }

  private def eep = e.emptyElementParsePolicy
  private lazy val e = term.asInstanceOf[ElementBase]

  /**
   * Must deal with nils, emptyness and string/hexBinary exceptional behavior
   * including the behavior for dfdl:emptyElementParsePolicy 'treatAsAbsent' which special cases
   * Required elements like scalars, iff they are emptyRep, emptyValueDelimiterPolicy,
   * nilValueDelimiterPolicy, complex elements that nillable, or fully defaultable.
   *
   * So we have ((simpleStringHexBinary x (treatAsAbsent, treatAsEmpty), simpleOther, complex) x (nillable, not) x
   * 4 behaviors. That's 32 combinations. Let's start with fewer cases and more runtime
   * decisions, and specialize if we think it will help clarity or performance.
   */
  private lazy val sepScalarElementHelper: SeparatedHelper = {
    Assert.invariant(e.isScalar)
    Assert.invariant(e.isRepresented)
    val isd = e.isSimpleType && (e.lengthKind eq LengthKind.Delimited)
    this.sscb match {
      case PositionalTrailingLax | PositionalTrailingStrict =>
        new PositionalTrailingScalarElementSeparatedSequenceChildParseResultHelper(
          sscb,
          erd,
          isd,
          eep,
          isEmptyRepZeroLength,
          isEmptyRepNonZeroLength
        )
      case Positional | PositionalNever =>
        new PositionalScalarElementSeparatedSequenceChildParseResultHelper(
          sscb,
          erd,
          isd,
          eep,
          isEmptyRepZeroLength,
          isEmptyRepNonZeroLength
        )
      case NonPositional =>
        new NonPositionalScalarElementSeparatedSequenceChildParseResultHelper(
          sscb,
          erd,
          isd,
          eep,
          isEmptyRepZeroLength,
          isEmptyRepNonZeroLength
        )
    }
  }

  override lazy val unseparatedHelper = {
    term match {
      case _: ModelGroup => unsepGroupHelper
      case _: ElementBase => unsepScalarElementHelper
    }
  }

  private lazy val unsepGroupHelper: UnseparatedHelper = {
    new GroupUnseparatedSequenceChildParseResultHelper(
      mgrd,
      isModelGroupRepPossiblyZeroLength,
      isModelGroupRepNonZeroLength
    )
  }

  private lazy val unsepScalarElementHelper: UnseparatedHelper = {
    new ScalarElementUnseparatedSequenceChildParseResultHelper(
      erd,
      eep,
      isEmptyRepZeroLength,
      isEmptyRepNonZeroLength
    )
  }
}

sealed abstract class RepElementSequenceChild(
  sq: SequenceTermBase,
  protected val e: ElementBase,
  groupIndex: Int
) extends SequenceChild(sq, e, groupIndex) {
  import SeparatedSequenceChildBehavior._

  Assert.usage(!e.isScalar)

  override lazy val sequenceChildUnparser: SequenceChildUnparser =
    sq.hasSeparator match {
      case true => {
        new RepOrderedSeparatedSequenceChildUnparser(
          childUnparser,
          srd,
          erd,
          sepUnparser,
          sq.separatorPosition,
          sq.separatorSuppressionPolicy,
          zeroLengthDetector,
          e.isPotentiallyTrailing,
          isKnownStaticallyNotToSuppressSeparator,
          isPositional,
          isDeclaredLast
        )
      }
      case false => new RepOrderedUnseparatedSequenceChildUnparser(childUnparser, srd, erd)
    }

  private lazy val eep = e.emptyElementParsePolicy

  protected lazy val separatedHelper: SeparatedHelper = {
    Assert.invariant(!e.isScalar)
    Assert.invariant(e.isRepresented)
    val isd = e.isSimpleType && (e.lengthKind eq LengthKind.Delimited)

    this.sscb match {
      case PositionalTrailingLax | PositionalTrailingStrict =>
        new PositionalTrailingRepElementSeparatedSequenceChildParseResultHelper(
          sscb,
          erd,
          isd,
          eep,
          isEmptyRepZeroLength,
          isEmptyRepNonZeroLength
        )
      case Positional | PositionalNever =>
        new PositionalRepElementSeparatedSequenceChildParseResultHelper(
          sscb,
          erd,
          isd,
          eep,
          isEmptyRepZeroLength,
          isEmptyRepNonZeroLength
        )
      case NonPositional =>
        new NonPositionalRepElementSeparatedSequenceChildParseResultHelper(
          sscb,
          erd,
          isd,
          eep,
          isEmptyRepZeroLength,
          isEmptyRepNonZeroLength
        )
    }
  }

  protected lazy val unseparatedHelper: UnseparatedHelper = {
    new RepElementUnseparatedSequenceChildParseResultHelper(
      erd,
      eep,
      isEmptyRepZeroLength,
      isEmptyRepNonZeroLength
    )
  }
}

class RepOrderedExactlyNSequenceChild(
  sq: SequenceTermBase,
  e: ElementBase,
  groupIndex: Int,
  repeatCount: Long
) extends RepElementSequenceChild(sq, e, groupIndex) {

  lazy val sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true =>
      new RepOrderedExactlyNSeparatedSequenceChildParser(
        childParser,
        srd,
        erd,
        sepParser,
        sq.separatorPosition,
        separatedHelper
      )
    case false =>
      new RepOrderedExactlyNUnseparatedSequenceChildParser(
        childParser,
        srd,
        erd,
        unseparatedHelper,
        repeatCount
      )
  }

}

class RepOrderedExpressionOccursCountSequenceChild(
  sq: SequenceTermBase,
  e: ElementBase,
  groupIndex: Int
) extends RepElementSequenceChild(sq, e, groupIndex) {

  lazy val sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true =>
      new RepOrderedExpressionOccursCountSeparatedSequenceChildParser(
        childParser,
        e.occursCountEv,
        srd,
        erd,
        sepParser,
        sq.separatorPosition,
        separatedHelper
      )
    case false =>
      new RepOrderedExpressionOccursCountUnseparatedSequenceChildParser(
        childParser,
        e.occursCountEv,
        srd,
        erd,
        unseparatedHelper
      )
  }

  // This class is only used for sequences with occursCountKind="expression",
  // which means that the separatorSuppressionPolicy is not applicable and the
  // implied behavior is separatorSuppresionPolicy="never"
  override lazy val sequenceChildUnparser: SequenceChildUnparser = sq.hasSeparator match {
    case true => {
      new RepOrderedSeparatedSequenceChildUnparser(
        childUnparser,
        srd,
        erd,
        sepUnparser,
        sq.separatorPosition,
        SeparatorSuppressionPolicy.Never,
        zeroLengthDetector,
        e.isPotentiallyTrailing,
        true,
        isPositional,
        isDeclaredLast
      )
    }
    case false => new RepOrderedUnseparatedSequenceChildUnparser(childUnparser, srd, erd)
  }
}

class RepOrderedWithMinMaxSequenceChild(sq: SequenceTermBase, e: ElementBase, groupIndex: Int)
  extends RepElementSequenceChild(sq, e, groupIndex) {

  lazy val sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true =>
      new RepOrderedWithMinMaxSeparatedSequenceChildParser(
        childParser,
        srd,
        erd,
        sepParser,
        sq.separatorPosition,
        separatedHelper
      )
    case false =>
      new RepOrderedWithMinMaxUnseparatedSequenceChildParser(
        childParser,
        srd,
        erd,
        unseparatedHelper
      )
  }
}
