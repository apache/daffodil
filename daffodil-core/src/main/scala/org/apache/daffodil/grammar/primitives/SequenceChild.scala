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

import org.apache.daffodil.grammar._
import org.apache.daffodil.dsom._
import org.apache.daffodil.processors.unparsers._
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.schema.annotation.props.gen.Representation

/**
 * A SequenceChild is exactly that, a child Term of a Sequence
 *
 * These objects are part of the Gram object hierarchy.
 * They represent the use of a Term in a context. They are
 * objects that belong to (are owned by exactly one) the enclosing sequence, are part of it, and
 * so it is reasonable for a SequenceChild to have a backpointer to the
 * enclosing Sequence object.
 *
 * This allows the Term object that provides the definition of the SequenceChild
 * to be shared/reused, in principle without having a backpointer to the
 * enclosing Sequence. That allows sharing, and removes lots of duplication/copying
 * in the schema compiler data strucures.
 *
 * Eventually things like alignment calculations should move from
 * Terms to these objects. That is, those calculations should not be done
 * on the DSOM objects, but on these SequenceChild objects in the Gram
 * objects.
 */
abstract class SequenceChild(
  protected val sq: SequenceTermBase, child: Term, groupIndex: Int)
  extends Terminal(child, true) {

  protected def childParser = child.termContentBody.parser
  protected def childUnparser = child.termContentBody.unparser

  final override def parser = sequenceChildParser
  final override def unparser = sequenceChildUnparser

  protected def sequenceChildParser: SequenceChildParser
  protected def sequenceChildUnparser: SequenceChildUnparser

  def optSequenceChildParser: Option[SequenceChildParser] =
    if (childParser.isEmpty) None else Some(parser)

  def optSequenceChildUnparser: Option[SequenceChildUnparser] =
    if (childUnparser.isEmpty) None else Some(unparser)

  protected lazy val sepGram = sq.sequenceSeparator
  protected lazy val sepParser = sepGram.parser
  protected lazy val sepUnparser = sepGram.unparser

  lazy val srd = sq.sequenceRuntimeData
  lazy val trd = child.termRuntimeData

  /**
   * Used by unparsing algorithms that involve separator suppression for
   * zero-length data.
   */
  final lazy val zeroLengthDetector: ZeroLengthDetector = {
    val result: ZeroLengthDetector = child match {
      case e: ElementBase => {
        Assert.invariant(e.isRepresented)
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
              ((e.impliedRepresentation eq Representation.Binary) && e.isKnownToBeAligned))

        val hasNilZL = (e.isNillable && !e.hasNilValueRequiredSyntax)
        val hasEmptyZL = (e.emptyIsAnObservableConcept && e.hasEmptyValueZLSyntax)
        val hasStringZL = e.isSimpleType && (e.simpleType.primType eq NodeInfo.String) && (couldBeZLSimpleType || hasEmptyZL)
        val hasHexBinaryZL = e.isSimpleType && (e.simpleType.primType eq NodeInfo.HexBinary) && (couldBeZLSimpleType || hasEmptyZL)
        (hasNilZL, hasStringZL, hasHexBinaryZL) match {
          case (true, false, false) => new NillableZeroLengthDetector
          case (false, true, false) => new StringZeroLengthDetector
          case (false, false, true) => new HexBinaryZeroLengthDetector
          case (true, true, false) => new NillableStringZeroLengthDetector
          case (true, false, true) => new NillableHexBinaryZeroLengthDetector
          case (_, true, true) => Assert.invariantFailed("Can't be both String and HexBinary type")
          case _ => new NeverZeroLengthDetector
        }
      }
      case m: ModelGroup => {
        new NeverZeroLengthDetector
      }
    }
    result
  }

  /**
   * Used when unparsing to determine which of several separator suppression
   * algorithms is required.
   *
   * Relevant only if the enclosing sequence is separated.
   */
  final lazy val separatorSuppressionMode: SeparatorSuppressionMode = {
    Assert.invariant(sq.hasSeparator)
    import SeparatorSuppressionMode._
    import OccursCountKind._
    child match {
      case e: ElementBase => {
        if (e.isScalar) None
        else e.occursCountKind match {
          case Fixed | Expression => FixedOrExpression
          case Parsed => new SuppressAnyEmpty(zeroLengthDetector)
          case StopValue => Assert.notYetImplemented()
          case Implicit => {
            import SeparatorSuppressionPolicy._
            sq.separatorSuppressionPolicy match {
              case Never => ImplicitNeverOrNotPotentiallyTrailing
              case AnyEmpty => new ImplicitSuppressAnyEmpty(zeroLengthDetector)
              case _ if (!e.isPotentiallyTrailing) => ImplicitNeverOrNotPotentiallyTrailing
              case _ => new ImplicitPotentiallyTrailing(zeroLengthDetector)
            }
          }
        }
      }
      case m: ModelGroup => {
        SeparatorSuppressionMode.None
      }
    }
  }

}

class ScalarOrderedSequenceChild(sq: SequenceTermBase, term: Term, groupIndex: Int)
  extends SequenceChild(sq, term, groupIndex) {

  def sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true => new ScalarOrderedSeparatedSequenceChildParser(
      childParser, srd, trd, sepParser, sq.separatorPosition, sq.separatorSuppressionPolicy)
    case false => new ScalarOrderedUnseparatedSequenceChildParser(childParser, srd, trd)
  }
  def sequenceChildUnparser: SequenceChildUnparser = sq.hasSeparator match {
    case true => new ScalarOrderedSeparatedSequenceChildUnparser(
      childUnparser, srd, trd, sepUnparser, sq.separatorPosition, sq.separatorSuppressionPolicy,
      this.separatorSuppressionMode)
    case false => new ScalarOrderedUnseparatedSequenceChildUnparser(childUnparser, srd, trd)
  }
}

sealed abstract class ElementSequenceChild(
  sq: SequenceTermBase,
  protected val e: ElementBase, groupIndex: Int)
  extends SequenceChild(sq, e, groupIndex) {

  protected lazy val erd = e.elementRuntimeData
}

sealed trait RepUnparserMixin { self: ElementSequenceChild =>

  protected def e: ElementBase

  def sequenceChildUnparser: SequenceChildUnparser = sq.hasSeparator match {
    case true => new RepOrderedSeparatedSequenceChildUnparser(
      childUnparser, srd, erd, sepUnparser, sq.separatorPosition, sq.separatorSuppressionPolicy,
      this.separatorSuppressionMode)
    case false => new RepOrderedUnseparatedSequenceChildUnparser(childUnparser, srd, erd)
  }
}

class RepOrderedExactlyNSequenceChild(sq: SequenceTermBase, e: ElementBase, groupIndex: Int, repeatCount: Long)
  extends ElementSequenceChild(sq, e, groupIndex)
  with RepUnparserMixin {

  def sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true => new RepOrderedExactlyNSeparatedSequenceChildParser(
      childParser, srd, erd, sepParser, sq.separatorPosition, sq.separatorSuppressionPolicy)
    case false => new RepOrderedExactlyNUnseparatedSequenceChildParser(childParser, srd, erd, repeatCount)
  }

}

class RepOrderedExactlyTotalOccursCountSequenceChild(sq: SequenceTermBase, e: ElementBase, groupIndex: Int)
  extends ElementSequenceChild(sq, e, groupIndex)
  with RepUnparserMixin {

  def sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true => new RepOrderedExactlyTotalOccursCountSeparatedSequenceChildParser(
      childParser, e.occursCountEv, srd, erd, sepParser, sq.separatorPosition, sq.separatorSuppressionPolicy)
    case false => new RepOrderedExactlyTotalOccursCountUnseparatedSequenceChildParser(childParser, e.occursCountEv, srd, erd)
  }
}

class RepOrderedWithMinMaxSequenceChild(sq: SequenceTermBase, e: ElementBase, groupIndex: Int)
  extends ElementSequenceChild(sq, e, groupIndex)
  with RepUnparserMixin {

  def sequenceChildParser: SequenceChildParser = sq.hasSeparator match {
    case true => new RepOrderedWithMinMaxSeparatedSequenceChildParser(
      childParser, srd, erd, sepParser, sq.separatorPosition, sq.separatorSuppressionPolicy,
      e.isPotentiallyTrailing, e.isLastDeclaredRepresentedInSequence)
    case false => new RepOrderedWithMinMaxUnseparatedSequenceChildParser(childParser, srd, erd)
  }
}
