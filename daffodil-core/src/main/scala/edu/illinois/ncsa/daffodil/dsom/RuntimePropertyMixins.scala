/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.equality.TypeEqual
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.BinaryFloatRepEv
import edu.illinois.ncsa.daffodil.processors.ByteOrderEv
import edu.illinois.ncsa.daffodil.processors.CharsetEv
import edu.illinois.ncsa.daffodil.processors.EncodingEv
import edu.illinois.ncsa.daffodil.processors.ExplicitLengthEv
import edu.illinois.ncsa.daffodil.processors.FillByteEv
import edu.illinois.ncsa.daffodil.processors.ImplicitLengthEv
import edu.illinois.ncsa.daffodil.processors.InitiatorParseEv
import edu.illinois.ncsa.daffodil.processors.InitiatorUnparseEv
import edu.illinois.ncsa.daffodil.processors.OccursCountEv
import edu.illinois.ncsa.daffodil.processors.OutputNewLineEv
import edu.illinois.ncsa.daffodil.processors.SeparatorParseEv
import edu.illinois.ncsa.daffodil.processors.SeparatorUnparseEv
import edu.illinois.ncsa.daffodil.processors.TerminatorParseEv
import edu.illinois.ncsa.daffodil.processors.TerminatorUnparseEv
import edu.illinois.ncsa.daffodil.processors.TextBooleanFalseRepEv
import edu.illinois.ncsa.daffodil.processors.TextBooleanTrueRepEv
import edu.illinois.ncsa.daffodil.processors.TextStandardDecimalSeparatorEv
import edu.illinois.ncsa.daffodil.processors.TextStandardExponentRepEv
import edu.illinois.ncsa.daffodil.processors.TextStandardGroupingSeparatorEv
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.DFDLBaseTypeMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.DFDLSimpleTypeMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthAGMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.OccursAGMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Sequence_AnnotationMixin
import edu.illinois.ncsa.daffodil.processors.LengthInBitsEv
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.LengthEv
import edu.illinois.ncsa.daffodil.processors.MinLengthInBitsEv
import edu.illinois.ncsa.daffodil.processors.UnparseTargetLengthInBitsEv
import edu.illinois.ncsa.daffodil.processors.unparsers.NilStringLiteralForUnparserEv
import edu.illinois.ncsa.daffodil.processors.UnparseTargetLengthInCharactersEv

/*
 * These are the DFDL properties which can have their values come
 * from the data stream itself by way of expressions.
 *
 * TODO: EscapeScheme's have a few more of these runtime properties
 * escapeCharacter, and escapeEscapeCharacter.
 */

trait TermRuntimeValuedPropertiesMixin
  extends DFDLBaseTypeMixin
  with RawCommonRuntimeValuedPropertiesMixin { decl: Term =>

  private lazy val encodingExpr = LV('encoding) {
    val qn = this.qNameForProperty("encoding")
    this match {
      case eb: ElementBase if (eb.isSimpleType && eb.primType =:= PrimType.HexBinary && eb.lengthKind =:= LengthKind.Delimited) => {
        if (encodingRaw.value.toUpperCase != "ISO-8859-1") {
          SDE("xs:hexBinary with dfdl:lengthKind=\"delimited\" must have dfdl:encoding=\"ISO-8859-1\", but was \"%s\"", encodingRaw.value)
        }
        //
        // We treat delimited hex binary as a string in iso-8859-1 encoding.
        // That lets us reuse the various string parsing bases to grab the content.
        //
        val qn = decl.qNameForProperty("encoding")
        val exp = ConstantExpression(qn, PrimType.HexBinary, "iso-8859-1")
        exp
      }
      case _ => ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, encodingRaw)
    }
  }.value

  final lazy val encodingEv = {
    val ev = new EncodingEv(encodingExpr, termRuntimeData)
    ev.compile()
    ev
  }

  final lazy val charsetEv = maybeCharsetEv.get

  final lazy val maybeCharsetEv =
    if (optionEncodingRaw.isDefined) {
      val ev = new CharsetEv(encodingEv, termRuntimeData)
      ev.compile()
      One(ev)
    } else
      Nope

  final lazy val fillByteEv = {
    val ev = new FillByteEv(fillByte, charsetEv, termRuntimeData)
    ev.compile()
    ev
  }

  /*
   * Property OutputNewLine
   */

  lazy val outputNewLineEv = {
    val outputNewLineExpr = {
      val qn = this.qNameForProperty("outputNewLine")
      ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, outputNewLineRaw)
    }
    val ev = new OutputNewLineEv(outputNewLineExpr, termRuntimeData)
    ev.compile()
    ev
  }

  /**
   * Use when we might or might not need the outputNewLine property
   */
  lazy val maybeOutputNewLineEv = {
    if (optionOutputNewLineRaw.isDefined)
      One(outputNewLineEv)
    else
      Nope
  }

}

trait DelimitedRuntimeValuedPropertiesMixin
  extends TermRuntimeValuedPropertiesMixin
  with RawDelimitedRuntimeValuedPropertiesMixin { decl: Term =>

  lazy val isLengthKindDelimited = {
    decl.referredToComponent match {
      case mg: ModelGroup => mg.enclosingElement.get.lengthKind == LengthKind.Delimited
      case eb: ElementBase => eb.lengthKind == LengthKind.Delimited
    }
  }

  private lazy val initiatorExpr = {
    val qn = this.qNameForProperty("initiator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compile(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, initiatorRaw)
  }

  lazy val initiatorParseEv = {
    val ev = new InitiatorParseEv(initiatorExpr, decl.termRuntimeData)
    ev.compile()
    ev
  }
  lazy val initiatorUnparseEv = {
    val ev = new InitiatorUnparseEv(initiatorExpr, outputNewLineEv, decl.termRuntimeData)
    ev.compile()
    ev
  }

  final def initiatorLoc = (this.prettyName, this.path)

  private lazy val terminatorExpr = LV('terminator) {
    val qn = this.qNameForProperty("terminator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compile(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, terminatorRaw)
  }.value

  final def terminatorLoc = (this.prettyName, this.path)

  lazy val terminatorParseEv = {
    val ev = new TerminatorParseEv(terminatorExpr, isLengthKindDelimited, decl.termRuntimeData)
    ev.compile()
    ev
  }
  lazy val terminatorUnparseEv = {
    val ev = new TerminatorUnparseEv(terminatorExpr, isLengthKindDelimited, outputNewLineEv, decl.termRuntimeData)
    ev.compile()
    ev
  }
}

trait ElementRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with OccursAGMixin
  with LengthAGMixin
  with SimpleTypeRuntimeValuedPropertiesMixin
  with RawElementRuntimeValuedPropertiesMixin { decl: ElementBase =>

  private lazy val byteOrderExpr = LV('byteOrder) {
    val qn = this.qNameForProperty("byteOrder")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, byteOrderRaw)
  }.value

  final lazy val byteOrderEv = LV('byteOrderEv) {
    val ev = new ByteOrderEv(byteOrderExpr, elementRuntimeData)
    ev.compile()
    ev
  }.value

  protected final lazy val lengthExpr = {
    val qn = this.qNameForProperty("length")
    ExpressionCompilers.JLong.compile(qn, NodeInfo.Long, lengthRaw)
  }

  private lazy val explicitLengthEv = {
    Assert.usage(lengthKind eq LengthKind.Explicit)
    val ev = new ExplicitLengthEv(lengthExpr, erd)
    ev.compile()
    ev
  }

  private lazy val implicitLengthEv = {
    Assert.usage(lengthKind eq LengthKind.Implicit)
    import Representation._
    import NodeInfo._
    lazy val maxLengthLong = maxLength.longValueExact
    val ev = (impliedRepresentation, typeDef.kind) match {
      case (Text, String) => new ImplicitLengthEv(maxLengthLong, erd)
      case (Binary, HexBinary) => new ImplicitLengthEv(maxLengthLong, erd)
      case (Binary, _) => new ImplicitLengthEv(implicitBinaryLengthInBits, erd)
      case (Text, _) => SDE("Type %s with dfdl:representation='text' cannot have dfdl:lengthKind='implicit'", typeDef.kind.name)
    }
    ev.compile()
    ev
  }

  // TODO: remove when no longer being used
  // @deprecated("2016-08-11", "Use elementLengthInBitsEv instead. Converts to bits for you.")
  final lazy val lengthEv = {
    val ev =
      lengthKind match {
        case LengthKind.Explicit => explicitLengthEv
        case LengthKind.Implicit => implicitLengthEv
        case _ =>
          Assert.usageError("should only be used for Explicit or Implicit length kinds: " + lengthKind)
      }
    Assert.invariant(ev.isCompiled)
    ev
  }

  protected final lazy val optLengthConstant: Option[Long] = lengthEv.optConstant.map { _.longValue }

  /**
   * For specified-length elements, computes the Ev which determines
   * when unparsing, there is a target length in units of bits that
   * can cause the need to insert, for simple types, padding or fillByte, or to truncate.
   * Or, for complex types, to insert ElementUnused region.
   *
   * Evs enable elimination of the proliferation of dual code paths for known
   * vs. unknown byteOrder, encoding, length, etc. Just code as if it was
   * runtime-valued using the Ev. The "right thing" happens if the information
   * is constant.
   */
  private lazy val elementLengthInBitsEv: LengthInBitsEv = {
    Assert.usage((lengthKind eq LengthKind.Implicit) || (lengthKind eq LengthKind.Explicit))
    import LengthKind._
    import Representation._
    import NodeInfo._
    val (units: LengthUnits, lenEv: LengthEv) =
      (lengthKind, impliedRepresentation, typeDef.kind) match {
        case (Explicit, Binary, HexBinary) => (LengthUnits.Bytes, explicitLengthEv)
        case (Implicit, Binary, HexBinary) => (LengthUnits.Bytes, implicitLengthEv)
        case (Explicit, Binary, _) => (lengthUnits, explicitLengthEv)
        case (Implicit, Binary, _) => (LengthUnits.Bits, implicitLengthEv)
        case (Explicit, Text, _) => (lengthUnits, explicitLengthEv)
        case (Implicit, Text, _) => (lengthUnits, implicitLengthEv)
        case _ => Assert.invariantFailed("not Implicit or Explicit")
      }
    val ev = new LengthInBitsEv(units, lengthKind, maybeCharsetEv, lenEv, erd)
    ev.compile()
    ev
  }

  /**
   * This evaluatable can only be used when a minimum length is a concept.
   *
   * This is the case for variable-length things like strings, and hexbinary
   * when Explicit, or fixed (meaning implicit) length.
   *
   * Other textual simple types as well (via textOutputMinLength).
   *
   */
  private lazy val minLengthInBitsEv: MinLengthInBitsEv = {
    val ev = new MinLengthInBitsEv(minLenUnits, lengthKind, maybeCharsetEv, minLen, erd)
    ev.compile()
    ev
  }

  private lazy val (minLenUnits: LengthUnits, minLen: Long) = {
    Assert.usage((lengthKind eq LengthKind.Implicit) || (lengthKind eq LengthKind.Explicit))
    import LengthKind._
    import Representation._
    import NodeInfo._
    lazy val maxLengthLong = maxLength.longValueExact
    lazy val minLengthLong = minLength.longValueExact
    (lengthKind, impliedRepresentation, typeDef.kind) match {
      case (Implicit, Binary, HexBinary) => (LengthUnits.Bytes, maxLengthLong) // fixed length
      case (Implicit, Text, AnySimpleType) => (lengthUnits, textOutputMinLength) // fixed length
      case (Implicit, Text, String) => (lengthUnits, maxLengthLong) // fixed length
      case (Explicit, Text, String) => (lengthUnits, minLengthLong)
      case (Explicit, Binary, HexBinary) => (LengthUnits.Bytes, minLengthLong)
      case (Explicit, Text, AnySimpleType) => (lengthUnits, textOutputMinLength)
      case _ => (LengthUnits.Bits, 0L) // anything else. This shuts off checking a min.
    }
  }

  final lazy val maybeUnparseTargetLengthInBitsEv = {
    if ((this.optionLengthRaw.isDefined &&
      (lengthKind _eq_ LengthKind.Explicit)) ||
      ((lengthKind _eq_ LengthKind.Implicit) && isSimpleType)) {
      val ev = unparseTargetLengthInBitsEv
      One(ev)
    } else
      Nope
  }

  final lazy val unparseTargetLengthInBitsEv = {
    val ev = new UnparseTargetLengthInBitsEv(elementLengthInBitsEv, minLengthInBitsEv, erd)
    ev.compile()
    ev
  }

  final lazy val maybeUnparseTargetLengthInCharactersEv = {
    if ((lengthUnits eq LengthUnits.Characters) &&
      (this.optionLengthRaw.isDefined &&
        (lengthKind _eq_ LengthKind.Explicit)) ||
        ((lengthKind _eq_ LengthKind.Implicit) && isSimpleType)) {
      val optCs = charsetEv.optConstant
      if (optCs.isEmpty || optCs.get.maybeFixedWidth.isEmpty) {
        val ev = new UnparseTargetLengthInCharactersEv(lengthEv, charsetEv, minLen, erd)
        ev.compile()
        One(ev)
      } else
        Nope
    } else
      Nope
  }

  //
  // The occursCount expression is written on the array element, but that expression
  // is actually evaluated before any instances of the element exist. Hence, an
  // expression like { ../c } that appears to be reaching back to a prior peer to get
  // the count... well that expression gets evaluated while we are in the parent context
  // hence if we literally execute the ".." we'll be one element too high in the
  // infoset.
  //
  // So we adjust the expression so that the context is as if on the parent.
  //
  // Not at all sure why this worked with Saxon, but in our new Infoset and DPath
  // implementation, the ".." does get literally evaluated.
  //

  private lazy val occursCountExpr = LV('occursCount) {
    val qn = this.qNameForProperty("occursCount")
    val isEvaluatedAbove = true
    ExpressionCompilers.JLong.compile(qn, NodeInfo.Long, occursCountRaw, isEvaluatedAbove)
  }.value

  lazy val occursCountEv = {
    val ev = new OccursCountEv(occursCountExpr, erd)
    ev.compile()
    ev
  }

  lazy val nilStringLiteralForUnparserEv = {
    val ev = new NilStringLiteralForUnparserEv(termRuntimeData, maybeOutputNewLineEv, rawNilValuesForUnparse.head)
    ev.compile()
    ev
  }
}

trait SequenceRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with Sequence_AnnotationMixin
  with RawSequenceRuntimeValuedPropertiesMixin { decl: GroupBase =>

  private lazy val separatorExpr = {
    val qn = this.qNameForProperty("separator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compile(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, separatorRaw)
  }

  lazy val separatorParseEv = {
    val ev = new SeparatorParseEv(separatorExpr, decl.termRuntimeData)
    ev.compile()
    ev
  }
  lazy val separatorUnparseEv = {
    val ev = new SeparatorUnparseEv(separatorExpr, outputNewLineEv, decl.termRuntimeData)
    ev.compile()
    ev
  }

  final def separatorLoc = (this.prettyName, this.path)
}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends DFDLSimpleTypeMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin { decl: ElementBase =>

  private lazy val textStandardDecimalSeparatorExpr = LV('textStandardDecimalSeparator) {
    val qn = this.qNameForProperty("textStandardDecimalSeparator")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.String, textStandardDecimalSeparatorRaw)
    c
  }.value

  final lazy val textStandardDecimalSeparatorEv = {
    val ev = new TextStandardDecimalSeparatorEv(textStandardDecimalSeparatorExpr, erd)
    ev.compile()
    ev
  }

  private lazy val textStandardGroupingSeparatorExpr = LV('textStandardGroupingSeparator) {
    val qn = this.qNameForProperty("textStandardGroupingSeparator")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.String, textStandardGroupingSeparatorRaw)
    c
  }.value

  final lazy val textStandardGroupingSeparatorEv = {
    val ev = new TextStandardGroupingSeparatorEv(textStandardGroupingSeparatorExpr, erd)
    ev.compile()
    ev
  }

  private lazy val textStandardExponentRepExpr = LV('textStandardExponentRep) {
    val qn = this.qNameForProperty("textStandardExponentRep")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.String, textStandardExponentRepRaw)
    c
  }.value

  final lazy val textStandardExponentRepEv = {
    val ev = new TextStandardExponentRepEv(textStandardExponentRepExpr, erd)
    ev.compile()
    ev
  }

  private lazy val binaryFloatRepExpr = LV('binaryFloatRep) {
    val qn = this.qNameForProperty("binaryFloatRep")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, binaryFloatRepRaw)
  }.value

  final lazy val binaryFloatRepEv = {
    val ev = new BinaryFloatRepEv(binaryFloatRepExpr, erd)
    ev.compile()
    ev
  }

  private lazy val textBooleanTrueRepExpr = LV('textBooleanTrueRep) {
    val qn = this.qNameForProperty("textBooleanTrueRep")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, textBooleanTrueRepRaw)
  }.value

  final lazy val textBooleanTrueRepEv = {
    val ev = new TextBooleanTrueRepEv(textBooleanTrueRepExpr, erd)
    ev.compile()
    ev
  }

  private lazy val textBooleanFalseRepExpr = LV('textBooleanFalseRep) {
    val qn = this.qNameForProperty("textBooleanFalseRep")
    ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, textBooleanFalseRepRaw)
  }.value

  final lazy val textBooleanFalseRepEv = {
    val ev = new TextBooleanFalseRepEv(textBooleanFalseRepExpr, erd)
    ev.compile()
    ev
  }

  final lazy val calendarLanguage = LV('calendarLanguage) {
    val qn = this.qNameForProperty("calendarLanguage")
    val c = ExpressionCompilers.String.compile(qn, NodeInfo.NonEmptyString, calendarLanguageRaw)
    c
  }.value
}
