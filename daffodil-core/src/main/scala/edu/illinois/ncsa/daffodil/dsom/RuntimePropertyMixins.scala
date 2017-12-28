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
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.Evaluatable
import edu.illinois.ncsa.daffodil.util.MaybeJULong
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.NilKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextTrimKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextPadKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo
import edu.illinois.ncsa.daffodil.processors.CheckEncodingEv

/*
 * These are the DFDL properties which can have their values come
 * from the data stream itself by way of expressions.
 *
 * TODO: EscapeScheme's have a few more of these runtime properties
 * escapeCharacter, and escapeEscapeCharacter.
 */

trait TermRuntimeValuedPropertiesMixin
  extends DFDLBaseTypeMixin
  with PropertyReferencedElementInfosMixin
  with RawCommonRuntimeValuedPropertiesMixin { decl: Term =>

  private lazy val encodingExpr = LV('encoding) {
    val qn = this.qNameForProperty("encoding")
    this match {
      case eb: ElementBase if (eb.isSimpleType && eb.primType =:= PrimType.HexBinary && (eb.lengthKind =:= LengthKind.Delimited || eb.lengthKind =:= LengthKind.Pattern)) => {
        if (encodingRaw.value.toUpperCase != "ISO-8859-1") {
          SDE("xs:hexBinary with dfdl:lengthKind=\"delimited\" or dfdl:lengthKind=\"pattern\" must have dfdl:encoding=\"ISO-8859-1\", but was \"%s\"", encodingRaw.value)
        }
        //
        // We treat delimited hex binary as a string in iso-8859-1 encoding.
        // That lets us reuse the various string parsing bases to grab the content.
        //
        val qn = decl.qNameForProperty("encoding")
        val exp = ConstantExpression(qn, PrimType.HexBinary, "iso-8859-1")
        exp
      }
      case _ => ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, encodingRaw, decl)
    }
  }.value

  final lazy val encodingEv = {
    val ev = new EncodingEv(encodingExpr, termRuntimeData)
    ev.compile()
    ev
  }

  final lazy val charsetEv = {
    if (maybeCharsetEv.isEmpty) encodingRaw // required property
    maybeCharsetEv.get
  }

  final lazy val maybeCharsetEv =
    if (optionEncodingRaw.isDefined) {
      val ev = new CharsetEv(encodingEv, termRuntimeData)
      ev.compile()
      One(ev)
    } else
      Nope

  final lazy val checkEncodingEv = {
    val ev = new CheckEncodingEv(termRuntimeData, alignmentValueInBits, charsetEv)
    ev.compile()
    ev
  }

  final lazy val maybeFillByteEv = {
    if (optionFillByteRaw.isDefined) {
      val ev = new FillByteEv(fillByte, charsetEv, termRuntimeData)
      ev.compile()
      One(ev)
    } else {
      Nope
    }
  }

  /*
   * Property OutputNewLine
   */

  lazy val outputNewLineEv = {
    val outputNewLineExpr = {
      val qn = this.qNameForProperty("outputNewLine")
      ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, outputNewLineRaw, decl)
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

  private lazy val myPropertyContentReferencedElementInfos =
    propExprElts(optionEncodingRaw, encodingEv, creis(_)) ++
      propExprElts(optionOutputNewLineRaw, outputNewLineEv, creis(_))

  override protected def propertyContentReferencedElementInfos =
    myPropertyContentReferencedElementInfos

  private lazy val myPropertyValueReferencedElementInfos =
    propExprElts(optionEncodingRaw, encodingEv, vreis(_)) ++
      propExprElts(optionOutputNewLineRaw, outputNewLineEv, vreis(_))

  override protected def propertyValueReferencedElementInfos =
    myPropertyValueReferencedElementInfos

  lazy val ignoreCaseBool = ignoreCase == YesNo.Yes
}

trait DelimitedRuntimeValuedPropertiesMixin
  extends TermRuntimeValuedPropertiesMixin
  with RawDelimitedRuntimeValuedPropertiesMixin { decl: Term =>

  lazy val isLengthKindDelimited = {
    this match {
      case mg: ModelGroup => mg.enclosingElement.get.lengthKind == LengthKind.Delimited
      case eb: ElementBase => eb.lengthKind == LengthKind.Delimited
    }
  }

  final protected lazy val initiatorExpr = {
    val qn = this.qNameForProperty("initiator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compileDelimiter(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, initiatorRaw, decl)
  }

  lazy val initiatorParseEv = {
    val ev = new InitiatorParseEv(initiatorExpr, decl.ignoreCaseBool, decl.termRuntimeData)
    ev.compile()
    ev
  }
  lazy val initiatorUnparseEv = {
    val ev = new InitiatorUnparseEv(initiatorExpr, outputNewLineEv, decl.termRuntimeData)
    ev.compile()
    ev
  }

  final def initiatorLoc = (this.diagnosticDebugName, this.path)

  final protected lazy val terminatorExpr = LV('terminator) {
    val qn = this.qNameForProperty("terminator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    val raw = terminatorRaw
    ExpressionCompilers.String.compileDelimiter(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, raw, decl)
  }.value

  final def terminatorLoc = (this.diagnosticDebugName, this.path)

  lazy val terminatorParseEv = {
    val ev = new TerminatorParseEv(terminatorExpr, isLengthKindDelimited, decl.ignoreCaseBool, decl.termRuntimeData)
    ev.compile()
    ev
  }
  lazy val terminatorUnparseEv = {
    val ev = new TerminatorUnparseEv(terminatorExpr, isLengthKindDelimited, outputNewLineEv, decl.termRuntimeData)
    ev.compile()
    ev
  }

  private lazy val myPropertyContentReferencedElementInfos =
    super.propertyContentReferencedElementInfos ++
      propExprElts(optionInitiatorRaw, initiatorExpr, creis(_)) ++
      propExprElts(optionTerminatorRaw, terminatorExpr, creis(_))

  override protected def propertyContentReferencedElementInfos =
    myPropertyContentReferencedElementInfos
}

trait ElementRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with OccursAGMixin
  with LengthAGMixin
  with SimpleTypeRuntimeValuedPropertiesMixin
  with RawElementRuntimeValuedPropertiesMixin { decl: ElementBase =>

  private lazy val byteOrderExpr = LV('byteOrder) {
    val qn = this.qNameForProperty("byteOrder")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, byteOrderRaw, decl)
  }.value

  final lazy val byteOrderEv = {
    if (maybeByteOrderEv.isEmpty) byteOrderRaw // must be defined
    maybeByteOrderEv.get
  }

  final lazy val maybeByteOrderEv = {
    if (optionByteOrderRaw.isDefined) {
      val ev = new ByteOrderEv(byteOrderExpr, elementRuntimeData)
      ev.compile()
      One(ev)
    } else {
      Nope
    }
  }

  protected final lazy val lengthExpr = {
    val qn = this.qNameForProperty("length")
    ExpressionCompilers.JLong.compileProperty(qn, NodeInfo.Long, lengthRaw, decl)
  }

  private lazy val explicitLengthEv: ExplicitLengthEv = {
    Assert.usage(lengthKind eq LengthKind.Explicit)
    val ev = new ExplicitLengthEv(lengthExpr, erd)
    ev.compile()
    ev
  }

  private lazy val implicitLengthEv: LengthEv = {
    Assert.usage(lengthKind eq LengthKind.Implicit)
    import Representation._
    import NodeInfo._
    lazy val maxLengthLong = maxLength.longValueExact
    val ev = (impliedRepresentation, typeDef.typeNode) match {
      case (Text, String) => new ImplicitLengthEv(maxLengthLong, erd)
      case (Binary, HexBinary) => new ImplicitLengthEv(maxLengthLong, erd)
      case (Binary, _) => new ImplicitLengthEv(implicitBinaryLengthInBits, erd)
      case (Text, _) =>
        SDE("Type %s with dfdl:representation='text' cannot have dfdl:lengthKind='implicit'", typeDef.typeNode.name)
    }
    ev.compile()
    ev
  }

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

  final lazy val maybeLengthEv: Maybe[LengthEv] = {
    if (this.optionLengthRaw.isDefined && (lengthKind eq LengthKind.Explicit)) One(lengthEv)
    else None
  }

  protected final lazy val optLengthConstant: Option[Long] = {
    if (maybeLengthEv.isDefined) {
      lengthEv.optConstant.map { _.longValue }
    } else None
  }

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
  lazy val elementLengthInBitsEv: LengthInBitsEv = {
    Assert.usage((lengthKind eq LengthKind.Implicit) || (lengthKind eq LengthKind.Explicit))
    import LengthKind._
    import Representation._
    import NodeInfo._
    val (units: LengthUnits, lenEv: LengthEv) =
      (lengthKind, impliedRepresentation, typeDef.typeNode) match {
        case (Explicit, Binary, HexBinary) => (lengthUnits, explicitLengthEv)
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

  protected lazy val (minLenUnits: LengthUnits, minLen: Long) = {
    import LengthKind._
    import Representation._
    import NodeInfo._
    lazy val maxLengthLong = maxLength.longValueExact
    lazy val minLengthLong = minLength.longValueExact
    val res: (LengthUnits, Long) =
      (lengthKind, impliedRepresentation, typeDef.typeNode) match {
        case (Implicit, Binary, HexBinary) => (LengthUnits.Bytes, maxLengthLong) // fixed length
        case (Implicit, Text, AnySimpleType) => (lengthUnits, textOutputMinLength) // fixed length
        case (Implicit, Text, String) => (lengthUnits, maxLengthLong) // fixed length
        case (Explicit, Text, String) => (lengthUnits, minLengthLong)
        case (Explicit, Binary, HexBinary) => (LengthUnits.Bytes, minLengthLong)
        case (Explicit, Text, AnySimpleType) => (lengthUnits, textOutputMinLength)
        case (Prefixed, _, String) => (lengthUnits, minLengthLong)
        case (Prefixed, Text, _) => (lengthUnits, textOutputMinLength)
        case (Pattern, _, String) => (lengthUnits, minLengthLong)
        case (Pattern, Text, _) => (lengthUnits, textOutputMinLength)
        case (Delimited, _, String) => (lengthUnits, minLengthLong)
        case (Delimited, Text, _) => (lengthUnits, textOutputMinLength)
        case _ => (LengthUnits.Bits, 0L) // anything else. This shuts off checking a min.
      }
    res
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

  /**
   * Used for padding, which might be for specified-length, or might be
   * for delimited.
   */
  final lazy val maybeUnparseMinOrTargetLengthInBitsEv: Maybe[Evaluatable[MaybeJULong]] = {
    if ((this.optionLengthRaw.isDefined &&
      (lengthKind _eq_ LengthKind.Explicit)) ||
      ((lengthKind _eq_ LengthKind.Implicit) && isSimpleType)) {
      maybeUnparseTargetLengthInBitsEv
    } else if (this.isDelimitedPrefixedPatternWithPadding) {
      //
      // if delimited but there is a min length, we just need the min
      // length. There is no target length other than it.
      //
      val ev = minLengthInBitsEv
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
    ExpressionCompilers.JLong.compileProperty(qn, NodeInfo.Long, occursCountRaw, decl, isEvaluatedAbove)
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

  lazy val maybeLiteralNilEv = {
    if (this.isNillable && (this.nilKind eq NilKind.LiteralValue))
      One(nilStringLiteralForUnparserEv)
    else
      Nope
  }

  private def lengthReferencedElements(f: F) =
    if (maybeLengthEv.isDefined)
      f(this.explicitLengthEv)
    else
      ReferencedElementInfos.None

  private def localElementPropertyReferencedElements(f: F) = {
    propExprElts(optionByteOrderRaw, byteOrderEv, f) ++
      lengthReferencedElements(f) ++
      propExprElts(optionOccursCountRaw, occursCountEv, f) ++
      propExprElts(optionTextStandardDecimalSeparatorRaw, textStandardDecimalSeparatorEv, f) ++
      propExprElts(optionTextStandardGroupingSeparatorRaw, textStandardGroupingSeparatorEv, f) ++
      propExprElts(optionTextStandardExponentRepRaw, textStandardExponentRepEv, f) ++
      propExprElts(optionBinaryFloatRepRaw, binaryFloatRepEv, f) ++
      (
        if (isSimpleType && primType =:= PrimType.Boolean) {
          propExprElts(optionTextBooleanTrueRepRaw, textBooleanTrueRepEv, f) ++
            propExprElts(optionTextBooleanFalseRepRaw, textBooleanFalseRepEv, f)
        } else {
          ReferencedElementInfos.None
        }) ++
        propExprElts(optionCalendarLanguageRaw, calendarLanguage, f) ++
        (
          if (optionEscapeScheme.isDefined) {
            val es: DFDLEscapeScheme = optionEscapeScheme.get
            val ee =
              if (es.optionEscapeEscapeCharacterEv.isDefined)
                f(es.optionEscapeEscapeCharacterEv.get)
              else
                ReferencedElementInfos.None
            ee ++
              propExprElts(es.optionEscapeCharacterRaw, es.escapeCharacterEv, f)
          } else {
            ReferencedElementInfos.None
          })
  }

  private lazy val myPropertyContentReferencedElementInfos =
    super.propertyContentReferencedElementInfos ++
      localElementPropertyReferencedElements(creis(_))

  final override protected def propertyContentReferencedElementInfos =
    myPropertyContentReferencedElementInfos

  private lazy val myPropertyValueReferencedElementInfos =
    super.propertyValueReferencedElementInfos ++
      localElementPropertyReferencedElements(vreis(_))

  final override protected def propertyValueReferencedElementInfos =
    myPropertyValueReferencedElementInfos
}

trait SequenceRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with Sequence_AnnotationMixin
  with RawSequenceRuntimeValuedPropertiesMixin { decl: SequenceTermBase =>

  private lazy val separatorExpr = {
    val qn = this.qNameForProperty("separator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compileDelimiter(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, separatorRaw, decl)
  }

  lazy val separatorParseEv = {
    val ev = new SeparatorParseEv(separatorExpr, decl.ignoreCaseBool, decl.termRuntimeData)
    ev.compile()
    ev
  }
  lazy val separatorUnparseEv = {
    val ev = new SeparatorUnparseEv(separatorExpr, outputNewLineEv, decl.termRuntimeData)
    ev.compile()
    ev
  }

  final def separatorLoc = (this.diagnosticDebugName, this.path)

  private lazy val myPropertyContentReferencedElementInfos =
    super.propertyContentReferencedElementInfos ++
      propExprElts(optionSeparatorRaw, separatorExpr, creis(_))

  final override protected def propertyContentReferencedElementInfos =
    myPropertyContentReferencedElementInfos

  private lazy val myPropertyValueReferencedElementInfos =
    super.propertyContentReferencedElementInfos ++
      propExprElts(optionSeparatorRaw, separatorExpr, vreis(_))

  final override protected def propertyValueReferencedElementInfos =
    myPropertyValueReferencedElementInfos

}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends DFDLSimpleTypeMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin { decl: ElementBase =>

  private lazy val textStandardDecimalSeparatorExpr = LV('textStandardDecimalSeparator) {
    val qn = this.qNameForProperty("textStandardDecimalSeparator")
    val c = ExpressionCompilers.String.compileProperty(qn, NodeInfo.String, textStandardDecimalSeparatorRaw, decl)
    c
  }.value

  final lazy val textStandardDecimalSeparatorEv = {
    val ev = new TextStandardDecimalSeparatorEv(textStandardDecimalSeparatorExpr, erd)
    ev.compile()
    ev
  }

  private lazy val textStandardGroupingSeparatorExpr = LV('textStandardGroupingSeparator) {
    val qn = this.qNameForProperty("textStandardGroupingSeparator")
    val c = ExpressionCompilers.String.compileProperty(qn, NodeInfo.String, textStandardGroupingSeparatorRaw, decl)
    c
  }.value

  final lazy val textStandardGroupingSeparatorEv = {
    val ev = new TextStandardGroupingSeparatorEv(textStandardGroupingSeparatorExpr, erd)
    ev.compile()
    ev
  }

  private lazy val textStandardExponentRepExpr = LV('textStandardExponentRep) {
    val qn = this.qNameForProperty("textStandardExponentRep")
    val c = ExpressionCompilers.String.compileProperty(qn, NodeInfo.String, textStandardExponentRepRaw, decl)
    c
  }.value

  final lazy val textStandardExponentRepEv = {
    val ev = new TextStandardExponentRepEv(textStandardExponentRepExpr, erd)
    ev.compile()
    ev
  }

  private lazy val binaryFloatRepExpr = LV('binaryFloatRep) {
    val qn = this.qNameForProperty("binaryFloatRep")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, binaryFloatRepRaw, decl)
  }.value

  final lazy val binaryFloatRepEv = {
    if (maybeBinaryFloatRepEv.isEmpty) binaryFloatRepRaw // property is required
    maybeBinaryFloatRepEv.get
  }

  final lazy val maybeBinaryFloatRepEv = {
    if (optionBinaryFloatRepRaw.isDefined) {
      val ev = new BinaryFloatRepEv(binaryFloatRepExpr, erd)
      ev.compile()
      One(ev)
    } else {
      Nope
    }
  }

  private lazy val textBooleanTrueRepExpr = LV('textBooleanTrueRep) {
    val qn = this.qNameForProperty("textBooleanTrueRep")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, textBooleanTrueRepRaw, decl)
  }.value

  private lazy val textBooleanFalseRepExpr = LV('textBooleanFalseRep) {
    val qn = this.qNameForProperty("textBooleanFalseRep")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, textBooleanFalseRepRaw, decl)
  }.value

  final lazy val textBooleanTrueRepEv = {
    val mustBeSameLength = (((this.lengthKind eq LengthKind.Explicit) || (this.lengthKind eq LengthKind.Implicit)) &&
      ((this.textPadKind eq TextPadKind.None) || (this.textTrimKind eq TextTrimKind.None)))
    val ev = new TextBooleanTrueRepEv(textBooleanTrueRepExpr, textBooleanFalseRepEv, mustBeSameLength, erd)
    ev.compile()
    ev
  }

  final lazy val textBooleanFalseRepEv = {
    val ev = new TextBooleanFalseRepEv(textBooleanFalseRepExpr, erd)
    ev.compile()
    ev
  }

  final lazy val calendarLanguage = LV('calendarLanguage) {
    val qn = this.qNameForProperty("calendarLanguage")
    val c = ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, calendarLanguageRaw, decl)
    c
  }.value
}
