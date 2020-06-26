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

package org.apache.daffodil.dsom

import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.equality.TypeEqual
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.BinaryFloatRepEv
import org.apache.daffodil.processors.ByteOrderEv
import org.apache.daffodil.processors.CharsetEv
import org.apache.daffodil.processors.EncodingEv
import org.apache.daffodil.processors.ExplicitLengthEv
import org.apache.daffodil.processors.FillByteEv
import org.apache.daffodil.processors.ImplicitLengthEv
import org.apache.daffodil.processors.InitiatorParseEv
import org.apache.daffodil.processors.InitiatorUnparseEv
import org.apache.daffodil.processors.OccursCountEv
import org.apache.daffodil.processors.OutputNewLineEv
import org.apache.daffodil.processors.SeparatorParseEv
import org.apache.daffodil.processors.SeparatorUnparseEv
import org.apache.daffodil.processors.TerminatorParseEv
import org.apache.daffodil.processors.TerminatorUnparseEv
import org.apache.daffodil.processors.TextBooleanFalseRepEv
import org.apache.daffodil.processors.TextBooleanTrueRepEv
import org.apache.daffodil.processors.TextStandardDecimalSeparatorEv
import org.apache.daffodil.processors.TextStandardExponentRepEv
import org.apache.daffodil.processors.TextStandardGroupingSeparatorEv
import org.apache.daffodil.schema.annotation.props.gen.DFDLBaseTypeMixin
import org.apache.daffodil.schema.annotation.props.gen.DFDLSimpleTypeMixin
import org.apache.daffodil.schema.annotation.props.gen.LengthAGMixin
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.schema.annotation.props.gen.ChoiceAGMixin
import org.apache.daffodil.schema.annotation.props.gen.OccursAGMixin
import org.apache.daffodil.schema.annotation.props.gen.Representation
import org.apache.daffodil.schema.annotation.props.gen.Sequence_AnnotationMixin
import org.apache.daffodil.processors.LengthInBitsEv
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.processors.LengthEv
import org.apache.daffodil.processors.MinLengthInBitsEv
import org.apache.daffodil.processors.UnparseTargetLengthInBitsEv
import org.apache.daffodil.processors.unparsers.NilStringLiteralForUnparserEv
import org.apache.daffodil.processors.UnparseTargetLengthInCharactersEv
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.util.MaybeJULong
import org.apache.daffodil.schema.annotation.props.gen.NilKind
import org.apache.daffodil.schema.annotation.props.gen.TextTrimKind
import org.apache.daffodil.schema.annotation.props.gen.TextPadKind
import org.apache.daffodil.schema.annotation.props.gen.YesNo
import org.apache.daffodil.processors.LayerTransformEv
import org.apache.daffodil.processors.LayerEncodingEv
import org.apache.daffodil.processors.LayerLengthInBytesEv
import org.apache.daffodil.processors.LayerBoundaryMarkEv
import org.apache.daffodil.processors.LayerCharsetEv
import org.apache.daffodil.schema.annotation.props.TextStandardExponentRepMixin
import org.apache.daffodil.schema.annotation.props.Found

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
  with ChoiceAGMixin
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
      case _ => ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, encodingRaw, decl, dpathCompileInfo)
    }
  }.value

  final lazy val encodingEv = {
    val ev = new EncodingEv(encodingExpr, tci)
    ev.compile(tunable)
    ev
  }

  final lazy val charsetEv = {
    if (maybeCharsetEv.isEmpty) encodingRaw // required property
    maybeCharsetEv.get
  }

  final lazy val maybeCharsetEv =
    if (optionEncodingRaw.isDefined) {
      val ev = new CharsetEv(encodingEv, tci)
      ev.compile(tunable)
      One(ev)
    } else
      Nope

  final lazy val maybeFillByteEv = {
    if (optionFillByteRaw.isDefined) {
      val ev = new FillByteEv(fillByte, charsetEv, tci)
      ev.compile(tunable)
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
      ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, outputNewLineRaw, decl, dpathCompileInfo)
    }
    val ev = new OutputNewLineEv(outputNewLineExpr, tci)
    ev.compile(tunable)
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

  private lazy val isLengthKindDelimited = {
    this match {
      case mg: ModelGroup => false
      case eb: ElementBase => eb.lengthKind == LengthKind.Delimited
    }
  }

  final protected lazy val initiatorExpr = {
    val qn = this.qNameForProperty("initiator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compileDelimiter(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, initiatorRaw, decl, dpathCompileInfo)
  }

  lazy val initiatorParseEv = {
    val ev = new InitiatorParseEv(initiatorExpr, decl.ignoreCaseBool, decl.tci)
    ev.compile(tunable)
    ev
  }
  lazy val initiatorUnparseEv = {
    val ev = new InitiatorUnparseEv(initiatorExpr, outputNewLineEv, decl.tci)
    ev.compile(tunable)
    ev
  }

  final def initiatorLoc = (this.diagnosticDebugName, this.path)

  final protected lazy val terminatorExpr = LV('terminator) {
    val qn = this.qNameForProperty("terminator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    val raw = terminatorRaw
    ExpressionCompilers.String.compileDelimiter(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, raw, decl, dpathCompileInfo)
  }.value

  final def terminatorLoc = (this.diagnosticDebugName, this.path)

  lazy val terminatorParseEv = {
    val ev = new TerminatorParseEv(terminatorExpr, isLengthKindDelimited, decl.ignoreCaseBool, decl.tci)
    ev.compile(tunable)
    ev
  }
  lazy val terminatorUnparseEv = {
    val ev = new TerminatorUnparseEv(terminatorExpr, isLengthKindDelimited, outputNewLineEv, decl.tci)
    ev.compile(tunable)
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
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, byteOrderRaw, decl, dpathCompileInfo)
  }.value

  final lazy val byteOrderEv = {
    if (maybeByteOrderEv.isEmpty) byteOrderRaw // must be defined
    maybeByteOrderEv.get
  }

  final lazy val maybeByteOrderEv = {
    if (isSimpleType && (primType == PrimType.HexBinary || primType == PrimType.AnyURI)) {
      // xs:hexBinary and xs:anyURI types should ignore the byteOrder property and
      // effectively always use a bigEndian byteOrder. One way to accomplish
      // this would be to modify the byteOrderExpr to return a constant value
      // of "bigEndian" for hexBinary types. The problem with this is that
      // Daffodil does not allow byteOrder="bigEndian" with
      // bitOrder="leastSignificantBitFirst", which is what would happen when
      // parsing hexBinary data with LSBF bitOrder, resulting in an SDE. So
      // instead, we just set this to Nope, and ensure we never try to get the
      // byteOrder property when the type is xs:hexBinary/anyURI. The IO layer will
      // see this Nope and do the right thing.
      Nope
    } else if (optionByteOrderRaw.isDefined) {
      val ev = new ByteOrderEv(byteOrderExpr, eci)
      ev.compile(tunable)
      One(ev)
    } else {
      Nope
    }
  }

  protected final lazy val lengthExpr = {
    val qn = this.qNameForProperty("length")
    ExpressionCompilers.JLong.compileProperty(qn, NodeInfo.Long, lengthRaw, decl, dpathCompileInfo)
  }

  private lazy val explicitLengthEv: ExplicitLengthEv = {
    Assert.usage(lengthKind eq LengthKind.Explicit)
    val ev = new ExplicitLengthEv(lengthExpr, eci)
    ev.compile(tunable)
    ev
  }

  private lazy val implicitLengthEv: LengthEv = {
    Assert.usage(lengthKind eq LengthKind.Implicit)
    import Representation._
    import NodeInfo._
    lazy val maxLengthLong = maxLength.longValueExact
    val ev = (impliedRepresentation, typeDef.typeNode) match {
      case (Text, String) => new ImplicitLengthEv(maxLengthLong, eci)
      case (Binary, HexBinary) => new ImplicitLengthEv(maxLengthLong, eci)
      case (Binary, _) => new ImplicitLengthEv(implicitBinaryLengthInBits, eci)
      case (Text, _) =>
        SDE("Type %s with dfdl:representation='text' cannot have dfdl:lengthKind='implicit'", typeDef.typeNode.name)
    }
    ev.compile(tunable)
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

  final lazy val optLengthConstant: Option[Long] = {
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
    val ev = new LengthInBitsEv(units, lengthKind, maybeCharsetEv, lenEv, eci)
    ev.compile(tunable)
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
    val ev = new MinLengthInBitsEv(minLenUnits, lengthKind, maybeCharsetEv, minLen, eci)
    ev.compile(tunable)
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
        case (Implicit, Text, String) => (lengthUnits, maxLengthLong) // fixed length
        case (Implicit, Text, AnySimpleType) => (lengthUnits, textOutputMinLength) // fixed length
        case (Explicit, Text, String) => (lengthUnits, 0L)
        case (Explicit, Binary, HexBinary) => (LengthUnits.Bytes, 0L)
        case (Explicit, Text, AnySimpleType) => (lengthUnits, 0L)
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

  protected final lazy val optionTextOutputMinLength = findPropertyOption("textOutputMinLength")

  /**
   * We only use textOutputMinLength in a very narrow set of circumstances.
   * Otherwise we assume 0.
   */
  lazy val textOutputMinLength: Long = {
    val d = decl
    val pt = decl.primType
    val useTextOutputMinLength: Boolean = {
      d.isSimpleType &&
        (pt ne PrimType.String) &&
        (pt ne PrimType.HexBinary) &&
        (d.impliedRepresentation eq Representation.Text) &&
        d.optionTextPadKind.isDefined &&
        (d.textPadKind eq TextPadKind.PadChar) &&
        {
          import LengthKind._
          val lk = d.lengthKind
          lk match {
            case Delimited | Prefixed | Pattern | EndOfParent => true
            case Explicit if (!d.lengthEv.isConstant) => true
            case _ => false
          }
        }
    }
    val res: Long =
      if (useTextOutputMinLength) {
        val Found(value, _, _, _) = findProperty("textOutputMinLength")
        value.toLong
      } else {
        // if it is defined, use it, otherwise 0.
        val optTOML = findPropertyOption("textOutputMinLength")
        optTOML match {
          case Found(value, _, _, _) => value.toLong
          case _ => 0
        }
      }
    res
  }

  final lazy val maybeUnparseTargetLengthInBitsEv: Maybe[UnparseTargetLengthInBitsEv] = {
    if (this.isSimpleType && this.simpleType.optRepTypeElement.isDefined) {
      this.simpleType.optRepTypeElement.get.maybeUnparseTargetLengthInBitsEv
    } else {
      if ((this.optionLengthRaw.isDefined &&
        (lengthKind _eq_ LengthKind.Explicit)) ||
        ((lengthKind _eq_ LengthKind.Implicit) && isSimpleType)) {
        val ev = unparseTargetLengthInBitsEv
        One(ev)
      } else
        Nope
    }
  }

  /**
   * Used for padding, which might be for specified-length, or might be
   * for delimited.
   */
  final lazy val maybeUnparseMinOrTargetLengthInBitsEv: Maybe[Evaluatable[MaybeJULong]] = {
    if (this.isSimpleType && this.simpleType.optRepTypeElement.isDefined) {
      this.simpleType.optRepTypeElement.get.maybeUnparseMinOrTargetLengthInBitsEv
    } else {
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
  }

  final lazy val unparseTargetLengthInBitsEv: UnparseTargetLengthInBitsEv = {
    if (this.isSimpleType && this.simpleType.optRepTypeElement.isDefined) {
      this.simpleType.optRepTypeElement.get.unparseTargetLengthInBitsEv
    } else {
      val ev = new UnparseTargetLengthInBitsEv(elementLengthInBitsEv, minLengthInBitsEv, eci)
      ev.compile(tunable)
      ev
    }
  }

  final lazy val maybeUnparseTargetLengthInCharactersEv: Maybe[UnparseTargetLengthInCharactersEv] = {
    if (this.isSimpleType && this.simpleType.optRepTypeElement.isDefined) {
      this.simpleType.optRepTypeElement.get.maybeUnparseTargetLengthInCharactersEv
    } else {
      if ((lengthUnits eq LengthUnits.Characters) &&
        (this.optionLengthRaw.isDefined &&
          (lengthKind _eq_ LengthKind.Explicit)) ||
          ((lengthKind _eq_ LengthKind.Implicit) && isSimpleType)) {
        val optCs = charsetEv.optConstant
        if (optCs.isEmpty || optCs.get.maybeFixedWidth.isEmpty) {
          val ev = new UnparseTargetLengthInCharactersEv(lengthEv, charsetEv, minLen, eci)
          ev.compile(tunable)
          One(ev)
        } else
          Nope
      } else
        Nope
    }
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
    ExpressionCompilers.JLong.compileProperty(qn, NodeInfo.Long, occursCountRaw, decl, dpathCompileInfo, isEvaluatedAbove)
  }.value

  lazy val occursCountEv = {
    val ev = new OccursCountEv(occursCountExpr, eci)
    ev.compile(tunable)
    ev
  }

  lazy val nilStringLiteralForUnparserEv = {
    val ev = new NilStringLiteralForUnparserEv(tci, maybeOutputNewLineEv, rawNilValuesForUnparse.head)
    ev.compile(tunable)
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
    val booleanTextExprElts =
      if (isSimpleType && primType =:= PrimType.Boolean) {
        propExprElts(optionTextBooleanTrueRepRaw, textBooleanTrueRepEv, f) ++
          propExprElts(optionTextBooleanFalseRepRaw, textBooleanFalseRepEv, f)
      } else {
        ReferencedElementInfos.None
      }

    val escapeSchemeExprElts =
      if (optionEscapeScheme.isDefined) {
        val es: DFDLEscapeScheme = optionEscapeScheme.get
        val ee =
          if (es.optionEscapeEscapeCharacterEv.isDefined)
            f(es.optionEscapeEscapeCharacterEv.get)
          else
            ReferencedElementInfos.None
        ee ++ propExprElts(es.optionEscapeCharacterRaw, es.escapeCharacterEv, f)
      } else {
        ReferencedElementInfos.None
      }

    val byteOrderExprElts =
      if (isSimpleType && (primType =:= PrimType.HexBinary || primType =:= PrimType.AnyURI)) {
        ReferencedElementInfos.None
      } else {
        propExprElts(optionByteOrderRaw, byteOrderEv, f)
      }

    lengthReferencedElements(f) ++
      propExprElts(optionOccursCountRaw, occursCountEv, f) ++
      propExprElts(optionTextStandardDecimalSeparatorRaw, textStandardDecimalSeparatorEv, f) ++
      propExprElts(optionTextStandardGroupingSeparatorRaw, textStandardGroupingSeparatorEv, f) ++
      propExprElts(optionTextStandardExponentRepRaw, textStandardExponentRepEv, f) ++
      propExprElts(optionBinaryFloatRepRaw, binaryFloatRepEv, f) ++
      propExprElts(optionCalendarLanguageRaw, calendarLanguage, f) ++
      booleanTextExprElts ++
      escapeSchemeExprElts ++
      byteOrderExprElts
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

  /**
   *  Insures at compile time if the separator and terminator are both
   *  statically known, that they are not the same.
   *
   *  If there is a possible terminator that could be after this,
   *  or enclosing group separator, that could be after this,
   *  then it has to not be ambiguous with this sequence's separator.
   *
   *  Note that checking, in general, for whether two delimiter DFA things
   *  can accept the same string, or one can accept a prefix of something the
   *  other accepts, is generally hard, and even if someone creates things with
   *  some ambiguity of that sort, real data might not ever run into that
   *  ambiguity. So spurious warnings are a possible outcome.
   *
   *  DFDL specifically does not check for, nor require detection of this
   *  sort of ambiguity at runtime or at compile time. But when it's
   *  completely obvious at compile time it's sensible to give an error.
   *
   *  TODO: An improvement - the enclosing sequence object should really be
   *  passing a list of possible terminating markup down to each sequence child object.
   *  Those that aren't runtime-valued exprsesions could be checked for
   *  ambiguity.
   *
   *  For now, we just check if this sequence itself has a constant
   *  separator and terminator that are the same. That is, we're checking
   *  for an obvious kind of cut/paste error by the schema author.
   */
  final lazy val checkSeparatorTerminatorConflict: Unit = {
    if (hasTerminator) {
      val termEV = this.terminatorParseEv
      val sepEV = this.separatorParseEv
      if (termEV.isConstant && sepEV.isConstant) {
        val termDelimArray = termEV.constValue
        val sepDelimArray = sepEV.constValue
        val terms = termDelimArray.map { _.lookingFor }.toSet
        val seps = sepDelimArray.map { _.lookingFor }.toSet
        val inBoth = terms.intersect(seps)
        if (!inBoth.isEmpty) {
          SDE(
            "The dfdl:terminator and dfdl:separator properties must be distinct. Both contain: %s.",
            inBoth.map { s => "'" + s + "'" }.mkString(", "))
        }
      }
    }
  }

  private lazy val separatorExpr = {
    val qn = this.qNameForProperty("separator")
    val typeIfStaticallyKnown = NodeInfo.String
    val typeIfRuntimeKnown = NodeInfo.NonEmptyString
    ExpressionCompilers.String.compileDelimiter(qn, typeIfStaticallyKnown, typeIfRuntimeKnown, separatorRaw, decl, dpathCompileInfo)
  }

  lazy val separatorParseEv = {
    val ev = new SeparatorParseEv(separatorExpr, decl.ignoreCaseBool, decl.tci)
    ev.compile(tunable)
    ev
  }

  lazy val separatorUnparseEv = {
    val ev = new SeparatorUnparseEv(separatorExpr, outputNewLineEv, decl.tci)
    ev.compile(tunable)
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

trait LayeringRuntimeValuedPropertiesMixin
  extends RawLayeringRuntimeValuedPropertiesMixin { decl: SequenceTermBase =>

  private lazy val layerTransformExpr = {
    val qn = this.qNameForProperty("layerTransform")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, layerTransformRaw, decl, dpathCompileInfo)
  }

  //  final lazy val layerTransformEv = {
  //    if (maybeLayerTransformEv.isEmpty) layerTransformRaw // must be defined
  //    maybeLayerTransformEv.get
  //  }

  final lazy val maybeLayerTransformEv = {
    if (optionLayerTransformRaw.isDefined) {
      val ev = new LayerTransformEv(layerTransformExpr, tci)
      ev.compile(tunable)
      One(ev)
    } else {
      Nope
    }
  }

  private lazy val layerEncodingExpr = {
    val qn = this.qNameForProperty("layerEncoding")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, layerEncodingRaw, decl, dpathCompileInfo)
  }

  private final lazy val layerEncodingEv = {
    if (maybeLayerEncodingEv.isEmpty) layerEncodingRaw // must be defined
    maybeLayerEncodingEv.get
  }

  private final lazy val maybeLayerEncodingEv = {
    if (optionLayerEncodingRaw.isDefined) {
      val ev = new LayerEncodingEv(layerEncodingExpr, tci)
      ev.compile(tunable)
      One(ev)
    } else {
      Nope
    }
  }

  final lazy val maybeLayerCharsetEv =
    if (optionLayerEncodingRaw.isDefined) {
      val ev = new LayerCharsetEv(layerEncodingEv, tci)
      ev.compile(tunable)
      One(ev)
    } else
      Nope

  private lazy val layerLengthExpr = {
    val qn = this.qNameForProperty("layerLength")
    ExpressionCompilers.JLong.compileProperty(qn, NodeInfo.Long, layerLengthRaw, decl, dpathCompileInfo)
  }

  final lazy val maybeLayerLengthInBytesEv = {
    if (optionLayerLengthRaw.isDefined) {
      layerLengthUnits
      val ev = new LayerLengthInBytesEv(layerLengthExpr, tci)
      ev.compile(tunable)
      One(ev)
    } else {
      Nope
    }
  }

  private lazy val layerBoundaryMarkExpr = {
    val qn = this.qNameForProperty("layerBoundaryMark")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.String, layerBoundaryMarkRaw, decl, dpathCompileInfo)
  }

  //  final lazy val layerBoundaryMarkEv = {
  //    if (maybeLayerBoundaryMarkEv.isEmpty) layerBoundaryMarkRaw // must be defined
  //    maybeLayerBoundaryMarkEv.get
  //  }

  final lazy val maybeLayerBoundaryMarkEv = {
    if (optionLayerBoundaryMarkRaw.isDefined) {
      val ev = new LayerBoundaryMarkEv(layerBoundaryMarkExpr, tci)
      ev.compile(tunable)
      One(ev)
    } else {
      Nope
    }
  }

}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends DFDLSimpleTypeMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin
  with TextStandardExponentRepMixin { decl: ElementBase =>

  private lazy val textStandardDecimalSeparatorExpr = LV('textStandardDecimalSeparator) {
    val qn = this.qNameForProperty("textStandardDecimalSeparator")
    val c = ExpressionCompilers.String.compileProperty(qn, NodeInfo.String, textStandardDecimalSeparatorRaw, decl, dpathCompileInfo)
    c
  }.value

  final lazy val textStandardDecimalSeparatorEv = {
    val ev = new TextStandardDecimalSeparatorEv(textStandardDecimalSeparatorExpr, eci)
    ev.compile(tunable)
    ev
  }

  private lazy val textStandardGroupingSeparatorExpr = LV('textStandardGroupingSeparator) {
    val qn = this.qNameForProperty("textStandardGroupingSeparator")
    val c = ExpressionCompilers.String.compileProperty(qn, NodeInfo.String, textStandardGroupingSeparatorRaw, decl, dpathCompileInfo)
    c
  }.value

  final lazy val textStandardGroupingSeparatorEv = {
    val ev = new TextStandardGroupingSeparatorEv(textStandardGroupingSeparatorExpr, eci)
    ev.compile(tunable)
    ev
  }

  private lazy val textStandardExponentRepExpr = LV('textStandardExponentRep) {
    val qn = this.qNameForProperty("textStandardExponentRep")
    val c = ExpressionCompilers.String.compileProperty(qn, NodeInfo.String, textStandardExponentRep, decl, dpathCompileInfo)
    c
  }.value

  final lazy val textStandardExponentRepEv = {
    val ev = new TextStandardExponentRepEv(textStandardExponentRepExpr, eci)
    ev.compile(tunable)
    ev
  }

  private lazy val binaryFloatRepExpr = LV('binaryFloatRep) {
    val qn = this.qNameForProperty("binaryFloatRep")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, binaryFloatRepRaw, decl, dpathCompileInfo)
  }.value

  final lazy val binaryFloatRepEv = {
    if (maybeBinaryFloatRepEv.isEmpty) binaryFloatRepRaw // property is required
    maybeBinaryFloatRepEv.get
  }

  final lazy val maybeBinaryFloatRepEv = {
    if (optionBinaryFloatRepRaw.isDefined) {
      val ev = new BinaryFloatRepEv(binaryFloatRepExpr, eci)
      ev.compile(tunable)
      One(ev)
    } else {
      Nope
    }
  }

  private lazy val textBooleanTrueRepExpr = LV('textBooleanTrueRep) {
    val qn = this.qNameForProperty("textBooleanTrueRep")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, textBooleanTrueRepRaw, decl, dpathCompileInfo)
  }.value

  private lazy val textBooleanFalseRepExpr = LV('textBooleanFalseRep) {
    val qn = this.qNameForProperty("textBooleanFalseRep")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, textBooleanFalseRepRaw, decl, dpathCompileInfo)
  }.value

  final lazy val textBooleanTrueRepEv = {
    val mustBeSameLength = (((this.lengthKind eq LengthKind.Explicit) || (this.lengthKind eq LengthKind.Implicit)) &&
      ((this.textPadKind eq TextPadKind.None) || (this.textTrimKind eq TextTrimKind.None)))
    val ev = new TextBooleanTrueRepEv(textBooleanTrueRepExpr, textBooleanFalseRepEv, mustBeSameLength, eci)
    ev.compile(tunable)
    ev
  }

  final lazy val textBooleanFalseRepEv = {
    val ev = new TextBooleanFalseRepEv(textBooleanFalseRepExpr, eci)
    ev.compile(tunable)
    ev
  }

  final lazy val calendarLanguage = LV('calendarLanguage) {
    val qn = this.qNameForProperty("calendarLanguage")
    val c = ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, calendarLanguageRaw, decl, eci)
    c
  }.value
}
