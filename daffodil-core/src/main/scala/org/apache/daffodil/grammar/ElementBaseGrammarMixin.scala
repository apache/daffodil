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

import java.lang.{ Long => JLong }
import scala.Boolean
import scala.Long
import org.apache.daffodil.api.WarnID
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom.PrefixLengthQuasiElementDecl
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.dsom.ExpressionCompilers
import org.apache.daffodil.dsom.InitiatedTerminatedMixin
import org.apache.daffodil.dsom.TunableLimitExceededError
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar.primitives._ // there are too many to show individually
import org.apache.daffodil.processors.TextJustificationType
import org.apache.daffodil.schema.annotation.props.Found
import org.apache.daffodil.schema.annotation.props.NotFound
import org.apache.daffodil.schema.annotation.props.gen._ // there are too many to show individually
import org.apache.daffodil.util.PackedSignCodes
import org.apache.daffodil.xml.GlobalQName
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.dsom.ExpressionCompilers
import org.apache.daffodil.runtime1.ElementBaseRuntime1Mixin
import org.apache.daffodil.grammar.primitives.ConvertTextBooleanPrim
import org.apache.daffodil.grammar.primitives.LeadingSkipRegion
import org.apache.daffodil.grammar.primitives.SimpleNilOrValue
import org.apache.daffodil.grammar.primitives.LiteralValueNilOfSpecifiedLength

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

trait ElementBaseGrammarMixin
  extends InitiatedTerminatedMixin
  with AlignedMixin
  with HasStatementsGrammarMixin
  with PaddingInfoMixin
  with ElementBaseRuntime1Mixin { self: ElementBase =>

  private val context = this

  private lazy val (leftPadding, rightPadding) = {
    if (unparsingPadChar.isEmpty)
      (EmptyGram, EmptyGram)
    else {
      import TextJustificationType._
      this.justificationPad match {
        case None => (EmptyGram, EmptyGram)
        case Left => (EmptyGram, OnlyPadding(context))
        case Right => (OnlyPadding(context), EmptyGram)
        case Center => (LeftCenteredPadding(context), RightCenteredPadding(context))
      }
    }
  }

  protected lazy val isDelimitedPrefixedPattern = {
    import LengthKind._
    lengthKind match {
      case Delimited => true // don't test for hasDelimiters because it might not be our delimiter, but a surrounding group's separator, or it's terminator, etc.
      case Pattern => true
      case Prefixed => true
      case _ => false
    }
  }

  /**
   * true if padding will be inserted for this delimited element when unparsing.
   */
  protected lazy val isDelimitedPrefixedPatternWithPadding = {
    (isDelimitedPrefixedPattern &&
      (impliedRepresentation eq Representation.Text) &&
      (justificationPad ne TextJustificationType.None) &&
      minLen > 0)
  }

  final lazy val prefixedLengthElementDecl: PrefixLengthQuasiElementDecl = {
    Assert.invariant(lengthKind == LengthKind.Prefixed)

    // We need to resolve the global simple type of the prefix length type
    // because we need to create a detached element with the same schema
    // document/parent of the GSTD.
    val prefixLengthTypeGSTD = schemaSet.getGlobalSimpleTypeDef(prefixLengthType).getOrElse(
      schemaDefinitionError(
        "Failed to resolve dfdl:prefixLengthType=\"%s\" to a simpleType",
        prefixLengthType.toQNameString))

    val detachedNode =
      <element name={ name + " (prefixLength)" } type={ prefixLengthType.toQNameString }/>
        .copy(scope = prefixLengthTypeGSTD.xml.scope)
    val detachedElementDecl =
      new PrefixLengthQuasiElementDecl(detachedNode, prefixLengthTypeGSTD)

    val prefixedLengthKind = detachedElementDecl.lengthKind
    prefixedLengthKind match {
      case LengthKind.Delimited | LengthKind.EndOfParent | LengthKind.Pattern =>
        schemaDefinitionError(
          "%s is specified as a dfdl:prefixLengthType, but has a dfdl:lengthKind of %s",
          prefixLengthType,
          prefixedLengthKind)
      case LengthKind.Explicit if detachedElementDecl.optLengthConstant.isEmpty =>
        schemaDefinitionError(
          "%s is specified as a dfdl:prefixLengthType, but has an expression for dfdl:length",
          prefixLengthType)
      case LengthKind.Implicit | LengthKind.Explicit if prefixIncludesPrefixLength == YesNo.Yes &&
        lengthUnits != detachedElementDecl.lengthUnits =>
        schemaDefinitionError(
          "%s is specified as a dfdl:prefixLengthType where dfdl:prefixIncludesPrefixLength=\"yes\" " +
            "with dfdl:lengthKind %s, but has different dfdl:lengthUnits than the element",
          prefixLengthType,
          prefixedLengthKind)
      case _ =>
    }

    schemaDefinitionUnless(
      detachedElementDecl.primType.isSubtypeOf(NodeInfo.Integer),
      "%s is specified as a dfdl:prefixLengthType, but its type xs:%s is not a subtype of xs:integer",
      prefixLengthType,
      detachedElementDecl.primType.toString.toLowerCase)

    schemaDefinitionWhen(
      detachedElementDecl.isOutputValueCalc,
      "%s is specified as a dfdl:prefixLengthType, but specifies dfdl:outputValueCalc",
      prefixLengthType)
    schemaDefinitionWhen(
      detachedElementDecl.hasInitiator,
      "%s is specified as a dfdl:prefixLengthType, but specifies a dfdl:initiator",
      prefixLengthType)
    schemaDefinitionWhen(
      detachedElementDecl.hasTerminator,
      "%s is specified as a dfdl:prefixLengthType, but specifies a dfdl:terminator",
      prefixLengthType)
    schemaDefinitionWhen(
      detachedElementDecl.alignment != 1,
      "%s is specified as a dfdl:prefixLengthType, but specifies a dfdl:alignment other than 1",
      prefixLengthType)
    schemaDefinitionWhen(
      detachedElementDecl.leadingSkip != 0,
      "%s is specified as a dfdl:prefixLengthType, but specifies a dfdl:leadingSkip other than 0",
      prefixLengthType)
    schemaDefinitionWhen(
      detachedElementDecl.trailingSkip != 0,
      "%s is specified as a dfdl:prefixLengthType, but specifies a dfdl:trailingSkip other than 0",
      prefixLengthType)

    if (detachedElementDecl.lengthKind == LengthKind.Prefixed &&
      detachedElementDecl.prefixedLengthElementDecl.lengthKind == LengthKind.Prefixed) {
      schemaDefinitionError(
        "Nesting level for dfdl:prefixLengthType exceeds 1: %s > %s > %s > %s",
        name,
        prefixLengthType,
        detachedElementDecl.prefixLengthType,
        detachedElementDecl.prefixedLengthElementDecl.prefixLengthType)
    }

    subset(
      detachedElementDecl.lengthKind != LengthKind.Prefixed,
      "Nested dfdl:lengthKind=\"prefixed\" is not supported.")

    detachedElementDecl
  }
  final lazy val prefixedLengthAdjustmentInUnits: Long = prefixIncludesPrefixLength match {
    case YesNo.Yes => {
      // get the known length of the prefix element in lengthUnits
      val pl = prefixedLengthElementDecl
      pl.lengthKind match {
        case LengthKind.Explicit => pl.lengthEv.optConstant.get // already in lengthUnits when explicit
        case LengthKind.Implicit =>
          Assert.invariant(impliedRepresentation == Representation.Binary)
          val len = pl.lengthEv.optConstant.get // always in bits when implicit
          pl.lengthUnits match {
            case LengthUnits.Bits => len
            case LengthUnits.Bytes => len / 8
            case LengthUnits.Characters => Assert.impossible()
          }
        case _ => Assert.impossible()
      }
    }
    case YesNo.No => 0
  }
  final lazy val prefixedLengthBody = prefixedLengthElementDecl.parsedValue

  /**
   * Quite tricky when we add padding or fill
   *
   * For complex types, there has to be a length defined. That's it.
   *
   * For simple types, we need the fill region processors
   * to detect excess length
   */
  final lazy val shouldAddPadding = {
    val res =
      if (isComplexType &&
        (this.maybeLengthEv.isDefined) &&
        (lengthUnits eq LengthUnits.Characters))
        maybeUnparseTargetLengthInBitsEv.isDefined // "pad" complex types.
      else if (isDelimitedPrefixedPatternWithPadding)
        true // simple type for unparse that needs to be padded.
      else
        // simple type, specified length.
        (maybeUnparseTargetLengthInBitsEv.isDefined &&
          (justificationPad ne TextJustificationType.None)) // None if not text.
    //    if (res)
    //      println("%s should add padding.".format(this.diagnosticDebugName))
    //    else
    //      println("%s NOT adding padding.".format(this.diagnosticDebugName))
    res
  }

  /**
   * We add fill to complex types of specified length so long as length units
   * are bytes/bits. If characters then "pad" puts the characters on.
   *
   * We also add it to text elements of variable specified length again, unless
   * length units are in characters.
   */
  final lazy val shouldAddFill = {
    val res =
      (isComplexType &&
        isSpecifiedLengthForUnparsing &&
        (lengthUnits ne LengthUnits.Characters)) ||
        (isSimpleType &&
          isSpecifiedLengthForUnparsing &&
          couldBeVariableLengthInfoset &&
          (lengthUnits ne LengthUnits.Characters))
    res
  }

  //  /**
  //   * Means the representation could have varying sizes.
  //   *
  //   * Does not mean the infoset element might be bigger or smaller and
  //   * might get truncated or padded to fit a fixed size representation.
  //   *
  //   * This is about whether the box we're fitting it into is fixed or varying size.
  //   */
  //  final protected lazy val isVariableLengthRep: Boolean = {
  //    isDelimitedPrefixedPattern ||
  //      ((lengthKind eq LengthKind.Explicit) &&
  //        !lengthEv.optConstant.isDefined)
  //        //
  //        // TODO: do we have to consider utf-8 situation for strings when
  //        // even though the length is constant, because it's utf-8
  //        // the length units are characters but we don't know
  //        // how many bytes without knowing what characters are?
  //        //
  //        // This requires an Evaluatable, since it depends on charset.
  //        //
  //        // Also, whether we want this to be true/false depends on
  //        // context. If the context is a complex type with lengthUnits characters
  //        // then we're ok with utf-8 fixed number of characters. We're not
  //        // ok if the complex type has length its bytes, but contains an
  //        // element with length units bytes and charset utf-8.
  //        //
  //  }

  /**
   * Means the specified length must, necessarily, be big enough to hold the representation
   * so long as the value in the infoset is legal for the type.
   *
   * This does not include numeric range checking. So for example if you
   * have an xs:unsignedInt but length is 3 bits, this will be true even though an
   * integer value of greater than 7 cannot fit.
   *
   * Another way to think of this is that legal infoset values will have fixed
   * length representations.
   *
   * This is a conservative analysis, meaning if true the property definitely
   * holds, but if false it may mean we just couldn't tell if it holds or not.
   *
   * If this is true, then we never need to check how many bits were written when
   * unparsing, because we know a legal value has to fit. If the value is illegal
   * then we'll get an unparse error anyway.
   *
   * If this is false, then it's possible that the value, even a legal value,
   * might not fit if the length is specified. We're unable to prove that all
   * legal values WILL fit.
   *
   * A critical case is that fixed length binary integers should always
   * return true here so that we're not doing excess length checks on them
   * Or computing their value length unnecessarily.
   */
  final protected lazy val mustBeFixedLengthInfoset = {
    (isSimpleType &&
      (impliedRepresentation eq Representation.Binary) &&
      (primType ne PrimType.HexBinary) &&
      {
        import NodeInfo._
        primType match {
          case Float => true
          case Double => true;
          case n: Numeric.Kind =>
            binaryNumberRep match {
              case BinaryNumberRep.Binary => true
              case _ => false
            }
          case _ => true
        }
      } &&
      ((lengthKind eq LengthKind.Implicit) ||
        ((lengthKind eq LengthKind.Explicit) &&
          lengthEv.optConstant.isDefined)) // TODO: consider if delimiters matter, alignment, nil values,.... Are we excluding
    // that stuff here? For example, if the element is nillable, but
    // if isNilled, the representation will still be exactly the same
    // length, then we'd like this to be true.
    ) ||
          (
            // a fixed-length textual number, where a legal value
            // (or nil value) must by definition be smaller than the
            // fixed length. E.g., since the largest unsignedByte value
            // is 256, and with textNumberPattern="###" the max width is 3
            // then so long as the fixed length is 3 or greater we know
            // it's going to fit.
            //
            // Furthermore, if it is nillable, and the nilValue is "nil", which
            // is also length 3, then it's going to fit.
            //
            // Also matters that if the textNumberPattern can output fewer
            // characters than the fixed width, then padding must be enabled and a
            // pad char specified, and that the padChar is the same width as
            // a digit. (similarly if the nilValue was "-" we need to pad that too)
            //
            // This also assumes that in every charset encoding the digit
            // characters are always the same width so the whole utf-8
            // variable-width encoding crud doesn't apply.
            //
            // However, textNumberPattern
            // matters, because sometimes the lengths of positive and negative
            // numbers can differ. So the fixed length has to be big enough
            // for the largest possible textNumber matching the pattern.
            //
            false // TODO: implement this
          ) ||
            (isComplexType && !isNillable &&
              (
                //
                // recursively, the complex type can contain only isFixedLengthInfoset items
                // but in addition, no delimiter can be varying width - so there can only be one
                // delimiter value for each of these, nor can there be any variable-width
                // alignment regions.
                //
                false // TODO: implement this
              ))
  }

  /**
   * Means the infoset element could have varying length.
   *
   * And that means if there is a specified length box to fit it into
   * that we have to check if it is too big/small for the box.
   *
   * So that means hexBinary, or representation text (for simple types)
   * or any complex type unless everything in it is fixedLengthInfoset.
   *
   * So for example, a complex type containing only fixed length binary
   * integers is itself fixed length.
   */
  final protected lazy val couldBeVariableLengthInfoset: Boolean = {
    !mustBeFixedLengthInfoset
  }

  /**
   * Only strings can be truncated, only if they are specified length,
   * and only if truncateSpecifiedLengthString is 'yes'.
   *
   * Note that specified length might mean fixed length or variable (but specified)
   * length.
   */
  final protected lazy val isTruncatable = {
    isSimpleType &&
      (primType eq PrimType.String) &&
      (truncateSpecifiedLengthString eq YesNo.Yes) &&
      isSpecifiedLengthForUnparsing
  }

  /**
   * Fixed length, or variable length with explicit length expression.
   */
  final protected lazy val isSpecifiedLengthForUnparsing: Boolean = {
    (isSimpleType && (
      (lengthKind eq LengthKind.Explicit) ||
      (lengthKind eq LengthKind.Implicit))) ||
      (isComplexType &&
        (lengthKind eq LengthKind.Explicit))
  }

  /**
   * Check for excess length if it's variable length and we cannot truncate it.
   */
  final lazy val shouldCheckExcessLength = {
    val res =
      couldBeVariableLengthInfoset &&
        !isTruncatable &&
        isSpecifiedLengthForUnparsing
    res
  }

  private lazy val rightFill = new RightFill(context)

  private lazy val elementUnused = new ElementUnused(context)

  private lazy val parsedNil = prod("parsedNil", NYI && isNillable && nilKind == NilKind.LogicalValue) {
    nilElementInitiator ~
      captureLengthRegions(leftPadding, LogicalNilValue(this), rightPadding ~ rightFill) ~
      nilElementTerminator
  }

  private def captureLengthRegions(leftPaddingArg: => Gram, bodyArg: => Gram, rightPadFillArg: => Gram) = {
    lazy val leftPadding = leftPaddingArg
    lazy val rightPadFill = rightPadFillArg
    lazy val body = bodyArg
    CaptureContentLengthStart(this) ~
      leftPadding ~
      CaptureValueLengthStart(this) ~
      body ~
      CaptureValueLengthEnd(this) ~
      rightPadFill ~
      CaptureContentLengthEnd(this)
  }

  lazy val parsedValue = prod("parsedValue", isSimpleType) {
    initiatorRegion ~
      valueMTA ~
      sharedSimpleParsedValue
  }

  /**
   * We share the schema compilation objects for the sharable part of simple values
   * which is the part inside the framing.
   *
   * This eliminates redundant computation when element references reuse elements.
   *
   * It is critical that the 2nd argument to getShared is passed by name, so not
   * evaluated a second time when sharing opportunities are discovered (same shareKey).
   */
  lazy val sharedSimpleParsedValue =
    schemaSet.sharedSimpleValueFactory.getShared(
      shareKey,
      captureLengthRegions(leftPadding, retrySimpleType(value), rightPadding ~ rightFill) ~
        terminatorRegion)

  /**
   * Wrapped around the simple value unparsers where the simple type value is
   * dynamic (e.g. prefixed length, dfdl:outputValueCalc). The correct parser
   * will be inserted to retry until the simple type value is available.
   *
   * Evaporates for parsing, and when a dynamic simple type can't occur.
   *
   * Note: must be wrapped around the unparsed value only, not the framing.
   * Because some framing is alignment regions, which are themselves possibly
   * blocking/retrying. Setup of these must happen on the first pass, we cannot
   * do setups of ustate/data-output-streams when unparsing the result of an OVC.
   */
  private def retrySimpleType(allowedValueArg: => Gram) = {
    lazy val allowedValue = allowedValueArg
    if (this.isOutputValueCalc)
      SimpleTypeRetry(this, allowedValue)
    else if (this.isInstanceOf[PrefixLengthQuasiElementDecl]) {
      //
      // If an element has a prefixed length, it
      // means that the prefix length value will be calculated using a
      // specified length unparser. This unparser works by suspending the
      // prefix length unaprser, eventually calculating the length of the
      // unparsed data, then setting the length to the value of the detached
      // element. Since the prefixed length unparser will need to suspend until
      // its data value is dynamically set, it needs this SimpleTypeRetry.
      //
      SimpleTypeRetry(this, allowedValue)
    } else
      allowedValue
  }

  // Length is in bits, (size would be in bytes) (from DFDL Spec 12.3.3)
  final protected lazy val implicitBinaryLengthInBits: Long = primType match {
    case PrimType.Byte | PrimType.UnsignedByte => 8
    case PrimType.Short | PrimType.UnsignedShort => 16
    case PrimType.Float | PrimType.Int | PrimType.UnsignedInt | PrimType.Boolean => 32
    case PrimType.Double | PrimType.Long | PrimType.UnsignedLong => 64
    case PrimType.DateTime => binaryCalendarRep match {
      case BinaryCalendarRep.BinarySeconds => 32
      case BinaryCalendarRep.BinaryMilliseconds => 64
      case _ => schemaDefinitionError("Length of binary data '" + primType.name + "' with binaryCalendarRep='" + binaryCalendarRep + "' cannot be determined implicitly.")
    }
    case _ => schemaDefinitionError("Length of binary data '" + primType.name + "' cannot be determined implicitly.")
  }

  /**
   * Property consistency check for called for all binary numbers
   *
   * Returns -1 if the binary number is not of known length,or is of a
   * lengthKind inconsistent with knowing the length.
   *
   * SDE if the lengthKind is inconsistent with binary numbers, not yet implemented
   * for binary numbers, or not supported by Daffodil.
   */
  protected lazy val binaryNumberKnownLengthInBits: Long = lengthKind match {
    case _ if optRepTypeElement.isDefined => optRepTypeElement.get.binaryNumberKnownLengthInBits
    case LengthKind.Implicit => implicitBinaryLengthInBits
    case LengthKind.Explicit if (lengthEv.isConstant) => explicitBinaryLengthInBits()
    case LengthKind.Explicit => -1 // means must be computed at runtime.
    case LengthKind.Prefixed => -1 // means must be computed at runtime
    case LengthKind.Delimited => primType match {
      case PrimType.DateTime | PrimType.Date | PrimType.Time =>
        if (binaryCalendarRep == BinaryCalendarRep.BinaryMilliseconds || binaryCalendarRep == BinaryCalendarRep.BinarySeconds)
          SDE("lengthKind='delimited' only supported for packed binary formats.")
        else -1 // only for packed binary data, length must be computed at runtime.
      case _ => if (binaryNumberRep == BinaryNumberRep.Binary) SDE("lengthKind='delimited' only supported for packed binary formats.")
      else -1 // only for packed binary data, length must be computed at runtime.
    }
    case LengthKind.Pattern => schemaDefinitionError("Binary data elements cannot have lengthKind='pattern'.")
    case LengthKind.EndOfParent => schemaDefinitionError("Binary data elements cannot have lengthKind='endOfParent'.")
  }

  private def explicitBinaryLengthInBits() = {
    val lengthFromProp: JLong = lengthEv.optConstant.get
    val nbits = lengthUnits match {
      case LengthUnits.Bits => lengthFromProp.longValue()
      case LengthUnits.Bytes => lengthFromProp.longValue() * 8
      case LengthUnits.Characters => SDE("The lengthUnits for the binary type %s must be either 'bits' or 'bytes'. Not 'characters'.", primType.name)
    }
    nbits
  }

  private lazy val specifiedLengthHexBinary = prod("specifiedLengthHexBinary") {
    lengthUnits match {
      case LengthUnits.Bytes => HexBinarySpecifiedLength(this)
      case LengthUnits.Bits => HexBinarySpecifiedLength(this)
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    }
  }

  private lazy val stringDelimitedEndOfData = prod("stringDelimitedEndOfData") { StringDelimitedEndOfData(this) }

  private lazy val stringValue = prod("stringValue") { stringPrim }

  private lazy val stringPrim = {
    lengthKind match {
      case LengthKind.Explicit => specifiedLength(StringOfSpecifiedLength(this))
      case LengthKind.Prefixed => specifiedLength(StringOfSpecifiedLength(this))
      case LengthKind.Delimited => stringDelimitedEndOfData
      case LengthKind.Pattern => specifiedLength(StringOfSpecifiedLength(this))
      case LengthKind.Implicit => {
        val pt = this.simpleType.primType
        Assert.invariant(pt == PrimType.String)
        specifiedLength(StringOfSpecifiedLength(this))
      }
      case LengthKind.EndOfParent if isComplexType => notYetImplemented("lengthKind='endOfParent' for complex type")
      case LengthKind.EndOfParent => notYetImplemented("lengthKind='endOfParent' for simple type")
      case _ => SDE("Unimplemented lengthKind %s", lengthKind)
    }
  }

  private lazy val hexBinaryDelimitedEndOfData = prod("hexBinaryDelimitedEndOfData") { HexBinaryDelimitedEndOfData(this) }

  private lazy val hexBinaryLengthPattern = prod("hexBinaryLengthPattern") { new SpecifiedLengthPattern(this, new HexBinaryEndOfBitLimit(this)) }

  private lazy val hexBinaryLengthPrefixed = prod("hexBinaryLengthPrefixed") { new HexBinaryLengthPrefixed(this) }

  private lazy val hexBinaryValue = prod("hexBinaryValue") {
    schemaDefinitionWhen(lengthUnits == LengthUnits.Characters, "Hex binary Numbers must have dfdl:lengthUnits of \"bits\" or \"bytes\".")
    lengthKind match {
      case LengthKind.Explicit => specifiedLengthHexBinary
      case LengthKind.Implicit => specifiedLengthHexBinary
      case LengthKind.Delimited => hexBinaryDelimitedEndOfData
      case LengthKind.Pattern => hexBinaryLengthPattern
      case LengthKind.Prefixed => hexBinaryLengthPrefixed
      case LengthKind.EndOfParent if isComplexType => notYetImplemented("lengthKind='endOfParent' for complex type")
      case LengthKind.EndOfParent => notYetImplemented("lengthKind='endOfParent' for simple type")
      case _ => SDE("Unimplemented lengthKind %s", lengthKind)
    }
  }

  private lazy val specifiedLengthBlob = prod("specifiedLengthBlob") {
    lengthUnits match {
      case LengthUnits.Bytes => BlobSpecifiedLength(this)
      case LengthUnits.Bits => BlobSpecifiedLength(this)
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for blob data.")
    }
  }

  private lazy val blobValue = prod("blobValue") {
    lengthKind match {
      case LengthKind.Explicit => specifiedLengthBlob
      case _ => SDE("objectKind='bytes' must have dfdl:lengthKind='explicit'")
    }
  }

  private lazy val clobValue = prod("clobValue") {
    notYetImplemented("objectKind='chars'")
  }

  private lazy val textNumber = textStandardNumber || textZonedNumber

  private lazy val textNonNumber = {
    ConvertTextCombinator(this, stringValue, textConverter)
  }

  private lazy val textStandardNumber = prod("textStandardNumber", textNumberRep == TextNumberRep.Standard) {
    val converter = textStandardBaseDefaulted match {
      case 10 => textConverter
      case 2 | 8 | 16 => textStandardNonBaseTenConverter
      case _ => Assert.impossible()
    }
    ConvertTextCombinator(this, stringValue, converter)
  }

  private lazy val textZonedNumber = prod("textZonedNumber", textNumberRep == TextNumberRep.Zoned) {
    ConvertZonedCombinator(this, stringValue, textZonedConverter)
  }

  private lazy val textConverter = {
    primType match {
      case _: NodeInfo.Numeric.Kind => ConvertTextNumberPrim(this)
      case PrimType.Boolean => ConvertTextBooleanPrim(this)
      case PrimType.Date => ConvertTextDatePrim(this)
      case PrimType.Time => ConvertTextTimePrim(this)
      case PrimType.DateTime => ConvertTextDateTimePrim(this)

      case PrimType.HexBinary | PrimType.String | PrimType.AnyURI =>
        Assert.invariantFailed("textConverter not to be used for binary or string types")
    }
  }

  private lazy val textStandardNonBaseTenConverter = {
    primType match {
      case _: NodeInfo.Integer.Kind => ConvertNonBaseTenTextNumberPrim(this)
      case _ => SDE("dfdl:textStandardBase=\"%s\" cannot be used with %s", textStandardBaseDefaulted, primType.globalQName)
    }
  }

  private lazy val textZonedConverter = {
    primType match {
      case _: NodeInfo.Decimal.Kind => ConvertZonedNumberPrim(this)
      case _ => SDE("dfdl:textNumberRep=\"zoned\" cannot be used with %s", primType.globalQName)
    }
  }

  private lazy val bcdKnownLengthCalendar = prod("bcdKnownLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Bcd) {
    ConvertZonedCombinator(this, new BCDIntegerKnownLength(this, binaryNumberKnownLengthInBits), textConverter)
  }
  private lazy val bcdRuntimeLengthCalendar = prod("bcdRuntimeLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Bcd) {
    ConvertZonedCombinator(this, new BCDIntegerRuntimeLength(this), textConverter)
  }
  private lazy val bcdDelimitedLengthCalendar = prod("bcdDelimitedLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Bcd) {
    ConvertZonedCombinator(this, new BCDIntegerDelimitedEndOfData(this), textConverter)
  }
  private lazy val bcdPrefixedLengthCalendar = prod("bcdPrefixedLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Bcd) {
    ConvertZonedCombinator(this, new BCDIntegerPrefixedLength(this), textConverter)
  }

  private lazy val ibm4690PackedKnownLengthCalendar = prod("ibm4690PackedKnownLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Ibm4690Packed) {
    ConvertZonedCombinator(this, new IBM4690PackedIntegerKnownLength(this, false, binaryNumberKnownLengthInBits), textConverter)
  }
  private lazy val ibm4690PackedRuntimeLengthCalendar = prod("ibm4690PackedRuntimeLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Ibm4690Packed) {
    ConvertZonedCombinator(this, new IBM4690PackedIntegerRuntimeLength(this, false), textConverter)
  }
  private lazy val ibm4690PackedDelimitedLengthCalendar = prod("ibm4690PackedDelimitedLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Ibm4690Packed) {
    ConvertZonedCombinator(this, new IBM4690PackedIntegerDelimitedEndOfData(this, false), textConverter)
  }
  private lazy val ibm4690PackedPrefixedLengthCalendar = prod("ibm4690PackedPrefixedLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Ibm4690Packed) {
    ConvertZonedCombinator(this, new IBM4690PackedIntegerPrefixedLength(this, false), textConverter)
  }

  private lazy val packedKnownLengthCalendar = prod("packedKnownLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Packed) {
    ConvertZonedCombinator(this, new PackedIntegerKnownLength(this, false, packedSignCodes, binaryNumberKnownLengthInBits), textConverter)
  }
  private lazy val packedRuntimeLengthCalendar = prod("packedRuntimeLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Packed) {
    ConvertZonedCombinator(this, new PackedIntegerRuntimeLength(this, false, packedSignCodes), textConverter)
  }
  private lazy val packedDelimitedLengthCalendar = prod("packedDelimitedLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Packed) {
    ConvertZonedCombinator(this, new PackedIntegerDelimitedEndOfData(this, false, packedSignCodes), textConverter)
  }
  private lazy val packedPrefixedLengthCalendar = prod("packedPrefixedLengthCalendar", binaryCalendarRep == BinaryCalendarRep.Packed) {
    ConvertZonedCombinator(this, new PackedIntegerPrefixedLength(this, false, packedSignCodes), textConverter)
  }

  def primType: PrimType

  protected final lazy val value = prod("value", isSimpleType) {
    // TODO: Consider issues with matching a stopValue. Can't say isScalar here because
    // this gets used for array contents also.
    {
      primType match {
        case PrimType.String => stringValue
        case PrimType.HexBinary => hexBinaryValue
        case PrimType.AnyURI => {
          val res = impliedRepresentation match {
            case Representation.Binary => blobValue
            case Representation.Text => clobValue
          }
          res
        }
        case _ => {
          val res = impliedRepresentation match {
            case Representation.Binary => binaryValue
            case Representation.Text => textValue
          }
          res
        }
      }
    }
  }

  private lazy val staticBinaryFloatRep = {
    subset(binaryFloatRepEv.isConstant, "Dynamic binaryFloatRep is not supported.")
    binaryFloatRepEv.optConstant.get
  }

  val ieee = BinaryFloatRep.Ieee
  type BO = java.nio.ByteOrder

  private lazy val packedSignCodes = PackedSignCodes(binaryPackedSignCodes, binaryNumberCheckPolicy)

  private def binaryIntegerValue(isSigned: Boolean) = {
    //
    // Is it a single byte or smaller
    //
    if ((primType != PrimType.Byte) &&
      (binaryNumberKnownLengthInBits == -1 ||
        binaryNumberKnownLengthInBits > 8)) {
      byteOrderRaw // must be defined or SDE
    }
    (binaryNumberRep, lengthKind, binaryNumberKnownLengthInBits) match {
      case (BinaryNumberRep.Binary, LengthKind.Prefixed, _) => new BinaryIntegerPrefixedLength(this, isSigned)
      case (BinaryNumberRep.Binary, _, -1) => new BinaryIntegerRuntimeLength(this, isSigned)
      case (BinaryNumberRep.Binary, _, _) => new BinaryIntegerKnownLength(this, isSigned, binaryNumberKnownLengthInBits)
      case (_, LengthKind.Implicit, _) => SDE("lengthKind='implicit' is not allowed with packed binary formats")
      case (_, _, _) if ((binaryNumberKnownLengthInBits != -1) && (binaryNumberKnownLengthInBits % 4) != 0) =>
        SDE("The given length (%s bits) must be a multiple of 4 when using packed binary formats", binaryNumberKnownLengthInBits)
      case (BinaryNumberRep.Packed, LengthKind.Delimited, -1) => new PackedIntegerDelimitedEndOfData(this, isSigned, packedSignCodes)
      case (BinaryNumberRep.Packed, LengthKind.Prefixed, -1) => new PackedIntegerPrefixedLength(this, isSigned, packedSignCodes)
      case (BinaryNumberRep.Packed, _, -1) => new PackedIntegerRuntimeLength(this, isSigned, packedSignCodes)
      case (BinaryNumberRep.Packed, _, _) => new PackedIntegerKnownLength(this, isSigned, packedSignCodes, binaryNumberKnownLengthInBits)
      case (BinaryNumberRep.Ibm4690Packed, LengthKind.Delimited, -1) => new IBM4690PackedIntegerDelimitedEndOfData(this, isSigned)
      case (BinaryNumberRep.Ibm4690Packed, LengthKind.Prefixed, -1) => new IBM4690PackedIntegerPrefixedLength(this, isSigned)
      case (BinaryNumberRep.Ibm4690Packed, _, -1) => new IBM4690PackedIntegerRuntimeLength(this, isSigned)
      case (BinaryNumberRep.Ibm4690Packed, _, _) => new IBM4690PackedIntegerKnownLength(this, isSigned, binaryNumberKnownLengthInBits)
      case (BinaryNumberRep.Bcd, _, _) => primType match {
        case PrimType.Long | PrimType.Int | PrimType.Short | PrimType.Byte => SDE("%s is not an allowed type for bcd binary values", primType.name)
        case _ => (lengthKind, binaryNumberKnownLengthInBits) match {
          case (LengthKind.Delimited, -1) => new BCDIntegerDelimitedEndOfData(this)
          case (LengthKind.Prefixed, -1) => new BCDIntegerPrefixedLength(this)
          case (_, -1) => new BCDIntegerRuntimeLength(this)
          case (_, _) => new BCDIntegerKnownLength(this, binaryNumberKnownLengthInBits)
        }
      }
    }
  }

  private lazy val binaryValue: Gram = {
    Assert.invariant(primType != PrimType.String)

    schemaDefinitionWhen(lengthUnits == LengthUnits.Characters, "Binary Numbers must have dfdl:lengthUnits of \"bits\" or \"bytes\".")

    // We have to dispatch carefully here. We cannot force evaluation of properties
    // that may not be necessary. E.g., float does not need property binaryNumberRep, so
    // if our dispatch table uses that, it will create a false dependency on the property
    // being defined.
    // The DFDL spec has a section where it gives the precedence order of properties.
    // This is in the spirit of that section.
    val res: Gram = primType match {

      case PrimType.Byte | PrimType.Short | PrimType.Int | PrimType.Long | PrimType.Integer => {
        binaryIntegerValue(true)
      }

      case PrimType.UnsignedByte | PrimType.UnsignedShort | PrimType.UnsignedInt | PrimType.UnsignedLong | PrimType.NonNegativeInteger => {
        binaryIntegerValue(false)
      }

      case PrimType.Double | PrimType.Float => {
        byteOrderRaw // is required. SDE if not defined
        (primType, binaryNumberKnownLengthInBits, staticBinaryFloatRep) match {
          case (_, -1, BinaryFloatRep.Ieee) => SDE("Floating point binary numbers may not have runtime-specified lengths.")
          case (PrimType.Float, 32, BinaryFloatRep.Ieee) => new BinaryFloat(this)
          case (PrimType.Float, n, BinaryFloatRep.Ieee) => SDE("binary xs:float must be 32 bits. Length in bits was %s.", n)
          case (PrimType.Double, 64, BinaryFloatRep.Ieee) => new BinaryDouble(this)
          case (PrimType.Double, n, BinaryFloatRep.Ieee) => SDE("binary xs:double must be 64 bits. Length in bits was %s.", n)
          case (_, _, floatRep) => subsetError("binaryFloatRep='%s' not supported. Only binaryFloatRep='ieee'", floatRep.toString)
        }
      }

      case PrimType.Decimal => {
        if (binaryDecimalVirtualPoint > tunable.maxBinaryDecimalVirtualPoint)
          throw new TunableLimitExceededError(schemaFileLocation, "Property binaryDecimalVirtualPoint %s is greater than limit %s", binaryDecimalVirtualPoint, tunable.maxBinaryDecimalVirtualPoint)
        if (binaryDecimalVirtualPoint < tunable.minBinaryDecimalVirtualPoint)
          throw new TunableLimitExceededError(schemaFileLocation, "Property binaryDecimalVirtualPoint %s is less than limit %s", binaryDecimalVirtualPoint, tunable.minBinaryDecimalVirtualPoint)
        if (binaryNumberKnownLengthInBits == -1 ||
          binaryNumberKnownLengthInBits > 8) byteOrderRaw // must have or SDE

        (binaryNumberRep, lengthKind, binaryNumberKnownLengthInBits) match {
          case (BinaryNumberRep.Binary, LengthKind.Prefixed, _) => new BinaryDecimalPrefixedLength(this)
          case (BinaryNumberRep.Binary, _, -1) => new BinaryDecimalRuntimeLength(this)
          case (BinaryNumberRep.Binary, _, _) => new BinaryDecimalKnownLength(this, binaryNumberKnownLengthInBits)
          case (_, LengthKind.Implicit, _) => SDE("lengthKind='implicit' is not allowed with packed binary formats")
          case (_, _, _) if ((binaryNumberKnownLengthInBits != -1) && (binaryNumberKnownLengthInBits % 4) != 0) =>
            SDE("The given length (%s bits) must be a multiple of 4 when using packed binary formats", binaryNumberKnownLengthInBits)
          case (BinaryNumberRep.Packed, LengthKind.Delimited, -1) => new PackedDecimalDelimitedEndOfData(this, packedSignCodes)
          case (BinaryNumberRep.Packed, LengthKind.Prefixed, _) => new PackedDecimalPrefixedLength(this, packedSignCodes)
          case (BinaryNumberRep.Packed, _, -1) => new PackedDecimalRuntimeLength(this, packedSignCodes)
          case (BinaryNumberRep.Packed, _, _) => new PackedDecimalKnownLength(this, packedSignCodes, binaryNumberKnownLengthInBits)
          case (BinaryNumberRep.Bcd, LengthKind.Delimited, -1) => new BCDDecimalDelimitedEndOfData(this)
          case (BinaryNumberRep.Bcd, LengthKind.Prefixed, _) => new BCDDecimalPrefixedLength(this)
          case (BinaryNumberRep.Bcd, _, -1) => new BCDDecimalRuntimeLength(this)
          case (BinaryNumberRep.Bcd, _, _) => new BCDDecimalKnownLength(this, binaryNumberKnownLengthInBits)
          case (BinaryNumberRep.Ibm4690Packed, LengthKind.Delimited, -1) => new IBM4690PackedDecimalDelimitedEndOfData(this)
          case (BinaryNumberRep.Ibm4690Packed, LengthKind.Prefixed, _) => new IBM4690PackedDecimalPrefixedLength(this)
          case (BinaryNumberRep.Ibm4690Packed, _, -1) => new IBM4690PackedDecimalRuntimeLength(this)
          case (BinaryNumberRep.Ibm4690Packed, _, _) => new IBM4690PackedDecimalKnownLength(this, binaryNumberKnownLengthInBits)
        }
      }

      case PrimType.Boolean => {
        lengthKind match {
          case LengthKind.Prefixed => new BinaryBooleanPrefixedLength(this)
          case _ => new BinaryBoolean(this)
        }
      }

      case PrimType.DateTime | PrimType.Date | PrimType.Time => {
        (primType, binaryCalendarRep) match {
          case (PrimType.DateTime, BinaryCalendarRep.BinarySeconds) => (lengthUnits, binaryNumberKnownLengthInBits) match {
            case (LengthUnits.Bytes, 32) => new ConvertBinaryDateTimeSecMilliPrim(this, binaryNumberKnownLengthInBits)
            case (_, 32) => SDE("lengthUnits must be 'bytes' when binaryCalendarRep='binarySeconds'")
            case (_, n) => SDE("binary xs:dateTime must be 32 bits when binaryCalendarRep='binarySeconds'. Length in bits was %s.", n)
          }
          case (_, BinaryCalendarRep.BinarySeconds) => SDE("binaryCalendarRep='binarySeconds' is not allowed with type %s", primType.name)
          case (PrimType.DateTime, BinaryCalendarRep.BinaryMilliseconds) => (lengthUnits, binaryNumberKnownLengthInBits) match {
            case (LengthUnits.Bytes, 64) => new ConvertBinaryDateTimeSecMilliPrim(this, binaryNumberKnownLengthInBits)
            case (_, 64) => SDE("lengthUnits must be 'bytes' when binaryCalendarRep='binaryMilliseconds'")
            case (_, n) => SDE("binary xs:dateTime must be 64 bits when binaryCalendarRep='binaryMilliseconds'. Length in bits was %s.", n)
          }
          case (_, BinaryCalendarRep.BinaryMilliseconds) => SDE("binaryCalendarRep='binaryMilliseconds' is not allowed with type %s", primType.name)
          case _ => { // Packed Decimal representations
            if ((binaryNumberKnownLengthInBits != -1) && (binaryNumberKnownLengthInBits % 4) != 0)
              SDE("The given length (%s bits) must be a multiple of 4 when using binaryCalendarRep='%s'.", binaryNumberKnownLengthInBits, binaryCalendarRep)
            if (calendarPatternKind != CalendarPatternKind.Explicit)
              SDE("calendarPatternKind must be 'explicit' when binaryCalendarRep='%s'", binaryCalendarRep)

            binaryCalendarRep match {
              case (BinaryCalendarRep.Bcd) => {
                (lengthKind, binaryNumberKnownLengthInBits) match {
                  case (LengthKind.Delimited, -1) => bcdDelimitedLengthCalendar
                  case (LengthKind.Prefixed, -1) => bcdPrefixedLengthCalendar
                  case (_, -1) => bcdRuntimeLengthCalendar
                  case (_, _) => bcdKnownLengthCalendar
                }
              }
              case (BinaryCalendarRep.Ibm4690Packed) => {
                (lengthKind, binaryNumberKnownLengthInBits) match {
                  case (LengthKind.Delimited, -1) => ibm4690PackedDelimitedLengthCalendar
                  case (LengthKind.Prefixed, -1) => ibm4690PackedPrefixedLengthCalendar
                  case (_, -1) => ibm4690PackedRuntimeLengthCalendar
                  case (_, _) => ibm4690PackedKnownLengthCalendar
                }
              }
              case (BinaryCalendarRep.Packed) => {
                (lengthKind, binaryNumberKnownLengthInBits) match {
                  case (LengthKind.Delimited, -1) => packedDelimitedLengthCalendar
                  case (LengthKind.Prefixed, -1) => packedPrefixedLengthCalendar
                  case (_, -1) => packedRuntimeLengthCalendar
                  case (_, _) => packedKnownLengthCalendar
                }
              }
              case _ => notYetImplemented("Type %s when representation='binary' and binaryCalendarRep=%s", primType.name, binaryCalendarRep.toString)
            }
          }
        }
      }

      case _ => notYetImplemented("Type %s when representation='binary'", primType.name)
    }
    res
  }

  private lazy val textValue: Gram = {
    val pt = primType
    Assert.invariant(pt != PrimType.String)
    Assert.invariant(pt != PrimType.HexBinary)
    Assert.invariant(pt != PrimType.AnyURI)
    Assert.invariant(impliedRepresentation == Representation.Text)
    schemaDefinitionWhen(
      lengthKind == LengthKind.Implicit,
      "Type %s cannot have lengthKind='implicit' when representation='text'",
      pt.name)

    val res = primType match {
      case _: NodeInfo.Numeric.Kind => textNumber
      case PrimType.Boolean => textNonNumber
      case PrimType.Date => textNonNumber
      case PrimType.Time => textNonNumber
      case PrimType.DateTime => textNonNumber

      case PrimType.HexBinary | PrimType.AnyURI | PrimType.String =>
        Assert.invariantFailed("types handled in 'value' grammer element")
    }
    res
  }

  protected final lazy val empty = prod("empty", NYI && isEmptyAnObservableConcept) { EmptyGram }

  private lazy val nilElementInitiator = prod("nilElementInitiator", hasNilValueInitiator) { delimMTA ~ Initiator(this) }
  private lazy val nilElementTerminator = prod("nilElementTerminator", hasNilValueTerminator) { delimMTA ~ Terminator(this) }

  private lazy val complexContent = complexType.group.termContentBody

  private lazy val isNilLit = isNillable && ((nilKind == NilKind.LiteralValue) || (nilKind == NilKind.LiteralCharacter))

  /**
   * In the below, we must have nilLitMTA because, in the case where it's textual,
   * then to distinguish a lit nil from a value, we have to start at the same place.
   */
  private lazy val nilLit = prod("nilLit", isNilLit) {
    nilElementInitiator ~
      nilLitMTA ~
      sharedNilLit
  }

  /**
   * We share the schema compilation objects for the sharable part of nil literals
   * which is the part inside the framing.
   *
   * This eliminates redundant computation when element references reuse elements.
   *
   * It is critical that the 2nd argument to getShared is passed by name, so not
   * evaluated a second time when sharing opportunities are discovered (same shareKey).
   */
  private lazy val sharedNilLit: Gram =
    schemaSet.sharedNilLitFactory.getShared(
      shareKey,
      nilLitSimpleOrComplex ~
        nilElementTerminator)

  private lazy val nilLitSimpleOrComplex = prod("nilLitSimpleOrComplex") { nilLitSimple || nilLitComplex }

  private lazy val nilLitSimple = prod("nilLitSimple", isSimpleType) {
    captureLengthRegions(
      leftPadding,
      specifiedLength(nilLitContent),
      rightPadding ~ rightFill)
  }

  private lazy val nilLitComplex = prod("nilLitComplex", isComplexType) {
    // Note: the only allowed nil value for a complex type is ES. It's length will be zero always. (as of DFDL v1.0 - 2015-07-15)
    schemaDefinitionUnless(this.hasESNilValue && cookedNilValuesForParse.length == 1, "Nillable complex type elements can only have '%ES;' as their dfdl:nilValue property.")
    captureLengthRegions(
      EmptyGram,
      nilLitContent,
      //
      // Because nil complex can only be ES (e.g., length 0), there's no possible
      // ElementUnused region after a nil.
      EmptyGram)

  }

  /**
   * mandatory text alignment for a literal nil value is only needed
   * if there is no initiator that gets its own mandatory text alignment.
   */
  private lazy val nilLitMTA = prod("nilLitMTA", isNilLit && !hasNilValueInitiator) { mtaBase }

  private lazy val nilLitContent = prod(
    "nilLitContent",
    isNillable && (nilKind == NilKind.LiteralValue || nilKind == NilKind.LiteralCharacter)) {

      nilKind match {
        case NilKind.LiteralValue => {
          lengthKind match {
            case LengthKind.Delimited => LiteralNilDelimitedEndOfData(this)
            case LengthKind.Pattern => LiteralValueNilOfSpecifiedLength(this)
            case LengthKind.Explicit => LiteralValueNilOfSpecifiedLength(this)
            case LengthKind.Implicit if isSimpleType => {
              schemaDefinitionUnless(impliedRepresentation != Representation.Text, "LiteralValue Nils with lengthKind='implicit' cannot have representation='text'.")
              LiteralValueNilOfSpecifiedLength(this)
            }
            case LengthKind.Implicit if isComplexType =>
              LiteralValueNilOfSpecifiedLength(this)
            case LengthKind.Prefixed => LiteralValueNilOfSpecifiedLength(this)
            case LengthKind.EndOfParent if isComplexType => notYetImplemented("lengthKind='endOfParent' for complex type")
            case LengthKind.EndOfParent => notYetImplemented("lengthKind='endOfParent' for simple type")
          }
        }
        case NilKind.LiteralCharacter => {
          if (!isFixedLength) { SDE("dfdl:length must be fixed when nilKind='literalCharacter'.") }

          lengthKind match {
            case LengthKind.Explicit => LiteralCharacterNilOfSpecifiedLength(this)
            case LengthKind.Implicit if isSimpleType => LiteralCharacterNilOfSpecifiedLength(this)
            case LengthKind.Implicit if isComplexType => Assert.invariantFailed("literal nil complex types aren't handled here.")
            case LengthKind.Prefixed => SDE("nilKind='literalCharacter' is not valid for lengthKind='prefixed'")
            case LengthKind.EndOfParent => SDE("nilKind='literalCharacter' is not valid for lengthKind='endOfParent'")
            case LengthKind.Delimited => SDE("nilKind='literalCharacter' is not valid for lengthKind='delimited'")
            case LengthKind.Pattern => SDE("nilKind='literalCharacter' is not valid for lengthKind='pattern'")
          }
        }
        case NilKind.LogicalValue => notYetImplemented("nilLitContent nilKind='logicalValue'")
      }

    }

  private def withDelimiterStack(body: => Gram) = {
    if (hasDelimiters || immediatelyEnclosingModelGroup.map(_.hasDelimiters).getOrElse(false)) DelimiterStackCombinatorElement(this, body)
    else body
  }

  private lazy val nilOrValue = prod("nilOrValue", isNillable) { // TODO: make it exclude emptyness once emptyness is implemented
    SimpleNilOrValue(this, nilLit || parsedNil, parsedValue)
  }

  private lazy val nonNillableParsedValue = prod("nonNilnonEmptyParsedValue", !isNillable) {
    parsedValue
  }

  private lazy val scalarDefaultableSimpleContent = prod("scalarDefaultableSimpleContent", isSimpleType) {
    nilOrValue || nonNillableParsedValue
  }

  /**
   * Note: This must handle unspecified lengths, like lengthKind delimited,
   * as well, by not enclosing the body in a specified length enforcer.
   */
  private def specifiedLength(bodyArg: => Gram) = {
    lazy val body = bodyArg
    lazy val bitsMultiplier = lengthUnits match {
      case LengthUnits.Bits => 1
      case LengthUnits.Bytes => 8
      case LengthUnits.Characters if knownEncodingIsFixedWidth => this.knownEncodingWidthInBits
      case _ => 0 // zero means can't multiply to get width in bits.
    }
    val lk = lengthKind
    lk match {
      case LengthKind.Delimited => body
      case LengthKind.Pattern => new SpecifiedLengthPattern(this, body)
      case LengthKind.Explicit if bitsMultiplier != 0 =>
        new SpecifiedLengthExplicit(this, body, bitsMultiplier)
      case LengthKind.Explicit => {
        Assert.invariant(!knownEncodingIsFixedWidth)
        Assert.invariant(lengthUnits eq LengthUnits.Characters)
        new SpecifiedLengthExplicitCharacters(this, body)
      }
      case LengthKind.Prefixed if bitsMultiplier != 0 =>
        new SpecifiedLengthPrefixed(this, body, bitsMultiplier)
      case LengthKind.Prefixed => {
        Assert.invariant(!knownEncodingIsFixedWidth)
        Assert.invariant(lengthUnits eq LengthUnits.Characters)
        new SpecifiedLengthPrefixedCharacters(this, body)
      }
      case LengthKind.Implicit if isSimpleType && primType == PrimType.String &&
        encodingInfo.knownEncodingIsFixedWidth => {
        //
        // Important case to optimize
        // If we can convert to a number of bits, then we should do so
        //
        val nBits = encodingInfo.knownFixedWidthEncodingInCharsToBits(this.maxLength.longValue)
        new SpecifiedLengthImplicit(this, body, nBits)
      }
      case LengthKind.Implicit if isSimpleType && primType == PrimType.String =>
        new SpecifiedLengthImplicitCharacters(this, body, this.maxLength.longValue)

      case LengthKind.Implicit if isSimpleType && primType == PrimType.HexBinary =>
        new SpecifiedLengthImplicit(this, body, this.maxLength.longValue * bitsMultiplier)
      case LengthKind.Implicit if isSimpleType && impliedRepresentation == Representation.Binary =>
        new SpecifiedLengthImplicit(this, body, implicitBinaryLengthInBits)
      case LengthKind.Implicit if isComplexType => body // for complex types, implicit means "roll up from the bottom"
      case LengthKind.EndOfParent if isComplexType => notYetImplemented("lengthKind='endOfParent' for complex type")
      case LengthKind.EndOfParent => notYetImplemented("lengthKind='endOfParent' for simple type")
      case _ => {
        // TODO: implement other specified length like end of parent
        // for now, no restriction
        body
      }
    }
  }

  private lazy val complexContentSpecifiedLength = prod("complexContentSpecifiedLength", isComplexType) {
    initiatorRegion ~ sharedComplexContentRegion
  }

  /**
   * We share the schema compilation objects for the sharable part of complex elements
   * which is the part inside the start framing.
   *
   * This eliminates redundant computation when element references reuse elements.
   *
   * It is critical that the 2nd argument to getShared is passed by name, so not
   * evaluated a second time when sharing opportunities are discovered (same shareKey).
   */
  private lazy val sharedComplexContentRegion: Gram =
    schemaSet.sharedComplexContentFactory.getShared(
      shareKey,
      captureLengthRegions(
        EmptyGram,
        specifiedLength(complexContent),
        elementUnused) ~
        terminatorRegion)

  private lazy val scalarComplexContent = prod("scalarComplexContent", isComplexType) {
    if (!nilLit.isEmpty) {
      ComplexNilOrContent(this, nilLit, complexContentSpecifiedLength)
    } else {
      complexContentSpecifiedLength
    }
  }

  private lazy val hasDynamicEscapeScheme = this.optionEscapeScheme.isDefined && !this.optionEscapeScheme.get.escapeSchemeParseEv.isConstant

  private def withEscapeScheme(body: Gram) = {
    if (hasDynamicEscapeScheme) DynamicEscapeSchemeCombinatorElement(this, body)
    else body
  }

  lazy val hasRepType = (isSimpleType && simpleType.optRepType.isDefined)
  lazy val optRepType = if (hasRepType) Some(simpleType.optRepType.get) else None
  lazy val optRepTypeElement = if (isSimpleType && simpleType.optRepTypeElement.isDefined) Some(simpleType.optRepTypeElement.get) else None

  /**
   * the element left framing does not include the initiator nor the element right framing the terminator
   */
  private lazy val alignAndSkipFraming = prod("alignAndSkipFraming") {
    LeadingSkipRegion(this) ~ AlignmentFill(this)
  }

  private lazy val elementLeftFraming = alignAndSkipFraming

  private lazy val elementRightFraming = prod("elementRightFraming") { TrailingSkipRegion(this) }

  lazy val enclosedElement = prod("enclosedElement") {
    //
    // not isScalar, because this is reused inside arrays
    // that is, we're counting on reusuing this production for array elements
    // which are enclosed by the enclosing array and model group.
    //
    // if we didn't reuse this way we'd have to reproduce much of the grammar
    // for the array case and scalar case that is the same for both.
    //
    checkVariousPropertyconstraints
    (inputValueCalcOption, outputValueCalcOption) match {
      case (_: NotFound, _: NotFound) => scalarDefaultablePhysical
      case (_: Found, _: NotFound) => inputValueCalcElement
      case (_: NotFound, _: Found) => outputValueCalcElement
      case _ => SDE("Element with both dfdl:inputValueCalc and dfdl:outputValueCalc is not allowed.")
    }
  }

  //
  // Until empty detection is implemented, there really is no distinction between
  // defaultable and non-defaulting elements.
  //
  protected final def enclosedElementNonDefault = enclosedElement

  private lazy val inputValueCalcPrim = InputValueCalc(self, inputValueCalcOption)

  protected final def ivcCompiledExpression = inputValueCalcPrim.expr

  private lazy val inputValueCalcElement = prod(
    "inputValueCalcElement",
    isSimpleType && inputValueCalcOption.isInstanceOf[Found]) {
      // No framing surrounding inputValueCalc elements.
      // Note that we need these elements even when unparsing, because they appear in the infoset
      // as regular elements (most times), and so we have to have an unparser that consumes the corresponding events.
      new ElementParseAndUnspecifiedLength(this, dfdlScopeBegin,
        inputValueCalcPrim, dfdlScopeEnd, EmptyGram)
    }

  protected final lazy val ovcCompiledExpression = { // ovcValueCalcObject.expr
    val exprProp = outputValueCalcOption.asInstanceOf[Found]
    val exprText = exprProp.value
    val exprNamespaces = exprProp.location.namespaces
    val qn = GlobalQName(Some("daf"), "outputValueCalc", XMLUtils.dafintURI)
    val expr = ExpressionCompilers.AnyRef.compileExpression(
      qn, primType, exprText, exprNamespaces, dpathCompileInfo, false, self, dpathCompileInfo)
    expr
  }

  private lazy val outputValueCalcElement =

    prod(
      "outputValueCalcElement",
      isSimpleType && outputValueCalcOption.isInstanceOf[Found]) {
        scalarDefaultablePhysical
      }

  // Note: there is no such thing as defaultable complex content because you can't have a
  // default value for a complex type element....
  // NOT TRUE: a defaultable complex type is one where everything within it is
  // recursively defaultable and has no syntax. So you could recursively "parse"
  // it, get default values for simple type elements in the complex type structure,
  // yet consume zero bits.
  //
  // When parsing that's exactly what should happen. It should parse, come back with
  // a complex type element, and have consumed no bits.
  //
  // When unparsing, we have to recognize that the complex type element matches
  // this default value, and then choose to unparse nothing.
  //
  // TBD: is this actually required by DFDL? Perhaps it is legal to simply
  // unparse the overall structure. When we unparse a simple type, and the
  // representation happens to be empty (such as empty string) the question is
  // are we to recognize this, and put down the emptyValueInitiator/Terminator based
  // on the emptyValueDelimiterPolicy? That has to be right. In order for a complex
  // type to be defaultable, and for us to output nothing (empty) corresponding
  // to a complex type, then all the contained simple type elements must be empty
  // and must have effectively no initiator nor terminator for empty values.
  //
  // But there are additional things like when empty string causes a Nil value to be
  // created, or a zero to be created for a text number.

  private lazy val scalarDefaultablePhysical = prod("scalarDefaultablePhysical") {

    val elem = if (hasRepType) {
      new ElementCombinator(this, dfdlScopeBegin, scalarDefaultableSimpleContent, dfdlScopeEnd, new TypeValueCalc(this))
    } else {
      new ElementCombinator(this, elementLeftFraming ~ dfdlScopeBegin,
        withDelimiterStack {
          withEscapeScheme {
            scalarDefaultableSimpleContent || scalarComplexContent
          }
        },
        elementRightFraming ~ dfdlScopeEnd)
    }
    elem
  }

  private lazy val checkVariousPropertyconstraints: Unit = {
    //
    // check for consistency. If length units is bytes, and we're going to use the length facets
    // of xs:string for implicit length, the encoding must be SBCS. Otherwise validation could fail when the
    // number of characters in that many bytes doesn't satisfy the facet.
    //
    if (isSimpleType &&
      primType == PrimType.String &&
      lengthKind == LengthKind.Implicit &&
      lengthUnits == LengthUnits.Bytes) {
      if (!isKnownEncoding) {
        //
        // TODO: this check is insisting on this being clear at compile time. But DFDL doesn't strictly speaking, require that.
        // If encoding is runtime-valued, this check could be done at runtime.
        //
        SDE("dfdl:encoding is a runtime expression, but dfdl:lengthKind 'implicit' for type xs:string and dfdl:lengthUnits 'bytes' requires an explicit known single byte character set encoding (SBCS).")
      } else if (knownEncodingWidthInBits != 8) {
        SDE(
          "dfdl:encoding '%s' is not a single-byte encoding, but dfdl:lengthKind 'implicit' for type xs:string and dfdl:lengthUnits 'bytes' a single byte character set encoding (SBCS) is required.",
          knownEncodingName)
      }
    }
    if (lengthKind != LengthKind.Explicit
      && optionLengthRaw.isDefined
      && !optionLengthRaw.isFromDefaultFormat) {
      //
      // this warning only applies if the dfdl:length is specified on the element, and
      // not inherited from a default format.
      // The NACHA format uses dfdl:length="0" in its default format so that some
      // global elements can compile.
      //
      // JIRA DFDL-1690
      //
      SDW(WarnID.InconsistentLengthKind, "dfdl:lengthKind '%s' is not consistent with dfdl:length specified (as %s). The dfdl:length will be ignored.",
        lengthKind,
        lengthExpr.prettyExpr)
    }
    if ((lengthKind == LengthKind.Explicit || lengthKind == LengthKind.Implicit) &&
      impliedRepresentation == Representation.Binary &&
      lengthUnits == LengthUnits.Characters)
      SDE("Elements of dfdl:lengthKind '%s' cannot have dfdl:lengthUnits '%s' with binary representation.", lengthKind, lengthUnits)
    (inputValueCalcOption, outputValueCalcOption) match {
      case (_: Found, _: Found) => SDE("Cannot have both dfdl:inputValueCalc and dfdl:outputValueCalc on the same element.")
      case _ => // ok
    }
    /*
     * When lengthKind is explicit and length is a constant, it is a warning if
 * the type is a type that respects minLength and maxLength, and the constant length
 * is not in range.
 */
    val isTypeUsingMinMaxLengthFacets = typeDef.typeNode match {
      case s: NodeInfo.String.Kind => true
      case s: NodeInfo.HexBinary.Kind => true
      case _ => false
    }
    if ((lengthKind eq LengthKind.Explicit) &&
      isTypeUsingMinMaxLengthFacets &&
      optLengthConstant.isDefined) {
      val len = optLengthConstant.get
      val maxLengthLong = maxLength.longValueExact
      val minLengthLong = minLength.longValueExact
      def warn(m: String, value: Long) = SDW(WarnID.FacetExplicitLengthOutOfRange, "Explicit dfdl:length of %s is out of range for facet %sLength='%s'.", len, m, value)
      if (maxLengthLong != -1 && len > maxLengthLong) warn("max", maxLengthLong)
      Assert.invariant(minLengthLong >= 0)
      if (minLengthLong > 0 && len < minLengthLong) warn("min", minLengthLong)
    }

    /*
     *  When length kind is explicit, and length is a constant, it is an SDE if
     * the type is a type that uses dfdl:textOutputMinLength, and the length constant
     * is not greater than or equal to that value.
     */

    val isTypeUsingTextOutputMinLength = typeDef.typeNode match {
      case s: NodeInfo.String.Kind => false
      case s: NodeInfo.HexBinary.Kind => false
      case s: NodeInfo.AnySimpleType.Kind if (impliedRepresentation eq Representation.Text) &&
        this.textOutputMinLength > 0 => true
      case _ => false
    }

    if ((lengthKind eq LengthKind.Explicit) &&
      isTypeUsingTextOutputMinLength &&
      optLengthConstant.isDefined) {
      val len = optLengthConstant.get
      if (len < textOutputMinLength)
        SDW(
          WarnID.TextOutputMinLengthOutOfRange,
          "Explicit dfdl:length of %s is out of range for dfdl:textOutputMinLength='%s'.",
          len,
          textOutputMinLength)

    }
  }

  /**
   * Mandatory text alignment or mta
   *
   * mta can only apply to things with encodings. No encoding, no MTA.
   *
   * In addition, it has to be textual data. Just because there's an encoding
   * in the property environment shouldn't get you an MTA region. It has
   * to be textual.
   */
  protected final lazy val valueMTA = prod(
    "mandatoryTextAlignment",
    impliedRepresentation eq Representation.Text) {
      mtaBase
    }

}
