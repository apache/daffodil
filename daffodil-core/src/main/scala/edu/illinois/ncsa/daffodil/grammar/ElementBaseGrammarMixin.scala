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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.dsom.InitiatedTerminatedMixin
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import java.lang.{ Long => JLong }
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dsom.ExpressionCompilers
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.grammar.primitives.InputValueCalc
import edu.illinois.ncsa.daffodil.grammar.primitives.ZonedTextIntPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.PaddingInfoMixin
import edu.illinois.ncsa.daffodil.grammar.primitives.StringOfSpecifiedLength
import edu.illinois.ncsa.daffodil.grammar.primitives.StringDelimitedEndOfData
import edu.illinois.ncsa.daffodil.grammar.primitives.PrefixLength
import edu.illinois.ncsa.daffodil.grammar.primitives.LiteralNilDelimitedEndOfData
import edu.illinois.ncsa.daffodil.grammar.primitives.HexBinarySpecifiedLength
import edu.illinois.ncsa.daffodil.grammar.primitives.HexBinaryEndOfBitLimit
import edu.illinois.ncsa.daffodil.grammar.primitives.HexBinaryDelimitedEndOfData
import edu.illinois.ncsa.daffodil.grammar.primitives.TrailingSkipRegion
import edu.illinois.ncsa.daffodil.grammar.primitives.LeadingSkipRegion
import edu.illinois.ncsa.daffodil.grammar.primitives.AlignmentFill
import edu.illinois.ncsa.daffodil.grammar.primitives.LogicalNilValue
import edu.illinois.ncsa.daffodil.grammar.primitives.LiteralValueNilOfSpecifiedLength
import edu.illinois.ncsa.daffodil.grammar.primitives.LiteralCharacterNilOfSpecifiedLength
import edu.illinois.ncsa.daffodil.grammar.primitives.BinaryBoolean
import edu.illinois.ncsa.daffodil.grammar.primitives.SpecifiedLengthPattern
import edu.illinois.ncsa.daffodil.grammar.primitives.SpecifiedLengthImplicitCharacters
import edu.illinois.ncsa.daffodil.grammar.primitives.SpecifiedLengthImplicit
import edu.illinois.ncsa.daffodil.grammar.primitives.SpecifiedLengthExplicitCharacters
import edu.illinois.ncsa.daffodil.grammar.primitives.SpecifiedLengthExplicit
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextUnsignedShortPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextUnsignedLongPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextUnsignedIntPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextUnsignedBytePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextShortPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextNonNegativeIntegerPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextLongPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextIntegerPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextIntPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextFloatPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextDoublePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextDecimalPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextCombinator
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextBytePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextBooleanPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextTimePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextDateTimePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextDatePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.Terminator
import edu.illinois.ncsa.daffodil.grammar.primitives.Initiator
import edu.illinois.ncsa.daffodil.grammar.primitives.RightFill
import edu.illinois.ncsa.daffodil.grammar.primitives.RightCenteredPadding
import edu.illinois.ncsa.daffodil.grammar.primitives.OnlyPadding
import edu.illinois.ncsa.daffodil.grammar.primitives.OVCRetry
import edu.illinois.ncsa.daffodil.grammar.primitives.NilLiteralCharacter
import edu.illinois.ncsa.daffodil.grammar.primitives.LeftCenteredPadding
import edu.illinois.ncsa.daffodil.grammar.primitives.ElementUnused
import edu.illinois.ncsa.daffodil.grammar.primitives.ElementParseAndUnspecifiedLength
import edu.illinois.ncsa.daffodil.grammar.primitives.ElementCombinator
import edu.illinois.ncsa.daffodil.grammar.primitives.CaptureValueLengthStart
import edu.illinois.ncsa.daffodil.grammar.primitives.CaptureValueLengthEnd
import edu.illinois.ncsa.daffodil.grammar.primitives.CaptureContentLengthStart
import edu.illinois.ncsa.daffodil.grammar.primitives.CaptureContentLengthEnd
import edu.illinois.ncsa.daffodil.grammar.primitives.SimpleNilOrValue
import edu.illinois.ncsa.daffodil.grammar.primitives.ComplexNilOrContent
import edu.illinois.ncsa.daffodil.grammar.primitives.BinaryIntegerRuntimeLength
import edu.illinois.ncsa.daffodil.grammar.primitives.BinaryIntegerKnownLength
import edu.illinois.ncsa.daffodil.grammar.primitives.BinaryFloat
import edu.illinois.ncsa.daffodil.grammar.primitives.BinaryDouble
import edu.illinois.ncsa.daffodil.grammar.primitives.BinaryDecimalRuntimeLength
import edu.illinois.ncsa.daffodil.grammar.primitives.BinaryDecimalKnownLength
import edu.illinois.ncsa.daffodil.grammar.primitives.DynamicEscapeSchemeCombinatorElement
import edu.illinois.ncsa.daffodil.grammar.primitives.DelimiterStackCombinatorElement
import edu.illinois.ncsa.daffodil.grammar.primitives.LogicalNilValue
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextDatePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ZonedTextIntPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextShortPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.TrailingSkipRegion
import edu.illinois.ncsa.daffodil.grammar.primitives.DynamicEscapeSchemeCombinatorElement
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextDecimalPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextBooleanPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextBytePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.AlignmentFill
import edu.illinois.ncsa.daffodil.grammar.primitives.ComplexNilOrContent
import edu.illinois.ncsa.daffodil.grammar.primitives.LeadingSkipRegion
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextFloatPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextUnsignedIntPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextUnsignedBytePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextUnsignedLongPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextIntPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextLongPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextNonNegativeIntegerPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextUnsignedShortPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextIntegerPrim
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextDoublePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.LiteralCharacterNilOfSpecifiedLength
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextTimePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.DelimiterStackCombinatorElement
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextCombinator
import edu.illinois.ncsa.daffodil.grammar.primitives.SimpleNilOrValue
import edu.illinois.ncsa.daffodil.grammar.primitives.ConvertTextDateTimePrim
import edu.illinois.ncsa.daffodil.grammar.primitives.LiteralValueNilOfSpecifiedLength
import edu.illinois.ncsa.daffodil.schema.annotation.props.NotFound
import edu.illinois.ncsa.daffodil.schema.annotation.props.Found

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

trait ElementBaseGrammarMixin
  extends InitiatedTerminatedMixin
  with AlignedMixin
  with HasStatementsGrammarMixin
  with PaddingInfoMixin { self: ElementBase =>

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
    //    if (res)
    //      println("%s should add fill.".format(this.diagnosticDebugName))
    //    else
    //      println("%s NOT adding fill.".format(this.diagnosticDebugName))
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
            binaryNumberRep eq BinaryNumberRep.Binary
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
    //    if (res)
    //      println("%s should check excess length.".format(this.diagnosticDebugName))
    //    else
    //      println("%s NOT checking excess length.".format(this.diagnosticDebugName))
    res
  }

  private lazy val rightFill = new RightFill(context)

  // maybe can be private - if it is used still
  protected lazy val elementUnused = new ElementUnused(context)

  /**
   * provided by ElementBase for array considerations, and GlobalElementDecl - scalar only
   */
  protected def allowedValue: Gram
  //
  // This silly redundancy where the variable name has to also be passed as a string,
  // is, by the way, a good reason Scala needs real Lisp-style macros, that can take an argument and
  // turn it into a type/class, object, def, or val/var name, as well as a string, etc.
  //

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

  private lazy val parsedValue = prod("parsedValue", isSimpleType) {
    initiatorRegion ~
      valueMTA ~
      captureLengthRegions(leftPadding, ovcRetry(allowedValue), rightPadding ~ rightFill) ~
      terminatorRegion
  }

  /**
   * Wrapped around the simple value unparsers used for dfdl:outputValueCalc (OVC)
   * with a combinator. Will retry them until the value is available.
   *
   * Evaporates for parsing, and when the element is not dfdl:outputValueCalc.
   *
   * Note: must be wrapped around the unparsed value only, not the framing.
   * Because some framing is alignment regions, which are themselves possibly
   * blocking/retrying. Setup of these must happen on the first pass, we cannot
   * do setups of ustate/data-output-streams when unparsing the result of an OVC.
   */
  private def ovcRetry(allowedValueArg: => Gram) = {
    lazy val allowedValue = allowedValueArg
    if (this.isOutputValueCalc)
      OVCRetry(this, allowedValue)
    else
      allowedValue
  }

  // Length is in bits, (size would be in bytes) (from DFDL Spec 12.3.3)
  final protected lazy val implicitBinaryLengthInBits: Long = primType match {
    case PrimType.Byte | PrimType.UnsignedByte => 8
    case PrimType.Short | PrimType.UnsignedShort => 16
    case PrimType.Float | PrimType.Int | PrimType.UnsignedInt | PrimType.Boolean => 32
    case PrimType.Double | PrimType.Long | PrimType.UnsignedLong => 64
    case _ => schemaDefinitionError("Size of binary data '" + primType.name + "' cannot be determined implicitly.")
  }

  private lazy val binaryNumberKnownLengthInBits: Long = lengthKind match {
    case LengthKind.Implicit => implicitBinaryLengthInBits
    case LengthKind.Explicit if (lengthEv.isConstant) => explicitBinaryLengthInBits()
    case LengthKind.Explicit => -1 // means must be computed at runtime.
    case LengthKind.Delimited => subsetError("lengthKind='delimited' not yet supported.")
    case LengthKind.Pattern => schemaDefinitionError("Binary data elements cannot have lengthKind='pattern'.")
    case LengthKind.Prefixed => subsetError("lengthKind='prefixed' not yet supported.")
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
  //  private lazy val stringPatternMatched = prod("stringPatternMatched") { StringPatternMatched(this) }

  private lazy val stringValue = prod("stringValue") { stringPrim }

  private lazy val stringPrim = {
    lengthKind match {
      case LengthKind.Explicit => specifiedLength(StringOfSpecifiedLength(this))
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

  private lazy val hexBinaryValue = prod("hexBinaryValue") {
    lengthKind match {
      case LengthKind.Explicit => specifiedLengthHexBinary
      case LengthKind.Implicit => specifiedLengthHexBinary
      case LengthKind.Delimited => hexBinaryDelimitedEndOfData
      case LengthKind.Pattern => hexBinaryLengthPattern
      case LengthKind.EndOfParent if isComplexType => notYetImplemented("lengthKind='endOfParent' for complex type")
      case LengthKind.EndOfParent => notYetImplemented("lengthKind='endOfParent' for simple type")
      case _ => SDE("Unimplemented lengthKind %s", lengthKind)
    }
  }

  private lazy val textInt = prod("textInt", impliedRepresentation == Representation.Text) {
    standardTextInt || zonedTextInt
  }

  private lazy val textByte = prod("textByte", impliedRepresentation == Representation.Text) {
    standardTextByte || zonedTextInt
  }

  private lazy val textShort = prod("textShort", impliedRepresentation == Representation.Text) {
    standardTextShort || zonedTextInt
  }

  private lazy val textLong = prod("textLong", impliedRepresentation == Representation.Text) {
    standardTextLong || zonedTextInt
  }

  private lazy val textInteger = prod("textInteger", impliedRepresentation == Representation.Text) {
    standardTextInteger || zonedTextInt
  }

  private lazy val textDecimal = prod("textDecimal", impliedRepresentation == Representation.Text) {
    standardTextDecimal || zonedTextInt
  }

  private lazy val textNonNegativeInteger = prod("textNonNegativeInteger", impliedRepresentation == Representation.Text) {
    standardTextNonNegativeInteger || zonedTextInt
  }

  private lazy val textUnsignedInt = prod("textUnsignedInt", impliedRepresentation == Representation.Text) {
    standardTextUnsignedInt || zonedTextInt
  }

  private lazy val textUnsignedByte = prod("textUnsignedByte", impliedRepresentation == Representation.Text) {
    standardTextUnsignedByte || zonedTextInt
  }

  private lazy val textUnsignedShort = prod("textUnsignedShort", impliedRepresentation == Representation.Text) {
    standardTextUnsignedShort || zonedTextInt
  }

  private lazy val textUnsignedLong = prod("textUnsignedLong", impliedRepresentation == Representation.Text) {
    standardTextUnsignedLong || zonedTextInt
  }

  //
  // We could now break it down by lengthKind, and have specialized primitives
  // depending on the length kind.
  //
  private lazy val standardTextInteger = prod("standardTextInteger",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextIntegerPrim(this)) }
  private lazy val standardTextDecimal = prod("standardTextDecimal",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextDecimalPrim(this)) }
  private lazy val standardTextNonNegativeInteger = prod("standardTextNonNegativeInteger",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextNonNegativeIntegerPrim(this)) }
  private lazy val standardTextLong = prod("standardTextLong",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextLongPrim(this)) }
  private lazy val standardTextInt = prod("standardTextInt",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextIntPrim(this)) }
  private lazy val standardTextShort = prod("standardTextShort",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextShortPrim(this)) }
  private lazy val standardTextByte = prod("standardTextByte",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextBytePrim(this)) }
  private lazy val standardTextUnsignedLong = prod("standardTextUnsignedLong",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextUnsignedLongPrim(this)) }
  private lazy val standardTextUnsignedInt = prod("standardTextUnsignedInt",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextUnsignedIntPrim(this)) }
  private lazy val standardTextUnsignedShort = prod("standardTextUnsignedShort",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextUnsignedShortPrim(this)) }
  private lazy val standardTextUnsignedByte = prod("standardTextUnsignedByte",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextUnsignedBytePrim(this)) }
  private lazy val zonedTextInt = prod("zonedTextInt",
    textNumberRep == TextNumberRep.Zoned) { ZonedTextIntPrim(this) }

  private lazy val textDouble = prod("textDouble", impliedRepresentation == Representation.Text) {
    standardTextDouble || zonedTextDouble
  }

  //  private lazy val ibm390HexBinaryRepDouble = prod("ibm390HexBinaryRepDouble",
  //    binaryFloatRep.isConstant &&
  //      binaryFloatRep.constant == BinaryFloatRep.Ibm390Hex.toString) {
  //      subsetError("ibm390Hex not supported")
  //    }

  private lazy val standardTextDouble = prod("standardTextDouble",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextDoublePrim(this)) }

  private lazy val zonedTextDouble = prod("zonedTextDouble",
    textNumberRep == TextNumberRep.Zoned) { SDE("Zoned not supported for float and double") }

  private lazy val textFloat = prod("textFloat", impliedRepresentation == Representation.Text) {
    standardTextFloat || zonedTextFloat
  }

  private lazy val standardTextFloat = prod("standardTextFloat",
    textNumberRep == TextNumberRep.Standard) { ConvertTextCombinator(this, stringValue, ConvertTextFloatPrim(this)) }

  private lazy val zonedTextFloat = prod("zonedTextFloat",
    textNumberRep == TextNumberRep.Zoned) { SDE("Zoned not supported for float and double") }

  private lazy val textDate = prod("textDate", impliedRepresentation == Representation.Text) {
    ConvertTextCombinator(this, stringValue, ConvertTextDatePrim(this))
  }

  private lazy val textTime = prod("textTime", impliedRepresentation == Representation.Text) {
    ConvertTextCombinator(this, stringValue, ConvertTextTimePrim(this))
  }

  private lazy val textDateTime = prod("textDateTime", impliedRepresentation == Representation.Text) {
    ConvertTextCombinator(this, stringValue, ConvertTextDateTimePrim(this))
  }

  private lazy val textBoolean = prod("textBoolean", impliedRepresentation == Representation.Text) {
    ConvertTextCombinator(this, stringValue, ConvertTextBooleanPrim(this))
  }

  def primType: PrimType

  protected final lazy val value = prod("value", isSimpleType) {
    // TODO: Consider issues with matching a stopValue. Can't say isScalar here because
    // this gets used for array contents also.
    {
      primType match {
        case PrimType.String => stringValue
        case PrimType.HexBinary => hexBinaryValue
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

  // This is the right name that the DFDL property should have had!
  private lazy val binaryIntRep = {
    subset(binaryNumberRep == BinaryNumberRep.Binary, "binaryNumberRep='%s' is unsupported. Only 'binary' is supported.", binaryNumberRep.toString)
    binaryNumberRep
  }

  private lazy val staticBinaryFloatRep = {
    subset(binaryFloatRepEv.isConstant, "Dynamic binaryFloatRep is not supported.")
    binaryFloatRepEv.optConstant.get
  }

  val bin = BinaryNumberRep.Binary // shorthands for table dispatch
  val ieee = BinaryFloatRep.Ieee
  type BO = java.nio.ByteOrder

  private def binaryIntegerValue(isSigned: Boolean) = {
    //
    // Is it a single byte or smaller
    //
    if ((primType != PrimType.Byte) &&
      (binaryNumberKnownLengthInBits == -1 ||
        binaryNumberKnownLengthInBits > 8)) {
      byteOrderRaw // must be defined or SDE
    }
    Assert.invariant(binaryIntRep == bin)
    binaryNumberKnownLengthInBits match {
      case -1 => new BinaryIntegerRuntimeLength(this, isSigned)
      case _ => new BinaryIntegerKnownLength(this, isSigned, binaryNumberKnownLengthInBits)
    }
  }

  private lazy val binaryValue: Gram = {
    Assert.invariant(primType != PrimType.String)

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
        Assert.invariant(binaryIntRep == bin)
        if (binaryDecimalVirtualPoint > tunable.maxBinaryDecimalVirtualPoint)
          SDE("Property binaryDecimalVirtualPoint %s is greater than limit %s", binaryDecimalVirtualPoint, tunable.maxBinaryDecimalVirtualPoint)
        if (binaryDecimalVirtualPoint < tunable.minBinaryDecimalVirtualPoint)
          SDE("Property binaryDecimalVirtualPoint %s is less than limit %s", binaryDecimalVirtualPoint, tunable.minBinaryDecimalVirtualPoint)
        if (binaryNumberKnownLengthInBits == -1 ||
          binaryNumberKnownLengthInBits > 8) byteOrderRaw // must have or SDE
        binaryNumberKnownLengthInBits match {
          case -1 => new BinaryDecimalRuntimeLength(this)
          case _ => new BinaryDecimalKnownLength(this, binaryNumberKnownLengthInBits)
        }
      }

      case PrimType.Boolean => { new BinaryBoolean(this) }
      case _ => notYetImplemented("Type %s when representation='binary'", primType.name)
    }
    res
  }

  private lazy val textValue: Gram = {
    val pt = primType
    Assert.invariant(pt != PrimType.String)
    Assert.invariant(pt != PrimType.HexBinary)
    Assert.invariant(impliedRepresentation == Representation.Text)
    schemaDefinitionWhen(lengthKind == LengthKind.Implicit,
      "Type %s cannot have lengthKind='implicit' when representation='text'",
      pt.name)
    val res = primType match {
      case PrimType.Int => textInt
      case PrimType.Byte => textByte
      case PrimType.Short => textShort
      case PrimType.Long => textLong
      case PrimType.Integer => textInteger
      case PrimType.Decimal => textDecimal
      case PrimType.UnsignedInt => textUnsignedInt
      case PrimType.UnsignedByte => textUnsignedByte
      case PrimType.UnsignedShort => textUnsignedShort
      case PrimType.UnsignedLong => textUnsignedLong
      case PrimType.NonNegativeInteger => textNonNegativeInteger // Should be treated as unsigned xs:integer
      case PrimType.Double => textDouble
      case PrimType.Float => textFloat
      case PrimType.HexBinary => Assert.invariantFailed("Primitive hexBinary must be representation='binary'.")
      case PrimType.Boolean => textBoolean
      case PrimType.Date => textDate
      case PrimType.Time => textTime
      case PrimType.DateTime => textDateTime
      case _ => schemaDefinitionError("Unrecognized primitive type: " + primType.name)
    }
    res
  }

  protected final lazy val empty = prod("empty", NYI && emptyIsAnObservableConcept) { EmptyGram }

  //  private lazy val emptyRepresentation = prod("emptyRepresentation") {
  //    simpleOrNonImplicitComplexEmpty | complexImplicitEmpty
  //  }

  //  private lazy val simpleOrNonImplicitComplexEmpty = prod("simpleOrNonImplicitComplexEmpty",
  //    isSimpleType | isComplexType && lengthKind != LengthKind.Implicit) {
  //      emptyElementInitiator ~
  //        valueMTA ~
  //        captureLengthRegions(EmptyGram, EmptyGram, EmptyGram) ~
  //        emptyElementTerminator
  //    }
  //
  //  /**
  //   * This is about the case where we take an empty, parse a complex type recursively from it
  //   * and potentially succeed.
  //   */
  //  private lazy val complexImplicitEmpty = prod("complexImplicitEmpty",
  //    isComplexType && lengthKind == LengthKind.Implicit) {
  //      SaveInputStream(this) ~ SetEmptyInputStream(this) ~ complexType.mainGrammar ~
  //        RestoreInputStream(this) ~ emptyElementTerminator
  //    }

  //  private lazy val emptyDefaulted = prod("emptyDefaulted",
  //    isDefaultable && emptyIsAnObservableConcept) {
  //      empty ~ TheDefaultValue(this)
  //    }

  private lazy val nilElementInitiator = prod("nilElementInitiator", hasInitiator) { delimMTA ~ Initiator(this) }
  private lazy val nilElementTerminator = prod("nilElementTerminator", hasTerminator) { delimMTA ~ Terminator(this) }

  //  private lazy val emptyElementInitiator = prod("emptyElementInitiator", NYI && hasEmptyValueInitiator) { delimMTA ~ Initiator(this) }
  //  private lazy val emptyElementTerminator = prod("emptyElementTerminator", NYI && hasEmptyValueTerminator) { delimMTA ~ Terminator(this) }

  private lazy val complexContent = prod("complexContent", isComplexType) {
    complexType.mainGrammar
  }

  private lazy val isNilLit = isNillable && ((nilKind == NilKind.LiteralValue) || (nilKind == NilKind.LiteralCharacter))

  /**
   * In the below, we must have nilLitMTA because, in the case where it's textual,
   * then to distinguish a lit nil from a value, we have to start at the same place.
   */
  private lazy val nilLit = prod("nilLit", isNilLit) {
    nilElementInitiator ~
      nilLitMTA ~
      nilLitSimpleOrComplex ~
      nilElementTerminator
  }

  private lazy val nilLitSimpleOrComplex = prod("nilLitSimpleOrComplex") { nilLitSimple || nilLitComplex }

  private lazy val nilLitSimple = prod("nilLitSimple", isSimpleType) {
    captureLengthRegions(leftPadding,
      specifiedLength(nilLitContent) ~ // for parser
        NilLiteralCharacter(context), // for unparser
      rightPadding ~ rightFill)
  }

  private lazy val nilLitComplex = prod("nilLitComplex", isComplexType) {
    // Note: the only allowed nil value for a complex type is ES. It's length will be zero always. (as of DFDL v1.0 - 2015-07-15)
    schemaDefinitionUnless(this.hasESNilValue && cookedNilValuesForParse.length == 1, "Nillable complex type elements can only have '%ES;' as their dfdl:nilValue property.")
    captureLengthRegions(EmptyGram,
      nilLitContent,
      //
      // Because nil complex can only be ES (e.g., length 0), there's no possible
      // ElementUnused region after a nil.
      EmptyGram)

  }

  private lazy val nilLitMTA = prod("nilLitMTA", isNilLit) { mtaBase }

  private lazy val nilLitContent = prod("nilLitContent",
    isNillable && (nilKind == NilKind.LiteralValue || nilKind == NilKind.LiteralCharacter)) {

      nilKind match {
        case NilKind.LiteralValue => {
          // if (impliedRepresentation != Representation.Text) this.SDE("LiteralValue Nils require representation='text'.")
          lengthKind match {
            case LengthKind.Delimited => LiteralNilDelimitedEndOfData(this)
            case LengthKind.Pattern => LiteralValueNilOfSpecifiedLength(this)
            case LengthKind.Explicit => LiteralValueNilOfSpecifiedLength(this)
            case LengthKind.Implicit if isSimpleType => {
              schemaDefinitionUnless(impliedRepresentation != Representation.Text, "LiteralValue Nils with lengthKind='implicit' cannot have representation='text'.")
              LiteralValueNilOfSpecifiedLength(this)
            }
            case LengthKind.Implicit if isComplexType => Assert.invariantFailed("literal nil complex types aren't handled here.")
            case LengthKind.Prefixed => notYetImplemented("lengthKind='prefixed'")
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
    if (hasDelimiters || enclosingTerm.map(_.hasDelimiters).getOrElse(false)) DelimiterStackCombinatorElement(this, body)
    else body
  }

  private lazy val nilOrEmptyOrValue = prod("nilOrEmptyOrValue") {
    // anyOfNilOrEmptyOrValue ||
    nilOrValue ||
      //   emptyOrValue ||
      nonNilNonEmptyParsedValue
  }

  //  private lazy val anyOfNilOrEmptyOrValue = prod("anyOfNilOrEmptyOrValue", isNillable && NYI && emptyIsAnObservableConcept) {
  //    SimpleNilOrEmptyOrValue(this, nilLit || parsedNil, empty, parsedValue)
  //  }

  private lazy val nilOrValue = prod("nilOrValue", isNillable) { // TODO: make it exclude emptyness once emptyness is implemented
    SimpleNilOrValue(this, nilLit || parsedNil, parsedValue)
  }

  //  private lazy val emptyOrValue = prod("emptyOrValue", NYI && emptyIsAnObservableConcept && !isNillable) {
  //    SimpleEmptyOrValue(this, empty, parsedValue)
  //  }

  private lazy val nonNilNonEmptyParsedValue = prod("nonNilnonEmptyParsedValue", !isNillable) { // TODO: make it exclude emptyness once emptyness is implemented
    parsedValue
  }

  private lazy val scalarDefaultableSimpleContent = prod("scalarDefaultableSimpleContent", isSimpleType) {
    nilOrEmptyOrValue
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
        // TODO: implement other specified length like prefixed and end of parent
        // for now, no restriction
        body
      }
    }
  }

  private lazy val complexContentSpecifiedLength = prod("complexContentSpecifiedLength", isComplexType) {
    initiatorRegion ~
      captureLengthRegions(EmptyGram,
        specifiedLength(complexContent),
        elementUnused) ~
        terminatorRegion
  }

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

  /**
   * the element left framing does not include the initiator nor the element right framing the terminator
   */
  private lazy val alignAndSkipFraming = prod("alignAndSkipFraming") {
    LeadingSkipRegion(this) ~ AlignmentFill(this) ~ PrefixLength(this)
  }

  private lazy val elementLeftFraming = alignAndSkipFraming

  private lazy val elementRightFraming = prod("elementRightFraming") { TrailingSkipRegion(this) }

  protected lazy val enclosedElement = prod("enclosedElement") {
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

  private lazy val inputValueCalcElement = prod("inputValueCalcElement",
    isSimpleType && inputValueCalcOption.isInstanceOf[Found]) {
      // No framing surrounding inputValueCalc elements.
      // Note that we need these elements even when unparsing, because they appear in the infoset
      // as regular elements (most times), and so we have to have an unparser that consumes the corresponding events.
      new ElementParseAndUnspecifiedLength(this, dfdlScopeBegin,
        inputValueCalcPrim, dfdlScopeEnd)
    }

  protected final lazy val ovcCompiledExpression = { // ovcValueCalcObject.expr
    val exprProp = outputValueCalcOption.asInstanceOf[Found]
    val exprText = exprProp.value
    val exprNamespaces = exprProp.location.namespaces
    val qn = GlobalQName(Some("daf"), "outputValueCalc", XMLUtils.dafintURI)
    val expr = ExpressionCompilers.AnyRef.compile(qn,
      primType, exprText, exprNamespaces, dpathCompileInfo, false, self)
    expr
  }

  private lazy val outputValueCalcElement =

    prod("outputValueCalcElement",
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

    val elem = new ElementCombinator(this, elementLeftFraming ~ dfdlScopeBegin,
      withDelimiterStack {
        withEscapeScheme {
          scalarDefaultableSimpleContent || scalarComplexContent
        }
      },
      elementRightFraming ~ dfdlScopeEnd)
    elem
  }

  private def checkVariousPropertyconstraints {
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
        SDE("dfdl:encoding '%s' is not a single-byte encoding, but dfdl:lengthKind 'implicit' for type xs:string and dfdl:lengthUnits 'bytes' a single byte character set encoding (SBCS) is required.",
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
      SDW("dfdl:lengthKind '%s' is not consistent with dfdl:length specified (as %s). The dfdl:length will be ignored.",
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
      def warn(m: String, value: Long) = SDW("Explicit dfdl:length of %s is out of range for facet %sLength='%s'.", len, "max", value)
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
      case s: NodeInfo.AnySimpleType.Kind if (impliedRepresentation eq Representation.Binary) &&
        this.textOutputMinLength > 0 => true
      case _ => false
    }

    if ((lengthKind eq LengthKind.Explicit) &&
      isTypeUsingTextOutputMinLength &&
      optLengthConstant.isDefined) {
      val len = optLengthConstant.get
      if (len < textOutputMinLength) SDE("Explicit dfdl:length of %s is out of range for dfdl:textOutputMinLength='%s'.", len, textOutputMinLength)

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
  protected final lazy val valueMTA = prod("mandatoryTextAlignment",
    impliedRepresentation eq Representation.Text) {
      mtaBase
    }

}
