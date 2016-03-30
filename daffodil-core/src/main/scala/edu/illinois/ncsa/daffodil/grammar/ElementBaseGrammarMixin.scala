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
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.dsom.Found
import edu.illinois.ncsa.daffodil.dsom.InitiatedTerminatedMixin
import edu.illinois.ncsa.daffodil.dsom.NotFound
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeBase
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import java.lang.{ Long => JLong }

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

trait ElementBaseGrammarMixin
    extends InitiatedTerminatedMixin
    with AlignedMixin
    with ByteOrderMixin
    with HasStatementsGrammarMixin { self: ElementBase =>

  /**
   * provided by LocalElementBase for array considerations, and GlobalElementDecl - scalar only
   */
  protected def allowedValue: Gram
  //
  // This silly redundancy where the variable name has to also be passed as a string,
  // is, by the way, a good reason Scala needs real Lisp-style macros, that can take an argument and
  // turn it into a type/class, object, def, or val/var name, as well as a string, etc.
  //

  private lazy val parsedNil = prod("parsedNil", NYI && isNillable && nilKind == NilKind.LogicalValue) {
    nilElementInitiator ~ LogicalNilValue(this) ~ nilElementTerminator
  }

  private lazy val parsedValue = prod("parsedValue") { initiatorRegion ~ allowedValue ~ terminatorRegion }

  // Length is in bits, (size would be in bytes) (from DFDL Spec 12.3.3)
  private lazy val implicitBinaryLengthInBits: Long = primType match {
    case PrimType.Byte | PrimType.UnsignedByte => 8
    case PrimType.Short | PrimType.UnsignedShort => 16
    case PrimType.Float | PrimType.Int | PrimType.UnsignedInt | PrimType.Boolean => 32
    case PrimType.Double | PrimType.Long | PrimType.UnsignedLong => 64
    case _ => schemaDefinitionError("Size of binary data '" + primType.name + "' cannot be determined implicitly.")
  }

  private lazy val binaryNumberKnownLengthInBits: Long = lengthKind match {
    case LengthKind.Implicit => implicitBinaryLengthInBits
    case LengthKind.Explicit if (length.isConstant) => {
      val lengthFromProp: JLong = length.constant
      val nbits = lengthUnits match {
        case LengthUnits.Bits => lengthFromProp.longValue()
        case LengthUnits.Bytes => lengthFromProp.longValue() * 8
        case LengthUnits.Characters => SDE("The lengthUnits for binary numbers must be either 'bits' or 'bytes'. Not 'characters'.")
      }
      nbits
    }
    case LengthKind.Explicit => -1 // means must be computed at runtime.
    case LengthKind.Delimited => schemaDefinitionError("Binary data elements cannot have lengthKind='delimited'.")
    case LengthKind.Pattern => schemaDefinitionError("Binary data elements cannot have lengthKind='pattern'.")
    case LengthKind.Prefixed => subsetError("lengthKind='prefixed' not yet supported.")
    case LengthKind.EndOfParent => schemaDefinitionError("Binary data elements cannot have lengthKind='endOfParent'.")
  }

  private lazy val fixedLengthHexBinary = prod("fixedLengthHexBinary", isFixedLength) {
    lengthUnits match {
      case LengthUnits.Bytes => HexBinaryFixedLengthInBytes(this, fixedLength)
      case LengthUnits.Bits => SDE("lengthUnits='bits' is not valid for hexBinary.")
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    }
  }

  private lazy val implicitLengthHexBinary = prod("implicitLengthHexBinary", hasSpecifiedLength) {
    val maxLengthLong = maxLength.longValueExact
    lengthUnits match {
      case LengthUnits.Bytes => HexBinaryFixedLengthInBytes(this, maxLengthLong)
      case LengthUnits.Bits => SDE("lengthUnits='bits' is not valid for hexBinary.")
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    }
  }

  private lazy val variableLengthHexBinary = prod("variableLengthHexBinary", !isFixedLength) {
    lengthUnits match {
      case LengthUnits.Bytes => HexBinaryVariableLengthInBytes(this)
      case LengthUnits.Bits => SDE("lengthUnits='bits' is not valid for hexBinary.")
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    }
  }

  private lazy val stringDelimitedEndOfData = prod("stringDelimitedEndOfData") { StringDelimitedEndOfData(this) }
  //  private lazy val stringPatternMatched = prod("stringPatternMatched") { StringPatternMatched(this) }

  private lazy val stringValue = prod("stringValue") {
    lengthKind match {
      case LengthKind.Explicit => specifiedLength(StringOfSpecifiedLength(this))
      case LengthKind.Delimited => stringDelimitedEndOfData
      case LengthKind.Pattern => specifiedLength(StringOfSpecifiedLength(this))
      case LengthKind.Implicit => {
        val pt = this.simpleType.primitiveType
        Assert.invariant(pt == PrimType.String)
        specifiedLength(StringOfSpecifiedLength(this))
      }
      case _ => SDE("Unimplemented lengthKind %s", lengthKind)
    }
  }

  private lazy val hexBinaryDelimitedEndOfData = prod("hexBinaryDelimitedEndOfData") { HexBinaryDelimitedEndOfData(this) }

  private lazy val hexBinaryValue = prod("hexBinaryValue") {
    lengthKind match {
      case LengthKind.Explicit if isFixedLength => fixedLengthHexBinary
      case LengthKind.Explicit => variableLengthHexBinary
      case LengthKind.Delimited => hexBinaryDelimitedEndOfData
      case LengthKind.Pattern => SDE("lengthKind Pattern is not allowed for hexBinary.")
      case LengthKind.Implicit => implicitLengthHexBinary
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

  // shorthand
  final lazy val primType = {
    val res = typeDef.asInstanceOf[SimpleTypeBase].primitiveType
    res
  }

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
    subset(binaryFloatRep.isConstant, "Dynamic binaryFloatRep is not supported.")
    BinaryFloatRep(binaryFloatRep.constant, this)
  }

  //  private lazy val binary = {
  //    subset(lengthKind == LengthKind.Explicit, "Currently only lengthKind='explicit' is supported.")
  //    LengthKind(lengthKind.toString, this)
  //  }

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
        if (binaryDecimalVirtualPoint > DaffodilTunableParameters.maxBinaryDecimalVirtualPoint)
          SDE("Property binaryDecimalVirtualPoint %s is greater than limit %s", binaryDecimalVirtualPoint, DaffodilTunableParameters.maxBinaryDecimalVirtualPoint)
        if (binaryDecimalVirtualPoint < DaffodilTunableParameters.minBinaryDecimalVirtualPoint)
          SDE("Property binaryDecimalVirtualPoint %s is less than limit %s", binaryDecimalVirtualPoint, DaffodilTunableParameters.minBinaryDecimalVirtualPoint)
        if (binaryNumberKnownLengthInBits == -1 ||
          binaryNumberKnownLengthInBits > 8) byteOrderRaw // must have or SDE
        binaryNumberKnownLengthInBits match {
          case -1 => new BinaryDecimalRuntimeLength(this)
          case _ => new BinaryDecimalKnownLength(this, binaryNumberKnownLengthInBits)
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
      case PrimType.Boolean => notYetImplemented("textValue: boolean")
      case PrimType.Date => textDate
      case PrimType.Time => textTime
      case PrimType.DateTime => textDateTime
      case _ => schemaDefinitionError("Unrecognized primitive type: " + primType.name)
    }
    res
  }

  protected final lazy val empty = prod("empty", NYI && emptyIsAnObservableConcept) { emptyRepresentation }

  private lazy val emptyRepresentation = prod("emptyRepresentation") {
    simpleOrNonImplicitComplexEmpty | complexImplicitEmpty
  }

  private lazy val simpleOrNonImplicitComplexEmpty = prod("simpleOrNonImplicitComplexEmpty",
    NYI && isSimpleType | isComplexType && lengthKind != LengthKind.Implicit) {
      emptyElementInitiator ~ emptyElementTerminator
    }

  /**
   * This is about the case where we take an empty, parse a complex type recursively from it
   * and potentially succeed.
   */
  private lazy val complexImplicitEmpty = prod("complexImplicitEmpty", NYI &&
    isComplexType && lengthKind == LengthKind.Implicit) {
    SaveInputStream(this) ~ SetEmptyInputStream(this) ~ elementComplexType.mainGrammar ~
      RestoreInputStream(this) ~ emptyElementTerminator
  }

  //  private lazy val emptyDefaulted = prod("emptyDefaulted",
  //    isDefaultable && emptyIsAnObservableConcept) {
  //      empty ~ TheDefaultValue(this)
  //    }

  private lazy val nilElementInitiator = prod("nilElementInitiator", hasInitiator) { Initiator(this) }
  private lazy val nilElementTerminator = prod("nilElementTerminator", hasTerminator) { Terminator(this) }

  private lazy val emptyElementInitiator = prod("emptyElementInitiator", NYI && hasEmptyValueInitiator) { ErrorGram }
  private lazy val emptyElementTerminator = prod("emptyElementTerminator", NYI && hasEmptyValueTerminator) { ErrorGram }

  private lazy val complexContent = prod("complexContent", isComplexType) {
    elementComplexType.mainGrammar
  }

  private lazy val nilLit = prod("nilLit",
    isNillable && ((nilKind == NilKind.LiteralValue) || (nilKind == NilKind.LiteralCharacter))) {
      if (hasDelimiters)
        DelimiterStackCombinatorElement(this, nilElementInitiator ~ nilLitSimpleOrComplex ~ nilElementTerminator)
      else
        nilLitSimpleOrComplex
    }

  private lazy val nilLitSimpleOrComplex = prod("nilLitSimpleOrComplex") { nilLitSimple || nilLitComplex }

  private lazy val nilLitSimple = prod("nilLitSimple", isSimpleType) { specifiedLength(nilLitContent) }

  private lazy val nilLitComplex = prod("nilLitComplex", isComplexType) {
    // Note: the only allowed nil value for a complex type is ES. It's length will be zero always. (as of DFDL v1.0 - 2015-07-15)
    schemaDefinitionUnless(this.hasESNilValue && cookedNilValuesForParse.length == 1, "Nillable complex type elements can only have '%ES;' as their dfdl:nilValue property.")
    val nilLength = 0
    new SpecifiedLengthExplicitBytesFixed(this, LiteralValueNilOfSpecifiedLength(this), nilLength)
  }

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
            case LengthKind.EndOfParent => notYetImplemented("lengthKind='endOfParent'")
          }
        }
        case NilKind.LiteralCharacter => {
          if (!isFixedLength) { SDE("dfdl:length must be 'fixed' when nilKind='literalCharacter'.") }

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
    if (hasDelimiters) DelimiterStackCombinatorElement(this, body)
    else body
  }

  private lazy val nilOrEmptyOrValue = prod("nilOrEmptyOrValue") {
    anyOfNilOrEmptyOrValue ||
      nilOrValue ||
      emptyOrValue ||
      nonNilNonEmptyParsedValue
  }

  private lazy val anyOfNilOrEmptyOrValue = prod("anyOfNilOrEmptyOrValue", isNillable && NYI && emptyIsAnObservableConcept) {
    SimpleNilOrEmptyOrValue(this, nilLit || parsedNil, empty, parsedValue)
  }

  private lazy val nilOrValue = prod("nilOrValue", isNillable) { // TODO: make it exclude emptyness
    SimpleNilOrValue(this, nilLit || parsedNil, parsedValue)
  }

  private lazy val emptyOrValue = prod("emptyOrValue", NYI && emptyIsAnObservableConcept && !isNillable) {
    SimpleEmptyOrValue(this, empty, parsedValue)
  }

  private lazy val nonNilNonEmptyParsedValue = prod("nonNilnonEmptyParsedValue", !isNillable) {
    parsedValue
  }

  private lazy val scalarDefaultableSimpleContent = prod("scalarDefaultableSimpleContent", isSimpleType) {
    withDelimiterStack(nilOrEmptyOrValue)
  }

  private lazy val scalarNonDefaultSimpleContent = prod("scalarNonDefaultSimpleContent", isSimpleType) {
    withDelimiterStack(nilOrValue || nonNilNonEmptyParsedValue)
  }

  /**
   * Note: This must handle unspecified lengths, like lengthKind delimited,
   * as well, by not enclosing the body in a specified length enforcer.
   */
  private def specifiedLength(bodyArg: => Gram) = {
    lazy val body = bodyArg
    lengthKind match {
      case LengthKind.Delimited => body
      case LengthKind.Pattern => new SpecifiedLengthPattern(this, body)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bits && isFixedLength => new SpecifiedLengthExplicitBitsFixed(this, body, fixedLength)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bits && !isFixedLength => new SpecifiedLengthExplicitBits(this, body)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bytes && isFixedLength => new SpecifiedLengthExplicitBytesFixed(this, body, fixedLength)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bytes && !isFixedLength => new SpecifiedLengthExplicitBytes(this, body)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Characters && isFixedLength
        && encodingInfo.knownEncodingIsFixedWidth => {
        //
        // Important case to optimize
        // If we can convert to a number of bits, then we should do so
        //
        val nBits = encodingInfo.knownFixedWidthEncodingInCharsToBits(fixedLength)
        new SpecifiedLengthExplicitBitsFixed(this, body, nBits)
      }
      case LengthKind.Explicit if lengthUnits == LengthUnits.Characters && isFixedLength =>
        new SpecifiedLengthExplicitCharactersFixed(this, body, fixedLength)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Characters && !isFixedLength =>
        new SpecifiedLengthExplicitCharacters(this, body)
      case LengthKind.Implicit if isSimpleType && primType == PrimType.String &&
        encodingInfo.knownEncodingIsFixedWidth => {
        //
        // Important case to optimize
        // If we can convert to a number of bits, then we should do so
        //
        val nBits = encodingInfo.knownFixedWidthEncodingInCharsToBits(this.maxLength.longValue)
        new SpecifiedLengthExplicitBitsFixed(this, body, nBits)
      }
      case LengthKind.Implicit if isSimpleType && primType == PrimType.String =>
        new SpecifiedLengthExplicitCharactersFixed(this, body, this.maxLength.longValue)
      case LengthKind.Implicit if isSimpleType && primType == PrimType.HexBinary =>
        new SpecifiedLengthExplicitBytesFixed(this, body, this.maxLength.longValue)
      case LengthKind.Implicit if isSimpleType && impliedRepresentation == Representation.Binary =>
        new SpecifiedLengthExplicitBitsFixed(this, body, implicitBinaryLengthInBits)
      case LengthKind.Implicit if isComplexType => body // for complex types, implicit means "roll up from the bottom"
      case _ => {
        // TODO: implement other specified length like prefixed and end of parent
        // for now, no restriction
        body
      }
    }
  }

  private lazy val complexContentSpecifiedLength = prod("complexContentSpecifiedLength", isComplexType) {
    if (hasDelimiters)
      DelimiterStackCombinatorElement(this, initiatorRegion ~ specifiedLength(complexContent) ~ terminatorRegion)
    else specifiedLength(complexContent)
  }

  private lazy val scalarComplexContent = prod("scalarComplexContent", isComplexType) {
    if (!nilLit.isEmpty) {
      ComplexNilOrContent(this, nilLit, complexContentSpecifiedLength)
    } else {
      complexContentSpecifiedLength
    }
  }

  private lazy val hasEscapeScheme = this.optionEscapeScheme.isDefined

  private def withEscapeScheme(body: Gram) = {
    if (hasEscapeScheme) EscapeSchemeStackCombinatorElement(this, body)
    else body
  }

  // Note: there is no such thing as defaultable complex content because you can't have a
  // default value for a complex type element....
  // NOT TRUE: a defaultable complex type is one where everything within it is
  // recursively defaultable and has no syntax. So you could recursively "parse"
  // it, get default values for simple type elements in the complex type structure,
  // yet consume zero bits.
  lazy val scalarDefaultableContent = prod("scalarDefaultableContent") {
    withEscapeScheme(scalarDefaultableSimpleContent || scalarComplexContent)
  }

  lazy val scalarNonDefaultContent = prod("scalarNonDefaultContent") {
    withEscapeScheme(scalarNonDefaultSimpleContent || scalarComplexContent)
  }

  /**
   * the element left framing does not include the initiator nor the element right framing the terminator
   */
  private lazy val elementLeftFraming = prod("elementLeftFraming") {
    byteOrderChange ~ termIOPropertiesChange ~ leadingSkipRegion ~ alignmentFill ~ PrefixLength(this)
  }

  private lazy val elementRightFraming = prod("elementRightFraming") { trailingSkipRegion }

  private lazy val scalarNonDefaultPhysical = prod("scalarNonDefault") {
    val body1 = elementLeftFraming ~ dfdlScopeBegin ~
      scalarNonDefaultContent
    val body2 = elementRightFraming ~ dfdlScopeEnd
    if (this.isParentUnorderedSequence)
      new ChoiceElementCombinator(this, body1, body2)
    else
      new ElementCombinator(this, body1, body2)
  }

  protected final lazy val scalarDefaultable = prod("scalarDefaultable") {
    (inputValueCalcOption, outputValueCalcOption) match {
      case (_: Found, _: Found) => SDE("Cannot have both dfdl:inputValueCalc and dfdl:outputValueCalc on the same element.")
      case (_: NotFound, _: NotFound) => scalarDefaultablePhysical
      case _ => DefaultablePhysicalOrComputed(this, scalarDefaultablePhysical, inputValueCalcElement, outputValueCalcElement, defaultableValue)
    }
  }

  // TODO: implement defaulting. This should generate a unparser that fills in the
  // infoset value if it is not present in the infoset already.
  private lazy val defaultableValue = prod("defaultableValue", NYI) { EmptyGram }

  lazy val scalarNonDefault = prod("scalarNonDefault") {
    (inputValueCalcOption, outputValueCalcOption) match {
      case (_: NotFound, _: NotFound) => scalarNonDefaultPhysical
      case _ => scalarDefaultable
    }
  }

  private lazy val inputValueCalcElement = prod("inputValueCalcElement",
    isSimpleType && inputValueCalcOption.isInstanceOf[Found], forWhat = ForParser) {
      // No framing surrounding inputValueCalc elements.
      new ElementCombinator(this, dfdlScopeBegin ~
        ValueCalc("inputValueCalc", self, inputValueCalcOption), dfdlScopeEnd)
    }

  private lazy val ovcValueCalcObject =
    ValueCalc("outputValueCalc", self, outputValueCalcOption, elementLeftFraming ~ scalarNonDefaultContent)

  protected final lazy val ovcCompiledExpression = ovcValueCalcObject.expr

  private lazy val outputValueCalcElement = prod("outputValueCalcElement",
    isSimpleType && outputValueCalcOption.isInstanceOf[Found], forWhat = ForUnparser) {
      new ElementCombinator(this,
        dfdlScopeBegin ~ ovcValueCalcObject,
        elementRightFraming ~ dfdlScopeEnd)
    }

  private lazy val scalarDefaultablePhysical = prod("scalarDefaultablePhysical") {
    if (this.isParentUnorderedSequence)
      new ChoiceElementCombinator(this, elementLeftFraming ~ dfdlScopeBegin ~
        scalarDefaultableContent, elementRightFraming ~ dfdlScopeEnd)
    else
      new ElementCombinator(this, elementLeftFraming ~ dfdlScopeBegin ~
        scalarDefaultableContent, elementRightFraming ~ dfdlScopeEnd)
  }

}
