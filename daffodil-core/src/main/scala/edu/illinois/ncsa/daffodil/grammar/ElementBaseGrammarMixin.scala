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
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.util._
import java.math.BigInteger
import edu.illinois.ncsa.daffodil.dsom.Found
import edu.illinois.ncsa.daffodil.dsom.InitiatedTerminatedMixin
import edu.illinois.ncsa.daffodil.dsom.NotFound
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeBase
import edu.illinois.ncsa.daffodil.dsom.ElementBase

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

trait ElementBaseGrammarMixin
  extends InitiatedTerminatedMixin
  with AlignedMixin
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

  //  private lazy val explicitLengthBinary = prod("explicitLengthBinary", !isFixedLength,
  //    lengthUnits match {
  //      case LengthUnits.Bytes => BinaryExplicitLengthInBytes(this)
  //      case LengthUnits.Characters => schemaDefinitionError("Binary data elements cannot have lengthUnits='Character'.")
  //      case LengthUnits.Bits => BinaryExplicitLengthInBits(this)
  //    })
  //
  //  private lazy val binaryValueLength = binaryValueLength_.value
  //  private lazy val binaryValueLength_ = LV {
  //    val res = prod("BinaryValueLength", lengthKind match {
  //      case LengthKind.Explicit => explicitLengthBinary
  //      case LengthKind.Delimited => Assert.notYetImplemented() // Binary Data delimiters aren't supported TODO: Should we?
  //      case LengthKind.Pattern => schemaDefinitionError("Binary data elements cannot have lengthKind='Pattern'.")
  //      case LengthKind.Implicit => Assert.notYetImplemented() // TODO: Get size from xs:type
  //      case _ => Assert.notYetImplemented()
  //    })
  //    res
  //  }

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
      val lengthFromProp = length.constantAsLong
      val nbits = lengthUnits match {
        case LengthUnits.Bits => lengthFromProp
        case LengthUnits.Bytes => lengthFromProp * 8
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

  private lazy val fixedLengthString = prod("fixedLengthString", isFixedLength) {
    (lengthUnits, knownEncodingIsFixedWidth) match {
      case (LengthUnits.Bytes, true) => StringFixedLengthInBytesFixedWidthCharacters(this, fixedLength) // TODO: make sure it divides evenly.
      //case (LengthUnits.Bytes, true) => StringFixedLengthInBytes(this, fixedLength / knownEncodingWidth) // TODO: make sure it divides evenly.
      case (LengthUnits.Bytes, false) => {
        // StringFixedLengthInBytesVariableWidthCharacters(this, fixedLength)
        subsetError("The lengthUnits for encoding '%s' (which is, or could be, variable width) must be 'characters', not 'bytes'", this.knownEncodingName)
      }
      case (LengthUnits.Characters, true) => {
        //
        // we deal with the fact that some encodings have characters taking up smaller than 
        // a full byte. E.g., encoding='US-ASCII-7-bit-packed' are 7-bits packed with no unused
        // bits
        //
        val lengthInBits = fixedLength * knownEncodingWidthInBits
        val lengthInBytes = lengthInBits / 8
        val hasWholeBytesOnly = (lengthInBits % 8) == 0
        if (hasWholeBytesOnly)
          StringFixedLengthInBytesFixedWidthCharacters(this, lengthInBytes)
        else
          StringFixedLengthInBytesFixedWidthCharacters(this, lengthInBytes + 1) // 1 more for fragment byte
      }
      //
      // The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      // 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      case (LengthUnits.Characters, false) => StringFixedLengthInVariableWidthCharacters(this, fixedLength)
      case (LengthUnits.Bits, _) => notYetImplemented("lengthUnits='bits' for type " + typeDef)
      case _ => Assert.invariantFailed("all cases should have been exhausted.")
    }
  }

  private lazy val fixedLengthHexBinary = prod("fixedLengthHexBinary", isFixedLength) {
    lengthUnits match {
      case LengthUnits.Bytes => HexBinaryFixedLengthInBytes(this, fixedLength)
      case LengthUnits.Bits => SDE("lengthUnits='bits' is not valid for hexBinary.")
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    }
  }

  private lazy val implicitLengthString = prod("implicitLengthString", hasSpecifiedLength) {
    val maxLengthLong = maxLength.longValueExact
    (lengthUnits, knownEncodingIsFixedWidth) match {
      case (LengthUnits.Bytes, true) => StringFixedLengthInBytesFixedWidthCharacters(this, maxLengthLong) // TODO: make sure it divides evenly.
      //case (LengthUnits.Bytes, true) => StringFixedLengthInBytes(this, fixedLength / knownEncodingWidth) // TODO: make sure it divides evenly.
      case (LengthUnits.Bytes, false) => {
        // StringFixedLengthInBytesVariableWidthCharacters(this, maxLengthLong)
        subsetError("The lengthUnits for encoding '%s' (which is variable width) must be 'characters', not 'bytes'", this.knownEncodingName)
      }
      case (LengthUnits.Characters, true) => {
        //
        // we deal with the fact that some encodings have characters taking up smaller than 
        // a full byte. E.g., encoding='US-ASCII-7-bit-packed' are 7-bits packed with no unused
        // bits
        //
        val lengthInBits = maxLengthLong * knownEncodingWidthInBits
        val lengthInBytes = lengthInBits / 8
        val hasWholeBytesOnly = (lengthInBits % 8) == 0
        if (hasWholeBytesOnly)
          StringFixedLengthInBytesFixedWidthCharacters(this, lengthInBytes)
        else
          StringFixedLengthInBytesFixedWidthCharacters(this, lengthInBytes + 1) // 1 more for fragment byte
      }
      //
      // The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      // 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      case (LengthUnits.Characters, false) => StringFixedLengthInVariableWidthCharacters(this, maxLengthLong)
      case (LengthUnits.Bits, _) => SDE("Strings with lengthKind='implicit' may not have lengthUnits='bits'")
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

  private lazy val variableLengthString = prod("variableLengthString", !isFixedLength) {
    (lengthUnits, knownEncodingIsFixedWidth) match {
      //case (LengthUnits.Bytes, true) => StringExplicitLengthInBytes(this)
      //case (LengthUnits.Bytes, false) =>
      //  notYetImplemented("lengthKind='explicit' and lengthUnits='bytes' with non-fixed-width or potentially non-fixed-width encoding='%s'.", this.encodingRaw)
      //// StringExplicitLengthInBytesVariableWidthCharacters(this)
      //case (LengthUnits.Characters, _) =>
      // notYetImplemented("lengthKind='explicit' and lengthUnits='characters'")
      //// Above, keep in mind fixed length but variable-width encoding means variable width.
      //// The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      //// 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      //case (LengthUnits.Bits, _) =>
      //  SDE("lengthKind='explicit' and lengthUnits='bits' for type %s", this.typeDef)
      case (LengthUnits.Bytes, true) => StringVariableLengthInBytes(this)
      case (LengthUnits.Bytes, false) => {
        // StringVariableLengthInBytesVariableWidthCharacters(this)
        subsetError("The lengthUnits for encoding '%s' (which is variable width) must be 'characters', not 'bytes'", this.knownEncodingName)
      }
      case (LengthUnits.Characters, true) => StringVariableLengthInBytes(this)
      // The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      // 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      case (LengthUnits.Characters, false) => {
        StringVariableLengthInVariableWidthCharacters(this)
      }
      case (LengthUnits.Bits, _) => SDE("lengthKind='explicit' and lengthUnits='bits' for type %s", this.typeDef)
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
  private lazy val stringPatternMatched = prod("stringPatternMatched") { StringPatternMatched(this) }

  private lazy val stringValue = prod("stringValue") {
    lengthKind match {
      case LengthKind.Explicit if isFixedLength => fixedLengthString
      case LengthKind.Explicit => variableLengthString
      //case LengthKind.Delimited => stringDelimitedEndOfData
      case LengthKind.Delimited => stringDelimitedEndOfData
      case LengthKind.Pattern => stringPatternMatched
      case LengthKind.Implicit => {
        val pt = this.simpleType.primitiveType
        Assert.invariant(pt == PrimType.String)
        implicitLengthString
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

  //  private lazy val binaryByte = prod("binaryByte", impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  private lazy val binaryShort = prod("binaryShort", impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  private lazy val binaryLong = prod("binaryLong", impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  private lazy val binaryInteger = prod("binaryInteger", impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  private lazy val binaryUnsignedInt = prod("binaryInt", impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  private lazy val binaryUnsignedByte = prod("binaryByte", impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  private lazy val binaryUnsignedShort = prod("binaryShort", impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  private lazy val binaryUnsignedLong = prod("binaryLong", impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)

  //  private lazy val regularBinaryRepInt = prod("regularBinaryRepInt",
  //    binaryNumberRep == BinaryNumberRep.Binary, lengthKind match {
  //      case LengthKind.Implicit => {
  //        if (byteOrder.isConstant) {
  //          val javaByteOrder = ByteOrder(byteOrder.constantAsString, this) match {
  //            case ByteOrder.BigEndian => java.nio.ByteOrder.BigEndian
  //            case ByteOrder.LittleEndian => java.nio.ByteOrder.LittleEndian
  //          }
  //          Regular32bitIntPrim(this, javaByteOrder)
  //        else Assert.notYetImplemented() // Dynamic byte order not implemented
  //      }
  //      case _ => Assert.notYetImplemented() // binary number length kinds other than implicit not implemented
  //    })

  //  private lazy val bcdInt = prod("bcdInt",
  //    binaryNumberRep == BinaryNumberRep.Bcd, BCDIntPrim(this))
  //  private lazy val packedInt = prod("packedInt",
  //    binaryNumberRep == BinaryNumberRep.Packed, PackedIntPrim(this))

  // TODO: Handle the zonedTextXXX possibilities
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
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextIntegerPrim(this) }
  private lazy val standardTextDecimal = prod("standardTextDecimal",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextDecimalPrim(this) }
  private lazy val standardTextNonNegativeInteger = prod("standardTextNonNegativeInteger",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextNonNegativeIntegerPrim(this) }
  private lazy val standardTextLong = prod("standardTextLong",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextLongPrim(this) }
  private lazy val standardTextInt = prod("standardTextInt",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextIntPrim(this) }
  private lazy val standardTextShort = prod("standardTextShort",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextShortPrim(this) }
  private lazy val standardTextByte = prod("standardTextByte",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextBytePrim(this) }
  private lazy val standardTextUnsignedLong = prod("standardTextUnsignedLong",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextUnsignedLongPrim(this) }
  private lazy val standardTextUnsignedInt = prod("standardTextUnsignedInt",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextUnsignedIntPrim(this) }
  private lazy val standardTextUnsignedShort = prod("standardTextUnsignedShort",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextUnsignedShortPrim(this) }
  private lazy val standardTextUnsignedByte = prod("standardTextUnsignedByte",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextUnsignedBytePrim(this) }
  private lazy val zonedTextInt = prod("zonedTextInt",
    textNumberRep == TextNumberRep.Zoned) { ZonedTextIntPrim(this) }

  //  private lazy val binaryDouble = prod("binaryDouble", impliedRepresentation == Representation.Binary){
  //    ieeeBinaryRepDouble || ibm390HexBinaryRepDouble
  //    }

  private lazy val textDouble = prod("textDouble", impliedRepresentation == Representation.Text) {
    standardTextDouble || zonedTextDouble
  }

  //  private lazy val ieeeBinaryRepDouble = prod("ieeeBinaryRepDouble",
  //    {
  //      val bfr = binaryFloatRep
  //      val res = bfr.isConstant &&
  //        BinaryFloatRep(bfr.constantAsString, this) == BinaryFloatRep.Ieee
  //      res
  //    }){
  //    lengthKind match {
  //      case LengthKind.Implicit => {
  //        if (byteOrder.isConstant) ByteOrder(byteOrder.constantAsString, this) match {
  //          case ByteOrder.BigEndian => BigEndianDoublePrim(this)
  //          case ByteOrder.LittleEndian => LittleEndianDoublePrim(this)
  //        }
  //        else Assert.notYetImplemented()
  //      }
  //      case _ => Assert.notYetImplemented()
  //    }}

  private lazy val ibm390HexBinaryRepDouble = prod("ibm390HexBinaryRepDouble",
    binaryFloatRep.isConstant &&
      binaryFloatRep.constantAsString == BinaryFloatRep.Ibm390Hex.toString) {
      subsetError("ibm390Hex not supported")
    }

  private lazy val standardTextDouble = prod("standardTextDouble",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextDoublePrim(this) }

  private lazy val zonedTextDouble = prod("zonedTextDouble",
    textNumberRep == TextNumberRep.Zoned) { SDE("Zoned not supported for float and double") }

  //  private lazy val binaryFloat = prod("binaryFloat", impliedRepresentation == Representation.Binary){
  //    ieeeBinaryRepFloat || ibm390HexBinaryRepFloat
  //  }

  private lazy val textFloat = prod("textFloat", impliedRepresentation == Representation.Text) {
    standardTextFloat || zonedTextFloat
  }

  //  private lazy val ieeeBinaryRepFloat = prod("ieeeBinaryRepFloat",
  //    {
  //      val bfr = binaryFloatRep
  //      val res = bfr.isConstant &&
  //        BinaryFloatRep(bfr.constantAsString, this) == BinaryFloatRep.Ieee
  //      res
  //    }) {
  //    lengthKind match {
  //      case LengthKind.Implicit => {
  //        if (byteOrder.isConstant) ByteOrder(byteOrder.constantAsString, this) match {
  //          case ByteOrder.BigEndian => BigEndianFloatPrim(this)
  //          case ByteOrder.LittleEndian => LittleEndianFloatPrim(this)
  //        }
  //        else Assert.notYetImplemented()
  //      }
  //      case _ => Assert.notYetImplemented()
  //    }}
  //
  //  private lazy val ibm390HexBinaryRepFloat = prod("ibm390HexBinaryRepFloat",
  //    binaryFloatRep.isConstant &&
  //      binaryFloatRep.constantAsString == BinaryFloatRep.Ibm390Hex.toString){
  //    subsetError("ibm390Hex not supported")
  //    }

  private lazy val standardTextFloat = prod("standardTextFloat",
    textNumberRep == TextNumberRep.Standard) { stringValue ~ ConvertTextFloatPrim(this) }

  private lazy val zonedTextFloat = prod("zonedTextFloat",
    textNumberRep == TextNumberRep.Zoned) { SDE("Zoned not supported for float and double") }

  private lazy val textDate = prod("textDate", impliedRepresentation == Representation.Text) {
    stringValue ~ ConvertTextDatePrim(this)
  }

  private lazy val textTime = prod("textTime", impliedRepresentation == Representation.Text) {
    stringValue ~ ConvertTextTimePrim(this)
  }

  private lazy val textDateTime = prod("textDateTime", impliedRepresentation == Representation.Text) {
    stringValue ~ ConvertTextDateTimePrim(this)
  }

  // shorthand
  final lazy val primType = {
    val res = typeDef.asInstanceOf[SimpleTypeBase].primitiveType
    res
  }

  //  private lazy val value = prod("value", isSimpleType){
  //    // TODO: Consider issues with matching a stopValue. Can't say isScalar here because
  //    // This gets used for array contents also.
  //    {
  //      primType.name match {
  //        case "string" => stringValue
  //        case _ => {
  //          val res = impliedRepresentation match {
  //            case Representation.Binary => binaryValue
  //            case Representation.Text => textValue
  //          }
  //          res
  //        }
  //      }
  //    }}

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
    BinaryFloatRep(binaryFloatRep.constantAsString, this)
  }

  private lazy val binary = {
    subset(lengthKind == LengthKind.Explicit, "Currently only lengthKind='explicit' is supported.")
    LengthKind(lengthKind.toString, this)
  }

  val bin = BinaryNumberRep.Binary // shorthands for table dispatch
  val ieee = BinaryFloatRep.Ieee
  type BO = java.nio.ByteOrder

  private lazy val zero = new BigInteger("0")
  private lazy val two = new BigInteger("2")
  private lazy val maximumUnsignedLong = two.pow(64).subtract(new BigInteger("1"))

  private lazy val binaryValue: Gram = {
    Assert.invariant(primType != PrimType.String)

    // We have to dispatch carefully here. We cannot force evaluation of properties 
    // that may not be necessary. E.g., float does not need property binaryNumberRep, so
    // if our dispatch table uses that, it will create a false dependency on the property
    // being defined. 
    // The DFDL spec has a section where it gives the precedence order of properties. 
    // This is in the spirit of that section.
    val res: Gram = primType match {

      //      case PrimType.HexBinary =>
      //        (primType, binary) match { // TODO: Only takes explicit length
      //          case (PrimType.HexBinary, b) => new BinaryNumberBase[Array[Byte]](this.length.constantAsLong) {
      //            def getNum(bp : Long, in : InStream, bo : BO) = {
      //              // FIXME: size constraints, overflow
      //              in.getByteArray(bp, bo, length.constantAsLong.asInstanceOf[Int])
      //            }
      //            override def getNum(num : Number) = null //FIXME
      //            protected override val GramName = "hexBinary"
      //            protected override val GramDescription = "Hex Binary"
      //            protected override def numFormat = NumberFormat.getIntegerInstance()
      //            protected override def isInt = true
      //          }
      //          case _ => Assert.impossibleCase()
      //        }

      case PrimType.Byte | PrimType.Short | PrimType.Int | PrimType.Long | PrimType.Integer => {
        Assert.invariant(binaryIntRep == bin)
        binaryNumberKnownLengthInBits match {
          case -1 => new SignedRuntimeLengthRuntimeByteOrderBinaryNumber(this)
          case _ => new SignedKnownLengthRuntimeByteOrderBinaryNumber(this, binaryNumberKnownLengthInBits)
        }
      }

      case PrimType.UnsignedByte | PrimType.UnsignedShort | PrimType.UnsignedInt | PrimType.UnsignedLong | PrimType.NonNegativeInteger => {
        Assert.invariant(binaryIntRep == bin)
        binaryNumberKnownLengthInBits match {
          case -1 => new UnsignedRuntimeLengthRuntimeByteOrderBinaryNumber(this)
          case _ => new UnsignedKnownLengthRuntimeByteOrderBinaryNumber(this, binaryNumberKnownLengthInBits)
        }
      }

      case PrimType.Double | PrimType.Float =>
        (primType, binaryNumberKnownLengthInBits, staticBinaryFloatRep) match {
          case (_, -1, BinaryFloatRep.Ieee) => SDE("Floating point binary numbers may not have runtime-specified lengths.")
          case (PrimType.Float, 32, BinaryFloatRep.Ieee) => new FloatKnownLengthRuntimeByteOrderBinaryNumber(this, 32)
          case (PrimType.Float, n, BinaryFloatRep.Ieee) => SDE("binary xs:float must be 32 bits. Length in bits was %s.", n)
          case (PrimType.Double, 64, BinaryFloatRep.Ieee) => new DoubleKnownLengthRuntimeByteOrderBinaryNumber(this, 64)
          case (PrimType.Double, n, BinaryFloatRep.Ieee) => SDE("binary xs:double must be 64 bits. Length in bits was %s.", n)
          case (_, _, floatRep) => subsetError("binaryFloatRep='%s' not supported. Only binaryFloatRep='ieee'", floatRep.toString)
        }

      case PrimType.Decimal => {
        Assert.invariant(binaryIntRep == bin)
        if (binaryDecimalVirtualPoint > DaffodilTunableParameters.maxBinaryDecimalVirtualPoint)
          SDE("Property binaryDecimalVirtualPoint %s is greater than limit %s", binaryDecimalVirtualPoint, DaffodilTunableParameters.maxBinaryDecimalVirtualPoint)
        if (binaryDecimalVirtualPoint < DaffodilTunableParameters.minBinaryDecimalVirtualPoint)
          SDE("Property binaryDecimalVirtualPoint %s is less than limit %s", binaryDecimalVirtualPoint, DaffodilTunableParameters.minBinaryDecimalVirtualPoint)
        new DecimalKnownLengthRuntimeByteOrderBinaryNumber(this, binaryNumberKnownLengthInBits)
      }
      //        (primType, staticBinaryFloatRep) match {
      //          case (PrimType.Double, ieee) => new BinaryNumber[Double](this, 64) {
      //            Assert.invariant(staticBinaryFloatRep == BinaryFloatRep.Ieee)
      //            def getNum(bp : Long, in : InStream, bo : BO) = in.getDouble(bp, bo)
      //            override def getNum(num : Number) = num.doubleValue
      //            protected override val GramName = "double"
      //            protected override val GramDescription = "Double"
      //            protected override def numFormat = NumberFormat.getNumberInstance() // .getScientificInstance() Note: scientific doesn't allow commas as grouping separators.
      //            protected override def isInt = false
      //          }
      //          case (PrimType.Float, ieee) => new BinaryNumber[Float](this, 32) {
      //            Assert.invariant(staticBinaryFloatRep == BinaryFloatRep.Ieee)
      //            def getNum(bp : Long, in : InStream, bo : BO) = in.getFloat(bp, bo)
      //            override def getNum(num : Number) = num.floatValue
      //            protected override val GramName = "float"
      //            protected override val GramDescription = "Float"
      //            protected override def numFormat = NumberFormat.getNumberInstance() // .getScientificInstance() Note: scientific doesn't allow commas as grouping separators.
      //            protected override def isInt = false
      //          }
      //          case (_, floatRep) => subsetError("binaryFloatRep='%s' not supported. Only binaryFloatRep='ieee'", floatRep.toString)
      //        }
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

  private lazy val emptyDefaulted = prod("emptyDefaulted",
    isDefaultable && emptyIsAnObservableConcept) {
      empty ~ TheDefaultValue(this)
    }

  private lazy val nilElementInitiator = prod("nilElementInitiator", hasInitiator) { Initiator(this) }
  private lazy val nilElementTerminator = prod("nilElementTerminator", hasTerminator) { Terminator(this) }

  private lazy val emptyElementInitiator = prod("emptyElementInitiator", NYI && hasEmptyValueInitiator) { ErrorGram }
  private lazy val emptyElementTerminator = prod("emptyElementTerminator", NYI && hasEmptyValueTerminator) { ErrorGram }

  private lazy val complexContent = prod("complexContent", isComplexType) {
    elementComplexType.mainGrammar
  }

  private lazy val nilLit = prod("nilLit",
    isNillable && nilKind == NilKind.LiteralValue) {
      if (hasDelimiters)
        DelimiterStackCombinatorElement(this, nilElementInitiator ~ nilLitContent ~ nilElementTerminator)
      else
        nilLitContent
    }

  private lazy val nilLitSpecifiedLength = prod("nilLitSpecifiedLength", isNillable && nilKind == NilKind.LiteralValue) {
    if (hasDelimiters)
      DelimiterStackCombinatorElement(this, nilElementInitiator ~ specifiedLength(nilLitContent) ~ nilElementTerminator)
    else specifiedLength(nilLitContent)
  }

  private lazy val nilLitContent = prod("nilLitContent",
    isNillable && nilKind == NilKind.LiteralValue) {
      // if (impliedRepresentation != Representation.Text) this.SDE("LiteralValue Nils require representation='text'.")
      lengthKind match {
        //          case LengthKind.Delimited => LiteralNilDelimitedOrEndOfData(this)
        case LengthKind.Delimited => LiteralNilDelimitedEndOfData(this)
        case LengthKind.Pattern => LiteralNilPattern(this)
        case LengthKind.Explicit => {
          lengthUnits match {
            case LengthUnits.Bits => notYetImplemented("nilKind='literalValue' with lengthKind='bits'")
            case LengthUnits.Bytes => LiteralNilExplicitLengthInBytes(this)
            case LengthUnits.Characters => LiteralNilExplicitLengthInChars(this)
          }
        }
        case LengthKind.Implicit => {
          schemaDefinitionUnless(impliedRepresentation != Representation.Text, "LiteralValue Nils with lengthKind='implicit' cannot have representation='text'.")
          val lengthInBytes = implicitBinaryLengthInBits / 8
          LiteralNilKnownLengthInBytes(this, lengthInBytes)
        }
        case LengthKind.Prefixed => notYetImplemented("lengthKind='prefixed'")
        case LengthKind.EndOfParent => notYetImplemented("lengthKind='endOfParent'")
      }
    }

  //  private lazy val leftPadding =  Prod("leftPadding", hasLeftPadding, LeftPadding(this))

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
    withDelimiterStack(nilLit || parsedNil | parsedValue)
  }

  private def specifiedLength(bodyArg: => Gram) = {
    lazy val body = bodyArg
    lengthKind match {
      case LengthKind.Pattern => new SpecifiedLengthPattern(this, body)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bits && isFixedLength => new SpecifiedLengthExplicitBitsFixed(this, body, fixedLength)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bits && !isFixedLength => new SpecifiedLengthExplicitBits(this, body)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bytes && isFixedLength => new SpecifiedLengthExplicitBytesFixed(this, body, fixedLength)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Bytes && !isFixedLength => new SpecifiedLengthExplicitBytes(this, body)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Characters && isFixedLength => new SpecifiedLengthExplicitCharactersFixed(this, body, fixedLength)
      case LengthKind.Explicit if lengthUnits == LengthUnits.Characters && !isFixedLength => new SpecifiedLengthExplicitCharacters(this, body)
      case _ => {
        // TODO: implement other specified length restrictions
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
    nilLitSpecifiedLength | complexContentSpecifiedLength
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
    leadingSkipRegion ~ alignmentFill ~ PrefixLength(this)
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
    lazy val sdp = scalarDefaultablePhysical
    (inputValueCalcOption, outputValueCalcOption) match {
      case (_: Found, _: Found) => SDE("Cannot have both dfdl:inputValueCalc and dfdl:outputValueCalc on the same element.")
      case (_: NotFound, _: NotFound) => sdp
      //
      // We really do have to share the sdp. Repeating grammar terms that have large
      // expansions twice in one production results in a tree-explosion. 
      //
      case _ => scalarDefaultableForParser(sdp) | scalarDefaultableForUnparser
    }
  }

  private def scalarDefaultableForParser(sdp: => Gram) = prod("scalarDefaultableForParser", forWhat = ForParser) {
    (inputValueCalcOption, outputValueCalcOption) match {
      case (_: NotFound, _: Found) => sdp // outputValueCalc element is just a regular physical element for parser
      case (_: Found, _: NotFound) => inputValueCalcElement
      case _ => Assert.impossibleCase()
    }
  }

  private def scalarDefaultableForUnparser = prod("scalarDefaultableForUnparser", forWhat = ForUnparser) {
    (inputValueCalcOption, outputValueCalcOption) match {
      case (_: NotFound, _: Found) => outputValueCalcElement
      // when unparsing, inputValueCalc elements don't contribute to the data. 
      // They may get referenced from outputValueCalc or other expressions so their
      // element values may need to be in the infoset
      case (_: Found, _: NotFound) => defaultableValue
      case _ => Assert.impossibleCase()
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
    isSimpleType && inputValueCalcOption.isInstanceOf[Found]) {
      // No framing surrounding inputValueCalc elements.
      new ElementCombinator(this, dfdlScopeBegin ~
        ValueCalc("inputValueCalc", self, inputValueCalcOption), dfdlScopeEnd)
    }

  private lazy val outputValueCalcElement = prod("outputValueCalcElement",
    isSimpleType && outputValueCalcOption.isInstanceOf[Found]) {
      new ElementCombinator(this, dfdlScopeBegin ~
        ValueCalc("outputValueCalc", self, outputValueCalcOption), dfdlScopeEnd)
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

