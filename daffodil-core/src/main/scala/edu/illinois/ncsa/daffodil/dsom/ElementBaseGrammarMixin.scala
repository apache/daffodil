package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.xml.Node
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.util._
import com.ibm.icu.text.NumberFormat
import java.math.BigInteger

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

trait ElementBaseGrammarMixin
  extends InitiatedTerminatedMixin
  with AlignedMixin
  with HasStatementsGrammarMixin { self: ElementBase =>
  // 
  // This silly redundancy where the variable name has to also be passed as a string,
  // is, by the way, a good reason Scala needs real Lisp-style macros, that can take an argument and
  // turn it into a type/class, object, def, or val/var name, as well as a string, etc. 
  // 

  lazy val parsedNil = Prod("parsedNil", this, NYI && isNillable && nilKind == NilKind.LogicalValue,
    nilElementInitiator ~ LogicalNilValue(this) ~ nilElementTerminator)

  lazy val parsedValue = {
    val res = Prod("parsedValue", this, initiatorRegion ~ allowedValue ~ terminatorRegion)
    res
  }

  def allowedValue: Prod // provided by LocalElementBase for array considerations, and GlobalElementDecl - scalar only

  //  lazy val explicitLengthBinary = Prod("explicitLengthBinary", this, !isFixedLength,
  //    lengthUnits match {
  //      case LengthUnits.Bytes => BinaryExplicitLengthInBytes(this)
  //      case LengthUnits.Characters => schemaDefinitionError("Binary data elements cannot have lengthUnits='Character'.")
  //      case LengthUnits.Bits => BinaryExplicitLengthInBits(this)
  //    })
  //
  //  lazy val binaryValueLength = binaryValueLength_.value
  //  lazy val binaryValueLength_ = LV {
  //    val res = Prod("BinaryValueLength", this, lengthKind match {
  //      case LengthKind.Explicit => explicitLengthBinary
  //      case LengthKind.Delimited => Assert.notYetImplemented() // Binary Data delimiters aren't supported TODO: Should we?
  //      case LengthKind.Pattern => schemaDefinitionError("Binary data elements cannot have lengthKind='Pattern'.")
  //      case LengthKind.Implicit => Assert.notYetImplemented() // TODO: Get size from xs:type
  //      case _ => Assert.notYetImplemented()
  //    })
  //    res
  //  }

  // Length is in bits, (size would be in bytes) (from DFDL Spec 12.3.3)
  lazy val implicitBinaryLengthInBits: Long = primType match {
    case PrimType.Byte | PrimType.UnsignedByte => 8
    case PrimType.Short | PrimType.UnsignedShort => 16
    case PrimType.Float | PrimType.Int | PrimType.UnsignedInt | PrimType.Boolean => 32
    case PrimType.Double | PrimType.Long | PrimType.UnsignedLong => 64
    case _ => schemaDefinitionError("Size of binary data '" + primType.name + "' cannot be determined implicitly.")
  }

  lazy val binaryNumberKnownLengthInBits: Long = lengthKind match {
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

  lazy val fixedLengthString = Prod("fixedLengthString", this, isFixedLength,
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
    })

  lazy val fixedLengthHexBinary = Prod("fixedLengthHexBinary", this, isFixedLength,
    lengthUnits match {
      case LengthUnits.Bytes => HexBinaryFixedLengthInBytes(this, fixedLength)
      case LengthUnits.Bits => SDE("lengthUnits='bits' is not valid for hexBinary.")
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    })

  lazy val implicitLengthString = Prod("implicitLengthString", this, hasSpecifiedLength, {
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
  })

  lazy val implicitLengthHexBinary = Prod("implicitLengthHexBinary", this, hasSpecifiedLength, {
    val maxLengthLong = maxLength.longValueExact
    lengthUnits match {
      case LengthUnits.Bytes => HexBinaryFixedLengthInBytes(this, maxLengthLong)
      case LengthUnits.Bits => SDE("lengthUnits='bits' is not valid for hexBinary.")
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    }
  })

  lazy val variableLengthString = Prod("variableLengthString", this, !isFixedLength,
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
    })

  lazy val variableLengthHexBinary = Prod("variableLengthHexBinary", this, !isFixedLength,
    lengthUnits match {
      case LengthUnits.Bytes => HexBinaryVariableLengthInBytes(this)
      case LengthUnits.Bits => SDE("lengthUnits='bits' is not valid for hexBinary.")
      case LengthUnits.Characters => SDE("lengthUnits='characters' is not valid for hexBinary.")
    })

  //lazy val stringDelimitedEndOfData = Prod("stringDelimitedEndOfData", this, StringDelimitedEndOfData(this))
  lazy val stringDelimitedEndOfData = Prod("stringDelimitedEndOfData", this, StringDelimitedEndOfData(this))
  lazy val stringPatternMatched = Prod("stringPatternMatched", this, StringPatternMatched(this))

  lazy val stringValue = stringValue_.value
  private val stringValue_ = LV('stringValue) {
    val res = Prod("stringValue", this, lengthKind match {
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
    })
    res
  }

  lazy val hexBinaryDelimitedEndOfData = Prod("hexBinaryDelimitedEndOfData", this, HexBinaryDelimitedEndOfData(this))

  lazy val hexBinaryValue = hexBinaryValue_.value
  private val hexBinaryValue_ = LV('hexBinaryValue) {
    val res = Prod("hexBinaryValue", this, lengthKind match {
      case LengthKind.Explicit if isFixedLength => fixedLengthHexBinary
      case LengthKind.Explicit => variableLengthHexBinary
      case LengthKind.Delimited => hexBinaryDelimitedEndOfData
      case LengthKind.Pattern => SDE("lengthKind Pattern is not allowed for hexBinary.")
      case LengthKind.Implicit => implicitLengthHexBinary
      case _ => SDE("Unimplemented lengthKind %s", lengthKind)
    })
    res
  }

  //  lazy val binaryByte = Prod("binaryByte", this, impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryShort = Prod("binaryShort", this, impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryLong = Prod("binaryLong", this, impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryInteger = Prod("binaryInteger", this, impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedInt = Prod("binaryInt", this, impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedByte = Prod("binaryByte", this, impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedShort = Prod("binaryShort", this, impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedLong = Prod("binaryLong", this, impliedRepresentation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)

  //  lazy val regularBinaryRepInt = Prod("regularBinaryRepInt", this,
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

  //  lazy val bcdInt = Prod("bcdInt", this,
  //    binaryNumberRep == BinaryNumberRep.Bcd, BCDIntPrim(this))
  //  lazy val packedInt = Prod("packedInt", this,
  //    binaryNumberRep == BinaryNumberRep.Packed, PackedIntPrim(this))

  // TODO: Handle the zonedTextXXX possibilities
  lazy val textInt = Prod("textInt", this, impliedRepresentation == Representation.Text,
    standardTextInt | zonedTextInt)

  lazy val textByte = Prod("textByte", this, impliedRepresentation == Representation.Text,
    standardTextByte | zonedTextInt)

  lazy val textShort = Prod("textShort", this, impliedRepresentation == Representation.Text,
    standardTextShort | zonedTextInt)

  lazy val textLong = Prod("textLong", this, impliedRepresentation == Representation.Text,
    standardTextLong | zonedTextInt)

  lazy val textInteger = Prod("textInteger", this, impliedRepresentation == Representation.Text,
    standardTextInteger | zonedTextInt)

  lazy val textDecimal = Prod("textDecimal", this, impliedRepresentation == Representation.Text,
    standardTextDecimal | zonedTextInt)

  lazy val textNonNegativeInteger = Prod("textNonNegativeInteger", this, impliedRepresentation == Representation.Text,
    standardTextNonNegativeInteger | zonedTextInt)

  lazy val textUnsignedInt = Prod("textUnsignedInt", this, impliedRepresentation == Representation.Text,
    standardTextUnsignedInt | zonedTextInt)

  lazy val textUnsignedByte = Prod("textUnsignedByte", this, impliedRepresentation == Representation.Text,
    standardTextUnsignedByte | zonedTextInt)

  lazy val textUnsignedShort = Prod("textUnsignedShort", this, impliedRepresentation == Representation.Text,
    standardTextUnsignedShort | zonedTextInt)

  lazy val textUnsignedLong = Prod("textUnsignedLong", this, impliedRepresentation == Representation.Text,
    standardTextUnsignedLong | zonedTextInt)

  //
  // We could now break it down by lengthKind, and have specialized primitives
  // depending on the length kind.
  // 
  lazy val standardTextInteger = Prod("standardTextInteger", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextIntegerPrim(this))
  lazy val standardTextDecimal = Prod("standardTextDecimal", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextDecimalPrim(this))
  lazy val standardTextNonNegativeInteger = Prod("standardTextNonNegativeInteger", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextNonNegativeIntegerPrim(this))
  lazy val standardTextLong = Prod("standardTextLong", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextLongPrim(this))
  lazy val standardTextInt = Prod("standardTextInt", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextIntPrim(this))
  lazy val standardTextShort = Prod("standardTextShort", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextShortPrim(this))
  lazy val standardTextByte = Prod("standardTextByte", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextBytePrim(this))
  lazy val standardTextUnsignedLong = Prod("standardTextUnsignedLong", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextUnsignedLongPrim(this))
  lazy val standardTextUnsignedInt = Prod("standardTextUnsignedInt", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextUnsignedIntPrim(this))
  lazy val standardTextUnsignedShort = Prod("standardTextUnsignedShort", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextUnsignedShortPrim(this))
  lazy val standardTextUnsignedByte = Prod("standardTextUnsignedByte", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextUnsignedBytePrim(this))
  lazy val zonedTextInt = Prod("zonedTextInt", this,
    textNumberRep == TextNumberRep.Zoned, ZonedTextIntPrim(this))

  //  lazy val binaryDouble = Prod("binaryDouble", this, impliedRepresentation == Representation.Binary,
  //    ieeeBinaryRepDouble | ibm390HexBinaryRepDouble)

  lazy val textDouble = Prod("textDouble", this, impliedRepresentation == Representation.Text,
    standardTextDouble | zonedTextDouble)

  //  lazy val ieeeBinaryRepDouble = Prod("ieeeBinaryRepDouble", this,
  //    {
  //      val bfr = binaryFloatRep
  //      val res = bfr.isConstant &&
  //        BinaryFloatRep(bfr.constantAsString, this) == BinaryFloatRep.Ieee
  //      res
  //    },
  //    lengthKind match {
  //      case LengthKind.Implicit => {
  //        if (byteOrder.isConstant) ByteOrder(byteOrder.constantAsString, this) match {
  //          case ByteOrder.BigEndian => BigEndianDoublePrim(this)
  //          case ByteOrder.LittleEndian => LittleEndianDoublePrim(this)
  //        }
  //        else Assert.notYetImplemented()
  //      }
  //      case _ => Assert.notYetImplemented()
  //    })

  lazy val ibm390HexBinaryRepDouble = Prod("ibm390HexBinaryRepDouble", this,
    binaryFloatRep.isConstant &&
      binaryFloatRep.constantAsString == BinaryFloatRep.Ibm390Hex.toString,
    subsetError("ibm390Hex not supported"))

  lazy val standardTextDouble = Prod("standardTextDouble", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextDoublePrim(this))

  lazy val zonedTextDouble = Prod("zonedTextDouble", this,
    textNumberRep == TextNumberRep.Zoned, SDE("Zoned not supported for float and double"))

  //  lazy val binaryFloat = Prod("binaryFloat", this, impliedRepresentation == Representation.Binary,
  //    ieeeBinaryRepFloat | ibm390HexBinaryRepFloat)

  lazy val textFloat = Prod("textFloat", this, impliedRepresentation == Representation.Text,
    standardTextFloat | zonedTextFloat)

  //  lazy val ieeeBinaryRepFloat = Prod("ieeeBinaryRepFloat", this,
  //    {
  //      val bfr = binaryFloatRep
  //      val res = bfr.isConstant &&
  //        BinaryFloatRep(bfr.constantAsString, this) == BinaryFloatRep.Ieee
  //      res
  //    },
  //    lengthKind match {
  //      case LengthKind.Implicit => {
  //        if (byteOrder.isConstant) ByteOrder(byteOrder.constantAsString, this) match {
  //          case ByteOrder.BigEndian => BigEndianFloatPrim(this)
  //          case ByteOrder.LittleEndian => LittleEndianFloatPrim(this)
  //        }
  //        else Assert.notYetImplemented()
  //      }
  //      case _ => Assert.notYetImplemented()
  //    })
  //
  //  lazy val ibm390HexBinaryRepFloat = Prod("ibm390HexBinaryRepFloat", this,
  //    binaryFloatRep.isConstant &&
  //      binaryFloatRep.constantAsString == BinaryFloatRep.Ibm390Hex.toString,
  //    subsetError("ibm390Hex not supported"))

  lazy val standardTextFloat = Prod("standardTextFloat", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextFloatPrim(this))

  lazy val zonedTextFloat = Prod("zonedTextFloat", this,
    textNumberRep == TextNumberRep.Zoned, SDE("Zoned not supported for float and double"))

  lazy val textDate = Prod("textDate", this, impliedRepresentation == Representation.Text,
    stringValue ~ ConvertTextDatePrim(this))
  lazy val textTime = Prod("textTime", this, impliedRepresentation == Representation.Text,
    stringValue ~ ConvertTextTimePrim(this))
  lazy val textDateTime = Prod("textDateTime", this, impliedRepresentation == Representation.Text,
    stringValue ~ ConvertTextDateTimePrim(this))

  // shorthand
  lazy val primType = {
    val res = typeDef.asInstanceOf[SimpleTypeBase].primitiveType
    res
  }

  //  lazy val value = Prod("value", this, isSimpleType,
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
  //    })

  lazy val value = Prod("value", this, isSimpleType,
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
    })

  // This is the right name that the DFDL property should have had!
  lazy val binaryIntRep = {
    subset(binaryNumberRep == BinaryNumberRep.Binary, "binaryNumberRep='%s' is unsupported. Only 'binary' is supported.", binaryNumberRep.toString)
    binaryNumberRep
  }

  lazy val staticBinaryFloatRep = {
    subset(binaryFloatRep.isConstant, "Dynamic binaryFloatRep is not supported.")
    BinaryFloatRep(binaryFloatRep.constantAsString, this)
  }

  lazy val binary = {
    subset(lengthKind == LengthKind.Explicit, "Currently only lengthKind='explicit' is supported.")
    LengthKind(lengthKind.toString(), this)
  }

  val bin = BinaryNumberRep.Binary // shorthands for table dispatch
  val ieee = BinaryFloatRep.Ieee
  type BO = java.nio.ByteOrder

  lazy val zero = new BigInteger("0")
  lazy val two = new BigInteger("2")
  lazy val maximumUnsignedLong = two.pow(64).subtract(new BigInteger("1"))

  lazy val binaryValue: Gram = {
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
      //          case (PrimType.HexBinary, b) => new BinaryNumberBase[Array[Byte]](this, this.length.constantAsLong) {
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

  lazy val textValue: Gram = {
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

  lazy val empty = Prod("empty", this, NYI && emptyIsAnObservableConcept, emptyRepresentation)

  lazy val emptyRepresentation = Prod("emptyRepresentation", this,
    simpleOrNonImplicitComplexEmpty | complexImplicitEmpty)

  lazy val simpleOrNonImplicitComplexEmpty = Prod("simpleOrNonImplicitComplexEmpty", this,
    NYI && isSimpleType || isComplexType && lengthKind != LengthKind.Implicit,
    emptyElementInitiator ~ emptyElementTerminator)

  /**
   * This is about the case where we take an empty, parse a complex type recursively from it
   * and potentially succeed.
   */
  lazy val complexImplicitEmpty = Prod("complexImplicitEmpty", this, NYI &&
    isComplexType && lengthKind == LengthKind.Implicit,
    SaveInputStream(this) ~ SetEmptyInputStream(this) ~ elementComplexType.mainGrammar ~
      RestoreInputStream(this) ~ emptyElementTerminator)

  lazy val emptyDefaulted = Prod("emptyDefaulted", this,
    isDefaultable && emptyIsAnObservableConcept,
    empty ~ TheDefaultValue(this))

  lazy val nilElementInitiator = Prod("nilElementInitiator", this, hasNilValueInitiator,
    if (initiator.isConstant) StaticInitiator(this) else DynamicInitiator(this))
  lazy val nilElementTerminator = Prod("nilElementTerminator", this, hasNilValueTerminator,
    if (terminator.isConstant) StaticTerminator(this) else DynamicTerminator(this))

  lazy val emptyElementInitiator = Prod("emptyElementInitiator", this, NYI && hasEmptyValueInitiator, EmptyGram)
  lazy val emptyElementTerminator = Prod("emptyElementTerminator", this, NYI && hasEmptyValueTerminator, EmptyGram)

  lazy val complexContent = Prod("complexContent", this, isComplexType,
    elementComplexType.mainGrammar)

  lazy val nilLit = {
    Prod("nilLit", this,
      isNillable && nilKind == NilKind.LiteralValue,
      nilElementInitiator ~ nilLitContent ~ nilElementTerminator)
  }

  lazy val nilLitSpecifiedLength = {
    Prod("nilLitSpecifiedLength", this, isNillable && nilKind == NilKind.LiteralValue, {
      nilElementInitiator ~ specifiedLength(nilLitContent) ~ nilElementTerminator
    })
  }

  lazy val nilLitContent = {
    Prod("nilLitContent", this,
      isNillable && nilKind == NilKind.LiteralValue,
      {
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
      })
  }

  //  lazy val leftPadding =  Prod("leftPadding", this, hasLeftPadding, LeftPadding(this))

  lazy val scalarDefaultableSimpleContent = {
    val res = Prod("scalarDefaultableSimpleContent", this,
      isSimpleType, nilLit | emptyDefaulted | parsedNil | parsedValue)
    res
  }

  lazy val scalarNonDefaultSimpleContent = {
    val res = Prod("scalarNonDefaultSimpleContent", this,
      isSimpleType, nilLit | parsedNil | parsedValue)
    res
  }

  def specifiedLength(body: => Gram) = {
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

  lazy val complexContentSpecifiedLength = Prod("complexContentSpecifiedLength", this, isComplexType,
    initiatorRegion ~ specifiedLength(complexContent) ~ terminatorRegion)

  lazy val scalarComplexContent = Prod("scalarComplexContent", this, isComplexType,
    nilLitSpecifiedLength | complexContentSpecifiedLength)

  // Note: there is no such thing as defaultable complex content because you can't have a 
  // default value for a complex type element.
  lazy val scalarDefaultableContent = Prod("scalarDefaultableContent", this, scalarDefaultableSimpleContent | scalarComplexContent)

  lazy val scalarNonDefaultContent = Prod("scalarNonDefaultContent", this, scalarNonDefaultSimpleContent | scalarComplexContent)

  /**
   * the element left framing does not include the initiator nor the element right framing the terminator
   */
  lazy val elementLeftFraming = Prod("elementLeftFraming", this,
    leadingSkipRegion ~ alignmentFill ~ PrefixLength(this))

  lazy val elementRightFraming = Prod("elementRightFraming", this, trailingSkipRegion)

  //  lazy val scalarNonDefault = Prod("scalarNonDefault", this,
  //    dfdlElementBegin ~ elementLeftFraming ~ dfdlScopeBegin ~
  //      scalarNonDefaultContent ~ elementRightFraming ~ dfdlStatementEvaluations ~ dfdlScopeEnd ~ dfdlElementEnd)

  //  lazy val scalarNonDefaultPhysical = Prod("scalarNonDefault", this,
  //    StmtEval(this, dfdlElementBegin ~ elementLeftFraming ~ dfdlScopeBegin ~
  //      scalarNonDefaultContent) ~ elementRightFraming ~ dfdlScopeEnd ~ dfdlElementEnd)

  lazy val scalarNonDefaultPhysical = Prod("scalarNonDefault", this,
    if (this.isParentUnorderedSequence)
      new ChoiceElementCombinator(this, elementLeftFraming ~ dfdlScopeBegin ~
      scalarNonDefaultContent, elementRightFraming ~ dfdlScopeEnd)
    else
      new ElementCombinator(this, elementLeftFraming ~ dfdlScopeBegin ~
        scalarNonDefaultContent, elementRightFraming ~ dfdlScopeEnd))

  //  def scalarDefaultable: Prod
  //
  //  def scalarNonDefault: Prod
  lazy val scalarDefaultable = Prod("scalarDefaultable", this,
    inputValueCalcOption match {
      case _: NotFound => scalarDefaultablePhysical
      case _: Found => inputValueCalcElement
    })

  lazy val scalarNonDefault = Prod("scalarNonDefault", this,
    inputValueCalcOption match {
      case _: NotFound => scalarNonDefaultPhysical
      case _: Found => inputValueCalcElement
    })

  //  lazy val inputValueCalcElement = Prod("inputValueCalcElement", this,
  //    isSimpleType && inputValueCalcOption.isInstanceOf[Found],
  //    StmtEval(this, dfdlElementBegin ~ dfdlScopeBegin ~
  //      InputValueCalc(self)) ~ dfdlScopeEnd ~ dfdlElementEnd)

  lazy val inputValueCalcElement = Prod("inputValueCalcElement", this,
    isSimpleType && inputValueCalcOption.isInstanceOf[Found],
    // No framing surrounding inputValueCalc elements.
    new ElementCombinator(this, dfdlScopeBegin ~
      InputValueCalc(self), dfdlScopeEnd))

  lazy val scalarDefaultablePhysical = Prod("scalarDefaultablePhysical", this,
    if (this.isParentUnorderedSequence)
      new ChoiceElementCombinator(this, elementLeftFraming ~ dfdlScopeBegin ~
      scalarDefaultableContent, elementRightFraming ~ dfdlScopeEnd)
    else
      new ElementCombinator(this, elementLeftFraming ~ dfdlScopeBegin ~
        scalarDefaultableContent, elementRightFraming ~ dfdlScopeEnd))

}

