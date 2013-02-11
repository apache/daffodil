package daffodil.dsom

import scala.xml.Node
import daffodil.exceptions.Assert
import daffodil.grammar._
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
import daffodil.dsom.OOLAG._
import daffodil.util._
import daffodil.util.Misc.bytes2Hex
import com.ibm.icu.text.NumberFormat
import daffodil.processors._
import java.math.BigInteger

trait InitiatedTerminatedMixin
  extends AnnotatedMixin
  with DelimitedRuntimeValuedPropertiesMixin { self: Term =>

  lazy val parentSaysInitiatedContent = {
    val parentSays = self.immediatelyEnclosingModelGroup match {
      case Some(s) if (s.initiatedContent == YesNo.Yes) => true
      case _ => false
    }
    parentSays
  }

  lazy val hasInitiator = {
    val hasOne = initiator.isKnownNonEmpty
    if (parentSaysInitiatedContent)
      schemaDefinition(hasOne, "Enclosing group has initiatedContent='yes', but initiator is not defined.")
    hasOne
  }

  lazy val hasTerminator = terminator.isKnownNonEmpty

  lazy val initiatorDiscriminator = Prod("initiatorDiscriminator", this, parentSaysInitiatedContent, InitiatedContent(this))

  lazy val initiatorRegion = Prod("initiatorRegion", this, hasInitiator, initiatorItself ~ initiatorDiscriminator)
  lazy val initiatorItself = {
    if (initiator.isConstant) StaticInitiator(this)
    else DynamicInitiator(this)
  }

  lazy val terminatorRegion = Prod("terminatorRegion", this, hasTerminator,
    if (terminator.isConstant) StaticTerminator(this)
    else DynamicTerminator(this))

  lazy val escapeScheme: Option[DFDLEscapeScheme] = {
    val er = getPropertyOption("escapeSchemeRef")
    er match {
      case None => None
      case Some(qName) => {

        if (qName.length() == 0) {
          None
        } else {
          val (nsURI, name) = formatAnnotation.resolveQName(qName)
          val defES = schema.schemaSet.getDefineEscapeScheme(nsURI, name)
          defES match {
            case None => SDE("Define Escape Scheme Not Found")
            case Some(es) => Some(es.escapeScheme)
          }
        }
      }
    }
  }
}

trait AlignedMixin { self: Term =>
  lazy val leadingSkipRegion = Prod("leadingSkipRegion", this, LeadingSkipRegion(this))
  lazy val trailingSkipRegion = Prod("trailingSkipRegion", this, TrailingSkipRegion(this))
  lazy val alignmentFill = Prod("alignmentFill", this, AlignmentFill(this))
}

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
  lazy val implicitBinaryLengthInBits: Long = primType.myPrimitiveType match {
    case PrimType.Byte | PrimType.UByte => 8
    case PrimType.Short | PrimType.UShort => 16
    case PrimType.Float | PrimType.Int | PrimType.UInt | PrimType.Boolean => 32
    case PrimType.Double | PrimType.Long | PrimType.ULong => 64
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
      case (LengthUnits.Bytes, true) => StringFixedLengthInBytes(this, fixedLength) // TODO: make sure it divides evenly.
      //case (LengthUnits.Bytes, true) => StringFixedLengthInBytes(this, fixedLength / knownEncodingWidth) // TODO: make sure it divides evenly.
      case (LengthUnits.Bytes, false) => StringFixedLengthInBytesVariableWidthCharacters(this, fixedLength)
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
          StringFixedLengthInBytes(this, lengthInBytes)
        else
          StringFixedLengthInBytes(this, lengthInBytes + 1) // 1 more for fragment byte
      }
      //
      // The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      // 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      case (LengthUnits.Characters, false) => StringFixedLengthInVariableWidthCharacters(this, fixedLength)
      case (LengthUnits.Bits, _) => Assert.notYetImplemented()
    })

  lazy val implicitLengthString = Prod("implicitLengthString", this, hasSpecifiedLength,
    (lengthUnits, knownEncodingIsFixedWidth) match {
      case (LengthUnits.Bytes, true) => StringFixedLengthInBytes(this, facetMaxLength) // TODO: make sure it divides evenly.
      //case (LengthUnits.Bytes, true) => StringFixedLengthInBytes(this, fixedLength / knownEncodingWidth) // TODO: make sure it divides evenly.
      case (LengthUnits.Bytes, false) => StringFixedLengthInBytesVariableWidthCharacters(this, facetMaxLength)
      case (LengthUnits.Characters, true) => {
        //
        // we deal with the fact that some encodings have characters taking up smaller than 
        // a full byte. E.g., encoding='US-ASCII-7-bit-packed' are 7-bits packed with no unused
        // bits
        //
        val lengthInBits = facetMaxLength * knownEncodingWidthInBits
        val lengthInBytes = lengthInBits / 8
        val hasWholeBytesOnly = (lengthInBits % 8) == 0
        if (hasWholeBytesOnly)
          StringFixedLengthInBytes(this, lengthInBytes)
        else
          StringFixedLengthInBytes(this, lengthInBytes + 1) // 1 more for fragment byte
      }
      //
      // The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      // 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      case (LengthUnits.Characters, false) => StringFixedLengthInVariableWidthCharacters(this, facetMaxLength)
      case (LengthUnits.Bits, _) => Assert.notYetImplemented()
    })

  lazy val explicitLengthString = Prod("explicitLengthString", this, !isFixedLength,
    (lengthUnits, knownEncodingIsFixedWidth) match {
      case (LengthUnits.Bytes, true) => StringExplicitLengthInBytes(this)
      case (LengthUnits.Bytes, false) => Assert.notYetImplemented() // StringExplicitLengthInBytesVariableWidthCharacters(this)
      case (LengthUnits.Characters, true) => Assert.notYetImplemented() //StringExplicitLengthInBytes(this)
      // The string may be "fixed" length, but a variable-width charset like utf-8 means that N characters can take anywhere from N to 
      // 4*N bytes. So it's not really fixed width. We'll have to parse the string to determine the actual length.
      case (LengthUnits.Characters, false) => Assert.notYetImplemented() //StringExplicitLengthInVariableWidthCharacters(this)
      case (LengthUnits.Bits, _) => Assert.notYetImplemented()
    })

  lazy val stringDelimitedEndOfData = Prod("stringDelimitedEndOfData", this, StringDelimitedEndOfData(this))
  lazy val stringPatternMatched = Prod("stringPatternMatched", this, StringPatternMatched(this))

  lazy val stringValue = stringValue_.value
  lazy val stringValue_ = LV('stringValue) {
    val res = Prod("stringValue", this, lengthKind match {
      case LengthKind.Explicit if isFixedLength => fixedLengthString
      case LengthKind.Explicit => explicitLengthString
      case LengthKind.Delimited => stringDelimitedEndOfData
      case LengthKind.Pattern => stringPatternMatched
      case LengthKind.Implicit => implicitLengthString
      case _ => SDE("Unimplemented lengthKind %s", lengthKind)
    })
    res
  }

  //  lazy val binaryByte = Prod("binaryByte", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryShort = Prod("binaryShort", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryLong = Prod("binaryLong", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryInteger = Prod("binaryInteger", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedInt = Prod("binaryInt", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedByte = Prod("binaryByte", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedShort = Prod("binaryShort", this, representation == Representation.Binary,
  //    regularBinaryRepInt | bcdInt | packedInt)
  //
  //  lazy val binaryUnsignedLong = Prod("binaryLong", this, representation == Representation.Binary,
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
  lazy val textInt = Prod("textInt", this, representation == Representation.Text,
    standardTextInt | zonedTextInt)

  lazy val textByte = Prod("textByte", this, representation == Representation.Text,
    standardTextByte | zonedTextInt)

  lazy val textShort = Prod("textShort", this, representation == Representation.Text,
    standardTextShort | zonedTextInt)

  lazy val textLong = Prod("textLong", this, representation == Representation.Text,
    standardTextLong | zonedTextInt)

  lazy val textInteger = Prod("textInteger", this, representation == Representation.Text,
    standardTextInteger | zonedTextInt)

  lazy val textUnsignedInt = Prod("textUnsignedInt", this, representation == Representation.Text,
    standardTextUnsignedInt | zonedTextInt)

  lazy val textUnsignedByte = Prod("textUnsignedByte", this, representation == Representation.Text,
    standardTextUnsignedByte | zonedTextInt)

  lazy val textUnsignedShort = Prod("textUnsignedShort", this, representation == Representation.Text,
    standardTextUnsignedShort | zonedTextInt)

  lazy val textUnsignedLong = Prod("textUnsignedLong", this, representation == Representation.Text,
    standardTextUnsignedLong | zonedTextInt)

  //
  // We could now break it down by lengthKind, and have specialized primitives
  // depending on the length kind.
  // 
  lazy val standardTextInteger = Prod("standardTextInteger", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextIntegerPrim(this))
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

  //  lazy val binaryDouble = Prod("binaryDouble", this, representation == Representation.Binary,
  //    ieeeBinaryRepDouble | ibm390HexBinaryRepDouble)

  lazy val textDouble = Prod("textDouble", this, representation == Representation.Text,
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
    textNumberRep == TextNumberRep.Zoned, subsetError("Zoned not supported for float and double"))

  //  lazy val binaryFloat = Prod("binaryFloat", this, representation == Representation.Binary,
  //    ieeeBinaryRepFloat | ibm390HexBinaryRepFloat)

  lazy val textFloat = Prod("textFloat", this, representation == Representation.Text,
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
    textNumberRep == TextNumberRep.Zoned, subsetError("Zoned not supported for float and double"))

  lazy val textDate = Prod("textDate", this, representation == Representation.Text,
    standardTextDate | zonedTextDate)
  lazy val textTime = Prod("textTime", this, representation == Representation.Text,
    standardTextTime | zonedTextTime)
  lazy val textDateTime = Prod("textDateTime", this, representation == Representation.Text,
    standardTextDateTime | zonedTextDateTime)

  lazy val standardTextDate = Prod("standardTextDate", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextDatePrim(this))
  lazy val standardTextTime = Prod("standardTextTime", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextTimePrim(this))
  lazy val standardTextDateTime = Prod("standardTextDateTime", this,
    textNumberRep == TextNumberRep.Standard, stringValue ~ ConvertTextDateTimePrim(this))

  lazy val zonedTextDate = Prod("zonedTextDate", this,
    textNumberRep == TextNumberRep.Zoned, subsetError("Zoned not supported for date"))
  lazy val zonedTextTime = Prod("zonedTextDate", this,
    textNumberRep == TextNumberRep.Zoned, subsetError("Zoned not supported for time"))
  lazy val zonedTextDateTime = Prod("zonedTextDate", this,
    textNumberRep == TextNumberRep.Zoned, subsetError("Zoned not supported for dateTime"))

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
  //          val res = representation match {
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
      primType.myPrimitiveType match {
        case PrimType.String => stringValue
        case _ => {
          val res = representation match {
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
    Assert.invariant(primType.myPrimitiveType != PrimType.String)

    subset(byteOrder.isConstant, "Dynamic byte order is not currently supported.")

    // We have to dispatch carefully here. We cannot force evaluation of properties 
    // that may not be necessary. E.g., float does not need property binaryNumberRep, so
    // if our dispatch table uses that, it will create a false dependency on the property
    // being defined. 
    // The DFDL spec has a section where it gives the precedence order of properties. 
    // This is in the spirit of that section.
    val res: Gram = primType.myPrimitiveType match {

      //      case PrimType.HexBinary =>
      //        (primType.myPrimitiveType, binary) match { // TODO: Only takes explicit length
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

      case PrimType.UByte | PrimType.UShort | PrimType.UInt | PrimType.ULong => {
        Assert.invariant(binaryIntRep == bin)
        binaryNumberKnownLengthInBits match {
          case -1 => new UnsignedRuntimeLengthRuntimeByteOrderBinaryNumber(this)
          case _ => new UnsignedKnownLengthRuntimeByteOrderBinaryNumber(this, binaryNumberKnownLengthInBits)
        }
      }

      case PrimType.Double | PrimType.Float =>
        (primType.myPrimitiveType, binaryNumberKnownLengthInBits, staticBinaryFloatRep) match {
          case (_, -1, BinaryFloatRep.Ieee) => SDE("Floating point binary numbers may not have runtime-specified lengths.")
          case (PrimType.Float, 32, BinaryFloatRep.Ieee) => new FloatKnownLengthRuntimeByteOrderBinaryNumber(this, 32)
          case (PrimType.Float, n, BinaryFloatRep.Ieee) => SDE("binary xs:float must be 32 bits. Length in bits was %s.", n)
          case (PrimType.Double, 64, BinaryFloatRep.Ieee) => new DoubleKnownLengthRuntimeByteOrderBinaryNumber(this, 64)
          case (PrimType.Double, n, BinaryFloatRep.Ieee) => SDE("binary xs:double must be 64 bits. Length in bits was %s.", n)
          case (_, _, floatRep) => subsetError("binaryFloatRep='%s' not supported. Only binaryFloatRep='ieee'", floatRep.toString)
        }

      //        (primType.myPrimitiveType, staticBinaryFloatRep) match {
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
      case _ => schemaDefinitionError("Unrecognized primitive type: " + primType.name)
    }
    res
  }

  lazy val textValue: Gram = {
    Assert.invariant(primType.myPrimitiveType != PrimType.String)
    val res = primType.myPrimitiveType match {

      case PrimType.Int => textInt
      case PrimType.Byte => textByte
      case PrimType.Short => textShort
      case PrimType.Long => textLong
      case PrimType.Integer => textInteger
      case PrimType.UInt => textUnsignedInt
      case PrimType.UByte => textUnsignedByte
      case PrimType.UShort => textUnsignedShort
      case PrimType.ULong => textUnsignedLong
      case PrimType.Double => textDouble
      case PrimType.Float => textFloat
      case PrimType.HexBinary => Assert.notYetImplemented("textValue: hexBinary")
      case PrimType.Boolean => Assert.notYetImplemented("textValue: boolean")
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
    initiatorRegion ~ elementComplexType.mainGrammar ~ terminatorRegion)

  lazy val nilLit = {
    Prod("nilLit", this,
      isNillable && nilKind == NilKind.LiteralValue,
      nilElementInitiator ~ {
        // if (representation != Representation.Text) this.SDE("LiteralValue Nils require representation='text'.")
        lengthKind match {
          case LengthKind.Delimited => LiteralNilDelimitedOrEndOfData(this)
          case LengthKind.Pattern => LiteralNilPattern(this)
          case LengthKind.Explicit => {
            lengthUnits match {
              case LengthUnits.Bits => Assert.notYetImplemented()
              case LengthUnits.Bytes => LiteralNilExplicitLengthInBytes(this)
              case LengthUnits.Characters => LiteralNilExplicitLengthInChars(this)
            }
          }
          case LengthKind.Implicit => {
            schemaDefinition(representation != Representation.Text, "LiteralValue Nils with lengthKind='implicit' cannot have representation='text'.")
            val lengthInBytes = implicitBinaryLengthInBits / 8
            LiteralNilKnownLengthInBytes(this, lengthInBytes)
          }
          case LengthKind.Prefixed => Assert.notYetImplemented()
          case LengthKind.EndOfParent => Assert.notYetImplemented()
        }
      } ~ nilElementTerminator)
  }

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

  lazy val scalarComplexContent = Prod("scalarComplexContent", this, isComplexType, specifiedLength(nilLit | complexContent))

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

  lazy val dfdlElementBegin = Prod("dfdlElementBegin", this, ElementBegin(this))

  lazy val dfdlElementEnd = Prod("dfdlElementEnd", this, {
    if (isRepresented) ElementEnd(this)
    else ElementEndNoRep(this)
  })

  //  lazy val scalarNonDefault = Prod("scalarNonDefault", this,
  //    dfdlElementBegin ~ elementLeftFraming ~ dfdlScopeBegin ~
  //      scalarNonDefaultContent ~ elementRightFraming ~ dfdlStatementEvaluations ~ dfdlScopeEnd ~ dfdlElementEnd)

  lazy val scalarNonDefaultPhysical = Prod("scalarNonDefault", this,
    StmtEval(this, dfdlElementBegin ~ elementLeftFraming ~ dfdlScopeBegin ~
      scalarNonDefaultContent) ~ elementRightFraming ~ dfdlScopeEnd ~ dfdlElementEnd)

  //  def scalarDefaultable: Prod
  //
  //  def scalarNonDefault: Prod
  lazy val scalarDefaultable = Prod("scalarDefaultable", this,
    if (inputValueCalcOption == None) {
      scalarDefaultablePhysical
    } else {
      inputValueCalcElement
    })

  lazy val scalarNonDefault = Prod("scalarNonDefault", this,
    if (inputValueCalcOption == None) {
      scalarNonDefaultPhysical
    } else {
      inputValueCalcElement
    })

  lazy val inputValueCalcElement = Prod("inputValueCalcElement", this,
    isSimpleType && inputValueCalcOption != None,
    StmtEval(this, dfdlElementBegin ~ dfdlScopeBegin ~
      InputValueCalc(self)) ~ dfdlScopeEnd ~ dfdlElementEnd)

  lazy val scalarDefaultablePhysical = Prod("scalarDefaultablePhysical", this, {
    val res = StmtEval(this, dfdlElementBegin ~ elementLeftFraming ~ dfdlScopeBegin ~
      scalarDefaultableContent) ~ elementRightFraming ~ dfdlScopeEnd ~ dfdlElementEnd
    res
  })

}

trait ElementReferenceGrammarMixin { self: ElementRef =>
  override lazy val termContentBody = self.referencedElement.termContentBody
}

trait LocalElementGrammarMixin { self: LocalElementBase =>

  lazy val termContentBody = {
    val res = Prod("termContentBody", self, separatedScalarDefaultable | recurrance)
    res
  }

  lazy val allowedValue = Prod("allowedValue", this, notStopValue | value)

  lazy val notStopValue = Prod("notStopValue", this, hasStopValue, NotStopValue(this))

  lazy val separatedEmpty = Prod("separatedEmpty", this, emptyIsAnObservableConcept, separatedForPosition(empty))
  lazy val separatedScalarDefaultable = Prod("separatedScalarDefaultable", this, isScalar, separatedForPosition(scalarDefaultable))
  lazy val separatedRecurringDefaultable = Prod("separatedRecurringDefaultable", this, !isScalar, separatedForPosition(scalarDefaultable))
  lazy val separatedScalarNonDefault = Prod("separatedScalarNonDefault", this, isScalar, separatedForPosition(scalarNonDefault))
  lazy val separatedRecurringNonDefault = Prod("separatedRecurringNonDefault", this, !isScalar, separatedForPosition(scalarNonDefault))

  lazy val nonSeparatedScalarDefaultable = Prod("nonSeparatedScalarDefaultable", this, isScalar, scalarDefaultable)

  lazy val recurrance = Prod("recurrance", this,
    !isScalar,
    StartArray(this) ~ arrayContents ~ EndArray(this) ~ FinalUnusedRegion(this))

  override lazy val asTermInChoice = {
    val res = Prod("asTermInChoice", this, nonSeparatedScalarDefaultable | recurrance)
    res
  }

  /**
   * speculate parsing forward until we get an error
   */
  lazy val separatedContentUnboundedWithoutTrailingEmpties = Prod("separatedContentUnboundedWithoutTrailingEmpties", this, isRecurring,
    RepExactlyN(self, minOccurs, separatedRecurringDefaultable) ~
      RepUnbounded(self, separatedRecurringNonDefault) ~
      StopValue(this))

  lazy val separatedContentUnbounded = Prod("separatedContentUnbounded", this, isRecurring,
    separatedContentUnboundedWithoutTrailingEmpties // These are for tolerating trailing empties. Let's not tolerate them for now.
    //        ~
    //        RepUnbounded(separatedEmpty)
    )

  lazy val separatedContentAtMostNWithoutTrailingEmpties = Prod("separatedContentAtMostNWithoutTrailingEmpties", this, isRecurring,
    RepExactlyN(self, minOccurs, separatedRecurringDefaultable) ~
      RepAtMostTotalN(this, maxOccurs, separatedRecurringNonDefault) ~
      StopValue(this))

  // TODO: Do we have to adjust the count to take stopValue into account?
  // Answer: No because the counts are never used when there is a stopValue (at least in current
  // thinking about how occursCountKind='stopValue' works.)

  lazy val separatedContentAtMostN = Prod("separatedContentAtMostN", this, isRecurring,
    separatedContentAtMostNWithoutTrailingEmpties ~
      RepAtMostTotalN(self, maxOccurs, separatedEmpty)) // absorb extra separators, if found.

  /**
   *  parse counted number of occurrences exactly.
   */
  lazy val stopValueSize = if (hasStopValue) 1 else 0

  // TODO FIXME: We really want to have different productions for parsing and unparsing in these
  // complex cases where there is defaulting, etc. Unparsing has many fewer cases, and is just not
  // symmetric with parsing in these situations.
  def separatedContentExactlyN(count: Long) = {
    RepExactlyN(self, minOccurs, separatedRecurringDefaultable) ~
      RepAtMostTotalN(self, count, separatedRecurringNonDefault) ~
      StopValue(this) ~
      RepExactlyTotalN(self, maxOccurs + stopValueSize, separatedEmpty) // absorb reps remaining separators
  }

  lazy val separatedContentExactlyNComputed = {
    OccursCountExpression(this) ~
      RepAtMostOccursCount(this, minOccurs, separatedRecurringDefaultable) ~
      RepExactlyTotalOccursCount(this, separatedRecurringNonDefault)
  }

  // keep in mind that anything here that scans for a representation either knows the length it is going after, or knows what the terminating markup is, and
  // our invariant is, that it does NOT consume that markup ever. The parser consumes it with appropriate grammar terminals. 

  val UNB = -1 // UNBOUNDED

  lazy val arrayContents = {
    val res = Prod("arrayContents", this, isRecurring,
      arrayContentsNoSeparators | arrayContentsWithSeparators)
    res
  }

  lazy val contentUnbounded = {

    val res = Prod("contentUnbounded", this, isRecurring, RepUnbounded(self, separatedRecurringDefaultable))
    res
  }

  lazy val arrayContentsNoSeparators = Prod("arrayContentsNoSeparators", this, isRecurring && !hasSep, {
    val max = maxOccurs
    val min = minOccurs
    val res = occursCountKind match {
      case Expression => separatedContentExactlyNComputed
      case OccursCountKind.Fixed if (max == UNB) => SDE("occursCountKind='fixed' not allowed with unbounded maxOccurs")
      case OccursCountKind.Fixed if (min != max) => SDE("occursCountKind='fixed' requires minOccurs and maxOccurs to be equal (%d != %d)", min, max)
      case OccursCountKind.Fixed => separatedContentExactlyN(max)
      case OccursCountKind.Implicit if (max == UNB) => contentUnbounded // same as parsed
      case OccursCountKind.Implicit => separatedContentAtMostN // uses maxOccurs
      case OccursCountKind.Parsed => contentUnbounded
      case OccursCountKind.StopValue => contentUnbounded
    }
    res
  })

  //
  // Silly constants to make the lookup table below more readable without using fragile whitespace
  val Never______ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.Never
  val TrailingLax: SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.TrailingLax
  val Trailing___ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.Trailing
  val Always_____ : SeparatorSuppressionPolicy = SeparatorSuppressionPolicy.Always
  val StopValue_ = OccursCountKind.StopValue
  val Implicit__ = OccursCountKind.Implicit
  val Parsed____ = OccursCountKind.Parsed
  val Fixed_____ = OccursCountKind.Fixed
  val Expression = OccursCountKind.Expression
  /**
   * Matches the table about separator suppression policy.
   *
   * TODO: Right now that table is in DFDL WG subgroup working on "Issue 140" which is trying to
   * rationalize separator suppression among other things. Update this table to match the final spec.
   */
  lazy val arrayContentsWithSeparators = Prod("arrayContentsWithSeparators", this, isRecurring && hasSep, {
    val triple = (separatorSuppressionPolicy, occursCountKind, maxOccurs)
    val res = triple match {
      case (___________, Expression, ___) => separatedContentExactlyNComputed
      case (___________, Fixed_____, UNB) => SDE("occursCountKind='fixed' not allowed with unbounded maxOccurs")
      case (___________, Fixed_____, max) if (max != minOccurs) => SDE("occursCountKind='fixed' requires minOccurs to equal maxOccurs (%d != %d)", minOccurs, max)
      case (___________, Fixed_____, max) => separatedContentExactlyN(max)
      case (Never______, Implicit__, UNB) => SDE("separatorSuppressionPolicy='never' with occursCountKind='implicit' required bounded maxOccurs.")
      case (Never______, Implicit__, max) => separatedContentExactlyN(max)
      case (Never______, ock, ___) => SDE("separatorSuppressionPolicy='never' not allowed in combination with occursCountKind='" + ock + "'.")
      case (TrailingLax, Implicit__, UNB) if (!isLastRequiredElementOfSequence) => SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a sequence")
      case (TrailingLax, Implicit__, UNB) => separatedContentUnbounded
      case (TrailingLax, Implicit__, max) => separatedContentAtMostN // FIXME: have to have all of them - not trailing position 
      case (Trailing___, Implicit__, UNB) if (!isLastRequiredElementOfSequence) => SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a sequence")
      case (Trailing___, Implicit__, UNB) => separatedContentUnboundedWithoutTrailingEmpties // we're depending on optionalEmptyPart failing on empty content.
      case (Trailing___, Implicit__, max) => separatedContentAtMostNWithoutTrailingEmpties
      case (Always_____, Implicit__, UNB) => separatedContentUnbounded
      case (Always_____, Implicit__, max) => separatedContentAtMostN
      case (Always_____, Parsed____, ___) => separatedContentUnbounded
      case (Always_____, StopValue_, ___) => separatedContentUnbounded
      case (policy, ock, max) => SDE("separatorSuppressionPolicy='" + policy + "' not allowed with occursCountKind='" + ock + "'.")
    }
    res
  })
}

trait ElementDeclGrammarMixin { self: ElementBase with ElementDeclMixin =>

  override lazy val inputValueCalcOption = getPropertyOption("inputValueCalc")

}

trait GlobalElementDeclGrammarMixin
  extends LocalElementGrammarMixin // can be repeating if not root
  { self: GlobalElementDecl =>

  lazy val documentElement = Prod("documentElement", this, scalarDefaultable)

  lazy val document = Prod("document", this, {
    UnicodeByteOrderMark(this) ~ documentElement
  })

}

/////////////////////////////////////////////////////////////////
// Groups System
/////////////////////////////////////////////////////////////////

trait TermGrammarMixin { self: Term =>

  lazy val newVars = this.annotationObjs.filter { st =>
    st.isInstanceOf[DFDLNewVariableInstance]
  }.asInstanceOf[Seq[DFDLNewVariableInstance]]

  lazy val newVarStarts = newVars.map { _.gram }
  lazy val newVarEnds = newVars.map { _.endGram }

  lazy val dfdlScopeBegin = Prod("dfdlScopeBegin", this, newVarStarts.length > 0,
    newVarStarts.fold(EmptyGram) { _ ~ _ })

  lazy val dfdlScopeEnd = Prod("dfdlScopeEnd", this, newVarEnds.length > 0,
    newVarEnds.fold(EmptyGram) { _ ~ _ })

  def termContentBody: Prod

  // I am not sure we need to distinguish these two. 
  lazy val asTermInSequence = termContentBody
  lazy val asTermInChoice = termContentBody

  def separatedForPosition(body: => Gram) = {
    if (!isRepresented) body // no separators for things that have no representation in the data stream
    else {
      val res = prefixSep ~ infixSepRule ~ body ~ postfixSep
      res
    }
  }

  lazy val Some(es) = {
    //
    // Not sure how to assert this,
    // but an invariant we're assuming here is that we are NOT the 
    // root element, which has no enclosing sequence at all.
    //
    // The grammar rules shouldn't be asking for separated stuff
    // in that situation, so we shouldn't be here.
    //
    // TODO: FIXME:
    // Also note: we can get away with just looking upward for nearest enclosing
    // sequence because we have restrictions on what can be inside a choice,
    // and we disallow delimiters on choices. If one allows delimiters on 
    // choices... consider
    // <sequence dfdl:separator=",">
    //   <choice dfdl:initiator="[", terminator="]">
    //     <element ref="foo" maxOccurs="20"/>
    //     ...
    // In this case, what separates the multiple occurrances of foo? I claim 
    // they are comma separated.
    // But data could be like this 'a, b, c,[foo1,foo2,foo3],d,e,f'
    //
    // Not unreasonable, but just too much complexity. Postpone until later.

    //
    // TODO: fix this when those restrictions are lifted.
    //
    subset(hasES, "(Current restriction) There must be an enclosing sequence.")
    nearestEnclosingSequence
  }

  def hasES = nearestEnclosingSequence != None
  def ignoreES = inChoiceBeforeNearestEnclosingSequence == true

  lazy val staticSeparator = Prod("staticSeparator", this, !ignoreES && hasES && es.separator.isConstant,
    StaticSeparator(es, self))

  lazy val dynamicSeparator = Prod("dynamicSeparator", this, !ignoreES && hasES && !es.separator.isConstant,
    DynamicSeparator(es, self))

  lazy val sepRule = staticSeparator | dynamicSeparator

  lazy val prefixSep = Prod("prefixSep", this,
    {
      val res = !ignoreES && hasES && es.hasPrefixSep
      res
    },
    sepRule)

  lazy val postfixSep = Prod("postfixSep", this, !ignoreES && hasES && es.hasPostfixSep, sepRule)
  lazy val infixSep = Prod("infixSep", this, !ignoreES && hasES && es.hasInfixSep, sepRule)

  lazy val isStaticallyFirst = {
    es.hasInfixSep &&
      this.positionInNearestEnclosingSequence == 1 &&
      isScalar &&
      !hasPriorRequiredSiblings
  }

  lazy val infixSepRule = Prod("infixSepRule", this,
    !ignoreES && hasES && es.hasInfixSep, {
      if (isStaticallyFirst) Nada(this) // we're first, no infix sep.
      else if (hasPriorRequiredSiblings) infixSep // always in this case
      else if (positionInNearestEnclosingSequence > 1 || !isScalar) {
        // runtime check for group pos such that we need a separator.
        // Note that GroupPosGreaterThan(N,..) sets discriminator, so if it is true, and infixSep is not found, it won't
        // backtrack and try nothing. Only if GroupPos is not greater than N will it backtrack.
        // TODO: adding ChildPosGreaterThan and ArrayPosGreaterThan fixes bug with xs:choice and array tests--check for other cases
        (ArrayPosGreaterThan(1, self) ~ infixSep) |
          ((GroupPosGreaterThan(1, self) ~ infixSep) | Nada(this))
      } else Assert.invariantFailed("infixSepRule didn't understand what to lay down as grammar for this situation: " + this)
    })

}

trait HasStatementsGrammarMixin { self: Term with DFDLStatementMixin =>

  final lazy val statementGrams = statements.map { _.gram }
  // TODO: statements (but specifically not newVariableInstance) can appear on simple type definitions as well as terms.

  lazy val dfdlStatementEvaluations = Prod("dfdlStatementEvaluations", this, statementGrams.length > 0,
    statementGrams.fold(EmptyGram) { _ ~ _ })
}

trait ModelGroupGrammarMixin
  extends InitiatedTerminatedMixin
  with AlignedMixin
  with HasStatementsGrammarMixin
  with GroupCommonAGMixin { self: ModelGroup =>

  lazy val groupLeftFraming = Prod("groupLeftFraming", this, leadingSkipRegion ~ alignmentFill ~ initiatorRegion)
  lazy val groupRightFraming = Prod("groupRightFraming", this, terminatorRegion ~ trailingSkipRegion)

  // I believe we can have the same grammar rules whether we're directly inside a complex type, or
  // we're nested inside another group as a term.
  lazy val asChildOfComplexType = termContentBody

  lazy val modelGroupSyntax = Prod("modelGroupSyntax", this, dfdlStatementEvaluations ~ groupLeftFraming ~ groupContent ~ groupRightFraming)

  lazy val termContentBody = Prod("termContentBody", this, separatedForPosition(modelGroupSyntax))

  def mt = EmptyGram.asInstanceOf[Gram] // cast trick to shut up foldLeft compile errors below

  def groupContent: Prod
}

trait ChoiceGrammarMixin { self: Choice =>

  lazy val groupContent = Prod("choiceContent", this, alternatives.foldRight(mt)(folder))

  def folder(p: Gram, q: Gram): Gram = p | q

  lazy val alternatives = groupMembers.map { _.asTermInChoice }
}

trait SequenceGrammarMixin { self: Sequence =>

  lazy val groupContent = Prod("sequenceContent", this, StartSequence(this) ~ terms.foldRight(mt)(folder) ~ EndSequence(this))

  def folder(p: Gram, q: Gram): Gram = p ~ q

  lazy val terms = groupMembers.map { _.asTermInSequence }

  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */
  lazy val hasPrefixSep = sepExpr(SeparatorPosition.Prefix)

  lazy val hasInfixSep = sepExpr(SeparatorPosition.Infix)

  lazy val hasPostfixSep = sepExpr(SeparatorPosition.Postfix)

  lazy val hasSeparator = separator.isKnownNonEmpty

  // note use of pass by value. We don't want to even need the SeparatorPosition property unless there is a separator.
  def sepExpr(pos: => SeparatorPosition): Boolean = {
    if (hasSeparator) if (separatorPosition eq pos) true else false
    else false
  }
}

trait GroupRefGrammarMixin { self: GroupRef =>

  def termContentBody = self.group.termContentBody

}

/////////////////////////////////////////////////////////////////
// Types System
/////////////////////////////////////////////////////////////////

trait ComplexTypeBaseGrammarMixin { self: ComplexTypeBase =>
  lazy val startChildren = StartChildren(this, true)
  lazy val endChildren = EndChildren(this, true)

  lazy val mainGrammar = Prod("mainGrammar", self.element, startChildren ~ modelGroup.group.asChildOfComplexType ~ endChildren)

}
