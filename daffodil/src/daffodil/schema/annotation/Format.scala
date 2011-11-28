package daffodil.schema.annotation

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/*
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 */

import java.nio.charset.Charset

import java.io.ObjectOutputStream
import java.io.ObjectInputStream

import daffodil.schema._
import daffodil.schema.annotation.enumerations._
import daffodil.xml.XMLUtil
import daffodil.processors.xpath.XPathUtil
import daffodil.parser.AnnotationParser
import daffodil.parser.regex.Regex;
/**
 * A data structure to hold all the properties of physical format for a DFDL-annotated element
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
@SerialVersionUID(1)
class Format extends AnnotationPart {
		
  private var parentFormat:List[Format] = Nil


  private var alignmentValue:Option[Int] = None

  private var alignmentUnitsValue:Option[AlignmentUnit] = None

  private var baseValue:Option[Int] = None

  private var binaryBooleanTrueRepValue:Option[Long] = None

  private var binaryBooleanFalseRepValue:Option[Long] = None

  private var byteOrderValue:Option[ByteOrder] = None

  private var calendarPatternKindValue:Option[CalendarPatternKind] = None

  private var calendarPatternValue:Option[String] = None

  private var calendarTimeZoneValue:Option[String] = None

  private var decimalSeparatorValue:Option[String] = None

  private var defaultValueValue:Option[String] = None

  private var documentFinalTerminatorCanBeMissingValue:Option[Boolean] = None

  private var escapeBlockEndValue:Option[String] = None

  private var escapeBlockStartValue:Option[String] = None

  private var escapeCharacterValue:Option[String] = None

  private var escapeEscapeCharacterValue:Option[String] = None

  private var escapeKindValue:Option[EscapeKind] = None

  @transient
  private var encodingValue:Option[Charset] = None

  private var encodingName:String = _

  private var floatRepresentationValue:Option[BinaryFloatRepresentation] = None
  
  private var ignoreCaseValue:Option[Boolean] = None

  private var initiatedContentValue:Option[Boolean] = None

  private var initiatorValue:AttributeValue = EmptyValue

  private var leadingSkipBytesValue:Option[Int] = None

  private var lengthValue:Option[String] = None

  private var lengthKindValue:Option[LengthKind] = None

  private var lengthPatternValue:Option[String] = None

  private var maxOccursValue:Option[Int] = Some(1)

  private var minOccursValue:Option[Int] = Some(1)

  private var binaryNumberRepresentationValue:Option[BinaryNumberRepresentation] = None
  
  private var occursCountValue:Option[String] = None

  private var occursCountKindValue:Option[OccursCountKind] = None

  private var occursKindValue:Option[OccursCountKind] = None

  private var occursStopValueValue:AttributeValue = EmptyValue

  private var padCharacterValue:Option[Regex] = None
  
  private var prefixLengthTypeValue:Option[String] = None
  
  private var prefixIncludesPrefixLengthValue:Option[Boolean] = Some(true)

  private var representationValue:Option[Representation] = None

  private var separatorValue:AttributeValue = EmptyValue

  private var separatorPolicyValue:Option[SeparatorPolicy] = None

  private var separatorPositionValue:Option[SeparatorPosition] = None

  private var sequenceKindValue:Option[SequenceKind] = None

  private var stopValueValue:AttributeValue = EmptyValue

  private var terminatorValue:AttributeValue = EmptyValue

  private var textStringJustificationValue:Option[TextStringJustification] = None

  private var textBooleanTrueRepValue:AttributeValue = EmptyValue

  private var textBooleanFalseRepValue:AttributeValue = EmptyValue

  private var textNumberRepValue:Option[TextNumberRep] = None

  private var textNumberPatternValue:Option[String] = None

  private var textStandardDecimalSeparatorValue:Option[String] = None

  private var textStandardGroupingSeparatorValue:Option[String] = None

  private var textStandardExponentCharacterValue:Option[String] = None

  private var textNumberCheckPolicyValue:Option[CheckPolicy] = None

  private var textStandardInfinityRepValue:Option[String] = None

  private var textStandardNanRepValue:Option[String] = None

  private var textNumberRoundingModeValue:Option[RoundingMode] = None

  private var textZonedSignStyleValue:Option[ZonedSignStyle] = None

  private var textStandardZeroRepValue:AttributeValue = EmptyValue

  private var trailingSkipBytesValue:Option[Int] = None

  private var typeNameValue:Option[String] = None

  private def illegalValue(n:String,v:String,allowed:String) = {
	  throw new IllegalArgumentException("Invalid "+n+" in schema ["+v+"]. "+
	 		  "Valid values are {"+allowed+"}")
  }
  
  def +(other:Format) = {
    parentFormat = other :: parentFormat
    this
  }

  ////////////////////////////
  //   alignment
  
  def alignment:Option[Int] = {
    alignmentValue match {
      case Some(x) => alignmentValue
      case None => parentFormat.find { _.alignment != None }.flatMap { _.alignment }
    }
  }

  def setAlignment(a:String):Unit =
    setAlignment(a toInt)

  def setAlignment(a:Int):Unit =
    alignmentValue = Some(a)

  /////////////////////////////
  //    alignmentUnits
  def alignmentUnits:Option[AlignmentUnit] = {
    alignmentUnitsValue match {
      case Some(x) => alignmentUnitsValue
      case None => parentFormat.find { _.alignmentUnits != None }.flatMap { _.alignmentUnits }
    }
  }

  def setAlignmentUnits(a:String):Unit =
    a match {
      case "bits" => setAlignmentUnits(BitAlignment)
      case "bytes" => setAlignmentUnits(ByteAlignment)
      case _ => illegalValue("alignment units",a,"bits,bytes")
    }

  def setAlignmentUnits(a:AlignmentUnit):Unit =
    alignmentUnitsValue = Some(a)

  ////////////////////////////////
  //    textStandardBase

  def base:Option[Int] = {
    baseValue match {
      case Some(x) => baseValue
      case None => parentFormat.find { _.base != None }.flatMap { _.base }
    }
  }

  def setBase(base:String):Unit =
    base match {
      case "2" => setBase(2)
      case "8" => setBase(8)
      case "10" => setBase(10)
      case "16" => setBase(16)
      case _ => illegalValue("textStandardBase",base,"2,8,10,16")
    }

  def setBase(base:Int):Unit =
    this baseValue = Some(base)

  ////////////////////////////////
  //    binaryBooleanTrueRep

  def binaryBooleanTrueRep:Option[Long] = {
    binaryBooleanTrueRepValue match {
      case Some(x) => binaryBooleanTrueRepValue
      case None => parentFormat.find { _.binaryBooleanTrueRep != None }.flatMap { _.binaryBooleanTrueRep }
    }
  }

  def setBinaryBooleanTrueRep(base:String):Unit = setBinaryBooleanTrueRep(base.toLong)

  def setBinaryBooleanTrueRep(base:Long):Unit =
    this binaryBooleanTrueRepValue = Some(base)

  //////////////////////////////
  //     binaryBooleanFalseRep

  def binaryBooleanFalseRep:Option[Long] = {
    binaryBooleanFalseRepValue match {
      case Some(x) => binaryBooleanFalseRepValue
      case None => parentFormat.find { _.binaryBooleanFalseRep != None }.flatMap { _.binaryBooleanFalseRep }
    }
  }

  def setBinaryBooleanFalseRep(base:String):Unit = setBinaryBooleanFalseRep(base.toLong)

  def setBinaryBooleanFalseRep(base:Long):Unit =
    this binaryBooleanFalseRepValue = Some(base)

  //////////////////////////////
  //    byteOrder

  def byteOrder:Option[ByteOrder] = {
    byteOrderValue match {
      case Some(x) => byteOrderValue
      case None => parentFormat.find { _.byteOrder != None }.flatMap { _.byteOrder }
    }
  }

  def setByteOrder(order:String):Unit =
    order match {
      case "bigEndian" => setByteOrder(BigEndian)
      case "littleEndian" => setByteOrder(LittleEndian)
      case _ => illegalValue("byte order type",order,"bigEndian,little-Endian")
    }

  def setByteOrder(order:ByteOrder):Unit =
    byteOrderValue = Some(order)

  ///////////////////////////////////
  //    calendarPatternKind

  def calendarPatternKind:Option[CalendarPatternKind] = {
    calendarPatternKindValue match {
      case Some(x) => calendarPatternKindValue
      case None => parentFormat.find { _.calendarPatternKind != None }.flatMap { _.calendarPatternKind }
    }
  }

  def setCalendarPatternKind(patternKind:String):Unit =
    patternKind match {
      case "implicit" => setCalendarPatternKind(ImplicitCalendar)
      case "explicit" => setCalendarPatternKind(ExplicitCalendar)
      case _ => illegalValue("pattern kind type",patternKind,"implicit,explicit")
    }

  def setCalendarPatternKind(patternKind:CalendarPatternKind) =
    this calendarPatternKindValue = Some(patternKind)

  /////////////////////////////////////
  //  calendarPattern

  def calendarPattern:Option[String] = {
    calendarPatternValue match {
      case Some(x) => calendarPatternValue
      case None => parentFormat.find { _.calendarPattern != None }.flatMap { _.calendarPattern }
    }
  }

  def setCalendarPattern(pattern:String) =
    this calendarPatternValue = Some(pattern)

  /////////////////////////////////////
  //    calendarTimeZon

  def calendarTimeZone:Option[String] = {
    calendarTimeZoneValue match {
      case Some(x) => calendarTimeZoneValue
      case None => parentFormat.find { _.calendarTimeZone != None }.flatMap { _.calendarTimeZone }
    }
  }

  def setCalendarTimeZone(timeZone:String) =
    this calendarTimeZoneValue = Some(timeZone)

  //////////////////////////////////////
  //     decimalSeparator

  def decimalSeparator:Option[String] = {
    decimalSeparatorValue match {
      case Some(x) => decimalSeparatorValue
      case None => parentFormat.find { _.decimalSeparator != None }.flatMap { _.decimalSeparator }
    }
  }

  def setDecimalSeparator(value:String):Unit =
    this decimalSeparatorValue = Some(value)

  //////////////////////////////////
  //    defaultValue

  def defaultValue:Option[String] = {
    defaultValueValue match {
      case Some(x) => defaultValueValue
      case None => parentFormat.find { _.defaultValue != None }.flatMap { _.defaultValue }
    }
  }

  def setDefault(a:String):Unit =
    defaultValueValue = Some(a)
    
  /////////////////////////////////////
  //    documentFinalTerminatorCanBeMissing

  def documentFinalTerminatorCanBeMissing:Option[Boolean] = {
    documentFinalTerminatorCanBeMissingValue match {
      case Some(x) => documentFinalTerminatorCanBeMissingValue
      case None => parentFormat.find { _.documentFinalTerminatorCanBeMissing != None }.flatMap { _.documentFinalTerminatorCanBeMissing }
    }
  }

  def setFinalTerminatorCanBeMissing(b:String):Unit =
    b match {
      case "yes" => setFinalTerminatorCanBeMissing(true)
      case "no" => setFinalTerminatorCanBeMissing(false)
      case _ => throw new IllegalArgumentException("Invalid documentFinalTerminatorCanBeMissing in schema ["+b+"]. "+
        "Valid values are {yes,no}") 
    }


  def setFinalTerminatorCanBeMissing(b:Boolean):Unit =
    documentFinalTerminatorCanBeMissingValue = Some(b)

  ///////////////////////////////////
  //   encoding

  def encoding:Option[Charset] = {
    encodingValue match {
      case Some(x) => encodingValue
      case None => parentFormat.find { _.encoding != None }.flatMap { _.encoding }
    }
  }

  def setEncoding(encoding:String):Unit =  {
    setEncoding(Charset.forName(encoding))
    encodingName = encoding
  }

  def setEncoding(encoding:Charset):Unit = {
    this encodingValue = Some(encoding)
    encodingName = encoding displayName
  }

  ///////////////////////////////
  //   escapeBlockEnd

  def escapeBlockEnd:Option[String] = {
    escapeBlockEndValue match {
      case Some(x) => escapeBlockEndValue
      case None => parentFormat.find { _.escapeBlockEnd != None }.flatMap { _.escapeBlockEnd }
    }
  }

  def setEscapeBlockEnd(escapeBlock:String) =
    escapeBlockEndValue = Some(escapeBlock)

  ///////////////////////////////////
  //    escapeBlockStart

  def escapeBlockStart:Option[String] = {
    escapeBlockStartValue match {
      case Some(x) => escapeBlockStartValue
      case None => parentFormat.find { _.escapeBlockStart != None }.flatMap { _.escapeBlockStart }
    }
  }

  def setEscapeBlockStart(escapeBlock:String) =
    escapeBlockStartValue = Some(escapeBlock)


  /////////////////////////////////////
  //      escapeCharacter

  def escapeCharacter:Option[String] = {
    escapeCharacterValue match {
      case Some(x) => escapeCharacterValue
      case None => parentFormat.find { _.escapeCharacter != None }.flatMap { _.escapeCharacter }
    }
  }

  def setEscapeCharacter(escapeCharacter:String) =
    escapeCharacterValue = Some(escapeCharacter)

  /////////////////////////////////////
  //   escapeEscapeCharacter

  def escapeEscapeCharacter:Option[String] = {
    escapeEscapeCharacterValue match {
      case Some(x) => escapeEscapeCharacterValue
      case None => parentFormat.find { _.escapeEscapeCharacter != None }.flatMap { _.escapeEscapeCharacter }
    }
  }

  def setEscapeEscapeCharacter(escapeCharacter:String) =
    escapeEscapeCharacterValue = Some(escapeCharacter)

  ////////////////////////////////////
  //    escapeKind

  def escapeKind:Option[EscapeKind] = {
    escapeKindValue match {
      case Some(x) => escapeKindValue
      case None => parentFormat.find { _.escapeKind != None }.flatMap { _.escapeKind }
    }
  }

  def setEscapeKind(escapeKind:String):Unit = {
    escapeKind match {
      case "escapeCharacter" => setEscapeKind(EscapeCharacter)
      case "escapeBlock" => setEscapeKind(EscapeBlock)
      case x => throw new IllegalArgumentException("Invalid escapeKind in schema ["+x+"]. "+
        "Valid values are {escapeCharacter,escapeBlock}")
    }
  }

  def setEscapeKind(escapeKind:EscapeKind):Unit =
    escapeKindValue = Some(escapeKind)

  ////////////////////////////////////
  //   floatRepresentation

  def floatRepresentation:Option[BinaryFloatRepresentation] = {
    floatRepresentationValue match {
      case Some(x) => floatRepresentationValue
      case None => parentFormat.find { _.floatRepresentation != None }.flatMap { _.floatRepresentation }
    }
  }

  def setFloatRepresentation(a:String):Unit =
    a match {
      case "ieee" => setFloatRepresentation(IEEE)
      case "ibm390Hex" => setFloatRepresentation(IBM390Hex)
      case _ => throw new IllegalArgumentException("Invalid binary float representation in schema ["+a+"]. "+
        "Valid values are {ieee}")
    }

  def setFloatRepresentation(a:BinaryFloatRepresentation):Unit =
    floatRepresentationValue = Some(a)

  ///////////////////////////////////////

  def ignoreCase:Option[Boolean] = {
    ignoreCaseValue match {
      case Some(x) => ignoreCaseValue
      case None => parentFormat.find { _.ignoreCase != None }.flatMap { _.ignoreCase }
    }
  }

  def setIgnoreCase(ignoreCase:String):Unit = {
	  ignoreCase match {
	 	  case "yes" => ignoreCaseValue = Some(true)
	 	  case "no" => ignoreCaseValue = Some(false)
	 	  case x => throw new IllegalArgumentException("Invalid ignoreCase in schema ["+x+"]. "+
	 	  	"Valid values are {yes,no}")
	  }
  }

  def setIgnoreCase(ignoreCase:Boolean):Unit =
    this ignoreCaseValue = Some(ignoreCase)

  ////////////////////////////

  def initiatedContent:Option[Boolean] = {
    initiatedContentValue match {
      case None => parentFormat.find { _.initiatedContent != None } match {
        case Some(x) => x.initiatedContent
        case None => None
      }
      case Some(b) => initiatedContentValue
    }
  }

  def setInitiatedContent(initiated:String):Unit =
    initiated match {
      case "yes" => setInitiatedContent(true)
      case "no" => setInitiatedContent(false)
      case x => throw new IllegalArgumentException("Invalid initiatedContent in schema ["+x+"]. "+
        "Valid values are {yes,no}")
    }

  def setInitiatedContent(initiated:Boolean):Unit =
    initiatedContentValue = Some(initiated)
    
  /////////////////////////////

  def initiator:AttributeValue = {
    initiatorValue match {
      case EmptyValue => parentFormat.find { _.initiator != None } match {
        case Some(x) => x.initiator
        case None => EmptyValue
      }
      case _ => initiatorValue
    }
  }

  def setInitiator(initiator:String):Unit =
    this initiatorValue = XMLUtil getListFromValue(initiator)

  /////////////////////////////

  def leadingSkipBytes:Option[Int] = {
    leadingSkipBytesValue match {
      case Some(x) => leadingSkipBytesValue
      case None => parentFormat.find { _.leadingSkipBytes != None }.flatMap { _.leadingSkipBytes }
    }
  }

  def setLeadingSkipBytes(a:String):Unit =
    setLeadingSkipBytes(a toInt)

  def setLeadingSkipBytes(a:Int):Unit =
    leadingSkipBytesValue = Some(a)

  //////////////////////////////

  def length:Option[String] = {
    lengthValue match {
      case Some(x) => lengthValue
      case None => parentFormat.find { _.length != None }.flatMap { _.length }
    }
  }

  def setLength(length:String):Unit = this lengthValue = Some(length)

  //////////////////////////

  def lengthKind:Option[LengthKind] = {
    lengthKindValue match {
      case Some(x) => lengthKindValue
      case None => parentFormat.find { _.lengthKind != None }.flatMap { _.lengthKind }
    }
  }

  def setLengthKind(length:String):Unit =
    length match {
      case "implicit" => setLengthKind(Implicit)
      case "explicit" => setLengthKind(Explicit)
      case "delimited" => setLengthKind(Delimited)
      case "endOfParent" => setLengthKind(EndOfParent)
      case "pattern" => setLengthKind(Pattern)
      case "prefixed" => setLengthKind(Prefixed)
      case _ => throw new IllegalArgumentException("Invalid length type in schema ["+length+"]. "+
              "Valid values are {implicit,explicit,delimited,endOfParent,pattern,prefixed}")
    }

  def setLengthKind(length:LengthKind):Unit =
    this lengthKindValue = Some(length)

  /////////////////////////////////

  def lengthPattern:Option[String] = {
    lengthPatternValue match {
      case Some(x) => lengthPatternValue
      case None => parentFormat.find { _.lengthPattern != None }.flatMap { _.lengthPattern }
    }
  }

  def setLengthPattern(length:String):Unit =
    this lengthPatternValue = Some(length)

  //////////////////////////////

  def maxOccurs:Option[Int] = maxOccursValue

  def setMaxOccurs(n:String):Unit =
    n match {
      case "unbounded" => setMaxOccurs(-1)
      case _ => setMaxOccurs(n.toInt)
    }

  def setMaxOccurs(n:Int):Unit =
    this maxOccursValue = Some(n)

  /////////////////////////////////

  def minOccurs:Option[Int] = minOccursValue

  def setMinOccurs(n:String):Unit =
    setMinOccurs(n.toInt)

  def setMinOccurs(n:Int):Unit =
    this minOccursValue = Some(n)

  /////////////////////////////////

  def binaryNumberRepresentation:Option[BinaryNumberRepresentation] = {
    binaryNumberRepresentationValue match {
      case Some(x) => binaryNumberRepresentationValue
      case None => parentFormat.find { _.binaryNumberRepresentation != None }.flatMap { _.binaryNumberRepresentation }
    }
  }

  def setBinaryNumberRepresentation(a:String):Unit =
    a match {
      case "binary" => setBinaryNumberRepresentation(BinaryNumber)
      case "packed" => setBinaryNumberRepresentation(PackedNumber)
      case "bcd" => setBinaryNumberRepresentation(BCDNumber)
      case _ => throw new IllegalArgumentException("Invalid binary number representation in schema ["+a+"]. "+
        "Valid values are {binary,packed,bcd}")
    }

  def setBinaryNumberRepresentation(a:BinaryNumberRepresentation):Unit =
    binaryNumberRepresentationValue = Some(a)

  //////////////////////////////////////

  def occursCount:Option[String] = {
    occursCountValue match {
      case Some(x) => occursCountValue
      case None => parentFormat.find { _.occursCount != None }.flatMap { _.occursCount }
    }
  }

  def setOccursCount(n:String):Unit =
    this occursCountValue = Some(n)

  ////////////////////////////

  def occursCountKind:Option[OccursCountKind] = {
    occursCountKindValue match {
      case Some(x) => occursCountKindValue
      case None => parentFormat.find { _.occursCountKind != None }.flatMap { _.occursCountKind }
    }
  }

  def setOccursCountKind(occursKind:String):Unit = {
    occursKind match {
      case "fixed" => setOccursCountKind(Fixed)
      case "expression" => setOccursCountKind(Expression)
      case "parsed" => setOccursCountKind(Parsed)
      case "stopValue" => setOccursCountKind(StopValue)
    }
  }

  def setOccursCountKind(occursKind:OccursCountKind):Unit =
    occursCountKindValue = Some(occursKind)

  ////////////////////////////////////////

  def occursStopValue:AttributeValue = {
    occursStopValueValue match {
      case EmptyValue => parentFormat.find { _.occursStopValue != EmptyValue } match {
          case Some(x) => x occursStopValue
          case None => EmptyValue
      }
      case _  => occursStopValueValue      
    }
  }

  def setOccursStopValue(value:String):Unit =
    this occursStopValueValue = XMLUtil getListFromValue(value)

  //////////////////////////////

  def padCharacter:Option[Regex] = {
    padCharacterValue match {
      case Some(x) => padCharacterValue
      case None => parentFormat.find { _.padCharacter != None }.flatMap { _.padCharacter }
    }
  }

  def setPadCharacter(s:String) =
    this padCharacterValue = Some(AnnotationParser.unescape(s))

  //////////////////////////////

  ////
  def prefixLengthType:Option[String] = {
	  prefixLengthTypeValue match {
	 	  case Some(x) => prefixLengthTypeValue
	 	  case None => throw new IllegalArgumentException("prefixed length type requires that prefix length type is specified")
	  }
  }

  def setPrefixLengthType(s:String) = {
	  prefixLengthTypeValue = Some(s)
  }
  
  ///
  
  def prefixIncludesPrefixLength:Option[Boolean] = {
	  prefixIncludesPrefixLengthValue
	  // FIXME validate?
  }
  
  def setPrefixIncludesPrefixLength(b:String):Unit = {
	  b match {
	 	  case "yes" => setPrefixIncludesPrefixLength(true)
	 	  case "no" => setPrefixIncludesPrefixLength(false)
	 	  case x => illegalValue("prefix includes prefix length",x,"yes,no")
	  }
  }
  
  def setPrefixIncludesPrefixLength(b:Boolean):Unit = {
  	prefixIncludesPrefixLengthValue = Some(b)
  }

  ///
  
  def representation:Option[Representation] = {
    representationValue match {
      case Some(x) => representationValue
      case None => parentFormat.find { _.representation != None }.flatMap { _.representation }
    }
  }

  def setRepresentation(representation:String):Unit =
    representation match {
      case "text" => setRepresentation(Text)
      case "binary" => setRepresentation(Binary)
      case _ => throw new IllegalArgumentException("Invalid representation type in schema ["+representation+"]."+"" +
              "Valid values are {text,binary}")
    }

  def setRepresentation(representation:Representation):Unit =
    this representationValue = Some(representation)

  /////////////////////////////////
  //    separator

  def separator:AttributeValue = {
    separatorValue match {
      case EmptyValue => parentFormat.find { _.separator != EmptyValue } match {
        case Some(x) => x.separator
        case None => EmptyValue
      }
      case _ => separatorValue
    }
  }

  def setSeparator(separator:String):Unit =
    this separatorValue = XMLUtil getListFromValue(separator)

  /////////////////////////////

  def separatorPolicy:Option[SeparatorPolicy] = {
    separatorPolicyValue match {
      case Some(x) => separatorPolicyValue
      case None => parentFormat.find { _.separatorPolicy != None }.flatMap { _.separatorPolicy }
    }
  }

  def setSeparatorPolicy(policy:String):Unit =
    policy match {
      case "required" => setSeparatorPolicy(Required)
      case "suppressed" => setSeparatorPolicy(Supress)
      case "suppressedAtEndStrict" => setSeparatorPolicy(SupressAtEnd) // is our policy strict?
      case _ => throw new IllegalArgumentException("Invalid separator policy in schema ["+policy+"]. "+
        "Valid values are {required,suppressed,supressedAtEnd}")
    }

  def setSeparatorPolicy(policy:SeparatorPolicy):Unit =
    this separatorPolicyValue = Some(policy)

  /////////////////////////////////

  def separatorPosition:Option[SeparatorPosition] = {
    separatorPositionValue match {
      case Some(x) => separatorPositionValue
      case None => parentFormat.find { _.separatorPosition != None }.flatMap { _.separatorPosition }
    }
  }

  def setSeparatorPosition(position:String):Unit =
    position match {
      case "infix" => setSeparatorPosition(Infix)
      case "prefix" => setSeparatorPosition(Prefix)
      case "postfix" => setSeparatorPosition(Postfix)
      case _ => throw new IllegalArgumentException("Invalid separator position in schema ["+position+"]. "+
              "Valid values are {infix,prefix,postfix}")
    }

  def setSeparatorPosition(position:SeparatorPosition):Unit =
    this separatorPositionValue = Some(position)

  /////////////////////////////////

  def sequenceKind:Option[SequenceKind] = {
    sequenceKindValue match {
      case Some(x) => sequenceKindValue
      case None => parentFormat.find { _.sequenceKind != None }.flatMap { _.sequenceKind }
    }
  }

  def setSequenceKind(sequenceKind:String):Unit =
    sequenceKind match {
      case "ordered" => setSequenceKind(OrderedSequence)
      case "unordered" => setSequenceKind(UnorderedSequence)
      case _ => throw new IllegalArgumentException("Invalid sequenceKind in schema ["+sequenceKind+"]. "+
              "Valid values are {ordered,unordered}")
    }

  def setSequenceKind(sequenceKind:SequenceKind):Unit =
    this sequenceKindValue = Some(sequenceKind)
  
  ///////////////////////////////////////

  def stopValue:AttributeValue =  {
    stopValueValue match {
      case EmptyValue => parentFormat.find { _.stopValue != EmptyValue } match {
        case Some(x) => x.stopValue
        case None => EmptyValue
      }
      case _ => stopValueValue
    }
  }

  def setStopValue(stopValue:String):Unit =
    this stopValueValue = XMLUtil getListFromValue(stopValue)

  ///////////////////////////////////////

  def terminator:AttributeValue = {
    terminatorValue match {
      case EmptyValue => parentFormat.find { _.terminator != EmptyValue } match {
        case Some(x) => x.terminator
        case None => EmptyValue
      }
      case _ => terminatorValue
    }
  }

  def setTerminator(terminator:String):Unit =
    this terminatorValue = XMLUtil getListFromValue(terminator)

  ////////////////////////////////////

  def textBooleanTrueRep:AttributeValue = {
    textBooleanTrueRepValue match {
      case EmptyValue => parentFormat.find { _.textBooleanTrueRep != EmptyValue } match {
        case Some(x) => x.textBooleanTrueRep
        case None => EmptyValue
      }
      case _ => textBooleanTrueRepValue      
    }
  }

  def setTextBooleanTrueRep(booleanTrue:String):Unit =
    this textBooleanTrueRepValue = XMLUtil getListFromValue(booleanTrue)

  //////////////////////////////////////

  def textBooleanFalseRep:AttributeValue = {
    textBooleanFalseRepValue match {
      case EmptyValue => parentFormat.find { _.textBooleanFalseRep != EmptyValue } match {
        case Some(x) => x.textBooleanFalseRep
        case None => EmptyValue
      }
      case _ => textBooleanFalseRepValue
    }
  }

  def setTextBooleanFalseRep(booleanTrue:String):Unit =
    this textBooleanFalseRepValue = XMLUtil getListFromValue(booleanTrue)

  /////////////////////////////////

  def textStringJustification:Option[TextStringJustification] = {
    textStringJustificationValue match {
      case Some(x) => textStringJustificationValue
      case None => parentFormat.find { _.textStringJustification != None }.flatMap { _.textStringJustification }
    }
  }

  def setTextStringJustification(justification:String):Unit =
    justification match {
      case "left" => setTextStringJustification(LeftJustification)
      case "right" => setTextStringJustification(RightJustification)
      case "center" => setTextStringJustification(CenterJustification)
      case _ => throw new IllegalArgumentException("Invalid justification type in schema ["+justification+"]. "+
        "Valid values are {left,right,center}")
    }

  def setTextStringJustification(justification:TextStringJustification):Unit =
    textStringJustificationValue = Some(justification)

  ///////////////////////////////
  //    textNumberCheckPolicy

  def textNumberCheckPolicy:Option[CheckPolicy] = {
    textNumberCheckPolicyValue match {
      case Some(x) => textNumberCheckPolicyValue
      case None => parentFormat.find { _.textNumberCheckPolicy != None }.flatMap { _.textNumberCheckPolicy }
    }
  }

  def setTextNumberCheckPolicy(a:String):Unit =
    a match {
      case "lax" => setTextNumberCheckPolicy(Lax)
      case "strict" => setTextNumberCheckPolicy(Strict)
      case _ => throw new IllegalArgumentException("Invalid textNumberCheckPolicy in schema ["+a+"]. "+
        "Valid values are {lax,strict}")
    }

  def setTextNumberCheckPolicy(a:CheckPolicy):Unit =
    textNumberCheckPolicyValue = Some(a)
  

  ////////////////////////////////
  //    textNumberRep


  def textNumberRep:Option[TextNumberRep] = {
    textNumberRepValue match {
      case Some(x) => textNumberRepValue
      case None => parentFormat.find { _.textNumberRep != None }.flatMap { _.textNumberRep }
    }
  }

  def setTextNumberRep(a:String):Unit =
    a match {
      case "standard" => setTextNumberRep(StandardTextNumberRep)
      case "zoned" => setTextNumberRep(ZonedTextNumberRep)
      case _ => throw new IllegalArgumentException("Invalid textNumberRep in schema ["+a+"]. "+
        "Valid values are {standard,zoned}")
    }

  def setTextNumberRep(a:TextNumberRep):Unit =
    textNumberRepValue = Some(a)

  ///////////////////////////////////
  //   textNumberPattern

  def textNumberPattern:Option[String] = {
    textNumberPatternValue match {
      case Some(x) => textNumberPatternValue
      case None => parentFormat.find { _.textNumberPattern != None }.flatMap { _.textNumberPattern }
    }
  }

  def setTextNumberPattern(value:String):Unit =
    this textNumberPatternValue = Some(value)

  ///////////////////////////////////
  //   textNumberRoundingMode

  def textNumberRoundingMode:Option[RoundingMode] = {
    textNumberRoundingModeValue match {
      case Some(x) => textNumberRoundingModeValue
      case None => parentFormat.find { _.textNumberRoundingMode != None }.flatMap { _.textNumberRoundingMode }
    }
  }

  def setTextNumberRoundingMode(value:String):Unit =
    this textNumberRoundingModeValue = value match {
      case "roundCeiling" => Some(RoundCeiling)
      case "roundFloor" => Some(RoundFloor)
      case "roundDown" => Some(RoundDown)
      case "roundUp" => Some(RoundUp)
      case "roundHalfEven" => Some(RoundHalfEven)
      case "roundHalfDown" => Some(RoundHalfDown)
      case "roundHalfUp" => Some(RoundHalfUp)
      case _ => throw new IllegalArgumentException("Invalid textNumberRoundingMode in schema ["+value+"]. "+
        "Valid values are {roundCeiling,roundFloor,roundDown,roundUp,roundHalfEven,roundHalfDown,roundHalfUp}")
    }

  ///////////////////////////////////
  //   textZonedSignStyle

  def textZonedSignStyle:Option[ZonedSignStyle] = {
    textZonedSignStyleValue match {
      case Some(x) => textZonedSignStyleValue
      case None => parentFormat.find { _.textZonedSignStyle != None }.flatMap { _.textZonedSignStyle }
    }
  }

  def setTextZonedSignStyle(value:String):Unit =
    this textZonedSignStyleValue = value match {
      case "asciiStandard" => Some(ASCIIStandard)
      case "asciiTranslatedEBCDIC" => Some(ASCIITranslated)
      case "asciiCARealiaModified" => Some(ASCIICARealiaModified)
      case _ => throw new IllegalArgumentException("Invalid textZonedSignStyle in schema ["+value+"]. "+
        "Valid values are {asciiStandard,asciiTranslatedEBCDIC,asciiCARealiaModified}")
    }
  
  ///////////////////////////////////
  //   textStandardDecimalSeparator

  def textStandardDecimalSeparator:Option[String] = {
    textStandardDecimalSeparatorValue match {
      case Some(x) => textStandardDecimalSeparatorValue
      case None => parentFormat.find { _.textStandardDecimalSeparator != None }.flatMap { _.textStandardDecimalSeparator }
    }
  }

  def setTextStandardDecimalSeparator(value:String):Unit =
    this textStandardDecimalSeparatorValue = Some(value)

  ///////////////////////////////////
  //   textStandardGroupingSeparator

  def textStandardGroupingSeparator:Option[String] = {
    textStandardGroupingSeparatorValue match {
      case Some(x) => textStandardGroupingSeparatorValue
      case None => parentFormat.find { _.textStandardGroupingSeparator != None }.flatMap { _.textStandardGroupingSeparator }
    }
  }

  def setTextStandardGroupingSeparator(value:String):Unit =
    this textStandardGroupingSeparatorValue = Some(value)

  ///////////////////////////////////
  //   textStandardExponentCharacter

  def textStandardExponentCharacter:Option[String] = {
    textStandardExponentCharacterValue match {
      case Some(x) => textStandardGroupingSeparatorValue
      case None => parentFormat.find { _.textStandardExponentCharacter != None }.flatMap { _.textStandardExponentCharacter }
    }
  }

  def setTextStandardExponentCharacter(value:String):Unit =
    this textStandardExponentCharacterValue = Some(value)

  ///////////////////////////////////
  //   textStandardInfinityRep

  def textStandardInfinityRep:Option[String] = {
    textStandardInfinityRepValue match {
      case Some(x) => textStandardInfinityRepValue
      case None => parentFormat.find { _.textStandardInfinityRep != None }.flatMap { _.textStandardInfinityRep }
    }
  }

  def setTextStandardInfinityRep(value:String):Unit =
    this textStandardInfinityRepValue = Some(value)

  ///////////////////////////////////
  //   textStandardNanRep

  def textStandardNanRep:Option[String] = {
    textStandardNanRepValue match {
      case Some(x) => textStandardNanRepValue
      case None => parentFormat.find { _.textStandardNanRep != None }.flatMap { _.textStandardNanRep }
    }
  }

  def setTextStandardNanRep(value:String):Unit =
    this textStandardNanRepValue = Some(value)

  ///////////////////////////////////
  //   textStandardZeroRep

  def textStandardZeroRep:AttributeValue = {
    textStandardZeroRepValue match {
      case EmptyValue => parentFormat.find { _.textStandardZeroRep != EmptyValue } match {
        case Some(x) => x.textStandardZeroRep
        case None => EmptyValue
      }
      case _ => textStandardZeroRepValue
    }
  }

  def setTextStandardZeroRep(zeros:String):Unit =
    this textStandardZeroRepValue = XMLUtil getListFromValue(zeros)

  ////////////////////////////////

  def trailingSkipBytes:Option[Int] = {
    trailingSkipBytesValue match {
      case Some(x) => trailingSkipBytesValue
      case None => parentFormat.find { _.trailingSkipBytes != None }.flatMap { _.trailingSkipBytes }
    }
  }

  def setTrailingSkipBytes(a:String):Unit =
    setTrailingSkipBytes(a toInt)

  def setTrailingSkipBytes(a:Int):Unit =
    trailingSkipBytesValue = Some(a)

  ///////////////////////////////

  def typeName:Option[String] = {
    typeNameValue match {
      case Some(x) => typeNameValue
      case None => parentFormat.find { _.typeName != None }.flatMap { _.typeName }
    }
  }

  def setTypeName(typeName:String):Unit =
    typeNameValue = Some(typeName)

  ///////////////////////////////





//  override def canEqual(that:Any) = 
//    that match {
//      case _:Format => true
//      case _ => false
//    }
//  
//  
//  override def equals(that:Any) = 
//    that match {
//      case x:Format => if (x!=null && x.canEqual(this))
//        representation == x.representation &&
//      length == x.length &&
//      encoding == x.encoding &&
//      lengthKind == x.lengthKind &&
//      lengthPattern == x.lengthPattern &&
//      separator == x.separator &&
//      separatorPolicy == x.separatorPolicy &&
//      typeName == x.typeName &&
//      terminator == x.terminator &&
//      initiator == x.initiator &&
//      separatorPosition == x.separatorPosition &&
//      maxOccurs == x.maxOccurs &&
//      minOccurs == x.minOccurs &&
//      occursCount == x.occursCount &&
//      occursCountKind == x.occursCountKind &&
//      occursStopValue == x.occursStopValue &&
//      byteOrder == x.byteOrder &&
//      base == x.base &&
//      textNumberPattern == x.textNumberPattern &&
//      decimalSeparator == x.decimalSeparator &&
//      ignoreCase == x.ignoreCase &&
//      floatRepresentation == x.floatRepresentation &&
//      calendarPatternKind == x.calendarPatternKind &&
//      calendarPattern == x.calendarPattern &&
//      textStringJustification == x.textStringJustification &&
//      padCharacter == x.padCharacter &&
//      documentFinalTerminatorCanBeMissing == x.documentFinalTerminatorCanBeMissing 
//      
//                       else false
//      case _ => false	
//    }

  override def diff(o: Any): Similarity =
    o match {
      case x: Format => {
        // Don't call super Diff !!! That's only for if you don't want to implement Diff and want to implement equals instead
        // val superDiff = super.diff(x)
        // if (superDiff != Same) return superDiff
        if (representation != x.representation) return Different(representation, x.representation)
        if (length != x.length) return Different(length, x.length)
        if (encoding != x.encoding) return Different(encoding, x.encoding)
        if (lengthKind != x.lengthKind) return Different(lengthKind, x.lengthKind)
        if (lengthPattern != x.lengthPattern) return Different(lengthPattern, x.lengthPattern)
        if (separator != x.separator) return Different(separator, x.separator)
        if (separatorPolicy != x.separatorPolicy) return Different(separatorPolicy, x.separatorPolicy)
        if (typeName != x.typeName) return Different(typeName, x.typeName)
        if (terminator != x.terminator) return Different(terminator, x.terminator)
        if (initiator != x.initiator) return Different(initiator, x.initiator)
        if (separatorPosition != x.separatorPosition) return Different(separatorPosition, x.separatorPosition)
        if (maxOccurs != x.maxOccurs) return Different(maxOccurs, x.maxOccurs)
        if (minOccurs != x.minOccurs) return Different(minOccurs, x.minOccurs)
        if (occursCount != x.occursCount) return Different(occursCount, x.occursCount)
        if (occursCountKind != x.occursCountKind) return Different(occursCountKind, x.occursCountKind)
        if (occursStopValue != x.occursStopValue) return Different(occursStopValue, x.occursStopValue)
        if (byteOrder != x.byteOrder) return Different(byteOrder, x.byteOrder)
        if (base != x.base) return Different(base, x.base)
        if (textNumberPattern != x.textNumberPattern) return Different(textNumberPattern, x.textNumberPattern)
        if (decimalSeparator != x.decimalSeparator) return Different(decimalSeparator, x.decimalSeparator)
        if (ignoreCase != x.ignoreCase) return Different(ignoreCase, x.ignoreCase)
        if (floatRepresentation != x.floatRepresentation) return Different(floatRepresentation, x.floatRepresentation)
        if (calendarPatternKind != x.calendarPatternKind) return Different(calendarPatternKind, x.calendarPatternKind)
        if (calendarPattern != x.calendarPattern) return Different(calendarPattern, x.calendarPattern)
        if (textStringJustification != x.textStringJustification) return Different(textStringJustification, x.textStringJustification)
        if (padCharacter != x.padCharacter) return Different(padCharacter, x.padCharacter)
        if (documentFinalTerminatorCanBeMissing != x.documentFinalTerminatorCanBeMissing) return Different(documentFinalTerminatorCanBeMissing, x.documentFinalTerminatorCanBeMissing)
        Same
      }
      case _ => DifferentType
    }

  override def toString() = {
    val sb = new StringBuilder

    alignment map { x:Int => sb.append("dfdl:alignment="+ x + "\n" ) }
    alignmentUnits map { x:AlignmentUnit => sb.append("dfdl:alignmentUnits="+ x + "\n" ) }
    base map { x:Int => sb.append("dfdl:base="+x + "\n") }
    binaryBooleanFalseRep map { x:Long => sb.append("dfdl:binaryBooleanFalseRep="+x + "\n" ) }
    binaryBooleanTrueRep map { x:Long => sb.append("dfdl:binaryBooleanTrueRep="+x + "\n" ) }
    byteOrder map { x:ByteOrder => sb.append("dfdl:byteOrder="+x + "\n" ) }
    calendarPatternKind map { x:CalendarPatternKind => sb.append("dfdl:calendarPatternKind="+x + "\n") }
    calendarPattern map { x:String => sb.append("dfdl:calendarPattern=" + x + "\n" ) }
    calendarTimeZone map { x:String => sb.append("dfdl:calendarTimeZone=" + x + "\n" ) }
    decimalSeparator map { x:String => sb.append("dfdl:decimalSeparator=" + x + "\n" ) }
    defaultValue map { x:String => sb.append("default=" + x + "\n" ) }
    escapeBlockEnd map { x:String => sb.append("dfdl:escapeBlockEnd=" + x + "\n" ) }
    escapeBlockStart map { x:String => sb.append("dfdl:escapeBlockStart=" + x + "\n" ) }
    escapeCharacter map { x:String => sb.append("dfdl:escapeCharacter=" + x + "\n" ) }
    escapeEscapeCharacter map { x:String => sb.append("dfdl:escapeEscapeCharacter=" + x + "\n" ) }
    escapeKind map { x:EscapeKind => sb.append("dfdlescapeKind:=" + x + "\n" ) }
    encoding map { x:Charset => sb.append("dfdlencoding:=" + x + "\n" ) }
    documentFinalTerminatorCanBeMissing map { x:Boolean => sb.append("dfdl:documentFinalTerminatorCanBeMissing=" + x + "\n" ) }
    floatRepresentation map { x:BinaryFloatRepresentation => sb.append("dfdl:floatRepresentation=" + x + "\n" ) }
    ignoreCase map { x:Boolean => sb.append("dfdl:ignoreCase=" + x + "\n" ) }
    if (initiator!=EmptyValue)
      sb.append("dfdl:initator=" + initiator+"\n")
    leadingSkipBytes map { x:Int => sb.append("dfdl:leadingSkipBytes=" + x + "\n" ) }
    length map { x:String => sb.append("dfdllength:=" + x + "\n" ) }
    lengthKind map { x:LengthKind => sb.append("dfdl:lengthKind=" + x + "\n" ) }
    lengthPattern map { x:String => sb.append("dfdl:lengthPattern=" + x + "\n" ) }
    maxOccurs map { x:Int =>  if (x == -1) sb.append("maxOccurs=unbounded") else sb.append("maxOccurs=" + x + "\n" ) }
    minOccurs map { x:Int => sb.append("minOccurs=" + x + "\n" ) }    
    binaryNumberRepresentation map { x:BinaryNumberRepresentation => sb.append("dfdl:binaryNumberRepresentation=" + x + "\n" ) }
    occursCount map { x:String => sb.append("dfdl:occursCount=" + x + "\n" ) }
    occursCountKind map { x:OccursCountKind => sb.append("dfdloccursCountKind:=" + x + "\n" ) }
    if (occursStopValue!=EmptyValue)
      sb.append("dfdl:occursStopValue=" + occursStopValue+"\n")
    padCharacter map { x:Regex => sb.append("dfdl:padCharacter=" + x + "\n" ) }
    representation map { x:Representation => sb.append("dfdl:representation=" + x + "\n" ) }
    if (separator!=EmptyValue)
      sb.append("dfdl:separator=" + separator+"\n")
    separatorPolicy map { x:SeparatorPolicy => sb.append("dfdl:separatorPolicy=" + x + "\n" ) }
    separatorPosition map { x:SeparatorPosition => sb.append("dfdl:separatorPosition=" + x + "\n" ) }
    if (stopValue!=EmptyValue)
      sb.append("dfdl:stopValue=" + stopValue+"\n")
    if (terminator!=EmptyValue)
      sb.append("dfdl:terminator=" + terminator+"\n")    
    if (textBooleanFalseRep!=EmptyValue)
      sb.append("dfdl:textBooleanFalseRep=" + textBooleanFalseRep+"\n")
    if (textBooleanTrueRep!=EmptyValue)
      sb.append("dfdl:textBooleanTrueRep=" + textBooleanTrueRep+"\n") 
    textStringJustification map { x:TextStringJustification => sb.append("dfdl:textStringJustification=" + x + "\n" ) }
    textNumberRep map { x:TextNumberRep => sb.append("dfdl:textNumberRep=" + x + "\n" ) }
    textNumberPattern map { x:String => sb.append("dfdl:textNumberPattern=" + x + "\n" ) }
    textStandardDecimalSeparator map { x:String => sb.append("dfdl:textStandardDecimalSeparator=" + x + "\n" ) }
    textStandardGroupingSeparator  map { x:String => sb.append("dfdl:textStandardGrouping=" + x + "\n" ) }
    textStandardExponentCharacter map { x:String => sb.append("dfdl:textStandardExponentCharacter=" + x + "\n" ) }
    textNumberCheckPolicy map { x:CheckPolicy => sb.append("dfdl:textNumberCheckPolicy=" + x + "\n" ) }
    textStandardInfinityRep map { x:String => sb.append("dfdl:textStandardInfinityRep=" + x + "\n" ) }
    textStandardNanRep map { x:String => sb.append("dfdl:textStandardNanRep=" + x + "\n" ) }
    textNumberRoundingMode map { x:RoundingMode => sb.append("dfdl:textNumberRoundingMode=" + x + "\n" ) }
    textZonedSignStyle map { x:ZonedSignStyle => sb.append("dfdl:textZonedSignStyle=" + x + "\n" ) }
    if (textStandardZeroRep!=EmptyValue)
      sb.append("dfdl:textStandardZeroRep=" + textStandardZeroRep+"\n")
    trailingSkipBytes map { x:Int => sb.append("dfdl:trailingSkipBytes=" + x + "\n" ) }
    typeName map { x:String => sb.append("type=" + x + "\n" ) }

    sb toString
 }


  private def writeObject(out:ObjectOutputStream):Unit = {
    out.defaultWriteObject
  }

  private def readObject(in:ObjectInputStream):Unit = {
    in.defaultReadObject
    if (encodingName != null)
      setEncoding(encodingName)
    else 
      encodingValue = None
  }
}

object Format {
	val DEFAULT_FORMAT = "<default>" // name of the default format, parent of all other formats
}