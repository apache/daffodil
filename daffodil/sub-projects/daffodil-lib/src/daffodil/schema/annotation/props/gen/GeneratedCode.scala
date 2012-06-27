package daffodil.schema.annotation.props.gen

////////////////////////////////////////////////////////////////////////////////////////////
// 
// Generated Code - Do not hand modify!
//
// This file is entirely generated code created from the 
// XML Schema files that describe DFDL Annotation syntax.
// 
// Don't edit this. Go fix the generator to create what you need instead.
//  
////////////////////////////////////////////////////////////////////////////////////////////


import daffodil.schema.annotation.props._
    
////////////////////////////////////
// <xsd:simpleType name="DFDLExpression" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 	  <xsd:annotation>
// 		<xsd:documentation>A subset of XPath 2.0 expressions for use in properties</xsd:documentation>
// 	  </xsd:annotation>
// 	   <xsd:restriction base="xsd:token">
// 			<xsd:pattern value="(\{[^\{].*\})|(\{\})"></xsd:pattern>
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait DFDLExpressionMixin { /* nothing */ }
object DFDLExpression {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="EmptyString" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 	<xsd:annotation>
// 		<xsd:documentation>Matches the empty string only. No toleration of whitespace.</xsd:documentation>
// 	  </xsd:annotation>
// 	   <xsd:restriction base="xsd:string">
// 	        <xsd:whiteSpace value="preserve"></xsd:whiteSpace>
// 			<xsd:length value="0"></xsd:length> 
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait EmptyStringMixin { /* nothing */ }
object EmptyString {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="DFDLExpressionOrNothing" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 	  <xsd:annotation>
// 		<xsd:documentation>An expression, or nothing at all.</xsd:documentation>
// 	  </xsd:annotation>
// 	  <xsd:union memberTypes="dfdl:EmptyString dfdl:DFDLExpression"></xsd:union>
// 	  
// 	</xsd:simpleType>


trait DFDLExpressionOrNothingMixin { /* nothing */ }
object DFDLExpressionOrNothing {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="DFDLStringLiteral" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait DFDLStringLiteralMixin { /* nothing */ }
object DFDLStringLiteral {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="ListOfDFDLStringLiteral" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:list itemType="xsd:token"></xsd:list>
// 	</xsd:simpleType>


trait ListOfDFDLStringLiteralMixin { /* nothing */ }
object ListOfDFDLStringLiteral {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="DFDLNonNegativeInteger" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:int">
// 			<xsd:minInclusive value="0"></xsd:minInclusive>
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait DFDLNonNegativeIntegerMixin { /* nothing */ }
object DFDLNonNegativeInteger {
    def apply(s : String) = s.toInt
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="DFDLRegularExpression" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait DFDLRegularExpressionMixin { /* nothing */ }
object DFDLRegularExpression {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="DFDLQName" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:union>
// 			<xsd:simpleType>
// 				<xsd:restriction base="xsd:QName"></xsd:restriction>
// 			</xsd:simpleType>
// 			<xsd:simpleType>
// 				<xsd:restriction base="xsd:string">
// 					<xsd:length value="0"></xsd:length>
// 				</xsd:restriction>
// 			</xsd:simpleType>
// 		</xsd:union>
// 	</xsd:simpleType>


trait DFDLQNameMixin { /* nothing */ }
object DFDLQName {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="EncodingEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string"> 
// 		
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait EncodingMixin { /* nothing */ }
object Encoding {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextStringJustificationEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="left"></xsd:enumeration>
// 			<xsd:enumeration value="right"></xsd:enumeration>
// 			<xsd:enumeration value="center"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextStringJustification extends TextStringJustification.Value
object TextStringJustification extends Enum[TextStringJustification] {
  case object Left extends TextStringJustification ; forceConstruction(Left)
  case object Right extends TextStringJustification ; forceConstruction(Right)
  case object Center extends TextStringJustification ; forceConstruction(Center)

  def apply(name: String) : TextStringJustification = stringToEnum("textStringJustification", name)
}
  
trait TextStringJustificationMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textStringJustification = TextStringJustification(getProperty("textStringJustification"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textStringJustification")
   */
  lazy val optionTextStringJustification = getPropertyOption("textStringJustification")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textStringJustificationToString() = {
    optionTextStringJustification match {
      case None => "" // empty string if not present
      case Some(textStringJustification) =>  "textStringJustification='" + textStringJustification + "' "
    }
  }
    
  def textStringJustificationInit() = {
    registerToStringFunction(textStringJustificationToString)
  }
    
  textStringJustificationInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextNumberJustificationEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="left"></xsd:enumeration>
// 			<xsd:enumeration value="right"></xsd:enumeration>
// 			<xsd:enumeration value="center"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextNumberJustification extends TextNumberJustification.Value
object TextNumberJustification extends Enum[TextNumberJustification] {
  case object Left extends TextNumberJustification ; forceConstruction(Left)
  case object Right extends TextNumberJustification ; forceConstruction(Right)
  case object Center extends TextNumberJustification ; forceConstruction(Center)

  def apply(name: String) : TextNumberJustification = stringToEnum("textNumberJustification", name)
}
  
trait TextNumberJustificationMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textNumberJustification = TextNumberJustification(getProperty("textNumberJustification"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textNumberJustification")
   */
  lazy val optionTextNumberJustification = getPropertyOption("textNumberJustification")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textNumberJustificationToString() = {
    optionTextNumberJustification match {
      case None => "" // empty string if not present
      case Some(textNumberJustification) =>  "textNumberJustification='" + textNumberJustification + "' "
    }
  }
    
  def textNumberJustificationInit() = {
    registerToStringFunction(textNumberJustificationToString)
  }
    
  textNumberJustificationInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextNumberRoundingEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="pattern"></xsd:enumeration>
// 			<xsd:enumeration value="explicit"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextNumberRounding extends TextNumberRounding.Value
object TextNumberRounding extends Enum[TextNumberRounding] {
  case object Pattern extends TextNumberRounding ; forceConstruction(Pattern)
  case object Explicit extends TextNumberRounding ; forceConstruction(Explicit)

  def apply(name: String) : TextNumberRounding = stringToEnum("textNumberRounding", name)
}
  
trait TextNumberRoundingMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textNumberRounding = TextNumberRounding(getProperty("textNumberRounding"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textNumberRounding")
   */
  lazy val optionTextNumberRounding = getPropertyOption("textNumberRounding")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textNumberRoundingToString() = {
    optionTextNumberRounding match {
      case None => "" // empty string if not present
      case Some(textNumberRounding) =>  "textNumberRounding='" + textNumberRounding + "' "
    }
  }
    
  def textNumberRoundingInit() = {
    registerToStringFunction(textNumberRoundingToString)
  }
    
  textNumberRoundingInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="SignCodingValue" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="twosComplement"></xsd:enumeration>
// 			<xsd:enumeration value="onesComplement"></xsd:enumeration>
// 			<xsd:enumeration value="signMagnitude"></xsd:enumeration>
// 			<xsd:enumeration value="unsignedBinary"></xsd:enumeration>
// 			<xsd:enumeration value="unsignedDecimal"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait SignCodingValue extends SignCodingValue.Value
object SignCodingValue extends Enum[SignCodingValue] {
  case object TwosComplement extends SignCodingValue ; forceConstruction(TwosComplement)
  case object OnesComplement extends SignCodingValue ; forceConstruction(OnesComplement)
  case object SignMagnitude extends SignCodingValue ; forceConstruction(SignMagnitude)
  case object UnsignedBinary extends SignCodingValue ; forceConstruction(UnsignedBinary)
  case object UnsignedDecimal extends SignCodingValue ; forceConstruction(UnsignedDecimal)

  def apply(name: String) : SignCodingValue = stringToEnum("signCodingValue", name)
}
  
trait SignCodingValueMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val signCodingValue = SignCodingValue(getProperty("signCodingValue"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("signCodingValue")
   */
  lazy val optionSignCodingValue = getPropertyOption("signCodingValue")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def signCodingValueToString() = {
    optionSignCodingValue match {
      case None => "" // empty string if not present
      case Some(signCodingValue) =>  "signCodingValue='" + signCodingValue + "' "
    }
  }
    
  def signCodingValueInit() = {
    registerToStringFunction(signCodingValueToString)
  }
    
  signCodingValueInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="RepresentationEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="binary"></xsd:enumeration>
// 			<xsd:enumeration value="text"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait Representation extends Representation.Value
object Representation extends Enum[Representation] {
  case object Binary extends Representation ; forceConstruction(Binary)
  case object Text extends Representation ; forceConstruction(Text)

  def apply(name: String) : Representation = stringToEnum("representation", name)
}
  
trait RepresentationMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val representation = Representation(getProperty("representation"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("representation")
   */
  lazy val optionRepresentation = getPropertyOption("representation")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def representationToString() = {
    optionRepresentation match {
      case None => "" // empty string if not present
      case Some(representation) =>  "representation='" + representation + "' "
    }
  }
    
  def representationInit() = {
    registerToStringFunction(representationToString)
  }
    
  representationInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextPadKindEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="none"></xsd:enumeration>
// 			<xsd:enumeration value="padChar"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextPadKind extends TextPadKind.Value
object TextPadKind extends Enum[TextPadKind] {
  case object None extends TextPadKind ; forceConstruction(None)
  case object PadChar extends TextPadKind ; forceConstruction(PadChar)

  def apply(name: String) : TextPadKind = stringToEnum("textPadKind", name)
}
  
trait TextPadKindMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textPadKind = TextPadKind(getProperty("textPadKind"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textPadKind")
   */
  lazy val optionTextPadKind = getPropertyOption("textPadKind")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textPadKindToString() = {
    optionTextPadKind match {
      case None => "" // empty string if not present
      case Some(textPadKind) =>  "textPadKind='" + textPadKind + "' "
    }
  }
    
  def textPadKindInit() = {
    registerToStringFunction(textPadKindToString)
  }
    
  textPadKindInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextTrimKindEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="none"></xsd:enumeration>
// 			<xsd:enumeration value="padChar"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextTrimKind extends TextTrimKind.Value
object TextTrimKind extends Enum[TextTrimKind] {
  case object None extends TextTrimKind ; forceConstruction(None)
  case object PadChar extends TextTrimKind ; forceConstruction(PadChar)

  def apply(name: String) : TextTrimKind = stringToEnum("textTrimKind", name)
}
  
trait TextTrimKindMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textTrimKind = TextTrimKind(getProperty("textTrimKind"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textTrimKind")
   */
  lazy val optionTextTrimKind = getPropertyOption("textTrimKind")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textTrimKindToString() = {
    optionTextTrimKind match {
      case None => "" // empty string if not present
      case Some(textTrimKind) =>  "textTrimKind='" + textTrimKind + "' "
    }
  }
    
  def textTrimKindInit() = {
    registerToStringFunction(textTrimKindToString)
  }
    
  textTrimKindInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="EscapeKindEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="escapeCharacter"></xsd:enumeration>
// 			<xsd:enumeration value="escapeBlock"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait EscapeKind extends EscapeKind.Value
object EscapeKind extends Enum[EscapeKind] {
  case object EscapeCharacter extends EscapeKind ; forceConstruction(EscapeCharacter)
  case object EscapeBlock extends EscapeKind ; forceConstruction(EscapeBlock)

  def apply(name: String) : EscapeKind = stringToEnum("escapeKind", name)
}
  
trait EscapeKindMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val escapeKind = EscapeKind(getProperty("escapeKind"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("escapeKind")
   */
  lazy val optionEscapeKind = getPropertyOption("escapeKind")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def escapeKindToString() = {
    optionEscapeKind match {
      case None => "" // empty string if not present
      case Some(escapeKind) =>  "escapeKind='" + escapeKind + "' "
    }
  }
    
  def escapeKindInit() = {
    registerToStringFunction(escapeKindToString)
  }
    
  escapeKindInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="GenerateEscapeEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="always"></xsd:enumeration>
// 			<xsd:enumeration value="whenNeeded"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait GenerateEscape extends GenerateEscape.Value
object GenerateEscape extends Enum[GenerateEscape] {
  case object Always extends GenerateEscape ; forceConstruction(Always)
  case object WhenNeeded extends GenerateEscape ; forceConstruction(WhenNeeded)

  def apply(name: String) : GenerateEscape = stringToEnum("generateEscape", name)
}
  
trait GenerateEscapeMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val generateEscape = GenerateEscape(getProperty("generateEscape"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("generateEscape")
   */
  lazy val optionGenerateEscape = getPropertyOption("generateEscape")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def generateEscapeToString() = {
    optionGenerateEscape match {
      case None => "" // empty string if not present
      case Some(generateEscape) =>  "generateEscape='" + generateEscape + "' "
    }
  }
    
  def generateEscapeInit() = {
    registerToStringFunction(generateEscapeToString)
  }
    
  generateEscapeInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextBidiTextOrderingEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="implicit"></xsd:enumeration>
// 			<xsd:enumeration value="visual"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextBidiTextOrdering extends TextBidiTextOrdering.Value
object TextBidiTextOrdering extends Enum[TextBidiTextOrdering] {
  case object Implicit extends TextBidiTextOrdering ; forceConstruction(Implicit)
  case object Visual extends TextBidiTextOrdering ; forceConstruction(Visual)

  def apply(name: String) : TextBidiTextOrdering = stringToEnum("textBidiTextOrdering", name)
}
  
trait TextBidiTextOrderingMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textBidiTextOrdering = TextBidiTextOrdering(getProperty("textBidiTextOrdering"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textBidiTextOrdering")
   */
  lazy val optionTextBidiTextOrdering = getPropertyOption("textBidiTextOrdering")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textBidiTextOrderingToString() = {
    optionTextBidiTextOrdering match {
      case None => "" // empty string if not present
      case Some(textBidiTextOrdering) =>  "textBidiTextOrdering='" + textBidiTextOrdering + "' "
    }
  }
    
  def textBidiTextOrderingInit() = {
    registerToStringFunction(textBidiTextOrderingToString)
  }
    
  textBidiTextOrderingInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextBidiOrientationEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="LTR"></xsd:enumeration>
// 			<xsd:enumeration value="RTL"></xsd:enumeration>
// 			<xsd:enumeration value="contextual_LTR"></xsd:enumeration>
// 			<xsd:enumeration value="contextual_RTL"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextBidiOrientation extends TextBidiOrientation.Value
object TextBidiOrientation extends Enum[TextBidiOrientation] {
  case object LTR extends TextBidiOrientation ; forceConstruction(LTR)
  case object RTL extends TextBidiOrientation ; forceConstruction(RTL)
  case object Contextual_LTR extends TextBidiOrientation ; forceConstruction(Contextual_LTR)
  case object Contextual_RTL extends TextBidiOrientation ; forceConstruction(Contextual_RTL)

  def apply(name: String) : TextBidiOrientation = stringToEnum("textBidiOrientation", name)
}
  
trait TextBidiOrientationMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textBidiOrientation = TextBidiOrientation(getProperty("textBidiOrientation"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textBidiOrientation")
   */
  lazy val optionTextBidiOrientation = getPropertyOption("textBidiOrientation")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textBidiOrientationToString() = {
    optionTextBidiOrientation match {
      case None => "" // empty string if not present
      case Some(textBidiOrientation) =>  "textBidiOrientation='" + textBidiOrientation + "' "
    }
  }
    
  def textBidiOrientationInit() = {
    registerToStringFunction(textBidiOrientationToString)
  }
    
  textBidiOrientationInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextBidiNumeralShapesEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="nominal"></xsd:enumeration>
// 			<xsd:enumeration value="national"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextBidiNumeralShapes extends TextBidiNumeralShapes.Value
object TextBidiNumeralShapes extends Enum[TextBidiNumeralShapes] {
  case object Nominal extends TextBidiNumeralShapes ; forceConstruction(Nominal)
  case object National extends TextBidiNumeralShapes ; forceConstruction(National)

  def apply(name: String) : TextBidiNumeralShapes = stringToEnum("textBidiNumeralShapes", name)
}
  
trait TextBidiNumeralShapesMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textBidiNumeralShapes = TextBidiNumeralShapes(getProperty("textBidiNumeralShapes"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textBidiNumeralShapes")
   */
  lazy val optionTextBidiNumeralShapes = getPropertyOption("textBidiNumeralShapes")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textBidiNumeralShapesToString() = {
    optionTextBidiNumeralShapes match {
      case None => "" // empty string if not present
      case Some(textBidiNumeralShapes) =>  "textBidiNumeralShapes='" + textBidiNumeralShapes + "' "
    }
  }
    
  def textBidiNumeralShapesInit() = {
    registerToStringFunction(textBidiNumeralShapesToString)
  }
    
  textBidiNumeralShapesInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextNumberRepEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="standard"></xsd:enumeration>
// 			<xsd:enumeration value="zoned"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextNumberRep extends TextNumberRep.Value
object TextNumberRep extends Enum[TextNumberRep] {
  case object Standard extends TextNumberRep ; forceConstruction(Standard)
  case object Zoned extends TextNumberRep ; forceConstruction(Zoned)

  def apply(name: String) : TextNumberRep = stringToEnum("textNumberRep", name)
}
  
trait TextNumberRepMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textNumberRep = TextNumberRep(getProperty("textNumberRep"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textNumberRep")
   */
  lazy val optionTextNumberRep = getPropertyOption("textNumberRep")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textNumberRepToString() = {
    optionTextNumberRep match {
      case None => "" // empty string if not present
      case Some(textNumberRep) =>  "textNumberRep='" + textNumberRep + "' "
    }
  }
    
  def textNumberRepInit() = {
    registerToStringFunction(textNumberRepToString)
  }
    
  textNumberRepInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextNumberCheckPolicyEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="strict"></xsd:enumeration>
// 			<xsd:enumeration value="lax"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextNumberCheckPolicy extends TextNumberCheckPolicy.Value
object TextNumberCheckPolicy extends Enum[TextNumberCheckPolicy] {
  case object Strict extends TextNumberCheckPolicy ; forceConstruction(Strict)
  case object Lax extends TextNumberCheckPolicy ; forceConstruction(Lax)

  def apply(name: String) : TextNumberCheckPolicy = stringToEnum("textNumberCheckPolicy", name)
}
  
trait TextNumberCheckPolicyMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textNumberCheckPolicy = TextNumberCheckPolicy(getProperty("textNumberCheckPolicy"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textNumberCheckPolicy")
   */
  lazy val optionTextNumberCheckPolicy = getPropertyOption("textNumberCheckPolicy")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textNumberCheckPolicyToString() = {
    optionTextNumberCheckPolicy match {
      case None => "" // empty string if not present
      case Some(textNumberCheckPolicy) =>  "textNumberCheckPolicy='" + textNumberCheckPolicy + "' "
    }
  }
    
  def textNumberCheckPolicyInit() = {
    registerToStringFunction(textNumberCheckPolicyToString)
  }
    
  textNumberCheckPolicyInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="CalendarCheckPolicyEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="strict"></xsd:enumeration>
// 			<xsd:enumeration value="lax"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait CalendarCheckPolicy extends CalendarCheckPolicy.Value
object CalendarCheckPolicy extends Enum[CalendarCheckPolicy] {
  case object Strict extends CalendarCheckPolicy ; forceConstruction(Strict)
  case object Lax extends CalendarCheckPolicy ; forceConstruction(Lax)

  def apply(name: String) : CalendarCheckPolicy = stringToEnum("calendarCheckPolicy", name)
}
  
trait CalendarCheckPolicyMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val calendarCheckPolicy = CalendarCheckPolicy(getProperty("calendarCheckPolicy"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("calendarCheckPolicy")
   */
  lazy val optionCalendarCheckPolicy = getPropertyOption("calendarCheckPolicy")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def calendarCheckPolicyToString() = {
    optionCalendarCheckPolicy match {
      case None => "" // empty string if not present
      case Some(calendarCheckPolicy) =>  "calendarCheckPolicy='" + calendarCheckPolicy + "' "
    }
  }
    
  def calendarCheckPolicyInit() = {
    registerToStringFunction(calendarCheckPolicyToString)
  }
    
  calendarCheckPolicyInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextNumberRoundingModeEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="roundCeiling"></xsd:enumeration>
// 			<xsd:enumeration value="roundFloor"></xsd:enumeration>
// 			<xsd:enumeration value="roundDown"></xsd:enumeration>
// 			<xsd:enumeration value="roundUp"></xsd:enumeration>
// 			<xsd:enumeration value="roundHalfEven"></xsd:enumeration>
// 			<xsd:enumeration value="roundHalfDown"></xsd:enumeration>
// 			<xsd:enumeration value="roundHalfUp"></xsd:enumeration>
// 			<xsd:enumeration value="none"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextNumberRoundingMode extends TextNumberRoundingMode.Value
object TextNumberRoundingMode extends Enum[TextNumberRoundingMode] {
  case object RoundCeiling extends TextNumberRoundingMode ; forceConstruction(RoundCeiling)
  case object RoundFloor extends TextNumberRoundingMode ; forceConstruction(RoundFloor)
  case object RoundDown extends TextNumberRoundingMode ; forceConstruction(RoundDown)
  case object RoundUp extends TextNumberRoundingMode ; forceConstruction(RoundUp)
  case object RoundHalfEven extends TextNumberRoundingMode ; forceConstruction(RoundHalfEven)
  case object RoundHalfDown extends TextNumberRoundingMode ; forceConstruction(RoundHalfDown)
  case object RoundHalfUp extends TextNumberRoundingMode ; forceConstruction(RoundHalfUp)
  case object None extends TextNumberRoundingMode ; forceConstruction(None)

  def apply(name: String) : TextNumberRoundingMode = stringToEnum("textNumberRoundingMode", name)
}
  
trait TextNumberRoundingModeMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textNumberRoundingMode = TextNumberRoundingMode(getProperty("textNumberRoundingMode"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textNumberRoundingMode")
   */
  lazy val optionTextNumberRoundingMode = getPropertyOption("textNumberRoundingMode")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textNumberRoundingModeToString() = {
    optionTextNumberRoundingMode match {
      case None => "" // empty string if not present
      case Some(textNumberRoundingMode) =>  "textNumberRoundingMode='" + textNumberRoundingMode + "' "
    }
  }
    
  def textNumberRoundingModeInit() = {
    registerToStringFunction(textNumberRoundingModeToString)
  }
    
  textNumberRoundingModeInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextZonedSignStyleEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="asciiStandard"></xsd:enumeration>
// 			<xsd:enumeration value="asciiTranslatedEBCDIC"></xsd:enumeration>
// 			<xsd:enumeration value="asciiCARealiaModified"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextZonedSignStyle extends TextZonedSignStyle.Value
object TextZonedSignStyle extends Enum[TextZonedSignStyle] {
  case object AsciiStandard extends TextZonedSignStyle ; forceConstruction(AsciiStandard)
  case object AsciiTranslatedEBCDIC extends TextZonedSignStyle ; forceConstruction(AsciiTranslatedEBCDIC)
  case object AsciiCARealiaModified extends TextZonedSignStyle ; forceConstruction(AsciiCARealiaModified)

  def apply(name: String) : TextZonedSignStyle = stringToEnum("textZonedSignStyle", name)
}
  
trait TextZonedSignStyleMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textZonedSignStyle = TextZonedSignStyle(getProperty("textZonedSignStyle"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textZonedSignStyle")
   */
  lazy val optionTextZonedSignStyle = getPropertyOption("textZonedSignStyle")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textZonedSignStyleToString() = {
    optionTextZonedSignStyle match {
      case None => "" // empty string if not present
      case Some(textZonedSignStyle) =>  "textZonedSignStyle='" + textZonedSignStyle + "' "
    }
  }
    
  def textZonedSignStyleInit() = {
    registerToStringFunction(textZonedSignStyleToString)
  }
    
  textZonedSignStyleInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="BinaryNumberRepEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="packed"></xsd:enumeration>
// 			<xsd:enumeration value="bcd"></xsd:enumeration>
// 			<xsd:enumeration value="binary"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait BinaryNumberRep extends BinaryNumberRep.Value
object BinaryNumberRep extends Enum[BinaryNumberRep] {
  case object Packed extends BinaryNumberRep ; forceConstruction(Packed)
  case object Bcd extends BinaryNumberRep ; forceConstruction(Bcd)
  case object Binary extends BinaryNumberRep ; forceConstruction(Binary)

  def apply(name: String) : BinaryNumberRep = stringToEnum("binaryNumberRep", name)
}
  
trait BinaryNumberRepMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val binaryNumberRep = BinaryNumberRep(getProperty("binaryNumberRep"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("binaryNumberRep")
   */
  lazy val optionBinaryNumberRep = getPropertyOption("binaryNumberRep")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def binaryNumberRepToString() = {
    optionBinaryNumberRep match {
      case None => "" // empty string if not present
      case Some(binaryNumberRep) =>  "binaryNumberRep='" + binaryNumberRep + "' "
    }
  }
    
  def binaryNumberRepInit() = {
    registerToStringFunction(binaryNumberRepToString)
  }
    
  binaryNumberRepInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="BinaryNumberCheckPolicyEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="strict"></xsd:enumeration>
// 			<xsd:enumeration value="lax"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait BinaryNumberCheckPolicy extends BinaryNumberCheckPolicy.Value
object BinaryNumberCheckPolicy extends Enum[BinaryNumberCheckPolicy] {
  case object Strict extends BinaryNumberCheckPolicy ; forceConstruction(Strict)
  case object Lax extends BinaryNumberCheckPolicy ; forceConstruction(Lax)

  def apply(name: String) : BinaryNumberCheckPolicy = stringToEnum("binaryNumberCheckPolicy", name)
}
  
trait BinaryNumberCheckPolicyMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val binaryNumberCheckPolicy = BinaryNumberCheckPolicy(getProperty("binaryNumberCheckPolicy"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("binaryNumberCheckPolicy")
   */
  lazy val optionBinaryNumberCheckPolicy = getPropertyOption("binaryNumberCheckPolicy")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def binaryNumberCheckPolicyToString() = {
    optionBinaryNumberCheckPolicy match {
      case None => "" // empty string if not present
      case Some(binaryNumberCheckPolicy) =>  "binaryNumberCheckPolicy='" + binaryNumberCheckPolicy + "' "
    }
  }
    
  def binaryNumberCheckPolicyInit() = {
    registerToStringFunction(binaryNumberCheckPolicyToString)
  }
    
  binaryNumberCheckPolicyInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="BinaryFloatRepEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="ieee"></xsd:enumeration>
// 			<xsd:enumeration value="ibm390Hex"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait BinaryFloatRep extends BinaryFloatRep.Value
object BinaryFloatRep extends Enum[BinaryFloatRep] {
  case object Ieee extends BinaryFloatRep ; forceConstruction(Ieee)
  case object Ibm390Hex extends BinaryFloatRep ; forceConstruction(Ibm390Hex)

  def apply(name: String) : BinaryFloatRep = stringToEnum("binaryFloatRep", name)
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextBooleanJustificationEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="left"></xsd:enumeration>
// 			<xsd:enumeration value="right"></xsd:enumeration>
// 			<xsd:enumeration value="center"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextBooleanJustification extends TextBooleanJustification.Value
object TextBooleanJustification extends Enum[TextBooleanJustification] {
  case object Left extends TextBooleanJustification ; forceConstruction(Left)
  case object Right extends TextBooleanJustification ; forceConstruction(Right)
  case object Center extends TextBooleanJustification ; forceConstruction(Center)

  def apply(name: String) : TextBooleanJustification = stringToEnum("textBooleanJustification", name)
}
  
trait TextBooleanJustificationMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textBooleanJustification = TextBooleanJustification(getProperty("textBooleanJustification"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textBooleanJustification")
   */
  lazy val optionTextBooleanJustification = getPropertyOption("textBooleanJustification")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textBooleanJustificationToString() = {
    optionTextBooleanJustification match {
      case None => "" // empty string if not present
      case Some(textBooleanJustification) =>  "textBooleanJustification='" + textBooleanJustification + "' "
    }
  }
    
  def textBooleanJustificationInit() = {
    registerToStringFunction(textBooleanJustificationToString)
  }
    
  textBooleanJustificationInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TextCalendarJustificationEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="left"></xsd:enumeration>
// 			<xsd:enumeration value="right"></xsd:enumeration>
// 			<xsd:enumeration value="center"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TextCalendarJustification extends TextCalendarJustification.Value
object TextCalendarJustification extends Enum[TextCalendarJustification] {
  case object Left extends TextCalendarJustification ; forceConstruction(Left)
  case object Right extends TextCalendarJustification ; forceConstruction(Right)
  case object Center extends TextCalendarJustification ; forceConstruction(Center)

  def apply(name: String) : TextCalendarJustification = stringToEnum("textCalendarJustification", name)
}
  
trait TextCalendarJustificationMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val textCalendarJustification = TextCalendarJustification(getProperty("textCalendarJustification"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("textCalendarJustification")
   */
  lazy val optionTextCalendarJustification = getPropertyOption("textCalendarJustification")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def textCalendarJustificationToString() = {
    optionTextCalendarJustification match {
      case None => "" // empty string if not present
      case Some(textCalendarJustification) =>  "textCalendarJustification='" + textCalendarJustification + "' "
    }
  }
    
  def textCalendarJustificationInit() = {
    registerToStringFunction(textCalendarJustificationToString)
  }
    
  textCalendarJustificationInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="CalendarPatternKindEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="explicit"></xsd:enumeration>
// 			<xsd:enumeration value="implicit"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait CalendarPatternKind extends CalendarPatternKind.Value
object CalendarPatternKind extends Enum[CalendarPatternKind] {
  case object Explicit extends CalendarPatternKind ; forceConstruction(Explicit)
  case object Implicit extends CalendarPatternKind ; forceConstruction(Implicit)

  def apply(name: String) : CalendarPatternKind = stringToEnum("calendarPatternKind", name)
}
  
trait CalendarPatternKindMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val calendarPatternKind = CalendarPatternKind(getProperty("calendarPatternKind"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("calendarPatternKind")
   */
  lazy val optionCalendarPatternKind = getPropertyOption("calendarPatternKind")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def calendarPatternKindToString() = {
    optionCalendarPatternKind match {
      case None => "" // empty string if not present
      case Some(calendarPatternKind) =>  "calendarPatternKind='" + calendarPatternKind + "' "
    }
  }
    
  def calendarPatternKindInit() = {
    registerToStringFunction(calendarPatternKindToString)
  }
    
  calendarPatternKindInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="CalendarTimeZoneType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:pattern value="(UTC)([+\-]([01]\d|\d)((([:][0-5]\d){1,2})?))?)"></xsd:pattern>
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait CalendarTimeZoneTypeMixin { /* nothing */ }
object CalendarTimeZoneType {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="CalendarLanguageType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:pattern value="([A-Za-z]{1,8}([\-][A-Za-z0-9]{1,8})*)"></xsd:pattern>
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait CalendarLanguageTypeMixin { /* nothing */ }
object CalendarLanguageType {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="CalendarFirstDayOfWeekEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="Monday"></xsd:enumeration>
// 			<xsd:enumeration value="Tuesday"></xsd:enumeration>
// 			<xsd:enumeration value="Wednesday"></xsd:enumeration>
// 			<xsd:enumeration value="Thursday"></xsd:enumeration>
// 			<xsd:enumeration value="Friday"></xsd:enumeration>
// 			<xsd:enumeration value="Saturday"></xsd:enumeration>
// 			<xsd:enumeration value="Sunday"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait CalendarFirstDayOfWeek extends CalendarFirstDayOfWeek.Value
object CalendarFirstDayOfWeek extends Enum[CalendarFirstDayOfWeek] {
  case object Monday extends CalendarFirstDayOfWeek ; forceConstruction(Monday)
  case object Tuesday extends CalendarFirstDayOfWeek ; forceConstruction(Tuesday)
  case object Wednesday extends CalendarFirstDayOfWeek ; forceConstruction(Wednesday)
  case object Thursday extends CalendarFirstDayOfWeek ; forceConstruction(Thursday)
  case object Friday extends CalendarFirstDayOfWeek ; forceConstruction(Friday)
  case object Saturday extends CalendarFirstDayOfWeek ; forceConstruction(Saturday)
  case object Sunday extends CalendarFirstDayOfWeek ; forceConstruction(Sunday)

  def apply(name: String) : CalendarFirstDayOfWeek = stringToEnum("calendarFirstDayOfWeek", name)
}
  
trait CalendarFirstDayOfWeekMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val calendarFirstDayOfWeek = CalendarFirstDayOfWeek(getProperty("calendarFirstDayOfWeek"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("calendarFirstDayOfWeek")
   */
  lazy val optionCalendarFirstDayOfWeek = getPropertyOption("calendarFirstDayOfWeek")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def calendarFirstDayOfWeekToString() = {
    optionCalendarFirstDayOfWeek match {
      case None => "" // empty string if not present
      case Some(calendarFirstDayOfWeek) =>  "calendarFirstDayOfWeek='" + calendarFirstDayOfWeek + "' "
    }
  }
    
  def calendarFirstDayOfWeekInit() = {
    registerToStringFunction(calendarFirstDayOfWeekToString)
  }
    
  calendarFirstDayOfWeekInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="BinaryCalendarRepEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="packed"></xsd:enumeration>
// 			<xsd:enumeration value="bcd"></xsd:enumeration>
// 			<xsd:enumeration value="binarySeconds"></xsd:enumeration>
// 			<xsd:enumeration value="binaryMilliseconds"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait BinaryCalendarRep extends BinaryCalendarRep.Value
object BinaryCalendarRep extends Enum[BinaryCalendarRep] {
  case object Packed extends BinaryCalendarRep ; forceConstruction(Packed)
  case object Bcd extends BinaryCalendarRep ; forceConstruction(Bcd)
  case object BinarySeconds extends BinaryCalendarRep ; forceConstruction(BinarySeconds)
  case object BinaryMilliseconds extends BinaryCalendarRep ; forceConstruction(BinaryMilliseconds)

  def apply(name: String) : BinaryCalendarRep = stringToEnum("binaryCalendarRep", name)
}
  
trait BinaryCalendarRepMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val binaryCalendarRep = BinaryCalendarRep(getProperty("binaryCalendarRep"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("binaryCalendarRep")
   */
  lazy val optionBinaryCalendarRep = getPropertyOption("binaryCalendarRep")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def binaryCalendarRepToString() = {
    optionBinaryCalendarRep match {
      case None => "" // empty string if not present
      case Some(binaryCalendarRep) =>  "binaryCalendarRep='" + binaryCalendarRep + "' "
    }
  }
    
  def binaryCalendarRepInit() = {
    registerToStringFunction(binaryCalendarRepToString)
  }
    
  binaryCalendarRepInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="LengthKindEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="explicit"></xsd:enumeration>
// 			<xsd:enumeration value="delimited"></xsd:enumeration>
// 			<xsd:enumeration value="implicit"></xsd:enumeration>
// 			<xsd:enumeration value="prefixed"></xsd:enumeration>
// 			<xsd:enumeration value="endOfParent"></xsd:enumeration>
// 			<xsd:enumeration value="pattern"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait LengthKind extends LengthKind.Value
object LengthKind extends Enum[LengthKind] {
  case object Explicit extends LengthKind ; forceConstruction(Explicit)
  case object Delimited extends LengthKind ; forceConstruction(Delimited)
  case object Implicit extends LengthKind ; forceConstruction(Implicit)
  case object Prefixed extends LengthKind ; forceConstruction(Prefixed)
  case object EndOfParent extends LengthKind ; forceConstruction(EndOfParent)
  case object Pattern extends LengthKind ; forceConstruction(Pattern)

  def apply(name: String) : LengthKind = stringToEnum("lengthKind", name)
}
  
trait LengthKindMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val lengthKind = LengthKind(getProperty("lengthKind"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("lengthKind")
   */
  lazy val optionLengthKind = getPropertyOption("lengthKind")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def lengthKindToString() = {
    optionLengthKind match {
      case None => "" // empty string if not present
      case Some(lengthKind) =>  "lengthKind='" + lengthKind + "' "
    }
  }
    
  def lengthKindInit() = {
    registerToStringFunction(lengthKindToString)
  }
    
  lengthKindInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="NilKindEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="literalValue"></xsd:enumeration>
// 			<xsd:enumeration value="logicalValue"></xsd:enumeration>
// 			<xsd:enumeration value="literalCharacter"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait NilKind extends NilKind.Value
object NilKind extends Enum[NilKind] {
  case object LiteralValue extends NilKind ; forceConstruction(LiteralValue)
  case object LogicalValue extends NilKind ; forceConstruction(LogicalValue)
  case object LiteralCharacter extends NilKind ; forceConstruction(LiteralCharacter)

  def apply(name: String) : NilKind = stringToEnum("nilKind", name)
}
  
trait NilKindMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val nilKind = NilKind(getProperty("nilKind"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("nilKind")
   */
  lazy val optionNilKind = getPropertyOption("nilKind")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def nilKindToString() = {
    optionNilKind match {
      case None => "" // empty string if not present
      case Some(nilKind) =>  "nilKind='" + nilKind + "' "
    }
  }
    
  def nilKindInit() = {
    registerToStringFunction(nilKindToString)
  }
    
  nilKindInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="NilValueDelimiterPolicyEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="none"></xsd:enumeration>
// 			<xsd:enumeration value="initiator"></xsd:enumeration>
// 			<xsd:enumeration value="terminator"></xsd:enumeration>
// 			<xsd:enumeration value="both"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait NilValueDelimiterPolicy extends NilValueDelimiterPolicy.Value
object NilValueDelimiterPolicy extends Enum[NilValueDelimiterPolicy] {
  case object None extends NilValueDelimiterPolicy ; forceConstruction(None)
  case object Initiator extends NilValueDelimiterPolicy ; forceConstruction(Initiator)
  case object Terminator extends NilValueDelimiterPolicy ; forceConstruction(Terminator)
  case object Both extends NilValueDelimiterPolicy ; forceConstruction(Both)

  def apply(name: String) : NilValueDelimiterPolicy = stringToEnum("nilValueDelimiterPolicy", name)
}
  
trait NilValueDelimiterPolicyMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val nilValueDelimiterPolicy = NilValueDelimiterPolicy(getProperty("nilValueDelimiterPolicy"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("nilValueDelimiterPolicy")
   */
  lazy val optionNilValueDelimiterPolicy = getPropertyOption("nilValueDelimiterPolicy")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def nilValueDelimiterPolicyToString() = {
    optionNilValueDelimiterPolicy match {
      case None => "" // empty string if not present
      case Some(nilValueDelimiterPolicy) =>  "nilValueDelimiterPolicy='" + nilValueDelimiterPolicy + "' "
    }
  }
    
  def nilValueDelimiterPolicyInit() = {
    registerToStringFunction(nilValueDelimiterPolicyToString)
  }
    
  nilValueDelimiterPolicyInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="EmptyValueDelimiterPolicyEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="none"></xsd:enumeration>
// 			<xsd:enumeration value="initiator"></xsd:enumeration>
// 			<xsd:enumeration value="terminator"></xsd:enumeration>
// 			<xsd:enumeration value="both"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait EmptyValueDelimiterPolicy extends EmptyValueDelimiterPolicy.Value
object EmptyValueDelimiterPolicy extends Enum[EmptyValueDelimiterPolicy] {
  case object None extends EmptyValueDelimiterPolicy ; forceConstruction(None)
  case object Initiator extends EmptyValueDelimiterPolicy ; forceConstruction(Initiator)
  case object Terminator extends EmptyValueDelimiterPolicy ; forceConstruction(Terminator)
  case object Both extends EmptyValueDelimiterPolicy ; forceConstruction(Both)

  def apply(name: String) : EmptyValueDelimiterPolicy = stringToEnum("emptyValueDelimiterPolicy", name)
}
  
trait EmptyValueDelimiterPolicyMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val emptyValueDelimiterPolicy = EmptyValueDelimiterPolicy(getProperty("emptyValueDelimiterPolicy"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("emptyValueDelimiterPolicy")
   */
  lazy val optionEmptyValueDelimiterPolicy = getPropertyOption("emptyValueDelimiterPolicy")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def emptyValueDelimiterPolicyToString() = {
    optionEmptyValueDelimiterPolicy match {
      case None => "" // empty string if not present
      case Some(emptyValueDelimiterPolicy) =>  "emptyValueDelimiterPolicy='" + emptyValueDelimiterPolicy + "' "
    }
  }
    
  def emptyValueDelimiterPolicyInit() = {
    registerToStringFunction(emptyValueDelimiterPolicyToString)
  }
    
  emptyValueDelimiterPolicyInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="SequenceKindEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="ordered"></xsd:enumeration>
// 			<xsd:enumeration value="unordered"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait SequenceKind extends SequenceKind.Value
object SequenceKind extends Enum[SequenceKind] {
  case object Ordered extends SequenceKind ; forceConstruction(Ordered)
  case object Unordered extends SequenceKind ; forceConstruction(Unordered)

  def apply(name: String) : SequenceKind = stringToEnum("sequenceKind", name)
}
  
trait SequenceKindMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val sequenceKind = SequenceKind(getProperty("sequenceKind"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("sequenceKind")
   */
  lazy val optionSequenceKind = getPropertyOption("sequenceKind")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def sequenceKindToString() = {
    optionSequenceKind match {
      case None => "" // empty string if not present
      case Some(sequenceKind) =>  "sequenceKind='" + sequenceKind + "' "
    }
  }
    
  def sequenceKindInit() = {
    registerToStringFunction(sequenceKindToString)
  }
    
  sequenceKindInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="LengthUnitsEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="bits"></xsd:enumeration>
// 			<xsd:enumeration value="bytes"></xsd:enumeration>
// 			<xsd:enumeration value="characters"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait LengthUnits extends LengthUnits.Value
object LengthUnits extends Enum[LengthUnits] {
  case object Bits extends LengthUnits ; forceConstruction(Bits)
  case object Bytes extends LengthUnits ; forceConstruction(Bytes)
  case object Characters extends LengthUnits ; forceConstruction(Characters)

  def apply(name: String) : LengthUnits = stringToEnum("lengthUnits", name)
}
  
trait LengthUnitsMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val lengthUnits = LengthUnits(getProperty("lengthUnits"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("lengthUnits")
   */
  lazy val optionLengthUnits = getPropertyOption("lengthUnits")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def lengthUnitsToString() = {
    optionLengthUnits match {
      case None => "" // empty string if not present
      case Some(lengthUnits) =>  "lengthUnits='" + lengthUnits + "' "
    }
  }
    
  def lengthUnitsInit() = {
    registerToStringFunction(lengthUnitsToString)
  }
    
  lengthUnitsInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="AlignmentUnitsEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="bits"></xsd:enumeration>
// 			<xsd:enumeration value="bytes"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait AlignmentUnits extends AlignmentUnits.Value
object AlignmentUnits extends Enum[AlignmentUnits] {
  case object Bits extends AlignmentUnits ; forceConstruction(Bits)
  case object Bytes extends AlignmentUnits ; forceConstruction(Bytes)

  def apply(name: String) : AlignmentUnits = stringToEnum("alignmentUnits", name)
}
  
trait AlignmentUnitsMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val alignmentUnits = AlignmentUnits(getProperty("alignmentUnits"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("alignmentUnits")
   */
  lazy val optionAlignmentUnits = getPropertyOption("alignmentUnits")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def alignmentUnitsToString() = {
    optionAlignmentUnits match {
      case None => "" // empty string if not present
      case Some(alignmentUnits) =>  "alignmentUnits='" + alignmentUnits + "' "
    }
  }
    
  def alignmentUnitsInit() = {
    registerToStringFunction(alignmentUnitsToString)
  }
    
  alignmentUnitsInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="GenerateQuotesEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="always"></xsd:enumeration>
// 			<xsd:enumeration value="whenNeeded"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait GenerateQuotes extends GenerateQuotes.Value
object GenerateQuotes extends Enum[GenerateQuotes] {
  case object Always extends GenerateQuotes ; forceConstruction(Always)
  case object WhenNeeded extends GenerateQuotes ; forceConstruction(WhenNeeded)

  def apply(name: String) : GenerateQuotes = stringToEnum("generateQuotes", name)
}
  
trait GenerateQuotesMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val generateQuotes = GenerateQuotes(getProperty("generateQuotes"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("generateQuotes")
   */
  lazy val optionGenerateQuotes = getPropertyOption("generateQuotes")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def generateQuotesToString() = {
    optionGenerateQuotes match {
      case None => "" // empty string if not present
      case Some(generateQuotes) =>  "generateQuotes='" + generateQuotes + "' "
    }
  }
    
  def generateQuotesInit() = {
    registerToStringFunction(generateQuotesToString)
  }
    
  generateQuotesInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="SeparatorPositionEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="infix"></xsd:enumeration>
// 			<xsd:enumeration value="prefix"></xsd:enumeration>
// 			<xsd:enumeration value="postfix"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait SeparatorPosition extends SeparatorPosition.Value
object SeparatorPosition extends Enum[SeparatorPosition] {
  case object Infix extends SeparatorPosition ; forceConstruction(Infix)
  case object Prefix extends SeparatorPosition ; forceConstruction(Prefix)
  case object Postfix extends SeparatorPosition ; forceConstruction(Postfix)

  def apply(name: String) : SeparatorPosition = stringToEnum("separatorPosition", name)
}
  
trait SeparatorPositionMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val separatorPosition = SeparatorPosition(getProperty("separatorPosition"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("separatorPosition")
   */
  lazy val optionSeparatorPosition = getPropertyOption("separatorPosition")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def separatorPositionToString() = {
    optionSeparatorPosition match {
      case None => "" // empty string if not present
      case Some(separatorPosition) =>  "separatorPosition='" + separatorPosition + "' "
    }
  }
    
  def separatorPositionInit() = {
    registerToStringFunction(separatorPositionToString)
  }
    
  separatorPositionInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="SeparatorPolicyEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="required"></xsd:enumeration>
// 			<xsd:enumeration value="suppressed"></xsd:enumeration>
// 			<xsd:enumeration value="suppressedAtEndStrict"></xsd:enumeration>
// 			<xsd:enumeration value="suppressedAtEndLax"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait SeparatorPolicy extends SeparatorPolicy.Value
object SeparatorPolicy extends Enum[SeparatorPolicy] {
  case object Required extends SeparatorPolicy ; forceConstruction(Required)
  case object Suppressed extends SeparatorPolicy ; forceConstruction(Suppressed)
  case object SuppressedAtEndStrict extends SeparatorPolicy ; forceConstruction(SuppressedAtEndStrict)
  case object SuppressedAtEndLax extends SeparatorPolicy ; forceConstruction(SuppressedAtEndLax)

  def apply(name: String) : SeparatorPolicy = stringToEnum("separatorPolicy", name)
}
  
trait SeparatorPolicyMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val separatorPolicy = SeparatorPolicy(getProperty("separatorPolicy"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("separatorPolicy")
   */
  lazy val optionSeparatorPolicy = getPropertyOption("separatorPolicy")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def separatorPolicyToString() = {
    optionSeparatorPolicy match {
      case None => "" // empty string if not present
      case Some(separatorPolicy) =>  "separatorPolicy='" + separatorPolicy + "' "
    }
  }
    
  def separatorPolicyInit() = {
    registerToStringFunction(separatorPolicyToString)
  }
    
  separatorPolicyInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="ChoiceLengthKindEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="implicit"></xsd:enumeration>
// 			<xsd:enumeration value="explicit"></xsd:enumeration>
// 		</xsd:restriction>
// 
// 	</xsd:simpleType>

sealed trait ChoiceLengthKind extends ChoiceLengthKind.Value
object ChoiceLengthKind extends Enum[ChoiceLengthKind] {
  case object Implicit extends ChoiceLengthKind ; forceConstruction(Implicit)
  case object Explicit extends ChoiceLengthKind ; forceConstruction(Explicit)

  def apply(name: String) : ChoiceLengthKind = stringToEnum("choiceLengthKind", name)
}
  
trait ChoiceLengthKindMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val choiceLengthKind = ChoiceLengthKind(getProperty("choiceLengthKind"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("choiceLengthKind")
   */
  lazy val optionChoiceLengthKind = getPropertyOption("choiceLengthKind")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def choiceLengthKindToString() = {
    optionChoiceLengthKind match {
      case None => "" // empty string if not present
      case Some(choiceLengthKind) =>  "choiceLengthKind='" + choiceLengthKind + "' "
    }
  }
    
  def choiceLengthKindInit() = {
    registerToStringFunction(choiceLengthKindToString)
  }
    
  choiceLengthKindInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="OccursCountKindEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="fixed"></xsd:enumeration>
// 			<xsd:enumeration value="expression"></xsd:enumeration>
// 			<xsd:enumeration value="parsed"></xsd:enumeration>
// 			<xsd:enumeration value="stopValue"></xsd:enumeration>
// 			<xsd:enumeration value="implicit"></xsd:enumeration>			
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait OccursCountKind extends OccursCountKind.Value
object OccursCountKind extends Enum[OccursCountKind] {
  case object Fixed extends OccursCountKind ; forceConstruction(Fixed)
  case object Expression extends OccursCountKind ; forceConstruction(Expression)
  case object Parsed extends OccursCountKind ; forceConstruction(Parsed)
  case object StopValue extends OccursCountKind ; forceConstruction(StopValue)
  case object Implicit extends OccursCountKind ; forceConstruction(Implicit)

  def apply(name: String) : OccursCountKind = stringToEnum("occursCountKind", name)
}
  
trait OccursCountKindMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val occursCountKind = OccursCountKind(getProperty("occursCountKind"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("occursCountKind")
   */
  lazy val optionOccursCountKind = getPropertyOption("occursCountKind")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def occursCountKindToString() = {
    optionOccursCountKind match {
      case None => "" // empty string if not present
      case Some(occursCountKind) =>  "occursCountKind='" + occursCountKind + "' "
    }
  }
    
  def occursCountKindInit() = {
    registerToStringFunction(occursCountKindToString)
  }
    
  occursCountKindInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="ByteOrderEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="littleEndian"></xsd:enumeration>
// 			<xsd:enumeration value="bigEndian"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait ByteOrder extends ByteOrder.Value
object ByteOrder extends Enum[ByteOrder] {
  case object LittleEndian extends ByteOrder ; forceConstruction(LittleEndian)
  case object BigEndian extends ByteOrder ; forceConstruction(BigEndian)

  def apply(name: String) : ByteOrder = stringToEnum("byteOrder", name)
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="YesNoEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="yes"></xsd:enumeration>
// 			<xsd:enumeration value="no"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait YesNo extends YesNo.Value
object YesNo extends Enum[YesNo] {
  case object Yes extends YesNo ; forceConstruction(Yes)
  case object No extends YesNo ; forceConstruction(No)

  def apply(name: String) : YesNo = stringToEnum("yesNo", name)
}
  
trait YesNoMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val yesNo = YesNo(getProperty("yesNo"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("yesNo")
   */
  lazy val optionYesNo = getPropertyOption("yesNo")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def yesNoToString() = {
    optionYesNo match {
      case None => "" // empty string if not present
      case Some(yesNo) =>  "yesNo='" + yesNo + "' "
    }
  }
    
  def yesNoInit() = {
    registerToStringFunction(yesNoToString)
  }
    
  yesNoInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="UTF16WidthEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="fixed"></xsd:enumeration>
// 			<xsd:enumeration value="variable"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait UTF16Width extends UTF16Width.Value
object UTF16Width extends Enum[UTF16Width] {
  case object Fixed extends UTF16Width ; forceConstruction(Fixed)
  case object Variable extends UTF16Width ; forceConstruction(Variable)

  def apply(name: String) : UTF16Width = stringToEnum("utf16Width", name)
}
  
trait UTF16WidthMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val utf16Width = UTF16Width(getProperty("utf16Width"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("utf16Width")
   */
  lazy val optionUTF16Width = getPropertyOption("utf16Width")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def utf16WidthToString() = {
    optionUTF16Width match {
      case None => "" // empty string if not present
      case Some(utf16Width) =>  "utf16Width='" + utf16Width + "' "
    }
  }
    
  def utf16WidthInit() = {
    registerToStringFunction(utf16WidthToString)
  }
    
  utf16WidthInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="TestKindEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="xsd:string">
// 			<xsd:enumeration value="expression"></xsd:enumeration>
// 			<xsd:enumeration value="pattern"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait TestKind extends TestKind.Value
object TestKind extends Enum[TestKind] {
  case object Expression extends TestKind ; forceConstruction(Expression)
  case object Pattern extends TestKind ; forceConstruction(Pattern)

  def apply(name: String) : TestKind = stringToEnum("testKind", name)
}
  
trait TestKindMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val testKind = TestKind(getProperty("testKind"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("testKind")
   */
  lazy val optionTestKind = getPropertyOption("testKind")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def testKindToString() = {
    optionTestKind match {
      case None => "" // empty string if not present
      case Some(testKind) =>  "testKind='" + testKind + "' "
    }
  }
    
  def testKindInit() = {
    registerToStringFunction(testKindToString)
  }
    
  testKindInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="BinaryPackedSignCodes" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="dfdl:DFDLStringLiteral">
// 			
// 			<xsd:pattern value="(A|C|E|F) (B|D) (F) (A|C|E|F|0)"></xsd:pattern>
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait BinaryPackedSignCodesMixin { /* nothing */ }
object BinaryPackedSignCodes {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="CalendarDaysInFirstWeek" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="dfdl:DFDLNonNegativeInteger">
// 			<xsd:minInclusive value="1"></xsd:minInclusive>
// 			<xsd:maxInclusive value="7"></xsd:maxInclusive>
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait CalendarDaysInFirstWeekMixin { /* nothing */ }
object CalendarDaysInFirstWeek {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="CalendarCenturyStart" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:restriction base="dfdl:DFDLNonNegativeInteger">
// 			<xsd:minInclusive value="0"></xsd:minInclusive>
// 			<xsd:maxInclusive value="99"></xsd:maxInclusive>
// 		</xsd:restriction>
// 	</xsd:simpleType>


trait CalendarCenturyStartMixin { /* nothing */ }
object CalendarCenturyStart {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="ByteOrderEnum_Or_DFDLExpression" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:union>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:DFDLExpression"></xsd:restriction>
// 			</xsd:simpleType>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:ByteOrderEnum"></xsd:restriction>
// 			</xsd:simpleType>
// 		</xsd:union>
// 	</xsd:simpleType>


trait ByteOrderEnum_Or_DFDLExpressionMixin { /* nothing */ }
object ByteOrderEnum_Or_DFDLExpression {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="EncodingEnum_Or_DFDLExpression" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:union>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:DFDLExpression"></xsd:restriction>
// 			</xsd:simpleType>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:EncodingEnum"></xsd:restriction>
// 			</xsd:simpleType>
// 		</xsd:union>
// 	</xsd:simpleType>


trait EncodingEnum_Or_DFDLExpressionMixin { /* nothing */ }
object EncodingEnum_Or_DFDLExpression {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="BinaryFloatRepEnum_Or_DFDLExpression" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:union>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:DFDLExpression"></xsd:restriction>
// 			</xsd:simpleType>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:BinaryFloatRepEnum"></xsd:restriction>
// 			</xsd:simpleType>
// 		</xsd:union>
// 	</xsd:simpleType>


trait BinaryFloatRepEnum_Or_DFDLExpressionMixin { /* nothing */ }
object BinaryFloatRepEnum_Or_DFDLExpression {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="DFDLStringLiteral_Or_DFDLExpression" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:union>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:DFDLExpression"></xsd:restriction>
// 			</xsd:simpleType>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:DFDLStringLiteral"></xsd:restriction>
// 			</xsd:simpleType>
// 			
// 		</xsd:union>
// 	</xsd:simpleType>


trait DFDLStringLiteral_Or_DFDLExpressionMixin { /* nothing */ }
object DFDLStringLiteral_Or_DFDLExpression {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="ListOfDFDLStringLiteral_Or_DFDLExpression" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:union>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:DFDLExpression"></xsd:restriction>
// 			</xsd:simpleType>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:ListOfDFDLStringLiteral"></xsd:restriction>
// 			</xsd:simpleType>
// 		</xsd:union>
// 	</xsd:simpleType>


trait ListOfDFDLStringLiteral_Or_DFDLExpressionMixin { /* nothing */ }
object ListOfDFDLStringLiteral_Or_DFDLExpression {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="DFDLNonNegativeInteger_Or_DFDLExpression" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
// 		<xsd:union>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:DFDLExpression"></xsd:restriction>
// 			</xsd:simpleType>
// 			<xsd:simpleType>
// 				<xsd:restriction base="dfdl:DFDLNonNegativeInteger"></xsd:restriction>
// 			</xsd:simpleType>
// 		</xsd:union>
// 	</xsd:simpleType>


trait DFDLNonNegativeInteger_Or_DFDLExpressionMixin { /* nothing */ }
object DFDLNonNegativeInteger_Or_DFDLExpression {
    def apply(s : String) = s
}
////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="BaseAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="xsd:QName" name="ref"></xsd:attribute>
// 	</xsd:attributeGroup>

trait BaseAGMixin extends PropertyMixin {

  def baseAGInit() : Unit = {
  }
  baseAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="CommonAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:ByteOrderEnum_Or_DFDLExpression" name="byteOrder"></xsd:attribute>
// 		<xsd:attribute type="dfdl:EncodingEnum_Or_DFDLExpression" name="encoding"></xsd:attribute>
// 		<xsd:attribute type="dfdl:UTF16WidthEnum" name="utf16Width"></xsd:attribute>
// 		<xsd:attribute type="dfdl:YesNoEnum" name="ignoreCase"></xsd:attribute>
// 	</xsd:attributeGroup>

trait CommonAGMixin extends PropertyMixin
  with UTF16WidthMixin {
  lazy val ignoreCase = YesNo(getProperty("ignoreCase"))

  def commonAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("byteOrder") match {
        case None => ""
        case Some(value) => "byteOrder='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("encoding") match {
        case None => ""
        case Some(value) => "encoding='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("ignoreCase") match {
        case None => ""
        case Some(value) => "ignoreCase='" + value.toString + "'"
      }
    })
  }
  commonAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="AlignmentAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:AlignmentType" name="alignment"></xsd:attribute>
// 		<xsd:attribute type="dfdl:AlignmentUnitsEnum" name="alignmentUnits"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="fillByte"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLNonNegativeInteger" name="leadingSkip"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLNonNegativeInteger" name="trailingSkip"></xsd:attribute>
// 	</xsd:attributeGroup>

trait AlignmentAGMixin extends PropertyMixin
  with AlignmentUnitsMixin {
  lazy val alignment = AlignmentType(getProperty("alignment"))
  lazy val fillByte = DFDLStringLiteral(getProperty("fillByte"))
  lazy val leadingSkip = DFDLNonNegativeInteger(getProperty("leadingSkip"))
  lazy val trailingSkip = DFDLNonNegativeInteger(getProperty("trailingSkip"))

  def alignmentAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("alignment") match {
        case None => ""
        case Some(value) => "alignment='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("fillByte") match {
        case None => ""
        case Some(value) => "fillByte='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("leadingSkip") match {
        case None => ""
        case Some(value) => "leadingSkip='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("trailingSkip") match {
        case None => ""
        case Some(value) => "trailingSkip='" + value.toString + "'"
      }
    })
  }
  alignmentAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="MarkupAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:ListOfDFDLStringLiteral_Or_DFDLExpression" name="initiator"></xsd:attribute>
// 		<xsd:attribute type="dfdl:ListOfDFDLStringLiteral_Or_DFDLExpression" name="terminator"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="outputNewLine"></xsd:attribute>
// 	</xsd:attributeGroup>

trait MarkupAGMixin extends PropertyMixin {

  def markupAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("initiator") match {
        case None => ""
        case Some(value) => "initiator='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("terminator") match {
        case None => ""
        case Some(value) => "terminator='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("outputNewLine") match {
        case None => ""
        case Some(value) => "outputNewLine='" + value.toString + "'"
      }
    })
  }
  markupAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="NonGroupMarkupAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:EmptyValueDelimiterPolicyEnum" name="emptyValueDelimiterPolicy"></xsd:attribute>
// 	</xsd:attributeGroup>

trait NonGroupMarkupAGMixin extends PropertyMixin
  with EmptyValueDelimiterPolicyMixin {

  def nonGroupMarkupAGInit() : Unit = {
  }
  nonGroupMarkupAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="LengthAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:LengthKindEnum" name="lengthKind"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLNonNegativeInteger_Or_DFDLExpression" name="length"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLRegularExpression" name="lengthPattern"></xsd:attribute>
// 		<xsd:attribute type="dfdl:LengthUnitsEnum" name="lengthUnits"></xsd:attribute>
// 		<xsd:attribute type="dfdl:YesNoEnum" name="prefixIncludesPrefixLength"></xsd:attribute>
// 		<xsd:attribute type="xsd:QName" name="prefixLengthType"></xsd:attribute>
// 	</xsd:attributeGroup>

trait LengthAGMixin extends PropertyMixin
  with LengthKindMixin
  with LengthUnitsMixin {
  lazy val prefixLengthType = convertToQName(getProperty("prefixLengthType"))
  lazy val lengthPattern = DFDLRegularExpression(getProperty("lengthPattern"))
  lazy val prefixIncludesPrefixLength = YesNo(getProperty("prefixIncludesPrefixLength"))

  def lengthAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("length") match {
        case None => ""
        case Some(value) => "length='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("lengthPattern") match {
        case None => ""
        case Some(value) => "lengthPattern='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("prefixIncludesPrefixLength") match {
        case None => ""
        case Some(value) => "prefixIncludesPrefixLength='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("prefixLengthType") match {
        case None => ""
        case Some(value) => "prefixLengthType='" + value.toString + "'"
      }
    })
  }
  lengthAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="RepresentationAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:RepresentationEnum" name="representation"></xsd:attribute>
// 	</xsd:attributeGroup>

trait RepresentationAGMixin extends PropertyMixin
  with RepresentationMixin {

  def representationAGInit() : Unit = {
  }
  representationAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="FloatingAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:YesNoEnum" name="floating"></xsd:attribute>
// 	</xsd:attributeGroup>

trait FloatingAGMixin extends PropertyMixin {
  lazy val floating = YesNo(getProperty("floating"))

  def floatingAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("floating") match {
        case None => ""
        case Some(value) => "floating='" + value.toString + "'"
      }
    })
  }
  floatingAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="SimpleTypesTextAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:TextPadKindEnum" name="textPadKind"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TextTrimKindEnum" name="textTrimKind"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLNonNegativeInteger" name="textOutputMinLength"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLQName" name="escapeSchemeRef"></xsd:attribute>
// 	</xsd:attributeGroup>

trait SimpleTypesTextAGMixin extends PropertyMixin
  with TextPadKindMixin
  with TextTrimKindMixin {
  lazy val textOutputMinLength = DFDLNonNegativeInteger(getProperty("textOutputMinLength"))
  lazy val escapeSchemeRef = DFDLQName(getProperty("escapeSchemeRef"))

  def simpleTypesTextAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("textOutputMinLength") match {
        case None => ""
        case Some(value) => "textOutputMinLength='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("escapeSchemeRef") match {
        case None => ""
        case Some(value) => "escapeSchemeRef='" + value.toString + "'"
      }
    })
  }
  simpleTypesTextAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="EscapeSchemeAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:EscapeKindEnum" name="escapeKind"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral_Or_DFDLExpression" name="escapeCharacter"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="escapeBlockStart"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="escapeBlockEnd"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral_Or_DFDLExpression" name="escapeEscapeCharacter"></xsd:attribute>
// 		<xsd:attribute type="dfdl:ListOfDFDLStringLiteral" name="extraEscapedCharacters"></xsd:attribute>
// 		<xsd:attribute type="dfdl:GenerateEscapeEnum" name="generateEscapeBlock"></xsd:attribute>
// 	</xsd:attributeGroup>

trait EscapeSchemeAGMixin extends PropertyMixin
  with EscapeKindMixin {
  lazy val escapeBlockStart = DFDLStringLiteral(getProperty("escapeBlockStart"))
  lazy val escapeBlockEnd = DFDLStringLiteral(getProperty("escapeBlockEnd"))
  lazy val extraEscapedCharacters = ListOfDFDLStringLiteral(getProperty("extraEscapedCharacters"))
  lazy val generateEscapeBlock = GenerateEscape(getProperty("generateEscapeBlock"))

  def escapeSchemeAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("escapeCharacter") match {
        case None => ""
        case Some(value) => "escapeCharacter='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("escapeBlockStart") match {
        case None => ""
        case Some(value) => "escapeBlockStart='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("escapeBlockEnd") match {
        case None => ""
        case Some(value) => "escapeBlockEnd='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("escapeEscapeCharacter") match {
        case None => ""
        case Some(value) => "escapeEscapeCharacter='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("extraEscapedCharacters") match {
        case None => ""
        case Some(value) => "extraEscapedCharacters='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("generateEscapeBlock") match {
        case None => ""
        case Some(value) => "generateEscapeBlock='" + value.toString + "'"
      }
    })
  }
  escapeSchemeAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="TextBidiSchemeAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:YesNoEnum" name="textBidi"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TextBidiTextOrderingEnum" name="textBidiTextOrdering"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TextBidiOrientationEnum" name="textBidiOrientation"></xsd:attribute>
// 		<xsd:attribute type="dfdl:YesNoEnum" name="textBidiSymmetric"></xsd:attribute>
// 		<xsd:attribute type="dfdl:YesNoEnum" name="textBidiTextShaped"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TextBidiNumeralShapesEnum" name="textBidiNumeralShapes"></xsd:attribute>
// 	</xsd:attributeGroup>

trait TextBidiSchemeAGMixin extends PropertyMixin
  with TextBidiTextOrderingMixin
  with TextBidiOrientationMixin
  with TextBidiNumeralShapesMixin {
  lazy val textBidi = YesNo(getProperty("textBidi"))
  lazy val textBidiSymmetric = YesNo(getProperty("textBidiSymmetric"))
  lazy val textBidiTextShaped = YesNo(getProperty("textBidiTextShaped"))

  def textBidiSchemeAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("textBidi") match {
        case None => ""
        case Some(value) => "textBidi='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textBidiSymmetric") match {
        case None => ""
        case Some(value) => "textBidiSymmetric='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textBidiTextShaped") match {
        case None => ""
        case Some(value) => "textBidiTextShaped='" + value.toString + "'"
      }
    })
  }
  textBidiSchemeAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="StringTextAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:TextStringJustificationEnum" name="textStringJustification"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="textStringPadCharacter"></xsd:attribute>
// 		<xsd:attribute type="dfdl:YesNoEnum" name="truncateSpecifiedLengthString"></xsd:attribute>
// 	</xsd:attributeGroup>

trait StringTextAGMixin extends PropertyMixin
  with TextStringJustificationMixin {
  lazy val textStringPadCharacter = DFDLStringLiteral(getProperty("textStringPadCharacter"))
  lazy val truncateSpecifiedLengthString = YesNo(getProperty("truncateSpecifiedLengthString"))

  def stringTextAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("textStringPadCharacter") match {
        case None => ""
        case Some(value) => "textStringPadCharacter='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("truncateSpecifiedLengthString") match {
        case None => ""
        case Some(value) => "truncateSpecifiedLengthString='" + value.toString + "'"
      }
    })
  }
  stringTextAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="NumberAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:YesNoEnum" name="decimalSigned"></xsd:attribute>
// 	</xsd:attributeGroup>

trait NumberAGMixin extends PropertyMixin {
  lazy val decimalSigned = YesNo(getProperty("decimalSigned"))

  def numberAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("decimalSigned") match {
        case None => ""
        case Some(value) => "decimalSigned='" + value.toString + "'"
      }
    })
  }
  numberAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="NumberTextAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:TextNumberRepEnum" name="textNumberRep"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TextNumberJustificationEnum" name="textNumberJustification"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="textNumberPadCharacter"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TextNumberBaseEnum" name="textStandardBase"></xsd:attribute>
// 	</xsd:attributeGroup>

trait NumberTextAGMixin extends PropertyMixin
  with TextNumberRepMixin
  with TextNumberJustificationMixin {
  lazy val textNumberPadCharacter = DFDLStringLiteral(getProperty("textNumberPadCharacter"))
  lazy val textStandardBase = TextNumberBase(getProperty("textStandardBase"))

  def numberTextAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("textNumberPadCharacter") match {
        case None => ""
        case Some(value) => "textNumberPadCharacter='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textStandardBase") match {
        case None => ""
        case Some(value) => "textStandardBase='" + value.toString + "'"
      }
    })
  }
  numberTextAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="TextNumberFormatAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="textNumberPattern"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral_Or_DFDLExpression" name="textStandardGroupingSeparator"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral_Or_DFDLExpression" name="textStandardDecimalSeparator"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral_Or_DFDLExpression" name="textStandardExponentCharacter"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TextNumberCheckPolicyEnum" name="textNumberCheckPolicy"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="textStandardInfinityRep"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="textStandardNaNRep"></xsd:attribute>		
// 		<xsd:attribute type="dfdl:TextNumberRoundingModeEnum" name="textNumberRoundingMode"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TextNumberRoundingEnum" name="textNumberRounding"></xsd:attribute>
// 		<xsd:attribute type="xsd:double" name="textNumberRoundingIncrement"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TextZonedSignStyleEnum" name="textZonedSignStyle"></xsd:attribute>
// 		<xsd:attribute type="dfdl:ListOfDFDLStringLiteral" name="textStandardZeroRep"></xsd:attribute>
// 	</xsd:attributeGroup>

trait TextNumberFormatAGMixin extends PropertyMixin
  with TextNumberCheckPolicyMixin
  with TextNumberRoundingModeMixin
  with TextNumberRoundingMixin
  with TextZonedSignStyleMixin {
  lazy val textNumberRoundingIncrement = convertToDouble(getProperty("textNumberRoundingIncrement"))
  lazy val textNumberPattern = DFDLStringLiteral(getProperty("textNumberPattern"))
  lazy val textStandardExponentCharacter = DFDLStringLiteral_Or_DFDLExpression(getProperty("textStandardExponentCharacter"))
  lazy val textStandardInfinityRep = DFDLStringLiteral(getProperty("textStandardInfinityRep"))
  lazy val textStandardNaNRep = DFDLStringLiteral(getProperty("textStandardNaNRep"))
  lazy val textStandardZeroRep = ListOfDFDLStringLiteral(getProperty("textStandardZeroRep"))

  def textNumberFormatAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("textNumberPattern") match {
        case None => ""
        case Some(value) => "textNumberPattern='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textStandardGroupingSeparator") match {
        case None => ""
        case Some(value) => "textStandardGroupingSeparator='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textStandardDecimalSeparator") match {
        case None => ""
        case Some(value) => "textStandardDecimalSeparator='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textStandardExponentCharacter") match {
        case None => ""
        case Some(value) => "textStandardExponentCharacter='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textStandardInfinityRep") match {
        case None => ""
        case Some(value) => "textStandardInfinityRep='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textStandardNaNRep") match {
        case None => ""
        case Some(value) => "textStandardNaNRep='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textNumberRoundingIncrement") match {
        case None => ""
        case Some(value) => "textNumberRoundingIncrement='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textStandardZeroRep") match {
        case None => ""
        case Some(value) => "textStandardZeroRep='" + value.toString + "'"
      }
    })
  }
  textNumberFormatAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="NumberBinaryAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:BinaryNumberRepEnum" name="binaryNumberRep"></xsd:attribute>
// 		<xsd:attribute type="xsd:int" name="binaryDecimalVirtualPoint"></xsd:attribute>
// 		<xsd:attribute type="dfdl:BinaryPackedSignCodes" name="binaryPackedSignCodes"></xsd:attribute>
// 		<xsd:attribute type="dfdl:BinaryNumberCheckPolicyEnum" name="binaryNumberCheckPolicy"></xsd:attribute>
// 	</xsd:attributeGroup>

trait NumberBinaryAGMixin extends PropertyMixin
  with BinaryNumberRepMixin
  with BinaryNumberCheckPolicyMixin {
  lazy val binaryDecimalVirtualPoint = convertToInt(getProperty("binaryDecimalVirtualPoint"))
  lazy val binaryPackedSignCodes = BinaryPackedSignCodes(getProperty("binaryPackedSignCodes"))

  def numberBinaryAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("binaryDecimalVirtualPoint") match {
        case None => ""
        case Some(value) => "binaryDecimalVirtualPoint='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("binaryPackedSignCodes") match {
        case None => ""
        case Some(value) => "binaryPackedSignCodes='" + value.toString + "'"
      }
    })
  }
  numberBinaryAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="FloatBinaryAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:BinaryFloatRepEnum_Or_DFDLExpression" name="binaryFloatRep"></xsd:attribute>
// 	</xsd:attributeGroup>

trait FloatBinaryAGMixin extends PropertyMixin {

  def floatBinaryAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("binaryFloatRep") match {
        case None => ""
        case Some(value) => "binaryFloatRep='" + value.toString + "'"
      }
    })
  }
  floatBinaryAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="BooleanTextAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:ListOfDFDLStringLiteral_Or_DFDLExpression" name="textBooleanTrueRep"></xsd:attribute>
// 		<xsd:attribute type="dfdl:ListOfDFDLStringLiteral_Or_DFDLExpression" name="textBooleanFalseRep"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TextBooleanJustificationEnum" name="textBooleanJustification"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="textBooleanPadCharacter"></xsd:attribute>
// 	</xsd:attributeGroup>

trait BooleanTextAGMixin extends PropertyMixin
  with TextBooleanJustificationMixin {
  lazy val textBooleanPadCharacter = DFDLStringLiteral(getProperty("textBooleanPadCharacter"))

  def booleanTextAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("textBooleanTrueRep") match {
        case None => ""
        case Some(value) => "textBooleanTrueRep='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textBooleanFalseRep") match {
        case None => ""
        case Some(value) => "textBooleanFalseRep='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("textBooleanPadCharacter") match {
        case None => ""
        case Some(value) => "textBooleanPadCharacter='" + value.toString + "'"
      }
    })
  }
  booleanTextAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="BooleanBinaryAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:DFDLNonNegativeInteger" name="binaryBooleanTrueRep"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLNonNegativeInteger" name="binaryBooleanFalseRep"></xsd:attribute>
// 	</xsd:attributeGroup>

trait BooleanBinaryAGMixin extends PropertyMixin {
  lazy val binaryBooleanTrueRep = DFDLNonNegativeInteger(getProperty("binaryBooleanTrueRep"))
  lazy val binaryBooleanFalseRep = DFDLNonNegativeInteger(getProperty("binaryBooleanFalseRep"))

  def booleanBinaryAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("binaryBooleanTrueRep") match {
        case None => ""
        case Some(value) => "binaryBooleanTrueRep='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("binaryBooleanFalseRep") match {
        case None => ""
        case Some(value) => "binaryBooleanFalseRep='" + value.toString + "'"
      }
    })
  }
  booleanBinaryAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="CalendarFormatAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="calendarPattern"></xsd:attribute>
// 		<xsd:attribute type="dfdl:CalendarPatternKindEnum" name="calendarPatternKind"></xsd:attribute>
// 		<xsd:attribute type="dfdl:CalendarCheckPolicyEnum" name="calendarCheckPolicy"></xsd:attribute>
// 		<xsd:attribute type="dfdl:CalendarTimeZoneType" name="calendarTimeZone"></xsd:attribute>
// 		<xsd:attribute type="dfdl:YesNoEnum" name="calendarObserveDST"></xsd:attribute>		
// 		<xsd:attribute type="dfdl:CalendarFirstDayOfWeekEnum" name="calendarFirstDayOfWeek"></xsd:attribute>
// 		<xsd:attribute type="dfdl:CalendarDaysInFirstWeek" name="calendarDaysInFirstWeek"></xsd:attribute>
// 		<xsd:attribute type="dfdl:CalendarCenturyStart" name="calendarCenturyStart"></xsd:attribute>
// 		<xsd:attribute type="dfdl:CalendarLanguageType" name="calendarLanguage"></xsd:attribute>
// 	</xsd:attributeGroup>

trait CalendarFormatAGMixin extends PropertyMixin
  with CalendarPatternKindMixin
  with CalendarCheckPolicyMixin
  with CalendarFirstDayOfWeekMixin {
  lazy val calendarPattern = DFDLStringLiteral(getProperty("calendarPattern"))
  lazy val calendarTimeZone = CalendarTimeZoneType(getProperty("calendarTimeZone"))
  lazy val calendarObserveDST = YesNo(getProperty("calendarObserveDST"))
  lazy val calendarDaysInFirstWeek = CalendarDaysInFirstWeek(getProperty("calendarDaysInFirstWeek"))
  lazy val calendarCenturyStart = CalendarCenturyStart(getProperty("calendarCenturyStart"))
  lazy val calendarLanguage = CalendarLanguageType(getProperty("calendarLanguage"))

  def calendarFormatAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("calendarPattern") match {
        case None => ""
        case Some(value) => "calendarPattern='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("calendarTimeZone") match {
        case None => ""
        case Some(value) => "calendarTimeZone='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("calendarObserveDST") match {
        case None => ""
        case Some(value) => "calendarObserveDST='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("calendarDaysInFirstWeek") match {
        case None => ""
        case Some(value) => "calendarDaysInFirstWeek='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("calendarCenturyStart") match {
        case None => ""
        case Some(value) => "calendarCenturyStart='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("calendarLanguage") match {
        case None => ""
        case Some(value) => "calendarLanguage='" + value.toString + "'"
      }
    })
  }
  calendarFormatAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="CalendarTextAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:TextCalendarJustificationEnum" name="textCalendarJustification"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral" name="textCalendarPadCharacter"></xsd:attribute>
// 	</xsd:attributeGroup>

trait CalendarTextAGMixin extends PropertyMixin
  with TextCalendarJustificationMixin {
  lazy val textCalendarPadCharacter = DFDLStringLiteral(getProperty("textCalendarPadCharacter"))

  def calendarTextAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("textCalendarPadCharacter") match {
        case None => ""
        case Some(value) => "textCalendarPadCharacter='" + value.toString + "'"
      }
    })
  }
  calendarTextAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="CalendarBinaryAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:BinaryCalendarRepEnum" name="binaryCalendarRep"></xsd:attribute>
// 		<xsd:attribute type="xsd:string" name="binaryCalendarEpoch"></xsd:attribute>
// 	</xsd:attributeGroup>

trait CalendarBinaryAGMixin extends PropertyMixin
  with BinaryCalendarRepMixin {
  lazy val binaryCalendarEpoch = convertToString(getProperty("binaryCalendarEpoch"))

  def calendarBinaryAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("binaryCalendarEpoch") match {
        case None => ""
        case Some(value) => "binaryCalendarEpoch='" + value.toString + "'"
      }
    })
  }
  calendarBinaryAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="OpaqueAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 	</xsd:attributeGroup>

trait OpaqueAGMixin extends PropertyMixin {

  def opaqueAGInit() : Unit = {
  }
  opaqueAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="NillableAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:NilKindEnum" name="nilKind"></xsd:attribute>
// 		<xsd:attribute type="dfdl:ListOfDFDLStringLiteral" name="nilValue"></xsd:attribute>
// 		<xsd:attribute type="dfdl:NilValueDelimiterPolicyEnum" name="nilValueDelimiterPolicy"></xsd:attribute>
// 	</xsd:attributeGroup>

trait NillableAGMixin extends PropertyMixin
  with NilKindMixin
  with NilValueDelimiterPolicyMixin {
  lazy val nilValue = ListOfDFDLStringLiteral(getProperty("nilValue"))

  def nillableAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("nilValue") match {
        case None => ""
        case Some(value) => "nilValue='" + value.toString + "'"
      }
    })
  }
  nillableAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="DefaultValueControlAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:YesNoEnum" name="useNilForDefault"></xsd:attribute>
// 	</xsd:attributeGroup>

trait DefaultValueControlAGMixin extends PropertyMixin {
  lazy val useNilForDefault = YesNo(getProperty("useNilForDefault"))

  def defaultValueControlAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("useNilForDefault") match {
        case None => ""
        case Some(value) => "useNilForDefault='" + value.toString + "'"
      }
    })
  }
  defaultValueControlAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="SequenceAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:SequenceKindEnum" name="sequenceKind"></xsd:attribute>
// 		<xsd:attribute type="xsd:QName" name="hiddenGroupRef"></xsd:attribute>
// 	</xsd:attributeGroup>

trait SequenceAGMixin extends PropertyMixin
  with SequenceKindMixin {

  def sequenceAGInit() : Unit = {
  }
  sequenceAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="SeparatorAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:ListOfDFDLStringLiteral_Or_DFDLExpression" name="separator"></xsd:attribute>
// 		<xsd:attribute type="dfdl:SeparatorPositionEnum" name="separatorPosition"></xsd:attribute>
// 		<xsd:attribute type="dfdl:SeparatorPolicyEnum" name="separatorPolicy"></xsd:attribute>
// 	</xsd:attributeGroup>

trait SeparatorAGMixin extends PropertyMixin
  with SeparatorPositionMixin
  with SeparatorPolicyMixin {

  def separatorAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("separator") match {
        case None => ""
        case Some(value) => "separator='" + value.toString + "'"
      }
    })
  }
  separatorAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="GroupCommonAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:YesNoEnum" name="initiatedContent"></xsd:attribute>
// 	</xsd:attributeGroup>

trait GroupCommonAGMixin extends PropertyMixin {
  lazy val initiatedContent = YesNo(getProperty("initiatedContent"))

  def groupCommonAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("initiatedContent") match {
        case None => ""
        case Some(value) => "initiatedContent='" + value.toString + "'"
      }
    })
  }
  groupCommonAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="ChoiceAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:ChoiceLengthKindEnum" name="choiceLengthKind"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLNonNegativeInteger" name="choiceLength"></xsd:attribute>
// 	</xsd:attributeGroup>

trait ChoiceAGMixin extends PropertyMixin
  with ChoiceLengthKindMixin {
  lazy val choiceLength = DFDLNonNegativeInteger(getProperty("choiceLength"))

  def choiceAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("choiceLength") match {
        case None => ""
        case Some(value) => "choiceLength='" + value.toString + "'"
      }
    })
  }
  choiceAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="OccursAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:OccursCountKindEnum" name="occursCountKind"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLExpression" name="occursCount"></xsd:attribute>
// 		<xsd:attribute type="dfdl:ListOfDFDLStringLiteral" name="occursStopValue"></xsd:attribute>
// 	</xsd:attributeGroup>

trait OccursAGMixin extends PropertyMixin
  with OccursCountKindMixin {
  lazy val occursStopValue = ListOfDFDLStringLiteral(getProperty("occursStopValue"))

  def occursAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("occursCount") match {
        case None => ""
        case Some(value) => "occursCount='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("occursStopValue") match {
        case None => ""
        case Some(value) => "occursStopValue='" + value.toString + "'"
      }
    })
  }
  occursAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="CalculatedValueAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:DFDLExpression" name="inputValueCalc"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLExpression" name="outputValueCalc"></xsd:attribute>
// 	</xsd:attributeGroup>

trait CalculatedValueAGMixin extends PropertyMixin {

  def calculatedValueAGInit() : Unit = {
  }
  calculatedValueAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="DocumentPropertiesAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:YesNoEnum" name="documentFinalTerminatorCanBeMissing"></xsd:attribute>
// 	</xsd:attributeGroup>

trait DocumentPropertiesAGMixin extends PropertyMixin {
  lazy val documentFinalTerminatorCanBeMissing = YesNo(getProperty("documentFinalTerminatorCanBeMissing"))

  def documentPropertiesAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("documentFinalTerminatorCanBeMissing") match {
        case None => ""
        case Some(value) => "documentFinalTerminatorCanBeMissing='" + value.toString + "'"
      }
    })
  }
  documentPropertiesAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:simpleType name="PropertyNameType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:restriction base="xsd:string">
// 
// 			
// 			<xsd:enumeration value="initiator"></xsd:enumeration>
// 			<xsd:enumeration value="terminator"></xsd:enumeration>
// 			<xsd:enumeration value="documentFinalTerminatorCanBeMissing"></xsd:enumeration>
// 			<xsd:enumeration value="outputNewLine"></xsd:enumeration>
// 			<xsd:enumeration value="length"></xsd:enumeration>
// 			<xsd:enumeration value="lengthPattern"></xsd:enumeration>
// 			<xsd:enumeration value="textStringPadCharacter"></xsd:enumeration>
// 			<xsd:enumeration value="textNumberPadCharacter"></xsd:enumeration>
// 			<xsd:enumeration value="textCalendarPadCharacter"></xsd:enumeration>
// 			<xsd:enumeration value="textBooleanPadCharacter"></xsd:enumeration>
// 			<xsd:enumeration value="escapeCharacter"></xsd:enumeration>
// 			<xsd:enumeration value="escapeBlockStart"></xsd:enumeration>
// 			<xsd:enumeration value="escapeBlockEnd"></xsd:enumeration>
// 			<xsd:enumeration value="escapeEscapeCharacter"></xsd:enumeration>
// 			<xsd:enumeration value="extraEscapedCharacters"></xsd:enumeration>
// 			<xsd:enumeration value="textNumberPattern"></xsd:enumeration>
// 			<xsd:enumeration value="textStandardGroupingSeparator"></xsd:enumeration>
// 			<xsd:enumeration value="textStandardDecimalSeparator"></xsd:enumeration>
// 			<xsd:enumeration value="textStandardExponentCharacter"></xsd:enumeration>
// 			<xsd:enumeration value="textStandardInfinityRep"></xsd:enumeration>
// 			<xsd:enumeration value="textStandardNaNRep"></xsd:enumeration>
// 			<xsd:enumeration value="textStandardZeroRep"></xsd:enumeration>
// 			<xsd:enumeration value="textBooleanTrueRep"></xsd:enumeration>
// 			<xsd:enumeration value="textBooleanFalseRep"></xsd:enumeration>
// 			<xsd:enumeration value="calendarPattern"></xsd:enumeration>
// 			<xsd:enumeration value="calendarLanguage"></xsd:enumeration>
// 			<xsd:enumeration value="binaryCalendarEpoch"></xsd:enumeration>
// 			<xsd:enumeration value="nilValue"></xsd:enumeration>
// 			<xsd:enumeration value="separator"></xsd:enumeration>
// 			<xsd:enumeration value="occursStopValue"></xsd:enumeration>
// 			<xsd:enumeration value="inputValueCalc"></xsd:enumeration>
// 			<xsd:enumeration value="outputValueCalc"></xsd:enumeration>
// 
//         	      
// 	      	<xsd:enumeration value="textBidi"></xsd:enumeration>
// 	        	<xsd:enumeration value="textBidiTextOrdering"></xsd:enumeration>
// 	        	<xsd:enumeration value="textBidiOrientation"></xsd:enumeration>
// 	        	<xsd:enumeration value="textBidiSymmetric"></xsd:enumeration>
// 	        	<xsd:enumeration value="textBidiTextShaped"></xsd:enumeration>
// 	        	<xsd:enumeration value="textBidiNumeralShapes"></xsd:enumeration>
// 
// 			
// 			<xsd:enumeration value="byteOrder"></xsd:enumeration>
// 			<xsd:enumeration value="encoding"></xsd:enumeration>
// 			<xsd:enumeration value="utf16Width"></xsd:enumeration>
// 			<xsd:enumeration value="ignoreCase"></xsd:enumeration>
// 
// 			<xsd:enumeration value="alignment"></xsd:enumeration>
// 			<xsd:enumeration value="alignmentUnits"></xsd:enumeration>
// 			<xsd:enumeration value="fillByte"></xsd:enumeration>
// 			<xsd:enumeration value="leadingSkip"></xsd:enumeration>
// 			<xsd:enumeration value="trailingSkip"></xsd:enumeration>
// 
// 			<xsd:enumeration value="lengthKind"></xsd:enumeration>
// 			<xsd:enumeration value="lengthUnits"></xsd:enumeration>
// 
// 			<xsd:enumeration value="prefixIncludesPrefixLength"></xsd:enumeration>
// 			<xsd:enumeration value="prefixLengthType"></xsd:enumeration>
// 
// 			<xsd:enumeration value="representation"></xsd:enumeration>
// 
// 			<xsd:enumeration value="textPadKind"></xsd:enumeration>
// 			<xsd:enumeration value="textTrimKind"></xsd:enumeration>
// 			<xsd:enumeration value="textOutputMinLength"></xsd:enumeration>
// 
// 			<xsd:enumeration value="escapeKind"></xsd:enumeration>
// 			<xsd:enumeration value="generateEscapeBlock"></xsd:enumeration>
// 
// 			<xsd:enumeration value="textStringJustification"></xsd:enumeration>
// 			<xsd:enumeration value="textNumberRep"></xsd:enumeration>
// 			<xsd:enumeration value="textNumberJustification"></xsd:enumeration>		
// 
// 			<xsd:enumeration value="textNumberCheckPolicy"></xsd:enumeration>
// 			<xsd:enumeration value="textStandardBase"></xsd:enumeration>
// 			<xsd:enumeration value="textNumberRoundingMode"></xsd:enumeration>
// 			<xsd:enumeration value="textNumberRounding"></xsd:enumeration>
// 			<xsd:enumeration value="textNumberRoundingIncrement"></xsd:enumeration>
// 			<xsd:enumeration value="textZonedSignStyle"></xsd:enumeration>	
// 
// 			<xsd:enumeration value="binaryNumberRep"></xsd:enumeration>
// 			<xsd:enumeration value="binaryDecimalVirtualPoint"></xsd:enumeration>
// 			<xsd:enumeration value="binaryNumberCheckPolicy"></xsd:enumeration>
// 			<xsd:enumeration value="binaryPackedSignCodes"></xsd:enumeration>
// 			<xsd:enumeration value="binaryFloatRepresentation"></xsd:enumeration>	
// 
// 			<xsd:enumeration value="textBooleanJustification"></xsd:enumeration>
// 
// 			<xsd:enumeration value="integerBooleanTrueRep"></xsd:enumeration>
// 			<xsd:enumeration value="integerBooleanFalseRep"></xsd:enumeration>	
// 
// 			<xsd:enumeration value="textCalendarJustification"></xsd:enumeration>
// 
// 			<xsd:enumeration value="calendarPatternKind"></xsd:enumeration>
// 			<xsd:enumeration value="calendarCheckPolicy"></xsd:enumeration>
// 			<xsd:enumeration value="calendarTimeZone"></xsd:enumeration>
// 			<xsd:enumeration value="calendarObserveDST"></xsd:enumeration>			
// 			<xsd:enumeration value="calendarFirstDayOfWeek"></xsd:enumeration>
// 			<xsd:enumeration value="calendarDaysInFirstWeek"></xsd:enumeration>
// 			<xsd:enumeration value="calendarCenturyStart"></xsd:enumeration>		
// 			<xsd:enumeration value="binaryCalendarRep"></xsd:enumeration>
// 
// 			<xsd:enumeration value="nilKind"></xsd:enumeration>
// 			<xsd:enumeration value="nilValueDelimiterPolicy"></xsd:enumeration>
// 
// 			<xsd:enumeration value="useNilForDefault"></xsd:enumeration>
// 			<xsd:enumeration value="emptyValueDelimiterPolicy"></xsd:enumeration>
// 
// 			<xsd:enumeration value="sequenceKind"></xsd:enumeration>
// 			<xsd:enumeration value="hiddenGroupRef"></xsd:enumeration>
// 			<xsd:enumeration value="initiatedContent"></xsd:enumeration>
// 
// 			<xsd:enumeration value="separatorPosition"></xsd:enumeration>
// 			<xsd:enumeration value="separatorPolicy"></xsd:enumeration>
// 
// 			<xsd:enumeration value="choiceLengthKind"></xsd:enumeration>
// 			<xsd:enumeration value="choiceLength"></xsd:enumeration>
// 
// 			<xsd:enumeration value="occursCountKind"></xsd:enumeration>
// 			<xsd:enumeration value="occursCount"></xsd:enumeration>
// 			
// 			<xsd:enumeration value="floating"></xsd:enumeration>
// 			<xsd:enumeration value="truncateSpecifiedLengthString"></xsd:enumeration>
// 					
// 			<xsd:enumeration value="decimalSigned"></xsd:enumeration>
// 		</xsd:restriction>
// 	</xsd:simpleType>

sealed trait PropertyNameType extends PropertyNameType.Value
object PropertyNameType extends Enum[PropertyNameType] {
  case object Initiator extends PropertyNameType ; forceConstruction(Initiator)
  case object Terminator extends PropertyNameType ; forceConstruction(Terminator)
  case object DocumentFinalTerminatorCanBeMissing extends PropertyNameType ; forceConstruction(DocumentFinalTerminatorCanBeMissing)
  case object OutputNewLine extends PropertyNameType ; forceConstruction(OutputNewLine)
  case object Length extends PropertyNameType ; forceConstruction(Length)
  case object LengthPattern extends PropertyNameType ; forceConstruction(LengthPattern)
  case object TextStringPadCharacter extends PropertyNameType ; forceConstruction(TextStringPadCharacter)
  case object TextNumberPadCharacter extends PropertyNameType ; forceConstruction(TextNumberPadCharacter)
  case object TextCalendarPadCharacter extends PropertyNameType ; forceConstruction(TextCalendarPadCharacter)
  case object TextBooleanPadCharacter extends PropertyNameType ; forceConstruction(TextBooleanPadCharacter)
  case object EscapeCharacter extends PropertyNameType ; forceConstruction(EscapeCharacter)
  case object EscapeBlockStart extends PropertyNameType ; forceConstruction(EscapeBlockStart)
  case object EscapeBlockEnd extends PropertyNameType ; forceConstruction(EscapeBlockEnd)
  case object EscapeEscapeCharacter extends PropertyNameType ; forceConstruction(EscapeEscapeCharacter)
  case object ExtraEscapedCharacters extends PropertyNameType ; forceConstruction(ExtraEscapedCharacters)
  case object TextNumberPattern extends PropertyNameType ; forceConstruction(TextNumberPattern)
  case object TextStandardGroupingSeparator extends PropertyNameType ; forceConstruction(TextStandardGroupingSeparator)
  case object TextStandardDecimalSeparator extends PropertyNameType ; forceConstruction(TextStandardDecimalSeparator)
  case object TextStandardExponentCharacter extends PropertyNameType ; forceConstruction(TextStandardExponentCharacter)
  case object TextStandardInfinityRep extends PropertyNameType ; forceConstruction(TextStandardInfinityRep)
  case object TextStandardNaNRep extends PropertyNameType ; forceConstruction(TextStandardNaNRep)
  case object TextStandardZeroRep extends PropertyNameType ; forceConstruction(TextStandardZeroRep)
  case object TextBooleanTrueRep extends PropertyNameType ; forceConstruction(TextBooleanTrueRep)
  case object TextBooleanFalseRep extends PropertyNameType ; forceConstruction(TextBooleanFalseRep)
  case object CalendarPattern extends PropertyNameType ; forceConstruction(CalendarPattern)
  case object CalendarLanguage extends PropertyNameType ; forceConstruction(CalendarLanguage)
  case object BinaryCalendarEpoch extends PropertyNameType ; forceConstruction(BinaryCalendarEpoch)
  case object NilValue extends PropertyNameType ; forceConstruction(NilValue)
  case object Separator extends PropertyNameType ; forceConstruction(Separator)
  case object OccursStopValue extends PropertyNameType ; forceConstruction(OccursStopValue)
  case object InputValueCalc extends PropertyNameType ; forceConstruction(InputValueCalc)
  case object OutputValueCalc extends PropertyNameType ; forceConstruction(OutputValueCalc)
  case object TextBidi extends PropertyNameType ; forceConstruction(TextBidi)
  case object TextBidiTextOrdering extends PropertyNameType ; forceConstruction(TextBidiTextOrdering)
  case object TextBidiOrientation extends PropertyNameType ; forceConstruction(TextBidiOrientation)
  case object TextBidiSymmetric extends PropertyNameType ; forceConstruction(TextBidiSymmetric)
  case object TextBidiTextShaped extends PropertyNameType ; forceConstruction(TextBidiTextShaped)
  case object TextBidiNumeralShapes extends PropertyNameType ; forceConstruction(TextBidiNumeralShapes)
  case object ByteOrder extends PropertyNameType ; forceConstruction(ByteOrder)
  case object Encoding extends PropertyNameType ; forceConstruction(Encoding)
  case object Utf16Width extends PropertyNameType ; forceConstruction(Utf16Width)
  case object IgnoreCase extends PropertyNameType ; forceConstruction(IgnoreCase)
  case object Alignment extends PropertyNameType ; forceConstruction(Alignment)
  case object AlignmentUnits extends PropertyNameType ; forceConstruction(AlignmentUnits)
  case object FillByte extends PropertyNameType ; forceConstruction(FillByte)
  case object LeadingSkip extends PropertyNameType ; forceConstruction(LeadingSkip)
  case object TrailingSkip extends PropertyNameType ; forceConstruction(TrailingSkip)
  case object LengthKind extends PropertyNameType ; forceConstruction(LengthKind)
  case object LengthUnits extends PropertyNameType ; forceConstruction(LengthUnits)
  case object PrefixIncludesPrefixLength extends PropertyNameType ; forceConstruction(PrefixIncludesPrefixLength)
  case object PrefixLengthType extends PropertyNameType ; forceConstruction(PrefixLengthType)
  case object Representation extends PropertyNameType ; forceConstruction(Representation)
  case object TextPadKind extends PropertyNameType ; forceConstruction(TextPadKind)
  case object TextTrimKind extends PropertyNameType ; forceConstruction(TextTrimKind)
  case object TextOutputMinLength extends PropertyNameType ; forceConstruction(TextOutputMinLength)
  case object EscapeKind extends PropertyNameType ; forceConstruction(EscapeKind)
  case object GenerateEscapeBlock extends PropertyNameType ; forceConstruction(GenerateEscapeBlock)
  case object TextStringJustification extends PropertyNameType ; forceConstruction(TextStringJustification)
  case object TextNumberRep extends PropertyNameType ; forceConstruction(TextNumberRep)
  case object TextNumberJustification extends PropertyNameType ; forceConstruction(TextNumberJustification)
  case object TextNumberCheckPolicy extends PropertyNameType ; forceConstruction(TextNumberCheckPolicy)
  case object TextStandardBase extends PropertyNameType ; forceConstruction(TextStandardBase)
  case object TextNumberRoundingMode extends PropertyNameType ; forceConstruction(TextNumberRoundingMode)
  case object TextNumberRounding extends PropertyNameType ; forceConstruction(TextNumberRounding)
  case object TextNumberRoundingIncrement extends PropertyNameType ; forceConstruction(TextNumberRoundingIncrement)
  case object TextZonedSignStyle extends PropertyNameType ; forceConstruction(TextZonedSignStyle)
  case object BinaryNumberRep extends PropertyNameType ; forceConstruction(BinaryNumberRep)
  case object BinaryDecimalVirtualPoint extends PropertyNameType ; forceConstruction(BinaryDecimalVirtualPoint)
  case object BinaryNumberCheckPolicy extends PropertyNameType ; forceConstruction(BinaryNumberCheckPolicy)
  case object BinaryPackedSignCodes extends PropertyNameType ; forceConstruction(BinaryPackedSignCodes)
  case object BinaryFloatRepresentation extends PropertyNameType ; forceConstruction(BinaryFloatRepresentation)
  case object TextBooleanJustification extends PropertyNameType ; forceConstruction(TextBooleanJustification)
  case object IntegerBooleanTrueRep extends PropertyNameType ; forceConstruction(IntegerBooleanTrueRep)
  case object IntegerBooleanFalseRep extends PropertyNameType ; forceConstruction(IntegerBooleanFalseRep)
  case object TextCalendarJustification extends PropertyNameType ; forceConstruction(TextCalendarJustification)
  case object CalendarPatternKind extends PropertyNameType ; forceConstruction(CalendarPatternKind)
  case object CalendarCheckPolicy extends PropertyNameType ; forceConstruction(CalendarCheckPolicy)
  case object CalendarTimeZone extends PropertyNameType ; forceConstruction(CalendarTimeZone)
  case object CalendarObserveDST extends PropertyNameType ; forceConstruction(CalendarObserveDST)
  case object CalendarFirstDayOfWeek extends PropertyNameType ; forceConstruction(CalendarFirstDayOfWeek)
  case object CalendarDaysInFirstWeek extends PropertyNameType ; forceConstruction(CalendarDaysInFirstWeek)
  case object CalendarCenturyStart extends PropertyNameType ; forceConstruction(CalendarCenturyStart)
  case object BinaryCalendarRep extends PropertyNameType ; forceConstruction(BinaryCalendarRep)
  case object NilKind extends PropertyNameType ; forceConstruction(NilKind)
  case object NilValueDelimiterPolicy extends PropertyNameType ; forceConstruction(NilValueDelimiterPolicy)
  case object UseNilForDefault extends PropertyNameType ; forceConstruction(UseNilForDefault)
  case object EmptyValueDelimiterPolicy extends PropertyNameType ; forceConstruction(EmptyValueDelimiterPolicy)
  case object SequenceKind extends PropertyNameType ; forceConstruction(SequenceKind)
  case object HiddenGroupRef extends PropertyNameType ; forceConstruction(HiddenGroupRef)
  case object InitiatedContent extends PropertyNameType ; forceConstruction(InitiatedContent)
  case object SeparatorPosition extends PropertyNameType ; forceConstruction(SeparatorPosition)
  case object SeparatorPolicy extends PropertyNameType ; forceConstruction(SeparatorPolicy)
  case object ChoiceLengthKind extends PropertyNameType ; forceConstruction(ChoiceLengthKind)
  case object ChoiceLength extends PropertyNameType ; forceConstruction(ChoiceLength)
  case object OccursCountKind extends PropertyNameType ; forceConstruction(OccursCountKind)
  case object OccursCount extends PropertyNameType ; forceConstruction(OccursCount)
  case object Floating extends PropertyNameType ; forceConstruction(Floating)
  case object TruncateSpecifiedLengthString extends PropertyNameType ; forceConstruction(TruncateSpecifiedLengthString)
  case object DecimalSigned extends PropertyNameType ; forceConstruction(DecimalSigned)

  def apply(name: String) : PropertyNameType = stringToEnum("propertyNameType", name)
}
  
trait PropertyNameTypeMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val propertyNameType = PropertyNameType(getProperty("propertyNameType"))
    
  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for 
   * property existence. Just insist on the property you need by
   * calling getProperty("propertyNameType")
   */
  lazy val optionPropertyNameType = getPropertyOption("propertyNameType")
    
  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */   
  def propertyNameTypeToString() = {
    optionPropertyNameType match {
      case None => "" // empty string if not present
      case Some(propertyNameType) =>  "propertyNameType='" + propertyNameType + "' "
    }
  }
    
  def propertyNameTypeInit() = {
    registerToStringFunction(propertyNameTypeToString)
  }
    
  propertyNameTypeInit() // call at object creation to initialize
}

////////////////////////////////////////////////////////////////////////
// <xsd:element type="dfdl:DFDLDefineFormat" name="defineFormat" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema"></xsd:element>

trait DefineFormat_AnnotationMixin extends PropertyMixin
  with DFDLDefineFormatMixin {

  def defineFormat_AnnotationInit() : Unit = {
  }
  defineFormat_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLDefineFormat" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:sequence>
// 			<xsd:element ref="dfdl:format" minOccurs="0" maxOccurs="unbounded"></xsd:element>
// 		</xsd:sequence>
// 		<xsd:attribute type="xsd:NCName" name="name"></xsd:attribute>
// 	</xsd:complexType>

trait DFDLDefineFormatMixin extends PropertyMixin {
  lazy val prettyName = convertToNCName(getProperty("name"))

  def dFDLDefineFormatInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("name") match {
        case None => ""
        case Some(value) => "name='" + value.toString + "'"
      }
    })
  }
  dFDLDefineFormatInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:element type="dfdl:DFDLFormat" name="format" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema"></xsd:element>

trait Format_AnnotationMixin extends PropertyMixin
  with DFDLFormatMixin {

  def format_AnnotationInit() : Unit = {
  }
  format_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLFormat" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexContent>
// 			<xsd:extension base="dfdl:DFDLBaseType">
// 				<xsd:sequence></xsd:sequence>
// 				<xsd:attributeGroup ref="dfdl:FormatAG"></xsd:attributeGroup>
// 			</xsd:extension>
// 		</xsd:complexContent>
// 	</xsd:complexType>

trait DFDLFormatMixin extends PropertyMixin
  with DFDLBaseTypeMixin
  with FormatAGMixin {

  def dFDLFormatInit() : Unit = {
  }
  dFDLFormatInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:element type="dfdl:DFDLDefineEscapeScheme" name="defineEscapeScheme" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema"></xsd:element>

trait DefineEscapeScheme_AnnotationMixin extends PropertyMixin
  with DFDLDefineEscapeSchemeMixin {

  def defineEscapeScheme_AnnotationInit() : Unit = {
  }
  defineEscapeScheme_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLDefineEscapeScheme" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:sequence>
// 			<xsd:element ref="dfdl:escapeScheme"></xsd:element>
// 		</xsd:sequence>
// 		<xsd:attribute type="xsd:NCName" name="name"></xsd:attribute>
// 	</xsd:complexType>

trait DFDLDefineEscapeSchemeMixin extends PropertyMixin {
  lazy val prettyName = convertToNCName(getProperty("name"))

  def dFDLDefineEscapeSchemeInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("name") match {
        case None => ""
        case Some(value) => "name='" + value.toString + "'"
      }
    })
  }
  dFDLDefineEscapeSchemeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:element type="dfdl:DFDLEscapeScheme" name="escapeScheme" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema"></xsd:element>

trait EscapeScheme_AnnotationMixin extends PropertyMixin
  with DFDLEscapeSchemeMixin {

  def escapeScheme_AnnotationInit() : Unit = {
  }
  escapeScheme_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLEscapeScheme" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexContent>
// 			<xsd:extension base="dfdl:DFDLType">
// 				<xsd:sequence></xsd:sequence>
// 				<xsd:attributeGroup ref="dfdl:EscapeSchemeAG"></xsd:attributeGroup>
// 			</xsd:extension>
// 		</xsd:complexContent>
// 	</xsd:complexType>

trait DFDLEscapeSchemeMixin extends PropertyMixin
  with DFDLTypeMixin
  with EscapeSchemeAGMixin {

  def dFDLEscapeSchemeInit() : Unit = {
  }
  dFDLEscapeSchemeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:element type="dfdl:PropertyType" name="property" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema"></xsd:element>

trait Property_AnnotationMixin extends PropertyMixin
  with PropertyTypeMixin {

  def property_AnnotationInit() : Unit = {
  }
  property_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="PropertyType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:simpleContent>
// 			<xsd:extension base="xsd:string">
// 				<xsd:attribute type="dfdl:PropertyNameType" name="name"></xsd:attribute>
// 			</xsd:extension>
// 		</xsd:simpleContent>
// 	</xsd:complexType>

trait PropertyTypeMixin extends PropertyMixin {
  lazy val prettyName = PropertyNameType(getProperty("name"))

  def propertyTypeInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("name") match {
        case None => ""
        case Some(value) => "name='" + value.toString + "'"
      }
    })
  }
  propertyTypeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLVariableType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:simpleContent>
// 			<xsd:extension base="dfdl:DFDLStringLiteral_Or_DFDLExpression">
// 			</xsd:extension>
// 		</xsd:simpleContent>
// 	</xsd:complexType>

trait DFDLVariableTypeMixin extends PropertyMixin
  with DFDLStringLiteral_Or_DFDLExpressionMixin {

  def dFDLVariableTypeInit() : Unit = {
  }
  dFDLVariableTypeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="DefineVariableAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="xsd:NCName" name="name"></xsd:attribute>
// 		<xsd:attribute type="xsd:boolean" name="predefined"></xsd:attribute>
// 		<xsd:attribute type="xsd:QName" name="type"></xsd:attribute>
// 		<xsd:attribute type="xsd:boolean" name="external"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral_Or_DFDLExpression" name="defaultValue"></xsd:attribute>
// 	</xsd:attributeGroup>

trait DefineVariableAGMixin extends PropertyMixin {
  lazy val predefined = convertToBoolean(getProperty("predefined"))
  lazy val external = convertToBoolean(getProperty("external"))

  def defineVariableAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("predefined") match {
        case None => ""
        case Some(value) => "predefined='" + value.toString + "'"
      }
    })
    registerToStringFunction(()=>{getPropertyOption("external") match {
        case None => ""
        case Some(value) => "external='" + value.toString + "'"
      }
    })
  }
  defineVariableAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="SetVariableAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="xsd:QName" name="ref"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral_Or_DFDLExpression" name="value"></xsd:attribute>
// 	</xsd:attributeGroup>

trait SetVariableAGMixin extends PropertyMixin {
  lazy val value = DFDLStringLiteral_Or_DFDLExpression(getProperty("value"))

  def setVariableAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("value") match {
        case None => ""
        case Some(value) => "value='" + value.toString + "'"
      }
    })
  }
  setVariableAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="NewVariableInstanceAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="xsd:QName" name="ref"></xsd:attribute>
// 		<xsd:attribute type="dfdl:DFDLStringLiteral_Or_DFDLExpression" name="defaultValue"></xsd:attribute>
// 	</xsd:attributeGroup>

trait NewVariableInstanceAGMixin extends PropertyMixin {

  def newVariableInstanceAGInit() : Unit = {
  }
  newVariableInstanceAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 			<xsd:simpleContent>
// 				<xsd:extension base="dfdl:DFDLVariableType">
// 					<xsd:attributeGroup ref="dfdl:DefineVariableAG"></xsd:attributeGroup>
// 				</xsd:extension>
// 			</xsd:simpleContent>
// 		</xsd:complexType>

trait DFDLDefineVariableTypeMixin extends PropertyMixin
  with DFDLVariableTypeMixin
  with DefineVariableAGMixin {

  def dFDLDefineVariableTypeInit() : Unit = {
  }
  dFDLDefineVariableTypeInit()
}

////////////////////////////////////
////////////////////////////////////
// <xsd:element name="defineVariable" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexType>
// 			<xsd:simpleContent>
// 				<xsd:extension base="dfdl:DFDLVariableType">
// 					<xsd:attributeGroup ref="dfdl:DefineVariableAG"></xsd:attributeGroup>
// 				</xsd:extension>
// 			</xsd:simpleContent>
// 		</xsd:complexType>
// 	</xsd:element>

trait DefineVariable_AnnotationMixin extends PropertyMixin
  with DFDLDefineVariableTypeMixin
  with DefineVariableAGMixin {

  def defineVariable_AnnotationInit() : Unit = {
  }
  defineVariable_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 			<xsd:simpleContent>
// 				<xsd:extension base="dfdl:DFDLVariableType">
// 					<xsd:attributeGroup ref="dfdl:NewVariableInstanceAG"></xsd:attributeGroup>
// 				</xsd:extension>
// 			</xsd:simpleContent>
// 		</xsd:complexType>

trait DFDLNewVariableInstanceTypeMixin extends PropertyMixin
  with DFDLVariableTypeMixin
  with NewVariableInstanceAGMixin {

  def dFDLNewVariableInstanceTypeInit() : Unit = {
  }
  dFDLNewVariableInstanceTypeInit()
}

////////////////////////////////////
////////////////////////////////////
// <xsd:element name="newVariableInstance" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexType>
// 			<xsd:simpleContent>
// 				<xsd:extension base="dfdl:DFDLVariableType">
// 					<xsd:attributeGroup ref="dfdl:NewVariableInstanceAG"></xsd:attributeGroup>
// 				</xsd:extension>
// 			</xsd:simpleContent>
// 		</xsd:complexType>
// 	</xsd:element>

trait NewVariableInstance_AnnotationMixin extends PropertyMixin
  with DFDLNewVariableInstanceTypeMixin
  with NewVariableInstanceAGMixin {

  def newVariableInstance_AnnotationInit() : Unit = {
  }
  newVariableInstance_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 			<xsd:simpleContent>
// 				<xsd:extension base="dfdl:DFDLVariableType">
// 					<xsd:attributeGroup ref="dfdl:SetVariableAG"></xsd:attributeGroup>
// 				</xsd:extension>
// 			</xsd:simpleContent>
// 		</xsd:complexType>

trait DFDLSetVariableTypeMixin extends PropertyMixin
  with DFDLVariableTypeMixin
  with SetVariableAGMixin {

  def dFDLSetVariableTypeInit() : Unit = {
  }
  dFDLSetVariableTypeInit()
}

////////////////////////////////////
////////////////////////////////////
// <xsd:element name="setVariable" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexType>
// 			<xsd:simpleContent>
// 				<xsd:extension base="dfdl:DFDLVariableType">
// 					<xsd:attributeGroup ref="dfdl:SetVariableAG"></xsd:attributeGroup>
// 				</xsd:extension>
// 			</xsd:simpleContent>
// 		</xsd:complexType>
// 	</xsd:element>

trait SetVariable_AnnotationMixin extends PropertyMixin
  with DFDLSetVariableTypeMixin
  with SetVariableAGMixin {

  def setVariable_AnnotationInit() : Unit = {
  }
  setVariable_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="TestConditionAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attribute type="dfdl:DFDLExpression" name="test"></xsd:attribute>
// 		<xsd:attribute type="dfdl:TestKindEnum" name="testKind"></xsd:attribute>
// 		<xsd:attribute type="xsd:string" name="testPattern"></xsd:attribute>
// 		<xsd:attribute type="xsd:string" name="message"></xsd:attribute>
// 	</xsd:attributeGroup>

trait TestConditionAGMixin extends PropertyMixin
  with TestKindMixin {
  lazy val testPattern = convertToString(getProperty("testPattern"))

  def testConditionAGInit() : Unit = {
    registerToStringFunction(()=>{getPropertyOption("testPattern") match {
        case None => ""
        case Some(value) => "testPattern='" + value.toString + "'"
      }
    })
  }
  testConditionAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="TestCondition" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:simpleContent>
// 			<xsd:extension base="dfdl:DFDLExpressionOrNothing">
// 				<xsd:attributeGroup ref="dfdl:TestConditionAG"></xsd:attributeGroup>
// 			</xsd:extension>
// 		</xsd:simpleContent>
// 	</xsd:complexType>

trait TestConditionMixin extends PropertyMixin
  with DFDLExpressionOrNothingMixin
  with TestConditionAGMixin {

  def testConditionInit() : Unit = {
  }
  testConditionInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 			<xsd:simpleContent>
// 				<xsd:extension base="dfdl:TestCondition">
// 				</xsd:extension>
// 			</xsd:simpleContent>
// 		</xsd:complexType>

trait DFDLAssertTypeMixin extends PropertyMixin
  with TestConditionMixin {

  def dFDLAssertTypeInit() : Unit = {
  }
  dFDLAssertTypeInit()
}

////////////////////////////////////
////////////////////////////////////
// <xsd:element name="assert" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexType>
// 			<xsd:simpleContent>
// 				<xsd:extension base="dfdl:TestCondition">
// 				</xsd:extension>
// 			</xsd:simpleContent>
// 		</xsd:complexType>
// 	</xsd:element>

trait Assert_AnnotationMixin extends PropertyMixin
  with DFDLAssertTypeMixin {

  def assert_AnnotationInit() : Unit = {
  }
  assert_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 			<xsd:simpleContent>
// 				<xsd:extension base="dfdl:TestCondition">
// 				</xsd:extension>
// 			</xsd:simpleContent>
// 		</xsd:complexType>

trait DFDLDiscriminatorTypeMixin extends PropertyMixin
  with TestConditionMixin {

  def dFDLDiscriminatorTypeInit() : Unit = {
  }
  dFDLDiscriminatorTypeInit()
}

////////////////////////////////////
////////////////////////////////////
// <xsd:element name="discriminator" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexType>
// 			<xsd:simpleContent>
// 				<xsd:extension base="dfdl:TestCondition">
// 				</xsd:extension>
// 			</xsd:simpleContent>
// 		</xsd:complexType>
// 	</xsd:element>

trait Discriminator_AnnotationMixin extends PropertyMixin
  with DFDLDiscriminatorTypeMixin {

  def discriminator_AnnotationInit() : Unit = {
  }
  discriminator_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:element type="dfdl:DFDLElementType" name="element" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema"></xsd:element>

trait Element_AnnotationMixin extends PropertyMixin
  with DFDLElementTypeMixin {

  def element_AnnotationInit() : Unit = {
  }
  element_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:element type="dfdl:DFDLSequenceType" name="sequence" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema"></xsd:element>

trait Sequence_AnnotationMixin extends PropertyMixin
  with DFDLSequenceTypeMixin {

  def sequence_AnnotationInit() : Unit = {
  }
  sequence_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:element type="dfdl:DFDLChoiceType" name="choice" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema"></xsd:element>

trait Choice_AnnotationMixin extends PropertyMixin
  with DFDLChoiceTypeMixin {

  def choice_AnnotationInit() : Unit = {
  }
  choice_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:element type="dfdl:DFDLGroupType" name="group" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema"></xsd:element>

trait Group_AnnotationMixin extends PropertyMixin
  with DFDLGroupTypeMixin {

  def group_AnnotationInit() : Unit = {
  }
  group_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:element type="dfdl:DFDLSimpleType" name="simpleType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema"></xsd:element>

trait SimpleType_AnnotationMixin extends PropertyMixin
  with DFDLSimpleTypeMixin {

  def simpleType_AnnotationInit() : Unit = {
  }
  simpleType_AnnotationInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLType" abstract="true" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:sequence>
// 			<xsd:element ref="dfdl:property" minOccurs="0" maxOccurs="unbounded"></xsd:element>
// 		</xsd:sequence>
// 	</xsd:complexType>

trait DFDLTypeMixin extends PropertyMixin {

  def dFDLTypeInit() : Unit = {
  }
  dFDLTypeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLBaseType" abstract="true" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexContent>
// 			<xsd:extension base="dfdl:DFDLType">
// 				<xsd:sequence></xsd:sequence>
// 				<xsd:attributeGroup ref="dfdl:BaseAG"></xsd:attributeGroup>
// 				<xsd:attributeGroup ref="dfdl:CommonAG"></xsd:attributeGroup>
// 				<xsd:attributeGroup ref="dfdl:AlignmentAG"></xsd:attributeGroup>
// 				<xsd:attributeGroup ref="dfdl:MarkupAG"></xsd:attributeGroup>
// 			</xsd:extension>
// 		</xsd:complexContent>
// 	</xsd:complexType>

trait DFDLBaseTypeMixin extends PropertyMixin
  with DFDLTypeMixin
  with BaseAGMixin
  with CommonAGMixin
  with AlignmentAGMixin
  with MarkupAGMixin {

  def dFDLBaseTypeInit() : Unit = {
  }
  dFDLBaseTypeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLElementType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexContent>
// 			<xsd:extension base="dfdl:DFDLBaseType">
// 				<xsd:sequence></xsd:sequence>
// 				<xsd:attributeGroup ref="dfdl:ElementAG"></xsd:attributeGroup>
// 			</xsd:extension>
// 		</xsd:complexContent>
// 	</xsd:complexType>

trait DFDLElementTypeMixin extends PropertyMixin
  with DFDLBaseTypeMixin
  with ElementAGMixin {

  def dFDLElementTypeInit() : Unit = {
  }
  dFDLElementTypeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLSequenceType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexContent>
// 			<xsd:extension base="dfdl:DFDLBaseType">
// 				<xsd:sequence></xsd:sequence>
// 				<xsd:attributeGroup ref="dfdl:GroupCommonAG"></xsd:attributeGroup>
// 				<xsd:attributeGroup ref="dfdl:SequenceAG"></xsd:attributeGroup>
// 				<xsd:attributeGroup ref="dfdl:SeparatorAG"></xsd:attributeGroup>
// 			</xsd:extension>
// 		</xsd:complexContent>
// 	</xsd:complexType>

trait DFDLSequenceTypeMixin extends PropertyMixin
  with DFDLBaseTypeMixin
  with GroupCommonAGMixin
  with SequenceAGMixin
  with SeparatorAGMixin {

  def dFDLSequenceTypeInit() : Unit = {
  }
  dFDLSequenceTypeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLChoiceType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexContent>
// 			<xsd:extension base="dfdl:DFDLBaseType">
// 				<xsd:sequence></xsd:sequence>
// 				<xsd:attributeGroup ref="dfdl:GroupCommonAG"></xsd:attributeGroup>
// 				<xsd:attributeGroup ref="dfdl:ChoiceAG"></xsd:attributeGroup>
// 			</xsd:extension>
// 		</xsd:complexContent>
// 	</xsd:complexType>

trait DFDLChoiceTypeMixin extends PropertyMixin
  with DFDLBaseTypeMixin
  with GroupCommonAGMixin
  with ChoiceAGMixin {

  def dFDLChoiceTypeInit() : Unit = {
  }
  dFDLChoiceTypeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLSimpleType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexContent>
// 			<xsd:extension base="dfdl:DFDLBaseType">
// 				<xsd:sequence></xsd:sequence>
// 				<xsd:attributeGroup ref="dfdl:SimpleTypeAG"></xsd:attributeGroup>
// 			</xsd:extension>
// 		</xsd:complexContent>
// 	</xsd:complexType>

trait DFDLSimpleTypeMixin extends PropertyMixin
  with DFDLBaseTypeMixin
  with SimpleTypeAGMixin {

  def dFDLSimpleTypeInit() : Unit = {
  }
  dFDLSimpleTypeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:complexType name="DFDLGroupType" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:complexContent>
// 			<xsd:extension base="dfdl:DFDLBaseType">
// 				<xsd:sequence></xsd:sequence>
// 				<xsd:attributeGroup ref="dfdl:GroupAG"></xsd:attributeGroup>
// 			</xsd:extension>
// 		</xsd:complexContent>
// 	</xsd:complexType>

trait DFDLGroupTypeMixin extends PropertyMixin
  with DFDLBaseTypeMixin
  with GroupAGMixin {

  def dFDLGroupTypeInit() : Unit = {
  }
  dFDLGroupTypeInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="FormatAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attributeGroup ref="dfdl:ElementAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:GroupAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:DocumentPropertiesAG"></xsd:attributeGroup>
// 	</xsd:attributeGroup>

trait FormatAGMixin extends PropertyMixin
  with ElementAGMixin
  with GroupAGMixin
  with DocumentPropertiesAGMixin {

  def formatAGInit() : Unit = {
  }
  formatAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="ElementAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attributeGroup ref="dfdl:SimpleTypeAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:FloatingAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:NillableAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:DefaultValueControlAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:OccursAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:CalculatedValueAG"></xsd:attributeGroup>
// 	</xsd:attributeGroup>

trait ElementAGMixin extends PropertyMixin
  with SimpleTypeAGMixin
  with FloatingAGMixin
  with NillableAGMixin
  with DefaultValueControlAGMixin
  with OccursAGMixin
  with CalculatedValueAGMixin {

  def elementAGInit() : Unit = {
  }
  elementAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="GroupAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attributeGroup ref="dfdl:GroupCommonAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:SequenceAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:ChoiceAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:SeparatorAG"></xsd:attributeGroup>
// 	</xsd:attributeGroup>

trait GroupAGMixin extends PropertyMixin
  with GroupCommonAGMixin
  with SequenceAGMixin
  with ChoiceAGMixin
  with SeparatorAGMixin {

  def groupAGInit() : Unit = {
  }
  groupAGInit()
}

////////////////////////////////////////////////////////////////////////
// <xsd:attributeGroup name="SimpleTypeAG" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
// 		<xsd:attributeGroup ref="dfdl:RepresentationAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:NonGroupMarkupAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:LengthAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:SimpleTypesTextAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:StringTextAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:NumberAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:NumberTextAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:TextNumberFormatAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:NumberBinaryAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:FloatBinaryAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:TextBidiSchemeAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:CalendarTextAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:CalendarBinaryAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:CalendarFormatAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:BooleanTextAG"></xsd:attributeGroup>
// 		<xsd:attributeGroup ref="dfdl:BooleanBinaryAG"></xsd:attributeGroup>
// 	</xsd:attributeGroup>

trait SimpleTypeAGMixin extends PropertyMixin
  with RepresentationAGMixin
  with NonGroupMarkupAGMixin
  with LengthAGMixin
  with SimpleTypesTextAGMixin
  with StringTextAGMixin
  with NumberAGMixin
  with NumberTextAGMixin
  with TextNumberFormatAGMixin
  with NumberBinaryAGMixin
  with FloatBinaryAGMixin
  with TextBidiSchemeAGMixin
  with CalendarTextAGMixin
  with CalendarBinaryAGMixin
  with CalendarFormatAGMixin
  with BooleanTextAGMixin
  with BooleanBinaryAGMixin {

  def simpleTypeAGInit() : Unit = {
  }
  simpleTypeAGInit()
}

////////////////////////////////////