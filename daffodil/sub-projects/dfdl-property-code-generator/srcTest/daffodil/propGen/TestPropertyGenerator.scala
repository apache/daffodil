package daffodil.propGen


import org.xml.sax.InputSource
import scala.xml._

import junit.framework.Assert._ ;
import org.scalatest.junit.JUnit3Suite ;

class TestPropertyGenerator extends JUnit3Suite {

  def testGenEnum() {
    val sch = <xsd:simpleType name="NilKindEnum">
                <xsd:restriction base="xsd:string">
                  <xsd:enumeration value="literalValue"/>
                  <xsd:enumeration value="logicalValue"/>
                  <xsd:enumeration value="literalCharacter"/>
                  <xsd:enumeration value="nilIndicator"/>
                </xsd:restriction>
                <!-- 15.9	Properties for Nillable Elements -->
              </xsd:simpleType>

    val pg = new PropertyGenerator(sch)
    val nk = pg.genSimpleType(sch)
    assertTrue(nk.contains("sealed trait NilKind"))
    }
  
  def testGenPropMixins() {
      val sch = 
	<xsd:attributeGroup name="LengthPropertiesAG">
		<xsd:attribute name="lengthKind" type="dfdl:LengthKindEnum" />
		<xsd:attribute name="length" type="xsd:string" />
		<xsd:attribute name="lengthPattern" type="xsd:string" />
		<xsd:attribute name="lengthUnits" type="dfdl:LengthUnitsEnum" />
		<xsd:attribute name="prefixIncludesPrefixLength" type="xsd:boolean" />
		<xsd:attribute name="prefixLengthType" type="xsd:QName" />
	</xsd:attributeGroup>
     val pg = new PropertyGenerator(sch)   
     val mx = pg.genAttributeGroup(sch)
     assertTrue(mx.contains(""" = LengthKind(getProperty("lengthKind"))"""))
     assertTrue(mx.contains(""" = convertToBoolean(getProperty("prefixIncludesPrefixLength"))"""))
  }
  
    def testGenPropMixins2() {
      val sch = 
	<xsd:attributeGroup name="ElementAG">
		<xsd:attributeGroup ref="dfdl:SimpleTypeAG"></xsd:attributeGroup>
		<xsd:attributeGroup ref="dfdl:NillableAG"></xsd:attributeGroup>
		<xsd:attributeGroup ref="dfdl:DefaultValueControlAG"></xsd:attributeGroup>
		<xsd:attributeGroup ref="dfdl:OccursAG"></xsd:attributeGroup>
	</xsd:attributeGroup>
     val pg = new PropertyGenerator(sch)   
     val mx = pg.genAttributeGroup(sch)
     assertTrue(mx.contains("""with SimpleTypeAGMixin"""))
     assertTrue(mx.contains("""with NillableAGMixin"""))
  }
    
   def testGenPropMixins3() {
      val sch = 
	<xsd:attributeGroup name="TextNumberFormatAG1">
		<xsd:attribute name="textNumberFormatRef" type="xsd:NCName" />
	</xsd:attributeGroup>
     val pg = new PropertyGenerator(sch)   
     val mx = pg.genAttributeGroup(sch)
     assertTrue(mx.contains("""convertToNCName(getProperty("textNumberFormatRef")"""))
  }
   
   def testGenCT() {
           val sch = 
		<xsd:complexType name="DFDLSequenceType">
		<xsd:complexContent>
			<xsd:extension base="dfdl:DFDLBaseType">
				<xsd:sequence />
				<xsd:attributeGroup ref="dfdl:SequenceGroupsAG" />
				<xsd:attributeGroup ref="dfdl:SequenceGroupsWithDelimitersAG" />
			</xsd:extension>
		</xsd:complexContent>
	</xsd:complexType>
     val pg = new PropertyGenerator(sch)   
     val mx = pg.genComplexType(sch)
     assertTrue(mx.contains("""with SequenceGroupsAGMixin"""))
     assertTrue(mx.contains("""with SequenceGroupsWithDelimitersAGMixin"""))
   }
   
   def testGenCT2() {
           val sch = 
	<xsd:complexType name="DFDLDefineFormat">
		<xsd:all>
			<xsd:element ref="dfdl:format" minOccurs="0" />
		</xsd:all>
		<xsd:attribute name="name" type="xsd:NCName" />
		<xsd:attribute name="baseFormat" type="xsd:QName" />
	</xsd:complexType>
     val pg = new PropertyGenerator(sch)   
     val mx = pg.genComplexType(sch)
     print(mx)
     assertTrue(mx.contains("""convertToNCName(getProperty("name"))"""))
     assertTrue(mx.contains("""convertToQName(getProperty("baseFormat")"""))
   }
   
   def testElement1() {
              val sch = 
	<xsd:element name="defineFormat" type="dfdl:DFDLDefineFormat" />
     val pg = new PropertyGenerator(sch)   
     val mx = pg.genElement(sch)
     print(mx)
     assertTrue(mx.contains("""with DFDLDefineFormatMixin"""))
   }
    
  def testGenProperties() {
    val thunks = PropertyGenerator.generateThunks()
    thunks.foreach(System.err.print)
  }
  
  def testElementWithImmediateComplexType1() {
    val sch = 
	<xsd:element name="defineVariable">
	<xsd:complexType>
		<xsd:simpleContent>
			<xsd:extension base="dfdl:DFDLVariableType">
				<xsd:attributeGroup ref="dfdl:DefineVariableAG" />
			</xsd:extension>
		</xsd:simpleContent>
	</xsd:complexType>
    </xsd:element>
     val pg = new PropertyGenerator(sch)   
     val mx = pg.genElement(sch)
     print(mx)
     assertTrue(mx.contains("""trait DFDLDefineVariableTypeMixin extends PropertyMixin"""))
     assertTrue(mx.contains("""trait DefineVariable_AnnotationMixin extends PropertyMixin"""))
     assertTrue(mx.contains("""with DFDLDefineVariableTypeMixin"""))
  }
  
}

/**
 * When you run this, it regenerates the Generated code and overwrites 
 * the GeneratedCode.scala file
 */
object Main extends App {
    PropertyGenerator.main(null)
}