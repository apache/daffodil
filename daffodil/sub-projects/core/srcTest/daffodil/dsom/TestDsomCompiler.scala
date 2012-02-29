package daffodil.dsom

import daffodil.xml.XMLUtil
import scala.xml._

import org.scalatest.junit.JUnit3Suite

import daffodil.schema.annotation.props.gen._
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue

/**
 * Scala Unit Testing Notes:
 *
 * It is important that the Eclipse IDE make it convenient to run the unit tests, step the user directly to the point
 * of failure, etc.
 *
 * Scalatest doesn't do this directly, but using it driven by JUnit3 does.
 *
 * So I'm advocating that a much more vanilla approach be taken to unit tests. Straight use of Junit3.
 *
 * Here is an example. Some simple tests, some that intercept exceptions, and demonstrate that the intercept
 * device works properly.
 */
class TestDsomCompiler extends JUnit3Suite {

  val xsd = XMLUtil.XSD_NAMESPACE
  val dfdl = XMLUtil.DFDL_NAMESPACE
  val xsi = XMLUtil.XSI_NAMESPACE
  val example = XMLUtil.EXAMPLE_NAMESPACE
  
  // @Test
  def testHasProps() {
    val testSchema = <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <annotation>
          <appinfo source={ dfdl }>
            <dfdl:format byteOrder="bigEndian"/>
          </appinfo>
        </annotation>
        <element name="list" type="tns:example1"/>
        <complexType name="example1">
          <sequence>
            <element name="w" type="xsd:int" dfdl:inputValueCalc="{ $x + 1 }"/>
          </sequence>
        </complexType>
      </schema>

    val compiler = Compiler()
    val sset = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(decl) = schemaDoc.globalElementDecls

    val df = schemaDoc.defaultFormat
    val bo = df.byteOrder
    assertEquals(ByteOrder.BigEndian.toString().toLowerCase(), bo.toLowerCase())
  }

  // @Test
  def testSchemaValidationSubset() {
    val sch : Node = <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="list" type="tns:example1"/>
        <complexType name="example1">
          <sequence maxOccurs="2"> <!-- DFDL SUBSET DOESN'T ALLOW THIS -->
            <element name="w" type="xsd:int"/>
          </sequence>
        </complexType>
      </schema>
    val ex = intercept[Exception] {
      Compiler().frontEnd(sch)
    }
    // should throw a validation error. 
    println(ex)
    val msg = ex.getMessage()
    val hasErrorText = msg.contains("maxOccurs");
    assertTrue(hasErrorText)
  }

  // @Test
  def testSchemaValidationPropertyChecking() {
    val s : Node = <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="list" type="tns:example1"/>
        <complexType name="example1">
          <sequence>
            <element name="w" type="xsd:int" dfdl:byteOrder="invalidValue"/>
          </sequence>
        </complexType>
      </schema>
    val ex = intercept[Exception] {
      Compiler().frontEnd(s)
    }
    // should throw a validation error. 
    println(ex)
    val msg = ex.getMessage()
    val hasErrorText = msg.contains("invalidValue");
    assertTrue(hasErrorText)
  }

  def test2() {
    val sc =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="list" type="tns:example1">
          <annotation>
            <appinfo source={ dfdl }>
              <dfdl:element encoding="ASCII" alignmentUnits="bytes"/>
            </appinfo>
          </annotation>
        </element>
        <complexType name="example1">
          <sequence>
            <element name="w" type="xsd:int" dfdl:inputValueCalc="{ $x + 1 }"/>
          </sequence>
        </complexType>
      </schema>

    val sset = Compiler().frontEnd(sc)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(decl) = schemaDoc.globalElementDecls
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)

    val fa = decl.formatAnnotation.asInstanceOf[DFDLElement]
    assertEquals(AlignmentUnits.Bytes, fa.alignmentUnits)
    fa.alignmentUnits match {
      case AlignmentUnits.Bits => println("was bits")
      case AlignmentUnits.Bytes => println("was bytes")
    }
  }

  /* def testXsomMultifile(){
   
    val parser = new XSOMParser()
    val apf = new DomAnnotationParserFactory()
    parser.setAnnotationParser(apf)

    val inFile = new File("test/first.xsd")

    parser.parse(inFile)

    val sset = parser.getResult()
    val sds = parser.getDocuments().toList
    assertTrue(sds.size() >= 2)
  
    sds.map{sd => println(sd.getSystemId)}
  }*/

  def testSequence1() {
    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="list" type="tns:example1">
          <annotation>
            <appinfo source={ dfdl }>
              <dfdl:element encoding="ASCII" alignmentUnits="bytes"/>
            </appinfo>
          </annotation>
        </element>
        <complexType name="example1">
          <sequence>
            <element name="w" type="xsd:int" maxOccurs="unbounded"/>
          </sequence>
        </complexType>
      </schema>

    val w = Utility.trim(testSchema)

    val sset = Compiler().frontEnd(w)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(decl) = schemaDoc.globalElementDecls
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)

    val mg = ct.modelGroup.asInstanceOf[Sequence]
    assertTrue(mg.isInstanceOf[Sequence])

    val Seq(elem) = mg.children
    assertTrue(elem.isInstanceOf[LocalElementDecl])

  }

  // @Test
  def testAPI1() {
    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="data" type="xsd:int" dfdl:inputValueCalc="{ 42 }"/>
      </schema>
    val actual = Compiler.testString(testSchema, "")
    val actualString = actual.toString
    assertTrue(actualString.contains("<data"))
    assertTrue(actualString.contains(">42</data>"))
  }

  // @Test
  def testAPI2() {
    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="data" type="xsd:int" dfdl:terminator="%NL;" dfdl:encoding="ASCII" dfdl:representation="text" dfdl:lengthKind="delimited" dfdl:documentFinalTerminatorCanBeMissing="yes"/>
      </schema>
    val actual = Compiler.testString(testSchema, "37\n")
    val actualString = actual.toString
    assertTrue(actualString.contains("<data"))
    assertTrue(actualString.contains(">37</data>"))
  }
  
    // @Test
  def testUnparse1() {
    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="data" type="xsd:int" dfdl:lengthKind="explicit" dfdl:encoding="ASCII" dfdl:representation="text" dfdl:length="{ 2 }"/>
      </schema>
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val unparser = pf.onPath("/")
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    unparser.unparse(out, <data xmlns={ example }>37</data>)
    out.close()
    val actualString = outputStream.toString()
    assertEquals("37", actualString)
  }
  
  def test3 {
    val testSchema = XML.loadFile("test/example-of-most-dfdl-constructs.dfdl.xml")
    val compiler = Compiler()
    
    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments
    
    // No annotations
    val Seq(ct) = sd.globalComplexTypeDefs
    val Seq(gr1,gr2) = sd.globalGroupDefs
    
    val Seq(e1,e2,e3) = sd.globalElementDecls
    assertEquals(ByteOrder.BigEndian.toString().toLowerCase(), e1.formatAnnotation.asInstanceOf[DFDLElement].byteOrder.toLowerCase())
    val Seq(a1,a2) = e3.annotationObjs
    assertTrue(a2.isInstanceOf[DFDLNewVariableInstance])
    val e1ct = e1.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val seq = e1ct.modelGroup.asInstanceOf[Sequence]
    val sfa = seq.formatAnnotation.asInstanceOf[DFDLSequence]
    assertEquals(YesNo.No, sfa.initiatedContent)
   
    val Seq(st1,st2) = sd.globalSimpleTypeDefs
    val Seq(b1, b2, b3, b4) = st1.annotationObjs
    assertEquals(AlignmentUnits.Bytes, b1.asInstanceOf[DFDLSimpleType].alignmentUnits)
    assertEquals("tns:myVar1", b2.asInstanceOf[DFDLSetVariable].ref)
    assertEquals("yadda yadda yadda", b4.asInstanceOf[DFDLAssert].message)
    
    val Seq(df1,df2) = sd.defineFormats
    val def1 = df1.asInstanceOf[DFDLDefineFormat]
    assertEquals("def1", def1.name)
    assertEquals(Representation.Text, def1.formatAnnotation.representation)

    val Seq(dv1,dv2) = sd.defineVariables
    //assertEquals("архив", dv2.asInstanceOf[DFDLDefineVariable].name)
    
    val Seq(desc1) = sd.defineEscapeSchemes
    val es = desc1.asInstanceOf[DFDLDefineEscapeScheme].escapeScheme.escapeCharacter
    assertEquals("%%", es)
    
  }

}