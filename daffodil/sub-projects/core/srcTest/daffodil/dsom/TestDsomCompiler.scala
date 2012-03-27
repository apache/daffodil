package daffodil.dsom

import daffodil.xml.XMLUtil
import daffodil.util._
import scala.xml._

import org.scalatest.junit.JUnit3Suite

import daffodil.schema.annotation.props.gen._
import daffodil.schema.annotation.props._
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue


class TestDsomCompiler extends JUnit3Suite {

  val xsd = XMLUtil.XSD_NAMESPACE
  val dfdl = XMLUtil.DFDL_NAMESPACE
  val xsi = XMLUtil.XSI_NAMESPACE
  val example = XMLUtil.EXAMPLE_NAMESPACE

  val dummyGroupRef = null // just because otherwise we have to construct too many things.
  
  // @Test
  def testHasProps() {
    val testSchema = <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
                       <annotation>
                         <appinfo source={ dfdl }>
                           <dfdl:format textNumberRep="standard" terminator="" representation="text" occursStopValue="-1" byteOrder="bigEndian" emptyValueDelimiterPolicy="none" initiator="" separator="" lengthKind="implicit" occursCountKind="expression"/>
                         </appinfo>
                       </annotation>
                       <element name="list" type="tns:example1" />
                       <complexType name="example1">
                         <sequence>
                           <element name="w" type="xsd:int" dfdl:inputValueCalc="{ $x + 1 }"/>
                         </sequence>
                       </complexType>
                     </schema>
    val compiler = Compiler()
    val (sset, _, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(decl) = schemaDoc.globalElementDecls

    val df = schemaDoc.defaultFormat
    val bo = df.byteOrder
    assertEquals(ByteOrder.BigEndian.toString().toLowerCase(), bo.toLowerCase())
  }

  // @Test
  def testSchemaValidationSubset() {
    val sch: Node = <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
                      <element name="list" type="tns:example1"/>
                      <complexType name="example1">
                        <sequence maxOccurs="2">
                          <!-- DFDL SUBSET DOESN'T ALLOW THIS -->
                          <element name="w" type="xsd:int"/>
                        </sequence>
                      </complexType>
                    </schema>
    val ex = intercept[Exception] {
      Compiler().frontEnd(sch)
    }
    // should throw a validation error. 
    // println(ex)
    val msg = ex.getMessage()
    val hasErrorText = msg.contains("maxOccurs");
    assertTrue(hasErrorText)
  }

  // @Test
  def testSchemaValidationPropertyChecking() {
    val s: Node = <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
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
          <annotation>
            <appinfo source={ dfdl }>
              <dfdl:format representation="text" occursCountKind="parsed" initiator="" terminator="" emptyValueDelimiterPolicy="none" occursStopValue="-1" textNumberRep="standard"/>
            </appinfo>
          </annotation>
        <element name="list" type="tns:example1">
          <annotation>
            <appinfo source={ dfdl }>
              <dfdl:element encoding="US-ASCII" alignmentUnits="bytes" />
            </appinfo>
          </annotation>
        </element>
        <complexType name="example1">
          <sequence dfdl:separator="">
            <element name="w" type="xsd:int" dfdl:inputValueCalc="{ $x + 1 }"/>
          </sequence>
        </complexType>
      </schema>

    val (sset, _, _) = Compiler().frontEnd(sc)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(declFactory) = schemaDoc.globalElementDecls
    val decl = declFactory.forRoot()
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
       	  <annotation>
            <appinfo source={ dfdl }>
              <dfdl:format representation="text" occursStopValue="-1" initiator="" terminator="" emptyValueDelimiterPolicy="none"/>
            </appinfo>
          </annotation>        
          <element name="list" type="tns:example1">
          <annotation>
            <appinfo source={ dfdl }>
              <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
            </appinfo>
          </annotation>
        </element>
        <complexType name="example1">
          <sequence dfdl:separatorPolicy="required" dfdl:separator="">
            <element name="w" type="xsd:int" maxOccurs="unbounded" dfdl:occursCountKind="expression"/>
          </sequence>
        </complexType>
      </schema>

    val w = Utility.trim(testSchema)

    val (sset, _, _) = Compiler().frontEnd(w)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(decl) = schemaDoc.globalElementDecls
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)

    val mg = ct.forElement(null).modelGroup.asInstanceOf[Sequence]
    assertTrue(mg.isInstanceOf[Sequence])

    val Seq(elem) = mg.groupMembers
    assertTrue(elem.isInstanceOf[LocalElementDecl])

  }

  // @Test
  def testInputValueCalc1() {
    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="data" type="xsd:string" dfdl:textNumberRep="standard" dfdl:representation="text" dfdl:terminator="" dfdl:emptyValueDelimiterPolicy="none" dfdl:inputValueCalc="{ 42 }" dfdl:initiator=""/>
      </schema>
    val actual = Compiler.testString(testSchema, "")
    val actualString = actual.toString
    assertTrue(actualString.contains("<data"))
    assertTrue(actualString.contains(">42</data>"))
  }

  // @Test
  def testTerminator1() {
    val testSchema =
      <schema xmlns={ xsd } targetNamespace={ example } xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
        <element name="data" type="xsd:string" dfdl:ignoreCase="no" dfdl:initiator="" dfdl:textNumberRep="standard" dfdl:emptyValueDelimiterPolicy="none" dfdl:terminator="\n" dfdl:encoding="US-ASCII" dfdl:representation="text" dfdl:lengthKind="explicit" dfdl:lengthUnits="bytes" dfdl:length="{ 3 }" dfdl:documentFinalTerminatorCanBeMissing="yes"/>
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
        <element name="data" type="xsd:int" dfdl:occursStopValue="-1" dfdl:textNumberRep="standard" dfdl:terminator="" dfdl:emptyValueDelimiterPolicy="none" dfdl:initiator="" dfdl:lengthKind="explicit" dfdl:encoding="US-ASCII" dfdl:representation="text" dfdl:length="{ 2 }"/>
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

    // Explore global element decl
    val Seq(e1f, e2f, e3f) = sd.globalElementDecls // there are 3 factories
    val e1 = e1f.forRoot()
    val e2 = e2f.forRoot()
    val e3 = e3f.forRoot()
    assertEquals(ByteOrder.BigEndian.toString().toLowerCase(), e1.formatAnnotation.asInstanceOf[DFDLElement].byteOrder.toLowerCase())
    val Seq(a1, a2) = e3.annotationObjs // third one has two annotations
    assertTrue(a2.isInstanceOf[DFDLNewVariableInstance]) // second annotation is newVariableInstance
    assertEquals(OccursCountKind.Implicit, a1.asInstanceOf[DFDLElement].occursCountKind)
    val e1ct = e1.immediateType.get.asInstanceOf[LocalComplexTypeDef] // first one has immediate complex type
    // Explore local complex type def
    val seq = e1ct.modelGroup.asInstanceOf[Sequence] //... which is a sequence
    val sfa = seq.formatAnnotation.asInstanceOf[DFDLSequence] //...annotated with...
    assertEquals(YesNo.No, sfa.initiatedContent) // initiatedContent="no"

    val Seq(e1a: DFDLElement) = e1.annotationObjs
    assertEquals("UTF-8", e1a.encoding)

    // Explore global simple type defs
    val Seq(st1, st2) = sd.globalSimpleTypeDefs // there are two.
    val Seq(b1, b2, b3, b4) = st1.forElement(e1).annotationObjs // first one has 4 annotations
    assertEquals(AlignmentUnits.Bytes, b1.asInstanceOf[DFDLSimpleType].alignmentUnits) // first has alignmentUnits
    assertEquals("tns:myVar1", b2.asInstanceOf[DFDLSetVariable].ref) // second is setVariable with a ref
    assertEquals("yadda yadda yadda", b4.asInstanceOf[DFDLAssert].message.get) // foruth is an assert with yadda message

    // Explore define formats
    val Seq(df1, df2) = sd.defineFormats // there are two
    val def1 = df1.asInstanceOf[DFDLDefineFormat]
    assertEquals("def1", def1.name) // first is named "def1"
    assertEquals(Representation.Text, def1.formatAnnotation.representation) // has representation="text"

    // Explore define variables
    val Seq(dv1, dv2) = sd.defineVariables // there are two
    //assertEquals("2003年08月27日", dv2.asInstanceOf[DFDLDefineVariable].defaultValue) // second has kanji chars in default value

    // Explore define escape schemes
    val Seq(desc1) = sd.defineEscapeSchemes // only one of these
    val es = desc1.asInstanceOf[DFDLDefineEscapeScheme].escapeScheme.escapeCharacter
    assertEquals("%%", es) // has escapeCharacter="%%" (note: string literals not digested yet, so %% is %%, not %.

    // Explore global group defs
    val Seq(gr1, gr2) = sd.globalGroupDefs // there are two
    val seq1 = gr1.forGroupRef(dummyGroupRef, 1).modelGroup.asInstanceOf[Sequence]

    //Explore LocalSimpleTypeDef
    val Seq(gr2c1, gr2c2, gr2c3) = gr2.forGroupRef(dummyGroupRef, 1).modelGroup.asInstanceOf[ModelGroup].groupMembers
    val ist = gr2c3.asInstanceOf[LocalElementDecl].immediateType.get.asInstanceOf[LocalSimpleTypeDef]
    assertEquals("tns:aType", ist.baseName)

    //Explore LocalElementDecl
    val led = gr2c1.asInstanceOf[LocalElementDecl]
    assertEquals(1, led.maxOccurs)
    val Seq(leda) = led.annotationObjs
    assertEquals("{ $myVar1 eq (+47 mod 4) }", leda.asInstanceOf[DFDLDiscriminator].testBody)

    // Explore sequence
    val Seq(seq1a: DFDLSequence) = seq1.annotationObjs // one format annotation with a property
    assertEquals(SeparatorPosition.Infix, seq1a.separatorPosition)
    val Seq(seq1e1, seq1s1) = seq1.groupMembers // has an element and a sub-sequence as its children.
    assertEquals(2, seq1e1.asInstanceOf[ElementRef].maxOccurs)
    assertEquals("ex:a", seq1e1.asInstanceOf[ElementRef].ref)
    assertEquals(0, seq1s1.asInstanceOf[Sequence].groupMembers.length)
  }

  def test4 {
    val testSchema = XML.loadFile("test/example-of-most-dfdl-constructs.dfdl.xml")
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(gd1, gd2) = sd.globalGroupDefs // Obtain Group nodes
    val ch1 = gd2.forGroupRef(dummyGroupRef, 1).modelGroup.asInstanceOf[Choice] // Downcast child-node of group to Choice
    val Seq(cd1, cd2, cd3) = ch1.groupMembers // Children nodes of Choice-node, there are 3

    val Seq(a1: DFDLChoice) = gd2.forGroupRef(dummyGroupRef, 1).modelGroup.annotationObjs // Obtain the annotation object that is a child
    // of the group node.

    assertEquals(AlignmentType.Implicit, a1.alignment)
    assertEquals(ChoiceLengthKind.Implicit, a1.choiceLengthKind)

    val Seq(asrt1) = cd2.asInstanceOf[LocalElementDecl].annotationObjs // Obtain Annotation object that is child
    // of cd2.

    assertEquals("{ $myVar1 eq xs:int(xs:string(fn:round-half-to-even(8.5))) }", asrt1.asInstanceOf[DFDLAssert].test.get)

  }

  def test_named_format_chaining {
    val testSchema = XML.loadFile("test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml")
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f, ge2f, ge3f, ge4f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()
    val Seq(a1: DFDLElement) = ge1.annotationObjs

    val props: Map[String, String] = a1.getFormatProperties()

    def foundValues(collection: Map[String, String], key: String, value: String): Boolean = {
      val found: Boolean = Option(collection.find(x => x._1 == key && x._2 == value)) match {
        case Some(_) => true
        case None => false
      }
      found
    }

    assertEquals(true, foundValues(props, "occursCountKind", "parsed"))
    assertEquals(true, foundValues(props, "lengthKind", "pattern"))
    assertEquals(true, foundValues(props, "representation", "text"))
    assertEquals(true, foundValues(props, "binaryNumberRep", "packed"))

  }
  
  def test_simple_types_property_combining {
    val testSchema = XML.loadFile("test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml")
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1, ge2, ge3, ge4) = sd.globalElementDecls // Obtain global element nodes
  }

  def testTerminatingMarkup {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()
    val ct = ge1.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val sq = ct.modelGroup.group.asInstanceOf[Sequence]
    val Seq(s1, s2) = sq.groupMembers.asInstanceOf[List[LocalElementDecl]]
    val s1tm = s1.terminatingMarkup
    val Seq(ce) = s1tm
    assertTrue(ce.isConstant)
    assertEquals(";", ce.constant)
  }
  
  def testTerminatingMarkup2 {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix" dfdl:separatorPolicy="required" dfdl:terminator=";">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" minOccurs="0" dfdl:occursCountKind="parsed"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()
    val ct = ge1.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val sq = ct.modelGroup.group.asInstanceOf[Sequence]
    val Seq(s1, s2) = sq.groupMembers.asInstanceOf[List[LocalElementDecl]]
    val s1tm = s1.terminatingMarkup
    val Seq(ce) = s1tm
    assertTrue(ce.isConstant)
    assertEquals(",", ce.constant)
    val s2tm = s2.terminatingMarkup
    val Seq(ce1, ce2) = s2tm
    assertTrue(ce1.isConstant)
    assertEquals(",", ce1.constant)
    assertTrue(ce2.isConstant)
    assertEquals(";", ce2.constant)
  }
}