package edu.illinois.ncsa.daffodil.dsom
import scala.xml.{ XML, Utility, Node }
import org.junit.Test
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.{ YesNo, TextNumberRep, SeparatorPosition, Representation, OccursCountKind, NilKind, LengthKind, ChoiceLengthKind, ByteOrder, BinaryNumberRep, AlignmentUnits }
import edu.illinois.ncsa.daffodil.schema.annotation.props.AlignmentType
import edu.illinois.ncsa.daffodil.util.{ Misc, Logging }
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import junit.framework.Assert.{ assertTrue, assertEquals, assertFalse, fail }
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.processors.PrimitiveFactory
import org.junit.Test
import edu.illinois.ncsa.daffodil.util.Fakes

class TestDsomCompiler extends Logging {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  // The below is lazy for a reason.
  // It defers evaluation until used. This is nice because suppose there is a bug
  // in the Fakes stuff. Then you want tests that use that to fail. But lots of 
  // these tests don't use this. If you make this an eager val, then if there
  // is any problem in the Fakes, the whole class can't be constructed, and None
  // of the tests will run. Lazy lets this class be constructed no matter what.
  lazy val dummyGroupRef = Fakes.fakeGroupRef

  @Test def testHasProps() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()

    val df = schemaDoc.defaultFormat
    val tnr = schemaDoc.textNumberRep
    assertEquals(TextNumberRep.Standard, tnr)
    val tnr2 = decl.textNumberRep
    assertEquals(TextNumberRep.Standard, tnr2)
  }

  @Test def testSchemaValidationSubset() {
    val sch: Node = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence maxOccurs="2">
            <!-- DFDL SUBSET DOESN'T ALLOW MULTIPLE RECURRING SEQUENCE OR CHOICE -->
            <xs:element name="w" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    compiler.setCheckAllTopLevel(true)
    val (sset, _) = compiler.frontEnd(sch)
    assertTrue(sset.isError)
    val diagnostics = sset.getDiagnostics.asInstanceOf[Seq[Diagnostic]]
    val msgs = diagnostics.map { _.getMessage }
    val msg = msgs.mkString("\n")
    val hasErrorText = msg.contains("maxOccurs");
    if (!hasErrorText) fail("Didn't get expected error. Got: " + msg)
  }

  // FIXME: convert this test to TDML or discard. It is testing internal APIs
  // and not defending itself from thrown exceptions the way the real APIs do (or
  // are supposed to anyway.
  @Test def testTypeReferentialError() {
    val sch: Node = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="typeDoesNotExist"/>)
    val (_, pf) = Compiler().compileInternal(sch)
    assertTrue(pf.isError)
    val msg = pf.getDiagnostics.toString
    val hasErrorText = msg.contains("typeDoesNotExist");
    if (!hasErrorText) fail("Didn't get expected error. Got: " + msg)
  }

  // FIXME - convert this test to TDML or drop if there is coverage other places.
  @Test def testTypeReferentialError2() {
    val sch: Node = <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com">
                      <element name="foo" type="bar"/><!-- Illegal: no prefix on name of the type. -->
                      <complexType name="bar">
                        <sequence/>
                      </complexType>
                    </schema>
    val (_, pf) = Compiler().compileInternal(sch)
    assertTrue(pf.isError)
    val msg = pf.getDiagnostics.toString
    println(msg)
    val hasErrorText = msg.contains("bar");
    if (!hasErrorText) fail("Didn't get expected error. Got: " + msg)
  }

  @Test def testSchemaValidationPropertyChecking() {
    val s: Node = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="w" type="xsd:int" dfdl:byteOrder="invalidValue" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()
    compiler.setCheckAllTopLevel(true)
    val (sset, _) = Compiler().frontEnd(s)
    sset.isError // forces compilation
    val diags = sset.getDiagnostics
    // diags.foreach { println(_) }
    val msg = diags.toString
    assertTrue(sset.isError)
    val hasErrorText = msg.contains("invalidValue");
    if (!hasErrorText) fail("Didn't get expected error. Got: " + msg)
  }

  @Test def test2() {
    val sc = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator="">
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)

    val (sset, _) = Compiler().frontEnd(sc)

    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declFactory) = schemaDoc.globalElementDecls
    val decl = declFactory.forRoot()
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)

    val fa = decl.formatAnnotation.asInstanceOf[DFDLElement]
    assertEquals(AlignmentUnits.Bytes, decl.alignmentUnits)
    //    fa.alignmentUnits match {
    //      case AlignmentUnits.Bits => println("was bits")
    //      case AlignmentUnits.Bytes => println("was bytes")
    //    }
  }

  /* @Test def testXsomMultifile(){
   
    val parser = new XSOMParser()
    val apf = new DomAnnotationParserFactory()
    parser.setAnnotationParser(apf)

    val inFile = new File(Misc.getRequiredResource("/test/first.xsd"))

    parser.parse(inFile)

    val sset = parser.getResult()
    val sds = parser.getDocuments().toList
    assertTrue(sds.size() >= 2)
  
    // sds.map{sd => println(sd.getSystemId)}
  }*/

  @Test def testSequence1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separatorSuppressionPolicy="never" dfdl:separator="">
          <xs:element name="w" type="xs:int" maxOccurs="1" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:occursCountKind="fixed"/>
        </xs:sequence>
      </xs:complexType>)

    val w = Utility.trim(testSchema)

    val (sset, _) = Compiler().frontEnd(w)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(decl) = schemaDoc.globalElementDecls
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)

    val mg = ct.forElement(null).modelGroup.asInstanceOf[Sequence]
    assertTrue(mg.isInstanceOf[Sequence])

    val Seq(elem) = mg.groupMembers
    assertTrue(elem.isInstanceOf[LocalElementDecl])
  }

  @Test def test3 {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml").toURL)
    val compiler = Compiler()

    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // No annotations
    val Seq(ct) = sd.globalComplexTypeDefs

    // Explore global element decl
    val Seq(e1f, e2f, e3f, e4f, e5f) = sd.globalElementDecls // there are 3 factories
    val e1 = e1f.forRoot()
    val e2 = e2f.forRoot()
    val e3 = e3f.forRoot()
    assertEquals(
      ByteOrder.BigEndian.toString().toLowerCase(),
      e1.formatAnnotation.asInstanceOf[DFDLElement].getProperty("byteOrder").toLowerCase())
    val Seq(a1, a2) = e3.annotationObjs // third one has two annotations
    assertTrue(a2.isInstanceOf[DFDLAssert]) // second annotation is newVariableInstance
    assertEquals("implicit", a1.asInstanceOf[DFDLElement].getProperty("occursCountKind"))
    val e1ct = e1.immediateType.get.asInstanceOf[LocalComplexTypeDef] // first one has immediate complex type
    // Explore local complex type def
    val seq = e1ct.modelGroup.asInstanceOf[Sequence] //... which is a sequence
    val sfa = seq.formatAnnotation.asInstanceOf[DFDLSequence] //...annotated with...
    assertEquals(YesNo.No, seq.initiatedContent) // initiatedContent="no"

    val Seq(e1a: DFDLElement) = e1.annotationObjs
    assertEquals("UTF-8", e1a.getProperty("encoding"))

    // Explore global simple type defs
    val Seq(st1, st2, st3, st4) = sd.globalSimpleTypeDefs // there are two.
    val Seq(b1, b2, b3, b4) = st1.forElement(e1).annotationObjs // first one has 4 annotations
    assertEquals(AlignmentUnits.Bytes.toString.toLowerCase, b1.asInstanceOf[DFDLSimpleType].getProperty("alignmentUnits")) // first has alignmentUnits
    assertEquals("tns:myVar1", b2.asInstanceOf[DFDLSetVariable].ref) // second is setVariable with a ref
    assertEquals("yadda yadda yadda", b4.asInstanceOf[DFDLAssert].message) // fourth is an assert with yadda message

    // Explore define formats
    val Seq(df1, df2) = sd.defineFormats // there are two
    val def1 = df1.asInstanceOf[DFDLDefineFormat]
    assertEquals("def1", def1.name) // first is named "def1"
    assertEquals(Representation.Text.toString.toLowerCase, def1.formatAnnotation.getProperty("representation")) // has representation="text"

    // Explore define variables
    val Seq(dv1, dv2) = sd.defineVariables // there are two
    //assertEquals("2003年08月27日", dv2.asInstanceOf[DFDLDefineVariable].defaultValue) // second has kanji chars in default value

    // Explore define escape schemes
    val Seq(desc1) = sd.defineEscapeSchemes // only one of these
    val es = desc1.forComponent(e1a).escapeScheme.escapeCharacterRaw.value
    assertEquals("%%", es) // has escapeCharacter="%%" (note: string literals not digested yet, so %% is %%, not %.

    // Explore global group defs
    val Seq(gr1, gr2, gr3, gr4, gr5) = sd.globalGroupDefs // there are two
    val seq1 = gr1.forGroupRef(dummyGroupRef, 1).modelGroup.asInstanceOf[Sequence]

    //Explore LocalSimpleTypeDef
    val Seq(gr2c1, gr2c2, gr2c3) = gr2.forGroupRef(dummyGroupRef, 1).modelGroup.asInstanceOf[ModelGroup].groupMembers
    val ist = gr2c3.asInstanceOf[LocalElementDecl].immediateType.get.asInstanceOf[LocalSimpleTypeDef]
    assertEquals("tns:aType", ist.baseName)

    //Explore LocalElementDecl
    val led = gr2c1.asInstanceOf[LocalElementDecl]
    assertEquals(1, led.maxOccurs)
    val Seq(leda) = led.annotationObjs
    assertEquals("{ $myVar1 eq (+47 mod 4) }", leda.asInstanceOf[DFDLDiscriminator].testBody.get)

    // Explore sequence
    val Seq(seq1a: DFDLSequence) = seq1.annotationObjs // one format annotation with a property
    assertEquals(SeparatorPosition.Infix, seq1.separatorPosition)
    val Seq(seq1e1, seq1s1) = seq1.groupMembers // has an element and a sub-sequence as its children.
    assertEquals(2, seq1e1.asInstanceOf[ElementRef].maxOccurs)
    assertEquals("ex:a", seq1e1.asInstanceOf[ElementRef].ref)
    assertEquals(1, seq1s1.asInstanceOf[Sequence].groupMembers.length) // it has the hidden group
  }

  @Test def test4 {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml").toURL)
    val compiler = Compiler()

    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(gd1, gd2f, gd3, gd4, gd5) = sd.globalGroupDefs // Obtain Group nodes
    val gd2 = gd2f.forGroupRef(dummyGroupRef, 1)
    val ch1 = gd2.modelGroup.asInstanceOf[Choice] // Downcast child-node of group to Choice
    val Seq(cd1, cd2, cd3) = ch1.groupMembers // Children nodes of Choice-node, there are 3

    val Seq(a1: DFDLChoice) = ch1.annotationObjs // Obtain the annotation object that is a child
    // of the group node.

    assertEquals(AlignmentType.Implicit, ch1.alignment)
    assertEquals(ChoiceLengthKind.Implicit, ch1.choiceLengthKind)

    val Seq(asrt1) = cd2.asInstanceOf[LocalElementDecl].annotationObjs // Obtain Annotation object that is child
    // of cd2.

    assertEquals("{ $myVar1 eq xs:int(xs:string(fn:round-half-to-even(8.5))) }", asrt1.asInstanceOf[DFDLAssert].testTxt)
  }

  @Test def test_named_format_chaining {
    val testSchema =
      XML.load(
        Misc.getRequiredResource(
          "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml").toURL)

    val compiler = Compiler()
    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f, ge2f, ge3f, ge4f, ge5f, ge6f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()
    val Seq(a1: DFDLElement) = ge1.annotationObjs

    assertEquals(true, a1.verifyPropValue("occursCountKind", "parsed"))
    assertEquals(true, a1.verifyPropValue("lengthKind", "pattern"))
    assertEquals(true, a1.verifyPropValue("representation", "text"))
    assertEquals(true, a1.verifyPropValue("binaryNumberRep", "packed"))
  }

  @Test def test_simple_types_access_works {
    val testSchema =
      XML.load(
        Misc.getRequiredResource(
          "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml").toURL)

    val compiler = Compiler()

    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1, ge2, ge3, ge4, ge5, ge6) = sd.globalElementDecls // Obtain global element nodes

    val x = ge2.forRoot().typeDef.asInstanceOf[LocalSimpleTypeDef]

    assertEquals(AlignmentUnits.Bytes, x.alignmentUnits)
  }

  @Test def test_simple_types_property_combining {
    val testSchema =
      XML.load(
        Misc.getRequiredResource(
          "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml").toURL)

    val compiler = Compiler()

    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f, ge2f, ge3f, ge4f, ge5f, ge6f) = sd.globalElementDecls // Obtain global element nodes

    val ge2 = ge2f.forRoot()
    val ge3 = ge3f.forRoot()
    val ge4 = ge4f.forRoot()
    val ge5 = ge5f.forRoot()
    val ge6 = ge6f.forRoot()

    assertEquals(AlignmentUnits.Bytes, ge2.alignmentUnits)

    assertEquals(AlignmentUnits.Bytes, ge3.alignmentUnits)
    assertEquals(NilKind.LiteralValue, ge3.nilKind)

    // Tests overlapping properties
    ge4.lengthKind
    assertTrue(ge4.isError)

    assertEquals(AlignmentUnits.Bytes, ge5.alignmentUnits) // local
    assertEquals(OccursCountKind.Parsed, ge5.occursCountKind) // def1
    assertEquals(BinaryNumberRep.Packed, ge5.binaryNumberRep) // def3
    assertEquals(NilKind.LiteralValue, ge5.nilKind) // local
    assertEquals(Representation.Text, ge5.representation) // def3
    assertEquals(LengthKind.Pattern, ge5.lengthKind) // superseded by local

    // Test Defaulting
    assertEquals(BinaryNumberRep.Packed, ge6.binaryNumberRep)
  }

  @Test def testTerminatingMarkup {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments
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

  @Test def testTerminatingMarkup2 {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix" dfdl:separatorSuppressionPolicy="never" dfdl:terminator=";">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" minOccurs="0" dfdl:occursCountKind="parsed"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

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

  @Test def test_simpleType_base_combining {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml").toURL)
    val compiler = Compiler()

    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // No annotations
    val Seq(ct) = sd.globalComplexTypeDefs

    // Explore global element decl
    val Seq(e1f, e2f, e3f, e4f, e5f) = sd.globalElementDecls // there are 3 factories
    val e1 = e1f.forRoot()
    val e2 = e2f.forRoot()
    val e3 = e3f.forRoot()

    val Seq(gs1f, gs2f, gs3f, gs4f) = sd.globalSimpleTypeDefs

    val gs1 = gs1f.forElement(e1) // Global Simple Type - aType

    assertEquals("ex:aaType", gs1.restrictionBase)

    // println(gs1.properties)
    assertTrue(gs1.verifyPropValue("alignmentUnits", "bytes")) // SimpleType - Local

    assertTrue(gs1.verifyPropValue("byteOrder", "bigEndian")) // SimpleType - Base
    assertTrue(gs1.verifyPropValue("occursCountKind", "implicit")) // Default Format
    assertTrue(gs1.verifyPropValue("representation", "text")) // Define Format - def1
    assertTrue(gs1.verifyPropValue("encoding", "utf-8")) // Define Format - def1
    assertTrue(gs1.verifyPropValue("textStandardBase", "10")) // Define Format - def2
    assertTrue(gs1.verifyPropValue("escapeSchemeRef", "tns:quotingScheme")) // Define Format - def2

    val gs3 = gs3f.forElement(e1) // Global SimpleType - aTypeError - overlapping base props

    // Tests overlapping properties

    //    println(gs3.nonDefaultPropertySources)
    //    println(gs3.defaultPropertySources)
    // gs3.valueAsAny // because these unit tests are outside the normal framework,
    // we sometimes have to demand things in order for errors to be noticed.
    assertTrue(gs3.isError)
    val msgs = gs3.getDiagnostics.mkString("\n").toLowerCase
    assertTrue(msgs.contains("overlap"))
    //    println(msgs)
    assertTrue(msgs.contains("alignmentUnits".toLowerCase))
  }

  @Test def test_group_references {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml").toURL)
    val compiler = Compiler()

    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // No annotations
    val Seq(ct) = sd.globalComplexTypeDefs

    // Explore global element decl
    val Seq(e1f, e2f, e3f, e4f, e5f) = sd.globalElementDecls // there are 3 factories

    // GroupRefTest
    val e4 = e4f.forRoot() // groupRefTest
    // println(e4)

    val e4ct = e4.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // println(e4ct)
    val e4ctgref = e4ct.modelGroup.asInstanceOf[GroupRef] // groupRefTests' local group decl

    // println(e4ctgref)
    val myGlobal1 = e4ctgref.groupDef
    // println(myGlobal1)

    val myGlobal1Seq = myGlobal1.modelGroup.asInstanceOf[Sequence]

    // println(myGlobal1Seq)
    val myGlobal2Seq = myGlobal1Seq.groupRefChildren(0).group.asInstanceOf[Sequence]

    //    println(myGlobal2Seq)
    //    println(myGlobal1Seq.properties)
    //    println(myGlobal2Seq.properties)

    // val myGlobal2Seq = myGlobal2.modelGroup.asInstanceOf[Sequence]

    // myGlobal1 Properties
    assertTrue(myGlobal1Seq.verifyPropValue("separator", ","))

    // myGlobal2 Properties
    assertTrue(myGlobal2Seq.verifyPropValue("separator", ";"))
    assertTrue(myGlobal2Seq.verifyPropValue("separatorPosition", "infix"))

    // GroupRefTestOverlap

    val e5 = e5f.forRoot() // groupRefTestOverlap

    val e5ct = e5.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    val e5ctgref = e5ct.modelGroup.asInstanceOf[GroupRef] // groupRefTestOverlap's local group decl

    val myGlobal3 = e5ctgref.groupDef
    val myGlobal3Seq = myGlobal3.modelGroup.asInstanceOf[Sequence]

    // Tests overlapping properties
    assertTrue(e5ctgref.isError)

    val msg = e5ctgref.getDiagnostics.mkString("\n").toLowerCase
    val res = msg.contains("overlap")
    if (!res) println(msg)
    assertTrue(res)

  }

  @Test def test_ibm_7132 {
    val ibm7132Schema = XML.load(Misc.getRequiredResource("/test/TestRefChainingIBM7132.dfdl.xml").toURL)
    // val ibm7132Schema = "test/TestRefChainingIBM7132.dfdl.xml"
    val compiler = Compiler()
    val sset = new SchemaSet(PrimitiveFactory, ibm7132Schema)
    // val Seq(sch) = sset.schemas
    val Seq(sd) = sset.allSchemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()

    val f1 = ge1.formatAnnotation

    // println(f1.properties)

    assertTrue(f1.verifyPropValue("separatorPosition", "infix"))
    assertTrue(f1.verifyPropValue("lengthKind", "implicit"))
    assertFalse(f1.verifyPropValue("representation", "text"))
    assertTrue(f1.verifyPropValue("textNumberRep", "standard"))

    val ct = ge1.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]

    val Seq(e1: ElementBase, e2: ElementBase) = seq.groupMembers

    val e1f = e1.formatAnnotation.asInstanceOf[DFDLElement]
    //
    assertTrue(e1f.verifyPropValue("initiator", ""))
    assertTrue(e1f.verifyPropValue("representation", "text"))
    //println(e1f.initiatorRaw)

    //e1f.initiatorRaw
    //e1f.byteOrderRaw
    e1.lengthKind
  }

  @Test def testDfdlRef = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:defineFormat name="ref1"> <dfdl:format initiator=":"/> </dfdl:defineFormat>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:ref="tns:ref1" type="xs:string">
      </xs:element>)
    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()
    val props = ge1.formatAnnotation.properties

    // println(props)
    //assertEquals(":", ge1.initiatorRaw)
  }

  @Test def testGetQName = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:defineFormat name="ref1">
        <dfdl:format initiator=":"/>
      </dfdl:defineFormat>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:ref="tns:ref1" type="xs:string">
      </xs:element>)
    // println(testSchema)
    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()

    val (nsURI, localName) = ge1.formatAnnotation.resolveQName("tns:ref1")

    // println(nsURI + ", " + localName)
    assertEquals("ref1", localName)
    assertEquals(XMLUtils.EXAMPLE_NAMESPACE, nsURI)
  }

  @Test def testGetAllNamespaces() {
    val xml = <bar xmlns:foo="fooNS" xmlns:bar="barNS">
                <quux xmlns:baz="bazNS" attr1="x"/>
              </bar>

    val scope = (xml \ "quux")(0).scope
    // println(scope)
    val newElem = scala.xml.Elem("dfdl", "element", scala.xml.Null, scope, true)
    // println(newElem)
  }

  @Test def test_delim_inheritance {
    val delimiterInheritance = XML.load(Misc.getRequiredResource("/test/TestDelimiterInheritance.dfdl.xml").toURL)

    val compiler = Compiler()
    val sset = new SchemaSet(PrimitiveFactory, delimiterInheritance)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()

    val ct = ge1.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]

    val Seq(e1: LocalElementDecl, e2: ElementBase, e3: ElementBase) = seq.groupMembers

    //    println(e1.properties)
    assertEquals(3, e1.allTerminatingMarkup.length) // 1 Level + ref on global element decl
    //    assertEquals("a", e1.allTerminatingMarkup(0).prettyExpr)
    //    assertEquals("b", e1.allTerminatingMarkup(1).prettyExpr)
    //    assertEquals("g", e1.allTerminatingMarkup(2).prettyExpr)
    assertEquals("a", e1.allTerminatingMarkup(0)._1.prettyExpr)
    assertEquals("b", e1.allTerminatingMarkup(1)._1.prettyExpr)
    assertEquals("g", e1.allTerminatingMarkup(2)._1.prettyExpr)

    val ct2 = e3.asInstanceOf[ElementBase].typeDef.asInstanceOf[ComplexTypeBase]
    val seq2 = ct2.modelGroup.asInstanceOf[Sequence]

    val Seq(e3_1: ElementBase, e3_2: ElementBase) = seq2.groupMembers

    assertEquals(6, e3_1.allTerminatingMarkup.length) // 2 Level + ref on global element decl
    assertEquals("e", e3_1.allTerminatingMarkup(0)._1.prettyExpr)
    assertEquals("c", e3_1.allTerminatingMarkup(1)._1.prettyExpr)
    assertEquals("d", e3_1.allTerminatingMarkup(2)._1.prettyExpr)
    assertEquals("a", e3_1.allTerminatingMarkup(3)._1.prettyExpr)
    assertEquals("b", e3_1.allTerminatingMarkup(4)._1.prettyExpr)
    assertEquals("g", e3_1.allTerminatingMarkup(5)._1.prettyExpr)

    assertEquals(6, e3_2.allTerminatingMarkup.length) // 2 Level + ref on global element decl + ref on local element decl
    assertEquals("f", e3_2.allTerminatingMarkup(0)._1.prettyExpr) // f instead of e, due to ref
    assertEquals("c", e3_2.allTerminatingMarkup(1)._1.prettyExpr)
    assertEquals("d", e3_2.allTerminatingMarkup(2)._1.prettyExpr)
    assertEquals("a", e3_2.allTerminatingMarkup(3)._1.prettyExpr)
    assertEquals("b", e3_2.allTerminatingMarkup(4)._1.prettyExpr)
    assertEquals("g", e3_2.allTerminatingMarkup(5)._1.prettyExpr)
  }

  @Test def test_escapeSchemeOverride = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format separator="" initiator="" terminator="" emptyValueDelimiterPolicy="none" textNumberRep="standard" representation="text" occursStopValue="-1" occursCountKind="expression" escapeSchemeRef="pound"/>
      <dfdl:defineEscapeScheme name="pound">
        <dfdl:escapeScheme escapeCharacter='#' escapeKind="escapeCharacter"/>
      </dfdl:defineEscapeScheme>
      <dfdl:defineEscapeScheme name='cStyleComment'>
        <dfdl:escapeScheme escapeBlockStart='/*' escapeBlockEnd='*/' escapeKind="escapeBlock"/>
      </dfdl:defineEscapeScheme>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="character" type="xsd:string" maxOccurs="unbounded" dfdl:representation="text" dfdl:separator="," dfdl:terminator="%NL;"/>
            <xs:element name="block" type="xsd:string" maxOccurs="unbounded" dfdl:representation="text" dfdl:separator="," dfdl:terminator="%NL;" dfdl:escapeSchemeRef="cStyleComment"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()

    val ct = ge1.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]

    val Seq(e1: LocalElementDecl, e2: LocalElementDecl) = seq.groupMembers
    val e1f = e1.formatAnnotation.asInstanceOf[DFDLElement]
    // val props = e1.properties

    val e1f_esref = e1.getProperty("escapeSchemeRef")
    // println(e1f_esref)

    assertEquals("pound", e1f_esref)

    // Should have escapeCharacter and escapeKind

    val e2f = e2.formatAnnotation.asInstanceOf[DFDLElement]
    val e2f_esref = e2.getProperty("escapeSchemeRef")
    // escapeBlockStart/End escapeBlockKind (NOTHING ELSE)
    assertEquals("cStyleComment", e2f_esref)
  }

  @Test def test_element_references {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml").toURL)

    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // No annotations
    val Seq(ct) = sd.globalComplexTypeDefs

    // g1.name == "gr"
    val Seq(g1: GlobalGroupDefFactory, g2, g3, g4, g5) = sd.globalGroupDefs

    val seq1 = g1.forGroupRef(dummyGroupRef, 1).modelGroup.asInstanceOf[Sequence]

    // e1.ref == "ex:a"
    val Seq(e1r: ElementRef, s1: Sequence) = seq1.groupMembers
    val e1 = e1r.referencedElement
    assertEquals(2, e1r.maxOccurs)
    assertEquals(1, e1r.minOccurs)
    assertEquals(AlignmentUnits.Bytes, e1.alignmentUnits)
    //assertEquals(true, e1.nillable) // TODO: e1.nillable doesn't exist?
    //assertEquals("%ES; %% %#0; %NUL;%ACK; foo%#rF2;%#rF7;bar %WSP*; %#2024;%#xAABB; &amp;&#2023;&#xCCDD; -1", e1.nilValue) // TODO: Do not equal each other!
    assertEquals(NilKind.LiteralValue, e1.nilKind)
  }

  @Test def testPathWithIndexes() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="r" type="tns:myType"/>
      <xs:complexType name="myType">
        <xs:sequence>
          <xs:sequence/>
          <xs:sequence/>
          <xs:sequence>
            <xs:element name="s" type="xs:int"/>
          </xs:sequence>
        </xs:sequence>
      </xs:complexType>)
    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()

    val ct = ge1.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]

    val Seq(s1, s2, s3) = seq.groupMembers
    val s3s = s3.asInstanceOf[Sequence]
    val Seq(es) = s3s.groupMembers
    val ese = es.asInstanceOf[LocalElementDecl]
    // println(ese)
    assertTrue(ese.path.contains("sequence[3]"))
  }

}

