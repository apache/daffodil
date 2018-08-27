/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.dsom

import scala.xml.{ XML, Utility, Node }
import org.junit.Test
import org.apache.daffodil.compiler._
import org.apache.daffodil.Implicits._; object INoWarnDSOM1 { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.schema.annotation.props.gen.{ YesNo, TextNumberRep, SeparatorPosition, Representation, OccursCountKind, NilKind, LengthKind, ChoiceLengthKind, ByteOrder, BinaryNumberRep, AlignmentUnits }

import org.apache.daffodil.schema.annotation.props.AlignmentType
import org.apache.daffodil.util.{ Misc, Logging }
import org.apache.daffodil.xml.XMLUtils
import junit.framework.Assert._
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.util.SchemaUtils
import org.junit.Test
import org.apache.daffodil.schema.annotation.props.Found

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

  @Test def testHasProps() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()

    schemaDoc.defaultFormat
    val tnr = schemaDoc.textNumberRep
    assertEquals(TextNumberRep.Standard, tnr)
    val tnr2 = decl.textNumberRep
    assertEquals(TextNumberRep.Standard, tnr2)
  }

  @Test def testSchemaValidationSubset() {
    val sch: Node = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
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
    val sset = compiler.compileNode(sch).sset
    assertTrue(sset.isError)
    val diagnostics = sset.getDiagnostics.asInstanceOf[Seq[Diagnostic]]
    val msgs = diagnostics.map { _.getMessage() }
    val msg = msgs.mkString("\n")
    val hasErrorText = msg.contains("maxOccurs");
    if (!hasErrorText) fail("Didn't get expected error. Got: " + msg)
  }

  // FIXME: convert this test to TDML or discard. It is testing internal APIs
  // and not defending itself from thrown exceptions the way the real APIs do (or
  // are supposed to anyway.
  @Test def testTypeReferentialError() {
    // LoggingDefaults.setLoggingLevel(LogLevel.OOLAGDebug)
    val sch: Node = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="typeDoesNotExist"/>)
    val pf = Compiler().compileNode(sch)
    assertTrue(pf.isError)
    val msg = pf.getDiagnostics.toString
    val hasErrorText = msg.contains("typeDoesNotExist");
    if (!hasErrorText) fail("Didn't get expected error. Got: " + msg)
  }

  // FIXME - convert this test to TDML or drop if there is coverage other places.
  @Test def testTypeReferentialError2() {
    val sch: Node = <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
                      <element name="foo" type="bar"/><!-- Illegal: no prefix on name of the type. -->
                      <complexType name="bar">
                        <sequence/>
                      </complexType>
                    </schema>
    val pf = Compiler().compileNode(sch)
    assertTrue(pf.isError)
    val msg = pf.getDiagnostics.toString
    val hasErrorText = msg.contains("bar");
    if (!hasErrorText) fail("Didn't get expected error. Got: " + msg)
  }

  @Test def testSchemaValidationPropertyChecking() {
    val s: Node = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="w" type="xsd:int" dfdl:byteOrder="invalidValue" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()
    compiler.setCheckAllTopLevel(true)
    val sset = Compiler().compileNode(s).sset
    sset.isError // forces compilation
    val diags = sset.getDiagnostics
    val msg = diags.toString
    assertTrue(sset.isError)
    val hasErrorText = msg.contains("invalidValue");
    if (!hasErrorText) fail("Didn't get expected error. Got: " + msg)
  }

  @Test def test2() {
    val sc = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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

    val sset = Compiler().compileNode(sc).sset

    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declFactory) = schemaDoc.globalElementDecls
    val decl = declFactory.forRoot()
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)

    decl.formatAnnotation.asInstanceOf[DFDLElement]
    assertEquals(AlignmentUnits.Bytes, decl.alignmentUnits)
  }

  @Test def testSequence1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,

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

    val sset = Compiler().compileNode(w).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val geds @ Seq(_) = schemaDoc.globalElementDecls; assertNotNull(geds)
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)

    val mg = ct.forElement(null).modelGroup.asInstanceOf[Sequence]
    assertTrue(mg.isInstanceOf[Sequence])

    val Seq(elem) = mg.groupMembers
    assertTrue(elem.isInstanceOf[LocalElementDecl])
  }

  @Test def test3 {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml").toURL)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // No annotations
    val Seq(gct) = sd.globalComplexTypeDefs
    assertNotNull(gct)

    // Explore global element decl
    val Seq(e1f, e2f, e3f, _, _) = sd.globalElementDecls // there are 3 factories
    val root1 = e1f.forRoot()
    val e1 = root1.referencedElement
    e2f.forRoot()
    val e3 = e3f.forRoot()
    assertEquals(
      ByteOrder.BigEndian.toString.toLowerCase(),
      e1.formatAnnotation.asInstanceOf[DFDLElement].getPropertyForUnitTest("byteOrder").toLowerCase())
    val Seq(_, a2) = e3.referencedElement.annotationObjs // third one has two annotations
    assertTrue(a2.isInstanceOf[DFDLAssert]) // second annotation is newVariableInstance
    assertEquals(OccursCountKind.Implicit.toString, e3.getProperty("occursCountKind"))
    // Explore local complex type def
    val seq = e1.sequence //... which is a sequence
    seq.formatAnnotation.asInstanceOf[DFDLSequence] //...annotated with...
    assertEquals(YesNo.No, seq.initiatedContent) // initiatedContent="no"

    val Seq(e1a: DFDLElement) = e1.annotationObjs
    assertEquals("UTF-8", e1a.getPropertyForUnitTest("encoding"))

    // Explore global simple type defs
    val Seq(st1, _, _, _) = sd.globalSimpleTypeDefs // there are two.
    val Seq(b1, b2, _, b4) = st1.forElement(e1).annotationObjs // first one has 4 annotations
    assertEquals(AlignmentUnits.Bytes.toString.toLowerCase, b1.asInstanceOf[DFDLSimpleType].getPropertyForUnitTest("alignmentUnits")) // first has alignmentUnits
    assertEquals("tns:myVar1", b2.asInstanceOf[DFDLSetVariable].ref) // second is setVariable with a ref
    assertEquals("yadda yadda yadda", b4.asInstanceOf[DFDLAssert].messageAttrib.get) // fourth is an assert with yadda message

    // Explore define formats
    val Seq(df1, _) = sd.defineFormats // there are two
    val def1 = df1.asInstanceOf[DFDLDefineFormat]
    assertEquals("def1", def1.name) // first is named "def1"
    assertEquals(Representation.Text.toString.toLowerCase, def1.formatAnnotation.getPropertyForUnitTest("representation")) // has representation="text"

    // Explore define variables
    val Seq(dv1, _) = sd.defineVariables // there are two
    assertNotNull(dv1)
    //assertEquals("2003年08月27日", dv2.asInstanceOf[DFDLDefineVariable].defaultValue) // second has kanji chars in default value

    // Explore define escape schemes
    val Seq(desc1) = sd.defineEscapeSchemes // only one of these
    val es = desc1.forComponent(e1a).escapeScheme.escapeCharacterRaw.asInstanceOf[Found].value
    assertEquals("%%", es) // has escapeCharacter="%%" (note: string literals not digested yet, so %% is %%, not %.

    // Explore global group defs
    val Seq(_, gedf, _*) = sd.globalElementDecls
    val root = gedf.forRoot()
    val seq1 = root.complexType.modelGroup.asInstanceOf[SequenceGroupRef]

    val Seq(e1r: ElementRef, cgr: ChoiceGroupRef) = seq1.groupMembers

    val Seq(ggd1, ggd2, _, _, _) = sd.globalGroupDefs // there are two
    assertEquals("gr", ggd1.namedQName.local)
    assertEquals("hiddenGroup", ggd2.namedQName.local)

    //Explore LocalSimpleTypeDef
    val Seq(c1: LocalElementDecl, _: LocalElementDecl, c3: LocalElementDecl) = cgr.groupMembers
    val ist = c3.asInstanceOf[LocalElementDecl].immediateType.get.asInstanceOf[LocalSimpleTypeDef]
    val istBase = ist.optRestriction.get.baseQName.toQNameString
    assertEquals("tns:aType", istBase)

    //Explore LocalElementDecl
    assertEquals(1, c1.maxOccurs)
    val Seq(c1a) = c1.annotationObjs
    assertEquals("{ $myVar1 eq (+47 mod 4) }", c1a.asInstanceOf[DFDLDiscriminator].testBody.get)

    // Explore sequence
    val Seq(ann: DFDLGroup) = seq1.annotationObjs // one format annotation with a property
    assertNotNull(ann)
    assertEquals(SeparatorPosition.Infix, seq1.separatorPosition)
    assertEquals(2, e1r.maxOccurs)
    assertEquals("ex:a", e1r.ref)
  }

  @Test def test4 {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml").toURL)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // Explore global group defs
    val Seq(_, gedf, _*) = sd.globalElementDecls
    val root = gedf.forRoot()
    val seq1 = root.complexType.modelGroup.asInstanceOf[SequenceGroupRef]

    val Seq(_: ElementRef, gr: GroupRef) = seq1.groupMembers
    val cgd = gr.groupDef.asInstanceOf[GlobalChoiceGroupDef]
    val cgr = cgd.groupRef.asInstanceOf[ChoiceGroupRef]
    val Seq(_, cd2: LocalElementDecl, _) = cgd.groupMembers // Children nodes of Choice-node, there are 3

    // val Seq(a1: DFDLChoice) = ch1.annotationObjs // Obtain the annotation object that is a child
    // of the group node.

    assertEquals(AlignmentType.Implicit, cgr.alignment)
    assertEquals(ChoiceLengthKind.Implicit, cgr.choiceLengthKind)

    val Seq(asrt1: DFDLAssert) = cd2.annotationObjs // Obtain Annotation object that is child
    // of cd2.

    assertEquals("{ $myVar1 eq xs:int(xs:string(fn:round-half-to-even(8.5))) }", asrt1.testTxt)
  }

  @Test def test_named_format_chaining {
    val testSchema =
      XML.load(
        Misc.getRequiredResource(
          "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml").toURL)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f, _, _, _, _, _) = sd.globalElementDecls // Obtain global element nodes
    val a1 = ge1f.forRoot()

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

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(_, ge2, _, _, _, _) = sd.globalElementDecls // Obtain global element nodes

    val x = ge2.forRoot()

    assertEquals(AlignmentUnits.Bytes, x.alignmentUnits)
  }

  @Test def test_simple_types_property_combining {
    val testSchema =
      XML.load(
        Misc.getRequiredResource(
          "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml").toURL)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(_, ge2f, ge3f, ge4f, ge5f, ge6f) = sd.globalElementDecls // Obtain global element nodes

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

  @Test def test_simpleType_base_combining {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml").toURL)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // No annotations
    val Seq(gct) = sd.globalComplexTypeDefs
    assertNotNull(gct)

    // Explore global element decl
    val Seq(e1f, e2f, e3f, _, _) = sd.globalElementDecls // there are 3 factories
    val e1 = e1f.forRoot()
    e2f.forRoot()
    e3f.forRoot()

    val Seq(gs1f, _, gs3f, _) = sd.globalSimpleTypeDefs

    val gs1 = gs1f.forElement(e1.referencedElement) // Global Simple Type - aType

    val baseQName = gs1.optRestriction.get.baseQName

    assertEquals("ex", baseQName.prefix.get)
    assertEquals("aaType", baseQName.local)

    assertTrue(gs1.formatAnnotation.verifyPropValue("alignmentUnits", "bytes")) // SimpleType - Local

    assertTrue(e1.verifyPropValue("byteOrder", "bigEndian")) // SimpleType - Base
    assertTrue(e1.verifyPropValue("occursCountKind", "implicit")) // Default Format
    assertTrue(e1.verifyPropValue("representation", "text")) // Define Format - def1
    assertTrue(e1.verifyPropValue("encoding", "UTF-8")) // Define Format - def1
    assertTrue(e1.verifyPropValue("textStandardBase", "10")) // Define Format - def2
    assertTrue(e1.verifyPropValue("escapeSchemeRef", "tns:quotingScheme")) // Define Format - def2

    val gs3 = gs3f.forElement(e1.referencedElement) // Global SimpleType - aTypeError - overlapping base props

    // Tests overlapping properties
    // because these unit tests are outside the normal framework,
    // we sometimes have to demand things in order for errors to be noticed.
    assertTrue(gs3.isError)
    val msgs = gs3.getDiagnostics.mkString("\n").toLowerCase
    assertTrue(msgs.contains("overlap"))
    assertTrue(msgs.contains("alignmentUnits".toLowerCase))
  }

  @Test def test_group_references {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml").toURL)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // No annotations
    val Seq(g) = sd.globalComplexTypeDefs;
    assertNotNull(g)

    // Explore global element decl
    val Seq(_, _, _, e4f, e5f) = sd.globalElementDecls // there are 3 factories

    // GroupRefTest
    val e4 = e4f.forRoot() // groupRefTest

    val e4ct = e4.complexType

    val e4ctgref = e4ct.modelGroup.asInstanceOf[GroupRef]

    val myGlobal1Seq = e4ctgref.asInstanceOf[SequenceTermBase]

    val myGlobal2Seq = myGlobal1Seq.groupMembers(0).asInstanceOf[SequenceGroupRef]

    // myGlobal1 Properties
    assertTrue(myGlobal1Seq.verifyPropValue("separator", ","))

    // myGlobal2 Properties
    assertTrue(myGlobal2Seq.verifyPropValue("separator", ";"))
    assertTrue(myGlobal2Seq.verifyPropValue("separatorPosition", "infix"))

    // GroupRefTestOverlap

    val e5 = e5f.forRoot() // groupRefTestOverlap

    val e5ct = e5.complexType

    val e5ctgref = e5ct.modelGroup.asInstanceOf[SequenceGroupRef]
    val myGlobal3 = e5ctgref.groupDef
    // val myGlobal3Seq =
    myGlobal3.asInstanceOf[GlobalSequenceGroupDef]

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
    val sset = new SchemaSet(ibm7132Schema)
    // val Seq(sch) = sset.schemas
    val Seq(sd) = sset.allSchemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()

    val f1 = ge1

    assertTrue(f1.verifyPropValue("separatorPosition", "infix"))
    assertTrue(f1.verifyPropValue("lengthKind", "implicit"))
    assertFalse(f1.verifyPropValue("representation", "text"))
    assertTrue(f1.verifyPropValue("textNumberRep", "standard"))

    val ct = ge1.complexType
    val seq = ct.sequence

    val Seq(e1: ElementBase, _: ElementBase) = seq.groupMembers
    //
    assertTrue(e1.verifyPropValue("initiator", ""))
    assertTrue(e1.verifyPropValue("representation", "text"))

    e1.lengthKind
  }

  @Test def testDfdlRef() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:defineFormat name="ref1"> <dfdl:format initiator=":"/> </dfdl:defineFormat>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:ref="tns:ref1" type="xs:string">
      </xs:element>)
    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()
    ge1.formatAnnotation.properties

  }

  @Test def testGetQName = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:defineFormat name="ref1">
        <dfdl:format initiator=":" alignmentUnits="bits"/>
      </dfdl:defineFormat>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:ref="tns:ref1" type="xs:string">
      </xs:element>)
    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot().referencedElement

    val qn = ge1.formatAnnotation.resolveQName("tns:ref1")
    val nsURI = qn.namespace
    val localName = qn.local

    assertEquals("ref1", localName)
    assertEquals(XMLUtils.EXAMPLE_NAMESPACE, nsURI)
  }

  @Test def testGetAllNamespaces() {
    val xml = <bar xmlns:foo="fooNS" xmlns:bar="barNS">
                <quux xmlns:baz="bazNS" attr1="x"/>
              </bar>

    val scope = (xml \ "quux")(0).scope
    scala.xml.Elem("dfdl", "element", scala.xml.Null, scope, true)
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
    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot().referencedElement

    val seq = ge1.sequence

    val Seq(e1: LocalElementDecl, e2: LocalElementDecl) = seq.groupMembers
    e1.formatAnnotation.asInstanceOf[DFDLElement]
    // val props = e1.properties

    val e1f_esref = e1.getProperty("escapeSchemeRef")

    assertEquals("pound", e1f_esref)

    // Should have escapeCharacter and escapeKind

    e2.formatAnnotation.asInstanceOf[DFDLElement]
    val e2f_esref = e2.getProperty("escapeSchemeRef")
    // escapeBlockStart/End escapeBlockKind (NOTHING ELSE)
    assertEquals("cStyleComment", e2f_esref)
  }

  @Test def test_element_references {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml").toURL)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    val Seq(_, gedf, _*) = sd.globalElementDecls
    val root = gedf.forRoot()
    val sgr = root.complexType.modelGroup.asInstanceOf[SequenceGroupRef]

    val Seq(e1r: ElementRef, cgr: ChoiceGroupRef) = sgr.groupMembers

    assertEquals("hiddenGroup", cgr.groupDef.namedQName.local)
    assertEquals(AlignmentType.Implicit, cgr.alignment)

    assertEquals(2, e1r.maxOccurs)
    assertEquals(1, e1r.minOccurs)
    assertEquals(AlignmentUnits.Bytes, e1r.alignmentUnits)
    //assertEquals(true, e1.nillable) // TODO: e1.nillable doesn't exist?
    //assertEquals("%ES; %% %#0; %NUL;%ACK; foo%#rF2;%#rF7;bar %WSP*; %#2024;%#xAABB; &amp;&#2023;&#xCCDD; -1", e1.nilValue) // TODO: Do not equal each other!
    assertEquals(NilKind.LiteralValue, e1r.nilKind)
  }

  @Test def testPathWithIndexes() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
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
    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot().referencedElement

    val seq = ge1.sequence

    val Seq(_, _, s3) = seq.groupMembers
    val s3s = s3.asInstanceOf[Sequence]
    val Seq(es) = s3s.groupMembers
    val ese = es.asInstanceOf[LocalElementDecl]
    assertTrue(ese.path.contains("sequence[3]"))
  }

}
