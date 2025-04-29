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

package org.apache.daffodil.core.dsom

import java.nio.file.Paths
import scala.xml.Node
import scala.xml.Utility
import scala.xml.XML

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits._

import org.junit.Assert._
import org.junit.Test; object INoWarnDSOM1 { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.core.compiler._
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.iapi.URISchemaSource
import org.apache.daffodil.lib.schema.annotation.props.AlignmentType
import org.apache.daffodil.lib.schema.annotation.props.Found
import org.apache.daffodil.lib.schema.annotation.props.gen.AlignmentUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.BinaryNumberRep
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ChoiceLengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.NilKind
import org.apache.daffodil.lib.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.lib.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.lib.schema.annotation.props.gen.TextNumberRep
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.DaffodilXMLLoader
import org.apache.daffodil.lib.xml.XMLUtils

class TestDsomCompiler {

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

  @Test def testHasProps(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>
    )

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val Seq(decl) = schemaDoc.globalElementDecls.map { _.asRoot }

    val tnr = decl.textNumberRep
    assertEquals(TextNumberRep.Standard, tnr)
  }

  @Test def testSchemaValidationSubset(): Unit = {
    val sch: Node = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence maxOccurs="2">
            <!-- DFDL SUBSET DOESN'T ALLOW MULTIPLE RECURRING SEQUENCE OR CHOICE -->
            <xs:element name="w" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val compiler = Compiler().withCheckAllTopLevel(true)
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
  @Test def testTypeReferentialError(): Unit = {
    val sch: Node = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="typeDoesNotExist"/>
    )
    val pf = Compiler().compileNode(sch)
    assertTrue(pf.isError)
    val msg = pf.getDiagnostics.toString
    val hasErrorText = msg.contains("typeDoesNotExist");
    if (!hasErrorText) fail("Didn't get expected error. Got: " + msg)
  }

  // FIXME - convert this test to TDML or drop if there is coverage other places.
  @Test def testTypeReferentialError2(): Unit = {
    val sch: Node =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
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

  @Test def testSchemaValidationPropertyChecking(): Unit = {
    val s: Node = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="w" type="xsd:int" dfdl:byteOrder="invalidValue" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val compiler = Compiler().withCheckAllTopLevel(true)
    val sset = Compiler().compileNode(s).sset
    sset.isError // forces compilation
    val diags = sset.getDiagnostics
    val msg = diags.toString
    assertTrue(sset.isError)
    val hasErrorText = msg.contains("invalidValue");
    if (!hasErrorText) fail("Didn't get expected error. Got: " + msg)
  }

  @Test def test2(): Unit = {
    val sc = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={dfdl}>
            <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator="">
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>
    )

    val sset = Compiler().compileNode(sc).sset

    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val Seq(decl) = schemaDoc.globalElementDecls.map { _.asRoot }
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)

    decl.formatAnnotation.asInstanceOf[DFDLElement]
    assertEquals(AlignmentUnits.Bytes, decl.alignmentUnits)
  }

  @Test def testSequence1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={dfdl}>
            <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separatorSuppressionPolicy="never" dfdl:separator="">
          <xs:element name="w" type="xs:int" maxOccurs="1" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:occursCountKind="fixed"/>
        </xs:sequence>
      </xs:complexType>
    )

    val w = Utility.trim(testSchema)

    val sset = Compiler().compileNode(w).sset
    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val geds @ Seq(_) = schemaDoc.globalElementDecls; assertNotNull(geds)
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)

    val mg = ct.modelGroup.asInstanceOf[LocalSequence]
    assertTrue(mg.isInstanceOf[LocalSequence])

    val Seq(elem) = mg.groupMembers
    assertTrue(elem.isInstanceOf[LocalElementDecl])
  }

  private val exampleOfMostDFDLConstructsXML = "/test/example-of-most-dfdl-constructs.dfdl.xml"

  @Test def test3(): Unit = {
    // Newer versions of the scala-xml library changed the XML.load() function
    // so that it does not ignore comments/processing instructions. This causes
    // issues with parts of the schema compilation that expects these to be
    // removed. So for this particular test, use the DaffodilXMLLoader (which
    // does remove comments and what is normally used by Daffodil) to load the
    // XML to a Scala XML Node.
    val source = URISchemaSource(
      Paths.get(exampleOfMostDFDLConstructsXML).toFile,
      Misc.getRequiredResource(exampleOfMostDFDLConstructsXML)
    )
    val loader = new DaffodilXMLLoader(null)
    val testSchema = loader.load(source, None)

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    // No annotations
    val Seq(gct) = sd.globalComplexTypeDefs
    assertNotNull(gct)

    // Explore global element decl
    val Seq(e1d, e2d, e3d, _, _) = sd.globalElementDecls // there are 3 factories
    val e1 = e1d.asRoot
    val e2 = e2d.asRoot
    val e3 = e3d.asRoot
    assertEquals(
      ByteOrder.BigEndian.toString.toLowerCase(),
      e1d.formatAnnotation
        .asInstanceOf[DFDLElement]
        .getPropertyForUnitTest("byteOrder")
        .toLowerCase()
    )
    val Seq(_, a2) = e3d.annotationObjs // third one has two annotations
    assertTrue(a2.isInstanceOf[DFDLAssert]) // second annotation is newVariableInstance
    assertEquals(OccursCountKind.Implicit.toString, e3.getProperty("occursCountKind"))
    // Explore local complex type def
    val seq = e1.complexType.modelGroup.asInstanceOf[LocalSequence] // ... which is a sequence
    seq.formatAnnotation.asInstanceOf[DFDLSequence] // ...annotated with...
    assertEquals(YesNo.No, seq.initiatedContent) // initiatedContent="no"

    val Seq(e1a: DFDLElement) = e1d.annotationObjs
    assertEquals("UTF-8", e1a.getPropertyForUnitTest("encoding"))

    // Explore global simple type defs
    val Seq(st1, _, _, _) = sd.globalSimpleTypeDefs // there are two.
    val Seq(b1, b2, _, b4) = st1.annotationObjs // first one has 4 annotations
    assertEquals(
      AlignmentUnits.Bytes.toString.toLowerCase,
      b1.asInstanceOf[DFDLSimpleType].getPropertyForUnitTest("alignmentUnits")
    ) // first has alignmentUnits
    assertEquals(
      "tns:myVar1",
      b2.asInstanceOf[DFDLSetVariable].ref
    ) // second is setVariable with a ref
    assertEquals(
      "yadda yadda yadda",
      b4.asInstanceOf[DFDLAssert].messageAttrib.get
    ) // fourth is an assert with yadda message

    // Explore define formats
    val Seq(df1, _) = sd.defineFormats // there are two
    val def1 = df1.asInstanceOf[DFDLDefineFormat]
    assertEquals("def1", def1.name) // first is named "def1"
    assertEquals(
      Representation.Text.toString.toLowerCase,
      def1.formatAnnotation.getPropertyForUnitTest("representation")
    ) // has representation="text"

    // Explore define variables
    val Seq(dv1, _) = sd.defineVariables // there are two
    assertNotNull(dv1)
    // assertEquals("2003年08月27日", dv2.asInstanceOf[DFDLDefineVariable].defaultValue) // second has kanji chars in default value

    // Explore define escape schemes
    val Seq(desc1) = sd.defineEscapeSchemes // only one of these
    val es = desc1.forComponent(e1a).escapeScheme.escapeCharacterRaw.asInstanceOf[Found].value
    assertEquals(
      "%%",
      es
    ) // has escapeCharacter="%%" (note: string literals not digested yet, so %% is %%, not %.

    // Explore global group defs
    val Seq(_, root, _*) = sd.globalElementDecls
    val seq1 = root.complexType.modelGroup.asInstanceOf[SequenceGroupRef]

    val Seq(e1r: ElementRef, cgr: ChoiceGroupRef) = seq1.groupMembers

    val Seq(ggd1, ggd2, _, _, _) = sd.globalGroupDefs // there are two
    assertEquals("gr", ggd1.namedQName.local)
    assertEquals("hiddenGroup", ggd2.namedQName.local)

    // Explore LocalSimpleTypeDef
    val Seq(c1: LocalElementDecl, _: LocalElementDecl, c3: LocalElementDecl, rest @ _*) =
      cgr.groupMembers
    val ist =
      c3.asInstanceOf[LocalElementDecl].immediateType.get.asInstanceOf[LocalSimpleTypeDef]
    val istBase = ist.optRestriction.get.baseQName.toQNameString
    assertEquals("tns:aType", istBase)

    // Explore LocalElementDecl
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

  @Test def test4(): Unit = {
    val testSchema =
      XML.load(Misc.getRequiredResource(exampleOfMostDFDLConstructsXML).toURL)

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    // Explore global group defs
    val Seq(_, root, _*) = sd.globalElementDecls
    val seq1 = root.complexType.modelGroup.asInstanceOf[SequenceGroupRef]

    val Seq(_: ElementRef, cgr: ChoiceGroupRef) = seq1.groupMembers
    val cgd = cgr.groupDef
    val Seq(_, cd2: LocalElementDecl, rest @ _*) =
      cgd.groupMembersNotShared // Children nodes of Choice-node, there are 3

    // val Seq(a1: DFDLChoice) = ch1.annotationObjs // Obtain the annotation object that is a child
    // of the group node.

    assertEquals(AlignmentType.Implicit, cgr.alignment)
    assertEquals(ChoiceLengthKind.Implicit, cgr.choiceLengthKind)

    val Seq(asrt1: DFDLAssert) = cd2.annotationObjs // Obtain Annotation object that is child
    // of cd2.

    assertEquals("{ $myVar1 eq xs:int(xs:string(fn:round-half-to-even(8.5))) }", asrt1.testTxt)
  }

  @Test def test_named_format_chaining(): Unit = {
    val testSchema =
      XML.load(
        Misc
          .getRequiredResource(
            "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml"
          )
          .toURL
      )

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    val Seq(a1d, _, _, _, _, _) = sd.globalElementDecls // Obtain global element nodes
    val a1 = a1d.asRoot
    assertEquals(true, a1.verifyPropValue("occursCountKind", "parsed"))
    assertEquals(true, a1.verifyPropValue("lengthKind", "pattern"))
    assertEquals(true, a1.verifyPropValue("representation", "text"))
    assertEquals(true, a1.verifyPropValue("binaryNumberRep", "packed"))
  }

  @Test def test_simple_types_access_works(): Unit = {
    val testSchema =
      XML.load(
        Misc
          .getRequiredResource(
            "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml"
          )
          .toURL
      )

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    val Seq(_, x, _, _, _, _) = sd.globalElementDecls.map {
      _.asRoot
    } // Obtain global element nodes

    assertEquals(AlignmentUnits.Bytes, x.alignmentUnits)
  }

  @Test def test_simple_types_property_combining(): Unit = {
    val testSchema =
      XML.load(
        Misc
          .getRequiredResource(
            "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml"
          )
          .toURL
      )

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    val Seq(_, ge2, ge3, ge4, ge5, ge6) = sd.globalElementDecls.map {
      _.asRoot
    } // Obtain global element nodes

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

  @Test def test_simpleType_base_combining(): Unit = {
    val testSchema =
      XML.load(Misc.getRequiredResource(exampleOfMostDFDLConstructsXML).toURL)

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val root = sset.root
    val sd = sch.schemaDocuments.head

    // No annotations
    val Seq(gct) = sd.globalComplexTypeDefs
    assertNotNull(gct)

    // Explore global element decl
    val Seq(e1f, e2f, e3f, _, _) = sd.globalElementDecls // there are 3 factories
    val e1 = e1f.asRoot
    e2f.asRoot
    e3f.asRoot

    val Seq(gs1f, _, gs3f, _) = sd.globalSimpleTypeDefs

    val gs1 = gs1f // Global Simple Type - aType

    val baseQName = gs1.optRestriction.get.baseQName

    assertEquals("ex", baseQName.prefix.get)
    assertEquals("aaType", baseQName.local)

    assertTrue(
      gs1.formatAnnotation.verifyPropValue("alignmentUnits", "bytes")
    ) // SimpleType - Local

    assertTrue(e1.verifyPropValue("byteOrder", "bigEndian")) // SimpleType - Base
    assertTrue(e1.verifyPropValue("occursCountKind", "implicit")) // Default Format
    assertTrue(e1.verifyPropValue("representation", "text")) // Define Format - def1
    assertTrue(e1.verifyPropValue("encoding", "UTF-8")) // Define Format - def1
    assertTrue(e1.verifyPropValue("textStandardBase", "10")) // Define Format - def2
    assertTrue(
      e1.verifyPropValue("escapeSchemeRef", "tns:quotingScheme")
    ) // Define Format - def2

    val gs3 = gs3f // Global SimpleType - aTypeError - overlapping base props

    // Tests overlapping properties
    // because these unit tests are outside the normal framework,
    // we sometimes have to demand things in order for errors to be noticed.
    assertTrue(root.isError)
    val msgs = root.getDiagnostics.mkString("\n").toLowerCase
    assertTrue(msgs.contains("overlap"))
    assertTrue(msgs.contains("initiator".toLowerCase))
  }

  @Test def test_group_references(): Unit = {
    val testSchema =
      XML.load(Misc.getRequiredResource(exampleOfMostDFDLConstructsXML).toURL)

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    // No annotations
    val Seq(g) = sd.globalComplexTypeDefs;
    assertNotNull(g)

    // Explore global element decl
    val Seq(_, _, _, e4f, e5f) = sd.globalElementDecls // there are 3 factories

    // GroupRefTest
    val e4 = e4f.asRoot // groupRefTest

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

    val e5 = e5f.asRoot // groupRefTestOverlap

    val e5ct = e5.complexType

    val e5ctgref = e5ct.modelGroup.asInstanceOf[SequenceGroupRef]
    val myGlobal3 = e5ctgref.groupDef
    // val myGlobal3Seq =
    myGlobal3.asInstanceOf[GlobalSequenceGroupDef]

    // Tests overlapping properties
    assertTrue(e5.isError)

    val msg = e5.getDiagnostics.mkString("\n").toLowerCase
    val res = msg.contains("overlap")
    if (!res) println(msg)
    assertTrue(res)

  }

  @Test def test_ibm_7132(): Unit = {
    val ibm7132Schema =
      XML.load(Misc.getRequiredResource("/test/TestRefChainingIBM7132.dfdl.xml").toURL)
    // val ibm7132Schema = "test/TestRefChainingIBM7132.dfdl.xml"
    val sset = SchemaSet(ibm7132Schema)
    // val Seq(sch) = sset.schemas
    val sd = sset.allSchemaDocuments.head

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.asRoot

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

  @Test def testDfdlRef(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:defineFormat name="ref1"> <dfdl:format initiator=":"/> </dfdl:defineFormat>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:ref="tns:ref1" type="xs:string">
      </xs:element>
    )
    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.asRoot
    ge1.formatAnnotation.properties

  }

  @Test def testGetQName() = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:defineFormat name="ref1">
        <dfdl:format initiator=":" alignmentUnits="bits"/>
      </dfdl:defineFormat>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:ref="tns:ref1" type="xs:string">
      </xs:element>
    )
    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.asRoot.referencedElement

    val qn = ge1.formatAnnotation.resolveQName("tns:ref1")
    val nsURI = qn.namespace
    val localName = qn.local

    assertEquals("ref1", localName)
    assertEquals(XMLUtils.EXAMPLE_NAMESPACE, nsURI)
  }

  @Test def testGetAllNamespaces(): Unit = {
    val xml = <bar xmlns:foo="fooNS" xmlns:bar="barNS">
                <quux xmlns:baz="bazNS" attr1="x"/>
              </bar>

    val scope = (xml \ "quux")(0).scope
    scala.xml.Elem("dfdl", "element", scala.xml.Null, scope, true)
  }

  @Test def test_escapeSchemeOverride() = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" separator="" initiator="" terminator="" emptyValueDelimiterPolicy="none" textNumberRep="standard" representation="text" occursStopValue="-1" occursCountKind="expression" escapeSchemeRef="pound"/>
      <dfdl:defineEscapeScheme name="pound">
        <dfdl:escapeScheme escapeCharacter='#' escapeKind="escapeCharacter"/>
      </dfdl:defineEscapeScheme>
      <dfdl:defineEscapeScheme name='cStyleComment'>
        <dfdl:escapeScheme escapeBlockStart='/*' escapeBlockEnd='*/' escapeKind="escapeBlock"/>
      </dfdl:defineEscapeScheme>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered">
            <xs:element name="character" type="xsd:string" maxOccurs="unbounded" dfdl:representation="text" dfdl:separator="," dfdl:terminator="%NL;"/>
            <xs:element name="block" type="xsd:string" maxOccurs="unbounded" dfdl:representation="text" dfdl:separator="," dfdl:terminator="%NL;" dfdl:escapeSchemeRef="cStyleComment"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.asRoot.referencedElement

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

  @Test def test_element_references(): Unit = {
    val testSchema =
      XML.load(Misc.getRequiredResource(exampleOfMostDFDLConstructsXML).toURL)

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    val Seq(_, gedf, _*) = sd.globalElementDecls
    val root = gedf.asRoot
    val sgr = root.complexType.modelGroup.asInstanceOf[SequenceGroupRef]

    val Seq(e1r: ElementRef, cgr: ChoiceGroupRef) = sgr.groupMembers

    assertEquals("hiddenGroup", cgr.groupDef.namedQName.local)
    assertEquals(AlignmentType.Implicit, cgr.alignment)

    assertEquals(2, e1r.maxOccurs)
    assertEquals(1, e1r.minOccurs)
    assertEquals(AlignmentUnits.Bytes, e1r.alignmentUnits)
    // assertEquals(true, e1.nillable) // TODO: e1.nillable doesn't exist?
    // assertEquals("%ES; %% %#0; %NUL;%ACK; foo%#rF2;%#rF7;bar %WSP*; %#2024;%#xAABB; &amp;&#2023;&#xCCDD; -1", e1.nilValue) // TODO: Do not equal each other!
    assertEquals(NilKind.LiteralValue, e1r.nilKind)
  }

  @Test def testPathWithIndexes(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:complexType>
    )
    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.asRoot.referencedElement

    val seq = ge1.sequence

    val Seq(_, _, s3) = seq.groupMembers
    val s3s = s3.asInstanceOf[LocalSequence]
    val Seq(es) = s3s.groupMembers
    val ese = es.asInstanceOf[LocalElementDecl]
    assertTrue(ese.path.contains("sequence[3]"))
  }

  val dummyGroupRef = null // just because otherwise we have to construct too many things.

  def FindValue(collection: Map[String, String], key: String, value: String): Boolean = {
    val found: Boolean = Option(collection.find(x => x._1 == key && x._2 == value)) match {
      case Some(_) => true
      case None => false
    }
    found
  }

  @Test def testInitiator(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:string" dfdl:initiator="*" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
    )
    //    val actual = TestUtils.testString(testSchema, "*word")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">word</data>"))

    val infoset = <data xmlns={example}>word</data>
    TestUtils.testUnparsing(testSchema, infoset, "*word")
  }

  @Test def testTerminator(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:string" dfdl:terminator="!" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>
    )
    //    val actual = TestUtils.testString(testSchema, "37!")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">37</data>"))

    val infoset = <data xmlns={example}>37</data>
    TestUtils.testUnparsing(testSchema, infoset, "37!")
  }

  @Test def testDelims(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:string" dfdl:initiator="*" dfdl:terminator="! $" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>
    )
    //    val actual = TestUtils.testString(testSchema, "*37$")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">37</data>"))

    val infoset = <data xmlns={example}>37</data>
    TestUtils.testUnparsing(testSchema, infoset, "*37!")
  }

  //  @Test def testUnparseMultiElem1() {
  //    val testSchema = SchemaUtils.dfdlTestSchema(
  //      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
  //      <dfdl:format ref="tns:GeneralFormat"/>,
  //
  //      <xs:element name="list" type="tns:example1">
  //        <xs:annotation>
  //          <xs:appinfo source={ dfdl }>
  //            <dfdl:element alignmentUnits="bytes"/>
  //          </xs:appinfo>
  //        </xs:annotation>
  //      </xs:element>
  //      <xs:complexType name="example1">
  //        <xs:sequence>
  //          <xs:element name="somedata" type="xs:float" dfdl:length="6" dfdl:lengthKind="explicit"/>
  //          <xs:element name="moredata" type="xs:int" dfdl:length="2" dfdl:lengthKind="explicit"/>
  //        </xs:sequence>
  //      </xs:complexType>)
  //    //    val actual = TestUtils.testString(testSchema, "943.2801")
  //    //    val actualString = actual.result.toString
  //    //    assertTrue(actualString.startsWith("<list"))
  //    //    assertTrue(actualString.endsWith("><somedata>943.28</somedata><moredata>1</moredata></list>"))
  //
  //    val infoset = <list xmlns={ example }><somedata>943.28</somedata><moredata>1</moredata></list>
  //    // TODO: unparse needs to restore leading zeros removed in parse?
  //    //testUnparsing(testSchema, infoset, "943.2801")
  //    TestUtils.testUnparsing(testSchema, infoset, "943.281")
  //  }

  @Test def testUnparseMultiElem2(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={dfdl}>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator="^">
          <xs:element name="somedata" type="xs:double" dfdl:length="5" dfdl:lengthKind="explicit"/>
          <xs:element name="moredata" type="xs:string" dfdl:length="3" dfdl:lengthKind="explicit" dfdl:initiator="%%"/>
          <xs:element name="anddata" type="xs:int" dfdl:length="2" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>
    )
    //    val actual = TestUtils.testString(testSchema, "50.93^%XYZ^42")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><somedata>50.93</somedata><moredata>XYZ</moredata><anddata>42</anddata></list>"))

    val infoset = <list xmlns={
      example
    }><somedata>50.93</somedata><moredata>XYZ</moredata><anddata>42</anddata></list>
    TestUtils.testUnparsing(testSchema, infoset, "50.93^%XYZ^42")
  }

  //  @Test def testUnparseNested() {
  //    val testSchema = SchemaUtils.dfdlTestSchema(
  //      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
  //      <dfdl:format ref="tns:GeneralFormat"/>,
  //
  //      <xs:element name="list" type="tns:example1">
  //        <xs:annotation>
  //          <xs:appinfo source={ dfdl }>
  //            <dfdl:element alignmentUnits="bytes"/>
  //          </xs:appinfo>
  //        </xs:annotation>
  //      </xs:element>
  //      <xs:complexType name="example1">
  //        <xs:sequence>
  //          <xs:element name="somedata" type="tns:example2">
  //            <xs:annotation>
  //              <xs:appinfo source={ dfdl }>
  //                <dfdl:element alignmentUnits="bytes"/>
  //              </xs:appinfo>
  //            </xs:annotation>
  //          </xs:element>
  //        </xs:sequence>
  //      </xs:complexType>
  //      <xs:complexType name="example2">
  //        <xs:sequence>
  //          <xs:element name="moredata" type="xs:double" dfdl:length="7" dfdl:lengthKind="explicit"/>
  //          <xs:element name="anddata" type="xs:string" dfdl:length="6" dfdl:lengthKind="explicit"/>
  //        </xs:sequence>
  //      </xs:complexType>)
  //    //    val actual = TestUtils.testString(testSchema, "11235.8qwerty")
  //    //    val actualString = actual.result.toString
  //    //    assertTrue(actualString.startsWith("<list"))
  //    //    assertTrue(actualString.endsWith("><somedata><moredata>11235.8</moredata><anddata>qwerty</anddata></somedata></list>"))
  //
  //    val infoset = <list xmlns={ example }><somedata><moredata>11235.8</moredata><anddata>qwerty</anddata></somedata></list>
  //    TestUtils.testUnparsing(testSchema, infoset, "11235.8qwerty")
  //  }

  //  //  @Test def testUnparseNestedChildren() {
  //    val testSchema = SchemaUtils.dfdlTestSchema(
  //      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
  //      <dfdl:format ref="tns:GeneralFormat"/>,
  //
  //      <xs:element name="list" type="tns:example1">
  //        <xs:annotation>
  //          <xs:appinfo source={ dfdl }>
  //            <dfdl:element alignmentUnits="bytes"/>
  //          </xs:appinfo>
  //        </xs:annotation>
  //      </xs:element>
  //      <xs:complexType name="example1">
  //        <xs:sequence>
  //          <xs:element name="data" type="tns:example2">
  //            <xs:annotation>
  //              <xs:appinfo source={ dfdl }>
  //                <dfdl:element alignmentUnits="bytes"/>
  //              </xs:appinfo>
  //            </xs:annotation>
  //          </xs:element>
  //        </xs:sequence>
  //      </xs:complexType>
  //      <xs:complexType name="example2">
  //        <xs:sequence>
  //          <xs:element name="somedata" type="xs:string" dfdl:length="3" dfdl:lengthKind="explicit"/>
  //          <xs:element name="moredata" type="xs:int" dfdl:length="8" dfdl:lengthKind="explicit"/>
  //        </xs:sequence>
  //      </xs:complexType>)
  //    //    val actual = TestUtils.testString(testSchema, "abc87654321")
  //    //    val actualString = actual.result.toString
  //    //    assertTrue(actualString.startsWith("<list"))
  //    //    assertTrue(actualString.endsWith("><data><somedata>abc</somedata><moredata>87654321</moredata></data></list>"))
  //
  //    val infoset = <list xmlns={ example }><data><somedata>abc</somedata><moredata>87654321</moredata></data></list>
  //    TestUtils.testUnparsing(testSchema, infoset, "abc87654321")
  //  }

  //  @Test def testUnparseDelimited() {
  //    val testSchema = SchemaUtils.dfdlTestSchema(
  //      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
  //      <dfdl:format ref="tns:GeneralFormat"/>,
  //
  //      <xs:element name="list" type="tns:example1">
  //        <xs:annotation>
  //          <xs:appinfo source={ dfdl }>
  //            <dfdl:element alignmentUnits="bytes"/>
  //          </xs:appinfo>
  //        </xs:annotation>
  //      </xs:element>
  //      <xs:complexType name="example1">
  //        <xs:sequence dfdl:separator=",">
  //          <xs:element name="a" type="xs:int" dfdl:lengthKind="delimited"/>
  //          <xs:element name="b" type="xs:double" dfdl:lengthKind="delimited"/>
  //          <xs:element name="c" type="xs:string" dfdl:lengthKind="delimited"/>
  //          <xs:element name="d" type="xs:int" dfdl:lengthKind="delimited"/>
  //        </xs:sequence>
  //      </xs:complexType>)
  //    //        val actual = TestUtils.testString(testSchema, "246813579,90.3761,hello,100")
  //    //        val actualString = actual.result.toString
  //    //        assertTrue(actualString.startsWith("<list"))
  //    //        assertTrue(actualString.endsWith("><a>246813579</a><b>90.3761</b><c>hello</c><d>100</d></list>"))
  //
  //    val infoset = <list xmlns={ example }><a>246813579</a><b>90.3761</b><c>hello</c><d>100</d></list>
  //    TestUtils.testUnparsing(testSchema, infoset, "246813579,90.3761,hello,100")
  //  }

  //  @Test def testUnparseAlignmentBits() {
  //    val testSchema = SchemaUtils.dfdlTestSchema(
  //      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
  //      <dfdl:format ref="tns:GeneralFormat"/>,
  //
  //      <xs:element name="list" type="tns:example1">
  //        <xs:annotation>
  //          <xs:appinfo source={ dfdl }>
  //            <dfdl:element alignmentUnits="bits"/>
  //          </xs:appinfo>
  //        </xs:annotation>
  //      </xs:element>
  //      <xs:complexType name="example1">
  //        <xs:sequence dfdl:separator=",">
  //          <xs:element name="a" type="xs:int" dfdl:lengthKind="delimited"/>
  //          <xs:element name="b" type="xs:double" dfdl:lengthKind="delimited"/>
  //          <xs:element name="c" type="xs:string" dfdl:length="3" dfdl:lengthKind="explicit"/>
  //          <xs:element name="d" type="xs:int" dfdl:length="8" dfdl:lengthKind="explicit"/>
  //        </xs:sequence>
  //      </xs:complexType>)
  //
  //    val infoset = <list xmlns={ example }><a>246813579</a><b>90.3761</b><c>abc</c><d>10034567</d></list>
  //    TestUtils.testUnparsing(testSchema, infoset, "246813579,90.3761,abc,10034567")
  //  }

  //  @Test def testUnparseChoice1() {
  //    val testSchema = SchemaUtils.dfdlTestSchema(
  //      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
  //      <dfdl:format ref="tns:GeneralFormat"/>,
  //
  //      <xs:element name="list" type="tns:example1">
  //        <xs:annotation>
  //          <xs:appinfo source={ dfdl }>
  //            <dfdl:element alignmentUnits="bits"/>
  //          </xs:appinfo>
  //        </xs:annotation>
  //      </xs:element>
  //      <xs:complexType name="example1">
  //        <xs:sequence dfdl:separator=",">
  //          <xs:element name="a" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
  //          <xs:element name="b" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="4"/>
  //          <xs:element name="choice">
  //            <xs:complexType>
  //              <xs:choice dfdl:choiceLengthKind="implicit">
  //                <xs:element name="c" type="xs:int" dfdl:initiator="choice1:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
  //                <xs:element name="d" type="xs:double" dfdl:initiator="choice2:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
  //              </xs:choice>
  //            </xs:complexType>
  //          </xs:element>
  //        </xs:sequence>
  //      </xs:complexType>)
  //    //        val actual = TestUtils.testString(testSchema, "567,word,choice1:203867")
  //    //        val actualString = actual.result.toString
  //    //        assertTrue(actualString.startsWith("<list"))
  //    //        assertTrue(actualString.endsWith("><a>567</a><b>word</b><choice><c>203867</c></choice></list>"))
  //
  //    val infoset = <list xmlns={ example }><a>567</a><b>word</b><choice><c>203867</c></choice></list>
  //    TestUtils.testUnparsing(testSchema, infoset, "567,word,choice1:203867")
  //  }

  //  @Test def testUnparseChoice2() {
  //    val testSchema = SchemaUtils.dfdlTestSchema(
  //      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
  //      <dfdl:format ref="tns:GeneralFormat"/>,
  //
  //      <xs:element name="list" type="tns:example1">
  //        <xs:annotation>
  //          <xs:appinfo source={ dfdl }>
  //            <dfdl:element alignmentUnits="bits"/>
  //          </xs:appinfo>
  //        </xs:annotation>
  //      </xs:element>
  //      <xs:complexType name="example1">
  //        <xs:sequence dfdl:separator=",">
  //          <xs:element name="a" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
  //          <xs:element name="b" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="4"/>
  //          <xs:element name="choice">
  //            <xs:complexType>
  //              <xs:choice dfdl:choiceLengthKind="implicit">
  //                <xs:element name="c" type="xs:int" dfdl:initiator="choice1:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
  //                <xs:element name="d" type="xs:double" dfdl:initiator="choice2:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
  //              </xs:choice>
  //            </xs:complexType>
  //          </xs:element>
  //        </xs:sequence>
  //      </xs:complexType>)
  //    //        val actual = TestUtils.testString(testSchema, "567,word,choice2:2038.67")
  //    //        val actualString = actual.result.toString
  //    //        assertTrue(actualString.startsWith("<list"))
  //    //        assertTrue(actualString.endsWith("><a>567</a><b>word</b><choice><d>2038.67</d></choice></list>"))
  //
  //    val infoset = <list xmlns={ example }><a>567</a><b>word</b><choice><d>2038.67</d></choice></list>
  //    TestUtils.testUnparsing(testSchema, infoset, "567,word,choice2:2038.67")
  //  }

  @Test def testUnparseBinaryIntBE(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:int" dfdl:representation="binary"/>
    )
    //        val actual = TestUtils.testBinary(testSchema, "0000000F")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<data"))
    //        assertTrue(actualString.endsWith(">15</data>"))

    val infoset = <data xmlns={example}>15</data>
    val bytes = List[Byte](0, 0, 0, 15).toArray
    TestUtils.testUnparsingBinary(testSchema, infoset, bytes)
  }

  @Test def testUnparseBinaryIntLE(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:int" dfdl:representation="binary" dfdl:byteOrder='littleEndian'/>
    )
    //        val actual = TestUtils.testBinary(testSchema, "0F000000")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<data"))
    //        assertTrue(actualString.endsWith(">15</data>"))

    val infoset = <data xmlns={example}>15</data>
    val bytes = List[Byte](15, 0, 0, 0).toArray
    TestUtils.testUnparsingBinary(testSchema, infoset, bytes)
  }

  //  @Test def testUnparseBinary1() {
  //    val testSchema = SchemaUtils.dfdlTestSchema(
  //      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
  //      <dfdl:format ref="tns:GeneralFormat"/>,
  //
  //      <xs:element name="list" type="tns:example1">
  //        <xs:annotation>
  //          <xs:appinfo source={ dfdl }>
  //            <dfdl:element alignmentUnits="bytes"/>
  //          </xs:appinfo>
  //        </xs:annotation>
  //      </xs:element>
  //      <xs:complexType name="example1">
  //        <xs:sequence dfdl:separator="">
  //          <xs:element name="byte" type="xs:byte" dfdl:length="2" dfdl:lengthKind="explicit" dfdl:representation="binary"/>
  //          <xs:element name="short" type="xs:short" dfdl:length="4" dfdl:lengthKind="explicit" dfdl:representation="binary"/>
  //          <xs:element name="long" type="xs:long" dfdl:length="4" dfdl:lengthKind="explicit" dfdl:representation="binary"/>
  //        </xs:sequence>
  //      </xs:complexType>)
  //
  //    val infoset = <list xmlns={ example }><byte>31</byte><short>-112</short><long>1030</long></list>
  //    val bytes = List[Byte](31, -1, -112, 0, 0, 0, 0, 0, 0, 4, 6).toArray
  //    TestUtils.testUnparsingBinary(testSchema, infoset, bytes)
  //  }

  @Test def testHasPatternFacets(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" type="tns:st1" dfdl:lengthKind="explicit" dfdl:length="1"/>
      <xs:simpleType name="st1">
        <xs:restriction base="xs:string">
          <xs:pattern value="1"/>
          <xs:pattern value="2"/>
          <xs:pattern value="3"/>
        </xs:restriction>
      </xs:simpleType>
    )

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.asRoot

    assertEquals(1, decl.patternValues.length)
    val (_, pattern) = decl.patternValues(0)
    assertEquals("1|2|3", pattern.toString())
  }

  @Test def testPatternFacetsInheritance(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" type="tns:st1" dfdl:lengthKind="explicit" dfdl:length="1"/>
      <xs:simpleType name="st1">
        <xs:restriction base="tns:st2">
          <xs:pattern value="1"/>
          <xs:pattern value="2"/>
          <xs:pattern value="3"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:simpleType name="st2">
        <xs:restriction base="tns:st3">
          <xs:pattern value="4"/>
          <xs:pattern value="5"/>
          <xs:pattern value="6"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:simpleType name="st3">
        <xs:restriction base="xs:string">
          <xs:pattern value="7"/>
          <xs:pattern value="8"/>
          <xs:pattern value="9"/>
        </xs:restriction>
      </xs:simpleType>
    )

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.asRoot

    assertEquals(3, decl.patternValues.length)
    val (_, st1) = decl.patternValues(0)
    val (_, st2) = decl.patternValues(1)
    val (_, st3) = decl.patternValues(2)

    assertEquals("1|2|3", st1.toString())
    assertEquals("4|5|6", st2.toString())
    assertEquals("7|8|9", st3.toString())
  }

}
