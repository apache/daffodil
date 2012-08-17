package daffodil.dsom

import daffodil.xml.XMLUtils
import daffodil.util._
import scala.xml._
import daffodil.compiler._
import org.scalatest.junit.JUnit3Suite
import daffodil.schema.annotation.props.gen._
import daffodil.schema.annotation.props._
import daffodil.util.Misc
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import java.io.FileOutputStream
import java.nio.channels.WritableByteChannel
import java.io.FileWriter
import java.io.File
import java.nio.ByteBuffer

class TestDsomCompiler extends JUnit3Suite with Logging {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val dummyGroupRef = null // just because otherwise we have to construct too many things.

  def FindValue(collection: Map[String, String], key: String, value: String): Boolean = {
    val found: Boolean = Option(collection.find(x => x._1 == key && x._2 == value)) match {
      case Some(_) => true
      case None => false
    }
    found
  }

  // @Test
  def testHasProps() {
    val testSchema = TestUtils.dfdlTestSchema(
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
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()

    val df = schemaDoc.defaultFormat
    val tnr = df.textNumberRep
    assertEquals(TextNumberRep.Standard, tnr)
    val tnr2 = decl.textNumberRep
    assertEquals(TextNumberRep.Standard, tnr2)
  }

  // @Test
  def testSchemaValidationSubset() {
    val sch: Node = TestUtils.dfdlTestSchema(
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
    val diagnostics = sset.getDiagnostics
    val msgs = diagnostics.map { _.getMessage }
    val msg = msgs.mkString("\n")
    val hasErrorText = msg.contains("maxOccurs");
    if (!hasErrorText) this.fail("Didn't get expected error. Got: " + msg)
  }

  // @Test
  def testTypeReferentialError() {
    val sch: Node = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="typeDoesNotExist"/>)
    val (sset, _) = Compiler().frontEnd(sch)
    assertTrue(sset.isError)
    val msg = sset.getDiagnostics.toString
    val hasErrorText = msg.contains("typeDoesNotExist");
    if (!hasErrorText) this.fail("Didn't get expected error. Got: " + msg)
  }

  // @Test
  def testSchemaValidationPropertyChecking() {
    val s: Node = TestUtils.dfdlTestSchema(
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
    if (!hasErrorText) this.fail("Didn't get expected error. Got: " + msg)
  }

  def test2() {
    val sc = TestUtils.dfdlTestSchema(
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
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(declFactory) = schemaDoc.globalElementDecls
    val decl = declFactory.forRoot()
    val Seq(ct) = schemaDoc.globalComplexTypeDefs
    assertEquals("example1", ct.name)

    val fa = decl.formatAnnotation.asInstanceOf[DFDLElement]
    assertEquals(AlignmentUnits.Bytes, fa.alignmentUnits)
    //    fa.alignmentUnits match {
    //      case AlignmentUnits.Bits => println("was bits")
    //      case AlignmentUnits.Bytes => println("was bytes")
    //    }
  }

  /* def testXsomMultifile(){
   
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

  def testSequence1() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separatorPolicy="required" dfdl:separator="">
          <xs:element name="w" type="xs:int" maxOccurs="1" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:occursCountKind="fixed"/>
        </xs:sequence>
      </xs:complexType>)

    val w = Utility.trim(testSchema)

    val (sset, _) = Compiler().frontEnd(w)
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
  def testInitiator() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:string" dfdl:initiator="*" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    //    val actual = Compiler.testString(testSchema, "*word")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">word</data>"))

    val infoset = <data xmlns={ example }>word</data>
    Compiler.testUnparsing(testSchema, infoset, "*word")
  }

  // @Test
  def testTerminator() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:string" dfdl:terminator="!" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    //    val actual = Compiler.testString(testSchema, "37!")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">37</data>"))

    val infoset = <data xmlns={ example }>37</data>
    Compiler.testUnparsing(testSchema, infoset, "37!")
  }

  // @Test
  def testDelims() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:string" dfdl:initiator="*" dfdl:terminator="! $" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    //    val actual = Compiler.testString(testSchema, "*37$")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<data"))
    //    assertTrue(actualString.endsWith(">37</data>"))

    val infoset = <data xmlns={ example }>37</data>
    Compiler.testUnparsing(testSchema, infoset, "*37!")
  }

  // @Test
  def testUnparseBinaryIntBE() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:int" dfdl:representation="binary"/>)
//        val actual = Compiler.testBinary(testSchema, "0000000F")
//        val actualString = actual.result.toString
//        assertTrue(actualString.startsWith("<data"))
//        assertTrue(actualString.endsWith(">15</data>"))

    val infoset = <data xmlns={ example }>15</data>
    val bytes = Array[Byte](0, 0, 0, 15)
    Compiler.testUnparsingBinary(testSchema, infoset, bytes)
  }

  // @Tests
  def testUnparseBinaryIntLE() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:int" dfdl:representation="binary" dfdl:byteOrder='littleEndian'/>)
//        val actual = Compiler.testBinary(testSchema, "0F000000")
//        val actualString = actual.result.toString
//        assertTrue(actualString.startsWith("<data"))
//        assertTrue(actualString.endsWith(">15</data>"))

    val infoset = <data xmlns={ example }>15</data>
    val bytes = Array[Byte](15, 0, 0, 0)
    Compiler.testUnparsingBinary(testSchema, infoset, bytes)
  }

  // @Test
  def testUnparseMultiElem1() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="somedata" type="xs:float" dfdl:length="6" dfdl:lengthKind="explicit"/>
          <xs:element name="moredata" type="xs:int" dfdl:length="2" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)
    //    val actual = Compiler.testString(testSchema, "943.2801")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><somedata>943.28</somedata><moredata>1</moredata></list>"))

    val infoset = <list xmlns={ example }><somedata>943.28</somedata><moredata>1</moredata></list>
    // TODO: unparse needs to restore leading zeros removed in parse?
    //testUnparsing(testSchema, infoset, "943.2801")
    Compiler.testUnparsing(testSchema, infoset, "943.281")
  }

  // @Test
  def testUnparseMultiElem2() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator="^">
          <xs:element name="somedata" type="xs:double" dfdl:length="5" dfdl:lengthKind="explicit"/>
          <xs:element name="moredata" type="xs:string" dfdl:length="3" dfdl:lengthKind="explicit" dfdl:initiator="%"/>
          <xs:element name="anddata" type="xs:int" dfdl:length="2" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)
    //    val actual = Compiler.testString(testSchema, "50.93^%XYZ^42")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><somedata>50.93</somedata><moredata>XYZ</moredata><anddata>42</anddata></list>"))

    val infoset = <list xmlns={ example }><somedata>50.93</somedata><moredata>XYZ</moredata><anddata>42</anddata></list>
    Compiler.testUnparsing(testSchema, infoset, "50.93^%XYZ^42")
  }

  // @Test
//  def testBMP() {
//    val infoset = scala.xml.XML.loadFile("header.xml")
//    val compiler = Compiler()
//    val pf = compiler.compile("BMPtest.xsd") //pass compiler the schema
//    val unparser = pf.onPath("/") //create unparser for given schema
//    val outputStream = new FileOutputStream("testBMP")
//    val out = java.nio.channels.Channels.newChannel(outputStream)
//    unparser.unparse(out, infoset) //pass infoset to unparser
//    out.close()
//  }

  // @Test
//  def testBMPPixels() {
//    val infoset = scala.xml.XML.loadFile("pixeldata.xml")
//    val compiler = Compiler()
//    val pf = compiler.compile("BMPtestPixels.xsd") //pass compiler the schema
//    val unparser = pf.onPath("/") //create unparser for given schema
//    val outputStream = new FileOutputStream("testBMPPixels")
//    val out = java.nio.channels.Channels.newChannel(outputStream)
//    unparser.unparse(out, infoset) //pass infoset to unparser
//    out.close()
//  }
  
  // @Test
  def testUnparseBinary1() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator="">
          <xs:element name="byte" type="xs:byte" dfdl:length="2" dfdl:lengthKind="explicit" dfdl:representation="binary"/>
          <xs:element name="short" type="xs:short" dfdl:length="4" dfdl:lengthKind="explicit" dfdl:representation="binary"/>
          <xs:element name="long" type="xs:long" dfdl:length="4" dfdl:lengthKind="explicit" dfdl:representation="binary"/>
        </xs:sequence>
      </xs:complexType>)

    val infoset = <list xmlns={ example }><byte>31</byte><short>-112</short><long>1030</long></list>
    val bytes = Array[Byte](31, -1, -112, 0, 0, 0, 0, 0, 0, 4, 6)
    Compiler.testUnparsingBinary(testSchema, infoset, bytes)
  }

  // @Test
  def testUnparseNested() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="somedata" type="tns:example2">
            <xs:annotation>
              <xs:appinfo source={ dfdl }>
                <dfdl:element alignmentUnits="bytes"/>
              </xs:appinfo>
            </xs:annotation>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
      <xs:complexType name="example2">
        <xs:sequence>
          <xs:element name="moredata" type="xs:double" dfdl:length="7" dfdl:lengthKind="explicit"/>
          <xs:element name="anddata" type="xs:string" dfdl:length="6" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)
    //    val actual = Compiler.testString(testSchema, "11235.8qwerty")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><somedata><moredata>11235.8</moredata><anddata>qwerty</anddata></somedata></list>"))

    val infoset = <list xmlns={ example }><somedata><moredata>11235.8</moredata><anddata>qwerty</anddata></somedata></list>
    Compiler.testUnparsing(testSchema, infoset, "11235.8qwerty")
  }

  // @Test
  def testUnparseNestedChildren() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="data" type="tns:example2">
            <xs:annotation>
              <xs:appinfo source={ dfdl }>
                <dfdl:element alignmentUnits="bytes"/>
              </xs:appinfo>
            </xs:annotation>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
      <xs:complexType name="example2">
        <xs:sequence>
          <xs:element name="somedata" type="xs:string" dfdl:length="3" dfdl:lengthKind="explicit"/>
          <xs:element name="moredata" type="xs:int" dfdl:length="8" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)
    //    val actual = Compiler.testString(testSchema, "abc87654321")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<list"))
    //    assertTrue(actualString.endsWith("><data><somedata>abc</somedata><moredata>87654321</moredata></data></list>"))

    val infoset = <list xmlns={ example }><data><somedata>abc</somedata><moredata>87654321</moredata></data></list>
    Compiler.testUnparsing(testSchema, infoset, "abc87654321")
  }

  // @Test
  def testUnparseFixedArray() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="root">
        <xs:complexType>
          <xs:sequence dfdl:separator="#">
            <xs:element name="data" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="{ 5 }" dfdl:initiator="?" maxOccurs="3" minOccurs="3" dfdl:occursCountKind="fixed"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    //    val actual = Compiler.testString(testSchema, "?45.670#?45.670#?45.670")
    //    val actualString = actual.result.toString
    //    assertTrue(actualString.startsWith("<root"))
    //    assertTrue(actualString.endsWith("><data>45.67</data><data>45.67</data><data>45.67</data></root>"))

    // TODO: restore trailing 0's in unparse?
    // Compiler.testUnparsing(testSchema, actual.result, "?45.670#?45.670#?45.670")
    val infoset = <root xmlns={ example }><data>45.67</data><data>45.67</data><data>45.67</data></root>
    Compiler.testUnparsing(testSchema, infoset, "?45.67#?45.67#?45.67")
  }

  // @Test
  def testUnparseDelimited() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator=",">
          <xs:element name="a" type="xs:int" dfdl:lengthKind="delimited"/>
          <xs:element name="b" type="xs:double" dfdl:lengthKind="delimited"/>
          <xs:element name="c" type="xs:string" dfdl:lengthKind="delimited"/>
          <xs:element name="d" type="xs:int" dfdl:lengthKind="delimited"/>
        </xs:sequence>
      </xs:complexType>)
    //        val actual = Compiler.testString(testSchema, "246813579,90.3761,hello,100")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<list"))
    //        assertTrue(actualString.endsWith("><a>246813579</a><b>90.3761</b><c>hello</c><d>100</d></list>"))

    val infoset = <list xmlns={ example }><a>246813579</a><b>90.3761</b><c>hello</c><d>100</d></list>
    Compiler.testUnparsing(testSchema, infoset, "246813579,90.3761,hello,100")
  }

  // @Test
  def testUnparseAlignmentBits() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bits"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator=",">
          <xs:element name="a" type="xs:int" dfdl:lengthKind="delimited"/>
          <xs:element name="b" type="xs:double" dfdl:lengthKind="delimited"/>
          <xs:element name="c" type="xs:string" dfdl:length="3" dfdl:lengthKind="explicit"/>
          <xs:element name="d" type="xs:int" dfdl:length="8" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)

    val infoset = <list xmlns={ example }><a>246813579</a><b>90.3761</b><c>abc</c><d>10034567</d></list>
    Compiler.testUnparsing(testSchema, infoset, "246813579,90.3761,abc,10034567")
  }

  // @Test
  def testUnparseChoice1() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bits"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator=",">
          <xs:element name="a" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          <xs:element name="b" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="4"/>
          <xs:element name="choice">
            <xs:complexType>
              <xs:choice dfdl:choiceLengthKind="implicit">
                <xs:element name="c" type="xs:int" dfdl:initiator="choice1:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
                <xs:element name="d" type="xs:double" dfdl:initiator="choice2:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
              </xs:choice>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>)
    //        val actual = Compiler.testString(testSchema, "567,word,choice1:203867")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<list"))
    //        assertTrue(actualString.endsWith("><a>567</a><b>word</b><choice><c>203867</c></choice></list>"))

    val infoset = <list xmlns={ example }><a>567</a><b>word</b><choice><c>203867</c></choice></list>
    Compiler.testUnparsing(testSchema, infoset, "567,word,choice1:203867")
  }

  // @Test
  def testUnparseChoice2() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element alignmentUnits="bits"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator=",">
          <xs:element name="a" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          <xs:element name="b" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="4"/>
          <xs:element name="choice">
            <xs:complexType>
              <xs:choice dfdl:choiceLengthKind="implicit">
                <xs:element name="c" type="xs:int" dfdl:initiator="choice1:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
                <xs:element name="d" type="xs:double" dfdl:initiator="choice2:" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
              </xs:choice>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>)
    //        val actual = Compiler.testString(testSchema, "567,word,choice2:2038.67")
    //        val actualString = actual.result.toString
    //        assertTrue(actualString.startsWith("<list"))
    //        assertTrue(actualString.endsWith("><a>567</a><b>word</b><choice><d>2038.67</d></choice></list>"))

    val infoset = <list xmlns={ example }><a>567</a><b>word</b><choice><d>2038.67</d></choice></list>
    Compiler.testUnparsing(testSchema, infoset, "567,word,choice2:2038.67")
  }

  def test3 {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml"))
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
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
    assertTrue(a2.isInstanceOf[DFDLNewVariableInstance]) // second annotation is newVariableInstance
    assertEquals(OccursCountKind.Implicit, a1.asInstanceOf[DFDLElement].occursCountKind)
    val e1ct = e1.immediateType.get.asInstanceOf[LocalComplexTypeDef] // first one has immediate complex type
    // Explore local complex type def
    val seq = e1ct.modelGroup.asInstanceOf[Sequence] //... which is a sequence
    val sfa = seq.formatAnnotation.asInstanceOf[DFDLSequence] //...annotated with...
    assertEquals(YesNo.No, sfa.initiatedContent) // initiatedContent="no"

    val Seq(e1a: DFDLElement) = e1.annotationObjs
    assertEquals("UTF-8", e1a.getProperty("encoding"))

    // Explore global simple type defs
    val Seq(st1, st2, st3, st4) = sd.globalSimpleTypeDefs // there are two.
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
    val es = desc1.escapeScheme.escapeCharacterRaw
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
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml"))
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(gd1, gd2, gd3, gd4, gd5) = sd.globalGroupDefs // Obtain Group nodes
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
    val testSchema =
      XML.load(
        Misc.getRequiredResource(
          "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml"))

    val compiler = Compiler()
    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f, ge2f, ge3f, ge4f, ge5f, ge6f) = sd.globalElementDecls // Obtain global element nodes
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

  def test_simple_types_access_works {
    val testSchema =
      XML.load(
        Misc.getRequiredResource(
          "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml"))

    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1, ge2, ge3, ge4, ge5, ge6) = sd.globalElementDecls // Obtain global element nodes

    val x = ge2.forRoot().typeDef.asInstanceOf[LocalSimpleTypeDef]

    assertEquals(AlignmentUnits.Bytes, x.alignmentUnits)
  }

  def test_simple_types_property_combining {
    val testSchema =
      XML.load(
        Misc.getRequiredResource(
          "/test/example-of-named-format-chaining-and-element-simpleType-property-combining.dfdl.xml"))


    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
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
    intercept[daffodil.dsom.SchemaDefinitionError] { ge4.lengthKind }

    assertEquals(AlignmentUnits.Bytes, ge5.alignmentUnits) // local
    assertEquals(OccursCountKind.Parsed, ge5.occursCountKind) // def1
    assertEquals(BinaryNumberRep.Bcd, ge5.binaryNumberRep) // def3
    assertEquals(NilKind.LiteralValue, ge5.nilKind) // local
    assertEquals(Representation.Text, ge5.representation) // def3
    assertEquals(LengthKind.Pattern, ge5.lengthKind) // superseded by local

    // Test Defaulting
    assertEquals(BinaryNumberRep.Packed, ge6.binaryNumberRep)
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

  def test_simpleType_base_combining {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml"))
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
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

    val gs1 = gs1f.forRoot() // Global Simple Type - aType

    assertEquals("ex:aaType", gs1.restrictionBase)
    assertTrue(FindValue(gs1.allNonDefaultProperties, "alignmentUnits", "bytes")) // SimpleType - Local
    assertTrue(FindValue(gs1.allNonDefaultProperties, "byteOrder", "bigEndian")) // SimpleType - Base
    assertTrue(FindValue(gs1.allNonDefaultProperties, "occursCountKind", "implicit")) // Default Format
    assertTrue(FindValue(gs1.allNonDefaultProperties, "representation", "text")) // Define Format - def1
    assertTrue(FindValue(gs1.allNonDefaultProperties, "encoding", "utf-8")) // Define Format - def1
    assertTrue(FindValue(gs1.allNonDefaultProperties, "textStandardBase", "10")) // Define Format - def2
    assertTrue(FindValue(gs1.allNonDefaultProperties, "escapeSchemeRef", "tns:quotingScheme")) // Define Format - def2

    val gs3 = gs3f.forRoot() // Global SimpleType - aTypeError - overlapping base props

    // Tests overlapping properties
    intercept[daffodil.dsom.SchemaDefinitionError] { gs3.allNonDefaultProperties }
  }

  def test_group_references {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml"))
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // No annotations
    val Seq(ct) = sd.globalComplexTypeDefs

    // Explore global element decl
    val Seq(e1f, e2f, e3f, e4f, e5f) = sd.globalElementDecls // there are 3 factories

    // GroupRefTest
    val e4 = e4f.forRoot() // groupRefTest

    val e4ct = e4.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    val e4ctgref = e4ct.modelGroup.asInstanceOf[GroupRef] // groupRefTests' local group decl

    val myGlobal1 = e4ctgref.groupDef

    val myGlobal1Seq = myGlobal1.modelGroup.asInstanceOf[Sequence]

    val myGlobal2Seq = myGlobal1Seq.immediateGroup.get.asInstanceOf[Sequence]

    // val myGlobal2Seq = myGlobal2.modelGroup.asInstanceOf[Sequence]

    // myGlobal1 Properties
    assertTrue(FindValue(myGlobal1Seq.allNonDefaultProperties, "separator", ","))

    // myGlobal2 Properties
    assertTrue(FindValue(myGlobal2Seq.allNonDefaultProperties, "separator", ";"))
    assertTrue(FindValue(myGlobal2Seq.allNonDefaultProperties, "separatorPosition", "infix"))

    // GroupRefTestOverlap
    val e5 = e5f.forRoot() // groupRefTestOverlap

    val e5ct = e5.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    val e5ctgref = e5ct.modelGroup.asInstanceOf[GroupRef] // groupRefTestOverlap's local group decl

    val myGlobal3 = e5ctgref.groupDef
    val myGlobal3Seq = myGlobal3.modelGroup.asInstanceOf[Sequence]

    // Tests overlapping properties
    intercept[daffodil.dsom.SchemaDefinitionError] { myGlobal3Seq.allNonDefaultProperties }

  }

  def test_ibm_7132 {
    val ibm7132Schema = XML.load(Misc.getRequiredResource("/test/TestRefChainingIBM7132.dfdl.xml"))
    val compiler = Compiler()
    val sset = new SchemaSet(ibm7132Schema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()

    val f1 = ge1.formatAnnotation

    val props: Map[String, String] = f1.getFormatProperties()

    assertEquals(true, FindValue(props, "separatorPosition", "infix"))
    assertEquals(true, FindValue(props, "lengthKind", "implicit"))
    assertEquals(true, FindValue(props, "representation", "text"))
    assertEquals(true, FindValue(props, "textNumberRep", "standard"))

    val ct = ge1.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]

    val Seq(e1: ElementBase, e2: ElementBase) = seq.groupMembers

    val e1f = e1.formatAnnotation.asInstanceOf[DFDLElement]
    val e1fProps: Map[String, String] = e1f.getFormatProperties()

    //    println(e1fProps)
    //
    assertEquals(true, FindValue(e1fProps, "initiator", ""))
    //println(e1f.initiatorRaw)

    //e1f.initiatorRaw
    //e1f.byteOrderRaw
    e1f.lengthKind
  }

  def testDfdlRef = {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:defineFormat name="ref1"> <dfdl:format initiator=":"/> </dfdl:defineFormat>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:ref="tns:ref1" type="xs:string">
      </xs:element>)
    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()
    val props = ge1.formatAnnotation.getFormatProperties()

    // println(props)
    //assertEquals(":", ge1.initiatorRaw)
  }

  def testGetQName = {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:defineFormat name="ref1">
        <dfdl:format initiator=":"/>
      </dfdl:defineFormat>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:ref="tns:ref1" type="xs:string">
      </xs:element>)
    // println(testSchema)
    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()
    //val props = ge1.formatAnnotation.getFormatProperties()

    val (nsURI, localName) = ge1.formatAnnotation.getQName("ref1")

    // println(nsURI + ", " + localName)
    assertEquals("ref1", localName)
    assertEquals(XMLUtils.EXAMPLE_NAMESPACE, nsURI)
  }

  def testGetAllNamespaces() {
    val xml = <bar xmlns:foo="fooNS" xmlns:bar="barNS">
                <quux xmlns:baz="bazNS" attr1="x"/>
              </bar>

    val scope = (xml \ "quux")(0).scope
    // println(scope)
    val newElem = scala.xml.Elem("dfdl", "element", null, scope)
    // println(newElem)
  }

  def test_delim_inheritance {
    val delimiterInheritance = XML.load(Misc.getRequiredResource("/test/TestDelimiterInheritance.dfdl.xml"))

    val compiler = Compiler()
    val sset = new SchemaSet(delimiterInheritance)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()

    val ct = ge1.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]

    val Seq(e1: ElementBase, e2: ElementBase, e3: ElementBase) = seq.groupMembers

    assertEquals(3, e1.allTerminatingMarkup.length) // 1 Level + ref on global element decl
    assertEquals("a", e1.allTerminatingMarkup(0).prettyExpr)
    assertEquals("b", e1.allTerminatingMarkup(1).prettyExpr)
    assertEquals("g", e1.allTerminatingMarkup(2).prettyExpr)

    val ct2 = e3.asInstanceOf[ElementBase].typeDef.asInstanceOf[ComplexTypeBase]
    val seq2 = ct2.modelGroup.asInstanceOf[Sequence]

    val Seq(e3_1: ElementBase, e3_2: ElementBase) = seq2.groupMembers

    assertEquals(6, e3_1.allTerminatingMarkup.length) // 2 Level + ref on global element decl
    assertEquals("e", e3_1.allTerminatingMarkup(0).prettyExpr)
    assertEquals("c", e3_1.allTerminatingMarkup(1).prettyExpr)
    assertEquals("d", e3_1.allTerminatingMarkup(2).prettyExpr)
    assertEquals("a", e3_1.allTerminatingMarkup(3).prettyExpr)
    assertEquals("b", e3_1.allTerminatingMarkup(4).prettyExpr)
    assertEquals("g", e3_1.allTerminatingMarkup(5).prettyExpr)

    assertEquals(6, e3_2.allTerminatingMarkup.length) // 2 Level + ref on global element decl + ref on local element decl
    assertEquals("f", e3_2.allTerminatingMarkup(0).prettyExpr) // f instead of e, due to ref
    assertEquals("c", e3_2.allTerminatingMarkup(1).prettyExpr)
    assertEquals("d", e3_2.allTerminatingMarkup(2).prettyExpr)
    assertEquals("a", e3_2.allTerminatingMarkup(3).prettyExpr)
    assertEquals("b", e3_2.allTerminatingMarkup(4).prettyExpr)
    assertEquals("g", e3_2.allTerminatingMarkup(5).prettyExpr)
  }

  def test_escapeSchemeOverride = {
    val testSchema = TestUtils.dfdlTestSchema(
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
    val Seq(sd) = sch.schemaDocuments

    val Seq(ge1f) = sd.globalElementDecls // Obtain global element nodes
    val ge1 = ge1f.forRoot()

    val ct = ge1.typeDef.asInstanceOf[ComplexTypeBase]
    val seq = ct.modelGroup.asInstanceOf[Sequence]

    val Seq(e1: ElementBase, e2: ElementBase) = seq.groupMembers
    val e1f = e1.formatAnnotation.asInstanceOf[DFDLElement]
    val props = e1.allNonDefaultProperties ++ e1.defaultProperties

    val e1f_esref = e1.getProperty("escapeSchemeRef")
    // println(e1f_esref)

    assertEquals("pound", e1f_esref)

    // Should have escapeCharacter and escapeKind

    val e2f = e2.formatAnnotation.asInstanceOf[DFDLElement]
    val e2f_esref = e2.getProperty("escapeSchemeRef")
    // escapeBlockStart/End escapeBlockKind (NOTHING ELSE)
    assertEquals("cStyleComment", e2f_esref)
  }

  def test_element_references {
    val testSchema = XML.load(Misc.getRequiredResource("/test/example-of-most-dfdl-constructs.dfdl.xml"))

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // No annotations
    val Seq(ct) = sd.globalComplexTypeDefs

    // g1.name == "gr"
    val Seq(g1: GlobalGroupDefFactory, g2, g3, g4, g5) = sd.globalGroupDefs

    val seq1 = g1.forGroupRef(dummyGroupRef, 1).modelGroup.asInstanceOf[Sequence]

    // e1.ref == "ex:a"
    val Seq(e1: ElementRef, s1: Sequence) = seq1.groupMembers

    assertEquals(2, e1.maxOccurs)
    assertEquals(1, e1.minOccurs)
    assertEquals(AlignmentUnits.Bytes, e1.alignmentUnits)
    //assertEquals(true, e1.nillable) // TODO: e1.nillable doesn't exist?
    //assertEquals("%ES; %% %#0; %NUL;%ACK; foo%#rF2;%#rF7;bar %WSP*; %#2024;%#xAABB; &amp;&#2023;&#xCCDD; -1", e1.nilValue) // TODO: Do not equal each other!
    assertEquals(NilKind.LiteralValue, e1.nilKind)
  }
}

