package daffodil.dsom

import daffodil.xml.XMLUtils
import daffodil.util._
import scala.xml._
import daffodil.compiler._
import org.scalatest.junit.JUnitSuite
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
import org.junit.Test

class TestDsomCompiler2 extends JUnitSuite with Logging {

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

  @Test def testUnparseChoice1() {
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

  @Test def testUnparseChoice2() {
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

  @Test def testUnparseBinaryIntBE() {
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

  @Test def testUnparseBinaryIntLE() {
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

  //  @Test def testBMP() {
  //    val infoset = scala.xml.XML.loadFile("header.xml")
  //    val compiler = Compiler()
  //    val pf = compiler.compile("BMPtest.xsd") //pass compiler the schema
  //    val unparser = pf.onPath("/") //create unparser for given schema
  //    val outputStream = new FileOutputStream("testBMP")
  //    val out = java.nio.channels.Channels.newChannel(outputStream)
  //    unparser.unparse(out, infoset) //pass infoset to unparser
  //    out.close()
  //  }

  //  @Test def testBMPPixels() {
  //    val infoset = scala.xml.XML.loadFile("pixeldata.xml")
  //    val compiler = Compiler()
  //    val pf = compiler.compile("BMPtestPixels.xsd") //pass compiler the schema
  //    val unparser = pf.onPath("/") //create unparser for given schema
  //    val outputStream = new FileOutputStream("testBMPPixels")
  //    val out = java.nio.channels.Channels.newChannel(outputStream)
  //    unparser.unparse(out, infoset) //pass infoset to unparser
  //    out.close()
  //  }

  @Test def testUnparseBinary1() {
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
}

