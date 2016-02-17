package edu.illinois.ncsa.daffodil.processors.unparsers

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.Implicits._
import scala.io.Source
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.util.IteratorFromCursor
import edu.illinois.ncsa.daffodil.util.TestUtils

/*
 * These are all tests of OVC expressions that forward reference
 * (and backward. It's a mixture)
 */
class TestOutputValueCalcForwardReference {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testOutputValueCalcForwardReference1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s2>2</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "22", areTracing)
  }

  @Test def testOutputValueCalcForwardReference2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes" textTrimKind="padChar" textStringPadCharacter="%SP;"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s3 }"/>
            <xs:element name="s3" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s3>3</s3></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "333", areTracing)
  }

  @Test def testOutputValueCalcForwardReference3() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="s3" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s2>2</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "222", areTracing)
  }

  @Test def testOutputValueCalcDeadlock() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s3 }"/>
            <xs:element name="s3" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }></ex:e1>
    val areTracing = false
    val exc = intercept[SuspendedExpressionsDeadlockException] {
      TestUtils.testUnparsing(sch, infoset, "222", areTracing)
    }
    // println(exc)
    val msg = exc.getMessage().toLowerCase()
    assertTrue(msg.contains("deadlocked"))
    assertTrue(msg.contains("runtime schema definition error"))
  }

  @Test def testOutputValueCalcConstant() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="afterFoo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:bar xmlns:ex={ example }><afterFoo>fghij</afterFoo></ex:bar>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "abcdefghij", areTracing)
  }

  @Test def testOutputValueCalcAfterOptional() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",%#x20;">
            <xs:element name="beforeFoo" type="xs:string" dfdl:initiator="beforeFoo=" dfdl:lengthKind="explicit" dfdl:length="5" minOccurs="0"/>
            <xs:element name="foo" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:initiator="foo=" dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="afterFoo" dfdl:initiator="afterFoo=" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:bar xmlns:ex={ example }><beforeFoo>12345</beforeFoo><afterFoo>67890</afterFoo></ex:bar>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "beforeFoo=12345, foo=abcde, afterFoo=67890", areTracing)
  }

  @Test def testMultipleOutputValueCalcAndDefaultablePresent() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",%#x20;">
            <xs:element name="beforeFoo" type="xs:string" dfdl:initiator="beforeFoo=" dfdl:lengthKind="explicit" dfdl:length="5" minOccurs="0"/>
            <xs:element name="foo" dfdl:initiator="foo=" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="foo2" dfdl:initiator="foo2=" type="xs:string" default="fghij" dfdl:lengthKind="delimited" dfdl:terminator="!"/>
            <xs:element name="afterFoo" dfdl:initiator="afterFoo=" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:bar xmlns:ex={ example }><beforeFoo>12345</beforeFoo><foo2>pqrst</foo2><afterFoo>67890</afterFoo></ex:bar>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "beforeFoo=12345, foo=abcde, foo2=pqrst!, afterFoo=67890", areTracing)
  }

}
