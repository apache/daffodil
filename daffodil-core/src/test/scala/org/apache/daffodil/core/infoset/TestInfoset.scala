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

package org.apache.daffodil.core.infoset

import java.math.BigInteger

import org.apache.daffodil.api.InfosetArray
import org.apache.daffodil.api.InfosetSimpleElement
import org.apache.daffodil.core.compiler._
import org.apache.daffodil.core.dsom.{ ElementBase, Root }
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.unparsers.UStateMain

import org.apache.commons.io.output.NullOutputStream
import org.junit.Assert._
import org.junit.Test

object TestInfoset {

  /**
   * Compute a Daffodil Infoset from a SchemaSet, and a scala XML element representing
   * the infoset (as projected into XML).
   *
   * Insures the created infoset matches the schema (rooted at that element).
   * Converts xsi:nil="true" to isNilled on the infoset
   * Converts dafint:hidden='true' to isHidden on the infoset (Daffodil extension
   * allowing testing and visualization of the Augmented Infoset)
   *
   * This is used for testing the Infoset code in several different TestInfosetN
   * classes.
   */

  def elem2Infoset(xmlElem: scala.xml.Node, dp: DataProcessor): DIElement = {
    //
    // A prior version of this code just pulled events to force the
    // infoset to be constructed. That doesn't work anymore.
    // The structure of the schema must be respected and the
    // TermRuntimeData must be pushed/popped appropriately.
    //
    // Really the only way to do this is to invoke the unparser since that is
    // what maintains the schema dynamic runtime context.
    //
    val inputter = new ScalaXMLInfosetInputter(xmlElem)
    val dummyOutStream = NullOutputStream.INSTANCE
    val unparseResult = dp.unparse(inputter, dummyOutStream)
    if (unparseResult.isError) {
      val exc = unparseResult.getDiagnostics.filter(_.isError).head
      throw exc
    }
    val infosetRootNode = {
      val ustate = unparseResult.resultState.asInstanceOf[UStateMain]
      val diDocument: DIDocument = ustate.documentElement
      val rootElement = diDocument.child(0).asInstanceOf[DIElement]
      Assert.invariant(rootElement ne null)
      rootElement
    }
    infosetRootNode
  }

  /**
   * Returns the root element of the infoset, along with the schema compiler
   * Root object for examining schema-compiler computations for unit testing
   * them. Because this assumes tests will be inspecting the internal infoset
   * for correctness, it sets the releaseUnusedInfoset tunable to false so that
   * the infoset elements are not freed
   */
  def testInfoset(
    testSchema: scala.xml.Elem,
    infosetAsXML: scala.xml.Elem
  ): (DIElement, Root, DaffodilTunables) = {
    val schemaCompiler =
      Compiler()
        .withTunable("allowExternalPathExpressions", "true")
        .withTunable("releaseUnneededInfoset", "false")
    val pf = schemaCompiler.compileNode(testSchema).asInstanceOf[ProcessorFactory]
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map { _.getMessage() }.mkString("\n")
      fail("pf compile errors: " + msgs)
    }
    val dp = pf.onPath("/").asInstanceOf[DataProcessor]
    if (dp.isError) {
      val msgs = dp.getDiagnostics.map { _.getMessage() }.mkString("\n")
      fail("dp compile errors: " + msgs)
    }
    val infosetRootElem = TestInfoset.elem2Infoset(infosetAsXML, dp)
    (infosetRootElem, pf.sset.root, dp.tunables)
  }

}

class TestInfoset1 {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val ex = XMLUtils.EXAMPLE_NAMESPACE
  import TestInfoset._

  @Test def testXMLToInfoset1(): Unit = {
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

    val xmlInfoset = <list xmlns={ex}><w>4</w></list>

    val (infoset: DIComplex, _, tunable) = testInfoset(testSchema, xmlInfoset)
    val list_erd = infoset.erd
    assertEquals(list_erd, infoset.runtimeData)
    val Seq(w_erd) = list_erd.childERDs
    val wItem = infoset.getChild(w_erd, tunable).asSimple
    assertEquals(infoset, wItem.parent)
    assertEquals(4.toLong, wItem.getLong)
    assertEquals(4, wItem.getInt)
    assertEquals(4.toShort, wItem.getShort)
    assertEquals(4.toByte, wItem.getByte)
    assertEquals(new BigInteger("4"), wItem.getUnsignedLong)
    assertEquals(4.toLong, wItem.getUnsignedInt)
    assertEquals(4.toInt, wItem.getUnsignedShort)
    assertEquals(4.toShort, wItem.getUnsignedByte)
  }

  @Test def testXMLToInfoset2(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
          <xs:choice>
            <xs:element name="a" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element name="b" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
          </xs:choice>
        </xs:sequence>
      </xs:complexType>
    )

    val xmlInfoset = <list xmlns={ex}><w>4</w><c>7</c></list>
    val (infoset, _, tunable) = testInfoset(testSchema, xmlInfoset)
    assertNotNull(infoset.parent)
    val list_erd = infoset.erd
    val Seq(w_erd, _, _, c_erd) = list_erd.childERDs
    assertEquals(list_erd, infoset.runtimeData)
    val wItem = infoset.asComplex.getChild(w_erd, tunable).asSimple
    assertEquals(4, wItem.getShort.toInt)
    val cItem = infoset.asComplex.getChild(c_erd, tunable).asSimple
    assertEquals(7, cItem.getByte.toInt)
    assertEquals(infoset, cItem.parent)
  }

  @Test def testXMLToInfoset2a(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed"/>
        </xs:sequence>
      </xs:complexType>
    )

    val xmlInfoset = <list xmlns={ex}><w>4</w><w>5</w></list>
    val (infoset: DIComplex, _, tunable) = testInfoset(testSchema, xmlInfoset)
    val Seq(w_erd) = infoset.erd.childERDs
    infoset.getChildArray(w_erd, tunable) match {
      case arr: InfosetArray => {
        assertEquals(2, arr.length)
        var a = arr(1).asInstanceOf[InfosetSimpleElement] // 1-based
        assertEquals(w_erd, a.metadata)
        assertEquals(4, a.getUnsignedLong.longValue().toInt)
        var dia = a.asInstanceOf[DISimple]
        assertEquals(infoset, dia.parent)
        a = arr(2).asInstanceOf[InfosetSimpleElement] // 1-based
        assertEquals(5, a.getObject)
        dia = a.asInstanceOf[DISimple]
        assertEquals(infoset, dia.parent)
      }
    }
  }

  @Test def testXMLToInfoset3(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed"/>
          <xs:choice>
            <xs:element name="a" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element name="b" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
          </xs:choice>
        </xs:sequence>
      </xs:complexType>
    )

    val xmlInfoset = <list xmlns={ex}><w>4</w><w>5</w><c>7</c></list>

    val (infoset: DIComplex, _, tunable) = testInfoset(testSchema, xmlInfoset)
    val list_erd = infoset.erd
    val Seq(w_erd, _, _, c_erd) = list_erd.childERDs
    infoset.getChildArray(w_erd, tunable) match {
      case arr: InfosetArray => {
        var a = arr(1).asInstanceOf[InfosetSimpleElement]
        assertEquals(2, arr.length)
        assertEquals(w_erd, a.metadata)
        assertEquals(4, a.getUnsignedInt.toInt)
        var dia = a.asInstanceOf[DISimple]
        assertEquals(infoset, dia.parent)
        a = arr(2).asInstanceOf[InfosetSimpleElement]
        assertEquals(5, a.getUnsignedShort.toInt)
        dia = a.asInstanceOf[DISimple]
        assertEquals(infoset, dia.parent)
      }
    }
    infoset.getChild(c_erd, tunable) match {
      case s: DISimple => assertEquals(7, s.getUnsignedByte.toInt)
      case _ => fail("children should be DISimple")
    }
  }

  @Test def testXMLToInfosetNil1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="x" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed" nillable='true' dfdl:nilKind="literalValue" dfdl:nilValue="-"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val xmlInfoset = <list xmlns={ex} xmlns:xsi={xsi}><x xsi:nil='true'/></list>

    val (infoset: DIComplex, _, tunable) = testInfoset(testSchema, xmlInfoset)
    val list_erd = infoset.erd
    val Seq(x_erd) = list_erd.childERDs
    assertTrue(x_erd.isArray)

    assertTrue(infoset.isInstanceOf[DIComplex])
    val xchild = infoset.getChildArray(x_erd, tunable)
    xchild match {
      case arr: InfosetArray => {
        assertEquals(1, arr.length)
        val xa = arr(1)
        assertEquals(x_erd, xa.metadata)
        assertTrue(xa.isNilled)
      }
    }
  }

  @Test def testXMLToInfoset4(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="x" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed" nillable='true' dfdl:nilKind="literalValue" dfdl:nilValue="%ES;">
            <xs:complexType>
              <xs:choice>
                <xs:element name="a" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="b" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              </xs:choice>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    )

    val xmlInfoset = <list xmlns={ex} xmlns:xsi={xsi}><x xsi:nil='true'/></list>

    val (infoset: DIComplex, _, tunable) = testInfoset(testSchema, xmlInfoset)
    val list_erd = infoset.erd
    val Seq(x_erd) = list_erd.childERDs
    infoset.getChildArray(x_erd, tunable) match {
      case xa: InfosetArray => {
        assertEquals(1, xa.length)
        val e = xa(1)
        assertTrue(e.isNilled)
      }
    }
  }

  @Test def testXMLToInfoset5(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed"/>
          <xs:element name="x" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed" nillable='true' dfdl:nilKind="literalValue" dfdl:nilValue="%ES;">
            <xs:complexType>
              <xs:choice>
                <xs:element name="a" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="b" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              </xs:choice>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    )

    val xmlInfoset = <list xmlns={ex} xmlns:xsi={
      xsi
    }><w>4</w><w>5</w><x xsi:nil='true'/><x><c>7</c></x></list>

    val (infoset: DIComplex, rootTerm, tunable) = testInfoset(testSchema, xmlInfoset)
    val list_erd = infoset.erd
    val rootPossibles = rootTerm.possibleNextLexicalSiblingStreamingUnparserElements
    val Seq(wTerm: ElementBase, xTerm: ElementBase) =
      rootTerm.complexType.modelGroup.groupMembers
    val xPossibles = xTerm.possibleNextLexicalSiblingStreamingUnparserElements
    val Seq(w_erd, x_erd) = list_erd.childERDs
    val Seq(a_erd, b_erd, c_erd) = x_erd.childERDs
    infoset.getChildArray(x_erd, tunable) match {
      case arr: InfosetArray => {
        assertEquals(2, arr.length)
        var xa = arr(1).asInstanceOf[DIComplex]
        assertEquals(x_erd, xa.metadata)
        assertTrue(xa.isNilled)
        xa = arr(2).asInstanceOf[DIComplex] // 1-based
        val c = xa.getChild(c_erd, tunable)
        c match {
          case c: DISimple => assertEquals(7, c.getAnyRef)
          case _ => fail("should be DISimple")
        }
      }
    }
  }

  @Test def testXMLToInfoset6(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed"/>
          <xs:element name="x" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed" nillable='true' dfdl:nilKind="literalValue" dfdl:nilValue="%ES;">
            <xs:complexType>
              <xs:choice>
                <xs:element name="a" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="b" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              </xs:choice>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    )

    val xmlInfoset = <list xmlns={ex}><x><c>7</c></x><x><b>8</b></x></list>

    val (infoset: DIComplex, _, tunable) = testInfoset(testSchema, xmlInfoset)
    val list_erd = infoset.erd
    val Seq(w_erd, x_erd) = list_erd.childERDs
    val Seq(a_erd, b_erd, c_erd) = x_erd.childERDs

    try {
      infoset.getChildArray(w_erd, tunable)
      fail("Expected InfosetNoSuchChildElementException")
    } catch {
      case e: InfosetNoSuchChildElementException => /* w element is not in xmlInfoset */
    }

    infoset.getChildArray(x_erd, tunable) match {
      case arr: DIArray => {
        assertEquals(2, arr.length)
        var xa = arr(1).asInstanceOf[DIComplex]
        assertEquals(x_erd, xa.metadata)
        val c = xa.getChild(c_erd, tunable)
        c match {
          case c: DISimple => assertEquals(7, c.getAnyRef)
          case _ => fail("should be DISimple")
        }
        xa = arr(2).asInstanceOf[DIComplex]
        val b = xa.getChild(b_erd, tunable)
        b match {
          case c: DISimple => assertEquals(8, c.getAnyRef)
          case _ => fail("should be DISimple")
        }
      }
    }
  }

  /**
   * DAFFODIL-2538 - in release 3.1.0 of daffodil this test wouldn't pass.
   * Because the XML loading was assuming a single text, not 3 separate nodes
   * one for "EQUAL_TO_OR_" an Atom[String] for the "&amp;lt;" and one for "_0.0001_SQUARE_DATA_MILES".
   */
  @Test def testXMLToInfoset7(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
      <xs:element name="root" type="tns:example1"/>
        <xs:complexType name="example1">
          <xs:sequence>
            <xs:element name="enum" type="xs:string" />
          </xs:sequence>
        </xs:complexType>
    )

    val xmlInfoset = <root xmlns={
      ex
    }><enum>EQUAL_TO_OR_&lt;_0.0001_SQUARE_DATA_MILES</enum></root>

    val (infoset: DIComplex, _, tunable) = testInfoset(testSchema, xmlInfoset)
    val enumElt: DISimple = infoset.child(0).asInstanceOf[DISimple]
    val value = enumElt.dataValueAsString
    assertEquals("EQUAL_TO_OR_<_0.0001_SQUARE_DATA_MILES", value)

  }
}
