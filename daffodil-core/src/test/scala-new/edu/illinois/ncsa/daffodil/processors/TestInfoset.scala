/* Copyright (c) 2014 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.Implicits._
import scala.xml._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.util.Misc
import junit.framework.Assert._
import java.io.FileOutputStream
import java.nio.channels.WritableByteChannel
import java.io.FileWriter
import java.io.File
import java.nio.ByteBuffer
import org.junit.Test

class TestInfoset1 {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val ex = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testXMLToInfoset1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)

    val xmlInfoset = <ex:list xmlns:ex={ ex }><w>4</w></ex:list>

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val list_erd = decl.elementRuntimeData
    val Seq(w) = decl.elementChildren
    val w_erd = w.elementRuntimeData
    val wSlot = w.slotIndexInParent
    val infoset = Infoset.elem2Infoset(decl.elementRuntimeData, xmlInfoset).asInstanceOf[InfosetComplexElement]
    assertTrue(infoset.isInstanceOf[DIComplex])
    assertEquals(list_erd, infoset.runtimeData)
    val wItem = infoset.getChild(w_erd).asInstanceOf[InfosetSimpleElement]
    assertEquals(infoset, wItem.parent.get)
    assertEquals(4, wItem.dataValue)

  }

  @Test def testXMLToInfoset2() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
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
      </xs:complexType>)

    val xmlInfoset = <ex:list xmlns:ex={ ex }><w>4</w><c>7</c></ex:list>

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val list_erd = decl.elementRuntimeData
    val Seq(w, a, b, c) = decl.elementChildren
    val Seq(w_erd, a_erd, b_erd, c_erd) = decl.elementRuntimeData.childERDs
    val wSlot = w.slotIndexInParent
    val infoset = Infoset.elem2Infoset(decl.elementRuntimeData, xmlInfoset).asInstanceOf[InfosetComplexElement]
    assertTrue(infoset.isInstanceOf[DIComplex])
    assertEquals(Maybe.Nope, infoset.parent)
    assertEquals(list_erd, infoset.runtimeData)
    val wItem = infoset.getChild(w_erd).asInstanceOf[InfosetSimpleElement]
    assertEquals(4, wItem.dataValue)
    val cItem = infoset.getChild(c_erd).asInstanceOf[InfosetSimpleElement]
    assertEquals(7, cItem.dataValue)
    assertEquals(infoset, cItem.parent.get)
  }

  @Test def testRuns1() {

    val xmlInfoset = <ex:list xmlns:ex={ ex }><w>4</w><w>5</w><c>7</c></ex:list>

    val runs = Infoset.groupRuns(xmlInfoset.child)
    val Seq(wRun, cRun) = runs
    val Seq(w1, w2) = wRun
  }

  @Test def testXMLToInfoset2a() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed"/>
        </xs:sequence>
      </xs:complexType>)

    val xmlInfoset = <ex:list xmlns:ex={ ex }><w>4</w><w>5</w></ex:list>

    val runs = Infoset.groupRuns(xmlInfoset.child)
    val Seq(wRun) = runs
    val Seq(w1, w2) = wRun

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val list_erd = decl.elementRuntimeData
    val Seq(w) = decl.elementChildren
    val Seq(w_erd) = decl.elementRuntimeData.childERDs
    val wSlot = w.slotIndexInParent
    val infoset = Infoset.elem2Infoset(decl.elementRuntimeData, xmlInfoset).asInstanceOf[InfosetComplexElement]
    assertTrue(infoset.isInstanceOf[DIComplex])
    infoset.getChildArray(w_erd).get match {
      case arr: DIArray => {
        assertEquals(2, arr.length)
        var a = arr(1).asInstanceOf[InfosetSimpleElement]
        assertEquals(w_erd, a.runtimeData)

        assertEquals(4, a.dataValue)
        assertEquals(infoset, a.parent.get)
        a = arr(2).asInstanceOf[InfosetSimpleElement] // 1-based
        assertEquals(5, a.dataValue)
        assertEquals(infoset, a.parent.get)
      }
    }
  }

  @Test def testXMLToInfoset3() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
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
      </xs:complexType>)

    val xmlInfoset = <ex:list xmlns:ex={ ex }><w>4</w><w>5</w><c>7</c></ex:list>

    val runs = Infoset.groupRuns(xmlInfoset.child)
    val Seq(wRun, cRun) = runs
    val Seq(w1, w2) = wRun

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val list_erd = decl.elementRuntimeData
    val Seq(w, a, b, c) = decl.elementChildren
    val Seq(w_erd, a_erd, b_erd, c_erd) = decl.elementRuntimeData.childERDs
    val wSlot = w.slotIndexInParent
    val infoset = Infoset.elem2Infoset(decl.elementRuntimeData, xmlInfoset).asInstanceOf[InfosetComplexElement]
    assertTrue(infoset.isInstanceOf[DIComplex])
    infoset.getChildArray(w_erd).get match {
      case arr: DIArray => {
        var a = arr(1).asInstanceOf[InfosetSimpleElement]
        assertEquals(2, arr.length)
        assertEquals(w_erd, a.runtimeData)
        assertEquals(4, a.dataValue)
        assertEquals(infoset, a.parent.get)
        a = arr(2).asInstanceOf[InfosetSimpleElement] // 1-based
        assertEquals(5, a.dataValue)
        assertEquals(infoset, a.parent.get)
      }
    }
    infoset.getChild(c_erd) match {
      case s: DISimple => assertEquals(7, s.dataValue)
    }
  }

  @Test def testXMLToInfosetNil1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="x" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed" nillable='true' dfdl:nilKind="literalValue" dfdl:nilValue="-"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val xmlInfoset = <ex:list xmlns:ex={ ex }><x xsi:nil='true'/></ex:list>

    val runs = Infoset.groupRuns(xmlInfoset.child)
    val Seq(xRun) = runs
    val Seq(x1) = xRun

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val list_erd = decl.elementRuntimeData
    val Seq(x) = decl.elementChildren
    val Seq(x_erd) = decl.elementRuntimeData.childERDs
    assertTrue(x_erd.isArray)
    val infoset = Infoset.elem2Infoset(decl.elementRuntimeData, xmlInfoset).asInstanceOf[InfosetComplexElement]
    assertTrue(infoset.isInstanceOf[DIComplex])
    val xchild = infoset.getChildArray(x_erd).get
    xchild match {
      case arr: DIArray => {
        assertEquals(1, arr.length)
        var xa = arr(1)
        assertEquals(x_erd, xa.runtimeData)
        assertTrue(xa.isNilled)
      }
    }
  }

  @Test def testXMLToInfoset4() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="x" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed" nillable='true' dfdl:nilKind="literalValue" dfdl:nilValue="-">
            <xs:complexType>
              <xs:choice>
                <xs:element name="a" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="b" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              </xs:choice>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>)

    val xmlInfoset = <ex:list xmlns:ex={ ex }><x xsi:nil='true'/></ex:list>

    val runs = Infoset.groupRuns(xmlInfoset.child)
    val Seq(xRun) = runs
    val Seq(x1) = xRun

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val list_erd = decl.elementRuntimeData
    val Seq(x) = decl.elementChildren
    val Seq(x_erd) = decl.elementRuntimeData.childERDs
    val Seq(a_erd, b_erd, c_erd) = x_erd.childERDs
    assertTrue(x_erd.isArray)
    assertFalse(c_erd.isArray)
    val infoset = Infoset.elem2Infoset(decl.elementRuntimeData, xmlInfoset).asInstanceOf[InfosetComplexElement]
    assertTrue(infoset.isInstanceOf[DIComplex])
    infoset.getChildArray(x_erd).get match {
      case xa: DIArray => {
        assertEquals(1, xa.length)
        val e = xa.getOccurrence(1)
        assertTrue(e.isNilled)
      }
    }
  }

  @Test def testXMLToInfoset5() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed"/>
          <xs:element name="x" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed" nillable='true' dfdl:nilKind="literalValue" dfdl:nilValue="-">
            <xs:complexType>
              <xs:choice>
                <xs:element name="a" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="b" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              </xs:choice>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>)

    val xmlInfoset = <ex:list xmlns:ex={ ex }><w>4</w><w>5</w><x xsi:nil='true'/><x><c>7</c></x></ex:list>

    val runs = Infoset.groupRuns(xmlInfoset.child)
    val Seq(wRun, xRun) = runs
    val Seq(w1, w2) = wRun
    val Seq(x1, x2) = xRun

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val list_erd = decl.elementRuntimeData
    val Seq(w, x) = decl.elementChildren
    val Seq(w_erd, x_erd) = decl.elementRuntimeData.childERDs
    val Seq(a_erd, b_erd, c_erd) = x_erd.childERDs
    assertTrue(w_erd.isArray)
    assertTrue(x_erd.isArray)
    assertFalse(c_erd.isArray)
    val infoset = Infoset.elem2Infoset(decl.elementRuntimeData, xmlInfoset).asInstanceOf[InfosetComplexElement]
    assertTrue(infoset.isInstanceOf[DIComplex])
    infoset.getChildArray(x_erd).get match {
      case arr: DIArray => {
        assertEquals(2, arr.length)
        var xa = arr(1).asInstanceOf[InfosetComplexElement]
        assertEquals(x_erd, xa.runtimeData)
        assertTrue(xa.isNilled)
        xa = arr(2).asInstanceOf[InfosetComplexElement] // 1-based
        val c = xa.getChild(c_erd)
        c match {
          case c: DISimple => assertEquals(7, c.dataValue)
        }
      }
    }
  }

  @Test def testXMLToInfoset6() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed"/>
          <xs:element name="x" minOccurs="0" maxOccurs="2" dfdl:occursCountKind="parsed" nillable='true' dfdl:nilKind="literalValue" dfdl:nilValue="-">
            <xs:complexType>
              <xs:choice>
                <xs:element name="a" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="b" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
                <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              </xs:choice>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>)

    val xmlInfoset = <ex:list xmlns:ex={ ex }><x><c>7</c></x><x><b>8</b></x></ex:list>

    val runs = Infoset.groupRuns(xmlInfoset.child)
    val Seq(xRun) = runs
    val Seq(x1, x2) = xRun

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val list_erd = decl.elementRuntimeData
    val Seq(w, x) = decl.elementChildren
    val Seq(w_erd, x_erd) = decl.elementRuntimeData.childERDs
    val Seq(a_erd, b_erd, c_erd) = x_erd.childERDs
    assertTrue(w_erd.isArray)
    assertTrue(x_erd.isArray)
    assertFalse(c_erd.isArray)
    val infoset = Infoset.elem2Infoset(decl.elementRuntimeData, xmlInfoset).asInstanceOf[InfosetComplexElement]
    val warr = infoset.getChildArray(w_erd)
    assertTrue(warr.isDefined) // getChildArray now creates empty array if needed.
    assertTrue(infoset.isInstanceOf[DIComplex])
    infoset.getChildArray(x_erd).get match {
      case arr: DIArray => {
        assertEquals(2, arr.length)
        var xa = arr(1).asInstanceOf[InfosetComplexElement]
        assertEquals(x_erd, xa.runtimeData)
        val c = xa.getChild(c_erd)
        c match {
          case c: DISimple => assertEquals(7, c.dataValue)
        }
        xa = arr(2).asInstanceOf[InfosetComplexElement]
        val b = xa.getChild(b_erd)
        b match {
          case c: DISimple => assertEquals(8, c.dataValue)
        }
      }
    }
  }
}