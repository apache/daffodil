package edu.illinois.ncsa.daffodil.propGen

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

// Copyright (C) 2012 Michael J. Beckerle. All Rights Reserved.

import org.xml.sax.InputSource
import scala.xml._
import junit.framework.Assert._
import org.junit.Test

class TestPropertyGenerator {

  @Test def testGenEnum() {
    val sch = <xsd:simpleType name="NilKindEnum">
                <xsd:restriction base="xsd:string">
                  <xsd:enumeration value="literalValue"/>
                  <xsd:enumeration value="logicalValue"/>
                  <xsd:enumeration value="literalCharacter"/>
                  <xsd:enumeration value="nilIndicator"/>
                </xsd:restriction>
                <!-- 15.9	Properties for Nillable Elements -->
              </xsd:simpleType>

    val pg = new PropertyGenerator(sch)
    val nk = pg.genSimpleType(sch)
    assertTrue(nk.contains("sealed trait NilKind"))
  }

  /**
   * test that we get some of the right artifacts generated from a real example derived from
   * the XML Schema for DFDL Annotations.
   *
   * Includes both enum-valued properties and string-valued properties, qnames, etc.
   *
   * Test also looks for the registration of the toString function for the property.
   */
  @Test def testGenPropMixins() {
    val sch =
      <xsd:attributeGroup name="LengthPropertiesAG">
        <xsd:attribute name="lengthKind" type="dfdl:LengthKindEnum"/>
        <xsd:attribute name="length" type="xsd:string"/>
        <xsd:attribute name="lengthPattern" type="xsd:string"/>
        <xsd:attribute name="lengthUnits" type="dfdl:LengthUnitsEnum"/>
        <xsd:attribute name="prefixIncludesPrefixLength" type="xsd:boolean"/>
        <xsd:attribute name="prefixLengthType" type="xsd:QName"/>
      </xsd:attributeGroup>
    val pg = new PropertyGenerator(sch)
    val mx = pg.genAttributeGroup(sch)
    assertTrue(mx.contains(""" = convertToBoolean(getProperty("prefixIncludesPrefixLength"))"""))
    assertTrue(mx.contains("""LengthKindMixin"""))
    assertTrue(mx.contains("""def lengthPropertiesAGInit() : Unit = {"""))
    assertTrue(mx.contains("""registerToStringFunction(()=>{getPropertyOption("lengthPattern") match {
        case None => ""
        case Some(value) => "lengthPattern='" + value.toString + "'"
      }
    })"""))
  }

  /**
   * make sure we generate trait mixin for referenced attribute groups.
   */
  @Test def testGenPropMixins2() {
    val sch =
      <xsd:attributeGroup name="ElementAG">
        <xsd:attributeGroup ref="dfdl:SimpleTypeAG"></xsd:attributeGroup>
        <xsd:attributeGroup ref="dfdl:NillableAG"></xsd:attributeGroup>
        <xsd:attributeGroup ref="dfdl:DefaultValueControlAG"></xsd:attributeGroup>
        <xsd:attributeGroup ref="dfdl:OccursAG"></xsd:attributeGroup>
      </xsd:attributeGroup>
    val pg = new PropertyGenerator(sch)
    val mx = pg.genAttributeGroup(sch)
    assertTrue(mx.contains("""with SimpleTypeAGMixin"""))
    assertTrue(mx.contains("""with NillableAGMixin"""))
  }

  /**
   * verify that the right thing is generated when the type is xsd:NCName, i.e., a built-in type
   * not one defined in the XML Schema for DFDL annotations.
   */
  @Test def testGenPropMixins3() {
    val sch =
      <xsd:attributeGroup name="TextNumberFormatAG1">
        <xsd:attribute name="textNumberFormatRef" type="xsd:NCName"/>
      </xsd:attributeGroup>
    val pg = new PropertyGenerator(sch)
    val mx = pg.genAttributeGroup(sch)
    assertTrue(mx.contains("""convertToNCName(getProperty("textNumberFormatRef")"""))
  }

  @Test def testGenCT() {
    val sch =
      <xsd:complexType name="DFDLSequenceType">
        <xsd:complexContent>
          <xsd:extension base="dfdl:DFDLBaseType">
            <xsd:sequence/>
            <xsd:attributeGroup ref="dfdl:SequenceGroupsAG"/>
            <xsd:attributeGroup ref="dfdl:SequenceGroupsWithDelimitersAG"/>
          </xsd:extension>
        </xsd:complexContent>
      </xsd:complexType>
    val pg = new PropertyGenerator(sch)
    val mx = pg.genComplexType(sch)
    assertTrue(mx.contains("""with SequenceGroupsAGMixin"""))
    assertTrue(mx.contains("""with SequenceGroupsWithDelimitersAGMixin"""))
  }

  @Test def testGenCT2() {
    val sch =
      <xsd:complexType name="DFDLDefineFormat">
        <xsd:all>
          <xsd:element ref="dfdl:format" minOccurs="0"/>
        </xsd:all>
        <xsd:attribute name="name" type="xsd:NCName"/>
        <xsd:attribute name="baseFormat" type="xsd:QName"/>
      </xsd:complexType>
    val pg = new PropertyGenerator(sch)
    val mx = pg.genComplexType(sch)
    assertTrue(mx.contains("""convertToNCName(getProperty("name"))"""))
    assertTrue(mx.contains("""convertToQName(getProperty("baseFormat")"""))
  }

  @Test def testElement1() {
    val sch =
      <xsd:element name="defineFormat" type="dfdl:DFDLDefineFormat"/>
    val pg = new PropertyGenerator(sch)
    val mx = pg.genElement(sch)
    assertTrue(mx.contains("""with DFDLDefineFormatMixin"""))
  }

  @Test def testElementWithImmediateComplexType1() {
    val sch =
      <xsd:element name="defineVariable">
        <xsd:complexType>
          <xsd:simpleContent>
            <xsd:extension base="dfdl:DFDLVariableType">
              <xsd:attributeGroup ref="dfdl:DefineVariableAG"/>
            </xsd:extension>
          </xsd:simpleContent>
        </xsd:complexType>
      </xsd:element>
    val pg = new PropertyGenerator(sch)
    val mx = pg.genElement(sch)
    assertTrue(mx.contains("""trait DFDLDefineVariableTypeMixin extends PropertyMixin"""))
    assertTrue(mx.contains("""trait DefineVariable_AnnotationMixin extends PropertyMixin"""))
    assertTrue(mx.contains("""with DFDLDefineVariableTypeMixin"""))
  }

}

