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

package org.apache.daffodil.propGen

import java.nio.file.Files
import java.nio.file.Paths

import org.junit.Assert._
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class TestPropertyGenerator {

  @Test def testGenEnum(): Unit = {
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
  @Test def testGenPropMixins(): Unit = {
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
    assertTrue(
      mx.contains(""" = convertToBoolean(findProperty("prefixIncludesPrefixLength").value)""")
    )
    assertTrue(mx.contains("""LengthKindMixin"""))
    assertTrue(mx.contains("""def lengthPropertiesAGInit(): Unit = {"""))
    assertTrue(
      mx.contains(
        """registerToStringFunction(()=>{getPropertyOption("lengthPattern", false) match {
        case None => ""
        case Some(value) => "lengthPattern='" + value.toString + "'"
      }
    })"""
      )
    )
  }

  /**
   * make sure we generate trait mixin for referenced attribute groups.
   */
  @Test def testGenPropMixins2(): Unit = {
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
  @Test def testGenPropMixins3(): Unit = {
    val sch =
      <xsd:attributeGroup name="TextNumberFormatAG1">
        <xsd:attribute name="textNumberFormatRef" type="xsd:NCName"/>
      </xsd:attributeGroup>
    val pg = new PropertyGenerator(sch)
    val mx = pg.genAttributeGroup(sch)
    assertTrue(mx.contains("""convertToNCName(findProperty("textNumberFormatRef")"""))
  }

  @Test def testGenCT(): Unit = {
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

  @Test def testGenCT2(): Unit = {
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
    assertTrue(mx.contains("""convertToNCName(findProperty("name").value)"""))
    assertTrue(mx.contains("""findProperty("baseFormat")"""))
    assertTrue(mx.contains("""convertToQName"""))
  }

  @Test def testElement1(): Unit = {
    val sch =
      <xsd:element name="defineFormat" type="dfdl:DFDLDefineFormat"/>
    val pg = new PropertyGenerator(sch)
    val mx = pg.genElement(sch)
    assertTrue(mx.contains("""with DFDLDefineFormatMixin"""))
  }

  @Test def testElementWithImmediateComplexType1(): Unit = {
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

  /**
   * Test the entire PropertyGenerator package and verify that it creates some files.
   * Ulterior motivation is to push codecov's total coverage above 80% too.
   */
  private val _folder = new TemporaryFolder()
  @Rule def folder = _folder
  @Test def testPropertyGeneratorMain(): Unit = {
    val args = Array(folder.getRoot.getCanonicalPath)
    PropertyGenerator.main(args)
    val path1 = Paths.get(
      args(0),
      "org/apache/daffodil/lib/schema/annotation/props/gen/GeneratedCode.scala"
    )
    val path2 = Paths.get(args(0), "org/apache/daffodil/lib/iapi/DaffodilTunablesGen.scala")
    val path3 = Paths.get(args(0), "org/apache/daffodil/lib/iapi/WarnIdGen.scala")
    assert(Files.exists(path1), "Expected PropertyGenerator to create a file")
    assert(Files.exists(path2), "Expected PropertyGenerator to create a file")
    assert(Files.exists(path3), "Expected PropertyGenerator to create a file")
  }
}
