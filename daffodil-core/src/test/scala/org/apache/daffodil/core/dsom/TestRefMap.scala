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

import org.apache.daffodil.core.compiler._
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert._
import org.junit.Test

class TestRefMap {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testRefMapEmpty(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(2, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(1, numEntries)
    val numRefPairs = root.refPairsMap.size
    assertEquals(1, numRefPairs) // good proxy for schema size
  }

  @Test def testRefMapComplex1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="ex:ct"/>
        <xs:complexType name="ct">
          <xs:sequence>
            <xs:element name="e" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
          </xs:sequence>
        </xs:complexType>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(5, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(2, numEntries)
    val numRefPairs = root.refPairsMap.size
    assertEquals(2, numRefPairs) // good proxy for schema size
    val ctDef = root.complexType.asInstanceOf[GlobalComplexTypeDef]
    val Some(Seq((edecl, ctRefSpecs))) = refMap.get(ctDef)
    assertEquals(root.referencedElement.shortSchemaComponentDesignator, edecl)
  }

  @Test def testRefMapGroupRefSeq1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="ex:ct"/>
        <xs:complexType name="ct">
          <xs:group ref="ex:g"/>
        </xs:complexType>
        <xs:group name="g">
          <xs:sequence>
            <xs:element name="e" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
          </xs:sequence>
        </xs:group>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(6, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(3, numEntries)
    val numRefPairs = root.refPairsMap.size
    assertEquals(3, numRefPairs) // good proxy for schema size
    val rootDecl = root.referencedElement
    val ctDef = root.complexType.asInstanceOf[GlobalComplexTypeDef]
    val Some(Seq((edecl, ctRefSpecs))) = refMap.get(ctDef)
    assertEquals(rootDecl.shortSchemaComponentDesignator, edecl)
    val gref = rootDecl.complexType.modelGroup.asInstanceOf[SequenceGroupRef]
    val Seq((grefSSCD, gdRefSpecs)) = refMap.get(gref.groupDef).get
    assertEquals(gref.shortSchemaComponentDesignator, grefSSCD)
    assertEquals(6, root.numComponents)
    assertEquals(6, root.numUniqueComponents)
  }

  @Test def testRefMapGroupRefChoice1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="ex:ct"/>
        <xs:complexType name="ct">
          <xs:group ref="ex:g"/>
        </xs:complexType>
        <xs:group name="g">
          <xs:choice>
            <xs:element name="e" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element name="f" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
          </xs:choice>
        </xs:group>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(7, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(3, numEntries)
    val numRefPairs = root.refPairsMap.size
    assertEquals(3, numRefPairs) // good proxy for schema size
    val rootDecl = root.referencedElement
    val ctDef = root.complexType.asInstanceOf[GlobalComplexTypeDef]
    val Some(Seq((edecl, ctRefSpecs))) = refMap.get(ctDef)
    assertEquals(rootDecl.shortSchemaComponentDesignator, edecl)
    val gref = rootDecl.complexType.modelGroup.asInstanceOf[ChoiceGroupRef]
    val Seq((grefSSCD, gdRefSpecs)) = refMap.get(gref.groupDef).get
    assertEquals(gref.shortSchemaComponentDesignator, grefSSCD)
    assertEquals(7, root.numComponents)
    assertEquals(7, root.numUniqueComponents)
  }

  @Test def testRefMapGroupRefNest1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="ex:ct"/>
        <xs:complexType name="ct">
          <xs:group ref="ex:g"/>
        </xs:complexType>
        <xs:group name="g">
          <xs:choice>
            <xs:element name="e" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element name="f" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element ref="ex:r1"/>
          </xs:choice>
        </xs:group>
          <xs:element name="r1" type="ex:ct1"/>
        <xs:complexType name="ct1">
          <xs:sequence>
            <xs:sequence dfdl:hiddenGroupRef="ex:g1"/>
          </xs:sequence>
        </xs:complexType>
        <xs:group name="g1">
          <xs:choice>
            <xs:element name="e1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit" dfdl:outputValueCalc="{ 0 }"/>
            <xs:element name="f1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
          </xs:choice>
        </xs:group>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(15, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(6, numEntries)
    val numRefPairs = root.refPairsMap.size
    assertEquals(6, numRefPairs) // good proxy for schema size
    val rootDecl = root.referencedElement
    val ctDef = root.complexType.asInstanceOf[GlobalComplexTypeDef]
    val Some(Seq((edecl, ctRefSpecs))) = refMap.get(ctDef)
    assertEquals(rootDecl.shortSchemaComponentDesignator, edecl)
    val gref = rootDecl.complexType.modelGroup.asInstanceOf[ChoiceGroupRef]
    val Seq((grefSSCD, gdRefSpecs)) = refMap.get(gref.groupDef).get
    assertEquals(gref.shortSchemaComponentDesignator, grefSSCD)
    assertEquals(15, root.numComponents)
    assertEquals(15, root.numUniqueComponents)
  }

  @Test def testRefMapNonExplosion1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="ex:ct"/>
        <xs:complexType name="ct">
          <xs:sequence>
            <xs:group ref="ex:g"/>
            <xs:group ref="ex:g"/>
          </xs:sequence>
        </xs:complexType>
        <xs:group name="g">
          <xs:sequence>
            <!-- a choice here would be UPA ambiguous -->
            <xs:element ref="ex:r1"/>
            <xs:element ref="ex:r1"/>
          </xs:sequence>
        </xs:group>
        <xs:element name="r1">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              <xs:element name="f1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(14, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(4, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map {
      _._2.length
    }.sum
    assertEquals(6, numRefPairs) // good proxy for schema size
    val allGRefs = root.allGRefs
    assertEquals(2, allGRefs.size)
    val allERefsCount = root.allERefs.size
    assertEquals(2, allERefsCount)
    val allCTRefsCount = root.allCTRefs.size
    assertEquals(1, allCTRefsCount)
    assertEquals(14, root.numComponents)
    assertEquals(14, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion2(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="ex:ct"/>
        <xs:complexType name="ct">
          <xs:sequence>
            <xs:group ref="ex:g"/>
            <xs:group ref="ex:g"/>
          </xs:sequence>
        </xs:complexType>
        <xs:group name="g">
          <xs:sequence>
            <!-- a choice here would be UPA ambiguous -->
            <xs:element ref="ex:r1"/>
            <xs:element ref="ex:r1"/>
          </xs:sequence>
        </xs:group>
        <xs:element name="r1">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:r2"/>
              <xs:element ref="ex:r2"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="r2">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              <xs:element name="f1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(19, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(5, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map {
      _._2.length
    }.sum
    assertEquals(8, numRefPairs)
    assertEquals(19, root.numComponents)
    assertEquals(19, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion3(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:element ref="ex:r1"/>
            <xs:element ref="ex:r1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
        <xs:element name="r1">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:r2"/>
              <xs:element ref="ex:r2"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="r2">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:r3"/>
              <xs:element ref="ex:r3"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="r3">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              <xs:element name="f1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(21, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(4, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map {
      _._2.length
    }.sum
    assertEquals(7, numRefPairs)
    assertEquals(21, root.numComponents)
    assertEquals(21, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion4(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e5">
        <xs:complexType>
          <xs:sequence>
            <xs:element ref="ex:e4"/>
            <xs:element ref="ex:e4"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
        <xs:element name="e4">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e3"/>
              <xs:element ref="ex:e3"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e3">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e2"/>
              <xs:element ref="ex:e2"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e2">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e1"/>
              <xs:element ref="ex:e1"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e1">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e0" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              <xs:element name="f0" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(26, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(5, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map {
      _._2.length
    }.sum
    assertEquals(9, numRefPairs)
    assertEquals(26, root.numComponents)
    assertEquals(26, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion5(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e6">
        <xs:complexType>
          <xs:sequence>
            <xs:element ref="ex:e5"/>
            <xs:element ref="ex:e5"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
        <xs:element name="e5">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e4"/>
              <xs:element ref="ex:e4"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e4">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e3"/>
              <xs:element ref="ex:e3"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e3">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e2"/>
              <xs:element ref="ex:e2"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e2">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e1"/>
              <xs:element ref="ex:e1"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e1">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e0" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              <xs:element name="f0" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(31, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(6, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map {
      _._2.length
    }.sum
    assertEquals(11, numRefPairs)
    assertEquals(31, root.numComponents)
    assertEquals(31, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion6(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e7">
        <xs:complexType>
          <xs:sequence>
            <xs:element ref="ex:e6"/>
            <xs:element ref="ex:e6"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
        <xs:element name="e6">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e5"/>
              <xs:element ref="ex:e5"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e5">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e4"/>
              <xs:element ref="ex:e4"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e4">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e3"/>
              <xs:element ref="ex:e3"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e3">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e2"/>
              <xs:element ref="ex:e2"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e2">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e1"/>
              <xs:element ref="ex:e1"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e1">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e0" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              <xs:element name="f0" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(36, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(7, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map {
      _._2.length
    }.sum
    assertEquals(13, numRefPairs)
    assertEquals(36, root.numComponents)
    assertEquals(36, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion7(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e8">
        <xs:complexType>
          <xs:sequence>
            <xs:element ref="ex:e7"/>
            <xs:element ref="ex:e7"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
        <xs:element name="e7">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e6"/>
              <xs:element ref="ex:e6"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e6">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e5"/>
              <xs:element ref="ex:e5"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e5">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e4"/>
              <xs:element ref="ex:e4"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e4">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e3"/>
              <xs:element ref="ex:e3"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e3">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e2"/>
              <xs:element ref="ex:e2"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e2">
          <xs:complexType>
            <xs:sequence>
              <xs:element ref="ex:e1"/>
              <xs:element ref="ex:e1"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="e1">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e0" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
              <xs:element name="f0" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(41, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(8, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map {
      _._2.length
    }.sum
    assertEquals(15, numRefPairs)
    assertEquals(41, root.numComponents)
    assertEquals(41, root.numUniqueComponents)
  }

  /**
   * This test reproduces a situation observed in the VMF DFDL Schema.
   *
   * The situation is an element uses a complex type that references a global group g.
   * Another global group h, also references g.
   * But nothing uses h.
   *
   * This *was* failing because Daffodil was incorrectly giving the group definition
   * for h an enclosingElement of the root. It really should have no enclosing element.
   */
  @Test def testUnusedGroup1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
      <xs:element name="r" type="ex:ct"/>
        <xs:complexType name="ct">
          <xs:group ref="ex:g"/>
        </xs:complexType>
        <xs:group name="h"><!-- unused group shares referenced child group -->
          <xs:sequence>
            <xs:group ref="ex:g"/>
          </xs:sequence>
        </xs:group>
        <xs:group name="g">
          <xs:sequence>
            <!-- computed element expressions are not meaningful for the unused global group 'h' -->
            <xs:element name="e" type="xs:int" dfdl:outputValueCalc='{ ../f }'/>
            <xs:element name="f" type="xs:int" dfdl:inputValueCalc='{ ../e }' />
          </xs:sequence>
        </xs:group>,
      useDefaultNamespace = false,
      elementFormDefault = "unqualified"
    )
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    val refMap = root.refMap
    val numEntries = refMap.size
    val numRefPairs = root.refPairsMap.size
    val rootDecl = root.referencedElement
    val ctDef = root.complexType.asInstanceOf[GlobalComplexTypeDef]
    val Some(Seq((edecl, ctRefSpecs))) = refMap.get(ctDef)
    val gref = rootDecl.complexType.modelGroup.asInstanceOf[SequenceGroupRef]
    val g_gDef = gref.groupDef
    val eDecl = g_gDef.groupMembers(0)
    val fDecl = g_gDef.groupMembers(1)
    val e_enclComps = eDecl.enclosingComponents
    assertEquals(1, e_enclComps.length)
    val gDef = e_enclComps(0).encloser.asInstanceOf[GlobalSequenceGroupDef]
    val e_enclTerms = eDecl.enclosingTerms
    assertEquals(2, e_enclTerms.length)
    // The second of the above enclosingTerms should NOT have any enclosing element.
    val e_enclElems = eDecl.enclosingElements
    assertEquals(1, e_enclElems.length)
  }

  lazy val testRootPathsSchema1 = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
    //
    // note that the first element below will be the root element.
    // it doesn't use any of the rest of the schema
    //
    // The notRoot element uses type ct, which contains element rr
    // which makes the expressions inside group g's elements meaningful.
    // But... notRoot isn't the root.
    //
    // The h element also uses group g. But there is no element rr. The
    // relative paths inside group g aren't meaningful in the context of
    // element h. Since h is not the root, we will only check, and detect this
    // if compileAllTopLevels is true.
    //
    <xs:element name="r" type="xs:int"/>
        <xs:element name="notRoot" type="tns:ct"/>
      ++
        restOfXML,
    useDefaultNamespace = false,
    elementFormDefault = "unqualified"
  )

  lazy val restOfXML =
    (<xs:element name="h"><!-- uses the substructure, but expressions are past root-->
          <xs:complexType>
            <xs:sequence>
              <!-- The group g contains elements with paths that are past the root for
              this usage inside element h -->
              <xs:group ref="ex:g"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      ++
        restOfXML2)

  lazy val restOfXML2 =
    <xs:complexType name="ct">
          <xs:sequence>
            <xs:element name="rr">
              <xs:complexType>
                <xs:sequence>
                  <xs:group ref="ex:g"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
        <xs:group name="g">
          <xs:sequence>
            <!--
            computed element expressions are not meaningful for the unused global element 'h'
            The paths go up two levels of elements, so these upward paths are reaching
            outside of the element h, which is not inside element rr.

            These paths are only meaningful when this group g is used inside the context
            of element rr
            -->
            <xs:element name="e" type="xs:int" dfdl:outputValueCalc='{ ../../rr/f }'/>
            <xs:element name="f" type="xs:int" dfdl:inputValueCalc='{ ../../rr/e }' />
          </xs:sequence>
        </xs:group>
        <xs:group name="x">
          <xs:sequence>
            <!--
            likewise, this group is never used, and so should not create an error, even
            if checkAllTopLevel is true
            -->
            <xs:element name="y" type="xs:int" dfdl:outputValueCalc='{ ../../dne }'/>
            <xs:element name="z" type="xs:int" dfdl:inputValueCalc='{ ../../dne }' />
          </xs:sequence>
        </xs:group>

  /**
   * The root element doesn't have any substructure that eventually uses the
   * group h, and so the IVC/OVC expressions within it which reach past the
   * root are not validated. This is because such expressions potentially only
   * make sense within the context of the root element. checkAllTopLevel does
   * not mean that all elements that aren't used must make sense as root
   * elements--even though "h" could be a root because it isn't referenced
   * anywhere, that doesn't necessarily mean we should require that expressions
   * inside it are not up-and-out expressions, as if it were a root.
   */
  @Test def testPathPastRootFromGlobalDef1(): Unit = {
    val compiler = Compiler().withCheckAllTopLevel(true)
    val pf = compiler.compileNode(testRootPathsSchema1, None, Some("r"))
    val isError = pf.isError
    assertFalse(isError)
  }

  @Test def testPathPastRootFromGlobalDef2(): Unit = {
    val compiler = Compiler().withCheckAllTopLevel(false)
    val pf = compiler.compileNode(testRootPathsSchema1, None, Some("r"))
    val isError = pf.isError
    assertFalse(isError)
  }

  @Test def testPathPastRootFromGlobalDef3(): Unit = {
    val compiler = Compiler().withCheckAllTopLevel(true)
    val pf = compiler.compileNode(testRootPathsSchema1, None, Some("h"))
    val isError = pf.isError
    assertTrue(isError)
    val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
    assertTrue(isError)
    assertTrue(
      msgs.toLowerCase.contains("Relative path '../../rr/f' past root element".toLowerCase)
    )
  }

  lazy val testRootPathsSchema2 = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
    //
    // note that the first element below will be the root element.
    //
    // The h element also uses group g. But there is no element rr. The
    // relative paths inside group g aren't meaningful in the context of
    // element h.
    //
    // However, h is not the root, so even with checkAllTopLevel true
    // we don't report errors about these meaningless paths, as we only
    // generate code for the root.
    //
    <xs:element name="r" type="tns:ct"/>
      ++
        restOfXML,
    useDefaultNamespace = false,
    elementFormDefault = "unqualified"
  )

  /**
   * Shows that so long as the root reaches the expressions
   * and they are meaningful in that context, even checkAllTopLevels
   * doesn't detect that element h can't be correct (as a root)
   *
   * Note that if element h were used as part of the substructure of
   * root, then if it has proper placement so the expressions are
   * meaningful, then that is correct. It's usage as a root is incorrect
   * but isn't entertained as a consideration by the schema compiler.
   */
  @Test def testPathPastRootFromGlobalDef4(): Unit = {
    val compiler = Compiler().withCheckAllTopLevel(true)
    val pf = compiler.compileNode(testRootPathsSchema2)
    val isError = pf.isError
    assertFalse(isError)
  }

  lazy val testRootPathsSchema3 = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
    //
    // note that the first element below will be the root element.
    //
    // The h element also uses group g. But there is no element rr. The
    // relative paths inside group g aren't meaningful in the context of
    // element h.
    //
    // However, h is not the root, so even with checkAllTopLevel true
    // we don't report errors about these meaningless paths, as we only
    // generate code for the root.
    //
    // But, h is checked. The meaningless part of it is detected.
    //
    <xs:element name="r" type="tns:ct"/>
      ++
        <xs:element name="h"><!-- uses the substructure, but expressions are past root-->
          <xs:complexType>
            <xs:sequence>
              <!-- The group g contains elements with paths that are past the root for
              this usage inside element h -->
              <xs:group ref="ex:g"/>
              <xs:sequence dfdl:hiddenGroupRef="tns:noSuchGroup"/>
            </xs:sequence>
          </xs:complexType>
      </xs:element>
        ++
        restOfXML2,
    useDefaultNamespace = false,
    elementFormDefault = "unqualified"
  )

  /**
   * This test shows that if checkAllTopLevel is false,
   * then the error inside global element h, (hiddenGroupRef to non-existing group)
   * is NOT detected, since it's not part of the schema reachable
   * from the root.
   *
   * Note that checkAllTopLevels false will NOT prevent
   * schema validation checks. But hiddenGroupRef isn't
   * a validity check. It's an explicit check done by Daffodil.
   */
  @Test def testCheckAllTopLevelWorksToIgnore(): Unit = {
    val compiler = Compiler().withCheckAllTopLevel(false)
    val pf = compiler.compileNode(testRootPathsSchema3)
    val isError = pf.isError
    assertFalse(isError)
  }

  /**
   * Test shows that even though it is not the root,
   * element h is checked by Daffodil, and the
   * erroneous hiddenGroupRef is detected.
   */
  @Test def testCheckAllTopLevelWorksToCheck(): Unit = {
    val compiler = Compiler().withCheckAllTopLevel(true)
    val pf = compiler.compileNode(testRootPathsSchema3)
    val isError = pf.isError
    val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
    println(msgs)
    assertTrue(isError)
    assertTrue(msgs.toLowerCase.contains("not found".toLowerCase))
    assertTrue(msgs.toLowerCase.contains("noSuchGroup".toLowerCase))
  }
}
