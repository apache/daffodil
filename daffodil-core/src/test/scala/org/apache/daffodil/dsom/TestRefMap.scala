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
import org.apache.daffodil.Implicits._;
import org.apache.daffodil.schema.annotation.props.gen.{ YesNo, TextNumberRep, SeparatorPosition, Representation, OccursCountKind, NilKind, LengthKind, ChoiceLengthKind, ByteOrder, BinaryNumberRep, AlignmentUnits }
import org.apache.daffodil.schema.annotation.props.AlignmentType
import org.apache.daffodil.util.{ Misc, Logging }
import org.apache.daffodil.xml.XMLUtils
import junit.framework.Assert._
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.util._
import org.junit.Test
import org.apache.daffodil.schema.annotation.props.Found

class TestRefMap extends Logging {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testRefMapEmpty() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(1, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(1, numEntries)
    val numRefPairs = root.refPairsMap.size
    assertEquals(1, numRefPairs) // good proxy for schema size
    val rootDecl = root.referencedElement
    val rootRefPair = refMap.get(rootDecl.factory)
    assertTrue(rootRefPair.isDefined)
    val Seq((fromSSCD, refSpecs)) = rootRefPair.get
    assertEquals(root.shortSchemaComponentDesignator, fromSSCD)
    val Seq(RefSpec(rootRef, to, 1)) = refSpecs
    assertEquals(root, rootRef)
    assertEquals(rootDecl.factory, to)
  }

  @Test def testRefMapComplex1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="ex:ct"/>
      <xs:complexType name="ct">
        <xs:sequence>
          <xs:element name="e" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(4, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(2, numEntries)
    val numRefPairs = root.refPairsMap.size
    assertEquals(2, numRefPairs) // good proxy for schema size
    val rootDecl = root.referencedElement
    val rootRefPair = refMap.get(rootDecl.factory)
    assertTrue(rootRefPair.isDefined)
    val Seq((fromSSCD, refSpecs)) = rootRefPair.get
    assertEquals(root.shortSchemaComponentDesignator, fromSSCD)
    val ctDef = root.complexType.asInstanceOf[GlobalComplexTypeDef]
    val Some(Seq((edecl, ctRefSpecs))) = refMap.get(ctDef.factory)
    assertEquals(rootDecl.shortSchemaComponentDesignator, edecl)
  }

  @Test def testRefMapGroupRefSeq1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="ex:ct"/>
      <xs:complexType name="ct">
        <xs:group ref="ex:g"/>
      </xs:complexType>
      <xs:group name="g">
        <xs:sequence>
          <xs:element name="e" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:group>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(4, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(3, numEntries)
    val numRefPairs = root.refPairsMap.size
    assertEquals(3, numRefPairs) // good proxy for schema size
    val rootDecl = root.referencedElement
    val rootRefPair = refMap.get(rootDecl.factory)
    assertTrue(rootRefPair.isDefined)
    val Seq((fromSSCD, refSpecs)) = rootRefPair.get
    assertEquals(root.shortSchemaComponentDesignator, fromSSCD)
    val ctDef = root.complexType.asInstanceOf[GlobalComplexTypeDef]
    val Some(Seq((edecl, ctRefSpecs))) = refMap.get(ctDef.factory)
    assertEquals(rootDecl.shortSchemaComponentDesignator, edecl)
    val gref = rootDecl.complexType.modelGroup.asInstanceOf[SequenceGroupRef]
    val Seq((grefSSCD, gdRefSpecs)) = refMap.get(gref.groupDef.factory).get
    assertEquals(gref.shortSchemaComponentDesignator, grefSSCD)
    assertEquals(4, root.numComponents)
    assertEquals(4, root.numUniqueComponents)
  }

  @Test def testRefMapGroupRefChoice1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:group>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(5, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(3, numEntries)
    val numRefPairs = root.refPairsMap.size
    assertEquals(3, numRefPairs) // good proxy for schema size
    val rootDecl = root.referencedElement
    val rootRefPair = refMap.get(rootDecl.factory)
    assertTrue(rootRefPair.isDefined)
    val Seq((fromSSCD, refSpecs)) = rootRefPair.get
    assertEquals(root.shortSchemaComponentDesignator, fromSSCD)
    val ctDef = root.complexType.asInstanceOf[GlobalComplexTypeDef]
    val Some(Seq((edecl, ctRefSpecs))) = refMap.get(ctDef.factory)
    assertEquals(rootDecl.shortSchemaComponentDesignator, edecl)
    val gref = rootDecl.complexType.modelGroup.asInstanceOf[ChoiceGroupRef]
    val Seq((grefSSCD, gdRefSpecs)) = refMap.get(gref.groupDef.factory).get
    assertEquals(gref.shortSchemaComponentDesignator, grefSSCD)
    assertEquals(5, root.numComponents)
    assertEquals(5, root.numUniqueComponents)
  }

  @Test def testRefMapGroupRefNest1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
        <xs:sequence dfdl:hiddenGroupRef="ex:g1"/>
      </xs:complexType>
      <xs:group name="g1">
        <xs:choice>
          <xs:element name="e1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
          <xs:element name="f1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:choice>
      </xs:group>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(10, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(6, numEntries)
    val numRefPairs = root.refPairsMap.size
    assertEquals(6, numRefPairs) // good proxy for schema size
    val rootDecl = root.referencedElement
    val rootRefPair = refMap.get(rootDecl.factory)
    assertTrue(rootRefPair.isDefined)
    val Seq((fromSSCD, refSpecs)) = rootRefPair.get
    assertEquals(root.shortSchemaComponentDesignator, fromSSCD)
    val ctDef = root.complexType.asInstanceOf[GlobalComplexTypeDef]
    val Some(Seq((edecl, ctRefSpecs))) = refMap.get(ctDef.factory)
    assertEquals(rootDecl.shortSchemaComponentDesignator, edecl)
    val gref = rootDecl.complexType.modelGroup.asInstanceOf[ChoiceGroupRef]
    val Seq((grefSSCD, gdRefSpecs)) = refMap.get(gref.groupDef.factory).get
    assertEquals(gref.shortSchemaComponentDesignator, grefSSCD)
    assertEquals(10, root.numComponents)
    assertEquals(10, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="ex:ct"/>
      <xs:complexType name="ct">
        <xs:sequence>
          <xs:group ref="ex:g"/>
          <xs:group ref="ex:g"/>
        </xs:sequence>
      </xs:complexType>
      <xs:group name="g">
        <xs:choice>
          <xs:element ref="ex:r1"/>
          <xs:element ref="ex:r1"/>
        </xs:choice>
      </xs:group>
      <xs:element name="r1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element name="f1" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(25, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(4, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map { _._2.length }.sum
    assertEquals(6, numRefPairs) // good proxy for schema size
    val allGRefs = root.allGRefs
    assertEquals(2, allGRefs.size)
    val allERefsCount = root.allERefs.size
    assertEquals(2, allERefsCount)
    val allCTRefsCount = root.allCTRefs.size
    assertEquals(1, allCTRefsCount)
    assertEquals(25, root.numComponents)
    assertEquals(11, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion2() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="r" type="ex:ct"/>
      <xs:complexType name="ct">
        <xs:sequence>
          <xs:group ref="ex:g"/>
          <xs:group ref="ex:g"/>
        </xs:sequence>
      </xs:complexType>
      <xs:group name="g">
        <xs:choice>
          <xs:element ref="ex:r1"/>
          <xs:element ref="ex:r1"/>
        </xs:choice>
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
      </xs:element>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(57, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(5, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map { _._2.length }.sum
    assertEquals(8, numRefPairs)
    assertEquals(57, root.numComponents)
    assertEquals(15, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion3() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:element>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(61, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(4, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map { _._2.length }.sum
    assertEquals(7, numRefPairs)
    assertEquals(61, root.numComponents)
    assertEquals(17, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion4() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:element>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(125, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(5, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map { _._2.length }.sum
    assertEquals(9, numRefPairs)
    assertEquals(125, root.numComponents)
    assertEquals(21, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion5() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:element>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(253, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(6, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map { _._2.length }.sum
    assertEquals(11, numRefPairs)
    assertEquals(253, root.numComponents)
    assertEquals(25, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion6() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:element>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(509, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(7, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map { _._2.length }.sum
    assertEquals(13, numRefPairs)
    assertEquals(509, root.numComponents)
    assertEquals(29, root.numUniqueComponents)
  }

  @Test def testRefMapExplosion7() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:element>)
    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val root = sset.root
    val comps = root.allComponents
    assertEquals(1021, comps.length)
    val refMap = root.refMap
    val numEntries = refMap.size
    assertEquals(8, numEntries)
    val refPairsMap = root.refPairsMap.toSeq
    val numRefPairs = refPairsMap.map { _._2.length }.sum
    assertEquals(15, numRefPairs)
    assertEquals(1021, root.numComponents)
    assertEquals(33, root.numUniqueComponents)
  }
}
