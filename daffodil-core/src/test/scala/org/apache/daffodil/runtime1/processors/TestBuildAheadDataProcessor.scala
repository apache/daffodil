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

package org.apache.daffodil.runtime1.processors

import java.io.ByteArrayOutputStream
import scala.xml.Node

import org.apache.daffodil.api
import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter

import org.junit.Assert.*
import org.junit.Test

/**
 * Checks that a compiled schema always carries an infoset builder, that the
 * infosetBuilderMode tunable can be changed on a compiled DataProcessor, and how
 * DataProcessor.unparse behaves when it cannot start. Whether unparse output is the same in both
 * modes is covered by buildAhead.tdml, run with DAFFODIL_TDML_TUNABLES set to
 * each value of infosetBuilderMode.
 */
class TestBuildAheadDataProcessor {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  private def unparseToBytes(dp: DataProcessor, infosetXML: Node): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val res = dp.unparse(new ScalaXMLInfosetInputter(infosetXML), out)
    assertFalse(res.getDiagnostics.toString, res.isError)
    out.toByteArray
  }

  // A setup failure (inputter never produces StartDocument) must yield
  // a failed UnparseResult, not an NPE from a null error-path state.
  @Test def testMalformedInfosetInputterGetsCleanErrorNotNPE(): Unit = {
    // Needs build ahead in use (the tunable on), or DataProcessor.unparse takes the
    // event-driven path and unparseBuildAhead (the method under test) would never run.
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="actual" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2"/>
            <xs:element name="computed" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="2"
              dfdl:outputValueCalc="{ ../actual + 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val dp = Compiler()
      .withTunable("infosetBuilderMode", "buildAhead")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]

    // hasNext() = false immediately means initialize()'s
    // "!delegate.hasNext" check fires straight away, before any actual
    // infoset event is produced; exactly the "never starts with
    // StartDocument" failure this guards.
    val neverStartsInputter = new api.infoset.InfosetInputter {
      override def getEventType() = null
      override def getLocalName() = null
      override def getNamespaceURI() = null
      override def getSimpleText(
        primType: org.apache.daffodil.runtime1.dpath.NodeInfo.Kind,
        runtimeProperties: java.util.Map[String, String]
      ) = null
      override def isNilled(): java.lang.Boolean = null
      override def hasNext() = false
      override def next(): Unit = ()
      override def fini(): Unit = ()
    }

    val out = new ByteArrayOutputStream()
    val res = dp.unparse(neverStartsInputter, out)

    assertTrue("expected a failed UnparseResult, not a successful one", res.isError)
    assertTrue(
      res.getDiagnostics.get(0).getMessage.contains("does not start with StartDocument")
    )
  }

  // A schema with no OVC at all still gets a builder.
  @Test def testSchemaWithNoOVCAtAllStillGetsBuilder(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val dp = Compiler()
      .withTunable("infosetBuilderMode", "buildAhead")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    assertFalse(dp.ssrd.builder.isEmpty)
  }

  // A schema where every OVC is resolvable-without-writing gets a builder:
  // the common case.
  @Test def testSchemaWithOnlyResolvableOVCGetsBuilder(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="computed" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="3"
              dfdl:outputValueCalc="{ ../actual + 1 }"/>
            <xs:element name="actual" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val dp = Compiler()
      .withTunable("infosetBuilderMode", "buildAhead")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    assertFalse(dp.ssrd.builder.isEmpty)
  }

  // The builder exists even when the tunable is off at compile time, and the
  // same compiled processor unparses identically with the tunable switched
  // either way afterward.
  @Test def testTunableCanChangeAfterCompile(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="computed" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="3"
              dfdl:outputValueCalc="{ ../actual + 1 }"/>
            <xs:element name="actual" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val dp = Compiler()
      .withTunable("infosetBuilderMode", "eventDriven")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    assertFalse(dp.ssrd.builder.isEmpty)

    val infoset = <ex:row xmlns:ex={example}><actual>7</actual></ex:row>
    val eventDriven = unparseToBytes(dp, infoset)
    val buildAhead = unparseToBytes(
      dp.copy(tunables = dp.tunables.withTunable("infosetBuilderMode", "buildAhead")),
      infoset
    )
    assertArrayEquals(eventDriven, buildAhead)
    assertArrayEquals(
      eventDriven,
      unparseToBytes(
        dp.copy(tunables = dp.tunables.withTunable("infosetBuilderMode", "eventDriven")),
        infoset
      )
    )
  }

  // Confirms the builder survives a save/reload round trip.
  @Test def testBuilderSurvivesSaveReload(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="len" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="2"
              dfdl:outputValueCalc="{ dfdl:valueLength(../data, 'bytes') }"/>
            <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val dp = Compiler()
      .withTunable("infosetBuilderMode", "buildAhead")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    assertFalse(dp.ssrd.builder.isEmpty)

    val os = new ByteArrayOutputStream()
    dp.save(java.nio.channels.Channels.newChannel(os))
    val reloadedDp = Compiler()
      .reload(new java.io.ByteArrayInputStream(os.toByteArray))
      .asInstanceOf[DataProcessor]
    assertFalse(reloadedDp.ssrd.builder.isEmpty)

    val infoset = <ex:row xmlns:ex={example}><data>xyz</data></ex:row>
    assertArrayEquals(
      unparseToBytes(dp, infoset),
      unparseToBytes(reloadedDp, infoset)
    )
  }
}
