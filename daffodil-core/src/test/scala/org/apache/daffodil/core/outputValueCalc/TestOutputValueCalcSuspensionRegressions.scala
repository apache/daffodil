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

package org.apache.daffodil.core.outputValueCalc

import java.nio.channels.Channels

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.processors.DataProcessor

import org.junit.Assert.*
import org.junit.Test

/**
 * Regression guards for evalSuspensionQueue's buildResolvableOnly
 * restructuring: single-pass unparse doesn't use that mode, but must
 * still resolve these scenarios exactly as before.
 */
class TestOutputValueCalcSuspensionRegressions {

  private val example = XMLUtils.EXAMPLE_NAMESPACE

  private val readsOvcRecordCount = 3

  private def readsOvcSchema = {
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textPadKind="padChar"
        textStringJustification="left"
        textStringPadCharacter="%SP;"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
            <xs:element name="header" dfdl:lengthKind="implicit">
              <xs:complexType>
                <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
                  <xs:element name="lenA" type="xs:int"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"
                    dfdl:outputValueCalc="{ ../lenB }"/>
                  <xs:element name="lenB" type="xs:int"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"
                    dfdl:outputValueCalc={
        s"{ dfdl:valueLength(../../record[$readsOvcRecordCount]/data, 'bytes') }"
      }/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="record" minOccurs={readsOvcRecordCount.toString}
              maxOccurs={readsOvcRecordCount.toString}
              dfdl:lengthKind="implicit">
              <xs:complexType>
                <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
                  <xs:element name="tag" type="xs:string"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"/>
                  <xs:element name="data" type="xs:string"
                    dfdl:lengthKind="explicit"
                    dfdl:length="8"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
  }

  private def readsOvcInfoset = {
    val recordXml = (1 to readsOvcRecordCount).map { i =>
      <record>
        <tag>{f"T$i%03d"}</tag>
        <data>{f"data$i%04d"}</data>
      </record>
    }
    <ex:root xmlns:ex={example}>
      <header/>
      {recordXml}
    </ex:root>
  }

  @Test def testOvcReadsOvcResolvesViaFinalDrain(): Unit = {
    val compiler = Compiler().withTunables(Map("unparseSuspensionWaitOld" -> "1000000"))
    val pf = compiler.compileNode(readsOvcSchema)
    if (pf.isError) fail(pf.getDiagnostics.toString)
    val dp = pf.onPath("/").asInstanceOf[DataProcessor]
    if (dp.isError) fail(dp.getDiagnostics.toString)

    val outputStream = new java.io.ByteArrayOutputStream()
    val out = Channels.newChannel(outputStream)
    val inputter = new ScalaXMLInfosetInputter(readsOvcInfoset)
    val actual = dp.unparse(inputter, out)
    out.close()
    assertFalse(actual.getDiagnostics.toString, actual.isProcessingError)

    val unparsed = outputStream.toString
    // lenA reads lenB's own value (not its length): both should equal
    // record[readsOvcRecordCount]/data's length, 8.
    assertEquals("   8   8", unparsed.substring(0, 8))

    val recordsPart = unparsed.substring(8)
    val expectedRecords =
      (1 to readsOvcRecordCount).map(i => f"T$i%03d" + f"data$i%04d").mkString
    assertEquals(expectedRecords, recordsPart)
  }

  private val pendingRetryRecordCount = 12

  private def pendingRetrySchema = {
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textPadKind="padChar"
        textStringJustification="left"
        textStringPadCharacter="%SP;"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
            <xs:element name="header" dfdl:lengthKind="implicit">
              <xs:complexType>
                <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
                  <xs:element name="len1" type="xs:int"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"
                    dfdl:outputValueCalc={
        s"{ dfdl:valueLength(../../record[4]/data, 'bytes') }"
      }/>
                  <xs:element name="len2" type="xs:int"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"
                    dfdl:outputValueCalc={
        s"{ dfdl:valueLength(../../record[8]/data, 'bytes') }"
      }/>
                  <xs:element name="len3" type="xs:int"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"
                    dfdl:outputValueCalc={
        s"{ dfdl:valueLength(../../record[$pendingRetryRecordCount]/data, 'bytes') }"
      }/>
                  <xs:element name="len4" type="xs:int"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"
                    dfdl:outputValueCalc={
        s"{ dfdl:valueLength(../../record[$pendingRetryRecordCount]/data, 'bytes') }"
      }/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="record" minOccurs={pendingRetryRecordCount.toString}
              maxOccurs={pendingRetryRecordCount.toString}
              dfdl:lengthKind="implicit">
              <xs:complexType>
                <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
                  <xs:element name="tag" type="xs:string"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"/>
                  <xs:element name="data" type="xs:string"
                    dfdl:lengthKind="explicit"
                    dfdl:length="8"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
  }

  private def pendingRetryInfoset = {
    val recordXml = (1 to pendingRetryRecordCount).map { i =>
      <record>
        <tag>{f"T$i%03d"}</tag>
        <data>{f"data$i%04d"}</data>
      </record>
    }
    <ex:root xmlns:ex={example}>
      <header/>
      {recordXml}
    </ex:root>
  }

  // Every len field is a fixed-length "data" element (8 bytes), so all
  // four should compute to 8 regardless of which record they target.
  private def pendingRetryExpectedOutput = {
    val header = "   8   8   8   8"
    val records = (1 to pendingRetryRecordCount).map(i => f"T$i%03d" + f"data$i%04d").mkString
    header + records
  }

  @Test def testManyForceRetryCyclesResolveCorrectly(): Unit = {
    TestUtils.testUnparsing(
      pendingRetrySchema,
      pendingRetryInfoset,
      pendingRetryExpectedOutput,
      tunables = Map("unparseSuspensionWaitOld" -> "1", "unparseSuspensionWaitYoung" -> "1")
    )
  }

  @Test def testManyForceRetryCyclesDefaultTunablesStillMatch(): Unit = {
    // Same schema at default tunables: confirms the pass above isn't
    // vacuous (e.g. a schema mistake unrelated to the tunable).
    TestUtils.testUnparsing(pendingRetrySchema, pendingRetryInfoset, pendingRetryExpectedOutput)
  }
}
