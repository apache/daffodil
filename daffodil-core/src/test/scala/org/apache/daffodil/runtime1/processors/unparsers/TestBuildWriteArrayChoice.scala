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

package org.apache.daffodil.runtime1.processors.unparsers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset.DIArray
import org.apache.daffodil.unparsers.runtime1.ElementUnparserBase

import org.junit.Assert.*
import org.junit.Test

/**
 * Validates write-side dispatch against a single-pass tree, and a
 * standalone BuildState run, for both scalar and array/choice content.
 */
class TestBuildWriteArrayChoice {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testWriteWalkerMatchesActualUnparse(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        outputNewLine="%CR;%LF;"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="name" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="age" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="city" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    val infoset =
      <ex:row xmlns:ex={example}>
        <name>Alice</name>
        <age>30</age>
        <city>Boston</city>
      </ex:row>

    val dp = TestUtils.compileForUnparse(
      sch,
      Map("releaseUnneededInfoset" -> "false", "useBuildWritePrefetch" -> "true")
    )

    val (singlePassBytes, walkerBytes) =
      TestUtils.getSinglePassAndWriteContentBytes(dp, infoset)

    assertArrayEquals(singlePassBytes, walkerBytes)
  }

  @Test def testArrayAndChoiceWriteContentMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="header" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"
              dfdl:lengthKind="delimited"
              dfdl:occursCountKind="implicit"/>
            <xs:choice>
              <xs:element name="typeA" type="xs:string" dfdl:lengthKind="delimited"/>
              <xs:element name="typeB" type="xs:string" dfdl:lengthKind="delimited"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    // Three "item" occurrences exercise the array loop; typeB (not typeA)
    // exercises actual choice resolution. lengthKind=delimited (not
    // explicit) avoids double-firing CaptureStartOfContentLengthUnparser's
    // non-idempotent marker, since this tree is reused from a completed unparse.
    val infoset =
      <ex:row xmlns:ex={example}>
        <header>H</header>
        <item>a</item>
        <item>b</item>
        <item>c</item>
        <typeB>X</typeB>
      </ex:row>

    val dp = TestUtils.compileForUnparse(
      sch,
      Map("releaseUnneededInfoset" -> "false", "useBuildWritePrefetch" -> "true")
    )

    val (singlePassBytes, walkerBytes) =
      TestUtils.getSinglePassAndWriteContentBytes(dp, infoset)

    assertEquals("H,a,b,c,X", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, walkerBytes)
  }

  // Standalone-build regression: drives BuildState directly, then feeds
  // its tree into write's writeContent (end-to-end build-then-write).
  @Test def testStandaloneBuildStateNavigatesArrayChoiceSeparator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="header" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"
              dfdl:lengthKind="delimited"
              dfdl:occursCountKind="implicit"/>
            <xs:choice>
              <xs:element name="typeA" type="xs:string" dfdl:lengthKind="delimited"/>
              <xs:element name="typeB" type="xs:string" dfdl:lengthKind="delimited"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    val infoset =
      <ex:row xmlns:ex={example}>
        <header>H</header>
        <item>a</item>
        <item>b</item>
        <item>c</item>
        <typeB>X</typeB>
      </ex:row>

    val dp = TestUtils.compileForUnparse(
      sch,
      Map("releaseUnneededInfoset" -> "false", "useBuildWritePrefetch" -> "true")
    )

    // Build phase: standalone BuildState drives the actual Unparser recursion,
    // navigating past the sequence's separator and through the
    // array/choice content, purely to build the tree.
    val buildInputter = TestUtils.newInitializedInputter(infoset, dp)

    val sharedCtx =
      UnparseSharedContextTestFixture.build(dp, prefetchLimit = 100)()
    val buildState = new BuildState(buildInputter, sharedCtx, Nil, false)

    dp.ssrd.builder.get.build(buildState)

    // row, header, item x3, typeB = 6 elements total.
    assertEquals(6L, sharedCtx.currentLead)

    val rootNode = buildInputter.documentElement.child(0).asComplex
    assertEquals(3, rootNode.numChildren)
    assertEquals("header", rootNode.child(0).erd.name)
    assertEquals("item", rootNode.child(1).erd.name)
    assertEquals(3, rootNode.child(1).asInstanceOf[DIArray].numChildren)
    assertEquals("typeB", rootNode.child(2).erd.name)

    // Build was driven directly (no coroutine handoff), so sharedCtx can't
    // know build is done; tell it so write's awaitChild call takes the
    // post-BuildFinished (suspension-retry) path instead of resuming a
    // coroutine that was never set up.
    sharedCtx.observeBuildSignal(BuildFinished)

    // Write phase: write the tree BuildState just constructed, confirming
    // it's a usable, fully-built tree, not just a navigation exercise.
    val walkerOut = new ByteArrayOutputStream()
    val writeInputter = TestUtils.newInitializedInputter(infoset, dp)
    val writeState = UState.createInitialUState(walkerOut, dp, writeInputter, false)
    writeState.setSharedContext(sharedCtx)
    writeState.getDataOutputStream.setPriorBitOrder(dp.ssrd.elementRuntimeData.defaultBitOrder)

    val rootUnparser = dp.ssrd.unparser.asInstanceOf[ElementUnparserBase]
    val rootWriteNode = sharedCtx.awaitChild(buildInputter.documentElement, 0)
    rootUnparser.writeContent(rootWriteNode, writeState)
    // The separator (default separatorSuppressionPolicy "anyEmpty") is
    // written speculatively via a suspension deciding, once known, if the
    // region it precedes is zero-length; this drains that chain before the
    // DOS is finalized, mirroring DataProcessor's finishWriteSide.
    writeState.evalSuspensions(isFinal = true)
    writeState.getDataOutputStream.setFinished(writeState)

    assertEquals("H,a,b,c,X", new String(walkerOut.toByteArray, StandardCharsets.US_ASCII))
  }
}
