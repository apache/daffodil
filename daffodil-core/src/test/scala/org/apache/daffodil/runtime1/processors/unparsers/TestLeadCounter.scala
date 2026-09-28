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
import org.apache.daffodil.unparsers.runtime1.ElementUnparserBase

import org.junit.Assert.*
import org.junit.Test

/**
 * Validates the shared build/write lead counter end to end: build
 * increments and write decrements against the same shared instance.
 */
class TestLeadCounter {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testLeadCounterIncrementsOnBuildAndDecrementsOnWrite(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      {
        <dfdl:format ref="tns:GeneralFormat"
          encoding="ascii"
          lengthUnits="bytes"/>
        <dfdl:defineVariable name="marker" type="xs:string" defaultValue="M"/>
      },
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="name" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="age" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="2"/>
            <xs:element name="city" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="6"/>
            <!-- A variable reference: no element references
                 (canResolveWithoutWriting) and not a compile-time
                 constant (unlike a literal, which the compiler folds to
                 isConstant=true) - the only kind hasAnyPrefetchBeneficialOVC
                 counts, so builder actually gets constructed here. -->
            <xs:element name="marker" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"
              dfdl:outputValueCalc="{ $ex:marker }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    // Fixed dfdl:length is safe here because this tree comes from
    // BuildState, which never runs content-writing (including
    // CaptureStartOfContentLengthUnparser).
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

    // Build phase: drive BuildState through the actual recursion, incrementing
    // the shared lead counter via the actual unparseBegin hookup, as in
    // BuildStateTest.
    val buildInputter = TestUtils.newInitializedInputter(infoset, dp)

    val sharedCtx =
      UnparseSharedContextTestFixture.build(dp, prefetchLimit = 100)()
    val buildState = new BuildState(buildInputter, sharedCtx, Nil, false)

    assertEquals(0L, sharedCtx.currentLead)
    dp.ssrd.builder.get.build(buildState)

    // row itself, name, age, city, marker = 5 elements total, each
    // incrementing once via unparseBegin's actual hookup.
    assertEquals(5L, sharedCtx.currentLead)

    // Build was driven directly (no coroutine handoff), so sharedCtx can't
    // know build is done; tell it so write's awaitChild calls take the
    // post-BuildFinished (suspension-retry) path instead of resuming a
    // coroutine that was never set up.
    sharedCtx.observeBuildSignal(BuildFinished)

    // Write phase: write the SAME already-built tree
    // (buildInputter.documentElement) against the SAME sharedCtx,
    // decrementing the lead counter as it goes.
    val walkerOut = new ByteArrayOutputStream()
    val writeInputter = TestUtils.newInitializedInputter(infoset, dp)
    val writeState = UState.createInitialUState(walkerOut, dp, writeInputter, false)
    writeState.setSharedContext(sharedCtx)
    writeState.getDataOutputStream.setPriorBitOrder(dp.ssrd.elementRuntimeData.defaultBitOrder)

    val rootUnparser = dp.ssrd.unparser.asInstanceOf[ElementUnparserBase]
    val rootNode = sharedCtx.awaitChild(buildInputter.documentElement, 0)
    rootUnparser.writeContent(rootNode, writeState)
    writeState.getDataOutputStream.setFinished(writeState)

    assertEquals("Alice30BostonM", new String(walkerOut.toByteArray, StandardCharsets.US_ASCII))

    // Write decremented once per element too, so the counter is back to 0:
    // build and write agree on how many nodes exist, coordinated through
    // the shared UnparseSharedContext.
    assertEquals(0L, sharedCtx.currentLead)
  }
}
