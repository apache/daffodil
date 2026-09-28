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
 * Proves build pauses to let write catch up: with a small prefetchLimit,
 * the lead counter stays bounded mid-recursion, not equal to the full count.
 */
class TestBoundedPrefetch {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testBuildLeadStaysBoundedDuringRecursion(): Unit = {
    val numItems = 40
    val prefetchLimit = 3L

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
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"
              dfdl:lengthKind="delimited"
              dfdl:occursCountKind="implicit"/>
            <!-- A variable reference: no element references
                 (canResolveWithoutWriting) and not a compile-time
                 constant (unlike a literal, which the compiler folds to
                 isConstant=true) - the only kind hasAnyPrefetchBeneficialOVC
                 counts, so builder actually gets constructed here. -->
            <xs:element name="marker" type="xs:string" dfdl:lengthKind="delimited"
              dfdl:outputValueCalc="{ $ex:marker }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    val items = (0 until numItems).map(i => <item>{s"i$i"}</item>)
    val infoset =
      <ex:row xmlns:ex={example}>
        {items}
      </ex:row>
    val expectedBytes = (0 until numItems).map(i => s"i$i").mkString(",") + ",M"

    val dp = TestUtils.compileForUnparse(
      sch,
      Map("releaseUnneededInfoset" -> "false", "useBuildWritePrefetch" -> "true")
    )

    val buildInputter = TestUtils.newInitializedInputter(infoset, dp)

    val sharedCtx =
      UnparseSharedContextTestFixture.build(dp, prefetchLimit)()

    val walkerOut = new ByteArrayOutputStream()
    val writeInputter = TestUtils.newInitializedInputter(infoset, dp)
    val writeState = UState.createInitialUState(walkerOut, dp, writeInputter, false)
    writeState.setSharedContext(sharedCtx)
    writeState.getDataOutputStream.setPriorBitOrder(dp.ssrd.elementRuntimeData.defaultBitOrder)

    val rootUnparser = dp.ssrd.unparser.asInstanceOf[ElementUnparserBase]
    UnparseSharedContextTestFixture.wireCoroutines(
      sharedCtx,
      buildInputter.documentElement,
      rootUnparser,
      writeState
    )

    val buildState = new BuildState(buildInputter, sharedCtx, Nil, false)

    dp.ssrd.builder.get.build(buildState)

    // See class doc above for why this proves interleaving; numItems + 2
    // (row + all items + marker) is what currentLead would equal here if
    // resumeWrite never fired mid-recursion.
    assertTrue(
      s"expected lead close to prefetchLimit=$prefetchLimit after build, but was ${sharedCtx.currentLead} " +
        s"(numItems=$numItems); build did not actually pause for write",
      sharedCtx.currentLead <= prefetchLimit + 1
    )
    assertTrue(
      "expected build to have gotten ahead of write by at least one node",
      sharedCtx.currentLead > 0
    )

    // Drain the rest and confirm the output is byte-for-byte correct
    // despite having been produced across many separate resumeWrite calls
    // rather than a single one-shot write pass.
    val finalSignal = sharedCtx.resumeWrite(BuildFinished)
    finalSignal match {
      case WriteDone(Some(t)) => throw t
      case WriteDone(None) => // continue below
      case other => fail(s"unexpected final signal: $other")
    }
    writeState.evalSuspensions(isFinal = true)
    writeState.getDataOutputStream.setFinished(writeState)

    assertEquals(expectedBytes, new String(walkerOut.toByteArray, StandardCharsets.US_ASCII))
  }
}
