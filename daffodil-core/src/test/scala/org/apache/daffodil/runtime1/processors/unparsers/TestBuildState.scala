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

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert.*
import org.junit.Test

/**
 * Validates BuildState in isolation: confirms it surfaces the same
 * event sequence a real UStateMain would, for a separated schema.
 */
class TestBuildState {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testBuildStateSurfacesCorrectEventSequence(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      {
        <dfdl:format ref="tns:GeneralFormat"
          encoding="ascii"
          lengthUnits="bytes"
          outputNewLine="%CR;%LF;"/>
        <dfdl:defineVariable name="marker" type="xs:string" defaultValue="M"/>
      },
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="name" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="age" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="city" type="xs:string" dfdl:lengthKind="delimited"/>
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

    val inputter = TestUtils.newInitializedInputter(infoset, dp)
    // initialize() pushes the root TRD as its last step - the same
    // setup a real unparse's invariant check relies on.

    val sharedCtx =
      UnparseSharedContextTestFixture.build(dp, prefetchLimit = 100)()
    val buildState = new BuildState(inputter, sharedCtx, Nil, false)

    // Drives through the ACTUAL Builder recursion, not hand-driven
    // advance() calls, since next-element resolution depends on the same
    // TRD push/pop dance ElementBuilder.build performs. This schema's
    // separator never reaches BuildState at all: the Builder tree skips
    // straight past the delimiter-stack wrapper unparser entirely.
    dp.ssrd.builder.get.build(buildState)

    assertEquals(5L, sharedCtx.currentLead) // row, name, age, city, marker

    val rootNode = inputter.documentElement.child(0).asComplex
    assertEquals(4, rootNode.numChildren)
    assertEquals("name", rootNode.child(0).erd.name)
    assertEquals("age", rootNode.child(1).erd.name)
    assertEquals("city", rootNode.child(2).erd.name)
    assertEquals("marker", rootNode.child(3).erd.name)
  }
}
