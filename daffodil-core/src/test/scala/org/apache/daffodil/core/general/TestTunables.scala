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

package org.apache.daffodil.core.general

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.Implicits.ImplicitsSuppressUnusedImportWarning

import org.junit.Test; object INoWarnDSOM1 { ImplicitsSuppressUnusedImportWarning() }

import org.apache.daffodil.core.util.Fakes
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert.assertEquals

class TestTunables {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  // The below is lazy for a reason.
  // It defers evaluation until used. This is nice because suppose there is a bug
  // in the Fakes stuff. Then you want tests that use that to fail. But lots of
  // these tests don't use this. If you make this an eager val, then if there
  // is any problem in the Fakes, the whole class can't be constructed, and None
  // of the tests will run. Lazy lets this class be constructed no matter what.
  lazy val dummyGroupRef = Fakes.fakeGroupRef

  @Test def testTunableCopy(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>
    )

    val c = Compiler()
    val c1 = c.withTunable("maxSkipLengthInBytes", "1026")
    val pf1 = c1.compileNode(testSchema)

    val c2 = c.withTunable("maxSkipLengthInBytes", "2048")
    val pf2 = c2.compileNode(testSchema)

    val dp1 = pf1.onPath("/")
    var dp2 = pf2.onPath("/")

    val t1 = dp1.tunables
    val t2 = dp2.tunables

    /* Set tunable at run-time via data processor */
    dp2 = dp2.withTunable("maxSkipLengthInBytes", "50")

    val t3 = dp2.tunables // modified tunables at 'run-time'
    val t4 = dp1.tunables // obtain first data processor to see if anything changed

    assertEquals(1026, t1.maxSkipLengthInBytes) // initial compiler-set value
    assertEquals(2048, t2.maxSkipLengthInBytes) // overwrite of compiler-set value
    assertEquals(50, t3.maxSkipLengthInBytes) // data-processor-set value
    //
    //  initial compiler-set value not changed
    //  for first data processor object.
    assertEquals(1026, t4.maxSkipLengthInBytes)
  }

  @Test def testTunableSuppressionListCopying(): Unit = {
    val t1 = DaffodilTunables("suppressSchemaDefinitionWarnings", "escapeSchemeRefUndefined")
    val t2 = DaffodilTunables("suppressSchemaDefinitionWarnings", "all")

    val w1 = t1.suppressSchemaDefinitionWarnings.mkString(",")
    val w2 = t2.suppressSchemaDefinitionWarnings.mkString(",")
    assertEquals(true, w1.contains("escapeSchemeRefUndefined"))
    assertEquals(true, w2.contains("all"))

    val w3 = t1.suppressSchemaDefinitionWarnings.mkString(",")
    val w4 = t1.copy().suppressSchemaDefinitionWarnings.mkString(",")
    assertEquals(true, w3.contains("escapeSchemeRefUndefined"))
    assertEquals(true, w4.contains("escapeSchemeRefUndefined"))
  }
}
