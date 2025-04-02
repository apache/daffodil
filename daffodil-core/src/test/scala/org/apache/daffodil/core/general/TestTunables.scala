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

import org.apache.daffodil.lib.Implicits.ImplicitsSuppressUnusedImportWarning

import org.junit.Test; object INoWarnDSOM1 { ImplicitsSuppressUnusedImportWarning() }

import org.apache.daffodil.core.util.Fakes
import org.apache.daffodil.lib.iapi.DaffodilTunables
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
