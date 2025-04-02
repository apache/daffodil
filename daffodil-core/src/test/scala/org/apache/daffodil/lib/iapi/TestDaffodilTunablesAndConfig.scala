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

package org.apache.daffodil.lib.iapi

import org.apache.daffodil.lib.util.Misc

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class TestDaffodilTunablesAndConfig {

  @Test
  def testDaffodilConfig1(): Unit = {
    val uri = Misc.getRequiredResource("test/configExample.cfg")
    val cfg = DaffodilConfig.fromURI(uri)
    val Seq(binding) = cfg.externalVariableBindings
    assertEquals("byteOrder", binding.varQName.local)

    val Seq((k1, v1)) = cfg.tunablesMap.toSeq
    assertEquals("suppressSchemaDefinitionWarnings", k1)
    assertTrue(v1.contains("facetExplicitLengthOutOfRange"))
    assertTrue(v1.contains("encodingErrorPolicyError"))

    val tunables = DaffodilTunables(cfg.tunablesMap)
    val warnings = tunables.suppressSchemaDefinitionWarnings
    assertEquals(2, warnings.length)
    assertTrue(warnings.contains(WarnID.FacetExplicitLengthOutOfRange))
    assertTrue(warnings.contains(WarnID.EncodingErrorPolicyError))
  }

  @Test
  def testDaffodilConfigBad(): Unit = {
    val uri = Misc.getRequiredResource("test/configBad.txt")
    assertThrows(classOf[DaffodilConfigException], () => DaffodilConfig.fromURI(uri))
  }
}
