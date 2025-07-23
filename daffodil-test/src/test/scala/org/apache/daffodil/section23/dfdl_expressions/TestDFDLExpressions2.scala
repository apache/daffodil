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

package org.apache.daffodil.section23.dfdl_expressions

import org.apache.daffodil.core.util.TestUtils.intercept
import org.apache.daffodil.lib.exceptions.UsageException

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class TestHexBinaryComp {

  @Test def test_hexBinaryComparison_06(): Unit = {
    import org.apache.daffodil.runtime1.dpath.ComparisonOps
    import org.apache.daffodil.runtime1.dpath.NodeInfo
    val compOps = ComparisonOps.forType(NodeInfo.HexBinary)

    val ba1 = Array[Byte](0xde.toByte, 0xad.toByte)
    val ba2 = Array[Byte](0xbe.toByte, 0xef.toByte)

    val eEQ = compOps.eq.operate(ba1, ba2).getBoolean
    val eNE = compOps.ne.operate(ba1, ba2).getBoolean
    val eLT = intercept[UsageException] {
      compOps.lt.operate(ba1, ba2)
    }
    val eLE = intercept[UsageException] {
      compOps.le.operate(ba1, ba2)
    }
    val eGT = intercept[UsageException] {
      compOps.gt.operate(ba1, ba2)
    }
    val eGE = intercept[UsageException] {
      compOps.ge.operate(ba1, ba2)
    }
    assertFalse(eEQ)
    assertTrue(eNE)
    assertTrue(eLT.getMessage.contains("Unsupported operation LT"))
    assertTrue(eLE.getMessage.contains("Unsupported operation LE"))
    assertTrue(eGT.getMessage.contains("Unsupported operation GT"))
    assertTrue(eGE.getMessage.contains("Unsupported operation GE"))
  }
}
