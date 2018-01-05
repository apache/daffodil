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

package org.apache.daffodil.dpath

import junit.framework.Assert.assertEquals
import org.junit.Test
import org.apache.daffodil.Implicits._; object INoWarn { ImplicitsSuppressUnusedImportWarning() }

class TestRounding {

  @Test def test_howToRoundFloatingPoint = {
    //
    // We ran into an issue where just passing the Float
    // to BigDecimal would change the value passed to it.
    // This would cause an issue with the round-half-to-even
    // function results
    //
    // This is a test to demonstrate that behavior and our
    // solution.
    //
    // We expect that a round-half-to-even function
    // applied to this value would yield 3.46 using a precision of 2.
    //
    val f: Float = 3.455f

    //val bd0 = BigDecimal(f) // 3.4549999237060547 Deprecated
    //val bd1 = BigDecimal.valueOf(f) // 3.4549999237060547 Deprecated
    val bd2 = BigDecimal(f.doubleValue()) // 3.4549999237060547
    val bd3 = BigDecimal(f.toString) // 3.455

    def round(value: BigDecimal, precision: Int): BigDecimal = {
      val rounded = value.setScale(precision, BigDecimal.RoundingMode.HALF_EVEN)
      rounded
    }

    val expected0_to_2 = BigDecimal(3.45) // Incorrect/unexpected result
    val expected3 = BigDecimal(3.46) // Correct result

    //val res0 = round(bd0, 2) Deprecated
    //val res1 = round(bd1, 2) Deprecated
    val res2 = round(bd2, 2)
    val res3 = round(bd3, 2)

    //assertEquals(0, res0.compare(expected0_to_2)) Deprecated
    //assertEquals(0, res1.compare(expected0_to_2)) Deprecated
    assertEquals(0, res2.compare(expected0_to_2))
    assertEquals(0, res3.compare(expected3))

  }

}
