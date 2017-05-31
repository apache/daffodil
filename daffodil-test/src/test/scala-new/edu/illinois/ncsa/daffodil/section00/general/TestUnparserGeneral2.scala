/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.illinois.ncsa.daffodil.section00.general

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestUnparserGeneral2 {
  val testDir = "/edu/illinois/ncsa/daffodil/section00/general/"
  val runner2 = Runner(testDir, "testUnparserBitOrderOVC.tdml")

  @AfterClass def shutDown {
    runner2.reset
  }
}

class TestUnparserGeneral2 {

  import TestUnparserGeneral2._

  // test for DAFFODIL-1843
  @Test def test_bitOrderOVC1() = { runner2.runOneTest("bitOrderOVC1") }
}
