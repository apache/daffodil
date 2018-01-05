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

package org.apache.daffodil.usertests
import org.junit.Test
import org.apache.daffodil.util._
import org.apache.daffodil.tdml.DFDLTestSuite

class TestLeaks {
  val testDir = "/org/apache/daffodil/usertests/"
  val aa = testDir + "leaks.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  def volumeRunner(tc: => Unit) {
    1 to 100 foreach { _ =>
      // println("")
      1 to 100 foreach { _ =>
        // print(".")
        1 to 100 foreach { _ =>
          System.gc()
          Thread.sleep(100)
          tc
        }
      }
    }
  }

  @Test def test_leak1() { runner.runOneTest("leak1") }
  @Test def test_leak2() { runner.runOneTest("leak2") }
  @Test def test_leak3() { runner.runOneTest("leak3") }
  @Test def test_leak4() { runner.runOneTest("leak4") }

  @Test def testLeak1() { volumeRunner(test_leak1()) } // no leak - correct, but no parsing (input value calc)

  @Test def testLeak2() { volumeRunner(test_leak2()) } // no leak - runtime error

  @Test def testLeak3() { volumeRunner(test_leak3()) } // no leak - compilation error
  @Test def testLeak4() { volumeRunner(test_leak4()) } // leaks horribly

}
