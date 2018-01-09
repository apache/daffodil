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

package edu.illinois.ncsa.daffodil.tdml

import org.junit.Test
import org.junit.AfterClass

object TestTDMLRunnerWarnings {
  val runner = Runner("/test/tdml/", "testWarnings.tdml")

  @AfterClass def shutDown {
    runner.reset
  }
}

class TestTDMLRunnerWarnings {
  import TestTDMLRunnerWarnings._

  // DAFFODIL-1583
  @Test def test_warningWhenExpectingSuccess() = { runner.runOneTest("warningWhenExpectingSuccess") }
  @Test def test_warningWhenExpectingError() = { runner.runOneTest("warningWhenExpectingError") }

}
