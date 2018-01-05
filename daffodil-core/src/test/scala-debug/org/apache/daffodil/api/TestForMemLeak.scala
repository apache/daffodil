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

package org.apache.daffodil.api
import scala.xml._
import org.junit.Test
import org.apache.daffodil.Implicits._; object INoWarnAPI1 { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.util.TestUtils

class TestForMemLeak {

  @Test def testParseSimpleLeakChecking() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    val (_, actual) = TestUtils.testString(sch, "5678")
    val expected: Node = <e1>5678</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testLeak() {
    1 to 100 foreach { _ =>
      // println("")
      1 to 100 foreach { _ =>
        // print(".")
        1 to 100 foreach { _ =>
          System.gc()
          Thread.sleep(100)
          testParseSimpleLeakChecking()
        }
      }
    }
  }

}
