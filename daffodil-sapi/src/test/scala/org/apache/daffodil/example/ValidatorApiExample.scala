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

package org.apache.daffodil.example

import org.junit.Test
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

class ValidatorApiExample extends ValidatorExamplesSupport {
  @Test
  def testAlwaysPass(): Unit =
    withSchema("/test/sapi/mySchema5.dfdl.xsd") { dp =>
      withInput("/test/sapi/myData5.dat") { input =>
        val res = dp.withValidator(Always.passes).parse(input, `/dev/null`)
        assertFalse(res.isValidationError())
      }
  }

  @Test
  def testAlwaysFail(): Unit =
    withSchema("/test/sapi/mySchema5.dfdl.xsd") { dp =>
      withInput("/test/sapi/myData5.dat") { input =>
        val res = dp.withValidator(Always.fails).parse(input, `/dev/null`)
        assertTrue(res.isValidationError())
      }
    }
}
