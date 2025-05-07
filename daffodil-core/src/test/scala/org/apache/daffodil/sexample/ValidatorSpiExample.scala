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

package org.apache.daffodil.sexample

import org.apache.daffodil.lib.validation.Validators

import com.typesafe.config.ConfigFactory
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

// there is no support for passing validators by name into the dp as of yet
// so these tests simply load via spi a validator and pass it into the SAPI
class ValidatorSpiExample extends ValidatorExamplesSupport {
  @Test
  def testAlwaysPass(): Unit =
    withSchema("/test/api/mySchema5.dfdl.xsd") { dp =>
      withInput("/test/api/myData5.dat") { input =>
        val v = Validators.get(PassingValidator.name).make(ConfigFactory.empty)
        val res = dp.withValidator(v).parse(input, `/dev/null`)
        assertFalse(res.isValidationError())
      }
    }

  @Test
  def testAlwaysFail(): Unit =
    withSchema("/test/api/mySchema5.dfdl.xsd") { dp =>
      withInput("/test/api/myData5.dat") { input =>
        val v = Validators.get(FailingValidator.name).make(ConfigFactory.empty)
        val res = dp.withValidator(v).parse(input, `/dev/null`)
        assertTrue(res.isValidationError())
      }
    }
}
