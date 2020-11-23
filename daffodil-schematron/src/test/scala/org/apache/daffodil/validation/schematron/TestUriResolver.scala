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

package org.apache.daffodil.validation.schematron

import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.api.ValidatorInitializationException
import org.junit.Test

class TestUriResolver {
  @Test def testExists(): Unit = {
    ClassPathUriResolver("sch").resolve("schematron-1.sch", null)
  }

  @Test def testFallback(): Unit = {
    ClassPathUriResolver("foo", ClassPathUriResolver("sch")).resolve("schematron-1.sch", null)
  }

  @Test def testFileNotFound(): Unit = {
    intercept[ValidatorInitializationException] {
      ClassPathUriResolver("foo").resolve("bar.sch", null)
    }
  }

  @Test def testFallbackNotFound(): Unit = {
    intercept[ValidatorInitializationException] {
      ClassPathUriResolver("foo", ClassPathUriResolver("foo2")).resolve("bar.sch", null)
    }
  }
}
