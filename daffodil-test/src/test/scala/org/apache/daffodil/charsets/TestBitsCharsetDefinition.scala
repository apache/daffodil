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

package org.apache.daffodil.charsets

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestCharsets extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/charsets/TestBitsCharsetDefinition.tdml"
}

class TestCharsets extends TdmlTests {
  val tdmlSuite = TestCharsets

  @Test def parse_charsets = test
  @Test def unparse_charsets = test
  @Test def parse_charsets2 = test
  @Test def unparse_charsets2 = test
  @Test def parse_charsets3 = test
  @Test def unparse_charsets3 = test
  @Test def verify_error_message = test
}
