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

package org.apache.daffodil.extensions

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestEnums extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/extensions/enum/enums.tdml"
}

object TestEnumsInvalid extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/extensions/enum/enumInvalid.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

class TestEnums extends TdmlTests {
  val tdmlSuite = TestEnums

  @Test def enumValid1 = test
  @Test def enumInvalid1 = test
  @Test def enumMiss1 = test
  @Test def repTypeAlignment = test
}

class TestEnumsInvalid extends TdmlTests {
  val tdmlSuite = TestEnumsInvalid

  @Test def emptyRepValues = test
  @Test def noRepValues = test
  @Test def enumRepTypeNonInt = test
}
