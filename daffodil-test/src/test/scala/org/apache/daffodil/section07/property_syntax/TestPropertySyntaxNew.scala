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

package org.apache.daffodil.section07.property_syntax

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.tdml.RunnerOpts
import org.junit.AfterClass

object TestPropertySyntax2 {

  val testDir1 = "/org/apache/daffodil/section07/property_syntax/"
  val runner = Runner(testDir1, "PropertySyntax.tdml", RunnerOpts(validateTDMLFile=false, validateDFDLSchemas=false))

  @AfterClass def shutDown: Unit = {
    runner.reset
  }

}

class TestPropertySyntax2 {

  import TestPropertySyntax2._

  // JIRA DFDL-1722
  @Test def test_badElementFormProperty(): Unit = { runner.runOneTest("badElementFormProperty") }

  // DAFFODIL-2202
  @Test def test_badElementFormProperty2(): Unit = { runner.runOneTest("badElementFormProperty2") }

}
