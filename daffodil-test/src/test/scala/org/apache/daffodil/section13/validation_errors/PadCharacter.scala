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

package org.apache.daffodil.section13.validation_errors

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestPadCharacter extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/text_number_props/TextPad.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

class TestPadCharacter extends TdmlTests {
  val tdmlSuite = TestPadCharacter

  // in the short form of the pad character's property binding syntax
  @Test def short_form_pad_char = test

  // This test demonstrates that you cannot use a literal whitespace character
  // in the attribute (long) form of the pad character's property binding syntax
  @Test def long_form_pad_char = test

  // This test demonstrates that you can use a literal whitespace character
  // in the element (property) form of the pad character's property binding
  // syntax but internal validation logic will throw an error
  @Test def property_form_pad_char = test
}
