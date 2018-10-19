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
import org.junit.AfterClass

object TestPropertySyntax {
  val testDir1 = "/org/apache/daffodil/section07/property_syntax/"
  val runner1 = Runner(testDir1, "PropertySyntax.tdml", false, false)
  val runner1V = Runner(testDir1, "PropertySyntax.tdml", validateTDMLFile = false)

  @AfterClass def shutDown {
    runner1.reset
  }

}

class TestPropertySyntax {

  import TestPropertySyntax._

  @Test def test_ShortAndLongForm() { runner1.runOneTest("ShortAndLongForm") }
  @Test def test_ShortAnnotationAndElementForm() { runner1.runOneTest("ShortAnnotationAndElementForm") }
  @Test def test_AnnotationAndElementForm() { runner1.runOneTest("AnnotationAndElementForm") }
  @Test def test_ShortAndElementForm() { runner1.runOneTest("ShortAndElementForm") }
  @Test def test_Lesson3_attribute_form() { runner1.runOneTest("Lesson3_attribute_form") }
  @Test def test_Lesson3_element_form() { runner1.runOneTest("Lesson3_element_form") }
  @Test def test_Lesson3_short_form() { runner1.runOneTest("Lesson3_short_form") }
  @Test def test_encodingEmptyFail() { runner1V.runOneTest("encodingEmptyFail") }

  @Test def test_dafProperty1() { runner1.runOneTest("dafProperty1") }
  @Test def test_dafProperty2() { runner1.runOneTest("dafProperty2") }

}
