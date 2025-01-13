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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestPropertySyntax extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/property_syntax/PropertySyntax.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = false, validateDFDLSchemas = false)
}

object TestPropertySyntaxValidate extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/property_syntax/PropertySyntax.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = false, validateDFDLSchemas = true)
}

class TestPropertySyntax extends TdmlTests {
  val tdmlSuite = TestPropertySyntax

  @Test def ShortAndLongForm = test
  @Test def ShortAnnotationAndElementForm = test
  @Test def AnnotationAndElementForm = test
  @Test def ShortAndElementForm = test
  @Test def Lesson3_attribute_form = test
  @Test def Lesson3_element_form = test
  @Test def Lesson3_short_form = test

  @Test def dafProperty1 = test
  @Test def dafProperty2 = test
  @Test def dfdlxProperty1 = test
  @Test def dfdlxProperty2 = test

  @Test def ignoredPropertiesWarning = test

  // DFDL-1842
  @Test def overlappingProperties1 = test
  @Test def overlappingProperties2 = test

  @Test def badElementFormProperty = test
  @Test def badElementFormProperty2 = test
}

class TestPropertySyntaxValidate extends TdmlTests {
  val tdmlSuite = TestPropertySyntaxValidate

  @Test def encodingEmptyFail = test
}
