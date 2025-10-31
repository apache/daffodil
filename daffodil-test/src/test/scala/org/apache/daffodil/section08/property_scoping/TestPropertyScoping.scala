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

package org.apache.daffodil.section08.property_scoping

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestPropertyScoping extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section08/property_scoping/PropertyScoping.tdml"
}

object TestPropertyScoping01 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section08/property_scoping/PropertyScoping_01.tdml"
}

class TestPropertyScoping extends TdmlTests {
  val tdmlSuite = TestPropertyScoping

  @Test def defaultForm_01 = test
  @Test def defaultForm_02 = test
  @Test def defaultForm_03 = test
  @Test def defaultForm_04 = test

  @Test def localAnnotation_01 = test
  @Test def localAnnotation_02 = test
  @Test def localAnnotation_03 = test
  @Test def localAnnotation_04 = test
  @Test def localAnnotation_05 = test

  @Test def property_scoping_01 = test
  @Test def unparse_property_scoping_01 = test
  @Test def property_scoping_06 = test
  @Test def unparse_property_scoping_06 = test
  @Test def group_ref = test
  @Test def multipleDefinition = test
  @Test def multipleDefinition2 = test
  @Test def multipleDefinition3 = test

  @Test def format_nesting_01 = test
}

class TestPropertyScoping01 extends TdmlTests {
  val tdmlSuite = TestPropertyScoping01

  @Test def property_scoping_02 = test
  @Test def unparse_property_scoping_02 = test
  @Test def property_scoping_03 = test
  @Test def unparse_property_scoping_03 = test
  @Test def property_scoping_04 = test
  @Test def property_scoping_05 = test
  @Test def unparse_property_scoping_04 = test
  @Test def unparse_property_scoping_05 = test
  @Test def property_scoping_07 = test
  @Test def unparse_property_scoping_07 = test
  @Test def property_scoping_08 = test
  @Test def unparse_property_scoping_08 = test
  @Test def property_scoping_09 = test
  @Test def unparse_property_scoping_09 = test
  @Test def property_scoping_10 = test
  @Test def unparse_property_scoping_10 = test
  @Test def property_scoping_11 = test
  @Test def unparse_property_scoping_11 = test
  @Test def unparse_property_scoping_12 = test
  @Test def NearestEnclosingSequenceElementRef = test
  @Test def property_scoping_12 = test

  @Test def refElementFormFail = test
}
