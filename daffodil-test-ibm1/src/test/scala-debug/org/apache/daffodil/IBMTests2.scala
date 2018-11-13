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

package org.apache.daffodil

import org.junit.Test
import org.apache.daffodil.tdml.DFDLTestSuite
import org.apache.daffodil.util.Misc
import org.junit.Test

/**
 * Delete this class once these regressions have been addressed.
 */
//class IBMTestRegressions {
//  
//  val testDir = "/test-suite/ibm-contributed/"
//  val tdml1 = testDir + "dpaext1.tdml"
//  val tdml2 = testDir + "dpaext2.tdml"
//  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
//  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))
//
//  // Nothing here! Great!
//}

class IBMTestsThatThrow {

  val testDir = "/test-suite/ibm-contributed/"
  val tdml1 = testDir + "dpaext1.tdml"
  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_alignment_bytes_12_05() { runner1.runOneTest("alignment_bytes_12_05") } //DFDL-99 binary dateTime

  @Test def test_length_implicit_12_02() { runner1.runOneTest("length_implicit_12_02") } // implicit length string - bug in IBM test (doesn't have minLength - both are required)

  @Test def test_length_delimited_12_05() { runner1.runOneTest("length_delimited_12_05") } // DAFFODIL-1541 Need support for handling delimited data with encoding other than ISO-8859-1

  @Test def test_simple_type_properties_text_number_13_01() { runner2.runOneTest("simple_type_properties_text_number_13_01") } // DFDL-452 text number advanced props
  @Test def test_simple_type_properties_text_number_13_03() { runner2.runOneTest("simple_type_properties_text_number_13_03") } // DFDL-452 textStandardBase (base 16)

  @Test def test_simple_type_properties_text_boolean_13_03() { runner2.runOneTest("simple_type_properties_text_boolean_13_03") } // DFDL-462 boolean type
  @Test def test_simple_type_properties_bin_boolean_13_01() { runner2.runOneTest("simple_type_properties_bin_boolean_13_01") } // DFDL-461 boolean type

  @Test def test_simple_type_properties_text_calendar_13_01() { runner2.runOneTest("simple_type_properties_text_calendar_13_01") } // DAFFODIL-1945

  @Test def test_sequences_separated_14_04() { runner2.runOneTest("sequences_separated_14_04") } // left over data

}
