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

package org.apache.daffodil.section12.delimiter_properties

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestDelimiterProperties extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section12/delimiter_properties/DelimiterProperties.tdml"
}

class TestDelimiterProperties extends TdmlTests {
  val tdmlSuite = TestDelimiterProperties

  @Test def DelimProp_01 = test
  @Test def ParseSequence4 = test
  @Test def ParseSequence5 = test
  @Test def DelimProp_02 = test
  @Test def DelimProp_03 = test
  @Test def DelimProp_04 = test
  @Test def DelimProp_05 = test
  @Test def DelimProp_06 = test
  @Test def DelimProp_07 = test
  @Test def initiatedContentSimple1 = test
  @Test def Lesson4_initiators_terminators = test

  @Test def DelimProp_10 = test
  @Test def DelimProp_10_01 = test
  @Test def DelimProp_10_02 = test
  @Test def DelimProp_10_03 = test

  @Test def E1 = test

  @Test def ReqFieldMissingAndSepIsPrefixOfTerminator_Prefix = test
  @Test def ReqFieldMissingAndSepIsPrefixOfTerminator_Infix = test
  @Test def ReqFieldMissingAndSepIsPrefixOfTerminator_Postfix = test

  @Test def OptionalWSPTermWithExplicitLength = test
  @Test def OptionalWSPTermWithExplicitLength2 = test

  @Test def delims_ignorecase_01 = test

  @Test def percentInitiator = test
  @Test def percentSeparator = test
  @Test def percentTerminator = test
  @Test def percentTerminator2 = test
  @Test def percentExpression = test

  @Test def emptyInitiator1 = test
  @Test def emptyInitiator2 = test
  @Test def emptyInitiator3 = test
  @Test def emptyInitiator4 = test
}
