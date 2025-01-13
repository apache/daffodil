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

package org.apache.daffodil.tutorials

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestTutorialBitOrder extends TdmlSuite {
  val tdmlResource = "/bitorder.tutorial.tdml.xml"
}

object TestTutorialTdml extends TdmlSuite {
  val tdmlResource = "/tdmlTutorial.tdml.xml"
}

object TestTutorialBugReport extends TdmlSuite {
  val tdmlResource = "/bugReportTemplate.tdml.xml"
}

class TestTutorialBitOrder extends TdmlTests {
  val tdmlSuite = TestTutorialBitOrder

  // removed for now. This will probably go back into this tutorial
  @Ignore @Test def TestMIL2045_47001D_1 = test
  @Test def leastSignificantBitFirst = test
  @Test def leastSignificantBitFirstRTL = test
  @Test def mostSignificantBitFirst = test
  @Test def littleEndianLeastFirstLTR = test
  @Test def littleEndianLeastFirstRTL = test

}

class TestTutorialTdml extends TdmlTests {
  val tdmlSuite = TestTutorialTdml

  @Test def dateTimeTest = test
  @Test def unparseDateTimeTest = test
}

class TestTutorialBugReport extends TdmlTests {
  val tdmlSuite = TestTutorialBugReport

  @Test def dateTimeTest = test
  @Test def unparseDateTimeTest = test
}
