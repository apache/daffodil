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

package org.apache.daffodil.section05.simple_types

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestSimpleTypes2 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/SimpleTypes2.tdml"
}

object TestSimpleTypesAJ extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/AJ.tdml"
}

object TestSimpleTypesAK extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/AK.tdml"
}

object TestSimpleTypesAL extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/AL.tdml"
}

object TestSimpleTypesBitOrder extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/BitOrder.tdml"
}

object TestSimpleTypesST extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/simple-type-bases.tdml"
}

object TestSimpleTypesWS extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/WhiteSpace.tdml"
}

class TestSimpleTypes2 extends TdmlTests {
  val tdmlSuite = TestSimpleTypes2

  // DAFFODIL-2204
  @Test def terminatorErrorMessage = test
}

class TestSimpleTypesAJ extends TdmlTests {
  val tdmlSuite = TestSimpleTypesAJ

  @Test def AJ000 = test
  @Test def AJ001 = test
}

class TestSimpleTypesAK extends TdmlTests {
  val tdmlSuite = TestSimpleTypesAK

  @Test def AK000 = test
  @Test def AK001 = test
}

class TestSimpleTypesAL extends TdmlTests {
  val tdmlSuite = TestSimpleTypesAL

  @Test def AL000 = test
}

class TestSimpleTypesBitOrder extends TdmlTests {
  val tdmlSuite = TestSimpleTypesBitOrder

  // THIS TEST won't round trip until encoding for 7-bit ascii is implemented
  // Currently this is set to roundTrip="false"
  @Test def TestMIL2045_47001D_Page70_TableB_I_with_string = test
  @Test def TestMIL2045_47001D_1 = test
  @Test def TestLSBFirstSpan3Bytes = test
  @Test def leastSignificantBitFirst = test
  @Test def leastSigBitFirstFloat = test
  @Test def leastSigBitFirstDouble = test
  @Test def leastSignificantBitFirstRTL = test
  @Test def mostSignificantBitFirst1 = test
  @Test def mostSigBitFirstFloat = test
  @Test def mostSigBitFirstDouble = test
  @Test def littleEndianLeastFirstLTR = test
  @Test def littleEndianLeastFirstRTL = test
  @Test def bitOrderChangeInvalid2 = test
  @Test def bitOrderChangeInvalid2Unparser = test
  @Test def noByteOrder = test

  // DAFFODIL-897
  @Ignore @Test def bitOrderChange = test
  @Ignore @Test def bitOrderDocument = test
  @Ignore @Test def bitOrderTypeByte = test
  @Ignore @Test def bitOrderChangeInvalid3 = test
  @Ignore @Test def bitOrderChangeInvalid = test

  // DAFFODIL-1001 fixed.
  @Test def bigEndianLeastFirst = test

  @Test def mostSigBitFirstLEFloat = test
  @Test def mostSigBitFirstLEDouble = test
}

class TestSimpleTypesST extends TdmlTests {
  val tdmlSuite = TestSimpleTypesST

  @Test def simpleTypeDerivedFromPrimType = test
  @Test def simpleTypeChainedDerivations = test
  @Test def simpleTypeOverlapPrimError = test
  @Test def simpleTypeOverlapSimpleTypeError = test
}

class TestSimpleTypesWS extends TdmlTests {
  val tdmlSuite = TestSimpleTypesWS

  @Test def redefinedFormat = test
  @Test def whiteSpaceBeforeLax = test
  @Test def whiteSpaceAfterLax = test
  @Test def whiteSpaceDuringLax = test
  @Test def whiteSpaceBeforeStrict = test
  @Test def whiteSpaceDuringStrict = test
  @Test def whiteSpaceAfterStrict = test
}
