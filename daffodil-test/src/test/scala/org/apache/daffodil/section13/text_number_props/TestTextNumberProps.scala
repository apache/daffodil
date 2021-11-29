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

package org.apache.daffodil.section13.text_number_props

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestTextNumberProps {
  val testDir = "/org/apache/daffodil/section13/text_number_props/"
  val runner = Runner(testDir, "TextNumberProps.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}
class TestTextNumberProps {
  import TestTextNumberProps._

  @Test def test_textNumberPattern_baseConflict(): Unit = { runner.runOneTest("textNumberPattern_baseConflict") }

  @Test def test_textNumberPattern_positiveMandatory(): Unit = { runner.runOneTest("textNumberPattern_positiveMandatory") }
  @Test def test_textNumberPattern_negativeOptional(): Unit = { runner.runOneTest("textNumberPattern_negativeOptional") }

  @Test def test_textNumberPattern_negativeIgnored01(): Unit = { runner.runOneTest("textNumberPattern_negativeIgnored01") }
  @Test def test_textNumberPattern_negativeIgnored02(): Unit = { runner.runOneTest("textNumberPattern_negativeIgnored02") }
  @Test def test_textNumberPattern_negativeIgnored03(): Unit = { runner.runOneTest("textNumberPattern_negativeIgnored03") }
  @Test def test_textNumberPattern_negativeIgnored04(): Unit = { runner.runOneTest("textNumberPattern_negativeIgnored04") }
  @Test def test_textNumberPattern_negativeIgnored05(): Unit = { runner.runOneTest("textNumberPattern_negativeIgnored05") }
  @Test def test_textNumberPattern_negativeIgnored06(): Unit = { runner.runOneTest("textNumberPattern_negativeIgnored06") }

  @Test def test_textNumberPattern_exponent01(): Unit = { runner.runOneTest("textNumberPattern_exponent01") }
  @Test def test_textNumberPattern_specialChar01(): Unit = { runner.runOneTest("textNumberPattern_specialChar01") }
  @Test def test_textNumberPattern_specialChar02(): Unit = { runner.runOneTest("textNumberPattern_specialChar02") }
  @Test def test_textNumberPattern_specialChar03(): Unit = { runner.runOneTest("textNumberPattern_specialChar03") }
  @Test def test_textNumberPattern_specialChar04(): Unit = { runner.runOneTest("textNumberPattern_specialChar04") }
  @Test def test_textNumberPattern_specialChar05(): Unit = { runner.runOneTest("textNumberPattern_specialChar05") }
  @Test def test_textNumberPattern_specialChar06(): Unit = { runner.runOneTest("textNumberPattern_specialChar06") }
  @Test def test_textNumberPattern_specialChar07(): Unit = { runner.runOneTest("textNumberPattern_specialChar07") }
  @Test def test_textNumberPattern_specialChar08(): Unit = { runner.runOneTest("textNumberPattern_specialChar08") }

  @Test def test_textNumberPattern_inputValueCalc(): Unit = { runner.runOneTest("textNumberPattern_inputValueCalc") }

  @Test def test_patternDeterminesChoice01(): Unit = { runner.runOneTest("patternDeterminesChoice01") }
  @Test def test_patternDeterminesChoice02(): Unit = { runner.runOneTest("patternDeterminesChoice02") }
  @Test def test_patternDeterminesChoice03(): Unit = { runner.runOneTest("patternDeterminesChoice03") }

  @Test def test_textNumberPattern_facets01(): Unit = { runner.runOneTest("textNumberPattern_facets01") }
  @Test def test_textNumberPattern_facets02(): Unit = { runner.runOneTest("textNumberPattern_facets02") }

  @Test def test_standardZeroRep01(): Unit = { runner.runOneTest("standardZeroRep01") }
  @Test def test_standardZeroRep02(): Unit = { runner.runOneTest("standardZeroRep02") }
  @Test def test_standardZeroRep03(): Unit = { runner.runOneTest("standardZeroRep03") }
  @Test def test_standardZeroRep04(): Unit = { runner.runOneTest("standardZeroRep04") }
  @Test def test_standardZeroRep04b(): Unit = { runner.runOneTest("standardZeroRep04b") }
  @Test def test_standardZeroRep05(): Unit = { runner.runOneTest("standardZeroRep05") }
  @Test def test_standardZeroRep06(): Unit = { runner.runOneTest("standardZeroRep06") }
  @Test def test_standardZeroRep07(): Unit = { runner.runOneTest("standardZeroRep07") }
  @Test def test_standardZeroRep08(): Unit = { runner.runOneTest("standardZeroRep08") }
  @Test def test_standardZeroRep09(): Unit = { runner.runOneTest("standardZeroRep09") }
  @Test def test_standardZeroRep10(): Unit = { runner.runOneTest("standardZeroRep10") }
  @Test def test_standardZeroRep11(): Unit = { runner.runOneTest("standardZeroRep11") }

  @Test def test_textNumberPattern_noNegative(): Unit = { runner.runOneTest("textNumberPattern_noNegative") }

  @Test def test_lengthDeterminedFirst01(): Unit = { runner.runOneTest("lengthDeterminedFirst01") }
  @Test def test_lengthDeterminedFirst02(): Unit = { runner.runOneTest("lengthDeterminedFirst02") }

  @Test def test_textNumberPattern_unsignedType01(): Unit = { runner.runOneTest("textNumberPattern_unsignedType01") }
  @Test def test_textNumberPattern_unsignedType02(): Unit = { runner.runOneTest("textNumberPattern_unsignedType02") }

  @Test def test_dynamicExp(): Unit = { runner.runOneTest("dynamicExp") }
  @Test def test_dynamicExp2(): Unit = { runner.runOneTest("dynamicExp2") }
  @Test def test_dynamicExp_neg(): Unit = { runner.runOneTest("dynamicExpNeg") }
  @Test def test_dynamicExpInvalid(): Unit = { runner.runOneTest("dynamicExpInvalid") }
  @Test def test_expCaseInsensitive(): Unit = { runner.runOneTest("expCaseInsensitive") }
  // DAFFODIL-861
  //@Test def test_expCaseSensitive() { runner.runOneTest("expCaseSensitive") }
  @Test def test_expRawByteInvalid(): Unit = { runner.runOneTest("expRawByteInvalid") }
  @Test def test_expRawByteNotAllowed(): Unit = { runner.runOneTest("expRawByteNotAllowed") }
  @Test def test_expCharClasses(): Unit = { runner.runOneTest("expCharClasses") }
  @Test def test_expCharEntities(): Unit = { runner.runOneTest("expCharEntities") }
  @Test def test_expCharEntities2(): Unit = { runner.runOneTest("expCharEntities2") }
  @Test def test_noStandardExpRep(): Unit = { runner.runOneTest("noStandardExpRep") }
  @Test def test_noStandardExpRep2(): Unit = { runner.runOneTest("noStandardExpRep2") }
  // DAFFODIL-1981
  //@Test def test_expEmptyString() { runner.runOneTest("expEmptyString") }
  //@Test def test_expEmptyString2() { runner.runOneTest("expEmptyString2") }

  @Test def test_zero(): Unit = { runner.runOneTest("zero") }
  @Test def test_pattern_neg1(): Unit = { runner.runOneTest("pattern_neg1") }

  @Test def test_infnanDouble(): Unit = { runner.runOneTest("infnanDouble") }
  @Test def test_nanFloat(): Unit = { runner.runOneTest("nanFloat") }
  @Test def test_infFloat(): Unit = { runner.runOneTest("infFloat") }
  @Test def test_infnan2(): Unit = { runner.runOneTest("infnan2") }
  @Test def test_nanInvalidType(): Unit = { runner.runOneTest("nanInvalidType") }
  @Test def test_infInvalidType(): Unit = { runner.runOneTest("infInvalidType") }
  @Test def test_infInvalid(): Unit = { runner.runOneTest("infInvalid") }
  @Test def test_nanInvalid(): Unit = { runner.runOneTest("nanInvalid") }
  @Test def test_nanCharClasses(): Unit = { runner.runOneTest("nanCharClasses") }
  @Test def test_nanCharEntities(): Unit = { runner.runOneTest("nanCharEntities") }
  @Test def test_infCharClasses(): Unit = { runner.runOneTest("infCharClasses") }
  @Test def test_infCharEntities(): Unit = { runner.runOneTest("infCharEntities") }
  // DAFFODIL-861
  //@Test def test_infnanCaseInsensitive() { runner.runOneTest("infnanCaseInsensitive") }

  @Test def test_textNumberCheckPolicy_lax01(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax01") }
  @Test def test_textNumberCheckPolicy_lax02(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax02") }
  @Test def test_textNumberCheckPolicy_lax03(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax03") }
  @Test def test_textNumberCheckPolicy_lax04(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax04") }
  @Test def test_textNumberCheckPolicy_lax05(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax05") }
  @Test def test_textNumberCheckPolicy_lax06(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax06") }
  @Test def test_textNumberCheckPolicy_lax07(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax07") }
  @Test def test_textNumberCheckPolicy_lax08(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax08") }
  @Test def test_textNumberCheckPolicy_lax09(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax09") }
  @Test def test_textNumberCheckPolicy_lax10(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax10") }
  @Test def test_textNumberCheckPolicy_lax11(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax11") }
  @Test def test_textNumberCheckPolicy_lax12(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax12") }
  @Test def test_textNumberCheckPolicy_lax13(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax13") }
  @Test def test_textNumberCheckPolicy_lax14(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax14") }
  @Test def test_textNumberCheckPolicy_lax15(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax15") }
  @Test def test_textNumberCheckPolicy_lax16(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax16") }
  @Test def test_textNumberCheckPolicy_lax17(): Unit = { runner.runOneTest("textNumberCheckPolicy_lax17") }

  @Test def test_textNumberCheckPolicy_strict01(): Unit = { runner.runOneTest("textNumberCheckPolicy_strict01") }
  @Test def test_textNumberCheckPolicy_strict02(): Unit = { runner.runOneTest("textNumberCheckPolicy_strict02") }
  @Test def test_textNumberCheckPolicy_strict03(): Unit = { runner.runOneTest("textNumberCheckPolicy_strict03") }
  @Test def test_textNumberCheckPolicy_strict04(): Unit = { runner.runOneTest("textNumberCheckPolicy_strict04") }
  @Test def test_textNumberCheckPolicy_strict05(): Unit = { runner.runOneTest("textNumberCheckPolicy_strict05") }
  @Test def test_textNumberCheckPolicy_strict06(): Unit = { runner.runOneTest("textNumberCheckPolicy_strict06") }
  @Test def test_textNumberCheckPolicy_strict07(): Unit = { runner.runOneTest("textNumberCheckPolicy_strict07") }

  @Test def test_textStandardDecimalSeparator01(): Unit = { runner.runOneTest("textStandardDecimalSeparator01") }
  @Test def test_textStandardDecimalSeparator02(): Unit = { runner.runOneTest("textStandardDecimalSeparator02") }
  @Test def test_textStandardDecimalSeparator03(): Unit = { runner.runOneTest("textStandardDecimalSeparator03") }
  @Test def test_textStandardDecimalSeparator04(): Unit = { runner.runOneTest("textStandardDecimalSeparator04") }
  @Test def test_textStandardDecimalSeparator05(): Unit = { runner.runOneTest("textStandardDecimalSeparator05") }
  @Test def test_textStandardDecimalSeparator06(): Unit = { runner.runOneTest("textStandardDecimalSeparator06") }
  @Test def test_textStandardDecimalSeparator07(): Unit = { runner.runOneTest("textStandardDecimalSeparator07") }
  @Test def test_textStandardDecimalSeparator08(): Unit = { runner.runOneTest("textStandardDecimalSeparator08") }
  @Test def test_textStandardDecimalSeparator09(): Unit = { runner.runOneTest("textStandardDecimalSeparator09") }
  @Test def test_textStandardDecimalSeparator12(): Unit = { runner.runOneTest("textStandardDecimalSeparator12") }
  @Test def test_textStandardDecimalSeparator13(): Unit = { runner.runOneTest("textStandardDecimalSeparator13") }
  @Test def test_textStandardDecimalSeparator14(): Unit = { runner.runOneTest("textStandardDecimalSeparator14") }
  @Test def test_textStandardDecimalSeparator15(): Unit = { runner.runOneTest("textStandardDecimalSeparator15") }
  @Test def test_textStandardDecimalSeparator16(): Unit = { runner.runOneTest("textStandardDecimalSeparator16") }
  @Test def test_textStandardDecimalSeparator17(): Unit = { runner.runOneTest("textStandardDecimalSeparator17") }

  // DFDL-847
  //  @Test def test_textStandardDecimalSeparator10() { runner.runOneTest("textStandardDecimalSeparator10") }
  //  @Test def test_textStandardDecimalSeparator11() { runner.runOneTest("textStandardDecimalSeparator11") }

  @Test def test_textStandardGroupingSeparator01(): Unit = { runner.runOneTest("textStandardGroupingSeparator01") }
  @Test def test_textStandardGroupingSeparator02(): Unit = { runner.runOneTest("textStandardGroupingSeparator02") }
  @Test def test_textStandardGroupingSeparator03(): Unit = { runner.runOneTest("textStandardGroupingSeparator03") }
  @Test def test_textStandardGroupingSeparator04(): Unit = { runner.runOneTest("textStandardGroupingSeparator04") }
  @Test def test_textStandardGroupingSeparator05(): Unit = { runner.runOneTest("textStandardGroupingSeparator05") }
  @Test def test_textStandardGroupingSeparator06(): Unit = { runner.runOneTest("textStandardGroupingSeparator06") }
  @Test def test_textStandardGroupingSeparator07(): Unit = { runner.runOneTest("textStandardGroupingSeparator07") }
  @Test def test_textStandardGroupingSeparator08(): Unit = { runner.runOneTest("textStandardGroupingSeparator08") }
  @Test def test_textStandardGroupingSeparator09(): Unit = { runner.runOneTest("textStandardGroupingSeparator09") }
  @Test def test_textStandardGroupingSeparator10(): Unit = { runner.runOneTest("textStandardGroupingSeparator10") }
  @Test def test_textStandardGroupingSeparator11(): Unit = { runner.runOneTest("textStandardGroupingSeparator11") }
  @Test def test_textStandardGroupingSeparator12(): Unit = { runner.runOneTest("textStandardGroupingSeparator12") }
  @Test def test_textStandardGroupingSeparator13(): Unit = { runner.runOneTest("textStandardGroupingSeparator13") }
  @Test def test_textStandardGroupingSeparator14(): Unit = { runner.runOneTest("textStandardGroupingSeparator14") }
  @Test def test_textStandardGroupingSeparator15(): Unit = { runner.runOneTest("textStandardGroupingSeparator15") }
  @Test def test_textStandardGroupingSeparator16(): Unit = { runner.runOneTest("textStandardGroupingSeparator16") }
  @Test def test_textStandardGroupingSeparator17(): Unit = { runner.runOneTest("textStandardGroupingSeparator17") }

  // DFDL-853
  //  @Test def test_textNumberPattern_pSymbol01() { runner.runOneTest("textNumberPattern_pSymbol01") }
  //  @Test def test_textNumberPattern_pSymbol02() { runner.runOneTest("textNumberPattern_pSymbol02") }

  @Test def test_textNumberPattern_scientificNotation01(): Unit = { runner.runOneTest("textNumberPattern_scientificNotation01") }
  @Test def test_textNumberPattern_scientificNotation02(): Unit = { runner.runOneTest("textNumberPattern_scientificNotation02") }
  @Test def test_textNumberPattern_scientificNotation03(): Unit = { runner.runOneTest("textNumberPattern_scientificNotation03") }
  @Test def test_textNumberPattern_scientificNotation04(): Unit = { runner.runOneTest("textNumberPattern_scientificNotation04") }
  @Test def test_textNumberPattern_scientificNotation05(): Unit = { runner.runOneTest("textNumberPattern_scientificNotation05") }
  @Test def test_textNumberPattern_scientificNotation06(): Unit = { runner.runOneTest("textNumberPattern_scientificNotation06") }
  @Test def test_textNumberPattern_scientificNotation07(): Unit = { runner.runOneTest("textNumberPattern_scientificNotation07") }

  @Test def test_textNumberPattern_padding01(): Unit = { runner.runOneTest("textNumberPattern_padding01") }
  @Test def test_textNumberPattern_padding02(): Unit = { runner.runOneTest("textNumberPattern_padding02") }
  @Test def test_textNumberPattern_padding03(): Unit = { runner.runOneTest("textNumberPattern_padding03") }
  @Test def test_textNumberPattern_padding04(): Unit = { runner.runOneTest("textNumberPattern_padding04") }
  @Test def test_textNumberPattern_padding05(): Unit = { runner.runOneTest("textNumberPattern_padding05") }
  @Test def test_textNumberPattern_padding06(): Unit = { runner.runOneTest("textNumberPattern_padding06") }
  @Test def test_textNumberPattern_padding07(): Unit = { runner.runOneTest("textNumberPattern_padding07") }
  @Test def test_textNumberPattern_padding08(): Unit = { runner.runOneTest("textNumberPattern_padding08") }
  @Test def test_textNumberPattern_padding09(): Unit = { runner.runOneTest("textNumberPattern_padding09") }
  @Test def test_textNumberPattern_padding10(): Unit = { runner.runOneTest("textNumberPattern_padding10") }
  @Test def test_textNumberPattern_padding11(): Unit = { runner.runOneTest("textNumberPattern_padding11") }
  @Test def test_textNumberPattern_padding12(): Unit = { runner.runOneTest("textNumberPattern_padding12") }
  @Test def test_textNumberPattern_padding13(): Unit = { runner.runOneTest("textNumberPattern_padding13") }

  @Test def test_textNumberPattern_paddingCombo01(): Unit = { runner.runOneTest("textNumberPattern_paddingCombo01") }
  @Test def test_textNumberPattern_paddingCombo02(): Unit = { runner.runOneTest("textNumberPattern_paddingCombo02") }
  @Test def test_textNumberPattern_paddingCombo03(): Unit = { runner.runOneTest("textNumberPattern_paddingCombo03") }
  @Test def test_textNumberPattern_paddingCombo04(): Unit = { runner.runOneTest("textNumberPattern_paddingCombo04") }
  @Test def test_textNumberPattern_paddingCombo05(): Unit = { runner.runOneTest("textNumberPattern_paddingCombo05") }
  @Test def test_textNumberPattern_paddingCombo06(): Unit = { runner.runOneTest("textNumberPattern_paddingCombo06") }

  @Test def test_decimalPadding01(): Unit = { runner.runOneTest("decimalPadding01") }
  @Test def test_decimalPadding02(): Unit = { runner.runOneTest("decimalPadding02") }
  @Test def test_decimalPadding03(): Unit = { runner.runOneTest("decimalPadding03") }
  @Test def test_decimalPadding04(): Unit = { runner.runOneTest("decimalPadding04") }
  @Test def test_decimalPadding05(): Unit = { runner.runOneTest("decimalPadding05") }

  @Test def test_nonNegIntPadding01(): Unit = { runner.runOneTest("nonNegIntPadding01") }
  @Test def test_nonNegIntPadding02(): Unit = { runner.runOneTest("nonNegIntPadding02") }
  @Test def test_nonNegIntPadding03(): Unit = { runner.runOneTest("nonNegIntPadding03") }
  @Test def test_nonNegIntPadding04(): Unit = { runner.runOneTest("nonNegIntPadding04") }

  @Test def test_hexBinaryPadding01(): Unit = { runner.runOneTest("hexBinaryPadding01") }

  @Test def test_dynamic(): Unit = { runner.runOneTest("dynamic") }
  @Test def test_dynamicNeg1(): Unit = { runner.runOneTest("dynamicNeg1") }
  @Test def test_dynamicNeg2(): Unit = { runner.runOneTest("dynamicNeg2") }

  @Test def test_textStandardDistinctValues(): Unit = { runner.runOneTest("textStandardDistinctValues") }
  @Test def test_textStandardDistinctValues2(): Unit = { runner.runOneTest("textStandardDistinctValues2") }
  @Test def test_textStandardDistinctValues3(): Unit = { runner.runOneTest("textStandardDistinctValues3") }

  @Test def test_textStandardFloatPatternNoSeparators1(): Unit = { runner.runOneTest("textStandardFloatPatternNoSeparators1") }
  @Test def test_textStandardFloatPatternNoSeparators2(): Unit = { runner.runOneTest("textStandardFloatPatternNoSeparators2") }

  @Test def test_textStandardRoundingIncrement1(): Unit = { runner.runOneTest("textNumberRoundingIncrement1") }
  @Test def test_textStandardRoundingIncrement2(): Unit = { runner.runOneTest("textNumberRoundingIncrement2") }
  @Test def test_textStandardRoundingIncrement3(): Unit = { runner.runOneTest("textNumberRoundingIncrement3") }
  @Test def test_textStandardRoundingIncrement4(): Unit = { runner.runOneTest("textNumberRoundingIncrement4") }
}
