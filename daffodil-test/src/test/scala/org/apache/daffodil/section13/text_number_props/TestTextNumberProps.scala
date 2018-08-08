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

  @AfterClass def shutDown() {
    runner.reset
  }
}
class TestTextNumberProps {
  import TestTextNumberProps._

  @Test def test_textNumberPattern_positiveMandatory() { runner.runOneTest("textNumberPattern_positiveMandatory") }
  @Test def test_textNumberPattern_negativeOptional() { runner.runOneTest("textNumberPattern_negativeOptional") }

  @Test def test_textNumberPattern_negativeIgnored01() { runner.runOneTest("textNumberPattern_negativeIgnored01") }
  @Test def test_textNumberPattern_negativeIgnored02() { runner.runOneTest("textNumberPattern_negativeIgnored02") }
  @Test def test_textNumberPattern_negativeIgnored03() { runner.runOneTest("textNumberPattern_negativeIgnored03") }
  @Test def test_textNumberPattern_negativeIgnored04() { runner.runOneTest("textNumberPattern_negativeIgnored04") }
  @Test def test_textNumberPattern_negativeIgnored05() { runner.runOneTest("textNumberPattern_negativeIgnored05") }
  @Test def test_textNumberPattern_negativeIgnored06() { runner.runOneTest("textNumberPattern_negativeIgnored06") }

  @Test def test_textNumberPattern_exponent01() { runner.runOneTest("textNumberPattern_exponent01") }
  @Test def test_textNumberPattern_specialChar01() { runner.runOneTest("textNumberPattern_specialChar01") }
  @Test def test_textNumberPattern_specialChar02() { runner.runOneTest("textNumberPattern_specialChar02") }
  @Test def test_textNumberPattern_specialChar03() { runner.runOneTest("textNumberPattern_specialChar03") }
  @Test def test_textNumberPattern_specialChar04() { runner.runOneTest("textNumberPattern_specialChar04") }
  @Test def test_textNumberPattern_specialChar05() { runner.runOneTest("textNumberPattern_specialChar05") }
  @Test def test_textNumberPattern_specialChar06() { runner.runOneTest("textNumberPattern_specialChar06") }
  @Test def test_textNumberPattern_specialChar07() { runner.runOneTest("textNumberPattern_specialChar07") }
  @Test def test_textNumberPattern_specialChar08() { runner.runOneTest("textNumberPattern_specialChar08") }

  @Test def test_textNumberPattern_inputValueCalc() { runner.runOneTest("textNumberPattern_inputValueCalc") }

  @Test def test_patternDeterminesChoice01() { runner.runOneTest("patternDeterminesChoice01") }
  @Test def test_patternDeterminesChoice02() { runner.runOneTest("patternDeterminesChoice02") }
  @Test def test_patternDeterminesChoice03() { runner.runOneTest("patternDeterminesChoice03") }

  @Test def test_textNumberPattern_facets01() { runner.runOneTest("textNumberPattern_facets01") }
  @Test def test_textNumberPattern_facets02() { runner.runOneTest("textNumberPattern_facets02") }

  @Test def test_standardZeroRep01() { runner.runOneTest("standardZeroRep01") }
  @Test def test_standardZeroRep02() { runner.runOneTest("standardZeroRep02") }
  @Test def test_standardZeroRep03() { runner.runOneTest("standardZeroRep03") }
  @Test def test_standardZeroRep04() { runner.runOneTest("standardZeroRep04") }
  @Test def test_standardZeroRep04b() { runner.runOneTest("standardZeroRep04b") }
  @Test def test_standardZeroRep05() { runner.runOneTest("standardZeroRep05") }
  @Test def test_standardZeroRep06() { runner.runOneTest("standardZeroRep06") }
  @Test def test_standardZeroRep07() { runner.runOneTest("standardZeroRep07") }
  @Test def test_standardZeroRep08() { runner.runOneTest("standardZeroRep08") }
  @Test def test_standardZeroRep09() { runner.runOneTest("standardZeroRep09") }
  @Test def test_standardZeroRep10() { runner.runOneTest("standardZeroRep10") }
  @Test def test_standardZeroRep11() { runner.runOneTest("standardZeroRep11") }

  @Test def test_textNumberPattern_noNegative() { runner.runOneTest("textNumberPattern_noNegative") }

  @Test def test_lengthDeterminedFirst01() { runner.runOneTest("lengthDeterminedFirst01") }
  @Test def test_lengthDeterminedFirst02() { runner.runOneTest("lengthDeterminedFirst02") }

  @Test def test_textNumberPattern_unsignedType01() { runner.runOneTest("textNumberPattern_unsignedType01") }
  @Test def test_textNumberPattern_unsignedType02() { runner.runOneTest("textNumberPattern_unsignedType02") }

  @Test def test_dynamicExp() { runner.runOneTest("dynamicExp") }
  @Test def test_dynamicExp2() { runner.runOneTest("dynamicExp2") }
  @Test def test_dynamicExp_neg() { runner.runOneTest("dynamicExpNeg") }
  @Test def test_dynamicExpInvalid() { runner.runOneTest("dynamicExpInvalid") }
  @Test def test_expCaseInsensitive() { runner.runOneTest("expCaseInsensitive") }
  // DAFFODIL-861
  //@Test def test_expCaseSensitive() { runner.runOneTest("expCaseSensitive") }
  @Test def test_expRawByteInvalid() { runner.runOneTest("expRawByteInvalid") }
  @Test def test_expRawByteNotAllowed() { runner.runOneTest("expRawByteNotAllowed") }
  @Test def test_expCharClasses() { runner.runOneTest("expCharClasses") }
  @Test def test_expCharEntities() { runner.runOneTest("expCharEntities") }
  @Test def test_expCharEntities2() { runner.runOneTest("expCharEntities2") }
  @Test def test_noStandardExpRep() { runner.runOneTest("noStandardExpRep") }
  @Test def test_noStandardExpRep2() { runner.runOneTest("noStandardExpRep2") }
  // DAFFODIL-1981
  //@Test def test_expEmptyString() { runner.runOneTest("expEmptyString") }
  //@Test def test_expEmptyString2() { runner.runOneTest("expEmptyString2") }

  @Test def test_zero() { runner.runOneTest("zero") }
  @Test def test_pattern_neg1() { runner.runOneTest("pattern_neg1") }

  @Test def test_infnanDouble() { runner.runOneTest("infnanDouble") }
  @Test def test_nanFloat() { runner.runOneTest("nanFloat") }
  @Test def test_infFloat() { runner.runOneTest("infFloat") }
  @Test def test_infnan2() { runner.runOneTest("infnan2") }
  @Test def test_nanInvalidType() { runner.runOneTest("nanInvalidType") }
  @Test def test_infInvalidType() { runner.runOneTest("infInvalidType") }
  @Test def test_infInvalid() { runner.runOneTest("infInvalid") }
  @Test def test_nanInvalid() { runner.runOneTest("nanInvalid") }
  @Test def test_nanCharClasses() { runner.runOneTest("nanCharClasses") }
  @Test def test_nanCharEntities() { runner.runOneTest("nanCharEntities") }
  @Test def test_infCharClasses() { runner.runOneTest("infCharClasses") }
  @Test def test_infCharEntities() { runner.runOneTest("infCharEntities") }
  // DAFFODIL-861
  //@Test def test_infnanCaseInsensitive() { runner.runOneTest("infnanCaseInsensitive") }

  @Test def test_textNumberCheckPolicy_lax01() { runner.runOneTest("textNumberCheckPolicy_lax01") }
  @Test def test_textNumberCheckPolicy_lax02() { runner.runOneTest("textNumberCheckPolicy_lax02") }
  @Test def test_textNumberCheckPolicy_lax03() { runner.runOneTest("textNumberCheckPolicy_lax03") }
  @Test def test_textNumberCheckPolicy_lax04() { runner.runOneTest("textNumberCheckPolicy_lax04") }
  @Test def test_textNumberCheckPolicy_lax05() { runner.runOneTest("textNumberCheckPolicy_lax05") }
  @Test def test_textNumberCheckPolicy_lax06() { runner.runOneTest("textNumberCheckPolicy_lax06") }
  @Test def test_textNumberCheckPolicy_lax07() { runner.runOneTest("textNumberCheckPolicy_lax07") }
  @Test def test_textNumberCheckPolicy_lax08() { runner.runOneTest("textNumberCheckPolicy_lax08") }
  @Test def test_textNumberCheckPolicy_lax09() { runner.runOneTest("textNumberCheckPolicy_lax09") }
  @Test def test_textNumberCheckPolicy_lax10() { runner.runOneTest("textNumberCheckPolicy_lax10") }
  @Test def test_textNumberCheckPolicy_lax11() { runner.runOneTest("textNumberCheckPolicy_lax11") }
  @Test def test_textNumberCheckPolicy_lax12() { runner.runOneTest("textNumberCheckPolicy_lax12") }
  @Test def test_textNumberCheckPolicy_lax13() { runner.runOneTest("textNumberCheckPolicy_lax13") }
  @Test def test_textNumberCheckPolicy_lax14() { runner.runOneTest("textNumberCheckPolicy_lax14") }
  @Test def test_textNumberCheckPolicy_lax15() { runner.runOneTest("textNumberCheckPolicy_lax15") }
  @Test def test_textNumberCheckPolicy_lax16() { runner.runOneTest("textNumberCheckPolicy_lax16") }
  @Test def test_textNumberCheckPolicy_lax17() { runner.runOneTest("textNumberCheckPolicy_lax17") }

  @Test def test_textNumberCheckPolicy_strict01() { runner.runOneTest("textNumberCheckPolicy_strict01") }
  @Test def test_textNumberCheckPolicy_strict02() { runner.runOneTest("textNumberCheckPolicy_strict02") }
  @Test def test_textNumberCheckPolicy_strict03() { runner.runOneTest("textNumberCheckPolicy_strict03") }
  @Test def test_textNumberCheckPolicy_strict04() { runner.runOneTest("textNumberCheckPolicy_strict04") }
  @Test def test_textNumberCheckPolicy_strict05() { runner.runOneTest("textNumberCheckPolicy_strict05") }
  @Test def test_textNumberCheckPolicy_strict06() { runner.runOneTest("textNumberCheckPolicy_strict06") }
  @Test def test_textNumberCheckPolicy_strict07() { runner.runOneTest("textNumberCheckPolicy_strict07") }

  @Test def test_textStandardDecimalSeparator01() { runner.runOneTest("textStandardDecimalSeparator01") }
  @Test def test_textStandardDecimalSeparator02() { runner.runOneTest("textStandardDecimalSeparator02") }
  @Test def test_textStandardDecimalSeparator03() { runner.runOneTest("textStandardDecimalSeparator03") }
  @Test def test_textStandardDecimalSeparator04() { runner.runOneTest("textStandardDecimalSeparator04") }
  @Test def test_textStandardDecimalSeparator05() { runner.runOneTest("textStandardDecimalSeparator05") }
  @Test def test_textStandardDecimalSeparator06() { runner.runOneTest("textStandardDecimalSeparator06") }
  @Test def test_textStandardDecimalSeparator07() { runner.runOneTest("textStandardDecimalSeparator07") }
  @Test def test_textStandardDecimalSeparator08() { runner.runOneTest("textStandardDecimalSeparator08") }
  @Test def test_textStandardDecimalSeparator09() { runner.runOneTest("textStandardDecimalSeparator09") }
  @Test def test_textStandardDecimalSeparator12() { runner.runOneTest("textStandardDecimalSeparator12") }
  @Test def test_textStandardDecimalSeparator13() { runner.runOneTest("textStandardDecimalSeparator13") }
  @Test def test_textStandardDecimalSeparator14() { runner.runOneTest("textStandardDecimalSeparator14") }
  @Test def test_textStandardDecimalSeparator15() { runner.runOneTest("textStandardDecimalSeparator15") }
  @Test def test_textStandardDecimalSeparator16() { runner.runOneTest("textStandardDecimalSeparator16") }
  @Test def test_textStandardDecimalSeparator17() { runner.runOneTest("textStandardDecimalSeparator17") }

  // DFDL-847
  //  @Test def test_textStandardDecimalSeparator10() { runner.runOneTest("textStandardDecimalSeparator10") }
  //  @Test def test_textStandardDecimalSeparator11() { runner.runOneTest("textStandardDecimalSeparator11") }

  @Test def test_textStandardGroupingSeparator01() { runner.runOneTest("textStandardGroupingSeparator01") }
  @Test def test_textStandardGroupingSeparator02() { runner.runOneTest("textStandardGroupingSeparator02") }
  @Test def test_textStandardGroupingSeparator03() { runner.runOneTest("textStandardGroupingSeparator03") }
  @Test def test_textStandardGroupingSeparator04() { runner.runOneTest("textStandardGroupingSeparator04") }
  @Test def test_textStandardGroupingSeparator05() { runner.runOneTest("textStandardGroupingSeparator05") }
  @Test def test_textStandardGroupingSeparator06() { runner.runOneTest("textStandardGroupingSeparator06") }
  @Test def test_textStandardGroupingSeparator07() { runner.runOneTest("textStandardGroupingSeparator07") }
  @Test def test_textStandardGroupingSeparator08() { runner.runOneTest("textStandardGroupingSeparator08") }
  @Test def test_textStandardGroupingSeparator09() { runner.runOneTest("textStandardGroupingSeparator09") }
  @Test def test_textStandardGroupingSeparator10() { runner.runOneTest("textStandardGroupingSeparator10") }
  @Test def test_textStandardGroupingSeparator11() { runner.runOneTest("textStandardGroupingSeparator11") }
  @Test def test_textStandardGroupingSeparator12() { runner.runOneTest("textStandardGroupingSeparator12") }
  @Test def test_textStandardGroupingSeparator13() { runner.runOneTest("textStandardGroupingSeparator13") }
  @Test def test_textStandardGroupingSeparator14() { runner.runOneTest("textStandardGroupingSeparator14") }
  @Test def test_textStandardGroupingSeparator15() { runner.runOneTest("textStandardGroupingSeparator15") }
  @Test def test_textStandardGroupingSeparator16() { runner.runOneTest("textStandardGroupingSeparator16") }
  @Test def test_textStandardGroupingSeparator17() { runner.runOneTest("textStandardGroupingSeparator17") }

  // DFDL-853
  //  @Test def test_textNumberPattern_pSymbol01() { runner.runOneTest("textNumberPattern_pSymbol01") }
  //  @Test def test_textNumberPattern_pSymbol02() { runner.runOneTest("textNumberPattern_pSymbol02") }

  @Test def test_textNumberPattern_scientificNotation01() { runner.runOneTest("textNumberPattern_scientificNotation01") }
  @Test def test_textNumberPattern_scientificNotation02() { runner.runOneTest("textNumberPattern_scientificNotation02") }
  @Test def test_textNumberPattern_scientificNotation03() { runner.runOneTest("textNumberPattern_scientificNotation03") }
  @Test def test_textNumberPattern_scientificNotation04() { runner.runOneTest("textNumberPattern_scientificNotation04") }
  @Test def test_textNumberPattern_scientificNotation05() { runner.runOneTest("textNumberPattern_scientificNotation05") }
  @Test def test_textNumberPattern_scientificNotation06() { runner.runOneTest("textNumberPattern_scientificNotation06") }
  @Test def test_textNumberPattern_scientificNotation07() { runner.runOneTest("textNumberPattern_scientificNotation07") }

  @Test def test_textNumberPattern_padding01() { runner.runOneTest("textNumberPattern_padding01") }
  @Test def test_textNumberPattern_padding02() { runner.runOneTest("textNumberPattern_padding02") }
  @Test def test_textNumberPattern_padding03() { runner.runOneTest("textNumberPattern_padding03") }
  @Test def test_textNumberPattern_padding04() { runner.runOneTest("textNumberPattern_padding04") }
  @Test def test_textNumberPattern_padding05() { runner.runOneTest("textNumberPattern_padding05") }
  @Test def test_textNumberPattern_padding06() { runner.runOneTest("textNumberPattern_padding06") }
  @Test def test_textNumberPattern_padding07() { runner.runOneTest("textNumberPattern_padding07") }
  @Test def test_textNumberPattern_padding08() { runner.runOneTest("textNumberPattern_padding08") }
  @Test def test_textNumberPattern_padding09() { runner.runOneTest("textNumberPattern_padding09") }
  @Test def test_textNumberPattern_padding10() { runner.runOneTest("textNumberPattern_padding10") }
  @Test def test_textNumberPattern_padding11() { runner.runOneTest("textNumberPattern_padding11") }
  @Test def test_textNumberPattern_padding12() { runner.runOneTest("textNumberPattern_padding12") }
  @Test def test_textNumberPattern_padding13() { runner.runOneTest("textNumberPattern_padding13") }

  @Test def test_textNumberPattern_paddingCombo01() { runner.runOneTest("textNumberPattern_paddingCombo01") }
  @Test def test_textNumberPattern_paddingCombo02() { runner.runOneTest("textNumberPattern_paddingCombo02") }
  @Test def test_textNumberPattern_paddingCombo03() { runner.runOneTest("textNumberPattern_paddingCombo03") }
  @Test def test_textNumberPattern_paddingCombo04() { runner.runOneTest("textNumberPattern_paddingCombo04") }
  @Test def test_textNumberPattern_paddingCombo05() { runner.runOneTest("textNumberPattern_paddingCombo05") }
  @Test def test_textNumberPattern_paddingCombo06() { runner.runOneTest("textNumberPattern_paddingCombo06") }

  @Test def test_decimalPadding01() { runner.runOneTest("decimalPadding01") }
  @Test def test_decimalPadding02() { runner.runOneTest("decimalPadding02") }
  @Test def test_decimalPadding03() { runner.runOneTest("decimalPadding03") }
  @Test def test_decimalPadding04() { runner.runOneTest("decimalPadding04") }
  @Test def test_decimalPadding05() { runner.runOneTest("decimalPadding05") }

  @Test def test_nonNegIntPadding01() { runner.runOneTest("nonNegIntPadding01") }
  @Test def test_nonNegIntPadding02() { runner.runOneTest("nonNegIntPadding02") }
  @Test def test_nonNegIntPadding03() { runner.runOneTest("nonNegIntPadding03") }
  @Test def test_nonNegIntPadding04() { runner.runOneTest("nonNegIntPadding04") }

  @Test def test_hexBinaryPadding01() { runner.runOneTest("hexBinaryPadding01") }

  @Test def test_dynamic() { runner.runOneTest("dynamic") }
  @Test def test_dynamicNeg1() { runner.runOneTest("dynamicNeg1") }
  @Test def test_dynamicNeg2() { runner.runOneTest("dynamicNeg2") }

  @Test def test_textStandardDistinctValues() { runner.runOneTest("textStandardDistinctValues") }
  @Test def test_textStandardDistinctValues2() { runner.runOneTest("textStandardDistinctValues2") }
  @Test def test_textStandardDistinctValues3() { runner.runOneTest("textStandardDistinctValues3") }
}
