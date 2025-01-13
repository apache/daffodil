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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestTextNumberProps extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section13/text_number_props/TextNumberProps.tdml"
}

class TestTextNumberProps extends TdmlTests {
  val tdmlSuite = TestTextNumberProps

  @Test def textNumberPattern_baseConflict = test

  @Test def textNumberPattern_positiveMandatory = test
  @Test def textNumberPattern_negativeOptional = test

  @Test def textNumberPattern_negativeIgnored01 = test
  @Test def textNumberPattern_negativeIgnored02 = test
  @Test def textNumberPattern_negativeIgnored03 = test
  @Test def textNumberPattern_negativeIgnored04 = test
  @Test def textNumberPattern_negativeIgnored05 = test
  @Test def textNumberPattern_negativeIgnored06 = test

  @Test def textNumberPattern_exponent01 = test
  @Test def textNumberPattern_specialChar01 = test
  @Test def textNumberPattern_specialChar02 = test
  @Test def textNumberPattern_specialChar03 = test
  @Test def textNumberPattern_specialChar04 = test
  @Test def textNumberPattern_specialChar05 = test
  @Test def textNumberPattern_specialChar06 = test
  @Test def textNumberPattern_specialChar07 = test
  @Test def textNumberPattern_specialChar08 = test

  @Test def textNumberPattern_inputValueCalc = test

  @Test def patternDeterminesChoice01 = test
  @Test def patternDeterminesChoice02 = test
  @Test def patternDeterminesChoice03 = test

  @Test def textNumberPattern_facets01 = test
  @Test def textNumberPattern_facets02 = test

  @Test def standardZeroRep01 = test
  @Test def standardZeroRep02 = test
  @Test def standardZeroRep03 = test
  @Test def standardZeroRep04 = test
  @Test def standardZeroRep04b = test
  @Test def standardZeroRep05 = test
  @Test def standardZeroRep06 = test
  @Test def standardZeroRep07 = test
  @Test def standardZeroRep08 = test
  @Test def standardZeroRep09 = test
  @Test def standardZeroRep10 = test
  @Test def standardZeroRep11 = test

  @Test def textNumberPattern_noNegative = test

  @Test def lengthDeterminedFirst01 = test
  @Test def lengthDeterminedFirst02 = test

  @Test def textNumberPattern_unsignedType01 = test
  @Test def textNumberPattern_unsignedType02 = test

  @Test def dynamicExp = test
  @Test def dynamicExp2 = test
  @Test def dynamicExpNeg = test
  @Test def dynamicExpInvalid = test
  @Test def expCaseInsensitive = test
  // DAFFODIL-861
  @Ignore @Test def expCaseSensitive = test
  @Test def expRawByteInvalid = test
  @Test def expRawByteNotAllowed = test
  @Test def expCharClasses = test
  @Test def expCharEntities = test
  @Test def expCharEntities2 = test
  @Test def noStandardExpRep = test
  @Test def noStandardExpRep2 = test
  // DAFFODIL-1981
  @Ignore @Test def expEmptyString = test
  @Ignore @Test def expEmptyString2 = test

  @Test def zero = test
  @Test def pattern_neg1 = test

  @Test def infnanDouble = test
  @Test def nanFloat = test
  @Test def infFloat = test
  @Test def infnan2 = test
  @Test def nanInvalidType = test
  @Test def infInvalidType = test
  @Test def infInvalid = test
  @Test def nanInvalid = test
  @Test def nanCharClasses = test
  @Test def nanCharEntities = test
  @Test def infCharClasses = test
  @Test def infCharEntities = test
  // DAFFODIL-861
  @Ignore @Test def infnanCaseInsensitive = test

  @Test def textNumberCheckPolicy_lax01 = test
  @Test def textNumberCheckPolicy_lax02 = test
  @Test def textNumberCheckPolicy_lax03 = test
  @Test def textNumberCheckPolicy_lax04 = test
  @Test def textNumberCheckPolicy_lax05 = test
  @Test def textNumberCheckPolicy_lax06 = test
  @Test def textNumberCheckPolicy_lax07 = test
  @Test def textNumberCheckPolicy_lax08 = test
  @Test def textNumberCheckPolicy_lax09 = test
  @Test def textNumberCheckPolicy_lax10 = test
  @Test def textNumberCheckPolicy_lax11 = test
  @Test def textNumberCheckPolicy_lax12 = test
  @Test def textNumberCheckPolicy_lax13 = test
  @Test def textNumberCheckPolicy_lax14 = test
  @Test def textNumberCheckPolicy_lax15 = test
  @Test def textNumberCheckPolicy_lax16 = test
  @Test def textNumberCheckPolicy_lax17 = test

  @Test def textNumberCheckPolicy_strict01 = test
  @Test def textNumberCheckPolicy_strict02 = test
  @Test def textNumberCheckPolicy_strict03 = test
  @Test def textNumberCheckPolicy_strict04 = test
  @Test def textNumberCheckPolicy_strict05 = test
  @Test def textNumberCheckPolicy_strict06 = test
  @Test def textNumberCheckPolicy_strict07 = test

  @Test def textStandardDecimalSeparator01 = test
  @Test def textStandardDecimalSeparator02 = test
  @Test def textStandardDecimalSeparator03 = test
  @Test def textStandardDecimalSeparator04 = test
  @Test def textStandardDecimalSeparator05 = test
  @Test def textStandardDecimalSeparator06 = test
  @Test def textStandardDecimalSeparator07 = test
  @Test def textStandardDecimalSeparator08 = test
  @Test def textStandardDecimalSeparator09 = test
  @Test def textStandardDecimalSeparator09u = test
  @Test def textStandardDecimalSeparator12 = test
  @Test def textStandardDecimalSeparator13 = test
  @Test def textStandardDecimalSeparator14 = test
  @Test def textStandardDecimalSeparator15 = test
  @Test def textStandardDecimalSeparator16 = test
  @Test def textStandardDecimalSeparator17 = test

  @Test def textStandardDecimalSeparatorOneOnly1 = test
  @Test def textStandardDecimalSeparatorOneOnly2 = test

  @Test def textStandardGroupingSeparator01 = test
  @Test def textStandardGroupingSeparator02 = test
  @Test def textStandardGroupingSeparator03 = test
  @Test def textStandardGroupingSeparator04 = test
  @Test def textStandardGroupingSeparator05 = test
  @Test def textStandardGroupingSeparator06 = test
  @Test def textStandardGroupingSeparator07 = test
  @Test def textStandardGroupingSeparator08 = test
  @Test def textStandardGroupingSeparator09 = test
  @Test def textStandardGroupingSeparator10 = test
  @Test def textStandardGroupingSeparator11 = test
  @Test def textStandardGroupingSeparator12 = test
  @Test def textStandardGroupingSeparator13 = test
  @Test def textStandardGroupingSeparator14 = test
  @Test def textStandardGroupingSeparator15 = test
  @Test def textStandardGroupingSeparator16 = test
  @Test def textStandardGroupingSeparator17 = test

  // DFDL-853
  @Ignore @Test def textNumberPattern_pSymbol01 = test
  @Ignore @Test def textNumberPattern_pSymbol02 = test

  @Test def textNumberPattern_scientificNotation01 = test
  @Test def textNumberPattern_scientificNotation02 = test
  @Test def textNumberPattern_scientificNotation03 = test
  @Test def textNumberPattern_scientificNotation04 = test
  @Test def textNumberPattern_scientificNotation05 = test
  @Test def textNumberPattern_scientificNotation06 = test
  @Test def textNumberPattern_scientificNotation07 = test

  @Test def textNumberPattern_padding01 = test
  @Test def textNumberPattern_padding02 = test
  @Test def textNumberPattern_padding03 = test
  @Test def textNumberPattern_padding04 = test
  @Test def textNumberPattern_padding05 = test
  @Test def textNumberPattern_padding06 = test
  @Test def textNumberPattern_padding07 = test
  @Test def textNumberPattern_padding08 = test
  @Test def textNumberPattern_padding09 = test
  @Test def textNumberPattern_padding10 = test
  @Test def textNumberPattern_padding11 = test
  @Test def textNumberPattern_padding12 = test
  @Test def textNumberPattern_padding13 = test

  @Test def textNumberPattern_paddingCombo01 = test
  @Test def textNumberPattern_paddingCombo02 = test
  @Test def textNumberPattern_paddingCombo03 = test
  @Test def textNumberPattern_paddingCombo04 = test
  @Test def textNumberPattern_paddingCombo05 = test
  @Test def textNumberPattern_paddingCombo06 = test

  @Test def decimalPadding01 = test
  @Test def decimalPadding02 = test
  @Test def decimalPadding03 = test
  @Test def decimalPadding04 = test
  @Test def decimalPadding05 = test

  @Test def nonNegIntPadding01 = test
  @Test def nonNegIntPadding02 = test
  @Test def nonNegIntPadding03 = test
  @Test def nonNegIntPadding04 = test
  @Test def nonNegIntPadding05 = test

  @Test def hexBinaryPadding01 = test

  @Test def dynamic = test
  @Test def dynamicNeg1 = test
  @Test def dynamicNeg2 = test

  @Test def textStandardDistinctValues = test
  @Test def textStandardDistinctValues2 = test
  @Test def textStandardDistinctValues3 = test

  @Test def textStandardFloatPatternNoSeparators1 = test
  @Test def textStandardFloatPatternNoSeparators2 = test

  @Test def textNumberRoundingIncrement1 = test
  @Test def textNumberRoundingIncrement2 = test
  @Test def textNumberRoundingIncrement3 = test
  @Test def textNumberRoundingIncrement4 = test

  @Test def textNumberIntWithDecimal01 = test
  @Test def textNumberIntWithDecimal02 = test
  @Test def textNumberIntWithDecimal03 = test
  @Test def textNumberIntWithDecimal04 = test

  @Test def textNumberIntegerWithDecimal01 = test
  @Test def textNumberIntegerWithDecimal02 = test
  @Test def textNumberIntegerWithDecimal03 = test
  @Test def textNumberIntegerWithDecimal04 = test

  @Test def textNumberPaddingAmbiguity01 = test
  @Test def textNumberPaddingAmbiguity02 = test
}
