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

object TestSimpleTypes extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/simple_types/SimpleTypes.tdml"
}

class TestSimpleTypes extends TdmlTests {
  val tdmlSuite = TestSimpleTypes

  @Test def warning_exercise = test

  @Test def OneOctetBinaryParse = test
  @Test def OneBit2 = test

  @Test def NonNegativeInteger = test
  @Test def NonNegativeInteger_Fail = test

  @Test def hexBinary_rep = test
  @Test def hexBinary_fromString = test

  @Test def hexBinary_01 = test
  @Test def hexBinary_01_chunk = test
  @Test def hexBinary_Delimited_01 = test
  @Test def hexBinary_Delimited_01a = test
  @Test def hexBinary_Delimited_02 = test
  @Test def hexBinary_Delimited_03 = test
  @Test def hexBinary_Delimited_04 = test
  @Test def hexBinary_Implicit_01 = test
  @Test def hexBinary_Implicit_02 = test
  @Test def hexBinary_Implicit_03 = test
  @Test def hexBinary_Implicit_03b = test
  @Test def hexBinary_Implicit_03c = test
  @Test def hexBinary_Implicit_04 = test
  @Test def hexBinary_func = test
  @Test def hexBinary_func_neg = test

  @Test def hexBinary_bits_be_msbf = test
  @Test def hexBinary_bits_le_msbf = test
  @Test def hexBinary_bits_le_lsbf = test

  @Test def hexBinary_bits_be_msbf_2 = test
  @Test def hexBinary_bits_le_msbf_2 = test
  @Test def hexBinary_bits_le_lsbf_2 = test

  @Test def dateTextNumberRep = test

  @Test def datePattern02 = test
  @Test def datePattern02b = test
  @Test def timePattern01 = test
  @Test def timePattern01b = test

  @Test def dateCalendarLanguage = test
  @Test def dateCalendarLanguage2 = test
  @Test def dateCalendarLanguage3 = test
  @Test def dateCalendarLanguage4 = test

  @Test def dateTimeCalendarDaysInFirstWeek = test
  @Test def dateTimeCalendarDaysInFirstWeek2 = test
  @Test def dateTimeCalendarDaysInFirstWeek3 = test
  @Test def dateTimeCalendarDaysInFirstWeek4 = test
  @Test def dateTimeCalendarDaysInFirstWeek5 = test
  @Test def dateTimeCalendarDaysInFirstWeek6 = test

  @Test def dateTimeTrim01 = test
  @Test def dateTimeTrim02 = test
  @Test def dateTimeTrim03 = test
  @Test def dateTimeTrim04 = test

  @Test def dateTimePattern01 = test
  @Test def dateTimePattern02 = test
  @Test def dateTimePattern03 = test

  @Test def dateEpochFillIn = test
  @Test def dateEpochFillIn2 = test
  @Test def dateEpochFillIn3 = test
  @Test def datePattern08 = test
  @Test def datePattern08b = test

  @Test def datePattern03 = test
  @Test def datePattern04 = test
  @Test def datePattern05 = test
  @Test def datePattern06 = test
  @Test def datePattern07 = test

  @Test def datePatternChoice = test

  // DFDL-519
  @Ignore @Test def dateCalendarCenturyStart = test
  @Ignore @Test def dateCalendarCenturyStart2 = test

  @Test def dateCalendarDaysInFirstWeek = test
  @Test def dateCalendarDaysInFirstWeek2 = test
  @Test def dateCalendarDaysInFirstWeek3 = test
  @Test def dateCalendarDaysInFirstWeek4 = test
  @Test def dateCalendarDaysInFirstWeek5 = test

  @Test def timeSymbols = test
  @Test def timeSymbols2 = test
  @Test def epochFillIn = test
  @Test def epochFillIn2 = test
  @Test def epochFillIn3 = test

  @Test def timeTrim01 = test
  @Test def timeTrim02 = test
  @Test def millisecondAccuracy = test
  @Test def millisecondAccuracy2 = test
  @Test def millisecondAccuracy3 = test
  @Test def millisecondAccuracy4 = test
  @Test def millisecondAccuracy5 = test
  @Test def millisecondAccuracy6 = test
  @Test def millisecondAccuracy7 = test

  @Test def timeFormatting = test
  @Test def timeFormatting2 = test
  @Test def timeFormatting2c = test
  @Test def timeFormatting2b = test
  @Test def timeFormatting3 = test
  @Test def timeFormatting4 = test
  @Test def timeFormatting6 = test
  @Test def timeFormatting7 = test

  @Test def timeCalendarTimeZone = test
  @Test def timeCalendarTimeZone2 = test
  @Test def timeCalendarTimeZone3 = test
  @Test def timeCalendarTimeZone3_unparse = test

  @Test def timeZoneFormats = test
  @Test def timeZoneFormats2 = test
  @Test def timeZoneFormats3 = test
  @Test def timeZoneFormats4 = test
  @Test def timeZoneFormats5 = test
  @Test def timeZoneFormats6 = test
  @Test def timeZoneFormats6_2 = test
  @Test def timeZoneFormats7 = test
  @Test def timeZoneFormats8 = test
  @Test def timeZoneFormats9 = test
  @Test def timeZoneFormats_unparse = test
  @Test def timeZoneFormats2_unparse = test
  @Test def timeZoneFormats3_unparse = test
  @Test def timeZoneFormats4_unparse = test
  @Test def timeZoneFormats5_unparse = test
  @Test def timeZoneFormats6_unparse = test
  @Test def timeZoneFormats6_2_unparse = test
  @Test def timeZoneFormats7_unparse = test
  @Test def timeZoneFormats9_unparse = test

  @Test def dateCountDeterminesFormat = test
  @Test def dateNonAlphaChars01 = test
  @Test def dateTrim01 = test
  @Test def dateTrim02 = test
  @Test def dateTrim03 = test

  @Test def dateCalendarFirstDayOfWeek01 = test
  @Test def dateCalendarFirstDayOfWeek02 = test
  @Test def dateCalendarFirstDayOfWeek03 = test
  @Test def dateCalendarFirstDayOfWeek04 = test
  @Test def timeFractionalSeconds01 = test
  @Test def dateText = test
  @Test def dateTextInvalid = test
  @Test def timeText = test
  @Test def timeTextInvalid = test
  @Test def dateTimeText = test
  @Test def dateTimeTextInvalid = test
  @Test def dateImplicitPattern = test
  @Test def dateImplicitPatternFail = test
  @Test def timeImplicitPattern = test
  @Test def timeImplicitPatternFail = test
  @Test def dateTimeBin = test
  @Test def dateTimeBin2 = test
  @Test def dateTimeBin3 = test
  @Test def dateTimeBin4 = test
  @Test def dateTimeBin5 = test
  @Test def dateTimeBin6 = test
  @Test def dateTimeBin7 = test
  @Test def dateTimeBin8 = test
  @Test def dateTimeBin9 = test
  @Test def dateTimeBin10 = test
  @Test def dateTimeBin11 = test
  @Test def dateTimeBin12 = test
  @Test def dateTimeBin13 = test
  @Test def dateTimeBin14 = test
  @Test def dateTimeBin15 = test
  @Test def dateTimeBin16 = test
  @Test def dateTimeBin17 = test
  @Test def dateTimeBin18 = test
  @Test def dateTimeBin19 = test
  @Test def dateTimeBin20 = test
  @Test def dateTimeBin21 = test
  @Test def dateTimeBin22 = test
  @Test def dateTimeBin23 = test
  @Test def dateTimeBin24 = test
  @Test def dateTimeBin25 = test
  @Test def dateBinBCD = test
  @Test def dateBinBCD2 = test
  @Test def dateBinBCD3 = test
  @Test def dateBinBCD4 = test
  @Test def dateBinBCD5 = test
  @Test def dateBinBCD6 = test
  @Test def dateBinBCD7 = test
  @Test def dateBinBCD8 = test
  @Test def dateBinBCD9 = test
  @Test def timeBinBCD = test
  @Test def timeBinBCD2 = test
  @Test def timeBinBCD3 = test
  @Test def timeBinBCD4 = test
  @Test def timeBinBCD5 = test
  @Test def timeBinBCD6 = test
  @Test def dateTimeBinBCD = test
  @Test def dateTimeBinBCD2 = test
  @Test def dateTimeBinBCD3 = test
  @Test def dateTimeBinBCD4 = test

  @Test def dateBinIBM4690Packed = test
  @Test def dateBinIBM4690Packed2 = test
  @Test def dateBinIBM4690Packed3 = test
  @Test def dateBinIBM4690Packed4 = test
  @Test def dateTimeBinIBM4690Packed = test
  @Test def dateTimeBinIBM4690Packed2 = test
  @Test def dateTimeBinIBM4690Packed3 = test
  @Test def timeBinIBM4690Packed = test
  @Test def timeBinIBM4690Packed2 = test

  @Test def dateBinPacked = test
  @Test def dateBinPacked2 = test
  @Test def dateBinPacked4 = test
  @Test def timeBinPacked = test
  @Test def timeBinPacked2 = test
  @Test def dateTimeBinPacked = test
  @Test def dateTimeBinPacked2 = test
  @Test def dateTimeBinPacked3 = test

  @Test def dateBinInvalid = test

  @Test def dateTimeImplicitPattern = test
  @Test def dateTimeImplicitPatternFail = test
  @Test def dateTimeImplicitPatternFail2 = test
  @Test def dateTimeImplicitPatternFail3 = test
  @Test def dateTimeImplicitPatternFail4 = test
  @Test def dateTimeImplicitPatternFail5 = test
  @Test def dateTimeImplicitPatternFail6 = test

  @Test def datePattern01 = test
  // DAFFODIL-488
  @Ignore @Test def datePattern01b = test
  @Test def timeLaxCheckPolicy01 = test
  @Test def timeLaxCheckPolicy02 = test
  @Test def timeLaxCheckPolicy03 = test
  @Test def dateTimeLaxCheckPolicy01 = test
  @Test def dateLaxCheckPolicy01 = test
  @Test def dateLaxCheckPolicy02 = test
  @Test def dateLaxCheckPolicy03 = test
  @Test def dateLaxCheckPolicy04 = test
  @Test def dateLaxCheckPolicy05 = test

  // DFDL-1042/DFDL-2951
  @Test def dateStrictCheckPolicy01 = test
  @Test def dateStrictCheckPolicy02 = test
  @Test def timeStrictCheckPolicy01 = test
  @Test def timeStrictCheckPolicy02 = test
  @Test def timeFormatting5 = test

  @Test def Long1 = test
  @Test def BigInteger1 = test
  @Test def Integer01 = test
  @Test def integer2 = test
  @Test def integer_fail = test
  @Test def integer_fail2 = test

  @Test def Int01 = test
  @Test def int_error = test

  @Test def UnsignedNumbers1 = test
  @Test def unsignedLong_01 = test
  @Test def unsignedLong_02 = test
  @Test def unsignedLong_03 = test
  @Test def Long2 = test
  @Test def Long3 = test
  @Test def Long4 = test
  @Test def int_error_02 = test
  @Test def int_error_03 = test
  @Test def short_01 = test
  @Test def short_02 = test
  @Test def unsignedInt_01 = test
  @Test def unsignedInt_02 = test

  @Test def whiteSpaceBeforeValidInt = test
  @Test def whiteSpaceBeforeValidInteger = test
  @Test def whiteSpaceBeforeValidLong = test
  @Test def whiteSpaceBeforeValidShort = test
  @Test def whiteSpaceBeforeValidByte = test
  @Test def whiteSpaceBeforeValidUnsignedInt = test
  @Test def whiteSpaceBeforeValidUnsignedByte = test
  @Test def whiteSpaceBeforeValidUnsignedLong = test
  @Test def whiteSpaceBeforeValidUnsignedShort = test

  // DFDL-845
  @Ignore @Test def whiteSpaceDuringValidShort = test
  @Ignore @Test def whiteSpaceDuringValidInteger = test
  @Ignore @Test def whiteSpaceDuringValidUnsignedLong = test
  @Ignore @Test def whiteSpaceDuringValidUnsignedShort = test
  @Ignore @Test def whiteSpaceDuringValidUnsignedByte = test
  @Ignore @Test def whiteSpaceDuringValidUnsignedInt = test
  @Ignore @Test def whiteSpaceDuringValidInt = test
  @Ignore @Test def whiteSpaceDuringValidLong = test
  @Ignore @Test def whiteSpaceDuringValidByte = test

  @Test def whiteSpaceAfterValidInt = test
  @Test def whiteSpaceAfterValidLong = test
  @Test def whiteSpaceAfterValidShort = test
  @Test def whiteSpaceAfterValidUnsignedInt = test
  @Test def whiteSpaceAfterValidUnsignedShort = test
  @Test def whiteSpaceAfterValidUnsignedByte = test
  @Test def whiteSpaceAfterValidByte = test
  @Test def whiteSpaceAfterValidUnsignedLong = test
  @Test def whiteSpaceAfterValidInteger = test

  @Test def characterDuringValidInt = test
  @Test def int_unparseError = test
  @Test def whiteSpaceAfterLengthExceededInt = test
  @Test def whiteSpaceBeforeLengthExceededInt = test
  @Test def whiteSpaceDuringLengthExceededInt = test
  @Test def characterDuringValidInteger = test
  @Test def integer_unparseError = test
  @Test def whiteSpaceAfterLengthExceededInteger = test
  @Test def whiteSpaceBeforeLengthExceededInteger = test
  @Test def whiteSpaceDuringLengthExceededInteger = test
  @Test def characterDuringValidLong = test
  @Test def long_unparseError = test
  @Test def whiteSpaceAfterLengthExceededLong = test
  @Test def whiteSpaceBeforeLengthExceededLong = test
  @Test def whiteSpaceDuringLengthExceededLong = test

  @Test def characterDuringValidShort = test
  @Test def short_unparseError = test
  @Test def whiteSpaceAfterLengthExceededShort = test
  @Test def whiteSpaceBeforeLengthExceededShort = test
  @Test def whiteSpaceDuringLengthExceededShort = test
  @Test def characterDuringValidByte = test
  @Test def byte_unparseError = test
  @Test def whiteSpaceAfterLengthExceededByte = test
  @Test def whiteSpaceBeforeLengthExceededByte = test
  @Test def whiteSpaceDuringLengthExceededByte = test
  @Test def characterDuringValidUnsignedInt = test
  @Test def negativeUnsignedInt = test
  @Test def whiteSpaceAfterLengthExceededUnsignedInt = test
  @Test def whiteSpaceBeforeLengthExceededUnsignedInt = test
  @Test def whiteSpaceDuringLengthExceededUnsignedInt = test
  @Test def characterDuringValidUnsignedByte = test
  @Test def unsignedByte_unparseError = test
  @Test def negativeUnsignedByte = test
  @Test def whiteSpaceAfterLengthExceededUnsignedByte = test
  @Test def whiteSpaceBeforeLengthExceededUnsignedByte = test
  @Test def whiteSpaceDuringLengthExceededUnsignedByte = test
  @Test def characterDuringValidUnsignedLong = test
  @Test def unsignedLong_unparseError = test
  @Test def negativeUnsignedLong = test
  @Test def whiteSpaceAfterLengthExceededUnsignedLong = test
  @Test def whiteSpaceBeforeLengthExceededUnsignedLong = test
  @Test def whiteSpaceDuringLengthExceededUnsignedLong = test

  @Test def characterDuringValidUnsignedShort = test
  @Test def unsignedShort_unparseError = test
  @Test def negativeUnsignedShort = test
  @Test def whiteSpaceAfterLengthExceededUnsignedShort = test
  @Test def whiteSpaceBeforeLengthExceededUnsignedShort = test
  @Test def whiteSpaceDuringLengthExceededUnsignedShort = test

  @Test def unsignedShort_01 = test
  @Test def unsignedByte_01 = test
  @Test def unsignedByte_02 = test

  // Test range checking for signed integers too!
  @Test def byte_01 = test
  @Test def byte_02 = test

  @Test def signedShort_binary = test
  @Test def signedShort_binary2 = test

  @Test def unsignedShort_binary = test
  @Test def unsignedShort_binary2 = test

  @Test def unsignedLong_binary = test
  @Test def unsignedLong_binary2 = test

  @Test def signedLong_binary = test
  @Test def signedLong_binary2 = test

  @Test def unsignedInt_binary = test
  @Test def unsignedInt_binary2 = test

  @Test def double_binary_01 = test
  @Test def double_binary_02 = test
  @Test def double_binary_03 = test
  @Test def double_binary_04 = test
  @Test def double_binary_05 = test
  @Test def double_binary_06 = test
  @Test def double_binary_07 = test

  @Test def byte_binary_01 = test
  @Test def byte_binary_02 = test
  @Test def byte_binary_03 = test
  @Test def byte_binary_04 = test
  @Test def byte_binary_05 = test

  @Test def byte_implicit = test

  @Test def double_07 = test

  @Test def ubyte_binary_01 = test
  @Test def ubyte_binary_02 = test
  @Test def ubyte_binary_03 = test
  @Test def ubyte_binary_04 = test
  @Test def ubyte_binary_05 = test

  @Test def ubyte_implicit = test

  @Test def int_binary_01 = test
  @Test def int_binary_02 = test
  @Test def int_binary_03 = test
  @Test def int_binary_04 = test
  @Test def int_binary_05 = test

  @Test def binaryInt_unparseError = test
  @Test def hexBinary_unparseError = test
  @Test def calendar_unparseError = test

  @Test def int_implicit = test

  @Test def nonNegInt_binary_01 = test

  @Test def literalChar_padding = test
  @Test def literalChar_padding2 = test
  @Test def literalChar_padding3 = test
  @Test def literalChar_padding4 = test
  @Test def literalChar_padding5 = test
  @Test def literalChar_padding6 = test
  @Test def charEntity_padding1 = test
  @Test def charEntity_padding2 = test
  @Test def charEntity_padding3 = test
  @Test def number_padding = test
  @Test def number_padding2 = test
  @Test def number_padding3 = test
  @Test def number_padding4 = test
  @Test def number_padding5 = test
  @Test def padding_escape = test
  @Test def padding_nil = test
  @Test def padding_nil2 = test
  @Test def padding_empty = test
  @Test def justification_1 = test
  @Test def percentPadding = test

  // Verification that user's test works for DFDL-677
  @Test def TestUnsignedInt = test

  @Test def nonNegativeInteger_text = test
  @Test def nonNegativeInteger_text2 = test
  @Test def nonNegativeInteger_text_fail = test
  @Test def nonNegativeInteger_text_fail2 = test
  @Test def nonNegativeInteger_text_fail3 = test
  @Test def nonNegativeInteger_unparseError = test

  @Test def nonNegativeInteger_bin = test
  @Test def nonNegativeInteger_bin2 = test
  @Test def nonNegativeInteger_bin3 = test
  @Test def nonNegativeInteger_bin4 = test
  @Test def nonNegativeInteger_bin5 = test
  @Test def nonNegativeInteger_bin6 = test

  @Test def integer_binary = test
  @Test def integer_binary_01 = test
  @Test def integer_binary_02 = test
  @Test def integer_binary_03 = test
  @Test def integer_binary_04 = test

  @Test def decimal_text = test
  @Test def decimal_text2 = test
  @Test def decimal_text3 = test
  @Test def decimal_text_fail = test
  @Test def characterDuringValidDecimal = test
  @Test def decimal_unparseError = test

  @Test def decimal_binary = test
  @Test def decimal_binary_01 = test
  @Test def decimal_binary_02 = test
  @Test def decimal_binary_03 = test
  @Test def decimal_binary_04 = test
  @Test def decimal_binary_05 = test
  @Test def decimal_binary_06 = test
  @Test def decimal_binary_fail_01 = test
  @Test def decimal_binary_fail_02 = test
  @Test def decimal_binary_fail_03 = test
  @Test def decimal_binary_fail_04 = test

  @Test def double_text = test
  @Test def double_text2_parse_ab = test
  @Test def double_text2_parse_ac = test
  @Test def double_text2_parse_ub = test
  @Test def double_text2_parse_uc = test
  @Test def double_text2_unparse_ab = test
  @Test def double_text2_unparse_ac = test
  @Test def double_text2_unparse_ub = test
  @Test def double_text2_unparse_uc = test
  @Test def double_text2_unparse_fail_ab = test
  @Test def double_text2_unparse_fail_ac = test
  @Test def double_text2_unparse_fail_ub = test
  @Test def double_text2_unparse_fail_uc = test
  @Test def double_text3 = test
  @Test def double_text4 = test
  @Test def characterDuringValidDouble = test
  @Test def double_unparseError = test
  @Test def double_negativeZero = test

  @Test def float_text = test
  @Test def float_text2 = test
  @Test def float_text3 = test
  @Test def float_text_max = test

  @Test def float_text_fail = test
  @Test def characterDuringValidFloat = test
  @Test def float_unparseError = test
  @Test def float_binary_01 = test
  @Test def float_binary_02 = test
  @Test def float_binary_03 = test
  @Test def float_binary_04 = test
  @Test def float_binary_05 = test
  @Test def float_binary_fail_01 = test
  @Test def float_binary_fail_02 = test
  @Test def float_binary_fail_03 = test

  @Test def time_calendarTimeZone_EmptyString = test
  @Test def time_calendarTimeZone_EST = test
  @Test def date_calendarTimeZone_EmptyString = test
  @Test def date_calendarTimeZone_EST = test
  @Test def dateTime_calendarTimeZone_EmptyString = test
  @Test def dateTime_calendarTimeZone_EST = test

  @Test def hexBinary_specifiedLengthUnaligned = test

  @Test def time_optional_empty = test
  @Test def time_optional_nonWellFormed = test

  @Test def restrictionBaseEmbeddedLeadingSpace = test
  @Test def restrictionBaseEmbeddedTrailingSpace = test
}
