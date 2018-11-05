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

import org.junit._
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestSimpleTypes {

  val testDir = "/org/apache/daffodil/section05/simple_types/"
  val testDir_01 = "/org/apache/daffodil/ibm-tests/"

  val runner = Runner(testDir, "SimpleTypes.tdml")

  @AfterClass def shutDown() {
    runner.reset
  }
}

class TestSimpleTypes {
  import TestSimpleTypes._

  @Test def test_one_octet() { runner.runOneTest("OneOctetBinaryParse") }
  @Test def test_oneBit2() { runner.runOneTest("OneBit2") }

  @Test def test_nonNegativeInteger() { runner.runOneTest("NonNegativeInteger") }
  @Test def test_nonNegativeInteger_Fail() { runner.runOneTest("NonNegativeInteger_Fail") }

  @Test def test_hexBinary_rep() { runner.runOneTest("hexBinary_rep") } //roundTrip

  @Test def test_hexBinary_01() { runner.runOneTest("hexBinary_01") } //roundTrip
  @Test def test_hexBinary_Delimited_01() { runner.runOneTest("hexBinary_Delimited_01") } //roundTrip
  @Test def test_hexBinary_Delimited_01a() { runner.runOneTest("hexBinary_Delimited_01a") } //roundTrip
  @Test def test_hexBinary_Delimited_02() { runner.runOneTest("hexBinary_Delimited_02") } //roundTrip
  @Test def test_hexBinary_Delimited_03() { runner.runOneTest("hexBinary_Delimited_03") } //roundTrip
  @Test def test_hexBinary_Implicit_01() { runner.runOneTest("hexBinary_Implicit_01") } //roundTrip
  @Test def test_hexBinary_Implicit_02() { runner.runOneTest("hexBinary_Implicit_02") } //roundTrip
  @Test def test_hexBinary_Implicit_03() { runner.runOneTest("hexBinary_Implicit_03") } //roundTrip
  @Test def test_hexBinary_Implicit_03b() { runner.runOneTest("hexBinary_Implicit_03b") } //roundTrip
  @Test def test_hexBinary_Implicit_03c() { runner.runOneTest("hexBinary_Implicit_03c") } //roundTrip
  @Test def test_hexBinary_Implicit_04() { runner.runOneTest("hexBinary_Implicit_04") } //roundTrip
  @Test def test_hexBinary_func() { runner.runOneTest("hexBinary_func") }
  @Test def test_hexBinary_func_neg() { runner.runOneTest("hexBinary_func_neg") }

  // DFDL-1707
  @Test def test_hexBinary_bits_be_msbf() { runner.runOneTest("hexBinary_bits_be_msbf") }
  @Test def test_hexBinary_bits_le_msbf() { runner.runOneTest("hexBinary_bits_le_msbf") }
  @Test def test_hexBinary_bits_le_lsbf() { runner.runOneTest("hexBinary_bits_le_lsbf") }

  @Test def test_dateTextNumberRep() { runner.runOneTest("dateTextNumberRep") }

  @Test def test_datePattern02() { runner.runOneTest("datePattern02") }
  @Test def test_datePattern02b() { runner.runOneTest("datePattern02b") }
  @Test def test_timePattern01() { runner.runOneTest("timePattern01") }
  @Test def test_timePattern01b() { runner.runOneTest("timePattern01b") }

  @Test def test_dateCalendarLanguage() { runner.runOneTest("dateCalendarLanguage") }
  @Test def test_dateCalendarLanguage2() { runner.runOneTest("dateCalendarLanguage2") }
  @Test def test_dateCalendarLanguage3() { runner.runOneTest("dateCalendarLanguage3") }
  @Test def test_dateCalendarLanguage4() { runner.runOneTest("dateCalendarLanguage4") }

  @Test def test_dateTimeCalendarDaysInFirstWeek() { runner.runOneTest("dateTimeCalendarDaysInFirstWeek") }
  @Test def test_dateTimeCalendarDaysInFirstWeek2() { runner.runOneTest("dateTimeCalendarDaysInFirstWeek2") }
  @Test def test_dateTimeCalendarDaysInFirstWeek3() { runner.runOneTest("dateTimeCalendarDaysInFirstWeek3") }
  @Test def test_dateTimeCalendarDaysInFirstWeek4() { runner.runOneTest("dateTimeCalendarDaysInFirstWeek4") }
  @Test def test_dateTimeCalendarDaysInFirstWeek5() { runner.runOneTest("dateTimeCalendarDaysInFirstWeek5") }
  @Test def test_dateTimeCalendarDaysInFirstWeek6() { runner.runOneTest("dateTimeCalendarDaysInFirstWeek6") }

  @Test def test_dateTimeTrim01() { runner.runOneTest("dateTimeTrim01") }
  @Test def test_dateTimeTrim02() { runner.runOneTest("dateTimeTrim02") }
  @Test def test_dateTimeTrim03() { runner.runOneTest("dateTimeTrim03") }
  @Test def test_dateTimeTrim04() { runner.runOneTest("dateTimeTrim04") }

  @Test def test_dateTimePattern01() { runner.runOneTest("dateTimePattern01") }
  @Test def test_dateTimePattern02() { runner.runOneTest("dateTimePattern02") }
  @Test def test_dateTimePattern03() { runner.runOneTest("dateTimePattern03") }

  @Test def test_dateEpochFillIn() { runner.runOneTest("dateEpochFillIn") }
  @Test def test_dateEpochFillIn2() { runner.runOneTest("dateEpochFillIn2") }
  @Test def test_dateEpochFillIn3() { runner.runOneTest("dateEpochFillIn3") }
  @Test def test_datePattern08() { runner.runOneTest("datePattern08") }
  @Test def test_datePattern08b() { runner.runOneTest("datePattern08b") }

  @Test def test_datePattern03() { runner.runOneTest("datePattern03") }
  @Test def test_datePattern04() { runner.runOneTest("datePattern04") }
  @Test def test_datePattern05() { runner.runOneTest("datePattern05") }
  @Test def test_datePattern06() { runner.runOneTest("datePattern06") }
  @Test def test_datePattern07() { runner.runOneTest("datePattern07") }

  @Test def test_datePatternChoice() { runner.runOneTest("datePatternChoice") }

  //  @Test def test_dateCalendarCenturyStart() { runner.runOneTest("dateCalendarCenturyStart") }
  //  @Test def test_dateCalendarCenturyStart2() { runner.runOneTest("dateCalendarCenturyStart2") }

  @Test def test_dateCalendarDaysInFirstWeek() { runner.runOneTest("dateCalendarDaysInFirstWeek") }
  @Test def test_dateCalendarDaysInFirstWeek2() { runner.runOneTest("dateCalendarDaysInFirstWeek2") }
  //DAFFODIL-1945 @Test def test_dateCalendarDaysInFirstWeek3() { runner.runOneTest("dateCalendarDaysInFirstWeek3") }
  @Test def test_dateCalendarDaysInFirstWeek4() { runner.runOneTest("dateCalendarDaysInFirstWeek4") }
  //DAFFODIL-1945 @Test def test_dateCalendarDaysInFirstWeek5() { runner.runOneTest("dateCalendarDaysInFirstWeek5") }

  @Test def test_timeSymbols() { runner.runOneTest("timeSymbols") }
  @Test def test_timeSymbols2() { runner.runOneTest("timeSymbols2") }
  @Test def test_epochFillIn() { runner.runOneTest("epochFillIn") }
  @Test def test_epochFillIn2() { runner.runOneTest("epochFillIn2") }
  @Test def test_epochFillIn3() { runner.runOneTest("epochFillIn3") }

  @Test def test_timeTrim01() { runner.runOneTest("timeTrim01") }
  @Test def test_timeTrim02() { runner.runOneTest("timeTrim02") }
  @Test def test_millisecondAccuracy() { runner.runOneTest("millisecondAccuracy") }
  @Test def test_millisecondAccuracy2() { runner.runOneTest("millisecondAccuracy2") }
  @Test def test_millisecondAccuracy3() { runner.runOneTest("millisecondAccuracy3") }
  @Test def test_millisecondAccuracy4() { runner.runOneTest("millisecondAccuracy4") }
  @Test def test_millisecondAccuracy5() { runner.runOneTest("millisecondAccuracy5") }
  @Test def test_millisecondAccuracy6() { runner.runOneTest("millisecondAccuracy6") }
  @Test def test_millisecondAccuracy7() { runner.runOneTest("millisecondAccuracy7") }

  @Test def test_timeFormatting() { runner.runOneTest("timeFormatting") }
  @Test def test_timeFormatting2() { runner.runOneTest("timeFormatting2") }
  @Test def test_timeFormatting2c() { runner.runOneTest("timeFormatting2c") }
  @Test def test_timeFormatting2b() { runner.runOneTest("timeFormatting2b") }
  @Test def test_timeFormatting3() { runner.runOneTest("timeFormatting3") }
  @Test def test_timeFormatting4() { runner.runOneTest("timeFormatting4") }
  @Test def test_timeFormatting6() { runner.runOneTest("timeFormatting6") }
  @Test def test_timeFormatting7() { runner.runOneTest("timeFormatting7") }

  @Test def test_timeCalendarTimeZone() { runner.runOneTest("timeCalendarTimeZone") }
  @Test def test_timeCalendarTimeZone2() { runner.runOneTest("timeCalendarTimeZone2") }
  @Test def test_timeCalendarTimeZone3() { runner.runOneTest("timeCalendarTimeZone3") }

  @Test def test_timeZoneFormats() { runner.runOneTest("timeZoneFormats") }
  @Test def test_timeZoneFormats2() { runner.runOneTest("timeZoneFormats2") }
  @Test def test_timeZoneFormats3() { runner.runOneTest("timeZoneFormats3") }
  @Test def test_timeZoneFormats4() { runner.runOneTest("timeZoneFormats4") }
  @Test def test_timeZoneFormats5() { runner.runOneTest("timeZoneFormats5") }
  @Test def test_timeZoneFormats6() { runner.runOneTest("timeZoneFormats6") }
  @Test def test_timeZoneFormats7() { runner.runOneTest("timeZoneFormats7") }
  @Test def test_timeZoneFormats8() { runner.runOneTest("timeZoneFormats8") }
  @Test def test_timeZoneFormats9() { runner.runOneTest("timeZoneFormats9") }

  //  @Test def test_dateCountDeterminesFormat() { runner.runOneTest("dateCountDeterminesFormat") }
  @Test def test_dateNonAlphaChars01() { runner.runOneTest("dateNonAlphaChars01") }
  @Test def test_dateTrim01() { runner.runOneTest("dateTrim01") }
  @Test def test_dateTrim02() { runner.runOneTest("dateTrim02") }
  @Test def test_dateTrim03() { runner.runOneTest("dateTrim03") }

  @Test def test_dateCalendarFirstDayOfWeek01() { runner.runOneTest("dateCalendarFirstDayOfWeek01") }
  @Test def test_dateCalendarFirstDayOfWeek02() { runner.runOneTest("dateCalendarFirstDayOfWeek02") }
  //DAFFODIL-1945 @Test def test_dateCalendarFirstDayOfWeek03() { runner.runOneTest("dateCalendarFirstDayOfWeek03") }
  //DAFFODIL-1945 @Test def test_dateCalendarFirstDayOfWeek04() { runner.runOneTest("dateCalendarFirstDayOfWeek04") }
  @Test def test_timeFractionalSeconds01() { runner.runOneTest("timeFractionalSeconds01") }
  @Test def test_dateText() { runner.runOneTest("dateText") }
  @Test def test_timeText() { runner.runOneTest("timeText") }
  @Test def test_dateTimeText() { runner.runOneTest("dateTimeText") }
  @Test def test_dateImplicitPattern() { runner.runOneTest("dateImplicitPattern") }
  @Test def test_dateImplicitPatternFail() { runner.runOneTest("dateImplicitPatternFail") }
  @Test def test_timeImplicitPattern() { runner.runOneTest("timeImplicitPattern") }
  @Test def test_timeImplicitPatternFail() { runner.runOneTest("timeImplicitPatternFail") }
  @Test def test_dateTimeBin() { runner.runOneTest("dateTimeBin") }
  @Test def test_dateTimeBin2() { runner.runOneTest("dateTimeBin2") }
  @Test def test_dateTimeBin3() { runner.runOneTest("dateTimeBin3") }
  @Test def test_dateTimeBin4() { runner.runOneTest("dateTimeBin4") }
  @Test def test_dateTimeBin5() { runner.runOneTest("dateTimeBin5") }
  @Test def test_dateTimeBin6() { runner.runOneTest("dateTimeBin6") }
  @Test def test_dateTimeBin7() { runner.runOneTest("dateTimeBin7") }
  @Test def test_dateTimeBin8() { runner.runOneTest("dateTimeBin8") }
  @Test def test_dateTimeBin9() { runner.runOneTest("dateTimeBin9") }
  @Test def test_dateTimeBin10() { runner.runOneTest("dateTimeBin10") }
  @Test def test_dateTimeBin11() { runner.runOneTest("dateTimeBin11") }
  @Test def test_dateTimeBin12() { runner.runOneTest("dateTimeBin12") }
  @Test def test_dateTimeBin13() { runner.runOneTest("dateTimeBin13") }
  @Test def test_dateTimeBin14() { runner.runOneTest("dateTimeBin14") }
  @Test def test_dateTimeBin15() { runner.runOneTest("dateTimeBin15") }
  @Test def test_dateTimeBin16() { runner.runOneTest("dateTimeBin16") }
  @Test def test_dateTimeBin17() { runner.runOneTest("dateTimeBin17") }
  @Test def test_dateTimeBin18() { runner.runOneTest("dateTimeBin18") }
  @Test def test_dateTimeBin19() { runner.runOneTest("dateTimeBin19") }
  @Test def test_dateTimeBin20() { runner.runOneTest("dateTimeBin20") }
  @Test def test_dateTimeBin21() { runner.runOneTest("dateTimeBin21") }
  @Test def test_dateTimeBin22() { runner.runOneTest("dateTimeBin22") }
  @Test def test_dateBinBCD() { runner.runOneTest("dateBinBCD") }
  @Test def test_dateBinBCD2() { runner.runOneTest("dateBinBCD2") }
  @Test def test_dateBinBCD3() { runner.runOneTest("dateBinBCD3") }
  @Test def test_dateBinBCD4() { runner.runOneTest("dateBinBCD4") }
  @Test def test_dateBinBCD5() { runner.runOneTest("dateBinBCD5") }
  @Test def test_dateBinBCD6() { runner.runOneTest("dateBinBCD6") }
  @Test def test_dateBinBCD7() { runner.runOneTest("dateBinBCD7") }
  @Test def test_dateBinBCD8() { runner.runOneTest("dateBinBCD8") }
  @Test def test_dateBinBCD9() { runner.runOneTest("dateBinBCD9") }
  @Test def test_timeBinBCD() { runner.runOneTest("timeBinBCD") }
  @Test def test_timeBinBCD2() { runner.runOneTest("timeBinBCD2") }
  @Test def test_timeBinBCD3() { runner.runOneTest("timeBinBCD3") }
  @Test def test_timeBinBCD4() { runner.runOneTest("timeBinBCD4") }
  @Test def test_timeBinBCD5() { runner.runOneTest("timeBinBCD5") }
  @Test def test_timeBinBCD6() { runner.runOneTest("timeBinBCD6") }
  @Test def test_dateTimeBinBCD() { runner.runOneTest("dateTimeBinBCD") }
  @Test def test_dateTimeBinBCD2() { runner.runOneTest("dateTimeBinBCD2") }
  @Test def test_dateTimeBinBCD3() { runner.runOneTest("dateTimeBinBCD3") }
  @Test def test_dateTimeBinBCD4() { runner.runOneTest("dateTimeBinBCD4") }

  @Test def test_dateBinIBM4690Packed() { runner.runOneTest("dateBinIBM4690Packed") }
  @Test def test_dateBinIBM4690Packed2() { runner.runOneTest("dateBinIBM4690Packed2") }
  @Test def test_dateBinIBM4690Packed3() { runner.runOneTest("dateBinIBM4690Packed3") }
  @Test def test_dateTimeBinIBM4690Packed() { runner.runOneTest("dateTimeBinIBM4690Packed") }
  @Test def test_dateTimeBinIBM4690Packed2() { runner.runOneTest("dateTimeBinIBM4690Packed2") }
  @Test def test_dateTimeBinIBM4690Packed3() { runner.runOneTest("dateTimeBinIBM4690Packed3") }
  @Test def test_timeBinIBM4690Packed() { runner.runOneTest("timeBinIBM4690Packed") }
  @Test def test_timeBinIBM4690Packed2() { runner.runOneTest("timeBinIBM4690Packed2") }

  @Test def test_dateBinPacked() { runner.runOneTest("dateBinPacked") }
  @Test def test_dateBinPacked2() { runner.runOneTest("dateBinPacked2") }
  @Test def test_dateBinPacked3() { runner.runOneTest("dateBinPacked3") }
  @Test def test_dateBinPacked4() { runner.runOneTest("dateBinPacked4") }
  @Test def test_timeBinPacked() { runner.runOneTest("timeBinPacked") }
  @Test def test_timeBinPacked2() { runner.runOneTest("timeBinPacked2") }
  @Test def test_dateTimeBinPacked() { runner.runOneTest("dateTimeBinPacked") }
  @Test def test_dateTimeBinPacked2() { runner.runOneTest("dateTimeBinPacked2") }
  @Test def test_dateTimeBinPacked3() { runner.runOneTest("dateTimeBinPacked3") }

  @Test def test_dateBinInvalid() { runner.runOneTest("dateBinInvalid") }

  @Test def test_dateTimeImplicitPattern() { runner.runOneTest("dateTimeImplicitPattern") }
  @Test def test_dateTimeImplicitPatternFail() { runner.runOneTest("dateTimeImplicitPatternFail") }
  @Test def test_dateTimeImplicitPatternFail2() { runner.runOneTest("dateTimeImplicitPatternFail2") }
  @Test def test_dateTimeImplicitPatternFail3() { runner.runOneTest("dateTimeImplicitPatternFail3") }
  @Test def test_dateTimeImplicitPatternFail4() { runner.runOneTest("dateTimeImplicitPatternFail4") }
  @Test def test_dateTimeImplicitPatternFail5() { runner.runOneTest("dateTimeImplicitPatternFail5") }
  @Test def test_dateTimeImplicitPatternFail6() { runner.runOneTest("dateTimeImplicitPatternFail6") }

  @Test def test_datePattern01() { runner.runOneTest("datePattern01") }
  //  @Test def test_datePattern01b() { runner.runOneTest("datePattern01b") }
  @Test def test_timeLaxCheckPolicy01() { runner.runOneTest("timeLaxCheckPolicy01") }
  @Test def test_timeLaxCheckPolicy02() { runner.runOneTest("timeLaxCheckPolicy02") }
  @Test def test_timeLaxCheckPolicy03() { runner.runOneTest("timeLaxCheckPolicy03") }
  @Test def test_dateTimeLaxCheckPolicy01() { runner.runOneTest("dateTimeLaxCheckPolicy01") }
  @Test def test_dateLaxCheckPolicy01() { runner.runOneTest("dateLaxCheckPolicy01") }
  @Test def test_dateLaxCheckPolicy02() { runner.runOneTest("dateLaxCheckPolicy02") }
  @Test def test_dateLaxCheckPolicy03() { runner.runOneTest("dateLaxCheckPolicy03") }
  @Test def test_dateLaxCheckPolicy04() { runner.runOneTest("dateLaxCheckPolicy04") }
  @Test def test_dateLaxCheckPolicy05() { runner.runOneTest("dateLaxCheckPolicy05") }

  // DFDL-1042 - not very strict
  // @Test def test_dateStrictCheckPolicy01() { runner.runOneTest("dateStrictCheckPolicy01") }
  // @Test def test_timeStrictCheckPolicy01() { runner.runOneTest("timeStrictCheckPolicy01") }
  // @Test def test_timeStrictCheckPolicy02() { runner.runOneTest("timeStrictCheckPolicy02") }
  // @Test def test_timeFormatting5() { runner.runOneTest("timeFormatting5") }

  @Test def test_Long1() { runner.runOneTest("Long1") }
  @Test def test_BigInteger1() { runner.runOneTest("BigInteger1") }
  @Test def test_Integer01() { runner.runOneTest("Integer01") }
  @Test def test_integer2() { runner.runOneTest("integer2") }
  @Test def test_integer_fail() { runner.runOneTest("integer_fail") }
  @Test def test_integer_fail2() { runner.runOneTest("integer_fail2") }

  @Test def test_Int01() { runner.runOneTest("Int01") }
  @Test def test_int_error() { runner.runOneTest("int_error") }

  // Test warning_exercise moved to scala-debug until warnings are implemented.

  @Test def test_UnsignedNumbers1() { runner.runOneTest("UnsignedNumbers1") }
  @Test def test_unsignedLong_01() { runner.runOneTest("unsignedLong_01") }
  @Test def test_unsignedLong_02() { runner.runOneTest("unsignedLong_02") }
  @Test def test_unsignedLong_03() { runner.runOneTest("unsignedLong_03") }
  @Test def test_Long2() { runner.runOneTest("Long2") }
  @Test def test_Long3() { runner.runOneTest("Long3") }
  @Test def test_Long4() { runner.runOneTest("Long4") }
  @Test def test_int_error_02() { runner.runOneTest("int_error_02") }
  @Test def test_int_error_03() { runner.runOneTest("int_error_03") }
  @Test def test_short_01() { runner.runOneTest("short_01") }
  @Test def test_short_02() { runner.runOneTest("short_02") }
  @Test def test_unsignedInt_01() { runner.runOneTest("unsignedInt_01") }
  @Test def test_unsignedInt_02() { runner.runOneTest("unsignedInt_02") }

  @Test def test_whiteSpaceBeforeValidInt() { runner.runOneTest("whiteSpaceBeforeValidInt") }
  @Test def test_whiteSpaceBeforeValidInteger() { runner.runOneTest("whiteSpaceBeforeValidInteger") }
  @Test def test_whiteSpaceBeforeValidLong() { runner.runOneTest("whiteSpaceBeforeValidLong") }
  @Test def test_whiteSpaceBeforeValidShort() { runner.runOneTest("whiteSpaceBeforeValidShort") }
  @Test def test_whiteSpaceBeforeValidByte() { runner.runOneTest("whiteSpaceBeforeValidByte") }
  @Test def test_whiteSpaceBeforeValidUnsignedInt() { runner.runOneTest("whiteSpaceBeforeValidUnsignedInt") }
  @Test def test_whiteSpaceBeforeValidUnsignedByte() { runner.runOneTest("whiteSpaceBeforeValidUnsignedByte") }
  @Test def test_whiteSpaceBeforeValidUnsignedLong() { runner.runOneTest("whiteSpaceBeforeValidUnsignedLong") }
  @Test def test_whiteSpaceBeforeValidUnsignedShort() { runner.runOneTest("whiteSpaceBeforeValidUnsignedShort") }

  //DFDL-845
  //@Test def test_whiteSpaceDuringValidShort() { runner.runOneTest("whiteSpaceDuringValidShort") }
  //@Test def test_whiteSpaceDuringValidInteger() { runner.runOneTest("whiteSpaceDuringValidInteger") }
  //@Test def test_whiteSpaceDuringValidUnsignedLong() { runner.runOneTest("whiteSpaceDuringValidUnsignedLong") }
  //@Test def test_whiteSpaceDuringValidUnsignedShort() { runner.runOneTest("whiteSpaceDuringValidUnsignedShort") }
  //@Test def test_whiteSpaceDuringValidUnsignedByte() { runner.runOneTest("whiteSpaceDuringValidUnsignedByte") }
  //@Test def test_whiteSpaceDuringValidUnsignedInt() { runner.runOneTest("whiteSpaceDuringValidUnsignedInt") }
  //@Test def test_whiteSpaceDuringValidInt() { runner.runOneTest("whiteSpaceDuringValidInt") }
  //@Test def test_whiteSpaceDuringValidLong() { runner.runOneTest("whiteSpaceDuringValidLong") }
  //@Test def test_whiteSpaceDuringValidByte() { runner.runOneTest("whiteSpaceDuringValidByte") }

  @Test def test_whiteSpaceAfterValidInt() { runner.runOneTest("whiteSpaceAfterValidInt") }
  @Test def test_whiteSpaceAfterValidLong() { runner.runOneTest("whiteSpaceAfterValidLong") }
  @Test def test_whiteSpaceAfterValidShort() { runner.runOneTest("whiteSpaceAfterValidShort") }
  @Test def test_whiteSpaceAfterValidUnsignedInt() { runner.runOneTest("whiteSpaceAfterValidUnsignedInt") }
  @Test def test_whiteSpaceAfterValidUnsignedShort() { runner.runOneTest("whiteSpaceAfterValidUnsignedShort") }
  @Test def test_whiteSpaceAfterValidUnsignedByte() { runner.runOneTest("whiteSpaceAfterValidUnsignedByte") }
  @Test def test_whiteSpaceAfterValidByte() { runner.runOneTest("whiteSpaceAfterValidByte") }
  @Test def test_whiteSpaceAfterValidUnsignedLong() { runner.runOneTest("whiteSpaceAfterValidUnsignedLong") }
  @Test def test_whiteSpaceAfterValidInteger() { runner.runOneTest("whiteSpaceAfterValidInteger") }

  @Test def test_characterDuringValidInt() { runner.runOneTest("characterDuringValidInt") }
  @Test def test_whiteSpaceAfterLengthExceededInt() { runner.runOneTest("whiteSpaceAfterLengthExceededInt") }
  @Test def test_whiteSpaceBeforeLengthExceededInt() { runner.runOneTest("whiteSpaceBeforeLengthExceededInt") }
  @Test def test_whiteSpaceDuringLengthExceededInt() { runner.runOneTest("whiteSpaceDuringLengthExceededInt") }
  @Test def test_characterDuringValidInteger() { runner.runOneTest("characterDuringValidInteger") }
  @Test def test_whiteSpaceAfterLengthExceededInteger() { runner.runOneTest("whiteSpaceAfterLengthExceededInteger") }
  @Test def test_whiteSpaceBeforeLengthExceededInteger() { runner.runOneTest("whiteSpaceBeforeLengthExceededInteger") }
  @Test def test_whiteSpaceDuringLengthExceededInteger() { runner.runOneTest("whiteSpaceDuringLengthExceededInteger") }
  //TODO: Find out why these test generate unexpected errors
  @Test def test_characterDuringValidLong() { runner.runOneTest("characterDuringValidLong") }
  @Test def test_whiteSpaceAfterLengthExceededLong() { runner.runOneTest("whiteSpaceAfterLengthExceededLong") }
  @Test def test_whiteSpaceBeforeLengthExceededLong() { runner.runOneTest("whiteSpaceBeforeLengthExceededLong") }
  @Test def test_whiteSpaceDuringLengthExceededLong() { runner.runOneTest("whiteSpaceDuringLengthExceededLong") }

  @Test def test_characterDuringValidShort() { runner.runOneTest("characterDuringValidShort") }
  @Test def test_whiteSpaceAfterLengthExceededShort() { runner.runOneTest("whiteSpaceAfterLengthExceededShort") }
  @Test def test_whiteSpaceBeforeLengthExceededShort() { runner.runOneTest("whiteSpaceBeforeLengthExceededShort") }
  @Test def test_whiteSpaceDuringLengthExceededShort() { runner.runOneTest("whiteSpaceDuringLengthExceededShort") }
  @Test def test_characterDuringValidByte() { runner.runOneTest("characterDuringValidByte") }
  @Test def test_whiteSpaceAfterLengthExceededByte() { runner.runOneTest("whiteSpaceAfterLengthExceededByte") }
  @Test def test_whiteSpaceBeforeLengthExceededByte() { runner.runOneTest("whiteSpaceBeforeLengthExceededByte") }
  @Test def test_whiteSpaceDuringLengthExceededByte() { runner.runOneTest("whiteSpaceDuringLengthExceededByte") }
  // TODO: Find out why these test generate unexpected errors
  @Test def test_characterDuringValidUnsignedInt() { runner.runOneTest("characterDuringValidUnsignedInt") }
  @Test def test_negativeUnsignedInt() { runner.runOneTest("negativeUnsignedInt") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedInt() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedInt") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedInt() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedInt") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedInt() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedInt") }
  @Test def test_characterDuringValidUnsignedByte() { runner.runOneTest("characterDuringValidUnsignedByte") }
  @Test def test_negativeUnsignedByte() { runner.runOneTest("negativeUnsignedByte") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedByte() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedByte") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedByte() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedByte") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedByte() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedByte") }
  @Test def test_characterDuringValidUnsignedLong() { runner.runOneTest("characterDuringValidUnsignedLong") }
  @Test def test_negativeUnsignedLong() { runner.runOneTest("negativeUnsignedLong") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedLong() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedLong") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedLong() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedLong") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedLong() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedLong") }
  // TODO: Find out why these test generate unexpected errors

  @Test def test_characterDuringValidUnsignedShort() { runner.runOneTest("characterDuringValidUnsignedShort") }
  @Test def test_negativeUnsignedShort() { runner.runOneTest("negativeUnsignedShort") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedShort() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedShort") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedShort() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedShort") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedShort() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedShort") }

  //  @Test def test_whiteSpaceAfterLengthExceeded() { runner.runOneTest("whiteSpaceAfterLengthExceeded") }
  //  @Test def test_whiteSpaceBeforeLengthExceeded() { runner.runOneTest("whiteSpaceBeforeLengthExceeded") }
  //  @Test def test_whiteSpaceDuringLengthExceeded() { runner.runOneTest("whiteSpaceDuringLengthExceeded") }

  @Test def test_unsignedShort_01() { runner.runOneTest("unsignedShort_01") }
  @Test def test_unsignedByte_01() { runner.runOneTest("unsignedByte_01") }
  @Test def test_unsignedByte_02() { runner.runOneTest("unsignedByte_02") }

  // Test range checking for signed integers too!
  @Test def test_byte_01() { runner.runOneTest("byte_01") }
  @Test def test_byte_02() { runner.runOneTest("byte_02") }

  @Test def test_signedShort_binary() { runner.runOneTest("signedShort_binary") }
  @Test def test_signedShort_binary2() { runner.runOneTest("signedShort_binary2") }

  @Test def test_unsignedShort_binary() { runner.runOneTest("unsignedShort_binary") }
  @Test def test_unsignedShort_binary2() { runner.runOneTest("unsignedShort_binary2") }

  @Test def test_unsignedLong_binary() { runner.runOneTest("unsignedLong_binary") }
  @Test def test_unsignedLong_binary2() { runner.runOneTest("unsignedLong_binary2") }

  @Test def test_signedLong_binary() { runner.runOneTest("signedLong_binary") }
  @Test def test_signedLong_binary2() { runner.runOneTest("signedLong_binary2") }

  @Test def test_unsignedInt_binary() { runner.runOneTest("unsignedInt_binary") }
  @Test def test_unsignedInt_binary2() { runner.runOneTest("unsignedInt_binary2") }

  @Test def test_double_binary_01() { runner.runOneTest("double_binary_01") }
  @Test def test_double_binary_02() { runner.runOneTest("double_binary_02") }
  @Test def test_double_binary_03() { runner.runOneTest("double_binary_03") }
  @Test def test_double_binary_04() { runner.runOneTest("double_binary_04") }
  @Test def test_double_binary_05() { runner.runOneTest("double_binary_05") }
  @Test def test_double_binary_06() { runner.runOneTest("double_binary_06") }
  @Test def test_double_binary_07() { runner.runOneTest("double_binary_07") }

  @Test def test_byte_binary_01() { runner.runOneTest("byte_binary_01") }
  @Test def test_byte_binary_02() { runner.runOneTest("byte_binary_02") }
  @Test def test_byte_binary_03() { runner.runOneTest("byte_binary_03") }
  @Test def test_byte_binary_04() { runner.runOneTest("byte_binary_04") }
  @Test def test_byte_binary_05() { runner.runOneTest("byte_binary_05") }

  @Test def test_byte_implicit() { runner.runOneTest("byte_implicit") }

  @Test def test_double_07() { runner.runOneTest("double_07") }

  @Test def test_ubyte_binary_01() { runner.runOneTest("ubyte_binary_01") }
  @Test def test_ubyte_binary_02() { runner.runOneTest("ubyte_binary_02") }
  @Test def test_ubyte_binary_03() { runner.runOneTest("ubyte_binary_03") }
  @Test def test_ubyte_binary_04() { runner.runOneTest("ubyte_binary_04") }
  @Test def test_ubyte_binary_05() { runner.runOneTest("ubyte_binary_05") }

  @Test def test_ubyte_implicit() { runner.runOneTest("ubyte_implicit") }

  @Test def test_int_binary_01() { runner.runOneTest("int_binary_01") }
  @Test def test_int_binary_02() { runner.runOneTest("int_binary_02") }
  @Test def test_int_binary_03() { runner.runOneTest("int_binary_03") }
  @Test def test_int_binary_04() { runner.runOneTest("int_binary_04") }
  @Test def test_int_binary_05() { runner.runOneTest("int_binary_05") }

  @Test def test_int_implicit() { runner.runOneTest("int_implicit") }

  //  @Test def test_posinteger_binary_01() { runner.runOneTest("nonNegInt_binary_01") }

  @Test def test_literalChar_padding() { runner.runOneTest("literalChar_padding") }
  @Test def test_literalChar_padding2() { runner.runOneTest("literalChar_padding2") }
  @Test def test_literalChar_padding3() { runner.runOneTest("literalChar_padding3") }
  @Test def test_literalChar_padding4() { runner.runOneTest("literalChar_padding4") }
  @Test def test_literalChar_padding5() { runner.runOneTest("literalChar_padding5") }
  @Test def test_literalChar_padding6() { runner.runOneTest("literalChar_padding6") }
  @Test def test_charEntity_padding1() { runner.runOneTest("charEntity_padding1") }
  @Test def test_charEntity_padding2() { runner.runOneTest("charEntity_padding2") }
  @Test def test_charEntity_padding3() { runner.runOneTest("charEntity_padding3") }
  @Test def test_number_padding() { runner.runOneTest("number_padding") }
  @Test def test_number_padding2() { runner.runOneTest("number_padding2") }
  @Test def test_number_padding3() { runner.runOneTest("number_padding3") }
  @Test def test_number_padding4() { runner.runOneTest("number_padding4") }
  @Test def test_number_padding5() { runner.runOneTest("number_padding5") }
  @Test def test_padding_escape() { runner.runOneTest("padding_escape") }
  @Test def test_padding_nil() { runner.runOneTest("padding_nil") }
  @Test def test_padding_nil2() { runner.runOneTest("padding_nil2") }
  @Test def test_padding_empty() { runner.runOneTest("padding_empty") }
  @Test def test_justification_1() { runner.runOneTest("justification_1") }

  // Verification that user's test works for DFDL-677
  @Test def test_unsignedInt() { runner.runOneTest("TestUnsignedInt") }

  @Test def test_nonNegativeInteger_text() { runner.runOneTest("nonNegativeInteger_text") }
  @Test def test_nonNegativeInteger_text2() { runner.runOneTest("nonNegativeInteger_text2") }
  @Test def test_nonNegativeInteger_text_fail() { runner.runOneTest("nonNegativeInteger_text_fail") }
  @Test def test_nonNegativeInteger_text_fail2() { runner.runOneTest("nonNegativeInteger_text_fail2") }
  @Test def test_nonNegativeInteger_text_fail3() { runner.runOneTest("nonNegativeInteger_text_fail3") }

  @Test def test_nonNegativeInteger_bin() { runner.runOneTest("nonNegativeInteger_bin") }
  @Test def test_nonNegativeInteger_bin2() { runner.runOneTest("nonNegativeInteger_bin2") }
  @Test def test_nonNegativeInteger_bin3() { runner.runOneTest("nonNegativeInteger_bin3") }
  @Test def test_nonNegativeInteger_bin4() { runner.runOneTest("nonNegativeInteger_bin4") }
  @Test def test_nonNegativeInteger_bin5() { runner.runOneTest("nonNegativeInteger_bin5") }
  @Test def test_nonNegativeInteger_bin6() { runner.runOneTest("nonNegativeInteger_bin6") }

  @Test def test_integer_binary() { runner.runOneTest("integer_binary") }
  @Test def test_integer_binary_01() { runner.runOneTest("integer_binary_01") }
  @Test def test_integer_binary_02() { runner.runOneTest("integer_binary_02") }
  @Test def test_integer_binary_03() { runner.runOneTest("integer_binary_03") }
  @Test def test_integer_binary_04() { runner.runOneTest("integer_binary_04") }

  @Test def test_decimal_text() { runner.runOneTest("decimal_text") }
  @Test def test_decimal_text2() { runner.runOneTest("decimal_text2") }
  @Test def test_decimal_text3() { runner.runOneTest("decimal_text3") }
  @Test def test_decimal_text_fail() { runner.runOneTest("decimal_text_fail") }
  @Test def test_characterDuringValidDecimal() { runner.runOneTest("characterDuringValidDecimal") }

  @Test def test_decimal_binary() { runner.runOneTest("decimal_binary") }
  @Test def test_decimal_binary_01() { runner.runOneTest("decimal_binary_01") }
  @Test def test_decimal_binary_02() { runner.runOneTest("decimal_binary_02") }
  @Test def test_decimal_binary_03() { runner.runOneTest("decimal_binary_03") }
  @Test def test_decimal_binary_04() { runner.runOneTest("decimal_binary_04") }
  @Test def test_decimal_binary_05() { runner.runOneTest("decimal_binary_05") }
  @Test def test_decimal_binary_06() { runner.runOneTest("decimal_binary_06") }
  @Test def test_decimal_binary_fail_01() { runner.runOneTest("decimal_binary_fail_01") }
  @Test def test_decimal_binary_fail_02() { runner.runOneTest("decimal_binary_fail_02") }
  @Test def test_decimal_binary_fail_03() { runner.runOneTest("decimal_binary_fail_03") }
  @Test def test_decimal_binary_fail_04() { runner.runOneTest("decimal_binary_fail_04") }

  @Test def test_double_text() { runner.runOneTest("double_text") }
  @Test def test_double_text2_parse_ab() { runner.runOneTest("double_text2_parse_ab") }
  @Test def test_double_text2_parse_ac() { runner.runOneTest("double_text2_parse_ac") }
  @Test def test_double_text2_parse_ub() { runner.runOneTest("double_text2_parse_ub") }
  @Test def test_double_text2_parse_uc() { runner.runOneTest("double_text2_parse_uc") }
  @Test def test_double_text2_unparse_ab() { runner.runOneTest("double_text2_unparse_ab") }
  @Test def test_double_text2_unparse_ac() { runner.runOneTest("double_text2_unparse_ac") }
  @Test def test_double_text2_unparse_ub() { runner.runOneTest("double_text2_unparse_ub") }
  @Test def test_double_text2_unparse_uc() { runner.runOneTest("double_text2_unparse_uc") }
  @Test def test_double_text2_unparse_fail_ab() { runner.runOneTest("double_text2_unparse_fail_ab") }
  @Test def test_double_text2_unparse_fail_ac() { runner.runOneTest("double_text2_unparse_fail_ac") }
  @Test def test_double_text2_unparse_fail_ub() { runner.runOneTest("double_text2_unparse_fail_ub") }
  @Test def test_double_text2_unparse_fail_uc() { runner.runOneTest("double_text2_unparse_fail_uc") }
  @Test def test_double_text3() { runner.runOneTest("double_text3") }
  @Test def test_double_text4() { runner.runOneTest("double_text4") }
  @Test def test_characterDuringValidDouble() { runner.runOneTest("characterDuringValidDouble") }

  @Test def test_float_text() { runner.runOneTest("float_text") }
  @Test def test_float_text2() { runner.runOneTest("float_text2") }
  @Test def test_float_text3() { runner.runOneTest("float_text3") }
  @Test def test_float_text_fail() { runner.runOneTest("float_text_fail") }
  @Test def test_characterDuringValidFloat() { runner.runOneTest("characterDuringValidFloat") }
  @Test def test_float_binary_01() { runner.runOneTest("float_binary_01") }
  @Test def test_float_binary_02() { runner.runOneTest("float_binary_02") }
  @Test def test_float_binary_03() { runner.runOneTest("float_binary_03") }
  @Test def test_float_binary_04() { runner.runOneTest("float_binary_04") }
  @Test def test_float_binary_05() { runner.runOneTest("float_binary_05") }
  @Test def test_float_binary_fail_01() { runner.runOneTest("float_binary_fail_01") }
  @Test def test_float_binary_fail_02() { runner.runOneTest("float_binary_fail_02") }
  @Test def test_float_binary_fail_03() { runner.runOneTest("float_binary_fail_03") }

}
