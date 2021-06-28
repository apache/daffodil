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

  val runner = Runner(testDir, "SimpleTypes.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestSimpleTypes {
  import TestSimpleTypes._

  @Test def test_warning_exercise(): Unit = { runner.runOneTest("warning_exercise") }

  @Test def test_one_octet(): Unit = { runner.runOneTest("OneOctetBinaryParse") }
  @Test def test_oneBit2(): Unit = { runner.runOneTest("OneBit2") }

  @Test def test_nonNegativeInteger(): Unit = { runner.runOneTest("NonNegativeInteger") }
  @Test def test_nonNegativeInteger_Fail(): Unit = { runner.runOneTest("NonNegativeInteger_Fail") }

  @Test def test_hexBinary_rep(): Unit = { runner.runOneTest("hexBinary_rep") } //roundTrip
  @Test def test_hexBinary_fromString(): Unit = { runner.runOneTest("hexBinary_fromString") } //roundTrip

  @Test def test_hexBinary_01(): Unit = { runner.runOneTest("hexBinary_01") } //roundTrip
  @Test def test_hexBinary_01_chunk(): Unit = { runner.runOneTest("hexBinary_01_chunk") } //roundTrip
  @Test def test_hexBinary_Delimited_01(): Unit = { runner.runOneTest("hexBinary_Delimited_01") } //roundTrip
  @Test def test_hexBinary_Delimited_01a(): Unit = { runner.runOneTest("hexBinary_Delimited_01a") } //roundTrip
  @Test def test_hexBinary_Delimited_02(): Unit = { runner.runOneTest("hexBinary_Delimited_02") } //roundTrip
  @Test def test_hexBinary_Delimited_03(): Unit = { runner.runOneTest("hexBinary_Delimited_03") } //roundTrip
  @Test def test_hexBinary_Implicit_01(): Unit = { runner.runOneTest("hexBinary_Implicit_01") } //roundTrip
  @Test def test_hexBinary_Implicit_02(): Unit = { runner.runOneTest("hexBinary_Implicit_02") } //roundTrip
  @Test def test_hexBinary_Implicit_03(): Unit = { runner.runOneTest("hexBinary_Implicit_03") } //roundTrip
  @Test def test_hexBinary_Implicit_03b(): Unit = { runner.runOneTest("hexBinary_Implicit_03b") } //roundTrip
  @Test def test_hexBinary_Implicit_03c(): Unit = { runner.runOneTest("hexBinary_Implicit_03c") } //roundTrip
  @Test def test_hexBinary_Implicit_04(): Unit = { runner.runOneTest("hexBinary_Implicit_04") } //roundTrip
  @Test def test_hexBinary_func(): Unit = { runner.runOneTest("hexBinary_func") }
  @Test def test_hexBinary_func_neg(): Unit = { runner.runOneTest("hexBinary_func_neg") }

  @Test def test_hexBinary_bits_be_msbf(): Unit = { runner.runOneTest("hexBinary_bits_be_msbf") }
  @Test def test_hexBinary_bits_le_msbf(): Unit = { runner.runOneTest("hexBinary_bits_le_msbf") }
  @Test def test_hexBinary_bits_le_lsbf(): Unit = { runner.runOneTest("hexBinary_bits_le_lsbf") }

  @Test def test_hexBinary_bits_be_msbf_2(): Unit = { runner.runOneTest("hexBinary_bits_be_msbf_2") }
  @Test def test_hexBinary_bits_le_msbf_2(): Unit = { runner.runOneTest("hexBinary_bits_le_msbf_2") }
  @Test def test_hexBinary_bits_le_lsbf_2(): Unit = { runner.runOneTest("hexBinary_bits_le_lsbf_2") }

  @Test def test_dateTextNumberRep(): Unit = { runner.runOneTest("dateTextNumberRep") }

  @Test def test_datePattern02(): Unit = { runner.runOneTest("datePattern02") }
  @Test def test_datePattern02b(): Unit = { runner.runOneTest("datePattern02b") }
  @Test def test_timePattern01(): Unit = { runner.runOneTest("timePattern01") }
  @Test def test_timePattern01b(): Unit = { runner.runOneTest("timePattern01b") }

  @Test def test_dateCalendarLanguage(): Unit = { runner.runOneTest("dateCalendarLanguage") }
  @Test def test_dateCalendarLanguage2(): Unit = { runner.runOneTest("dateCalendarLanguage2") }
  @Test def test_dateCalendarLanguage3(): Unit = { runner.runOneTest("dateCalendarLanguage3") }
  @Test def test_dateCalendarLanguage4(): Unit = { runner.runOneTest("dateCalendarLanguage4") }

  @Test def test_dateTimeCalendarDaysInFirstWeek(): Unit = { runner.runOneTest("dateTimeCalendarDaysInFirstWeek") }
  @Test def test_dateTimeCalendarDaysInFirstWeek2(): Unit = { runner.runOneTest("dateTimeCalendarDaysInFirstWeek2") }
  @Test def test_dateTimeCalendarDaysInFirstWeek3(): Unit = { runner.runOneTest("dateTimeCalendarDaysInFirstWeek3") }
  @Test def test_dateTimeCalendarDaysInFirstWeek4(): Unit = { runner.runOneTest("dateTimeCalendarDaysInFirstWeek4") }
  @Test def test_dateTimeCalendarDaysInFirstWeek5(): Unit = { runner.runOneTest("dateTimeCalendarDaysInFirstWeek5") }
  @Test def test_dateTimeCalendarDaysInFirstWeek6(): Unit = { runner.runOneTest("dateTimeCalendarDaysInFirstWeek6") }

  @Test def test_dateTimeTrim01(): Unit = { runner.runOneTest("dateTimeTrim01") }
  @Test def test_dateTimeTrim02(): Unit = { runner.runOneTest("dateTimeTrim02") }
  @Test def test_dateTimeTrim03(): Unit = { runner.runOneTest("dateTimeTrim03") }
  @Test def test_dateTimeTrim04(): Unit = { runner.runOneTest("dateTimeTrim04") }

  @Test def test_dateTimePattern01(): Unit = { runner.runOneTest("dateTimePattern01") }
  @Test def test_dateTimePattern02(): Unit = { runner.runOneTest("dateTimePattern02") }
  @Test def test_dateTimePattern03(): Unit = { runner.runOneTest("dateTimePattern03") }

  @Test def test_dateEpochFillIn(): Unit = { runner.runOneTest("dateEpochFillIn") }
  @Test def test_dateEpochFillIn2(): Unit = { runner.runOneTest("dateEpochFillIn2") }
  @Test def test_dateEpochFillIn3(): Unit = { runner.runOneTest("dateEpochFillIn3") }
  @Test def test_datePattern08(): Unit = { runner.runOneTest("datePattern08") }
  @Test def test_datePattern08b(): Unit = { runner.runOneTest("datePattern08b") }

  @Test def test_datePattern03(): Unit = { runner.runOneTest("datePattern03") }
  @Test def test_datePattern04(): Unit = { runner.runOneTest("datePattern04") }
  @Test def test_datePattern05(): Unit = { runner.runOneTest("datePattern05") }
  @Test def test_datePattern06(): Unit = { runner.runOneTest("datePattern06") }
  @Test def test_datePattern07(): Unit = { runner.runOneTest("datePattern07") }

  @Test def test_datePatternChoice(): Unit = { runner.runOneTest("datePatternChoice") }

  // DFDL-519
  //@Test def test_dateCalendarCenturyStart() { runner.runOneTest("dateCalendarCenturyStart") }
  //@Test def test_dateCalendarCenturyStart2() { runner.runOneTest("dateCalendarCenturyStart2") }

  @Test def test_dateCalendarDaysInFirstWeek(): Unit = { runner.runOneTest("dateCalendarDaysInFirstWeek") }
  @Test def test_dateCalendarDaysInFirstWeek2(): Unit = { runner.runOneTest("dateCalendarDaysInFirstWeek2") }
  @Test def test_dateCalendarDaysInFirstWeek3(): Unit =  { runner.runOneTest("dateCalendarDaysInFirstWeek3") }
  @Test def test_dateCalendarDaysInFirstWeek4(): Unit = { runner.runOneTest("dateCalendarDaysInFirstWeek4") }
  @Test def test_dateCalendarDaysInFirstWeek5(): Unit =  { runner.runOneTest("dateCalendarDaysInFirstWeek5") }

  @Test def test_timeSymbols(): Unit = { runner.runOneTest("timeSymbols") }
  @Test def test_timeSymbols2(): Unit = { runner.runOneTest("timeSymbols2") }
  @Test def test_epochFillIn(): Unit = { runner.runOneTest("epochFillIn") }
  @Test def test_epochFillIn2(): Unit = { runner.runOneTest("epochFillIn2") }
  @Test def test_epochFillIn3(): Unit = { runner.runOneTest("epochFillIn3") }

  @Test def test_timeTrim01(): Unit = { runner.runOneTest("timeTrim01") }
  @Test def test_timeTrim02(): Unit = { runner.runOneTest("timeTrim02") }
  @Test def test_millisecondAccuracy(): Unit = { runner.runOneTest("millisecondAccuracy") }
  @Test def test_millisecondAccuracy2(): Unit = { runner.runOneTest("millisecondAccuracy2") }
  @Test def test_millisecondAccuracy3(): Unit = { runner.runOneTest("millisecondAccuracy3") }
  @Test def test_millisecondAccuracy4(): Unit = { runner.runOneTest("millisecondAccuracy4") }
  @Test def test_millisecondAccuracy5(): Unit = { runner.runOneTest("millisecondAccuracy5") }
  @Test def test_millisecondAccuracy6(): Unit = { runner.runOneTest("millisecondAccuracy6") }
  @Test def test_millisecondAccuracy7(): Unit = { runner.runOneTest("millisecondAccuracy7") }

  @Test def test_timeFormatting(): Unit = { runner.runOneTest("timeFormatting") }
  @Test def test_timeFormatting2(): Unit = { runner.runOneTest("timeFormatting2") }
  @Test def test_timeFormatting2c(): Unit = { runner.runOneTest("timeFormatting2c") }
  @Test def test_timeFormatting2b(): Unit = { runner.runOneTest("timeFormatting2b") }
  @Test def test_timeFormatting3(): Unit = { runner.runOneTest("timeFormatting3") }
  @Test def test_timeFormatting4(): Unit = { runner.runOneTest("timeFormatting4") }
  @Test def test_timeFormatting6(): Unit = { runner.runOneTest("timeFormatting6") }
  @Test def test_timeFormatting7(): Unit = { runner.runOneTest("timeFormatting7") }

  @Test def test_timeCalendarTimeZone(): Unit = { runner.runOneTest("timeCalendarTimeZone") }
  @Test def test_timeCalendarTimeZone2(): Unit = { runner.runOneTest("timeCalendarTimeZone2") }
  @Test def test_timeCalendarTimeZone3(): Unit = { runner.runOneTest("timeCalendarTimeZone3") }
  @Test def test_timeCalendarTimeZone3_unparse(): Unit = { runner.runOneTest("timeCalendarTimeZone3_unparse") }

  @Test def test_timeZoneFormats(): Unit = { runner.runOneTest("timeZoneFormats") }
  @Test def test_timeZoneFormats2(): Unit = { runner.runOneTest("timeZoneFormats2") }
  @Test def test_timeZoneFormats3(): Unit = { runner.runOneTest("timeZoneFormats3") }
  @Test def test_timeZoneFormats4(): Unit = { runner.runOneTest("timeZoneFormats4") }
  @Test def test_timeZoneFormats5(): Unit = { runner.runOneTest("timeZoneFormats5") }
  @Test def test_timeZoneFormats6(): Unit = { runner.runOneTest("timeZoneFormats6") }
  @Test def test_timeZoneFormats6_2(): Unit = { runner.runOneTest("timeZoneFormats6_2") }
  @Test def test_timeZoneFormats7(): Unit = { runner.runOneTest("timeZoneFormats7") }
  @Test def test_timeZoneFormats8(): Unit = { runner.runOneTest("timeZoneFormats8") }
  @Test def test_timeZoneFormats9(): Unit = { runner.runOneTest("timeZoneFormats9") }
  @Test def test_timeZoneFormats_unparse(): Unit = { runner.runOneTest("timeZoneFormats_unparse") }
  @Test def test_timeZoneFormats2_unparse(): Unit = { runner.runOneTest("timeZoneFormats2_unparse") }
  @Test def test_timeZoneFormats3_unparse(): Unit = { runner.runOneTest("timeZoneFormats3_unparse") }
  @Test def test_timeZoneFormats4_unparse(): Unit = { runner.runOneTest("timeZoneFormats4_unparse") }
  @Test def test_timeZoneFormats5_unparse(): Unit = { runner.runOneTest("timeZoneFormats5_unparse") }
  @Test def test_timeZoneFormats6_unparse(): Unit = { runner.runOneTest("timeZoneFormats6_unparse") }
  @Test def test_timeZoneFormats6_2_unparse(): Unit = { runner.runOneTest("timeZoneFormats6_2_unparse") }
  @Test def test_timeZoneFormats7_unparse(): Unit = { runner.runOneTest("timeZoneFormats7_unparse") }
  @Test def test_timeZoneFormats9_unparse(): Unit = { runner.runOneTest("timeZoneFormats9_unparse") }

  @Test def test_dateCountDeterminesFormat(): Unit = { runner.runOneTest("dateCountDeterminesFormat") }
  @Test def test_dateNonAlphaChars01(): Unit = { runner.runOneTest("dateNonAlphaChars01") }
  @Test def test_dateTrim01(): Unit = { runner.runOneTest("dateTrim01") }
  @Test def test_dateTrim02(): Unit = { runner.runOneTest("dateTrim02") }
  @Test def test_dateTrim03(): Unit = { runner.runOneTest("dateTrim03") }

  @Test def test_dateCalendarFirstDayOfWeek01(): Unit = { runner.runOneTest("dateCalendarFirstDayOfWeek01") }
  @Test def test_dateCalendarFirstDayOfWeek02(): Unit = { runner.runOneTest("dateCalendarFirstDayOfWeek02") }
  @Test def test_dateCalendarFirstDayOfWeek03(): Unit = { runner.runOneTest("dateCalendarFirstDayOfWeek03") }
  @Test def test_dateCalendarFirstDayOfWeek04(): Unit = { runner.runOneTest("dateCalendarFirstDayOfWeek04") }
  @Test def test_timeFractionalSeconds01(): Unit = { runner.runOneTest("timeFractionalSeconds01") }
  @Test def test_dateText(): Unit = { runner.runOneTest("dateText") }
  @Test def test_dateTextInvalid(): Unit = { runner.runOneTest("dateTextInvalid") }
  @Test def test_timeText(): Unit = { runner.runOneTest("timeText") }
  @Test def test_timeTextInvalid(): Unit = { runner.runOneTest("timeTextInvalid") }
  @Test def test_dateTimeText(): Unit = { runner.runOneTest("dateTimeText") }
  @Test def test_dateTimeTextInvalid(): Unit = { runner.runOneTest("dateTimeTextInvalid") }
  @Test def test_dateImplicitPattern(): Unit = { runner.runOneTest("dateImplicitPattern") }
  @Test def test_dateImplicitPatternFail(): Unit = { runner.runOneTest("dateImplicitPatternFail") }
  @Test def test_timeImplicitPattern(): Unit = { runner.runOneTest("timeImplicitPattern") }
  @Test def test_timeImplicitPatternFail(): Unit = { runner.runOneTest("timeImplicitPatternFail") }
  @Test def test_dateTimeBin(): Unit = { runner.runOneTest("dateTimeBin") }
  @Test def test_dateTimeBin2(): Unit = { runner.runOneTest("dateTimeBin2") }
  @Test def test_dateTimeBin3(): Unit = { runner.runOneTest("dateTimeBin3") }
  @Test def test_dateTimeBin4(): Unit = { runner.runOneTest("dateTimeBin4") }
  @Test def test_dateTimeBin5(): Unit = { runner.runOneTest("dateTimeBin5") }
  @Test def test_dateTimeBin6(): Unit = { runner.runOneTest("dateTimeBin6") }
  @Test def test_dateTimeBin7(): Unit = { runner.runOneTest("dateTimeBin7") }
  @Test def test_dateTimeBin8(): Unit = { runner.runOneTest("dateTimeBin8") }
  @Test def test_dateTimeBin9(): Unit = { runner.runOneTest("dateTimeBin9") }
  @Test def test_dateTimeBin10(): Unit = { runner.runOneTest("dateTimeBin10") }
  @Test def test_dateTimeBin11(): Unit = { runner.runOneTest("dateTimeBin11") }
  @Test def test_dateTimeBin12(): Unit = { runner.runOneTest("dateTimeBin12") }
  @Test def test_dateTimeBin13(): Unit = { runner.runOneTest("dateTimeBin13") }
  @Test def test_dateTimeBin14(): Unit = { runner.runOneTest("dateTimeBin14") }
  @Test def test_dateTimeBin15(): Unit = { runner.runOneTest("dateTimeBin15") }
  @Test def test_dateTimeBin16(): Unit = { runner.runOneTest("dateTimeBin16") }
  @Test def test_dateTimeBin17(): Unit = { runner.runOneTest("dateTimeBin17") }
  @Test def test_dateTimeBin18(): Unit = { runner.runOneTest("dateTimeBin18") }
  @Test def test_dateTimeBin19(): Unit = { runner.runOneTest("dateTimeBin19") }
  @Test def test_dateTimeBin20(): Unit = { runner.runOneTest("dateTimeBin20") }
  @Test def test_dateTimeBin21(): Unit = { runner.runOneTest("dateTimeBin21") }
  @Test def test_dateTimeBin22(): Unit = { runner.runOneTest("dateTimeBin22") }
  @Test def test_dateBinBCD(): Unit = { runner.runOneTest("dateBinBCD") }
  @Test def test_dateBinBCD2(): Unit = { runner.runOneTest("dateBinBCD2") }
  @Test def test_dateBinBCD3(): Unit = { runner.runOneTest("dateBinBCD3") }
  @Test def test_dateBinBCD4(): Unit = { runner.runOneTest("dateBinBCD4") }
  @Test def test_dateBinBCD5(): Unit = { runner.runOneTest("dateBinBCD5") }
  @Test def test_dateBinBCD6(): Unit = { runner.runOneTest("dateBinBCD6") }
  @Test def test_dateBinBCD7(): Unit = { runner.runOneTest("dateBinBCD7") }
  @Test def test_dateBinBCD8(): Unit = { runner.runOneTest("dateBinBCD8") }
  @Test def test_dateBinBCD9(): Unit = { runner.runOneTest("dateBinBCD9") }
  @Test def test_timeBinBCD(): Unit = { runner.runOneTest("timeBinBCD") }
  @Test def test_timeBinBCD2(): Unit = { runner.runOneTest("timeBinBCD2") }
  @Test def test_timeBinBCD3(): Unit = { runner.runOneTest("timeBinBCD3") }
  @Test def test_timeBinBCD4(): Unit = { runner.runOneTest("timeBinBCD4") }
  @Test def test_timeBinBCD5(): Unit = { runner.runOneTest("timeBinBCD5") }
  @Test def test_timeBinBCD6(): Unit = { runner.runOneTest("timeBinBCD6") }
  @Test def test_dateTimeBinBCD(): Unit = { runner.runOneTest("dateTimeBinBCD") }
  @Test def test_dateTimeBinBCD2(): Unit = { runner.runOneTest("dateTimeBinBCD2") }
  @Test def test_dateTimeBinBCD3(): Unit = { runner.runOneTest("dateTimeBinBCD3") }
  @Test def test_dateTimeBinBCD4(): Unit = { runner.runOneTest("dateTimeBinBCD4") }

  @Test def test_dateBinIBM4690Packed(): Unit = { runner.runOneTest("dateBinIBM4690Packed") }
  @Test def test_dateBinIBM4690Packed2(): Unit = { runner.runOneTest("dateBinIBM4690Packed2") }
  @Test def test_dateBinIBM4690Packed3(): Unit = { runner.runOneTest("dateBinIBM4690Packed3") }
  @Test def test_dateTimeBinIBM4690Packed(): Unit = { runner.runOneTest("dateTimeBinIBM4690Packed") }
  @Test def test_dateTimeBinIBM4690Packed2(): Unit = { runner.runOneTest("dateTimeBinIBM4690Packed2") }
  @Test def test_dateTimeBinIBM4690Packed3(): Unit = { runner.runOneTest("dateTimeBinIBM4690Packed3") }
  @Test def test_timeBinIBM4690Packed(): Unit = { runner.runOneTest("timeBinIBM4690Packed") }
  @Test def test_timeBinIBM4690Packed2(): Unit = { runner.runOneTest("timeBinIBM4690Packed2") }

  @Test def test_dateBinPacked(): Unit = { runner.runOneTest("dateBinPacked") }
  @Test def test_dateBinPacked2(): Unit = { runner.runOneTest("dateBinPacked2") }
  @Test def test_dateBinPacked4(): Unit = { runner.runOneTest("dateBinPacked4") }
  @Test def test_timeBinPacked(): Unit = { runner.runOneTest("timeBinPacked") }
  @Test def test_timeBinPacked2(): Unit = { runner.runOneTest("timeBinPacked2") }
  @Test def test_dateTimeBinPacked(): Unit = { runner.runOneTest("dateTimeBinPacked") }
  @Test def test_dateTimeBinPacked2(): Unit = { runner.runOneTest("dateTimeBinPacked2") }
  @Test def test_dateTimeBinPacked3(): Unit = { runner.runOneTest("dateTimeBinPacked3") }

  @Test def test_dateBinInvalid(): Unit = { runner.runOneTest("dateBinInvalid") }

  @Test def test_dateTimeImplicitPattern(): Unit = { runner.runOneTest("dateTimeImplicitPattern") }
  @Test def test_dateTimeImplicitPatternFail(): Unit = { runner.runOneTest("dateTimeImplicitPatternFail") }
  @Test def test_dateTimeImplicitPatternFail2(): Unit = { runner.runOneTest("dateTimeImplicitPatternFail2") }
  @Test def test_dateTimeImplicitPatternFail3(): Unit = { runner.runOneTest("dateTimeImplicitPatternFail3") }
  @Test def test_dateTimeImplicitPatternFail4(): Unit = { runner.runOneTest("dateTimeImplicitPatternFail4") }
  @Test def test_dateTimeImplicitPatternFail5(): Unit = { runner.runOneTest("dateTimeImplicitPatternFail5") }
  @Test def test_dateTimeImplicitPatternFail6(): Unit = { runner.runOneTest("dateTimeImplicitPatternFail6") }

  @Test def test_datePattern01(): Unit = { runner.runOneTest("datePattern01") }
  // DAFFODIL-488
  // @Test def test_datePattern01b() { runner.runOneTest("datePattern01b") }
  @Test def test_timeLaxCheckPolicy01(): Unit = { runner.runOneTest("timeLaxCheckPolicy01") }
  @Test def test_timeLaxCheckPolicy02(): Unit = { runner.runOneTest("timeLaxCheckPolicy02") }
  @Test def test_timeLaxCheckPolicy03(): Unit = { runner.runOneTest("timeLaxCheckPolicy03") }
  @Test def test_dateTimeLaxCheckPolicy01(): Unit = { runner.runOneTest("dateTimeLaxCheckPolicy01") }
  @Test def test_dateLaxCheckPolicy01(): Unit = { runner.runOneTest("dateLaxCheckPolicy01") }
  @Test def test_dateLaxCheckPolicy02(): Unit = { runner.runOneTest("dateLaxCheckPolicy02") }
  @Test def test_dateLaxCheckPolicy03(): Unit = { runner.runOneTest("dateLaxCheckPolicy03") }
  @Test def test_dateLaxCheckPolicy04(): Unit = { runner.runOneTest("dateLaxCheckPolicy04") }
  @Test def test_dateLaxCheckPolicy05(): Unit = { runner.runOneTest("dateLaxCheckPolicy05") }

  // DFDL-1042 - not very strict
  // @Test def test_dateStrictCheckPolicy01() { runner.runOneTest("dateStrictCheckPolicy01") }
  // @Test def test_timeStrictCheckPolicy01() { runner.runOneTest("timeStrictCheckPolicy01") }
  // @Test def test_timeStrictCheckPolicy02() { runner.runOneTest("timeStrictCheckPolicy02") }
  // @Test def test_timeFormatting5() { runner.runOneTest("timeFormatting5") }

  @Test def test_Long1(): Unit = { runner.runOneTest("Long1") }
  @Test def test_BigInteger1(): Unit = { runner.runOneTest("BigInteger1") }
  @Test def test_Integer01(): Unit = { runner.runOneTest("Integer01") }
  @Test def test_integer2(): Unit = { runner.runOneTest("integer2") }
  @Test def test_integer_fail(): Unit = { runner.runOneTest("integer_fail") }
  @Test def test_integer_fail2(): Unit = { runner.runOneTest("integer_fail2") }

  @Test def test_Int01(): Unit = { runner.runOneTest("Int01") }
  @Test def test_int_error(): Unit = { runner.runOneTest("int_error") }

  // Test warning_exercise moved to scala-debug until warnings are implemented.

  @Test def test_UnsignedNumbers1(): Unit = { runner.runOneTest("UnsignedNumbers1") }
  @Test def test_unsignedLong_01(): Unit = { runner.runOneTest("unsignedLong_01") }
  @Test def test_unsignedLong_02(): Unit = { runner.runOneTest("unsignedLong_02") }
  @Test def test_unsignedLong_03(): Unit = { runner.runOneTest("unsignedLong_03") }
  @Test def test_Long2(): Unit = { runner.runOneTest("Long2") }
  @Test def test_Long3(): Unit = { runner.runOneTest("Long3") }
  @Test def test_Long4(): Unit = { runner.runOneTest("Long4") }
  @Test def test_int_error_02(): Unit = { runner.runOneTest("int_error_02") }
  @Test def test_int_error_03(): Unit = { runner.runOneTest("int_error_03") }
  @Test def test_short_01(): Unit = { runner.runOneTest("short_01") }
  @Test def test_short_02(): Unit = { runner.runOneTest("short_02") }
  @Test def test_unsignedInt_01(): Unit = { runner.runOneTest("unsignedInt_01") }
  @Test def test_unsignedInt_02(): Unit = { runner.runOneTest("unsignedInt_02") }

  @Test def test_whiteSpaceBeforeValidInt(): Unit = { runner.runOneTest("whiteSpaceBeforeValidInt") }
  @Test def test_whiteSpaceBeforeValidInteger(): Unit = { runner.runOneTest("whiteSpaceBeforeValidInteger") }
  @Test def test_whiteSpaceBeforeValidLong(): Unit = { runner.runOneTest("whiteSpaceBeforeValidLong") }
  @Test def test_whiteSpaceBeforeValidShort(): Unit = { runner.runOneTest("whiteSpaceBeforeValidShort") }
  @Test def test_whiteSpaceBeforeValidByte(): Unit = { runner.runOneTest("whiteSpaceBeforeValidByte") }
  @Test def test_whiteSpaceBeforeValidUnsignedInt(): Unit = { runner.runOneTest("whiteSpaceBeforeValidUnsignedInt") }
  @Test def test_whiteSpaceBeforeValidUnsignedByte(): Unit = { runner.runOneTest("whiteSpaceBeforeValidUnsignedByte") }
  @Test def test_whiteSpaceBeforeValidUnsignedLong(): Unit = { runner.runOneTest("whiteSpaceBeforeValidUnsignedLong") }
  @Test def test_whiteSpaceBeforeValidUnsignedShort(): Unit = { runner.runOneTest("whiteSpaceBeforeValidUnsignedShort") }

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

  @Test def test_whiteSpaceAfterValidInt(): Unit = { runner.runOneTest("whiteSpaceAfterValidInt") }
  @Test def test_whiteSpaceAfterValidLong(): Unit = { runner.runOneTest("whiteSpaceAfterValidLong") }
  @Test def test_whiteSpaceAfterValidShort(): Unit = { runner.runOneTest("whiteSpaceAfterValidShort") }
  @Test def test_whiteSpaceAfterValidUnsignedInt(): Unit = { runner.runOneTest("whiteSpaceAfterValidUnsignedInt") }
  @Test def test_whiteSpaceAfterValidUnsignedShort(): Unit = { runner.runOneTest("whiteSpaceAfterValidUnsignedShort") }
  @Test def test_whiteSpaceAfterValidUnsignedByte(): Unit = { runner.runOneTest("whiteSpaceAfterValidUnsignedByte") }
  @Test def test_whiteSpaceAfterValidByte(): Unit = { runner.runOneTest("whiteSpaceAfterValidByte") }
  @Test def test_whiteSpaceAfterValidUnsignedLong(): Unit = { runner.runOneTest("whiteSpaceAfterValidUnsignedLong") }
  @Test def test_whiteSpaceAfterValidInteger(): Unit = { runner.runOneTest("whiteSpaceAfterValidInteger") }

  @Test def test_characterDuringValidInt(): Unit = { runner.runOneTest("characterDuringValidInt") }
  @Test def test_int_unparseError(): Unit = { runner.runOneTest("int_unparseError") }
  @Test def test_whiteSpaceAfterLengthExceededInt(): Unit = { runner.runOneTest("whiteSpaceAfterLengthExceededInt") }
  @Test def test_whiteSpaceBeforeLengthExceededInt(): Unit = { runner.runOneTest("whiteSpaceBeforeLengthExceededInt") }
  @Test def test_whiteSpaceDuringLengthExceededInt(): Unit = { runner.runOneTest("whiteSpaceDuringLengthExceededInt") }
  @Test def test_characterDuringValidInteger(): Unit = { runner.runOneTest("characterDuringValidInteger") }
  @Test def test_integer_unparseError(): Unit = { runner.runOneTest("integer_unparseError") }
  @Test def test_whiteSpaceAfterLengthExceededInteger(): Unit = { runner.runOneTest("whiteSpaceAfterLengthExceededInteger") }
  @Test def test_whiteSpaceBeforeLengthExceededInteger(): Unit = { runner.runOneTest("whiteSpaceBeforeLengthExceededInteger") }
  @Test def test_whiteSpaceDuringLengthExceededInteger(): Unit = { runner.runOneTest("whiteSpaceDuringLengthExceededInteger") }
  @Test def test_characterDuringValidLong(): Unit = { runner.runOneTest("characterDuringValidLong") }
  @Test def test_long_unparseError(): Unit = { runner.runOneTest("long_unparseError") }
  @Test def test_whiteSpaceAfterLengthExceededLong(): Unit = { runner.runOneTest("whiteSpaceAfterLengthExceededLong") }
  @Test def test_whiteSpaceBeforeLengthExceededLong(): Unit = { runner.runOneTest("whiteSpaceBeforeLengthExceededLong") }
  @Test def test_whiteSpaceDuringLengthExceededLong(): Unit = { runner.runOneTest("whiteSpaceDuringLengthExceededLong") }

  @Test def test_characterDuringValidShort(): Unit = { runner.runOneTest("characterDuringValidShort") }
  @Test def test_short_unparseError(): Unit = { runner.runOneTest("short_unparseError") }
  @Test def test_whiteSpaceAfterLengthExceededShort(): Unit = { runner.runOneTest("whiteSpaceAfterLengthExceededShort") }
  @Test def test_whiteSpaceBeforeLengthExceededShort(): Unit = { runner.runOneTest("whiteSpaceBeforeLengthExceededShort") }
  @Test def test_whiteSpaceDuringLengthExceededShort(): Unit = { runner.runOneTest("whiteSpaceDuringLengthExceededShort") }
  @Test def test_characterDuringValidByte(): Unit = { runner.runOneTest("characterDuringValidByte") }
  @Test def test_byte_unparseError(): Unit = { runner.runOneTest("byte_unparseError") }
  @Test def test_whiteSpaceAfterLengthExceededByte(): Unit = { runner.runOneTest("whiteSpaceAfterLengthExceededByte") }
  @Test def test_whiteSpaceBeforeLengthExceededByte(): Unit = { runner.runOneTest("whiteSpaceBeforeLengthExceededByte") }
  @Test def test_whiteSpaceDuringLengthExceededByte(): Unit = { runner.runOneTest("whiteSpaceDuringLengthExceededByte") }
  @Test def test_characterDuringValidUnsignedInt(): Unit = { runner.runOneTest("characterDuringValidUnsignedInt") }
  @Test def test_negativeUnsignedInt(): Unit = { runner.runOneTest("negativeUnsignedInt") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedInt(): Unit = { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedInt") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedInt(): Unit = { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedInt") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedInt(): Unit = { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedInt") }
  @Test def test_characterDuringValidUnsignedByte(): Unit = { runner.runOneTest("characterDuringValidUnsignedByte") }
  @Test def test_unsignedByte_unparseError(): Unit = { runner.runOneTest("unsignedByte_unparseError") }
  @Test def test_negativeUnsignedByte(): Unit = { runner.runOneTest("negativeUnsignedByte") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedByte(): Unit = { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedByte") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedByte(): Unit = { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedByte") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedByte(): Unit = { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedByte") }
  @Test def test_characterDuringValidUnsignedLong(): Unit = { runner.runOneTest("characterDuringValidUnsignedLong") }
  @Test def test_unsignedLong_unparseError(): Unit = { runner.runOneTest("unsignedLong_unparseError") }
  @Test def test_negativeUnsignedLong(): Unit = { runner.runOneTest("negativeUnsignedLong") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedLong(): Unit = { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedLong") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedLong(): Unit = { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedLong") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedLong(): Unit = { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedLong") }

  @Test def test_characterDuringValidUnsignedShort(): Unit = { runner.runOneTest("characterDuringValidUnsignedShort") }
  @Test def test_unsignedShort_unparseError(): Unit = { runner.runOneTest("unsignedShort_unparseError") }
  @Test def test_negativeUnsignedShort(): Unit = { runner.runOneTest("negativeUnsignedShort") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedShort(): Unit = { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedShort") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedShort(): Unit = { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedShort") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedShort(): Unit = { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedShort") }

  @Test def test_unsignedShort_01(): Unit = { runner.runOneTest("unsignedShort_01") }
  @Test def test_unsignedByte_01(): Unit = { runner.runOneTest("unsignedByte_01") }
  @Test def test_unsignedByte_02(): Unit = { runner.runOneTest("unsignedByte_02") }

  // Test range checking for signed integers too!
  @Test def test_byte_01(): Unit = { runner.runOneTest("byte_01") }
  @Test def test_byte_02(): Unit = { runner.runOneTest("byte_02") }

  @Test def test_signedShort_binary(): Unit = { runner.runOneTest("signedShort_binary") }
  @Test def test_signedShort_binary2(): Unit = { runner.runOneTest("signedShort_binary2") }

  @Test def test_unsignedShort_binary(): Unit = { runner.runOneTest("unsignedShort_binary") }
  @Test def test_unsignedShort_binary2(): Unit = { runner.runOneTest("unsignedShort_binary2") }

  @Test def test_unsignedLong_binary(): Unit = { runner.runOneTest("unsignedLong_binary") }
  @Test def test_unsignedLong_binary2(): Unit = { runner.runOneTest("unsignedLong_binary2") }

  @Test def test_signedLong_binary(): Unit = { runner.runOneTest("signedLong_binary") }
  @Test def test_signedLong_binary2(): Unit = { runner.runOneTest("signedLong_binary2") }

  @Test def test_unsignedInt_binary(): Unit = { runner.runOneTest("unsignedInt_binary") }
  @Test def test_unsignedInt_binary2(): Unit = { runner.runOneTest("unsignedInt_binary2") }

  @Test def test_double_binary_01(): Unit = { runner.runOneTest("double_binary_01") }
  @Test def test_double_binary_02(): Unit = { runner.runOneTest("double_binary_02") }
  @Test def test_double_binary_03(): Unit = { runner.runOneTest("double_binary_03") }
  @Test def test_double_binary_04(): Unit = { runner.runOneTest("double_binary_04") }
  @Test def test_double_binary_05(): Unit = { runner.runOneTest("double_binary_05") }
  @Test def test_double_binary_06(): Unit = { runner.runOneTest("double_binary_06") }
  @Test def test_double_binary_07(): Unit = { runner.runOneTest("double_binary_07") }

  @Test def test_byte_binary_01(): Unit = { runner.runOneTest("byte_binary_01") }
  @Test def test_byte_binary_02(): Unit = { runner.runOneTest("byte_binary_02") }
  @Test def test_byte_binary_03(): Unit = { runner.runOneTest("byte_binary_03") }
  @Test def test_byte_binary_04(): Unit = { runner.runOneTest("byte_binary_04") }
  @Test def test_byte_binary_05(): Unit = { runner.runOneTest("byte_binary_05") }

  @Test def test_byte_implicit(): Unit = { runner.runOneTest("byte_implicit") }

  @Test def test_double_07(): Unit = { runner.runOneTest("double_07") }

  @Test def test_ubyte_binary_01(): Unit = { runner.runOneTest("ubyte_binary_01") }
  @Test def test_ubyte_binary_02(): Unit = { runner.runOneTest("ubyte_binary_02") }
  @Test def test_ubyte_binary_03(): Unit = { runner.runOneTest("ubyte_binary_03") }
  @Test def test_ubyte_binary_04(): Unit = { runner.runOneTest("ubyte_binary_04") }
  @Test def test_ubyte_binary_05(): Unit = { runner.runOneTest("ubyte_binary_05") }

  @Test def test_ubyte_implicit(): Unit = { runner.runOneTest("ubyte_implicit") }

  @Test def test_int_binary_01(): Unit = { runner.runOneTest("int_binary_01") }
  @Test def test_int_binary_02(): Unit = { runner.runOneTest("int_binary_02") }
  @Test def test_int_binary_03(): Unit = { runner.runOneTest("int_binary_03") }
  @Test def test_int_binary_04(): Unit = { runner.runOneTest("int_binary_04") }
  @Test def test_int_binary_05(): Unit = { runner.runOneTest("int_binary_05") }

  @Test def test_binaryInt_unparseError(): Unit = { runner.runOneTest("binaryInt_unparseError") }
  @Test def test_hexBinary_unparseError(): Unit = { runner.runOneTest("hexBinary_unparseError") }
  @Test def test_calendar_unparseError(): Unit = { runner.runOneTest("calendar_unparseError") }

  @Test def test_int_implicit(): Unit = { runner.runOneTest("int_implicit") }

  @Test def test_posinteger_binary_01(): Unit = { runner.runOneTest("nonNegInt_binary_01") }

  @Test def test_literalChar_padding(): Unit = { runner.runOneTest("literalChar_padding") }
  @Test def test_literalChar_padding2(): Unit = { runner.runOneTest("literalChar_padding2") }
  @Test def test_literalChar_padding3(): Unit = { runner.runOneTest("literalChar_padding3") }
  @Test def test_literalChar_padding4(): Unit = { runner.runOneTest("literalChar_padding4") }
  @Test def test_literalChar_padding5(): Unit = { runner.runOneTest("literalChar_padding5") }
  @Test def test_literalChar_padding6(): Unit = { runner.runOneTest("literalChar_padding6") }
  @Test def test_charEntity_padding1(): Unit = { runner.runOneTest("charEntity_padding1") }
  @Test def test_charEntity_padding2(): Unit = { runner.runOneTest("charEntity_padding2") }
  @Test def test_charEntity_padding3(): Unit = { runner.runOneTest("charEntity_padding3") }
  @Test def test_number_padding(): Unit = { runner.runOneTest("number_padding") }
  @Test def test_number_padding2(): Unit = { runner.runOneTest("number_padding2") }
  @Test def test_number_padding3(): Unit = { runner.runOneTest("number_padding3") }
  @Test def test_number_padding4(): Unit = { runner.runOneTest("number_padding4") }
  @Test def test_number_padding5(): Unit = { runner.runOneTest("number_padding5") }
  @Test def test_padding_escape(): Unit = { runner.runOneTest("padding_escape") }
  @Test def test_padding_nil(): Unit = { runner.runOneTest("padding_nil") }
  @Test def test_padding_nil2(): Unit = { runner.runOneTest("padding_nil2") }
  @Test def test_padding_empty(): Unit = { runner.runOneTest("padding_empty") }
  @Test def test_justification_1(): Unit = { runner.runOneTest("justification_1") }
  @Test def test_percentPadding(): Unit = { runner.runOneTest("percentPadding") }

  // Verification that user's test works for DFDL-677
  @Test def test_unsignedInt(): Unit = { runner.runOneTest("TestUnsignedInt") }

  @Test def test_nonNegativeInteger_text(): Unit = { runner.runOneTest("nonNegativeInteger_text") }
  @Test def test_nonNegativeInteger_text2(): Unit = { runner.runOneTest("nonNegativeInteger_text2") }
  @Test def test_nonNegativeInteger_text_fail(): Unit = { runner.runOneTest("nonNegativeInteger_text_fail") }
  @Test def test_nonNegativeInteger_text_fail2(): Unit = { runner.runOneTest("nonNegativeInteger_text_fail2") }
  @Test def test_nonNegativeInteger_text_fail3(): Unit = { runner.runOneTest("nonNegativeInteger_text_fail3") }
  @Test def test_nonNegativeInteger_unparseError(): Unit = { runner.runOneTest("nonNegativeInteger_unparseError") }

  @Test def test_nonNegativeInteger_bin(): Unit = { runner.runOneTest("nonNegativeInteger_bin") }
  @Test def test_nonNegativeInteger_bin2(): Unit = { runner.runOneTest("nonNegativeInteger_bin2") }
  @Test def test_nonNegativeInteger_bin3(): Unit = { runner.runOneTest("nonNegativeInteger_bin3") }
  @Test def test_nonNegativeInteger_bin4(): Unit = { runner.runOneTest("nonNegativeInteger_bin4") }
  @Test def test_nonNegativeInteger_bin5(): Unit = { runner.runOneTest("nonNegativeInteger_bin5") }
  @Test def test_nonNegativeInteger_bin6(): Unit = { runner.runOneTest("nonNegativeInteger_bin6") }

  @Test def test_integer_binary(): Unit = { runner.runOneTest("integer_binary") }
  @Test def test_integer_binary_01(): Unit = { runner.runOneTest("integer_binary_01") }
  @Test def test_integer_binary_02(): Unit = { runner.runOneTest("integer_binary_02") }
  @Test def test_integer_binary_03(): Unit = { runner.runOneTest("integer_binary_03") }
  @Test def test_integer_binary_04(): Unit = { runner.runOneTest("integer_binary_04") }

  @Test def test_decimal_text(): Unit = { runner.runOneTest("decimal_text") }
  @Test def test_decimal_text2(): Unit = { runner.runOneTest("decimal_text2") }
  @Test def test_decimal_text3(): Unit = { runner.runOneTest("decimal_text3") }
  @Test def test_decimal_text_fail(): Unit = { runner.runOneTest("decimal_text_fail") }
  @Test def test_characterDuringValidDecimal(): Unit = { runner.runOneTest("characterDuringValidDecimal") }
  @Test def test_decimal_unparseError(): Unit = { runner.runOneTest("decimal_unparseError") }

  @Test def test_decimal_binary(): Unit = { runner.runOneTest("decimal_binary") }
  @Test def test_decimal_binary_01(): Unit = { runner.runOneTest("decimal_binary_01") }
  @Test def test_decimal_binary_02(): Unit = { runner.runOneTest("decimal_binary_02") }
  @Test def test_decimal_binary_03(): Unit = { runner.runOneTest("decimal_binary_03") }
  @Test def test_decimal_binary_04(): Unit = { runner.runOneTest("decimal_binary_04") }
  @Test def test_decimal_binary_05(): Unit = { runner.runOneTest("decimal_binary_05") }
  @Test def test_decimal_binary_06(): Unit = { runner.runOneTest("decimal_binary_06") }
  @Test def test_decimal_binary_fail_01(): Unit = { runner.runOneTest("decimal_binary_fail_01") }
  @Test def test_decimal_binary_fail_02(): Unit = { runner.runOneTest("decimal_binary_fail_02") }
  @Test def test_decimal_binary_fail_03(): Unit = { runner.runOneTest("decimal_binary_fail_03") }
  @Test def test_decimal_binary_fail_04(): Unit = { runner.runOneTest("decimal_binary_fail_04") }

  @Test def test_double_text(): Unit = { runner.runOneTest("double_text") }
  @Test def test_double_text2_parse_ab(): Unit = { runner.runOneTest("double_text2_parse_ab") }
  @Test def test_double_text2_parse_ac(): Unit = { runner.runOneTest("double_text2_parse_ac") }
  @Test def test_double_text2_parse_ub(): Unit = { runner.runOneTest("double_text2_parse_ub") }
  @Test def test_double_text2_parse_uc(): Unit = { runner.runOneTest("double_text2_parse_uc") }
  @Test def test_double_text2_unparse_ab(): Unit = { runner.runOneTest("double_text2_unparse_ab") }
  @Test def test_double_text2_unparse_ac(): Unit = { runner.runOneTest("double_text2_unparse_ac") }
  @Test def test_double_text2_unparse_ub(): Unit = { runner.runOneTest("double_text2_unparse_ub") }
  @Test def test_double_text2_unparse_uc(): Unit = { runner.runOneTest("double_text2_unparse_uc") }
  @Test def test_double_text2_unparse_fail_ab(): Unit = { runner.runOneTest("double_text2_unparse_fail_ab") }
  @Test def test_double_text2_unparse_fail_ac(): Unit = { runner.runOneTest("double_text2_unparse_fail_ac") }
  @Test def test_double_text2_unparse_fail_ub(): Unit = { runner.runOneTest("double_text2_unparse_fail_ub") }
  @Test def test_double_text2_unparse_fail_uc(): Unit = { runner.runOneTest("double_text2_unparse_fail_uc") }
  @Test def test_double_text3(): Unit = { runner.runOneTest("double_text3") }
  @Test def test_double_text4(): Unit = { runner.runOneTest("double_text4") }
  @Test def test_characterDuringValidDouble(): Unit = { runner.runOneTest("characterDuringValidDouble") }
  @Test def test_double_unparseError(): Unit = { runner.runOneTest("double_unparseError") }

  @Test def test_float_text(): Unit = { runner.runOneTest("float_text") }
  @Test def test_float_text2(): Unit = { runner.runOneTest("float_text2") }
  @Test def test_float_text3(): Unit = { runner.runOneTest("float_text3") }
  @Test def test_float_text_fail(): Unit = { runner.runOneTest("float_text_fail") }
  @Test def test_characterDuringValidFloat(): Unit = { runner.runOneTest("characterDuringValidFloat") }
  @Test def test_float_unparseError(): Unit = { runner.runOneTest("float_unparseError") }
  @Test def test_float_binary_01(): Unit = { runner.runOneTest("float_binary_01") }
  @Test def test_float_binary_02(): Unit = { runner.runOneTest("float_binary_02") }
  @Test def test_float_binary_03(): Unit = { runner.runOneTest("float_binary_03") }
  @Test def test_float_binary_04(): Unit = { runner.runOneTest("float_binary_04") }
  @Test def test_float_binary_05(): Unit = { runner.runOneTest("float_binary_05") }
  @Test def test_float_binary_fail_01(): Unit = { runner.runOneTest("float_binary_fail_01") }
  @Test def test_float_binary_fail_02(): Unit = { runner.runOneTest("float_binary_fail_02") }
  @Test def test_float_binary_fail_03(): Unit = { runner.runOneTest("float_binary_fail_03") }
  
  @Test def test_time_calendarTimeZone_EmptyString(): Unit = { runner.runOneTest("time_calendarTimeZone_EmptyString") }
  @Test def test_time_calendarTimeZone_EST(): Unit = { runner.runOneTest("time_calendarTimeZone_EST") }
  @Test def test_date_calendarTimeZone_EmptyString(): Unit = { runner.runOneTest("date_calendarTimeZone_EmptyString") }
  @Test def test_date_calendarTimeZone_EST(): Unit = { runner.runOneTest("date_calendarTimeZone_EST") }
  @Test def test_dateTime_calendarTimeZone_EmptyString(): Unit = { runner.runOneTest("dateTime_calendarTimeZone_EmptyString") }
  @Test def test_dateTime_calendarTimeZone_EST(): Unit = { runner.runOneTest("dateTime_calendarTimeZone_EST") }

  @Test def test_hexBinary_specifiedLengthUnaligned(): Unit = { runner.runOneTest("hexBinary_specifiedLengthUnaligned") }

}
