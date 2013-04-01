package edu.illinois.ncsa.daffodil.section05.simple_types

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */


import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File

class TestSimpleTypes extends JUnitSuite {
  val testDir = "/edu/illinois/ncsa/daffodil/section05/simple_types/"
  val aa = testDir + "SimpleTypes.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_datePattern02() { runner.runOneTest("datePattern02") }
  @Test def test_datePattern02b() { runner.runOneTest("datePattern02b") }
  @Test def test_timePattern01() { runner.runOneTest("timePattern01") }
  @Test def test_timePattern01b() { runner.runOneTest("timePattern01b") }

//  @Test def test_dateCalendarLanguage() { runner.runOneTest("dateCalendarLanguage") }
//  @Test def test_dateCalendarLanguage2() { runner.runOneTest("dateCalendarLanguage2") }
//  @Test def test_dateCalendarLanguage3() { runner.runOneTest("dateCalendarLanguage3") }
  
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
//  @Test def test_dateEpochFillIn2() { runner.runOneTest("dateEpochFillIn2") }
  @Test def test_dateEpochFillIn3() { runner.runOneTest("dateEpochFillIn3") }
  @Test def test_datePattern08() { runner.runOneTest("datePattern08") }
  @Test def test_datePattern08b() { runner.runOneTest("datePattern08b") }

  @Test def test_datePattern03() { runner.runOneTest("datePattern03") }
//  @Test def test_datePattern04() { runner.runOneTest("datePattern04") }
  @Test def test_datePattern05() { runner.runOneTest("datePattern05") }
  @Test def test_datePattern06() { runner.runOneTest("datePattern06") }
  @Test def test_datePattern07() { runner.runOneTest("datePattern07") }

  @Test def test_datePatternChoice() { runner.runOneTest("datePatternChoice") }
  
//  @Test def test_dateCalendarCenturyStart() { runner.runOneTest("dateCalendarCenturyStart") }
//  @Test def test_dateCalendarCenturyStart() { runner.runOneTest("dateCalendarCenturyStart2") }
  
  @Test def test_dateCalendarDaysInFirstWeek() { runner.runOneTest("dateCalendarDaysInFirstWeek") }
  @Test def test_dateCalendarDaysInFirstWeek2() { runner.runOneTest("dateCalendarDaysInFirstWeek2") }
  @Test def test_dateCalendarDaysInFirstWeek3() { runner.runOneTest("dateCalendarDaysInFirstWeek3") }
  @Test def test_dateCalendarDaysInFirstWeek4() { runner.runOneTest("dateCalendarDaysInFirstWeek4") }
  @Test def test_dateCalendarDaysInFirstWeek5() { runner.runOneTest("dateCalendarDaysInFirstWeek5") }

  @Test def test_timeSymbols() { runner.runOneTest("timeSymbols") }
  @Test def test_timeSymbols2() { runner.runOneTest("timeSymbols2") }
  @Test def test_epochFillIn() { runner.runOneTest("epochFillIn") }
//  @Test def test_epochFillIn2() { runner.runOneTest("epochFillIn2") }
  @Test def test_epochFillIn3() { runner.runOneTest("epochFillIn3") }

  @Test def test_timeTrim01() { runner.runOneTest("timeTrim01") }
  @Test def test_timeTrim02() { runner.runOneTest("timeTrim02") }
  @Test def test_millisecondAccuracy() { runner.runOneTest("millisecondAccuracy") }
  @Test def test_millisecondAccuracy2() { runner.runOneTest("millisecondAccuracy2") }
  @Test def test_millisecondAccuracy3() { runner.runOneTest("millisecondAccuracy3") }
//  @Test def test_millisecondAccuracy4() { runner.runOneTest("millisecondAccuracy4") }
//  @Test def test_timeLaxCheckPolicy02() { runner.runOneTest("timeLaxCheckPolicy02") }
//  @Test def test_timeLaxCheckPolicy03() { runner.runOneTest("timeLaxCheckPolicy03") }
  @Test def test_timeStrictCheckPolicy02() { runner.runOneTest("timeStrictCheckPolicy02") }

  @Test def test_timeFormatting() { runner.runOneTest("timeFormatting") }
  @Test def test_timeFormatting2() { runner.runOneTest("timeFormatting2") }
  @Test def test_timeFormatting2c() { runner.runOneTest("timeFormatting2c") }
  @Test def test_timeFormatting2b() { runner.runOneTest("timeFormatting2b") }
  @Test def test_timeFormatting3() { runner.runOneTest("timeFormatting3") }
  @Test def test_timeFormatting4() { runner.runOneTest("timeFormatting4") }
  @Test def test_timeFormatting5() { runner.runOneTest("timeFormatting5") }
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
  @Test def test_dateCalendarFirstDayOfWeek03() { runner.runOneTest("dateCalendarFirstDayOfWeek03") }
  @Test def test_dateCalendarFirstDayOfWeek04() { runner.runOneTest("dateCalendarFirstDayOfWeek04") }
//  @Test def test_timeFractionalSeconds01() { runner.runOneTest("timeFractionalSeconds01") }
  @Test def test_dateText() { runner.runOneTest("dateText") }
  @Test def test_timeText() { runner.runOneTest("timeText") }
  @Test def test_dateTimeText() { runner.runOneTest("dateTimeText") }
  @Test def test_dateImplicitPattern() { runner.runOneTest("dateImplicitPattern") }
  @Test def test_dateImplicitPatternFail() { runner.runOneTest("dateImplicitPatternFail") }
//  @Test def test_timeImplicitPattern() { runner.runOneTest("timeImplicitPattern") }
  @Test def test_timeImplicitPatternFail() { runner.runOneTest("timeImplicitPatternFail") }

  @Test def test_dateTimeImplicitPattern() { runner.runOneTest("dateTimeImplicitPattern") }
  @Test def test_dateTimeImplicitPatternFail() { runner.runOneTest("dateTimeImplicitPatternFail") }
  @Test def test_dateTimeImplicitPattern2() { runner.runOneTest("dateTimeImplicitPatternFail2") }
  @Test def test_dateTimeImplicitPattern3() { runner.runOneTest("dateTimeImplicitPatternFail3") }
  @Test def test_dateTimeImplicitPattern4() { runner.runOneTest("dateTimeImplicitPatternFail4") }

  @Test def test_datePattern01() { runner.runOneTest("datePattern01") }
//  @Test def test_datePattern01b() { runner.runOneTest("datePattern01b") }
//  @Test def test_timeLaxCheckPolicy01() { runner.runOneTest("timeLaxCheckPolicy01") }
//  @Test def test_dateTimeLaxCheckPolicy01() { runner.runOneTest("dateTimeLaxCheckPolicy01") }
  @Test def test_dateLaxCheckPolicy01() { runner.runOneTest("dateLaxCheckPolicy01") }
  @Test def test_dateLaxCheckPolicy02() { runner.runOneTest("dateLaxCheckPolicy02") }
  @Test def test_dateLaxCheckPolicy03() { runner.runOneTest("dateLaxCheckPolicy03") }
  @Test def test_dateLaxCheckPolicy04() { runner.runOneTest("dateLaxCheckPolicy04") }
  @Test def test_dateLaxCheckPolicy05() { runner.runOneTest("dateLaxCheckPolicy05") }
  @Test def test_dateStrictCheckPolicy01() { runner.runOneTest("dateStrictCheckPolicy01") }
  @Test def test_timeStrictCheckPolicy01() { runner.runOneTest("timeStrictCheckPolicy01") }

  @Test def test_Long1() { runner.runOneTest("Long1") }
  @Test def test_BigInteger1() { runner.runOneTest("BigInteger1") }
  @Test def test_Integer01() { runner.runOneTest("Integer01") }
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

  @Test def test_characterDuringValidInt() { runner.runOneTest("characterDuringValidInt") }
  @Test def test_whiteSpaceAfterLengthExceededInt() { runner.runOneTest("whiteSpaceAfterLengthExceededInt") }
  @Test def test_whiteSpaceBeforeLengthExceededInt() { runner.runOneTest("whiteSpaceBeforeLengthExceededInt") }
  @Test def test_whiteSpaceDuringLengthExceededInt() { runner.runOneTest("whiteSpaceDuringLengthExceededInt") }
  @Test def test_whiteSpaceAfterValidInt() { runner.runOneTest("whiteSpaceAfterValidInt") }
  @Test def test_characterDuringValidInteger() { runner.runOneTest("characterDuringValidInteger") }
  @Test def test_whiteSpaceAfterLengthExceededInteger() { runner.runOneTest("whiteSpaceAfterLengthExceededInteger") }
  @Test def test_whiteSpaceBeforeLengthExceededInteger() { runner.runOneTest("whiteSpaceBeforeLengthExceededInteger") }
  @Test def test_whiteSpaceDuringLengthExceededInteger() { runner.runOneTest("whiteSpaceDuringLengthExceededInteger") }
  @Test def test_whiteSpaceBeforeValidInteger() { runner.runOneTest("whiteSpaceBeforeValidInteger") }
//TODO: Find out why these test generate unexpected errors
  @Test def test_whiteSpaceDuringValidInteger() { runner.runOneTest("whiteSpaceDuringValidInteger") }
  @Test def test_whiteSpaceAfterValidInteger() { runner.runOneTest("whiteSpaceAfterValidInteger") }
  @Test def test_characterDuringValidLong() { runner.runOneTest("characterDuringValidLong") }
  @Test def test_whiteSpaceAfterLengthExceededLong() { runner.runOneTest("whiteSpaceAfterLengthExceededLong") }
  @Test def test_whiteSpaceBeforeLengthExceededLong() { runner.runOneTest("whiteSpaceBeforeLengthExceededLong") }
  @Test def test_whiteSpaceDuringLengthExceededLong() { runner.runOneTest("whiteSpaceDuringLengthExceededLong") }
  @Test def test_whiteSpaceAfterValidLong() { runner.runOneTest("whiteSpaceAfterValidLong") }

  @Test def test_characterDuringValidShort() { runner.runOneTest("characterDuringValidShort") }
  @Test def test_whiteSpaceAfterLengthExceededShort() { runner.runOneTest("whiteSpaceAfterLengthExceededShort") }
  @Test def test_whiteSpaceBeforeLengthExceededShort() { runner.runOneTest("whiteSpaceBeforeLengthExceededShort") }
  @Test def test_whiteSpaceDuringLengthExceededShort() { runner.runOneTest("whiteSpaceDuringLengthExceededShort") }
  @Test def test_whiteSpaceAfterValidShort() { runner.runOneTest("whiteSpaceAfterValidShort") }
  @Test def test_characterDuringValidByte() { runner.runOneTest("characterDuringValidByte") }
  @Test def test_whiteSpaceAfterLengthExceededByte() { runner.runOneTest("whiteSpaceAfterLengthExceededByte") }
  @Test def test_whiteSpaceBeforeLengthExceededByte() { runner.runOneTest("whiteSpaceBeforeLengthExceededByte") }
  @Test def test_whiteSpaceDuringLengthExceededByte() { runner.runOneTest("whiteSpaceDuringLengthExceededByte") }
  @Test def test_whiteSpaceBeforeValidByte() { runner.runOneTest("whiteSpaceBeforeValidByte") }
// TODO: Find out why these test generate unexpected errors
  @Test def test_whiteSpaceDuringValidByte() { runner.runOneTest("whiteSpaceDuringValidByte") }
  @Test def test_whiteSpaceAfterValidByte() { runner.runOneTest("whiteSpaceAfterValidByte") }
  @Test def test_characterDuringValidUnsignedInt() { runner.runOneTest("characterDuringValidUnsignedInt") }
  @Test def test_negativeUnsignedInt() { runner.runOneTest("negativeUnsignedInt") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedInt() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedInt") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedInt() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedInt") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedInt() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedInt") }
  @Test def test_whiteSpaceAfterValidUnsignedInt() { runner.runOneTest("whiteSpaceAfterValidUnsignedInt") }

  @Test def test_characterDuringValidUnsignedByte() { runner.runOneTest("characterDuringValidUnsignedByte") }
  @Test def test_negativeUnsignedByte() { runner.runOneTest("negativeUnsignedByte") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedByte() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedByte") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedByte() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedByte") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedByte() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedByte") }
  @Test def test_whiteSpaceAfterValidUnsignedByte() { runner.runOneTest("whiteSpaceAfterValidUnsignedByte") }
  @Test def test_characterDuringValidUnsignedLong() { runner.runOneTest("characterDuringValidUnsignedLong") }
  @Test def test_negativeUnsignedLong() { runner.runOneTest("negativeUnsignedLong") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedLong() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedLong") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedLong() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedLong") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedLong() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedLong") }
  @Test def test_whiteSpaceBeforeValidUnsignedLong() { runner.runOneTest("whiteSpaceBeforeValidUnsignedLong") }
// TODO: Find out why these test generate unexpected errors
  @Test def test_whiteSpaceDuringValidUnsignedLong() { runner.runOneTest("whiteSpaceDuringValidUnsignedLong") }
  @Test def test_whiteSpaceAfterValidUnsignedLong() { runner.runOneTest("whiteSpaceAfterValidUnsignedLong") }

  @Test def test_characterDuringValidUnsignedShort() { runner.runOneTest("characterDuringValidUnsignedShort") }
  @Test def test_negativeUnsignedShort() { runner.runOneTest("negativeUnsignedShort") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedShort() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedShort") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedShort() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedShort") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedShort() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedShort") }
  @Test def test_whiteSpaceAfterValidUnsignedShort() { runner.runOneTest("whiteSpaceAfterValidUnsignedShort") }

  //  @Test def test_whiteSpaceAfterLengthExceeded() { runner.runOneTest("whiteSpaceAfterLengthExceeded") }
  //  @Test def test_whiteSpaceBeforeLengthExceeded() { runner.runOneTest("whiteSpaceBeforeLengthExceeded") }
  //  @Test def test_whiteSpaceDuringLengthExceeded() { runner.runOneTest("whiteSpaceDuringLengthExceeded") }
  //  @Test def test_whiteSpaceAfterValidValue() { runner.runOneTest("whiteSpaceAfterValidValue") }

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

  val aj = testDir + "AJ.tdml"
  lazy val runnerAJ = new DFDLTestSuite(Misc.getRequiredResource(aj))

  @Test def test_AJ000() { runnerAJ.runOneTest("AJ000") }
  @Test def test_AJ001() { runnerAJ.runOneTest("AJ001") }
  
  val ak = testDir + "AK.tdml"
  lazy val runnerAK = new DFDLTestSuite(Misc.getRequiredResource(ak))
  @Test def test_AK000() { runnerAK.runOneTest("AK000") }
  @Test def test_AK001() { runnerAK.runOneTest("AK001") }

  val testDir_01 = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  val aa_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(aa_01))
  @Test def test_schema_types_5_01() { runner_01.runOneTest("schema_types_5_01") }
  @Test def test_schema_types_5_02() { runner_01.runOneTest("schema_types_5_02") }
  @Test def test_schema_types_5_03() { runner_01.runOneTest("schema_types_5_03") }
  @Test def test_schema_types_5_04() { runner_01.runOneTest("schema_types_5_04") }
  @Test def test_schema_types_5_05() { runner_01.runOneTest("schema_types_5_05") }

  @Test def test_double_binary_01() { runner.runOneTest("double_binary_01") }
  @Test def test_double_binary_02() { runner.runOneTest("double_binary_02") }
  @Test def test_double_binary_03() { runner.runOneTest("double_binary_03") }
  @Test def test_double_binary_04() { runner.runOneTest("double_binary_04") }
  @Test def test_double_binary_05() { runner.runOneTest("double_binary_05") }
  
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
  
  @Test def test_integer_binary_01() { runner.runOneTest("integer_binary_01") }

//  @Test def test_posinteger_binary_01() { runner.runOneTest("nonNegInt_binary_01") }
  
//  @Test def test_hexBinary_01() { runner.runOneTest("hexBinary_01") }

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
  @Test def test_justification_1() { runner.runOneTest("justification_1") }

}
