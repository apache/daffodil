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

package org.apache.daffodil.core.grammar.primitives

import java.util.regex.PatternSyntaxException

import org.apache.daffodil.lib.Implicits.intercept

import com.ibm.icu.text.DecimalFormat
import com.ibm.icu.text.DecimalFormatSymbols
import com.ibm.icu.util.ULocale
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class TestPrimitives {

  @Test def testVRegexPositiveAndNegativeWithPrefixesAndSuffixes(): Unit = {
    val re = TextNumberPatternUtils.vRegexStandard
    val optMatch = re.findFirstMatchIn("A###012V34B;C####56V78D")
    optMatch match {
      case Some(re("A", "###012", "34", "B", "C", "####56V78", "D")) => // ok
      case _ => fail()
    }
  }

  @Test def testVRegexPositiveAndNegativeWithPrefixesAndSuffixes2(): Unit = {
    val re = TextNumberPatternUtils.vRegexStandard
    val optMatch = re.findFirstMatchIn("'P'###012V34B;C####56V78D")
    optMatch match {
      case Some(re("'P'", "###012", "34", "B", "C", "####56V78", "D")) => // ok
      case _ => fail()
    }
  }

  @Test def testVRegexPositiveAndNegativeWithPrefixesAndSuffixes3(): Unit = {
    val re = TextNumberPatternUtils.vRegexStandard
    val optMatch = re.findFirstMatchIn("'P'###012V34'P';N0V0N")
    optMatch match {
      case Some(re("'P'", "###012", "34", "'P'", "N", "0V0", "N")) => // ok
      case _ => fail()
    }
  }

  @Test def testVRegexPositiveAndNegativeWithPrefixesAndSuffixes4(): Unit = {
    val re = TextNumberPatternUtils.vRegexStandard
    val optMatch = re.findFirstMatchIn("'P'###012V34'P';N#N")
    println(optMatch)
    optMatch match {
      case Some(re("'P'", "###012", "34", "'P'", "N", "#", "N")) => // ok
      case _ => fail()
    }
  }

  @Test def testVRegexOnlyPositivePattern(): Unit = {
    val re = TextNumberPatternUtils.vRegexStandard
    val optMatch = re.findFirstMatchIn("A###012V34B")
    optMatch match {
      case Some(re("A", "###012", "34", "B", null, null, null)) => // ok
      case _ => fail()
    }
  }

  @Test def testVRegexOnlyPositivePatternNoPrefixNorSuffix(): Unit = {
    val re = TextNumberPatternUtils.vRegexStandard
    val optMyMatch = re.findFirstMatchIn("###012V34")

    optMyMatch match {
      case Some(re("", "###012", "34", "", null, null, null)) => // ok
      case _ => fail()
    }
  }

  @Test def testVRegexTrailingSign(): Unit = {
    val re = TextNumberPatternUtils.vRegexStandard
    val optMatch = re.findFirstMatchIn("012V34+") // for zoned, overpunched trailing sign.
    optMatch match {
      case Some(re("", "012", "34", "+", null, null, null)) => // ok
      case _ => fail()
    }
  }

  @Test def testVRegexZonedLeadingSign(): Unit = {
    val re = TextNumberPatternUtils.vRegexZoned
    val optMyMatch = re.findFirstMatchIn("+012V34") // for zoned, overpunched leading sign.
    optMyMatch match {
      case Some(re("+", "012", "34", "")) => // ok
      case _ => fail()
    }
  }

  @Test def testVRegexZonedTrailingSign(): Unit = {
    val re = TextNumberPatternUtils.vRegexZoned
    val optMyMatch = re.findFirstMatchIn("012V34+") // for zoned, overpunched trailing sign.
    optMyMatch match {
      case Some(re("", "012", "34", "+")) => // ok
      case _ => fail()
    }
  }

  @Test def testVRegexZonedNothingAfterSuffix(): Unit = {
    val re = TextNumberPatternUtils.vRegexZoned
    val optMyMatch =
      re.findFirstMatchIn("012V34+garbage") // for zoned, overpunched trailing sign.
    optMyMatch match {
      case Some(re("", "012", "34", "+")) =>
        fail("accepted trash at end of pattern")
      case _ => // ok
    }
  }

  @Test def testVRegexZonedSignNotPlus(): Unit = {
    val re = TextNumberPatternUtils.vRegexZoned
    val optMyMatch = re.findFirstMatchIn("A012V34")
    optMyMatch match {
      case Some(x) => fail(s"accepted A as leading sign: ${x.matched}")
      case None => // ok
    }
  }

  @Test def testVRegexZonedTwoSigns(): Unit = {
    assertTrue(
      TextNumberPatternUtils.textNumber_V_DecimalVirtualPointForZoned("+012V34+").isEmpty
    )
  }

  @Test def testRemoveUnquotedPAndV_01(): Unit = {
    val pattern = "PPzVz''Va'PPP'''V'b'''"
    val expected = "zz''a'PPP''''b'''"
    assertEquals(expected, TextNumberPatternUtils.removeUnquotedPV(pattern))
  }

  /**
   * This test shows that quoting of characters in ICU text number patterns
   * and hence DFDL text number patterns can quote strings, not just individual
   * characters.
   *
   * I found no examples of this anywhere. Everything shows patterns where
   * quotes surround only single characters, or are doubled up for self quoting.
   *
   * This is ICU behavior however, DFDL v1.0 specifies that the prefix/suffix
   * can only be single characters.
   */
  @Test def testICUDecimalFormatQuoting_01(): Unit = {
    {
      // Note that E is a pattern special character
      val pattern = "'POSITIVE' #.0###;'NEGATIVE' #.0###"
      val dfs = new DecimalFormatSymbols(ULocale.US)
      val df = new DecimalFormat(pattern, dfs)
      val actual = df.format(-6.847)
      assertEquals("NEGATIVE 6.847", actual)
    }
    {
      // here we quote only the E, not the other characters.
      val pattern = "POSITIV'E' #.0###;NEGATIV'E' #.0###"
      val dfs = new DecimalFormatSymbols(ULocale.US)
      val df = new DecimalFormat(pattern, dfs)
      val actual = df.format(6.847)
      assertEquals("POSITIVE 6.847", actual)
    }
  }

  @Test def howToUseRegexLookBehindWithReplaceAll(): Unit = {
    // despite the fact that we say "P not preceded by ...." that's not how you
    // structure the regex.
    val regexWithLookbehindAfter = "(?:P(?<!'))" // does not work.
    val actual = "abc'PdefP'ghi".replaceAll(regexWithLookbehindAfter, "X")
    // doesn't work. Quoted P gets replaced by X.
    assertEquals("abc'XdefX'ghi", actual)

    // you have to write the negative lookbehind part before the P
    val P_not_preceded_by_quote = "(?<!')P" // works
    val actual2 = "abc'PdefPghi".replaceAll(P_not_preceded_by_quote, "X")
    assertEquals("abc'PdefXghi", actual2)
  }

  /**
   * This test shows some formulations for unquoted P that one might
   * try, do not work and are a waste of time.
   */
  @Test def howNotToUseRegexLookBehindWithReplaceAll(): Unit = {
    val e = intercept[PatternSyntaxException] {
      // P preceded by a non-quote, or an even number of paired quotes.
      // or just P at the start of the string.
      "((?<=([^']|('')+))P|^P)".r
    }
    // doesn't work because of the ('')+ in the lookbehind.
    assertTrue(e.getMessage().contains("does not have an obvious maximum length"))
    //
    // Try bounding it to maximum of 10 paired quotes
    //
    val P_regex = "((?<=([^']|(''){1,10}))P|^P)"
    // Works for even number of quotes
    assertEquals("a''''''X", "a''''''P".replaceAll(P_regex, "X"))
    // Nope. Doesn't work.
    // This should leave the P alone. It's quoted because of 7 preceding quotes (3 matched pairs)
    // then its own quote.
    assertNotEquals("a'''''''P", "a'''''''P".replaceAll(P_regex, "X"))
    assertEquals("a'''''''X", "a'''''''P".replaceAll(P_regex, "X"))
  }

  @Test def testZonedVRegexWithPrefix(): Unit = {
    val re = TextNumberPatternUtils.vRegexZoned
    val optMatch = re.findFirstMatchIn("+012V34")
    optMatch match {
      case Some(re("+", "012", "34", "")) => // ok
      case _ => fail()
    }
  }

  @Test def testZonedVRegexWithSuffix(): Unit = {
    val re = TextNumberPatternUtils.vRegexZoned
    val optMatch = re.findFirstMatchIn("012V34+")
    optMatch match {
      case Some(re("", "012", "34", "+")) => // ok
      case _ => fail()
    }
  }

  @Test def testStandardPOnLeft(): Unit = {
    val re = TextNumberPatternUtils.pOnLeftRegexStandard
    println(re)
    val optMatch = re.findFirstMatchIn("+PPP000")
    optMatch match {
      case Some(re("+", "PPP", "000", "", null, null, null)) => // ok
      case _ => fail()
    }
  }

  @Test def testStandardPOnLeft2(): Unit = {
    val re = TextNumberPatternUtils.pOnLeftRegexStandard
    println(re)
    val optMatch = re.findFirstMatchIn("+PPP000;-#")
    optMatch match {
      case Some(re("+", "PPP", "000", "", "-", "#", "")) => // ok
      case _ => fail()
    }
  }

  @Test def testStandardPOnLeft3(): Unit = {
    val re = TextNumberPatternUtils.pOnLeftRegexStandard
    println(re)
    val optMatch = re.findFirstMatchIn("+PPP000;-P0")
    optMatch match {
      case Some(re("+", "PPP", "000", "", "-", "P0", "")) => // ok
      case _ => fail()
    }
  }

  @Test def testStandardPOnRight(): Unit = {
    val re = TextNumberPatternUtils.pOnRightRegexStandard
    val optMatch = re.findFirstMatchIn("000PPP+")
    optMatch match {
      case Some(re("", "000", "PPP", "+", null, null, null)) => // ok
      case _ => fail()
    }
  }

  @Test def testStandardPOnRight2(): Unit = {
    val re = TextNumberPatternUtils.pOnRightRegexStandard
    val optMatch = re.findFirstMatchIn("000PPP+;0P-")
    optMatch match {
      case Some(re("", "000", "PPP", "+", "", "0P", "-")) => // ok
      case _ => fail()
    }
  }

  @Test def testZonedPOnLeft(): Unit = {
    val re = TextNumberPatternUtils.pOnLeftRegexZoned
    val optMatch = re.findFirstMatchIn("+PPP000")
    optMatch match {
      case Some(re("+", "PPP", "000", "")) => // ok
      case _ => fail()
    }
  }

  @Test def testZonedPOnRight(): Unit = {
    val re = TextNumberPatternUtils.pOnRightRegexZoned
    val optMatch = re.findFirstMatchIn("000PPP+")
    optMatch match {
      case Some(re("", "000", "PPP", "+")) => // ok
      case _ => fail()
    }
  }

}
