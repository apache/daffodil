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

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.Implicits.intercept
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.Assert.fail

import java.util.regex.PatternSyntaxException

class TestPrimitives {

  @Test def testVRegexPositiveAndNegativeWithPrefixesAndSuffixes() : Unit = {
    val re = TextNumberPatternUtils.vregexStandard
    val Some(myMatch) = re.findFirstMatchIn("A###012V34B;C####56V78D")
    myMatch match {
      case re("A", "###012", "34", "B", "C", "####56", "78", "D") => // ok
    }
  }

  @Test def testVRegexOnlyPositivePattern(): Unit = {
    val re = TextNumberPatternUtils.vregexStandard
    val Some(myMatch) = re.findFirstMatchIn("A###012V34B")
    myMatch match {
      case re("A", "###012", "34", "B", null, null, null, null) =>
    }
  }

  @Test def testVRegexOnlyPositivePatternNoPrefixNorSuffix(): Unit = {
    val re = TextNumberPatternUtils.vregexStandard
    val Some(myMatch) = re.findFirstMatchIn("###012V34")
    myMatch match {
      case re("", "###012", "34", "", null, null, null, null) =>
    }
  }

  @Test def testVRegexTrailingSign(): Unit = {
    val re = TextNumberPatternUtils.vregexStandard
    val Some(myMatch) = re.findFirstMatchIn("012V34+") // for zoned, overpunched trailing sign.
    myMatch match {
      case re("", "012", "34", "+", null, null, null, null) =>
    }
  }

  @Test def testVRegexZonedLeadingSign(): Unit = {
    val re = TextNumberPatternUtils.vregexZoned
    val optMyMatch = re.findFirstMatchIn("+012V34") // for zoned, overpunched leading sign.
    val Some(re("+", "012", "34", "")) = optMyMatch
  }

  @Test def testVRegexZonedTrailingSign(): Unit = {
    val re = TextNumberPatternUtils.vregexZoned
    val optMyMatch = re.findFirstMatchIn("012V34+") // for zoned, overpunched trailing sign.
    val Some(re("", "012", "34", "+")) = optMyMatch
  }

  @Test def testVRegexZonedNothingAfterSuffix(): Unit = {
    val re = TextNumberPatternUtils.vregexZoned
    val optMyMatch = re.findFirstMatchIn("012V34+garbage") // for zoned, overpunched trailing sign.
    optMyMatch match {
      case Some(re("", "012", "34", "+")) => fail("accepted trash at end of pattern")
      case None => // ok
    }
  }

  @Test def testVRegexZonedSignNotPlus(): Unit = {
    val re = TextNumberPatternUtils.vregexZoned
    val optMyMatch = re.findFirstMatchIn("A012V34")
    optMyMatch match {
      case Some(x @ re(_*)) => fail(s"accepted A as leading sign: $x")
      case None => // ok
    }
  }

  @Test def testVRegexZonedTwoSigns(): Unit = {
    assertTrue(
      TextNumberPatternUtils.textDecimalVirtualPointForZoned("+012V34+").isEmpty
    )
  }

  @Test def testRemoveUnquotedPAndV_01(): Unit = {
    val actually = "zVz''Va'PPP'''V'b'''"
    val expected = "zz''a'P'''V'b'''"
    assertEquals(expected, TextNumberPatternUtils.removeUnquotedPV(actually))
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
    val re = TextNumberPatternUtils.vregexZoned
    val Some(myMatch) = re.findFirstMatchIn("+012V34")
    myMatch match {
      case re("+", "012", "34", "") => // ok
    }
  }

  @Test def testZonedVRegexWithSuffix(): Unit = {
    val re = TextNumberPatternUtils.vregexZoned
    val Some(myMatch) = re.findFirstMatchIn("012V34+")
    myMatch match {
      case re("", "012", "34", "+") => // ok
    }
  }
}
