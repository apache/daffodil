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

package org.apache.daffodil.runtime1.processors.input

import java.io.StringReader
import scala.util.parsing.combinator._

import org.apache.daffodil.lib.exceptions.Assert

import org.junit.Assert._
import org.junit.Test

class TestRegex extends RegexParsers {

  /**
   * Scala combinator parsers can do longest match via the '|||' combinator
   * Let's be sure we get longest match.
   */

  override def skipWhitespace = skipWS
  var skipWS = false // assign this to turn on/off whitespace skipping.

  @Test def testEscapeRemoval_Blocks() = {
    // Assumptions:
    //	1. Delimiter and padding removed
    //	2. Valid start-end blocks were already removed
    //	3. Because valid start-end blocks were already removed
    //		an escapeEscape character preceding the escapeBlockEnd
    //		must occur at the end of the string input.
    //
    // Step 1:
    //	Remove escapeEscapeCharacters preceding escapeBlockEnd
    // val blockStart = "T"
    val blockEnd = "N"
    val escapeEscape = "E"
    val rRemoveEscape =
      """(%1$s$)|(%1$s(?=%2$s))""" // Replace escape at end of line OR replace escapeEscape preceding escapeBlockEnd
    val removeEscapes = rRemoveEscape.format(escapeEscape, blockEnd)

    def test(regex: String, input: String) = {
      input.replaceAll(regex, "")
    }

    assertEquals("abc", test(removeEscapes, "abc"))
    assertEquals("abc", test(removeEscapes, "abcE"))
    assertEquals("abcE", test(removeEscapes, "abcEE"))
    assertEquals("TabcN", test(removeEscapes, "TabcENE"))
    assertEquals("TabEc", test(removeEscapes, "TabEcE"))
    assertEquals("EabcN", test(removeEscapes, "EabcENE"))
    assertEquals("Tblah1Eblah2N", test(removeEscapes, "Tblah1Eblah2EN"))
    assertEquals("Tblah1ENblah2N", test(removeEscapes, "Tblah1EENblah2EN"))
    assertEquals("blah1Eblah2", test(removeEscapes, "blah1Eblah2"))
    assertEquals("blah1EEblah2", test(removeEscapes, "blah1EEblah2"))
    assertEquals("blah1ETblah2", test(removeEscapes, "blah1ETblah2"))
  }

  @Test def testEscapeRemoval_SameCharacter() = {
    // This might not be possible using regex replacement
    // may have to stick with existing code in DelimParser
    //
    // Assumptions:
    //	1. Delimiter and padding removed
    //
    // Step
    //	1.	Remove escape characters not preceded by escapeEscape
    //	2.	Remove escapeEscape characters
    val escape = "E"
    // val escapeEscape = "E"
    // val rRemoveSingleEscapes = """(?<!(%s%s)+)%s(?!(%s+))""" // (?<!(EE+))E(?!E+)
    // used to cleanup escape characters
    val ERSplit = """(.*?)%1$s(.)(.*)""".format(escape).r

    def removeActiveEscapes(str: String): String = {
      // if contains ER, replace with just R
      val str2 = removeActiveEscapes1(str)
      str2
    }

    def removeActiveEscapes1(str: String): String = {
      val res = str match {
        case ERSplit(before, delim, after) => {
          val rest = removeActiveEscapes1(after)
          before + delim + rest
        }
        case _ => str
      }
      res
    }

    def test(input: String) = {
      removeActiveEscapes(input)
    }

    assertEquals("text1Etext2", test("text1EEtext2"))
    assertEquals("text1Etext2Etext3", test("text1EEtext2EEtext3"))
    assertEquals("text1EEtext2", test("text1EEEEtext2"))
    assertEquals("text1E;text2", test("text1EEE;text2"))
    assertEquals("text1;text2", test("text1E;text2"))
    assertEquals("text1Etext2E", test("text1EEtext2EE"))
    assertEquals("text1text2", test("text1Etext2"))
  }

  @Test def testEscapeRemoval_DiffCharacter() = {
    // Assumptions:
    //	1. Delimiter and padding removed
    //
    // Step
    //	1.	Remove escape characters not preceded by escapeEscape
    //	2.	Remove escapeEscape characters that precede escapes
    val escape = "E"
    val escapeEscape = "S"
    // val rRemoveEscape = """(%1$s$)|(%1$s(?=%2$s))""" // Replace escape at end of line OR replace escapeEscape preceding escapeBlockEnd
    // val rRemoveUnescapedEscapes = """((?<!S)E)"""
    val rRemoveUnescapedEscapes = """((?<!%1$s)%2$s)"""
    val removeUnescapedEscapes = rRemoveUnescapedEscapes.format(escapeEscape, escape)
    // val rRemoveEscapeEscapesThatEscape = """(S(?=E))"""
    val rRemoveEscapeEscapesThatEscape = """(%1$s(?=%2$s))"""
    val removeEscapeEscapesThatEscape =
      rRemoveEscapeEscapesThatEscape.format(escapeEscape, escape)

    def test(regex1: String, regex2: String, input: String) = {
      val r1 = input.replaceAll(regex1, "")
      val r2 = r1.replaceAll(regex2, "")
      r2
    }

    assertEquals(
      "text1Etext2",
      test(removeUnescapedEscapes, removeEscapeEscapesThatEscape, "text1SEtext2")
    )
    assertEquals(
      "text1Etext2Etext3",
      test(removeUnescapedEscapes, removeEscapeEscapesThatEscape, "text1SEtext2SEtext3")
    )
    assertEquals(
      "text1EEtext2",
      test(removeUnescapedEscapes, removeEscapeEscapesThatEscape, "text1SESEtext2")
    )
    assertEquals(
      "text1E;text2",
      test(removeUnescapedEscapes, removeEscapeEscapesThatEscape, "text1SEE;text2")
    )
    assertEquals(
      "text1;text2",
      test(removeUnescapedEscapes, removeEscapeEscapesThatEscape, "text1E;text2")
    )
    assertEquals(
      "text1Etext2E",
      test(removeUnescapedEscapes, removeEscapeEscapesThatEscape, "text1SEtext2SE")
    )
    assertEquals(
      "text1SEtext2",
      test(removeUnescapedEscapes, removeEscapeEscapesThatEscape, "text1SSEtext2")
    )
    assertEquals(
      "text1Stext2",
      test(removeUnescapedEscapes, removeEscapeEscapesThatEscape, "text1Stext2")
    )
  }

  // Need to capture everything to get length of parsed data
  @Test def testParser_CaptureEverything() = {
    var testNum = 0
    def test(
      theParser: Parser[(Vector[String], String)],
      theInput: String,
      isPadLeft: Boolean,
      isPadRight: Boolean
    ) = {
      testNum = testNum + 1
      // val result = this.parse(this.log(theParser)("testParserEscapeSchemes_DiffEscapesWithPadding." + testNum), theInput)
      val result = this.parse(theParser, theInput)
      if (result.isEmpty) { None }
      else {
        val (c, delim) = result.get
        // System.err.println(c)
        val res = (isPadLeft -> isPadRight) match {
          case (true, true) => {
            // Pad ~ content ~ pad
            val field = c(1)
            (field, delim, c.mkString)
          }
          case (true, false) => {
            val field = c(1)
            (field, delim, c.mkString)
          }
          case (false, true) => {
            val field = c(0)
            (field, delim, c.mkString)
          }
          case (false, false) => {
            val field = c(0)
            (field, delim, c.mkString)
          }
        }

        Some(res._1, res._2, res._3)
      }
    }
    val padChar = "P"
    val delim = "(DD)|(D)"
    val rPadChar = """(%s*)"""
    val pPadChar: Parser[String] = rPadChar.format(padChar).r
    // Everything until:
    // 1.	PadChar ~ delim
    // 2.	PadChar ~ End of Data/File
    // 3.	delim
    // 4.	End of Data/File
    //
    // (.*?)(?=(1 | 2 | 3 | 4))
    // 1 = P*d
    // 2 = P*\z
    // 3 = d
    // 4 = \z
    val rBefore = """(.*?)(?=(%1$s+(%2$s))|(%1$s+\z)|(%2$s)|(\z))"""
    val pBefore: Parser[String] = rBefore.format(padChar, delim).r
    val pDelims: Parser[String] = """D""".r ||| """DD""".r
    //    val pEOF: Parser[String] = """\z""".r

    val paddedContent = pPadChar ~ pBefore ~ pPadChar ^^ { case (lp ~ c ~ rp) =>
      Vector(lp, c, rp)
    }
    val leftPaddedContent = pPadChar ~ pBefore ^^ { case (lp ~ c) => Vector(lp, c) }
    val rightPaddedContent = pBefore ~ pPadChar ^^ { case (c ~ rp) => Vector(c, rp) }

    //    val content = pBefore ~ (pDelims | pEOF) ^^ { case (c ~ d) => (Vector(c), d) }
    //    val contentLeft = rightPaddedContent ~ (pDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
    //    val contentRight = leftPaddedContent ~ (pDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
    //    val contentCenter = paddedContent ~ (pDelims | pEOF) ^^ { case (c ~ d) => (c, d) }

    val contentDelimReq = pBefore ~ pDelims ^^ { case (c ~ d) => (Vector(c), d) }
    val contentLeftDelimReq = rightPaddedContent ~ pDelims ^^ { case (c ~ d) => (c, d) }
    val contentRightDelimReq = leftPaddedContent ~ pDelims ^^ { case (c ~ d) => (c, d) }
    val contentCenterDelimReq = paddedContent ~ pDelims ^^ { case (c ~ d) => (c, d) }

    assertEquals(Some("abc", "D", "abc"), test(contentDelimReq, "abcDefg", false, false))
    assertEquals(
      Some("abc", "D", "PPPabc"),
      test(contentRightDelimReq, "PPPabcDefg", true, false)
    )
    assertEquals(
      Some("abc", "D", "abcPPP"),
      test(contentLeftDelimReq, "abcPPPDefg", false, true)
    )
    assertEquals(
      Some("abc", "D", "PPPabcPPP"),
      test(contentCenterDelimReq, "PPPabcPPPDefg", true, true)
    )

  }

  // Need to capture everything to get length of parsed data
  @Test def testParserEscapeSchemes_DiffEscapesWithPaddingCapturesEverything() = {
    var testNum = 0
    def test(
      theParser: Parser[(Vector[String], String)],
      theInput: String,
      isPadLeft: Boolean,
      isPadRight: Boolean
    ) = {
      testNum = testNum + 1
      // val result = this.parse(this.log(theParser)("testParserEscapeSchemes_DiffEscapesWithPadding." + testNum), theInput)
      val result = this.parse(theParser, theInput)
      if (result.isEmpty) { None }
      else {
        val (c, delim) = result.get
        val res = (isPadLeft -> isPadRight) match {
          case (true, true) => {
            // Pad ~ content ~ pad
            val field = c(1)
            (field, delim, c.mkString)
          }
          case (true, false) => {
            val field = c(1)
            (field, delim, c.mkString)
          }
          case (false, true) => {
            val field = c(0)
            (field, delim, c.mkString)
          }
          case (false, false) => {
            val field = c(0)
            (field, delim, c.mkString)
          }
        }

        Some(res._1, res._2, res._3)
      }
    }

    val escapeEscape = """S"""
    val escape = """E"""
    val delim = """D"""
    val padChar = """P"""

    val rPadChar = """(%s*)"""
    val pPadChar: Parser[String] = rPadChar.format(padChar).r
    val rLeftPadChar = """(%1$s*)(?=([^%1$s]))""" // LeftPad precedes non pad characters
    val pLeftPadChar: Parser[String] = rLeftPadChar.format(padChar).r
    val pEOF: Parser[String] = """\z""".r

    // Content is anything until:
    // 1. Padding ~ delimiter
    // 2. unescaped delimiter
    // 3. Optional Padding ~ EndOfData
    val rBefore = """(.*?)(?=(%4$s+%3$s)|((?<!(?<!%1$s)%2$s)%3$s)|(%4$s*\z))"""
    val pBefore: Parser[String] = rBefore.format(escapeEscape, escape, delim, padChar).r

    //    val rBeforeIgnoreTrailingPadding = """(.*?)(?=(?:(?<!(?<!%1$s)%2$s)%3$s)|\z)"""
    //    val pBeforeIgnoreTrailingPadding: Parser[String] = rBeforeIgnoreTrailingPadding.format(escapeEscape, escape, delim).r
    //
    val pDelims: Parser[String] = """D""".r ||| """DD""".r
    val pEscape: Parser[String] = """E""".r
    val pEscapeEscape: Parser[String] = """S""".r
    //        val pEscapedEscape = pEscapeEscape ~ pEscape
    val pUnescapedDelims = ((pEscapeEscape ~ pEscape) ~> pDelims) | (not(
      pEscape
    ) ~> pDelims) | (pEscapeEscape ~> pDelims) | pDelims
    //
    //    // Here ^^ { case(b ~ d) => (b -> d) } changes the output of a successful parse to a Tuple2 of String (before, delimiter)
    val paddedContent: Parser[Vector[String]] = pLeftPadChar ~ pBefore ~ pPadChar ^^ {
      case (lp ~ c ~ rp) => Vector(lp, c, rp)
    }
    //    val leftPaddedContent: Parser[Vector[String]] = pLeftPadChar ~ pBefore ^^ { case (lp ~ c) => Vector(lp, c) }
    //    val rightPaddedContent: Parser[Vector[String]] = pBefore ~ pPadChar ^^ { case (c ~ rp) => Vector(c, rp) }

    //    val content: Parser[(Vector[String], String)] = (pBeforeIgnoreTrailingPadding ~ (pUnescapedDelims | pEOF)) ^^ { case (b ~ d) => (Vector(b) -> d) }
    val contentCenter: Parser[(Vector[String], String)] =
      paddedContent ~ (pUnescapedDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
    //    val contentLeft: Parser[(Vector[String], String)] = rightPaddedContent ~ (pUnescapedDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
    //    val contentRight: Parser[(Vector[String], String)] = leftPaddedContent ~ (pUnescapedDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
    //
    //    val contentDelimReq: Parser[(Vector[String], String)] = pBeforeIgnoreTrailingPadding ~ pUnescapedDelims ^^ { case (b ~ d) => (Vector(b) -> d) }
    //    val contentCenterDelimReq: Parser[(Vector[String], String)] = paddedContent ~ pUnescapedDelims ^^ { case (c ~ d) => (c, d) }
    //    val contentLeftDelimReq: Parser[(Vector[String], String)] = rightPaddedContent ~ pUnescapedDelims ^^ { case (c ~ d) => (c, d) }
    //    val contentRightDelimReq: Parser[(Vector[String], String)] = leftPaddedContent ~ pUnescapedDelims ^^ { case (c ~ d) => (c, d) }

    // NOTE: At this point we've left the escapes in the content
    // we can probably go a step further and execute a regex replace on
    // the escape characters.  But order will likely matter.
    //
    // 1. Escapes should be removed unless preceded by an escapeEscape
    // 2. EscapeEscapes should be removed when they precede an escape.

    // Test padding blind escape
    // Encounter non-pad char amongst padding
    assertEquals(Some("text", "", "PPPtextPPP"), test(contentCenter, "PPPtextPPP", true, true))
    assertEquals(
      Some("/PPPtext/", "", "/PPPtext/PPP"),
      test(contentCenter, "/PPPtext/PPP", true, true)
    )
    assertEquals(
      Some("/PPtextP/", "", "P/PPtextP/PP"),
      test(contentCenter, "P/PPtextP/PP", true, true)
    )
    assertEquals(
      Some("/PtextPP/", "", "PP/PtextPP/P"),
      test(contentCenter, "PP/PtextPP/P", true, true)
    )
    assertEquals(
      Some("/textPPP/", "", "PPP/textPPP/"),
      test(contentCenter, "PPP/textPPP/", true, true)
    )

    // Encounter non-pad char amongst padding (verify no dependence on /)
    assertEquals(Some("text", "", "PPPtextPPP"), test(contentCenter, "PPPtextPPP", true, true))
    assertEquals(
      Some("^PPPtext^", "", "^PPPtext^PPP"),
      test(contentCenter, "^PPPtext^PPP", true, true)
    )
    assertEquals(
      Some("^PPtextP^", "", "P^PPtextP^PP"),
      test(contentCenter, "P^PPtextP^PP", true, true)
    )
    assertEquals(
      Some("^PtextPP^", "", "PP^PtextPP^P"),
      test(contentCenter, "PP^PtextPP^P", true, true)
    )
    assertEquals(
      Some("^textPPP^", "", "PPP^textPPP^"),
      test(contentCenter, "PPP^textPPP^", true, true)
    )
  }

  // Need to capture everything
  @Test def testParserEscapeSchemes_BlockEscapeWithPaddingCapturesEverything() = {
    var testNum = 0
    def test(
      theParser: Parser[(Vector[String], String)],
      theInput: String,
      isPadLeft: Boolean,
      isPadRight: Boolean
    ) = {
      testNum = testNum + 1
      // val result = this.parse(this.log(theParser)("testParserEscapeSchemes_BlockEscapeWithPaddingCapturesEverything." + testNum), theInput)
      val result = this.parse(theParser, theInput)
      if (result.isEmpty) { None }
      else {
        val (bc, delim) = result.get
        val res = (isPadLeft -> isPadRight) match {
          case (true, true) => {
            val field = bc(2)
            (field, delim, bc.mkString)
          }
          case (true, false) => {
            val field = bc(2)
            (field, delim, bc.mkString)
          }
          case (false, true) => {
            val field = bc(1)
            (field, delim, bc.mkString)
          }
          case (false, false) => {
            val field = bc(1)
            (field, delim, bc.mkString)
          }
        }

        Some(res._1, res._2, res._3)
      }
    }

    val blockStart = "T"
    val blockEnd = "N"
    val escapeEscape = "S"
    val padChar = "P"

    val rPadChar = """(%s*)"""
    val pPadChar: Parser[String] = rPadChar.format(padChar).r

    val rUnescapedBlockStart = """(?<!%1$s)%2$s"""
    val pUnescapedBlockStart: Parser[String] =
      rUnescapedBlockStart.format(escapeEscape, blockStart).r

    val rUnescapedBlockEnd = """(?<!%1$s)%2$s"""
    val pUnescapedBlockEnd: Parser[String] = rUnescapedBlockEnd.format(escapeEscape, blockEnd).r

    //    val rBeforeUnescapedBlockEnd = """(.*?)(?=((?<!%1$s)%2$s))"""
    val rBeforeUnescapedBlockEnd = """(.*?)(?=(""" + rUnescapedBlockEnd + """))"""
    val pBeforeUnescapedBlockEnd: Parser[String] =
      rBeforeUnescapedBlockEnd.format(escapeEscape, blockEnd).r

    val pDelims: Parser[String] = """D""".r ||| """DDD""".r

    //    val pEscape: Parser[String] = escapeEscape.r
    //    val pBlockStart: Parser[String] = blockStart.r
    //    val pBlockEnd: Parser[String] = blockEnd.r
    val pEOF: Parser[String] = """\z""".r

    val blockedContent: Parser[Vector[String]] =
      pUnescapedBlockStart ~ pBeforeUnescapedBlockEnd ~ pUnescapedBlockEnd ^^ {
        case (bs ~ c ~ be) => Vector(bs, c, be)
      }

    // Here the ^^ { case (b ~ d) => (b -> d) } statements allow us to control the output of a successful parse
    // in this particular case we want the output to be a Tuple2 of String (blockedContent, delimiter)
    //    val content: Parser[(Vector[String], String)] = blockedContent ~ (pDelims | pEOF) ^^ { case (bc ~ d) => (bc -> d) }
    //    val contentDelimReq: Parser[(Vector[String], String)] = blockedContent ~ pDelims ^^ { case (bc ~ d) => (bc -> d) }

    val paddedBlockedContent: Parser[Vector[String]] = pPadChar ~ blockedContent ~ pPadChar ^^ {
      case (lp ~ bc ~ rp) => Vector(lp) ++ bc ++ Vector(rp)
    }
    val contentCenter: Parser[(Vector[String], String)] =
      paddedBlockedContent ~ (pDelims | pEOF) ^^ { case (bc ~ d) => (bc -> d) }
    //    val contentCenterDelimReq: Parser[(Vector[String], String)] = paddedBlockedContent ~ pDelims ^^ { case (bc ~ d) => (bc -> d) }

    //    val leftPaddedBlockedContent: Parser[Vector[String]] = pPadChar ~ blockedContent ^^ { case (lp ~ bc) => Vector(lp) ++ bc }
    //    val contentRight: Parser[(Vector[String], String)] = leftPaddedBlockedContent ~ (pDelims | pEOF) ^^ { case (bc ~ d) => (bc -> d) }
    //    val contentRightDelimReq: Parser[(Vector[String], String)] = leftPaddedBlockedContent ~ pDelims ^^ { case (bc ~ d) => (bc -> d) }

    //    val rightPaddedBlockedContent: Parser[Vector[String]] = blockedContent ~ pPadChar ^^ { case (bc ~ rp) => bc ++ Vector(rp) }
    //    val contentLeft: Parser[(Vector[String], String)] = rightPaddedBlockedContent ~ (pDelims | pEOF) ^^ { case (bc ~ d) => (bc -> d) }
    //    val contentLeftDelimReq: Parser[(Vector[String], String)] = rightPaddedBlockedContent ~ pDelims ^^ { case (bc ~ d) => (bc -> d) }

    //    val contentCenter: Parser[(String, String)] = (pPadChar ~> blockedContent <~ pPadChar) ~ (pDelims | pEOF) ^^ { case (b ~ d) => (b -> d) }
    //    val contentCenterDelimReq: Parser[(String, String)] = (pPadChar ~> blockedContent <~ pPadChar) ~ pDelims ^^ { case (b ~ d) => (b -> d) }
    //
    //    val contentLeft: Parser[(String, String)] = (blockedContent <~ pPadChar) ~ (pDelims | pEOF) ^^ { case (b ~ d) => (b -> d) }
    //    val contentLeftDelimReq: Parser[(String, String)] = (blockedContent <~ pPadChar) ~ pDelims ^^ { case (b ~ d) => (b -> d) }
    //
    //    val contentRight: Parser[(String, String)] = (pPadChar ~> blockedContent) ~ (pDelims | pEOF) ^^ { case (b ~ d) => (b -> d) }
    //    val contentRightDelimReq: Parser[(String, String)] = (pPadChar ~> blockedContent) ~ pDelims ^^ { case (b ~ d) => (b -> d) }
    //
    //    val content: Parser[(String, String)] = (blockedContent) ~ (pDelims | pEOF) ^^ { case (b ~ d) => (b -> d) }
    //    val contentDelimReq: Parser[(String, String)] = (blockedContent) ~ pDelims ^^ { case (b ~ d) => (b -> d) }

    // Testing Center justification
    assertEquals(
      Some("beforeafter", "", "PPPTbeforeafterNPPP"),
      test(contentCenter, "PPPTbeforeafterNPPP", true, true)
    )
    assertEquals(
      Some("before", "D", "PPPTbeforeNPPP"),
      test(contentCenter, "PPPTbeforeNPPPDafter", true, true)
    )
    assertEquals(
      None,
      test(contentCenter, "TbeforeDafter", true, true)
    ) // Fails because we expect a valid escapeBlockStart AND escapeBlockEnd
    assertEquals(
      Some("TbeforeSN", "D", "PPPTTbeforeSNNPPP"),
      test(contentCenter, "PPPTTbeforeSNNPPPDafter", true, true)
    )
    assertEquals(
      Some("TbeforeDPPPstillbeforeSN", "D", "PPPTTbeforeDPPPstillbeforeSNNPPP"),
      test(contentCenter, "PPPTTbeforeDPPPstillbeforeSNNPPPDafter", true, true)
    )
    assertEquals(
      None,
      test(contentCenter, "PPPTTbeforeDPPPstillbeforeNNPPPDafter", true, true)
    ) // escapeBlockEnd is not escaped
    assertEquals(
      None,
      test(contentCenter, "PPPTbeforeNafterNPPP", true, true)
    ) // escapeBlockEnd is not escaped
    assertEquals(
      Some("before1SNbefore2", "", "PPPTbefore1SNbefore2NPPP"),
      test(contentCenter, "PPPTbefore1SNbefore2NPPP", true, true)
    )
    //    assertEquals(None, test(content, "PPPTbeforeSNafterNPPP", false, false))
    //    assertEquals(Some("before1SNbefore2", ""), test(content, "Tbefore1SNbefore2N", false, false))
    //    assertEquals(Some("before1SNbefore2PP", ""), test(content, "Tbefore1SNbefore2PPN", false, false))
  }

  @Test def testParserEscapeSchemes_BlockEscapeWithPadding() = {
    var testNum = 0
    def test(theParser: Parser[(String, String)], theInput: String) = {
      testNum = testNum + 1
      // val result = this.parse(this.log(theParser)("testParserEscapeSchemes_BlockEscapeWithPadding." + testNum), theInput)
      val result = this.parse(theParser, theInput)
      if (result.isEmpty) { None }
      else {
        val res = result.get
        Some(res._1, res._2)
      }
    }

    val blockStart = "T"
    val blockEnd = "N"
    val escapeEscape = "S"
    val padChar = "P"

    val rPadChar = """(%s*)"""
    val pPadChar: Parser[String] = rPadChar.format(padChar).r

    val rUnescapedBlockStart = """(?<!%1$s)%2$s"""
    val pUnescapedBlockStart: Parser[String] =
      rUnescapedBlockStart.format(escapeEscape, blockStart).r

    val rUnescapedBlockEnd = """(?<!%1$s)%2$s"""
    val pUnescapedBlockEnd: Parser[String] = rUnescapedBlockEnd.format(escapeEscape, blockEnd).r

    //    val rBeforeUnescapedBlockEnd = """(.*?)(?=((?<!%1$s)%2$s))"""
    val rBeforeUnescapedBlockEnd = """(.*?)(?=(""" + rUnescapedBlockEnd + """))"""
    val pBeforeUnescapedBlockEnd: Parser[String] =
      rBeforeUnescapedBlockEnd.format(escapeEscape, blockEnd).r

    val pDelims: Parser[String] = """D""".r ||| """DDD""".r

    //    val pEscape: Parser[String] = escapeEscape.r
    //    val pBlockStart: Parser[String] = blockStart.r
    //    val pBlockEnd: Parser[String] = blockEnd.r
    val pEOF: Parser[String] = """\z""".r

    val blockedContent: Parser[String] =
      pUnescapedBlockStart ~> pBeforeUnescapedBlockEnd <~ pUnescapedBlockEnd

    // Here the ^^ { case (b ~ d) => (b -> d) } statements allow us to control the output of a successful parse
    // in this particular case we want the output to be a Tuple2 of String (blockedContent, delimiter)
    val contentCenter: Parser[(String, String)] =
      (pPadChar ~> blockedContent <~ pPadChar) ~ (pDelims | pEOF) ^^ { case (b ~ d) =>
        (b -> d)
      }
    val contentCenterDelimReq: Parser[(String, String)] =
      (pPadChar ~> blockedContent <~ pPadChar) ~ pDelims ^^ { case (b ~ d) => (b -> d) }

    //    val contentLeft: Parser[(String, String)] = (blockedContent <~ pPadChar) ~ (pDelims | pEOF) ^^ { case (b ~ d) => (b -> d) }
    //    val contentLeftDelimReq: Parser[(String, String)] = (blockedContent <~ pPadChar) ~ pDelims ^^ { case (b ~ d) => (b -> d) }

    //    val contentRight: Parser[(String, String)] = (pPadChar ~> blockedContent) ~ (pDelims | pEOF) ^^ { case (b ~ d) => (b -> d) }
    //    val contentRightDelimReq: Parser[(String, String)] = (pPadChar ~> blockedContent) ~ pDelims ^^ { case (b ~ d) => (b -> d) }

    val content: Parser[(String, String)] = (blockedContent) ~ (pDelims | pEOF) ^^ {
      case (b ~ d) => (b -> d)
    }
    //    val contentDelimReq: Parser[(String, String)] = (blockedContent) ~ pDelims ^^ { case (b ~ d) => (b -> d) }

    // Testing Center justification
    assertEquals(Some("beforeafter", ""), test(contentCenter, "PPPTbeforeafterNPPP"))
    assertEquals(Some("before", "D"), test(contentCenterDelimReq, "PPPTbeforeNPPPDafter"))
    assertEquals(
      None,
      test(contentCenter, "TbeforeDafter")
    ) // Fails because we expect a valid escapeBlockStart AND escapeBlockEnd
    assertEquals(Some("TbeforeSN", "D"), test(contentCenterDelimReq, "PPPTTbeforeSNNPPPDafter"))
    assertEquals(
      Some("TbeforeDPPPstillbeforeSN", "D"),
      test(contentCenterDelimReq, "PPPTTbeforeDPPPstillbeforeSNNPPPDafter")
    )
    assertEquals(
      None,
      test(contentCenterDelimReq, "PPPTTbeforeDPPPstillbeforeNNPPPDafter")
    ) // escapeBlockEnd is not escaped
    assertEquals(
      None,
      test(contentCenter, "PPPTbeforeNafterNPPP")
    ) // escapeBlockEnd is not escaped
    assertEquals(Some("before1SNbefore2", ""), test(contentCenter, "PPPTbefore1SNbefore2NPPP"))
    assertEquals(None, test(content, "PPPTbeforeSNafterNPPP"))
    assertEquals(Some("before1SNbefore2", ""), test(content, "Tbefore1SNbefore2N"))
    assertEquals(Some("before1SNbefore2PP", ""), test(content, "Tbefore1SNbefore2PPN"))
  }

  @Test def testParserEscapeSchemes_SameEscapeWithPadding() = {
    var testNum = 0
    def test(theParser: Parser[(String, String)], theInput: String) = {
      testNum = testNum + 1
      // val result = this.parse(this.log(theParser)("testParserEscapeSchemes_SameEscapeWithPadding." + testNum), theInput)
      val result = this.parse(theParser, theInput)
      if (result.isEmpty) { None }
      else {
        val res = result.get
        Some(res._1, res._2)
      }
    }
    val escape = """E"""
    val delim = """D"""
    val padChar = """P"""

    val rPadChar = """(%s*)"""
    val pPadChar: Parser[String] = rPadChar.format(padChar).r
    val rLeftPadChar = """(%1$s*)(?=([^%1$s]))"""
    val pLeftPadChar: Parser[String] = rLeftPadChar.format(padChar).r

    // Need to tolerate reading to end of data here
    //
    // Assumptions:
    //	A field can be terminated by...
    //		Unescaped delimiter
    //		Padding followed by unescaped delimiter
    //		Padding followed by end of data
    //		End of data
    //
    //    val rBefore = """(.*?)(?=(?:(?<!%1$s)((%1$s%1$s)*)(%3$s*)(((?<!%1$s)(%2$s))|(\z)))|(%1$s(%3$s+)(%2$s|\z))|((?<!%1$s)((%1$s%1$s)*)(%2$s))|\z)"""
    val rBefore = """(.*?)(?=""" + // Give me everything from this point until...
      """(?:(%1$s*)(%3$s*)(((?<!%1$s)(%2$s))|(\z)))""" + // An unescaped/escaped pad char followed by either an unescaped delimiter or end of data
      """|""" + // OR
      """(%1$s(%3$s+)(%2$s|\z))""" + // escape followed by one or more pad chars followed by a delimiter or end of data
      """|""" + // OR
      """((?<!%1$s)((%1$s%1$s)*)(%2$s))""" + // unescaped delimiter
      """|""" + // OR
      """\z)""" // End of data/file

    val pBefore: Parser[String] = rBefore.format(escape, delim, padChar).r

    val rBeforeIgnoreTrailingPadding = """(.*?)(?=((?<!%1$s)((%1$s%1$s)*)(%2$s))|(\z))"""
    val pBeforeIgnoreTrailingPadding: Parser[String] =
      rBeforeIgnoreTrailingPadding.format(escape, delim).r

    val pDelims: Parser[String] = """D""".r ||| """DD""".r
    val pEscape: Parser[String] = """E""".r
    val pEOF: Parser[String] = """\z""".r

    val pEscapedEscape = (pEscape ~ pEscape) ^^ {
      case (e1 ~ e2) => (e1 + e2)
    } // concatenate escapes
    // val pEscapedDelims = not(pEscapedEscape) ~> (pEscape ~> pDelims)
    val pUnescapedDelims = ((pEscapedEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | pDelims

    // Parser captures and creates a string representation of the escapes
    val pEscapes = ((pEscapedEscape.*) ~ opt(pEscape)) ^^ {
      case (l ~ None) => l.mkString
      case (l ~ Some(esc)) => l.mkString + esc
    }

    // Here because of the nature of using the same character for escape and escapeEscape
    // we need to capture the escapes if they exist and make them part of the 'before'
    val pBeforeAndEscsIgnoreTrailingPadding =
      (pBeforeIgnoreTrailingPadding ~ opt(pEscapes)) ^^ {
        case (b ~ None) => b
        case (b ~ Some(e)) => (b + e)
      }

    val pBeforeAndEscs = (pBefore ~ opt(pEscapes | pEscapedEscape | pEscape)) ^^ {
      case (b ~ None) => b
      case (b ~ Some(e)) => (b + e)
    }
    val contentDelimReq = pBeforeAndEscsIgnoreTrailingPadding ~ pUnescapedDelims ^^ {
      case (b ~ d) => (b -> d)
    }
    val contentCenterDelimReq =
      (((pLeftPadChar ~> pBeforeAndEscs) <~ pPadChar) ~ pUnescapedDelims) ^^ { case (b ~ d) =>
        (b -> d)
      }
    val contentLeftDelimReq = ((pBeforeAndEscs <~ pPadChar) ~ pUnescapedDelims) ^^ {
      case (b ~ d) => (b -> d)
    }
    val contentRightDelimReq =
      ((pLeftPadChar ~> pBeforeAndEscsIgnoreTrailingPadding) ~ pUnescapedDelims) ^^ {
        case (b ~ d) => (b -> d)
      }

    val content = pBeforeAndEscsIgnoreTrailingPadding ~ (pUnescapedDelims | pEOF) ^^ {
      case (b ~ d) => (b -> d)
    }
    val contentCenter =
      (((pLeftPadChar ~> pBeforeAndEscs) <~ pPadChar) ~ (pUnescapedDelims | pEOF)) ^^ {
        case (b ~ d) => (b -> d)
      }
    val contentLeft = ((pBeforeAndEscs <~ pPadChar) ~ (pUnescapedDelims | pEOF)) ^^ {
      case (b ~ d) => (b -> d)
    }
    val contentRight =
      ((pLeftPadChar ~> pBeforeAndEscsIgnoreTrailingPadding) ~ (pUnescapedDelims | pEOF)) ^^ {
        case (b ~ d) => (b -> d)
      }

    // Test Delim required works
    assertEquals(None, test(contentDelimReq, "abcEEEEEdefghi"))

    // Testing proper escaping of pads
    assertEquals(Some("textP/", "D"), test(contentLeftDelimReq, "textP/PPDafter"))
    assertEquals(Some("textPP/", "D"), test(contentLeftDelimReq, "textPP/PDafter"))
    assertEquals(Some("textPPP/", "D"), test(contentLeftDelimReq, "textPPP/Dafter"))
    assertEquals(Some("text/", "D"), test(contentLeftDelimReq, "text/PPPDafter"))
    assertEquals(Some("text", "D"), test(contentLeftDelimReq, "textPPPDafter"))
    assertEquals(Some("text", "D"), test(contentLeftDelimReq, "textDafter"))

    assertEquals(Some("/PPPtext", "D"), test(contentRightDelimReq, "/PPPtextDafter"))
    assertEquals(Some("/PPtext", "D"), test(contentRightDelimReq, "P/PPtextDafter"))
    assertEquals(Some("/Ptext", "D"), test(contentRightDelimReq, "PP/PtextDafter"))
    assertEquals(Some("/text", "D"), test(contentRightDelimReq, "PPP/textDafter"))
    assertEquals(Some("text", "D"), test(contentRightDelimReq, "PPPtextDafter"))
    assertEquals(Some("text", "D"), test(contentRightDelimReq, "textDafter"))

    // Verifying that the escapes are working correctly in conjunction with padding on right
    // various combinations attempting to break regex
    assertEquals(
      Some("textPPEE", "D"),
      test(contentCenterDelimReq, "PPPtextPPEEDafter")
    ) // escaped escape, padding part of content
    assertEquals(Some("textEE", "D"), test(contentCenterDelimReq, "PPPtextEEPPPDafter"))
    assertEquals(
      Some("textEPEPEDafter", ""),
      test(contentCenter, "PPPtextEPEPEDafter")
    ) // escaped delim, padding interwoven with escapes
    assertEquals(
      Some("textEPEPEE", "D"),
      test(contentCenter, "PPPtextEPEPEEDafter")
    ) // unescaped delim, padding interwoven with escapes
    assertEquals(
      Some("textPPEDafter", ""),
      test(contentCenter, "PPPtextPPEDafter")
    ) // escaped delim, padding part of content
    assertEquals(
      Some("textEEEDafter", ""),
      test(contentCenter, "PPPtextEEEDafter")
    ) // escaped delim
    assertEquals(Some("textEE", "D"), test(contentCenter, "PPPtextEEDafter")) // escaped escape
    assertEquals(
      Some("textEDafter", ""),
      test(contentCenter, "PPPtextEDafter")
    ) // escaped delim
    assertEquals(Some("text", "D"), test(contentCenter, "PPPtextDafter"))
    assertEquals(
      Some("text", "D"),
      test(contentCenter, "PPPtextPDafter")
    ) // 'unescaped' pad char
    assertEquals(
      Some("textE", "D"),
      test(contentCenter, "PPPtextEPDafter")
    ) // 'escaped' pad char

    assertEquals(Some("/PPPtextPPP/", "D"), test(contentCenterDelimReq, "/PPPtextPPP/Dafter"))
    assertEquals(Some("/PPtextPP/", "D"), test(contentCenterDelimReq, "P/PPtextPP/PDafter"))
    assertEquals(Some("/PtextP/", "D"), test(contentCenterDelimReq, "PP/PtextP/PPDafter"))
    assertEquals(Some("/text/", "D"), test(contentCenterDelimReq, "PPP/text/PPPDafter"))
    assertEquals(Some("text", "D"), test(contentCenterDelimReq, "PPPtextPPPDafter"))
    assertEquals(Some("text", "D"), test(contentCenterDelimReq, "PPPtextDafter"))
    assertEquals(Some("text", "D"), test(contentCenterDelimReq, "textPPPDafter"))
    assertEquals(Some("text", "D"), test(contentCenterDelimReq, "textDafter"))

    assertEquals(Some("textP/", ""), test(contentLeft, "textP/PP"))
    assertEquals(Some("textPP/", ""), test(contentLeft, "textPP/P"))
    assertEquals(Some("textPPP/", ""), test(contentLeft, "textPPP/"))
    assertEquals(Some("text/", ""), test(contentLeft, "text/PPP"))
    assertEquals(Some("text", ""), test(contentLeft, "textPPP"))
    assertEquals(Some("text", ""), test(contentLeft, "text"))

    assertEquals(Some("/PPPtext", ""), test(contentRight, "/PPPtext"))
    assertEquals(Some("/PPtext", ""), test(contentRight, "P/PPtext"))
    assertEquals(Some("/Ptext", ""), test(contentRight, "PP/Ptext"))
    assertEquals(Some("/text", ""), test(contentRight, "PPP/text"))
    assertEquals(Some("text", ""), test(contentRight, "PPPtext"))
    assertEquals(Some("text", ""), test(contentRight, "text"))

    assertEquals(Some("/PPPtextPPP/", ""), test(contentCenter, "/PPPtextPPP/"))
    assertEquals(Some("/PPtextPP/", ""), test(contentCenter, "P/PPtextPP/P"))
    assertEquals(Some("/PtextP/", ""), test(contentCenter, "PP/PtextP/PP"))
    assertEquals(Some("/text/", ""), test(contentCenter, "PPP/text/PPP"))
    assertEquals(Some("text", ""), test(contentCenter, "PPPtextPPP"))
    assertEquals(Some("text", ""), test(contentCenter, "PPPtext"))
    assertEquals(Some("text", ""), test(contentCenter, "textPPP"))
    assertEquals(Some("text", ""), test(contentCenter, "text"))

    // Pad char occur in middle of content
    assertEquals(Some("tePxt", ""), test(contentCenter, "PPPtePxtPPP"))
    assertEquals(Some("^PPPtePxt^", ""), test(contentCenter, "^PPPtePxt^PPP"))
    assertEquals(Some("^PPtePxtP^", ""), test(contentCenter, "P^PPtePxtP^PP"))
    assertEquals(Some("^PtePxtPP^", ""), test(contentCenter, "PP^PtePxtPP^P"))
    assertEquals(Some("^tePxtPPP^", ""), test(contentCenter, "PPP^tePxtPPP^"))

    // Verify that the escaping of delimiters still works with padding
    assertEquals(Some("teEDxt", ""), test(contentCenter, "PPPteEDxtPPP"))
    assertEquals(Some("^PPPteEDPxt^", ""), test(contentCenter, "^PPPteEDPxt^PPP"))
    assertEquals(Some("^PPteEDPxtP^", ""), test(contentCenter, "P^PPteEDPxtP^PP"))
    assertEquals(Some("^PteEDPxtPP^", ""), test(contentCenter, "PP^PteEDPxtPP^P"))
    assertEquals(Some("^teEDPxtPPP^", ""), test(contentCenter, "PPP^teEDPxtPPP^"))

    // Tests
    assertEquals(Some("textPE", "D"), test(contentCenterDelimReq, "PPPtextPEPPD"))

    // Testing no justification, padding is treated as part of content
    assertEquals(Some("before", "D"), test(contentDelimReq, "beforeDafter"))
    assertEquals(Some("PPPbefore", "D"), test(contentDelimReq, "PPPbeforeDafter"))
    assertEquals(Some("beforePPP", "D"), test(contentDelimReq, "beforePPPDafter"))
    assertEquals(Some("PPPbeforePPP", "D"), test(contentDelimReq, "PPPbeforePPPDafter"))

    // Testing no justification, padding is treated as part of content w/ escaping
    assertEquals(Some("abcEEEEEdef", "D"), test(contentDelimReq, "abcEEEEEdefDghi"))
    assertEquals(Some("abcSESESEdef", "D"), test(content, "abcSESESEdefDghi"))
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentDelimReq, "beforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("PPPbeforeEDstillbefore", "D"),
      test(contentDelimReq, "PPPbeforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbeforePPP", "D"),
      test(contentDelimReq, "beforePPPEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("PPPbeforePPPEDstillbeforePPP", "D"),
      test(contentDelimReq, "PPPbeforePPPEDstillbeforePPPDafter")
    )

    // Testing no justification, padding is treated as part of content w/ escaped escaping
    assertEquals(Some("beforeEE", "D"), test(contentDelimReq, "beforeEEDafter1Dafter2"))
    assertEquals(Some("PPPbeforeEE", "D"), test(contentDelimReq, "PPPbeforeEEDafter1Dafter2"))
    assertEquals(
      Some("beforePPPEE", "D"),
      test(contentDelimReq, "beforePPPEEDafter1PPPDafter2")
    )
    assertEquals(
      Some("PPPbeforePPPEE", "D"),
      test(contentDelimReq, "PPPbeforePPPEEDafter1PPPDafter2")
    )

    // Testing Center justification, padding is not part of content
    assertEquals(Some("before", "D"), test(contentCenterDelimReq, "beforeDafter"))
    assertEquals(Some("before", "D"), test(contentCenterDelimReq, "PPPbeforeDafter"))
    assertEquals(Some("before", "D"), test(contentCenterDelimReq, "beforePPPDafter"))
    assertEquals(Some("before", "D"), test(contentCenterDelimReq, "PPPbeforePPPDafter"))

    // Testing Center justification, padding part of content w/ escaping
    assertEquals(Some("abcEEEEEdef", "D"), test(contentCenterDelimReq, "abcEEEEEdefDghi"))
    assertEquals(Some("abcEEEEdef", "D"), test(contentCenterDelimReq, "abcEEEEdefDghi"))
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentCenterDelimReq, "beforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentCenterDelimReq, "PPPbeforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbefore", "D"),
      test(contentCenterDelimReq, "beforePPPEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbefore", "D"),
      test(contentCenterDelimReq, "PPPbeforePPPEDstillbeforePPPDafter")
    )

    // Testing Center justification, padding is part of content w/ escaped escaping
    assertEquals(Some("beforeEE", "D"), test(contentCenterDelimReq, "beforeEEDafter1Dafter2"))
    assertEquals(
      Some("beforeEE", "D"),
      test(contentCenterDelimReq, "PPPbeforeEEDafter1Dafter2")
    )
    assertEquals(
      Some("beforePPPEE", "D"),
      test(contentCenterDelimReq, "beforePPPEEDafter1PPPDafter2")
    )
    assertEquals(
      Some("beforePPPEE", "D"),
      test(contentCenterDelimReq, "PPPbeforePPPEEDafter1PPPDafter2")
    )

    // Testing Left justification, padding on right removed only
    assertEquals(Some("before", "D"), test(contentLeftDelimReq, "beforeDafter"))
    assertEquals(Some("PPPbefore", "D"), test(contentLeftDelimReq, "PPPbeforeDafter"))
    assertEquals(Some("before", "D"), test(contentLeftDelimReq, "beforePPPDafter"))
    assertEquals(Some("PPPbefore", "D"), test(contentLeftDelimReq, "PPPbeforePPPDafter"))

    // Testing Left justification, padding on right removed only w/ escaping
    assertEquals(Some("abcEEEEEdef", "D"), test(contentLeftDelimReq, "abcEEEEEdefDghi"))
    assertEquals(Some("abcEEEEEEdef", "D"), test(contentLeftDelimReq, "abcEEEEEEdefDghi"))
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentLeftDelimReq, "beforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("PPPbeforeEDstillbefore", "D"),
      test(contentLeftDelimReq, "PPPbeforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbefore", "D"),
      test(contentLeftDelimReq, "beforePPPEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("PPPbeforePPPEDstillbefore", "D"),
      test(contentLeftDelimReq, "PPPbeforePPPEDstillbeforePPPDafter")
    )

    // Testing Left justification, padding on right removed only w/ escaped escaping
    assertEquals(Some("beforeEE", "D"), test(contentLeftDelimReq, "beforeEEDafter1Dafter2"))
    assertEquals(
      Some("PPPbeforeEE", "D"),
      test(contentLeftDelimReq, "PPPbeforeEEDafter1Dafter2")
    )
    assertEquals(
      Some("beforePPPEE", "D"),
      test(contentLeftDelimReq, "beforePPPEEDafter1PPPDafter2")
    )
    assertEquals(
      Some("PPPbeforePPPEE", "D"),
      test(contentLeftDelimReq, "PPPbeforePPPEEDafter1PPPDafter2")
    )

    // Testing Right justification, padding on left removed only
    assertEquals(Some("before", "D"), test(contentRightDelimReq, "beforeDafter"))
    assertEquals(Some("before", "D"), test(contentRightDelimReq, "PPPbeforeDafter"))
    assertEquals(Some("beforePPP", "D"), test(contentRightDelimReq, "beforePPPDafter"))
    assertEquals(Some("beforePPP", "D"), test(contentRightDelimReq, "PPPbeforePPPDafter"))

    // Testing Right justification, padding on left removed only w/ escaping
    assertEquals(Some("abcEEEEEdef", "D"), test(contentRightDelimReq, "abcEEEEEdefDghi"))
    assertEquals(Some("abcEEEEEEdef", "D"), test(contentRightDelimReq, "abcEEEEEEdefDghi"))
    assertEquals(Some("abcEE", "D"), test(contentRightDelimReq, "abcEEDefDghi"))
    assertEquals(Some("abcEEEDef", "D"), test(contentRightDelimReq, "abcEEEDefDghi"))
    assertEquals(Some("abcEEEE", "D"), test(contentRightDelimReq, "abcEEEEDefDghi"))
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentRightDelimReq, "beforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentRightDelimReq, "PPPbeforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbeforePPP", "D"),
      test(contentRightDelimReq, "beforePPPEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbeforePPP", "D"),
      test(contentRightDelimReq, "PPPbeforePPPEDstillbeforePPPDafter")
    )

    // Testing Right justification, padding on left removed only w/ escaped escaping
    assertEquals(
      Some("beforeEE", "D"),
      test(contentRightDelimReq, "beforeEEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforeEE", "D"),
      test(contentRightDelimReq, "PPPbeforeEEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforePPPEE", "D"),
      test(contentRightDelimReq, "beforePPPEEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("beforePPPEE", "D"),
      test(contentRightDelimReq, "PPPbeforePPPEEDstillbeforePPPDafter")
    )

    // Testing Escaped Padding
    assertEquals(
      Some("beforeEEPPP", "D"),
      test(contentRightDelimReq, "PPPbeforeEEPPPDafter1Dafter2")
    )
    assertEquals(
      Some("beforeEPPP", "D"),
      test(contentRightDelimReq, "PPPbeforeEPPPDafter1Dafter2")
    )
    assertEquals(
      Some("PPPbeforeEE", "D"),
      test(contentLeftDelimReq, "PPPbeforeEEPPPDafter1Dafter2")
    )
    assertEquals(
      Some("PPPbeforeE", "D"),
      test(contentLeftDelimReq, "PPPbeforeEPPPDafter1Dafter2")
    )
    assertEquals(
      Some("beforeEE", "D"),
      test(contentCenterDelimReq, "PPPbeforeEEPPPDafter1Dafter2")
    )
    assertEquals(
      Some("beforeE", "D"),
      test(contentCenterDelimReq, "PPPbeforeEPPPDafter1Dafter2")
    )
    assertEquals(
      Some("EEPPPbeforeEE", "D"),
      test(contentCenterDelimReq, "EEPPPbeforeEEPPPDafter1Dafter2")
    )
    assertEquals(
      Some("EPPPbeforeE", "D"),
      test(contentCenterDelimReq, "EPPPbeforeEPPPDafter1Dafter2")
    )
  }

  @Test def testParserEscapeSchemes_DiffEscapesWithPadding() = {
    var testNum = 0
    def test(theParser: Parser[(String, String)], theInput: String) = {
      testNum = testNum + 1
      // val result = this.parse(this.log(theParser)("testParserEscapeSchemes_DiffEscapesWithPadding." + testNum), theInput)
      val result = this.parse(theParser, theInput)
      if (result.isEmpty) { None }
      else {
        val res = result.get
        Some(res._1, res._2)
      }
    }

    val escapeEscape = """S"""
    val escape = """E"""
    val delim = """D"""
    val padChar = """P"""

    val rPadChar = """(%s*)"""
    val pPadChar: Parser[String] = rPadChar.format(padChar).r
    val rLeftPadChar = """(%1$s*)(?=([^%1$s]))""" // LeftPad precedes non pad characters
    val pLeftPadChar: Parser[String] = rLeftPadChar.format(padChar).r
    val pEOF: Parser[String] = """\z""".r

    // Content is anything until:
    // 1. Padding ~ delimiter
    // 2. unescaped delimiter
    // 3. Optional Padding ~ EndOfData
    val rBefore = """(.*?)(?=(%4$s+%3$s)|((?<!(?<!%1$s)%2$s)%3$s)|(%4$s*\z))"""
    val pBefore: Parser[String] = rBefore.format(escapeEscape, escape, delim, padChar).r

    val rBeforeIgnoreTrailingPadding = """(.*?)(?=(?:(?<!(?<!%1$s)%2$s)%3$s)|\z)"""
    val pBeforeIgnoreTrailingPadding: Parser[String] =
      rBeforeIgnoreTrailingPadding.format(escapeEscape, escape, delim).r

    val pDelims: Parser[String] = """D""".r ||| """DD""".r
    val pEscape: Parser[String] = """E""".r
    val pEscapeEscape: Parser[String] = """S""".r
    //    val pEscapedEscape = pEscapeEscape ~ pEscape
    //    val pEscapedDelims = not(pEscapedEscape) ~> (pEscape ~> pDelims)
    val pUnescapedDelims = ((pEscapeEscape ~ pEscape) ~> pDelims) | (not(
      pEscape
    ) ~> pDelims) | (pEscapeEscape ~> pDelims) | pDelims

    // Here ^^ { case(b ~ d) => (b -> d) } changes the output of a successful parse to a Tuple2 of String (before, delimiter)
    val content = (pBeforeIgnoreTrailingPadding ~ (pUnescapedDelims | pEOF)) ^^ {
      case (b ~ d) => (b -> d)
    }
    val contentCenter =
      (((pLeftPadChar ~> pBefore) <~ pPadChar) ~ (pUnescapedDelims | pEOF)) ^^ { case (b ~ d) =>
        (b -> d)
      }
    val contentLeft = ((pBefore <~ pPadChar) ~ (pUnescapedDelims | pEOF)) ^^ { case (b ~ d) =>
      (b -> d)
    }
    val contentRight =
      ((pLeftPadChar ~> pBeforeIgnoreTrailingPadding) ~ (pUnescapedDelims | pEOF)) ^^ {
        case (b ~ d) => (b -> d)
      }

    //    val contentDelimReq = (pBeforeIgnoreTrailingPadding ~ pUnescapedDelims) ^^ { case (b ~ d) => (b -> d) }
    val contentCenterDelimReq =
      (((pLeftPadChar ~> pBefore) <~ pPadChar) ~ pUnescapedDelims) ^^ { case (b ~ d) =>
        (b -> d)
      }
    val contentLeftDelimReq = ((pBefore <~ pPadChar) ~ pUnescapedDelims) ^^ { case (b ~ d) =>
      (b -> d)
    }
    val contentRightDelimReq =
      ((pLeftPadChar ~> pBeforeIgnoreTrailingPadding) ~ pUnescapedDelims) ^^ { case (b ~ d) =>
        (b -> d)
      }

    // NOTE: At this point we've left the escapes in the content
    // we can probably go a step further and execute a regex replace on
    // the escape characters.  But order will likely matter.
    //
    // 1. Escapes should be removed unless preceded by an escapeEscape
    // 2. EscapeEscapes should be removed when they precede an escape.

    // Test padding blind escape
    // Encounter non-pad char amongst padding
    assertEquals(Some("text", ""), test(contentCenter, "PPPtextPPP"))
    assertEquals(Some("/PPPtext/", ""), test(contentCenter, "/PPPtext/PPP"))
    assertEquals(Some("/PPtextP/", ""), test(contentCenter, "P/PPtextP/PP"))
    assertEquals(Some("/PtextPP/", ""), test(contentCenter, "PP/PtextPP/P"))
    assertEquals(Some("/textPPP/", ""), test(contentCenter, "PPP/textPPP/"))

    // Encounter non-pad char amongst padding (verify no dependence on /)
    assertEquals(Some("text", ""), test(contentCenter, "PPPtextPPP"))
    assertEquals(Some("^PPPtext^", ""), test(contentCenter, "^PPPtext^PPP"))
    assertEquals(Some("^PPtextP^", ""), test(contentCenter, "P^PPtextP^PP"))
    assertEquals(Some("^PtextPP^", ""), test(contentCenter, "PP^PtextPP^P"))
    assertEquals(Some("^textPPP^", ""), test(contentCenter, "PPP^textPPP^"))

    // Encounter escape amongst padding
    assertEquals(Some("text", ""), test(contentCenter, "PPPtextPPP"))
    assertEquals(Some("EPPPtextE", ""), test(contentCenter, "EPPPtextEPPP"))
    assertEquals(Some("EPPtextPE", ""), test(contentCenter, "PEPPtextPEPP"))
    assertEquals(Some("EPtextPPE", ""), test(contentCenter, "PPEPtextPPEP"))
    assertEquals(Some("EtextPPPE", ""), test(contentCenter, "PPPEtextPPPE"))

    // Encounter escape and escapeEscape amongst padding
    assertEquals(Some("text", ""), test(contentCenter, "PPPtextPPP"))
    assertEquals(Some("SEPPPtextSE", ""), test(contentCenter, "SEPPPtextSEPPP"))
    assertEquals(Some("SEPPtextPSE", ""), test(contentCenter, "PSEPPtextPSEPP"))
    assertEquals(Some("SEPtextPPSE", ""), test(contentCenter, "PPSEPtextPPSEP"))
    assertEquals(Some("SEtextPPPSE", ""), test(contentCenter, "PPPSEtextPPPSE"))

    // Pad char occur in middle of content
    assertEquals(Some("tePxt", ""), test(contentCenter, "PPPtePxtPPP"))
    assertEquals(Some("^PPPtePxt^", ""), test(contentCenter, "^PPPtePxt^PPP"))
    assertEquals(Some("^PPtePxtP^", ""), test(contentCenter, "P^PPtePxtP^PP"))
    assertEquals(Some("^PtePxtPP^", ""), test(contentCenter, "PP^PtePxtPP^P"))
    assertEquals(Some("^tePxtPPP^", ""), test(contentCenter, "PPP^tePxtPPP^"))

    // Verify that the escaping of delimiters still works with padding
    assertEquals(Some("teEDxt", ""), test(contentCenter, "PPPteEDxtPPP"))
    assertEquals(Some("^PPPteEDPxt^", ""), test(contentCenter, "^PPPteEDPxt^PPP"))
    assertEquals(Some("^PPteEDPxtP^", ""), test(contentCenter, "P^PPteEDPxtP^PP"))
    assertEquals(Some("^PteEDPxtPP^", ""), test(contentCenter, "PP^PteEDPxtPP^P"))
    assertEquals(Some("^teEDPxtPPP^", ""), test(contentCenter, "PPP^teEDPxtPPP^"))

    // Verify that delim required works
    assertEquals(None, test(contentCenterDelimReq, "textafter"))

    // Testing proper escaping of pads when delimiter required
    assertEquals(Some("textP/", "D"), test(contentLeftDelimReq, "textP/PPDafter"))
    assertEquals(Some("textPP/", "D"), test(contentLeftDelimReq, "textPP/PDafter"))
    assertEquals(Some("textPPP/", "D"), test(contentLeftDelimReq, "textPPP/Dafter"))
    assertEquals(Some("text/", "D"), test(contentLeftDelimReq, "text/PPPDafter"))
    assertEquals(Some("text", "D"), test(contentLeftDelimReq, "textPPPDafter"))
    assertEquals(Some("text", "D"), test(contentLeftDelimReq, "textDafter"))

    assertEquals(Some("/PPPtext", "D"), test(contentRightDelimReq, "/PPPtextDafter"))
    assertEquals(Some("/PPtext", "D"), test(contentRightDelimReq, "P/PPtextDafter"))
    assertEquals(Some("/Ptext", "D"), test(contentRightDelimReq, "PP/PtextDafter"))
    assertEquals(Some("/text", "D"), test(contentRightDelimReq, "PPP/textDafter"))
    assertEquals(Some("text", "D"), test(contentRightDelimReq, "PPPtextDafter"))
    assertEquals(Some("text", "D"), test(contentRightDelimReq, "textDafter"))

    assertEquals(Some("/PPPtextPPP/", "D"), test(contentCenterDelimReq, "/PPPtextPPP/Dafter"))
    assertEquals(Some("/PPtextPP/", "D"), test(contentCenterDelimReq, "P/PPtextPP/PDafter"))
    assertEquals(Some("/PtextP/", "D"), test(contentCenterDelimReq, "PP/PtextP/PPDafter"))
    assertEquals(Some("/text/", "D"), test(contentCenterDelimReq, "PPP/text/PPPDafter"))
    assertEquals(Some("text", "D"), test(contentCenterDelimReq, "PPPtextPPPDafter"))
    assertEquals(Some("text", "D"), test(contentCenterDelimReq, "PPPtextDafter"))
    assertEquals(Some("text", "D"), test(contentCenterDelimReq, "textPPPDafter"))
    assertEquals(Some("text", "D"), test(contentCenterDelimReq, "textDafter"))

    assertEquals(Some("textP/", ""), test(contentLeft, "textP/PP"))
    assertEquals(Some("textPP/", ""), test(contentLeft, "textPP/P"))
    assertEquals(Some("textPPP/", ""), test(contentLeft, "textPPP/"))
    assertEquals(Some("text/", ""), test(contentLeft, "text/PPP"))
    assertEquals(Some("text", ""), test(contentLeft, "textPPP"))
    assertEquals(Some("text", ""), test(contentLeft, "text"))

    assertEquals(Some("/PPPtext", ""), test(contentRight, "/PPPtext"))
    assertEquals(Some("/PPtext", ""), test(contentRight, "P/PPtext"))
    assertEquals(Some("/Ptext", ""), test(contentRight, "PP/Ptext"))
    assertEquals(Some("/text", ""), test(contentRight, "PPP/text"))
    assertEquals(Some("text", ""), test(contentRight, "PPPtext"))
    assertEquals(Some("text", ""), test(contentRight, "text"))

    // Testing no justification, padding is treated as part of content
    assertEquals(Some("before", "D"), test(content, "beforeDafter"))
    assertEquals(Some("PPPbefore", "D"), test(content, "PPPbeforeDafter"))
    assertEquals(Some("beforePPP", "D"), test(content, "beforePPPDafter"))
    assertEquals(Some("PPPbeforePPP", "D"), test(content, "PPPbeforePPPDafter"))

    // Testing no justification, padding is treated as part of content w/ escaping
    assertEquals(Some("abcEEEEEdef", "D"), test(content, "abcEEEEEdefDghi"))
    assertEquals(Some("abcSESESEdef", "D"), test(content, "abcSESESEdefDghi"))
    assertEquals(Some("beforeEDstillbefore", "D"), test(content, "beforeEDstillbeforeDafter"))
    assertEquals(
      Some("PPPbeforeEDstillbefore", "D"),
      test(content, "PPPbeforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbeforePPP", "D"),
      test(content, "beforePPPEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("PPPbeforePPPEDstillbeforePPP", "D"),
      test(content, "PPPbeforePPPEDstillbeforePPPDafter")
    )

    // Testing no justification, padding is treated as part of content w/ escaped escaping
    assertEquals(Some("beforeSE", "D"), test(content, "beforeSEDstillbeforeDafter"))
    assertEquals(Some("PPPbeforeSE", "D"), test(content, "PPPbeforeSEDstillbeforeDafter"))
    assertEquals(Some("beforePPPSE", "D"), test(content, "beforePPPSEDstillbeforePPPDafter"))
    assertEquals(
      Some("PPPbeforePPPSE", "D"),
      test(content, "PPPbeforePPPSEDstillbeforePPPDafter")
    )

    // Testing Center justification, padding is not part of content
    assertEquals(Some("before", "D"), test(contentCenter, "beforeDafter"))
    assertEquals(Some("before", "D"), test(contentCenter, "PPPbeforeDafter"))
    assertEquals(Some("before", "D"), test(contentCenter, "beforePPPDafter"))
    assertEquals(Some("before", "D"), test(contentCenter, "PPPbeforePPPDafter"))

    // Testing Center justification, padding part of content w/ escaping
    assertEquals(Some("abcEEEEEdef", "D"), test(contentCenter, "abcEEEEEdefDghi"))
    assertEquals(Some("abcSESESEdef", "D"), test(contentCenter, "abcSESESEdefDghi"))
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentCenter, "beforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentCenter, "PPPbeforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbefore", "D"),
      test(contentCenter, "beforePPPEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbefore", "D"),
      test(contentCenter, "PPPbeforePPPEDstillbeforePPPDafter")
    )

    // Testing Center justification, padding is part of content w/ escaped escaping
    assertEquals(Some("beforeSE", "D"), test(contentCenter, "beforeSEDstillbeforeDafter"))
    assertEquals(Some("beforeSE", "D"), test(contentCenter, "PPPbeforeSEDstillbeforeDafter"))
    assertEquals(
      Some("beforePPPSE", "D"),
      test(contentCenter, "beforePPPSEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("beforePPPSE", "D"),
      test(contentCenter, "PPPbeforePPPSEDstillbeforePPPDafter")
    )

    // Testing Left justification, padding on right removed only
    assertEquals(Some("before", "D"), test(contentLeft, "beforeDafter"))
    assertEquals(Some("PPPbefore", "D"), test(contentLeft, "PPPbeforeDafter"))
    assertEquals(Some("before", "D"), test(contentLeft, "beforePPPDafter"))
    assertEquals(Some("PPPbefore", "D"), test(contentLeft, "PPPbeforePPPDafter"))

    // Testing Left justification, padding on right removed only w/ escaping
    assertEquals(Some("abcEEEEEdef", "D"), test(contentLeft, "abcEEEEEdefDghi"))
    assertEquals(Some("abcSESESEdef", "D"), test(contentLeft, "abcSESESEdefDghi"))
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentLeft, "beforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("PPPbeforeEDstillbefore", "D"),
      test(contentLeft, "PPPbeforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbefore", "D"),
      test(contentLeft, "beforePPPEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("PPPbeforePPPEDstillbefore", "D"),
      test(contentLeft, "PPPbeforePPPEDstillbeforePPPDafter")
    )

    // Testing Left justification, padding on right removed only w/ escaped escaping
    assertEquals(Some("beforeSE", "D"), test(contentLeft, "beforeSEDstillbeforeDafter"))
    assertEquals(Some("PPPbeforeSE", "D"), test(contentLeft, "PPPbeforeSEDstillbeforeDafter"))
    assertEquals(
      Some("beforePPPSE", "D"),
      test(contentLeft, "beforePPPSEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("PPPbeforePPPSE", "D"),
      test(contentLeft, "PPPbeforePPPSEDstillbeforePPPDafter")
    )

    // Testing Right justification, padding on left removed only
    assertEquals(Some("before", "D"), test(contentRight, "beforeDafter"))
    assertEquals(Some("before", "D"), test(contentRight, "PPPbeforeDafter"))
    assertEquals(Some("beforePPP", "D"), test(contentRight, "beforePPPDafter"))
    assertEquals(Some("beforePPP", "D"), test(contentRight, "PPPbeforePPPDafter"))

    // Testing Right justification, padding on left removed only w/ escaping
    assertEquals(Some("abcEEEEEdef", "D"), test(contentRight, "abcEEEEEdefDghi"))
    assertEquals(Some("abcSESESEdef", "D"), test(contentRight, "abcSESESEdefDghi"))
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentRight, "beforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforeEDstillbefore", "D"),
      test(contentRight, "PPPbeforeEDstillbeforeDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbeforePPP", "D"),
      test(contentRight, "beforePPPEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("beforePPPEDstillbeforePPP", "D"),
      test(contentRight, "PPPbeforePPPEDstillbeforePPPDafter")
    )

    // Testing Right justification, padding on left removed only w/ escaped escaping
    assertEquals(Some("beforeSE", "D"), test(contentRight, "beforeSEDstillbeforeDafter"))
    assertEquals(Some("beforeSE", "D"), test(contentRight, "PPPbeforeSEDstillbeforeDafter"))
    assertEquals(
      Some("beforePPPSE", "D"),
      test(contentRight, "beforePPPSEDstillbeforePPPDafter")
    )
    assertEquals(
      Some("beforePPPSE", "D"),
      test(contentRight, "PPPbeforePPPSEDstillbeforePPPDafter")
    )
  }

  //  @Test def testParserEscapeSchemes_SameEscapeWithPadding() = {
  //    var testNum = 0
  //    def test(theParser: Parser[(String, String)], theInput: String) = {
  //      testNum = testNum + 1
  //      val result = this.parse(this.log(theParser)("testParserEscapeSchemes_SameEscapeWithPadding." + testNum), theInput)
  //      if (result.isEmpty) { None }
  //      else {
  //        val res = result.get
  //        Some(res._1, res._2)
  //      }
  //    }
  //    val escape = """E"""
  //    val delim = """D"""
  //    val padChar = """P"""
  //
  //    val rPadChar = """(%s*)"""
  //    val pPadChar: Parser[String] = rPadChar.format(padChar).r
  //
  //    // (.*?)(?=(?:P+(?<!E)((?:EE)*)(?:P*)(?<!E)(D))|(?:(?<!E)((?:EE)*)(D)))
  //    val rBefore = """(.*?)(?=(?:(?<!%1$s)((%1$s%1$s)*)(%3$s*)(?<!%1$s)(%2$s))|(?<!%1$s)((%1$s%1$s)*)(%2$s))"""
  //    val rBeforeIgnoreTrailingPadding = """(.*?)(?=(?<!%1$s)((%1$s%1$s)*)(%2$s))"""
  //    val pBefore: Parser[String] = rBefore.format(escape, delim, padChar).r
  //    val pBeforeIgnoreTrailingPadding: Parser[String] = rBeforeIgnoreTrailingPadding.format(escape, delim).r
  //
  //    val pDelims: Parser[String] = """D""".r ||| """DD""".r
  //    val pEscape: Parser[String] = """E""".r
  //
  //    val pEscapedEscape = (pEscape ~ pEscape) ^^ { e => (e._1 + e._2) }
  //    val pEscapedDelims = not(pEscapedEscape) ~> (pEscape ~> pDelims)
  //    val pUnescapedDelims = ((pEscapedEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | pDelims
  //
  //    val pEscapes = ((pEscapedEscape*) ~ opt(pEscape)) ^^ {
  //      case (l ~ None) => {
  //        val escEscs = new StringBuilder
  //        l.foreach(x => escEscs.append(x))
  //        (escEscs)
  //      }
  //      case (l ~ Some(esc)) => {
  //        val escEscs = new StringBuilder
  //        l.foreach(x => escEscs.append(x))
  //        (escEscs + esc)
  //      }
  //    }
  //
  //    // Here because of the nature of using the same character for escape and escapeEscape
  //    // we need to capture the escapes
  //    val pBeforeAndEscsIgnoreTrailingPadding = (pBeforeIgnoreTrailingPadding ~ opt(pEscapes)) ^^ {
  //      case (b ~ None) => b
  //      case (b ~ Some(e)) => (b + e)
  //    }
  //
  //    val pBeforeAndEscs = (pBefore ~ opt(pEscapes | pEscapedEscape | pEscape)) ^^ {
  //      case (b ~ None) => b
  //      case (b ~ Some(e)) => (b + e)
  //    }
  //    val content = pBeforeAndEscsIgnoreTrailingPadding ~ pUnescapedDelims ^^ { case (b ~ d) => (b -> d) }
  //    val contentCenterJustified = (((pPadChar ~> pBeforeAndEscs) <~ pPadChar) ~ pUnescapedDelims) ^^ { case (b ~ d) => (b -> d) }
  //    val contentLeftJustified = ((pBeforeAndEscs <~ pPadChar) ~ pUnescapedDelims) ^^ { case (b ~ d) => (b -> d) }
  //    val contentRightJustified = ((pPadChar ~> pBeforeAndEscsIgnoreTrailingPadding) ~ pUnescapedDelims) ^^ { case (b ~ d) => (b -> d) }
  //
  //    assertEquals(Some("textPE", "D"), test(contentCenterJustified, "PPPtextPEPPD"))
  //
  //    // Testing no justification, padding is treated as part of content
  //    assertEquals(Some("before", "D"), test(content, "beforeDafter"))
  //    assertEquals(Some("PPPbefore", "D"), test(content, "PPPbeforeDafter"))
  //    assertEquals(Some("beforePPP", "D"), test(content, "beforePPPDafter"))
  //    assertEquals(Some("PPPbeforePPP", "D"), test(content, "PPPbeforePPPDafter"))
  //
  //    // Testing no justification, padding is treated as part of content w/ escaping
  //    assertEquals(Some("abcEEEEEdef", "D"), test(content, "abcEEEEEdefDghi"))
  //    assertEquals(Some("abcSESESEdef", "D"), test(content, "abcSESESEdefDghi"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(content, "beforeEDstillbeforeDafter"))
  //    assertEquals(Some("PPPbeforeEDstillbefore", "D"), test(content, "PPPbeforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEDstillbeforePPP", "D"), test(content, "beforePPPEDstillbeforePPPDafter"))
  //    assertEquals(Some("PPPbeforePPPEDstillbeforePPP", "D"), test(content, "PPPbeforePPPEDstillbeforePPPDafter"))
  //
  //    // Testing no justification, padding is treated as part of content w/ escaped escaping
  //    assertEquals(Some("beforeEE", "D"), test(content, "beforeEEDstillbeforeDafter"))
  //    assertEquals(Some("PPPbeforeEE", "D"), test(content, "PPPbeforeEEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEE", "D"), test(content, "beforePPPEEDstillbeforePPPDafter"))
  //    assertEquals(Some("PPPbeforePPPEE", "D"), test(content, "PPPbeforePPPEEDstillbeforePPPDafter"))
  //
  //    // Testing Center justification, padding is not part of content
  //    assertEquals(Some("before", "D"), test(contentCenterJustified, "beforeDafter"))
  //    assertEquals(Some("before", "D"), test(contentCenterJustified, "PPPbeforeDafter"))
  //    assertEquals(Some("before", "D"), test(contentCenterJustified, "beforePPPDafter"))
  //    assertEquals(Some("before", "D"), test(contentCenterJustified, "PPPbeforePPPDafter"))
  //
  //    // Testing Center justification, padding part of content w/ escaping
  //    assertEquals(Some("abcEEEEEdef", "D"), test(contentCenterJustified, "abcEEEEEdefDghi"))
  //    assertEquals(Some("abcEEEEdef", "D"), test(contentCenterJustified, "abcEEEEdefDghi"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(contentCenterJustified, "beforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(contentCenterJustified, "PPPbeforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEDstillbefore", "D"), test(contentCenterJustified, "beforePPPEDstillbeforePPPDafter"))
  //    assertEquals(Some("beforePPPEDstillbefore", "D"), test(contentCenterJustified, "PPPbeforePPPEDstillbeforePPPDafter"))
  //
  //    // Testing Center justification, padding is part of content w/ escaped escaping
  //    assertEquals(Some("beforeEE", "D"), test(contentCenterJustified, "beforeEEDstillbeforeDafter"))
  //    assertEquals(Some("beforeEE", "D"), test(contentCenterJustified, "PPPbeforeEEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEE", "D"), test(contentCenterJustified, "beforePPPEEDstillbeforePPPDafter"))
  //    assertEquals(Some("beforePPPEE", "D"), test(contentCenterJustified, "PPPbeforePPPEEDstillbeforePPPDafter"))
  //
  //    // Testing Left justification, padding on right removed only
  //    assertEquals(Some("before", "D"), test(contentLeftJustified, "beforeDafter"))
  //    assertEquals(Some("PPPbefore", "D"), test(contentLeftJustified, "PPPbeforeDafter"))
  //    assertEquals(Some("before", "D"), test(contentLeftJustified, "beforePPPDafter"))
  //    assertEquals(Some("PPPbefore", "D"), test(contentLeftJustified, "PPPbeforePPPDafter"))
  //
  //    // Testing Left justification, padding on right removed only w/ escaping
  //    assertEquals(Some("abcEEEEEdef", "D"), test(contentLeftJustified, "abcEEEEEdefDghi"))
  //    assertEquals(Some("abcEEEEEEdef", "D"), test(contentLeftJustified, "abcEEEEEEdefDghi"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(contentLeftJustified, "beforeEDstillbeforeDafter"))
  //    assertEquals(Some("PPPbeforeEDstillbefore", "D"), test(contentLeftJustified, "PPPbeforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEDstillbefore", "D"), test(contentLeftJustified, "beforePPPEDstillbeforePPPDafter"))
  //    assertEquals(Some("PPPbeforePPPEDstillbefore", "D"), test(contentLeftJustified, "PPPbeforePPPEDstillbeforePPPDafter"))
  //
  //    // Testing Left justification, padding on right removed only w/ escaped escaping
  //    assertEquals(Some("beforeEE", "D"), test(contentLeftJustified, "beforeEEDstillbeforeDafter"))
  //    assertEquals(Some("PPPbeforeEE", "D"), test(contentLeftJustified, "PPPbeforeEEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEE", "D"), test(contentLeftJustified, "beforePPPEEDstillbeforePPPDafter"))
  //    assertEquals(Some("PPPbeforePPPEE", "D"), test(contentLeftJustified, "PPPbeforePPPEEDstillbeforePPPDafter"))
  //
  //    // Testing Right justification, padding on left removed only
  //    assertEquals(Some("before", "D"), test(contentRightJustified, "beforeDafter"))
  //    assertEquals(Some("before", "D"), test(contentRightJustified, "PPPbeforeDafter"))
  //    assertEquals(Some("beforePPP", "D"), test(contentRightJustified, "beforePPPDafter"))
  //    assertEquals(Some("beforePPP", "D"), test(contentRightJustified, "PPPbeforePPPDafter"))
  //
  //    // Testing Right justification, padding on left removed only w/ escaping
  //    assertEquals(Some("abcEEEEEdef", "D"), test(contentRightJustified, "abcEEEEEdefDghi"))
  //    assertEquals(Some("abcEEEEEEdef", "D"), test(contentRightJustified, "abcEEEEEEdefDghi"))
  //    assertEquals(Some("abcEE", "D"), test(contentRightJustified, "abcEEDefDghi"))
  //    assertEquals(Some("abcEEEDef", "D"), test(contentRightJustified, "abcEEEDefDghi"))
  //    assertEquals(Some("abcEEEE", "D"), test(contentRightJustified, "abcEEEEDefDghi"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(contentRightJustified, "beforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(contentRightJustified, "PPPbeforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEDstillbeforePPP", "D"), test(contentRightJustified, "beforePPPEDstillbeforePPPDafter"))
  //    assertEquals(Some("beforePPPEDstillbeforePPP", "D"), test(contentRightJustified, "PPPbeforePPPEDstillbeforePPPDafter"))
  //
  //    // Testing Right justification, padding on left removed only w/ escaped escaping
  //    assertEquals(Some("beforeEE", "D"), test(contentRightJustified, "beforeEEDstillbeforeDafter"))
  //    assertEquals(Some("beforeEE", "D"), test(contentRightJustified, "PPPbeforeEEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEE", "D"), test(contentRightJustified, "beforePPPEEDstillbeforePPPDafter"))
  //    assertEquals(Some("beforePPPEE", "D"), test(contentRightJustified, "PPPbeforePPPEEDstillbeforePPPDafter"))
  //
  //    // Testing Escaped Padding
  //    assertEquals(Some("beforeEEPPP", "D"), test(contentRightJustified, "PPPbeforeEEPPPDstillbeforeDafter"))
  //    assertEquals(Some("beforeEPPP", "D"), test(contentRightJustified, "PPPbeforeEPPPDstillbeforeDafter"))
  //    assertEquals(Some("PPPbeforeEE", "D"), test(contentLeftJustified, "PPPbeforeEEPPPDstillbeforeDafter"))
  //    assertEquals(Some("PPPbeforeEP", "D"), test(contentLeftJustified, "PPPbeforeEPPPDstillbeforeDafter"))
  //    assertEquals(Some("beforeEE", "D"), test(contentCenterJustified, "PPPbeforeEEPPPDstillbeforeDafter"))
  //    assertEquals(Some("beforeEP", "D"), test(contentCenterJustified, "PPPbeforeEPPPDstillbeforeDafter"))
  //    assertEquals(Some("EEPPPbeforeEE", "D"), test(contentCenterJustified, "EEPPPbeforeEEPPPDstillbeforeDafter"))
  //    assertEquals(Some("EPPPbeforeEP", "D"), test(contentCenterJustified, "EPPPbeforeEPPPDstillbeforeDafter"))
  //
  //    //    val str =
  //    //        """(?<!%1$s)((?:%1$s%1$s)*)(?:%3$s*)""" + // trim unescaped pad char. aka right or center justified.
  //    //          """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
  //    //          """(?:""" +
  //    //          """(?<!%1$s)((?:%1$s%1$s)*)(?:%3$s*)(?<!%1$s)(%2$s)""" + // trim unescaped pad char. aka left or center justified. Then delim. (and if pad is size zero, then delimiter must be unescaped.)
  //    //          """|""" + // OR
  //    //          """(?<!%1$s)((?:%1$s%1$s)*)(%2$s)""" + // unescaped delimiter [delim] which is delim preceded by NOT an odd number of escapes.
  //    //          """)""" +
  //    //          """(.*)""" // trailing stuff [after]
  //    //      val ContentPattern = str.format(escape, delimiter, padChar).r
  //  }

  //  @Test def testParserEscapeSchemes_DiffEscapesWithPadding() = {
  //    var testNum = 0
  //    def test(theParser: Parser[(String, String)], theInput: String) = {
  //      testNum = testNum + 1
  //      val result = this.parse(this.log(theParser)("testParserEscapeSchemes_DiffEscapesWithPadding." + testNum), theInput)
  //      if (result.isEmpty) { None }
  //      else {
  //        val res = result.get
  //        Some(res._1, res._2)
  //      }
  //    }
  //
  //    val escapeEscape = """S"""
  //    val escape = """E"""
  //    val delim = """D"""
  //    val padChar = """P"""
  //
  //    val rPadChar = """(%s*)"""
  //    val pPadChar: Parser[String] = rPadChar.format(padChar).r
  //
  //    val rBefore = """(.*?)(?=(?:%4$s+(?<!(?<!%1$s)%2$s)%3$s)|(?:(?<!(?<!%1$s)%2$s)%3$s))"""
  //    val rBeforeIgnoreTrailingPadding = """(.*?)(?=(?:(?<!(?<!%1$s)%2$s)%3$s))"""
  //    val pBefore: Parser[String] = rBefore.format(escapeEscape, escape, delim, padChar).r
  //    val pBeforeIgnoreTrailingPadding: Parser[String] = rBeforeIgnoreTrailingPadding.format(escapeEscape, escape, delim).r
  //
  //    val pDelims: Parser[String] = """D""".r ||| """DD""".r
  //    val pEscape: Parser[String] = """E""".r
  //    val pEscapeEscape: Parser[String] = """S""".r
  //    val pEscapedEscape = pEscapeEscape ~ pEscape
  //    val pEscapedDelims = not(pEscapedEscape) ~> (pEscape ~> pDelims)
  //    val pUnescapedDelims = ((pEscapeEscape ~ pEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | (pEscapeEscape ~> pDelims) | pDelims
  //
  //    val content = (pBeforeIgnoreTrailingPadding ~ pUnescapedDelims) ^^ { c => (c._1 -> c._2) }
  //    val contentCenterJustified = (((pPadChar ~> pBefore) <~ pPadChar) ~ pUnescapedDelims) ^^ { c => (c._1 -> c._2) }
  //    val contentLeftJustified = ((pBefore <~ pPadChar) ~ pUnescapedDelims) ^^ { c => (c._1 -> c._2) }
  //    val contentRightJustified = ((pPadChar ~> pBeforeIgnoreTrailingPadding) ~ pUnescapedDelims) ^^ { c => (c._1 -> c._2) }
  //
  //    // Testing no justification, padding is treated as part of content
  //    assertEquals(Some("before", "D"), test(content, "beforeDafter"))
  //    assertEquals(Some("PPPbefore", "D"), test(content, "PPPbeforeDafter"))
  //    assertEquals(Some("beforePPP", "D"), test(content, "beforePPPDafter"))
  //    assertEquals(Some("PPPbeforePPP", "D"), test(content, "PPPbeforePPPDafter"))
  //
  //    // Testing no justification, padding is treated as part of content w/ escaping
  //    assertEquals(Some("abcEEEEEdef", "D"), test(content, "abcEEEEEdefDghi"))
  //    assertEquals(Some("abcSESESEdef", "D"), test(content, "abcSESESEdefDghi"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(content, "beforeEDstillbeforeDafter"))
  //    assertEquals(Some("PPPbeforeEDstillbefore", "D"), test(content, "PPPbeforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEDstillbeforePPP", "D"), test(content, "beforePPPEDstillbeforePPPDafter"))
  //    assertEquals(Some("PPPbeforePPPEDstillbeforePPP", "D"), test(content, "PPPbeforePPPEDstillbeforePPPDafter"))
  //
  //    // Testing no justification, padding is treated as part of content w/ escaped escaping
  //    assertEquals(Some("beforeSE", "D"), test(content, "beforeSEDstillbeforeDafter"))
  //    assertEquals(Some("PPPbeforeSE", "D"), test(content, "PPPbeforeSEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPSE", "D"), test(content, "beforePPPSEDstillbeforePPPDafter"))
  //    assertEquals(Some("PPPbeforePPPSE", "D"), test(content, "PPPbeforePPPSEDstillbeforePPPDafter"))
  //
  //    // Testing Center justification, padding is not part of content
  //    assertEquals(Some("before", "D"), test(contentCenterJustified, "beforeDafter"))
  //    assertEquals(Some("before", "D"), test(contentCenterJustified, "PPPbeforeDafter"))
  //    assertEquals(Some("before", "D"), test(contentCenterJustified, "beforePPPDafter"))
  //    assertEquals(Some("before", "D"), test(contentCenterJustified, "PPPbeforePPPDafter"))
  //
  //    // Testing Center justification, padding part of content w/ escaping
  //    assertEquals(Some("abcEEEEEdef", "D"), test(contentCenterJustified, "abcEEEEEdefDghi"))
  //    assertEquals(Some("abcSESESEdef", "D"), test(contentCenterJustified, "abcSESESEdefDghi"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(contentCenterJustified, "beforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(contentCenterJustified, "PPPbeforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEDstillbefore", "D"), test(contentCenterJustified, "beforePPPEDstillbeforePPPDafter"))
  //    assertEquals(Some("beforePPPEDstillbefore", "D"), test(contentCenterJustified, "PPPbeforePPPEDstillbeforePPPDafter"))
  //
  //    // Testing Center justification, padding is part of content w/ escaped escaping
  //    assertEquals(Some("beforeSE", "D"), test(contentCenterJustified, "beforeSEDstillbeforeDafter"))
  //    assertEquals(Some("beforeSE", "D"), test(contentCenterJustified, "PPPbeforeSEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPSE", "D"), test(contentCenterJustified, "beforePPPSEDstillbeforePPPDafter"))
  //    assertEquals(Some("beforePPPSE", "D"), test(contentCenterJustified, "PPPbeforePPPSEDstillbeforePPPDafter"))
  //
  //    // Testing Left justification, padding on right removed only
  //    assertEquals(Some("before", "D"), test(contentLeftJustified, "beforeDafter"))
  //    assertEquals(Some("PPPbefore", "D"), test(contentLeftJustified, "PPPbeforeDafter"))
  //    assertEquals(Some("before", "D"), test(contentLeftJustified, "beforePPPDafter"))
  //    assertEquals(Some("PPPbefore", "D"), test(contentLeftJustified, "PPPbeforePPPDafter"))
  //
  //    // Testing Left justification, padding on right removed only w/ escaping
  //    assertEquals(Some("abcEEEEEdef", "D"), test(contentLeftJustified, "abcEEEEEdefDghi"))
  //    assertEquals(Some("abcSESESEdef", "D"), test(contentLeftJustified, "abcSESESEdefDghi"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(contentLeftJustified, "beforeEDstillbeforeDafter"))
  //    assertEquals(Some("PPPbeforeEDstillbefore", "D"), test(contentLeftJustified, "PPPbeforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEDstillbefore", "D"), test(contentLeftJustified, "beforePPPEDstillbeforePPPDafter"))
  //    assertEquals(Some("PPPbeforePPPEDstillbefore", "D"), test(contentLeftJustified, "PPPbeforePPPEDstillbeforePPPDafter"))
  //
  //    // Testing Left justification, padding on right removed only w/ escaped escaping
  //    assertEquals(Some("beforeSE", "D"), test(contentLeftJustified, "beforeSEDstillbeforeDafter"))
  //    assertEquals(Some("PPPbeforeSE", "D"), test(contentLeftJustified, "PPPbeforeSEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPSE", "D"), test(contentLeftJustified, "beforePPPSEDstillbeforePPPDafter"))
  //    assertEquals(Some("PPPbeforePPPSE", "D"), test(contentLeftJustified, "PPPbeforePPPSEDstillbeforePPPDafter"))
  //
  //    // Testing Right justification, padding on left removed only
  //    assertEquals(Some("before", "D"), test(contentRightJustified, "beforeDafter"))
  //    assertEquals(Some("before", "D"), test(contentRightJustified, "PPPbeforeDafter"))
  //    assertEquals(Some("beforePPP", "D"), test(contentRightJustified, "beforePPPDafter"))
  //    assertEquals(Some("beforePPP", "D"), test(contentRightJustified, "PPPbeforePPPDafter"))
  //
  //    // Testing Right justification, padding on left removed only w/ escaping
  //    assertEquals(Some("abcEEEEEdef", "D"), test(contentRightJustified, "abcEEEEEdefDghi"))
  //    assertEquals(Some("abcSESESEdef", "D"), test(contentRightJustified, "abcSESESEdefDghi"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(contentRightJustified, "beforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforeEDstillbefore", "D"), test(contentRightJustified, "PPPbeforeEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPEDstillbeforePPP", "D"), test(contentRightJustified, "beforePPPEDstillbeforePPPDafter"))
  //    assertEquals(Some("beforePPPEDstillbeforePPP", "D"), test(contentRightJustified, "PPPbeforePPPEDstillbeforePPPDafter"))
  //
  //    // Testing Right justification, padding on left removed only w/ escaped escaping
  //    assertEquals(Some("beforeSE", "D"), test(contentRightJustified, "beforeSEDstillbeforeDafter"))
  //    assertEquals(Some("beforeSE", "D"), test(contentRightJustified, "PPPbeforeSEDstillbeforeDafter"))
  //    assertEquals(Some("beforePPPSE", "D"), test(contentRightJustified, "beforePPPSEDstillbeforePPPDafter"))
  //    assertEquals(Some("beforePPPSE", "D"), test(contentRightJustified, "PPPbeforePPPSEDstillbeforePPPDafter"))
  //  }

  //  @Test def testParserEscapeSchemes() = {
  //    //                escEsc     esc       delim        padChar
  //    //val test = tester("""S""", """E""", """\_*D\_*""", """P""") // put any regex in there for the delimiter
  //    val escapeEscape = """S"""
  //    val escape = """E"""
  //    val delim = """D"""
  //    val padChar = """P"""
  //
  //    val rPadChar = """(%s*)"""
  //    val pPadChar: Parser[String] = rPadChar.format(padChar).r
  //    // (?:(?:(?<!(?:(?<!(?:S))E))(?:D))?)
  //    //
  //    //val rBefore = """(.*?)(?:%4$s*)(?:(?<!(?:(?<!%1$s)%2$s))(%3$s))"""
  //    //val rBefore = """(.*?)"""
  //    //val rBefore = """(.*?)(?:%4$s*)(?:(?:(?<!(?:(?<!(?:%1$s))%2$s))(?:%3$s)(?:.*))?)"""
  //    //val pBefore: Parser[String] = rBefore.format(escapeEscape, escape, delim, padChar).r
  //    //val pBefore = """(.*?)(?=P+)""".r
  //    //val pBefore = """(.*?)(?=P+)""".r
  //    //val pBefore = """(.*?)(?=(?:P+(?<!(?<!S)E)D)|(?:(?<!(?<!S)E)D))""".r
  //
  //    // Before:
  //    // (.*?)(?=(?:P+(?<!(?<!S)E)D)|(?:(?<!(?<!S)E)D))
  //    // Give me everything until Padding followed by an unescaped delimiter OR
  //    // Give me everything until an unescaped delimiter
  //    val rBefore = """(.*?)(?=(?:%4$s+(?<!(?<!%1$s)%2$s)%3$s)|(?:(?<!(?<!%1$s)%2$s)%3$s))"""
  //    val pBefore: Parser[String] = rBefore.format(escapeEscape, escape, delim, padChar).r
  //    val rUnescapedDelim = """(?<!(?:(?<!%1$s)%2$s))(%3$s)"""
  //    val pUnescapedDelim: Parser[String] = rUnescapedDelim.format(escapeEscape, escape, delim).r
  //    val pTrailingStuff: Parser[String] = """(.*)""".r
  //
  //    // We ought to decompose the pUnescapedDelim because we care about longest match
  //    val pDelims: Parser[String] = """D""".r ||| """DD""".r
  //    val pEscape: Parser[String] = """E""".r
  //    val pEscapeEscape: Parser[String] = """S""".r
  //    val pEscapedEscape = pEscapeEscape ~ pEscape
  //    val pEscapedDelims = not(pEscapedEscape) ~> (pEscape ~> pDelims)
  //    val pUnescapedDelims = ((pEscapeEscape ~ pEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | (pEscapeEscape ~> pDelims) | pDelims
  //
  //    //val content = pPadChar ~ pBefore ~ pPadChar //~ pUnescapedDelim ~ pTrailingStuff
  //    val contentCenterJustified = ((pPadChar ~> pBefore) <~ pPadChar) ~ pUnescapedDelim
  //    val content = pBefore ~ pUnescapedDelim
  //    val contentLeftJustified = (pBefore <~ pPadChar) ~ pUnescapedDelim
  //    val contentRightJustified = (pPadChar ~> pBefore) ~ pUnescapedDelim
  //
  //    // Uses decomposed pUnescapedDelim because we care about longest match
  //    val contentCenterJustified2 = ((pPadChar ~> pBefore) <~ pPadChar) ~ pUnescapedDelims
  //
  //    //val result = this.parse(pUnescapedDelims, "SD")
  //    //val result = this.parse(pDelims, "DD") // Longest match
  //    //val result = this.parse(content, "PPPbeforePPPDafter")
  //    //    val result = this.parse(content, "PPPbeforeDafter")
  //    //    val result = this.parse(content, "PPPbeforePPP")
  //    //val result = this.parse(content, "beforeDafter")
  //    //val result = this.parse(contentCenterJustified2, "PPPbefEDorePPPDafter")
  //    val result = this.parse(contentCenterJustified2, "befEDoreDafter")
  //
  //    println(result)
  //
  //    val str =
  //      """(?:%4$s*)""" + // trim pad char. aka right or center justified.
  //        """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
  //        """(?:%4$s*)""" + // trim pad char. aka left or center justified.
  //        """(?:(?<!(?:(?<!%1$s)%2$s))(%3$s))""" + // unescaped delimiter [delim]
  //        """(.*)""" // trailing stuff [after]
  //    val ContentPattern = str.format(escapeEscape, escape, delim, padChar).r
  //
  //    //assertEquals(Some(("before", "D", "after")), test("PPPbeforePPPDafter"))
  //
  //  }

  /**
   * First let's avoid everything about whitespace and use _ (underscores)
   * instead.
   */

  def delim0 = """_*+;_*+""" // greedy whitespace consumption

  def anyUntil(delim: String) = {
    // at begining of string, anything repeated (lazy), followed by the delim
    """^((?>.*?""" + """(?=(?:""" + delim + """))))"""
    // the atomic group begining with (?> is used to reduce possibilities of backtracking
    // but may not be doing anything.
    // There's one capture group here
  }

  // def t0 = log(anyUntil(delim0).r)("before") ~! log(delim0.r)("delim") ~! log("""(.*)""".r)("after")
  def t0 = anyUntil(delim0).r ~! delim0.r ~! """(.*)""".r

  // test data. Stuff including whitespace, followed by the delimiter, followed by more stuff
  // also containing whitespace
  val rdr0 = new StringReader("b e f o r e ___;___ a f t e r")

  lazy val parsed0 = parseAll(t0, rdr0)

  @Test def testParsingDelims(): Unit = {
    skipWS = false // keep all the whitespace
    assertTrue(parsed0.successful)
    val a = parsed0.get
    // println(a)
    a match {
      // We need 'ending' here, not 'end'
      case (("b e f o r e " ~ "___;___") ~ " a f t e r") => {
        // we're ok
      }
      case _ => fail("not expected result: " + a)
    }
  }

  /**
   * First let's avoid everything about whitespace and use _ (underscores)
   * instead.
   */

  // two regex expressions with common prefix. 2nd is longer of the two
  // so first match would get the shorter match
  def delim1 = ("""_*end_*""".r ||| """_*ending_*""".r)

  def t1 = delim1 ~ "more"

  // test data has our longer delimiter string, followed by more stuff
  val rdr1 = new StringReader("___ending___more")

  lazy val parsed1 = parseAll(t1, rdr1)

  @Test def testRegexNoWSLongestMatch(): Unit = {
    skipWS =
      true // this is the default setting for scala comb. parsers, but we have no ws so it doesn't matter really.
    assertTrue(parsed1.successful)
    val a = parsed1.get
    // println(a)
    a match {
      // We need 'ending' here, not 'end'
      case ("___ending___" ~ "more") => {
        // we're ok
      }
      case _ => fail("not expected result: " + a)
    }
  }

  /**
   * Now let's put whitespace absorption into the definition of
   * our delimiters. So we'll turn off scala comb. parser whitespace skipping explicitly.
   */
  // two regex expressions with common prefix. 2nd is longer of the two
  // so first match would get the shorter match
  def endWS =
    """\s*end\s*""".r // note use \s for whiteSpace. \w is "word character" not whitespace
  def endingWS = """\s*ending\s*""".r
  // def delimWS = (log(endWS)("end") ||| log(endingWS)("ending"))
  def delimWS = (endWS ||| endingWS)

  def t2 = delimWS ~! "more"

  val rdr2 = new StringReader("   ending   more")

  lazy val parsed2 = parseAll(t2, rdr2)

  @Test def testRegexWSLongestMatch(): Unit = {
    skipWS = false // we want our delimiters to contain the whitespace.
    // (parsed2)
    assertTrue(parsed2.successful)
    val a = parsed2.get
    // println(a)
    a match {
      // We need 'ending' here, not 'end'
      case ("   ending   " ~ "more") => {
        // ok.
      }
      case _ => fail("not expected result: " + a)
    }
  }

  /*
   * Next we have tests that handle complexities of escape schemes of the
   * escape character kind and block escape kind.
   */

  /**
   * Tests a regular experssion to match a delimiter but taking
   * into account escape characters and padChar trimming.
   */
  @Test def testRegexToMatchOneDelimiterWithEscapeChars(): Unit = {

    /**
     * tester regexps and postprocessing algorithms are different
     * depending on whether you have escapeBlock or escapeCharacter type
     * escaping.
     */
    def tester(escapeEscape: String, escape: String, delim: String, padChar: String) = {
      Assert.usage(padChar.length == 1)
      //
      // Let S be the escape escape character
      // Let E be the escape character
      // Let D be the delimiter
      //
      // (.*?)(?=X) means anything (but looking ahead for X) lazy, not greedy.
      // where X is an unescaped delimiter so
      // X = (?<!Y)D
      // Y is an escape, which itself cannot be escaped
      // Y = (?<!S)E where E is the escape character, preceded by anything but S, the escape
      // escape character.
      //
      // Putting that all together
      //  (.*)(?=(?<!(?<!S)E)D)
      // matches data followed by an unescaped delimiter D.
      //   Note that looking ahead doesn't consume anything.
      //   We're left before the lookahead part of the match.
      // followed by delimiter,
      // followed by anything leftover
      val str =
        """(?:%4$s*)""" + // trim pad char. aka right or center justified.
          """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
          """(?:%4$s*)""" + // trim pad char. aka left or center justified.
          """(?:(?<!(?:(?<!%1$s)%2$s))(%3$s))""" + // unescaped delimiter [delim]
          """(.*)""" // trailing stuff [after]
      val ContentPattern = str.format(escapeEscape, escape, delim, padChar).r
      // println("Pattern = " + ContentPattern.pattern)

      // used to cleanup active escape characters
      val EDSplit = """(.*)%1$s(%2$s)(.*)""".format(escape, delim).r

      /**
       * Really really we want to use the java APIs that let us construct a matcher from
       * a pattern (which 'compiles' the pattern), then use that over and over.
       *
       * This code is just about figuring out the right regex, and putting
       * groups inside it that let us get at what we need.
       */
      def test(x: String) = x match {
        case ContentPattern(before, delim, after) => {
          // println("'%s' parsed to b = '%s', d = '%s', a = '%s'".format(x, before, delim, after))
          val before1 = removeActiveEscapes(before)
          Some((before1, delim, after))
        }
        case z => {
          // println("no match: " + z);
          None
        }
      }

      /**
       * postprocessing to remove active escape characters
       */
      def removeActiveEscapes(str: String): String = {
        // if ends with SE (an escaped escape), then change to end with just E
        // because the S was active.
        val str1 = if (str.endsWith(escapeEscape + escape)) {
          str.slice(0, str.length() - 2) + escape
        } else str
        // if contains ED, replace with just D
        val str2 = removeActiveEscapes1(str1)
        str2
      }

      def removeActiveEscapes1(str: String): String = {
        val res = str match {
          case EDSplit(before, delim, after) => {
            val rest = removeActiveEscapes1(after)
            before + delim + rest
          }
          case _ => str
        }
        res
      }

      test _
    }

    //                escEsc     esc       delim        padChar
    val test = tester(
      """S""",
      """E""",
      """\_*D\_*""",
      """P"""
    ) // put any regex in there for the delimiter

    assertEquals(Some(("before", "D", "after")), test("PPPbeforePPPDafter"))

    // Notice how (.*?) is non-greedy matching. SO we don't get "before_", we get "before"
    assertEquals(Some(("before", "_D_", "after")), test("PPPbefore_D_after"))

    assertEquals(Some(("beforeE", "__D_", "after")), test("PPPbeforeSE__D_after"))

    assertEquals(
      Some(("beforeDstillBefore", "D", "after")),
      test("PPPbeforeEDstillBeforePPPDafter")
    )

    // In the test below. Note that S does NOT escape D.
    assertEquals(Some(("beforeS", "D", "after")), test("PPPbeforeSDafter"))

    // In the test below note that SE is just data because D doesn't follow E
    assertEquals(
      Some(("beforeSEstillBefore", "D", "after")),
      test("PPPbeforeSEstillBeforePPPDafter")
    )

    // to keep out of slash hell, just pretend RN is a \r\n, and N is a \n, and _ is a whitespace.
    //                 escEsc     esc          delim           padChar
    val test2 = tester("""S""", """E""", """(?>RN|\_*N\_*)""", """P""")
    // in the above note careful use of ?> in delim, which is disjunction with no backtracking.

    assertEquals(
      Some(("before", "RN", "after")),
      test2("PPPbeforePPPRNafter")
    ) // works because (.*?) is lazy not greedy

    assertEquals(
      Some(("beforeRNstillBefore", "__N_", "after")),
      test2("PPPbeforeRENstillBeforePPP__N_after")
    )

  }

  /**
   * Tests a regular experssion to match a delimiter but taking
   * into account escape characters and padChar trimming.
   *
   * Special case for when EEC and EC are same.
   */
  @Test def testRegexToMatchOneDelimiterWithEscapeCharsWhenEECAndECAreSame(): Unit = {

    /**
     * tester regexps and postprocessing algorithms are different
     * depending on whether you have escapeBlock or escapeCharacter type
     * escaping.
     */
    def tester(escape: String, delimiter: String, padChar: String) = {
      Assert.usage(padChar.length == 1)
      //
      // Let E be the escape character
      // Let D be the delimiter
      // Let P be the pad char
      //
      // (?<!E)((?:EE)*)P matches unescaped pad characters

      val str =
        """(?<!%1$s)((?:%1$s%1$s)*)(?:%3$s*)""" + // trim unescaped pad char. aka right or center justified.
          """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
          """(?:""" +
          """(?<!%1$s)((?:%1$s%1$s)*)(?:%3$s*)(?<!%1$s)(%2$s)""" + // trim unescaped pad char. aka left or center justified. Then delim. (and if pad is size zero, then delimiter must be unescaped.)
          """|""" + // OR
          """(?<!%1$s)((?:%1$s%1$s)*)(%2$s)""" + // unescaped delimiter [delim] which is delim preceded by NOT an odd number of escapes.
          """)""" +
          """(.*)""" // trailing stuff [after]
      val ContentPattern = str.format(escape, delimiter, padChar).r
      // println("Pattern = " + ContentPattern.pattern)

      // used to cleanup escape characters
      val ERSplit = """(.*?)%1$s(.)(.*)""".format(escape).r

      /**
       * Really really we want to use the java APIs that let us construct a matcher from
       * a pattern (which 'compiles' the pattern), then use that over and over.
       *
       * This code is just about figuring out the right regex, and putting
       * groups inside it that let us get at what we need.
       */
      def test(x: String) = x match {
        case ContentPattern(ee1s, before, ee2s, delimAfterPad, ee3s, delimAfterEEs, after) => {
          //          println("'%s' parsed to ee1s = '%s', b = '%s', ee2s = '%s', delimAfterPad = '%s', ee3s = '%s', delimAfterEEs = '%s', a = '%s'".format(
          //            x, ee1s, before, ee2s, delimAfterPad, ee3s, delimAfterEEs, after))
          val before1 = removeActiveEscapes(
            (if (ee1s == null) "" else ee1s) +
              before +
              (if (ee2s == null) "" else ee2s) +
              (if (ee3s == null) "" else ee3s)
          )
          val delim1 = if (delimAfterPad == null) delimAfterEEs else delimAfterPad
          Some((before1, delim1, after))
        }
        case z => {
          // println("no match: " + z);
          None
        }
      }

      /**
       * postprocessing to remove active escape characters
       */
      // TBD: Are we supposed to remove ALL escape characters?
      // DFDL spec seems to say so. Only escape-escaped escape characters are preserved.
      //
      def removeActiveEscapes(str: String): String = {
        // System.err.println(str)
        // if contains ER, replace with just R
        val str2 = removeActiveEscapes1(str)
        str2
      }

      def removeActiveEscapes1(str: String): String = {
        val res = str match {
          case ERSplit(before, delim, after) => {
            val rest = removeActiveEscapes1(after)
            before + delim + rest
          }
          case _ => str
        }
        res
      }

      test _
    }
    val test3 = tester("""E""", """D""", """P""")
    val test4 = tester("""/""", """;""", """P""")

    assertEquals(Some("before", "D", "after"), test3("beforeDafter"))

    assertEquals(Some("beforeDstillBefore", "D", "after"), test3("beforeEDstillBeforeDafter"))

    assertEquals(Some("beforeE", "D", "after"), test3("beforeEEDafter"))

    assertEquals(
      Some("beforeEDstillBefore", "D", "after"),
      test3("beforeEEEDstillBeforeDafter")
    )

    assertEquals(Some("beforeEE", "D", "after"), test3("beforeEEEEDafter"))

    assertEquals(Some("beforeEE", "D", "after"), test3("PPPbeforeEEEEPPPDafter"))

    // We can escape a pad character (thereby making it "non pad")
    assertEquals(Some("PbeforeP", "D", "after"), test3("PPEPbeforeEPPPDafter"))

    assertEquals(Some("PPPbeforePPP", "D", "after"), test3("PEPPEPbeforeEPPEPDafter"))

    assertEquals(Some("before1EDbefore2", "D", "after"), test3("before1EEEDbefore2Dafter"))
    assertEquals(Some("..../;....", ";", "after"), test4("....///;....;after"))
  }

  /**
   * We assume here that a blockStart must be the very first thing in the string, and a blockEnd the
   * very last before the delimiter. For example, if , is delimiter, and [ and ] are block start and end:
   * @example {{{
   * [aaa,bbb],...
   * }}}is allowed but not
   * @example {{{ aaa[,]bbb,...
   * }}}
   *
   * We also assume that you can escape the block start or block end.
   * Or, if you choose not to use the block escapes, you can still escape the delimiter.
   *
   * Also illustrates how one would add padChar absorbing into the mix on the left, right, or both
   *
   */
  @Test def testRegexToMatchOneDelimiterWithBlockEscapesAndPaddingCharacters(): Unit = {

    def tester(
      bStart: String,
      bEnd: String,
      escapeEscape: String,
      escape: String,
      delim: String,
      padChar: String
    ) = {
      Assert.usage(padChar.length == 1)
      val str = """(?>""" +
        // First alternative is if the start is an unescaped Block start. In that case you must
        // have an unescaped block end followed directly by an unescaped delimiter as the termination.
        """(?:%6$s*)""" + // trim padChar off on the left. Aka right or center justified.
        """(?<!(?<!%1$s)%2$s)%4$s""" + // unescaped block start
        """(.*)""" + // captured content (before)
        """(?<!(?<!%1$s)%2$s)%5$s""" + // unescaped block end
        """(?:%6$s*)""" + // trim padChar off on the right. aka left or center justified.
        """(?<!(?<!%1$s)%2$s)(%3$s)""" + // unescaped delimiter (delim)
        """(.*)""" + // trailing stuff (after)
        """|""" +
        // Second alternative is if the start is NOT a block start, in which case we are looking
        // for an unescaped delimiter at the end.
        """(?:%6$s*)""" + // trim padChar off. Aka right or center justified.
        """(?<!%4$s)""" + // not a block start
        """(.*?)""" + // lazy content so it won't absorb the padChars (before2)
        """(?:%6$s*)""" + // trim padChar off. aka left or center justified.
        """(?<!(?<!%1$s)%2$s)(%3$s)""" + // unescaped delimiter (delim2)
        """(.*)""" + // trailing stuff (after2)
        """)"""

      val ContentPattern = str.format(escapeEscape, escape, delim, bStart, bEnd, padChar).r
      // println("Pattern = " + ContentPattern.pattern)

      // used to cleanup active escape characters
      val EDSplit = """(.*)%1$s(%2$s)(.*)""".format(escape, delim).r
      val ENSplit = """(.*)%1$s(%2$s)(.*)""".format(escape, bEnd).r

      def test(x: String) = x match {
        case ContentPattern(before, delim, after, null, null, null) => {
          val before1 = removeActiveEscapesBlocked(before)
          //          println("'%s' parsed to b = '%s', d = '%s', a = '%s'".
          //            format(x, before1, delim, after))
          Some((before1, delim, after))
        }
        case ContentPattern(null, null, null, before, delim, after) => {
          val before1 = removeActiveEscapesUnblocked(before)
          //          println("'%s' parsed to b = '%s', d = '%s', a = '%s'".
          //            format(x, before1, delim, after))
          Some((before1, delim, after))
        }
        case z => {
          // println("no match: " + z);
          None
        }
      }

      // postprocessing to remove active escape characters
      def removeActiveEscapesUnblocked(str: String): String = {
        if (str == null) return str
        // if ends with SE (an escaped escape), then change to end with just E
        // because the S was active.
        val str1 = if (str.endsWith(escapeEscape + escape)) {
          str.slice(0, str.length() - 2) + escape
        } else str
        val str2 = if (str.startsWith(escape + bStart)) {
          str1.slice(1, str1.length())
        } else str1
        // if contains ED, replace with just D
        val str3 = removeActiveEscapesUnblocked1(str2)
        str3
      }

      def removeActiveEscapesUnblocked1(str: String): String = {
        val res = str match {
          case EDSplit(before, delim, after) => {
            val rest = removeActiveEscapesUnblocked1(after)
            before + delim + rest
          }
          case _ => str
        }
        res
      }

      // postprocessing to remove active escape characters
      def removeActiveEscapesBlocked(str: String): String = {
        if (str == null) return str
        // if ends with SE (an escaped escape), then change to end with just E
        // because the S was active.
        val str1 = if (str.endsWith(escapeEscape + escape)) {
          str.slice(0, str.length() - 2) + escape
        } else str
        // if contains EN, replace with just N
        val str2 = removeActiveEscapesBlocked1(str1)
        str2
      }

      def removeActiveEscapesBlocked1(str: String): String = {
        val res = str match {
          case ENSplit(before, delim, after) => {
            val rest = removeActiveEscapesBlocked1(after)
            before + delim + rest
          }
          case _ => str
        }
        res
      }

      test _
    }

    //                blkStart blkEnd   escEsc   esc      delim    padChar
    val test = tester("""T""", """N""", """S""", """E""", """D""", """P""")

    // This particular regex lets you escape either the blockstart or blockend, or the delimiter

    // no blockstart/end
    assertEquals(Some(("before", "D", "after")), test("PPPbeforePPPDafter"))

    // no blockstart/end, but escape the delimiter
    assertEquals(
      Some(("beforeDstillBefore", "D", "after")),
      test("PPPbeforeEDstillBeforePPPDafter")
    )

    // with blockstart/end
    assertEquals(
      Some(("beforeDstillBefore", "D", "after")),
      test("PPPTbeforeDstillBeforeNPPPDafter")
    )

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(
      Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")),
      test("PPPTbeforeEDstillBeforeSEDstillBeforeNPPPDafter")
    )
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(
      Some(("beforeDstillBeforeNstillBefore", "D", "after")),
      test("PPPTbeforeDstillBeforeENstillBeforeNPPPDafter")
    )

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeE", "D", "after")),
      test("PPPTbeforeDstillBeforeTstillBeforeSENPPPDafter")
    )

    // with blockstart, but escape it so it's not really a block.
    assertEquals(
      Some(("Tbefore", "D", "afterNstillafter")),
      test("PPPETbeforePPPDafterNstillafter")
    )
  }

}
