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

package org.apache.daffodil.layers.runtime1

import java.io._
import java.nio.charset.StandardCharsets

import org.apache.daffodil.io.RegexLimitingInputStream

import org.apache.commons.io.IOUtils
import org.junit.Assert._
import org.junit.Test

class TestLengthLimitedLineFoldingStreams {

  val crlfRegex = "\\r\\n(?!(?:\\t|\\ ))"
  val crlf = "\r\n"

  /**
   * Has lines folded using IMF conventions.
   *
   * Notice use of the s"""...""" string interpolation. This interprets
   * the escape sequences even though triple quote doesn't.
   */
  val ipsumLorem1 = s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit,\r
\tsed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad\r
minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea\r
\tcommodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit\r
esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat\r
\tnon proident, sunt in culpa qui officia deserunt mollit anim id est laborum.""".replace(
    "\r\r\n",
    "\r\n"
  )

  val ipsumLorem1UnfoldedFirstLine =
    s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit,""" +
      s"""\tsed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad"""

  val iso8859 = StandardCharsets.ISO_8859_1

  /**
   * Shows that the regex will limit length to just the first line, but unfold will
   * then apply and unfold to a longer line.
   */
  @Test def testLineFoldedIMFOneLine() = {
    val dataString = ipsumLorem1
    val bba = new ByteArrayInputStream(dataString.getBytes("utf-8"))
    //
    // regex is CRLF not followed by a tab or space.
    //
    val rls = new RegexLimitingInputStream(bba, crlfRegex, "\r\n", iso8859)
    val lfs = new LineFoldedInputStream(LineFoldMode.IMF, rls)
    val resultString = IOUtils.toString(lfs, iso8859)
    val expected = ipsumLorem1UnfoldedFirstLine
    assertEquals(expected, resultString)
  }

  /**
   * This has lines folded with iCalendar conventions \r\n\t (uses tabs).
   * Because the \r\n\t will be removed, we have two \t consecutively so that the
   * result has one \t still.
   */
  val ipsumLorem2 = s"""Lorem ipsum dolor sit amet, consectetur adipiscing elit,\r
\t\tsed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad\r
minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea\r
\t\tcommodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit\r
esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat\r
\t\tnon proident, sunt in culpa qui officia deserunt mollit anim id est laborum.""".replace(
    "\r\r\n",
    "\r\n"
  )

  /**
   * Shows that the regex will limit length to just the first line, but unfold will
   * then apply and unfold to a longer line. (iCalendar conventions)
   */
  @Test def testLineFoldediCalendarOneLine() = {
    val dataString = ipsumLorem2
    val bba = new ByteArrayInputStream(dataString.getBytes("utf-8"))
    //
    // regex is CRLF not followed by a tab or space.
    //
    val rls = new RegexLimitingInputStream(bba, crlfRegex, "\r\n", iso8859)
    val lfs = new LineFoldedInputStream(LineFoldMode.iCalendar, rls)
    val resultString = IOUtils.toString(lfs, iso8859)
    val expected = ipsumLorem1UnfoldedFirstLine
    assertEquals(expected, resultString)
  }

  /**
   * All characters removed.
   */
  @Test def testLineFoldediCalendarNothing() = {
    val dataString = "\r\n \r\n \r\n \r\n \r\n \r\n \r\n \r\n \r\n "
    val bba = new ByteArrayInputStream(dataString.getBytes("utf-8"))
    //
    // regex is CRLF not followed by a tab or space.
    //
    val rls = new RegexLimitingInputStream(bba, crlfRegex, "\r\n", iso8859)
    val lfs = new LineFoldedInputStream(LineFoldMode.iCalendar, rls)
    val resultString = IOUtils.toString(lfs, iso8859)
    val expected = ""
    assertEquals(expected, resultString)
  }

  /**
   * All characters except spaces removed.
   */
  @Test def testLineFoldedIMFNothing() = {
    val dataString = "\r\n \r\n \r\n "
    val bba = new ByteArrayInputStream(dataString.getBytes("utf-8"))
    //
    // regex is CRLF not followed by a tab or space.
    //
    val rls = new RegexLimitingInputStream(bba, crlfRegex, "\r\n", iso8859)
    val lfs = new LineFoldedInputStream(LineFoldMode.IMF, rls)
    val resultString = IOUtils.toString(lfs, iso8859)
    val expected = "   "
    assertEquals(expected, resultString)
  }

  /**
   * Empty string doesn't trip things up. IMF
   */
  @Test def testLineFoldedEmptyIMF() = {
    val dataString = ""
    val bba = new ByteArrayInputStream(dataString.getBytes("utf-8"))
    //
    // regex is CRLF not followed by a tab or space.
    //
    val rls = new RegexLimitingInputStream(bba, crlfRegex, "\r\n", iso8859)
    val lfs = new LineFoldedInputStream(LineFoldMode.IMF, rls)
    val resultString = IOUtils.toString(lfs, iso8859)
    val expected = ""
    assertEquals(expected, resultString)
  }

  /**
   * Empty string doesn't trip things up. iCalendar
   */
  @Test def testLineFoldedEmptyICalendar() = {
    val dataString = ""
    val bba = new ByteArrayInputStream(dataString.getBytes("utf-8"))
    //
    // regex is CRLF not followed by a tab or space.
    //
    val rls = new RegexLimitingInputStream(bba, crlfRegex, "\r\n", iso8859)
    val lfs = new LineFoldedInputStream(LineFoldMode.iCalendar, rls)
    val resultString = IOUtils.toString(lfs, iso8859)
    val expected = ""
    assertEquals(expected, resultString)
  }

}
