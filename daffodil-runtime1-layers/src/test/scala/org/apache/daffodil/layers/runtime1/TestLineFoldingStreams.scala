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

import org.junit.Assert._
import org.junit.Test

class TestLineFoldingStreams {

  @Test def testLineFoldedOutputStream_iCal1() = {
    val bba = new ByteArrayOutputStream()
    val lfs = new LineFoldedOutputStream(LineFoldMode.iCalendar, bba)
    val baos = new ByteArrayOutputStream()
    val dataString = "Embedded\r\nIsolated NL\nwith isolated CR\rwith other" +
      " very long lines of things that are too long for the usual" +
      " line length which is 78-ish."
    baos.write(dataString.getBytes("utf-8"))
    lfs.write(baos.toByteArray())
    lfs.close()
    val resultString = new String(bba.toByteArray())
    val expected = "Embedded\r\nIsolated NL\r\nwith isolated CR\r\nwith other" +
      " very long lines of things that are too long for the usual" +
      " line l\r\n ength which is 78-ish."
    assertEquals(expected, resultString)
  }

  /**
   * Boundary condition where the character that is 1 too big for the line is
   * a CR.
   */
  @Test def testLineFoldedOutputStream_iCal2() = {
    val bba = new ByteArrayOutputStream()
    val lfs = new LineFoldedOutputStream(LineFoldMode.iCalendar, bba)
    val baos = new ByteArrayOutputStream()
    val dataString = "With other" +
      " very long lines of things that are too long for the usual" +
      " line l\r\nength which is 78-ish."
    baos.write(dataString.getBytes("utf-8"))
    lfs.write(baos.toByteArray())
    lfs.close()
    val resultString = new String(bba.toByteArray())
    val expected = "With other" +
      " very long lines of things that are too long for the usual" +
      " line l\r\n \r\nength which is 78-ish."
    assertEquals(expected, resultString)
  }

  /**
   * Boundary condition where the character that is 1 too big for the line is
   * a LF of CRLF.
   */
  @Test def testLineFoldedOutputStream_iCal3() = {
    val bba = new ByteArrayOutputStream()
    val lfs = new LineFoldedOutputStream(LineFoldMode.iCalendar, bba)
    val baos = new ByteArrayOutputStream()
    val dataString = "With other" +
      " very long lines of things that are too long for the usual" +
      " line \r\nlength which is 78-ish."
    baos.write(dataString.getBytes("utf-8"))
    lfs.write(baos.toByteArray())
    lfs.close()
    val resultString = new String(bba.toByteArray())
    val expected = "With other" +
      " very long lines of things that are too long for the usual" +
      " line \r\r\n \nlength which is 78-ish."
    assertEquals(expected, resultString)
  }

  /**
   * Tests that folding backs up to the prior space/tab and inserts the CRLF
   * before that space/tab. Also that isolated \r or \n are converted to CRLF,
   * and existing CRLF are preserved.
   */
  @Test def testLineFoldedOutputStream_IMF1() = {
    val bba = new ByteArrayOutputStream()
    val lfs = new LineFoldedOutputStream(LineFoldMode.IMF, bba)
    val baos = new ByteArrayOutputStream()
    val dataString = "Embedded\r\nIsolated NL\nwith isolated CR\rwith other" +
      " very long lines of things that are too long for the usual" +
      " line length which is 78-ish."

    baos.write(dataString.getBytes("utf-8"))
    lfs.write(baos.toByteArray())
    lfs.close()
    val resultString = new String(bba.toByteArray())
    val expected = "Embedded\r\nIsolated NL\r\nwith isolated CR\r\nwith other " +
      "very long lines of things that are too long for the usual " +
      "line\r\n length which is 78-ish."
    assertEquals(expected, resultString)
  }

  /**
   * Insures lines without tabs/spaces aren't folded, though isolated \r and \n
   * are converted to CRLFs. There are no spaces in this data. All replaced by "_"
   * (underscore), so by IMF folding rules, you can't fold the long part of this data.
   */
  @Test def testLineFoldedOutputStream_IMF2() = {
    val bba = new ByteArrayOutputStream()
    val lfs = new LineFoldedOutputStream(LineFoldMode.IMF, bba)
    val baos = new ByteArrayOutputStream()
    val dataString = "Embedded\r\nIsolated NL\nwith_isolated_CR\rwith_other" +
      "_very_long_lines_of_things_that_are_too_long_for_the_usual" +
      "_line_length_which_is_78-ish."

    /**/
    val ruler =
      "with_other_very_long_lines_of_things_that_are_too_long_for_the_usual_line_length_which_is_78-ish."
    //// val roolr = "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567"
    //// val rool2 = "         1         2         3         4         5         6         7         8         9       "

    baos.write(dataString.getBytes("utf-8"))
    lfs.write(baos.toByteArray())
    lfs.close()
    val resultString = new String(bba.toByteArray())
    val expected = "Embedded\r\nIsolated NL\r\nwith_isolated_CR\r\n" + ruler
    assertEquals(expected, resultString)
  }

  /**
   * CRLF on the boundary where it would fold, prevents folding.
   */
  @Test def testLineFoldedOutputStream_IMF3() = {
    val bba = new ByteArrayOutputStream()
    val lfs = new LineFoldedOutputStream(LineFoldMode.IMF, bba)
    val baos = new ByteArrayOutputStream()
    val dataString = "With_other_very_long_lines_of_things_that_are_too_long_for_the_usual" +
      "_line\r\n length_which_is_78-ish."

    /**/
    val ruler =
      "With_other_very_long_lines_of_things_that_are_too_long_for_the_usual_line\r\n length_which_is_78-ish."
    //// val roolr = "12345678901234567890123456789012345678901234567890123456789012345678901234 5 6789012345678901234567"
    //// val rool2 = "         1         2         3         4         5         6         7           8         9       "

    baos.write(dataString.getBytes("utf-8"))
    lfs.write(baos.toByteArray())
    lfs.close()
    val resultString = new String(bba.toByteArray())
    val expected = ruler
    assertEquals(expected, resultString)
  }

  /**
   * CRLF just past the boundary where it would fold, prevents folding.
   */
  @Test def testLineFoldedOutputStream_IMF4() = {
    val bba = new ByteArrayOutputStream()
    val lfs = new LineFoldedOutputStream(LineFoldMode.IMF, bba)
    val baos = new ByteArrayOutputStream()
    val dataString = "With_other_very_long_lines_of_things_that_are_too_long_for_the_usual" +
      "_line \r\nlength_which_is_78-ish."

    /**/
    val ruler =
      "With_other_very_long_lines_of_things_that_are_too_long_for_the_usual_line \r\nlength_which_is_78-ish."
    //// val roolr = "123456789012345678901234567890123456789012345678901234567890123456789012345 6 789012345678901234567"
    //// val rool2 = "         1         2         3         4         5         6         7           8         9       "

    baos.write(dataString.getBytes("utf-8"))
    lfs.write(baos.toByteArray())
    lfs.close()
    val resultString = new String(bba.toByteArray())
    val expected = ruler
    assertEquals(expected, resultString)
  }

  @Test def testLineFoldedInputStreamTab1() = {
    val dataString =
      "Here's data containing the line ending we care about\r\n\t followed by other text."
    val bba = new ByteArrayInputStream(dataString.getBytes("utf-8"))
    val lfs = new LineFoldedInputStream(LineFoldMode.iCalendar, bba)
    val baos = new ByteArrayOutputStream()
    var c: Int = -1
    while ({
      c = lfs.read()
      c != -1
    }) {
      baos.write(c)
    }
    baos.close()
    val resultString = new String(baos.toByteArray())
    val expected =
      "Here's data containing the line ending we care about followed by other text."
    assertEquals(expected, resultString)
  }

  @Test def testLineFoldedInputStreamIMFSpace1() = {
    val dataString =
      "Here's data containing the line ending we care about\r\n followed by other text."
    val bba = new ByteArrayInputStream(dataString.getBytes("utf-8"))
    val lfs = new LineFoldedInputStream(LineFoldMode.IMF, bba)
    val baos = new ByteArrayOutputStream()
    var c: Int = -1
    while ({
      c = lfs.read()
      c != -1
    }) {
      baos.write(c)
    }
    baos.close()
    val resultString = new String(baos.toByteArray())
    val expected =
      "Here's data containing the line ending we care about followed by other text."
    assertEquals(expected, resultString)
  }

  @Test def testLineFoldedInputStreamCRLFInteractions1() = {
    val dataString = "Foobar\r\n Quuxly"
    val bba = new ByteArrayInputStream(dataString.getBytes("utf-8"))
    val lfs = new LineFoldedInputStream(LineFoldMode.IMF, bba)
    val baos = new ByteArrayOutputStream()
    var c: Int = -1
    while ({
      c = lfs.read()
      c != -1
    }) {
      baos.write(c)
    }
    baos.close()
    val resultString = new String(baos.toByteArray())
    val expected = "Foobar Quuxly"
    assertEquals(expected, resultString)
  }

}
