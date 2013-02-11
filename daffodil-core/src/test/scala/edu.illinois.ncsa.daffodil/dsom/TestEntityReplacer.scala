package edu.illinois.ncsa.daffodil.dsom

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


import org.scalatest.junit.JUnitSuite
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import java.util.regex.Pattern
import java.util.regex.Matcher
import org.junit.Test

class TestEntityReplacer extends JUnitSuite {

  @Test def testEmptyString = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll(""), "")
  }

  @Test def testEscapeScheme = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll("Text%Text"), "Text%Text") // Works basic case
    assertEquals(e.replaceAll("Text%%Text"), "Text%Text") // Works basic case
    assertEquals(e.replaceAll("Text%%#%%Text"), "Text%#%Text") // Works multiple
    assertEquals(e.replaceAll("Text%%%#%%Text"), "Text%%#%Text") // Works multiple
    assertEquals(e.replaceAll("Text%%#65;Text"), "Text%AText") // Works multiple
  }

  @Test def testEntityReplacement = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll("Text%NUL;Text"), "Text\u0000Text") // Works basic case
    assertEquals(e.replaceAll("Text%NUL;%NULText"), "Text\u0000%NULText") // Works basic case
    assertEquals(e.replaceAll("Text%NUL;%BEL;Text"), "Text\u0000\u0007Text") // Works basic case
  }

  @Test def testHexadecimalCodePointReplacement = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll("Text%#x0000;Text"), "Text\u0000Text") // Works basic case
    assertEquals(e.replaceAll("Text%#x0000;Text%#x000D;"), "Text\u0000Text\u000D") // Works multiple hex
    assertEquals(e.replaceAll("Text%#x0000;Text%#x000D"), "Text\u0000Text%#x000D") // Works one proper, one improper
  }

  @Test def testDecimalCodePointReplacement = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll("Text%#65;Text"), "TextAText") // Works basic case
    assertEquals(e.replaceAll("Text%#0000000000065;Text"), "TextAText") // Works basic case w/ padding
    assertEquals(e.replaceAll("Text%#65;Text%#66;"), "TextATextB") // Works multiple
    assertEquals(e.replaceAll("Text%#65;Text%#000000000066;"), "TextATextB") // Works multiple w/ padding
    assertEquals(e.replaceAll("Text%#65;Text%#66"), "TextAText%#66") // Works one proper, one improper
  }

  @Test def testRawByteReplacement = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll("%#rFF;"), "%#rFF;")
    assertEquals(e.replaceAll("%#rFF;", false), "%#rFF;")
    assertEquals(e.replaceAll("%#rFF", true), "%#rFF")
    assertEquals(e.replaceAll("%#rFF;", true), "1515")
    assertEquals(e.replaceAll("%#rFF; %#rFA;", true), "1515 1510")
    assertEquals(e.replaceAll("%#rFF; %#rFA", true), "1515 %#rFA")
  }

  @Test def testAll = {
    val e = new EntityReplacer
    val testString = new StringBuilder
    testString.append("Text%%%#%%Text")
    testString.append("Text%#x0000;Text%#x000D")
    testString.append("Text%#65;Text%#66")
    testString.append("%#rFF; %#rFA")
    testString.append("Text%NUL;%BEL;Text")

    val solutionString = new StringBuilder
    solutionString.append("Text%%#%Text")
    solutionString.append("Text\u0000Text%#x000D")
    solutionString.append("TextAText%#66")
    solutionString.append("1515 %#rFA")
    solutionString.append("Text\u0000\u0007Text")

    assertEquals(e.replaceAll(testString.toString(), true), solutionString.toString())
  }

}
