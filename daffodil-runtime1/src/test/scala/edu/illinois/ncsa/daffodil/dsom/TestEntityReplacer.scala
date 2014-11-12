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

import junit.framework.Assert.assertEquals
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._

class TestEntityReplacer {

  @Test def testEmptyString = {
    EntityReplacer { e =>
      assertEquals(e.replaceAll(""), "")
    }
  }

  @Test def testEscapeScheme = {
    val f1 = intercept[Exception] { EntityReplacer { _.replaceAll("Text%Text") } }
    assertEquals("Invalid DFDL Entity (%Text) found in \"Text%Text\"", f1.getMessage()) // Works basic case
    assertEquals("Text%Text", EntityReplacer { _.replaceAll("Text%%Text") }) // Works basic case
    assertEquals("Text%#%Text", EntityReplacer { _.replaceAll("Text%%#%%Text") }) // Works multiple

    val f2 = intercept[Exception] { EntityReplacer { _.replaceAll("Text%%%#%%Text") } }
    assertEquals("Invalid DFDL Entity (%#) found in \"Text%%%#%%Text\"", f2.getMessage) // Works multiple
    assertEquals("Text%AText", EntityReplacer { _.replaceAll("Text%%%#65;Text") }) // Works multiple
  }

  @Test def testEntityReplacement = {
    assertEquals("Text\u0000Text", EntityReplacer { _.replaceAll("Text%NUL;Text") }) // Works basic case

    val f1 = intercept[Exception] { EntityReplacer { _.replaceAll("Text%NUL;%NULText") } }
    assertEquals("Invalid DFDL Entity (%NULText) found in \"Text%NUL;%NULText\"", f1.getMessage) // Works basic case
    assertEquals("Text\u0000\u0007Text", EntityReplacer { _.replaceAll("Text%NUL;%BEL;Text") }) // Works basic case
  }

  @Test def testHexadecimalCodePointReplacement = {
    assertEquals("Text\u0000Text", EntityReplacer { _.replaceAll("Text%#x0000;Text") }) // Works basic case
    assertEquals("Text\u0000Text\u000D", EntityReplacer { _.replaceAll("Text%#x0000;Text%#x000D;") }) // Works multiple hex

    val f1 = intercept[Exception] { EntityReplacer { _.replaceAll("Text%#x0000;Text%#x000D") } }
    assertEquals("Invalid DFDL Entity (%#x000D) found in \"Text%#x0000;Text%#x000D\"", f1.getMessage()) // Works one proper, one improper
  }

  @Test def testDecimalCodePointReplacement = {
    assertEquals("TextAText", EntityReplacer { _.replaceAll("Text%#65;Text") }) // Works basic case
    assertEquals("TextAText", EntityReplacer { _.replaceAll("Text%#0000000000065;Text") }) // Works basic case w/ padding
    assertEquals("TextATextB", EntityReplacer { _.replaceAll("Text%#65;Text%#66;") }) // Works multiple
    assertEquals("TextATextB", EntityReplacer { _.replaceAll("Text%#65;Text%#000000000066;") }) // Works multiple w/ padding

    val f1 = intercept[Exception] { EntityReplacer { _.replaceAll("Text%#65;Text%#66") } }
    assertEquals("Invalid DFDL Entity (%#66) found in \"Text%#65;Text%#66\"", f1.getMessage) // Works one proper, one improper
  }

  @Test def testRawByteReplacement = {
    assertEquals("ÿ", EntityReplacer { _.replaceAll("%#rFF;") })
    assertEquals("ÿ ú", EntityReplacer { _.replaceAll("%#rFF; %#rFA;") })

    val f1 = intercept[Exception] { EntityReplacer { _.replaceAll("%#rFF; %#rFA") } }
    assertEquals("Invalid DFDL Entity (%#rFA) found in \"%#rFF; %#rFA\"", f1.getMessage)
  }

  @Test def testInvalidDfdlEntities = {
    val f1 = intercept[Exception] { EntityReplacer { _.replaceAll("%#rTT;") } }
    assertEquals("Invalid DFDL Entity (%#rTT;) found in \"%#rTT;\"", f1.getMessage) // Verify fails: Raw Byte

    val f2 = intercept[Exception] { EntityReplacer { _.replaceAll("%#A;") } }
    assertEquals("Invalid DFDL Entity (%#A;) found in \"%#A;\"", f2.getMessage) // Verify fails: Decimal Code Point

    val f3 = intercept[Exception] { EntityReplacer { _.replaceAll("%#xTT;") } }
    assertEquals("Invalid DFDL Entity (%#xTT;) found in \"%#xTT;\"", f3.getMessage) // Verify fails: Hexadecimal Code Point

    val f4 = intercept[Exception] { EntityReplacer { _.replaceAll("%SomeInvalidName;") } }
    assertEquals("Invalid DFDL Entity (%SomeInvalidName;) found in \"%SomeInvalidName;\"", f4.getMessage) // Verify fails: Dfdl Entity
  }

  @Test def testAll = {
    EntityReplacer { e =>
      val testString = new StringBuilder
      testString.append("Text#%%Text")
      testString.append("Text%#x0000;Text%%#x000D")
      testString.append("Text%#65;Text")
      testString.append("%#rFF; %#rFA;")
      testString.append("Text%NUL;%BEL;Text")
      testString.append("Text%NL;Text")

      val solutionString = new StringBuilder
      solutionString.append("Text#%Text")
      solutionString.append("Text\u0000Text%#x000D")
      solutionString.append("TextAText")
      solutionString.append("ÿ ú")
      solutionString.append("Text\u0000\u0007Text")
      solutionString.append("Text%NL;Text")

      val resultString = e.replaceAll(testString.toString())

      assertEquals(solutionString.toString(), resultString)
    }
  }

  @Test def testMultiplePercents = {
    val testString1 = "Text1%%%%%%%%%%Text2"
    val solution1 = "Text1%%%%%Text2"
    val testString2 = "%Text1%%%%%%%%%%Text2"
    val testString3 = "Text1%%%%%%%%%%Text2%"
    val testString4 = "%Text1%%%%%%%%%%Text2%"
    val testString5 = "%%Text1%%%%%%%%%%Text2%%"
    val solution5 = "%Text1%%%%%Text2%"
    val testString6 = "%Text1%%%%%%%%%%Text2%%"
    val testString7 = "%%Text1%%%%%%%%%%Text2%"
    val testString8 = "%%Text1%%%%%%%%%%Text2"
    val solution8 = "%Text1%%%%%Text2"
    val testString9 = "Text1%%%%%%%%%%Text2%%"
    val solution9 = "Text1%%%%%Text2%"

    assertEquals(solution1, EntityReplacer { _.replaceAll(testString1, None) })

    val f1 = intercept[Exception] { EntityReplacer { _.replaceAll(testString2, None) } }
    assertEquals("Invalid DFDL Entity (%Text1) found in \"%Text1%%%%%%%%%%Text2\"", f1.getMessage)

    val f2 = intercept[Exception] { EntityReplacer { _.replaceAll(testString3, None) } }
    assertEquals("Invalid DFDL Entity (%) found in \"Text1%%%%%%%%%%Text2%\"", f2.getMessage)

    val f3 = intercept[Exception] { EntityReplacer { _.replaceAll(testString4) } }
    assertEquals("Invalid DFDL Entity (%Text1) found in \"%Text1%%%%%%%%%%Text2%\"", f3.getMessage)

    assertEquals(solution5, EntityReplacer { _.replaceAll(testString5, None) })

    val f4 = intercept[Exception] { EntityReplacer { _.replaceAll(testString6) } }
    assertEquals("Invalid DFDL Entity (%Text1) found in \"%Text1%%%%%%%%%%Text2%%\"", f4.getMessage)

    val f5 = intercept[Exception] { EntityReplacer { _.replaceAll(testString7) } }
    assertEquals("Invalid DFDL Entity (%) found in \"%%Text1%%%%%%%%%%Text2%\"", f5.getMessage)

    assertEquals(solution8, EntityReplacer { _.replaceAll(testString8, None) })
    assertEquals(solution9, EntityReplacer { _.replaceAll(testString9, None) })
  }

  @Test def testMultipleSemicolons = {
    val testString1 = "Text1;;;;;;;;;;Text2"
    val testString2 = ";Text1;;;;;;;;;;Text2"
    val testString3 = "Text1;;;;;;;;;;Text2;"
    val testString4 = ";Text1;;;;;;;;;;Text2;"
    val testString5 = ";;Text1;;;;;;;;;;Text2;;"
    val testString6 = ";Text1;;;;;;;;;;Text2;;"
    val testString7 = ";;Text1;;;;;;;;;;Text2;"
    val testString8 = ";;Text1;;;;;;;;;;Text2"
    val testString9 = "Text1;;;;;;;;;;Text2;;"
    assertEquals(testString1, EntityReplacer { _.replaceAll(testString1, None) })
    assertEquals(testString2, EntityReplacer { _.replaceAll(testString2, None) })
    assertEquals(testString3, EntityReplacer { _.replaceAll(testString3, None) })
    assertEquals(testString4, EntityReplacer { _.replaceAll(testString4, None) })
    assertEquals(testString5, EntityReplacer { _.replaceAll(testString5, None) })
    assertEquals(testString6, EntityReplacer { _.replaceAll(testString6, None) })
    assertEquals(testString7, EntityReplacer { _.replaceAll(testString7, None) })
    assertEquals(testString8, EntityReplacer { _.replaceAll(testString8, None) })
    assertEquals(testString9, EntityReplacer { _.replaceAll(testString9, None) })
  }

  @Test def testSemiColonSP = {
    val testString = ";%SP;"
    assertEquals("; ", EntityReplacer { _.replaceAll(testString) })
  }

}
