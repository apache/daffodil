/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

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
    EntityReplacer { er =>
      val f1 = intercept[Exception] { er.replaceAll("Text%Text") }
      assertEquals("Invalid DFDL Entity (%Text) found in \"Text%Text\"", f1.getMessage()) // Works basic case
      assertEquals("Text%Text", er.replaceAll("Text%%Text")) // Works basic case
      assertEquals("Text%#%Text", { er.replaceAll("Text%%#%%Text") }) // Works multiple

      val f2 = intercept[Exception] { { er.replaceAll("Text%%%#%%Text") } }
      assertEquals("Invalid DFDL Entity (%#) found in \"Text%%%#%%Text\"", f2.getMessage) // Works multiple
      assertEquals("Text%AText", { er.replaceAll("Text%%%#65;Text") }) // Works multiple
    }
  }

  @Test def testEntityReplacement = {
    EntityReplacer { er =>
      assertEquals("Text\u0000Text", { er.replaceAll("Text%NUL;Text") }) // Works basic case

      val f1 = intercept[Exception] { { er.replaceAll("Text%NUL;%NULText") } }
      assertEquals("Invalid DFDL Entity (%NULText) found in \"Text%NUL;%NULText\"", f1.getMessage) // Works basic case
      assertEquals("Text\u0000\u0007Text", { er.replaceAll("Text%NUL;%BEL;Text") }) // Works basic case
    }
  }

  @Test def testHexadecimalCodePointReplacement = {
    EntityReplacer { er =>
      assertEquals("Text\u0000Text", { er.replaceAll("Text%#x0000;Text") }) // Works basic case
      assertEquals("Text\u0000Text\u000D", { er.replaceAll("Text%#x0000;Text%#x000D;") }) // Works multiple hex

      val f1 = intercept[Exception] { { er.replaceAll("Text%#x0000;Text%#x000D") } }
      assertEquals("Invalid DFDL Entity (%#x000D) found in \"Text%#x0000;Text%#x000D\"", f1.getMessage()) // Works one proper, one improper
    }
  }

  @Test def testDecimalCodePointReplacement = {
    EntityReplacer { er =>
      assertEquals("TextAText", { er.replaceAll("Text%#65;Text") }) // Works basic case
      assertEquals("TextAText", { er.replaceAll("Text%#0000000000065;Text") }) // Works basic case w/ padding
      assertEquals("TextATextB", { er.replaceAll("Text%#65;Text%#66;") }) // Works multiple
      assertEquals("TextATextB", { er.replaceAll("Text%#65;Text%#000000000066;") }) // Works multiple w/ padding

      val f1 = intercept[Exception] { { er.replaceAll("Text%#65;Text%#66") } }
      assertEquals("Invalid DFDL Entity (%#66) found in \"Text%#65;Text%#66\"", f1.getMessage) // Works one proper, one improper
    }
  }

  @Test def testRawByteReplacement = {
    EntityReplacer { er =>
      assertEquals("ÿ", { er.replaceAll("%#rFF;") })
      assertEquals("ÿ ú", { er.replaceAll("%#rFF; %#rFA;") })

      val f1 = intercept[Exception] { { er.replaceAll("%#rFF; %#rFA") } }
      assertEquals("Invalid DFDL Entity (%#rFA) found in \"%#rFF; %#rFA\"", f1.getMessage)
    }
  }

  @Test def testInvalidDfdlEntities = {
    EntityReplacer { er =>
      val f1 = intercept[Exception] { { er.replaceAll("%#rTT;") } }
      assertEquals("Invalid DFDL Entity (%#rTT;) found in \"%#rTT;\"", f1.getMessage) // Verify fails: Raw Byte

      val f2 = intercept[Exception] { { er.replaceAll("%#A;") } }
      assertEquals("Invalid DFDL Entity (%#A;) found in \"%#A;\"", f2.getMessage) // Verify fails: Decimal Code Point

      val f3 = intercept[Exception] { { er.replaceAll("%#xTT;") } }
      assertEquals("Invalid DFDL Entity (%#xTT;) found in \"%#xTT;\"", f3.getMessage) // Verify fails: Hexadecimal Code Point

      val f4 = intercept[Exception] { { er.replaceAll("%SomeInvalidName;") } }
      assertEquals("Invalid DFDL Entity (%SomeInvalidName;) found in \"%SomeInvalidName;\"", f4.getMessage) // Verify fails: Dfdl Entity
    }
  }

  @Test def testAll = {
    EntityReplacer { er =>
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

      val resultString = er.replaceAll(testString.toString())

      assertEquals(solutionString.toString(), resultString)
    }
  }

  @Test def testMultiplePercents = {
    EntityReplacer { er =>
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

      assertEquals(solution1, { er.replaceAll(testString1, None) })

      val f1 = intercept[Exception] { { er.replaceAll(testString2, None) } }
      assertEquals("Invalid DFDL Entity (%Text1) found in \"%Text1%%%%%%%%%%Text2\"", f1.getMessage)

      val f2 = intercept[Exception] { { er.replaceAll(testString3, None) } }
      assertEquals("Invalid DFDL Entity (%) found in \"Text1%%%%%%%%%%Text2%\"", f2.getMessage)

      val f3 = intercept[Exception] { { er.replaceAll(testString4) } }
      assertEquals("Invalid DFDL Entity (%Text1) found in \"%Text1%%%%%%%%%%Text2%\"", f3.getMessage)

      assertEquals(solution5, { er.replaceAll(testString5, None) })

      val f4 = intercept[Exception] { { er.replaceAll(testString6) } }
      assertEquals("Invalid DFDL Entity (%Text1) found in \"%Text1%%%%%%%%%%Text2%%\"", f4.getMessage)

      val f5 = intercept[Exception] { { er.replaceAll(testString7) } }
      assertEquals("Invalid DFDL Entity (%) found in \"%%Text1%%%%%%%%%%Text2%\"", f5.getMessage)

      assertEquals(solution8, { er.replaceAll(testString8, None) })
      assertEquals(solution9, { er.replaceAll(testString9, None) })
    }
  }

  @Test def testMultipleSemicolons = {
    EntityReplacer { er =>
      val testString1 = "Text1;;;;;;;;;;Text2"
      val testString2 = ";Text1;;;;;;;;;;Text2"
      val testString3 = "Text1;;;;;;;;;;Text2;"
      val testString4 = ";Text1;;;;;;;;;;Text2;"
      val testString5 = ";;Text1;;;;;;;;;;Text2;;"
      val testString6 = ";Text1;;;;;;;;;;Text2;;"
      val testString7 = ";;Text1;;;;;;;;;;Text2;"
      val testString8 = ";;Text1;;;;;;;;;;Text2"
      val testString9 = "Text1;;;;;;;;;;Text2;;"
      assertEquals(testString1, { er.replaceAll(testString1, None) })
      assertEquals(testString2, { er.replaceAll(testString2, None) })
      assertEquals(testString3, { er.replaceAll(testString3, None) })
      assertEquals(testString4, { er.replaceAll(testString4, None) })
      assertEquals(testString5, { er.replaceAll(testString5, None) })
      assertEquals(testString6, { er.replaceAll(testString6, None) })
      assertEquals(testString7, { er.replaceAll(testString7, None) })
      assertEquals(testString8, { er.replaceAll(testString8, None) })
      assertEquals(testString9, { er.replaceAll(testString9, None) })
    }
  }

  @Test def testSemiColonSP = {
    EntityReplacer { er =>
      val testString = ";%SP;"
      assertEquals("; ", { er.replaceAll(testString) })
    }
  }

}
