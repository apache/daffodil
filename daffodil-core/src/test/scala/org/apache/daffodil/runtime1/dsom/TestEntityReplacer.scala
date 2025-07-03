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

package org.apache.daffodil.runtime1.dsom

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.cookers.EntityReplacer

import org.junit.Assert._
import org.junit.Test

class TestEntityReplacer {

  @Test def testEmptyString() = {
    EntityReplacer { e =>
      assertEquals(e.replaceAll(""), "")
    }
  }

  @Test def testEscapeScheme() = {
    EntityReplacer { er =>
      val f1 = intercept[Exception] { er.replaceAll("Text%Text") }
      assertTrue(
        f1.getMessage().contains("Invalid DFDL Entity (%Text) found in \"Text%Text\"")
      ) // Works basic case
      assertEquals(
        "Text%Text",
        er.replaceAll("Text%%Text", forUnparse = true)
      ) // Works basic case
      assertEquals(
        "Text%#%Text",
        { er.replaceAll("Text%%#%%Text", forUnparse = true) }
      ) // Works multiple

      val f2 = intercept[Exception] { { er.replaceAll("Text%%%#%%Text") } }
      assertTrue(
        f2.getMessage().contains("Invalid DFDL Entity (%#) found in \"Text%%%#%%Text\"")
      ) // Works multiple
      assertEquals(
        "Text%AText",
        { er.replaceAll("Text%%%#65;Text", forUnparse = true) }
      ) // Works multiple
    }
  }

  @Test def testEntityReplacement() = {
    EntityReplacer { er =>
      assertEquals("Text\u0000Text", { er.replaceAll("Text%NUL;Text") }) // Works basic case

      val f1 = intercept[Exception] { { er.replaceAll("Text%NUL;%NULText") } }
      assertTrue(
        f1.getMessage()
          .contains("Invalid DFDL Entity (%NULText) found in \"Text%NUL;%NULText\"")
      ) // Works basic case
      assertEquals(
        "Text\u0000\u0007Text",
        { er.replaceAll("Text%NUL;%BEL;Text") }
      ) // Works basic case
    }
  }

  @Test def testHexadecimalCodePointReplacement() = {
    EntityReplacer { er =>
      assertEquals("Text\u0000Text", { er.replaceAll("Text%#x0000;Text") }) // Works basic case
      assertEquals(
        "Text\u0000Text\u000D",
        { er.replaceAll("Text%#x0000;Text%#x000D;") }
      ) // Works multiple hex

      val f1 = intercept[Exception] { { er.replaceAll("Text%#x0000;Text%#x000D") } }
      assertTrue(
        f1.getMessage()
          .contains("Invalid DFDL Entity (%#x000D) found in \"Text%#x0000;Text%#x000D\"")
      ) // Works one proper, one improper
    }
  }

  @Test def testDecimalCodePointReplacement() = {
    EntityReplacer { er =>
      assertEquals("TextAText", { er.replaceAll("Text%#65;Text") }) // Works basic case
      assertEquals(
        "TextAText",
        { er.replaceAll("Text%#0000000000065;Text") }
      ) // Works basic case w/ padding
      assertEquals("TextATextB", { er.replaceAll("Text%#65;Text%#66;") }) // Works multiple
      assertEquals(
        "TextATextB",
        { er.replaceAll("Text%#65;Text%#000000000066;") }
      ) // Works multiple w/ padding

      val f1 = intercept[Exception] { { er.replaceAll("Text%#65;Text%#66") } }
      assertTrue(
        f1.getMessage().contains("Invalid DFDL Entity (%#66) found in \"Text%#65;Text%#66\"")
      ) // Works one proper, one improper
    }
  }

  @Test def testRawByteReplacement() = {
    EntityReplacer { er =>
      assertEquals("ÿ", { er.replaceAll("%#rFF;") })
      assertEquals("ÿ ú", { er.replaceAll("%#rFF; %#rFA;") })

      val f1 = intercept[Exception] { { er.replaceAll("%#rFF; %#rFA") } }
      assertTrue(
        f1.getMessage().contains("Invalid DFDL Entity (%#rFA) found in \"%#rFF; %#rFA\"")
      )
    }
  }

  @Test def testInvalidDfdlEntities() = {
    EntityReplacer { er =>
      val f1 = intercept[Exception] { { er.replaceAll("%#rTT;") } }
      assertTrue(
        f1.getMessage().contains("Invalid DFDL Entity (%#rTT;) found in \"%#rTT;\"")
      ) // Verify fails: Raw Byte

      val f2 = intercept[Exception] { { er.replaceAll("%#A;") } }
      assertTrue(
        f2.getMessage().contains("Invalid DFDL Entity (%#A;) found in \"%#A;\"")
      ) // Verify fails: Decimal Code Point

      val f3 = intercept[Exception] { { er.replaceAll("%#xTT;") } }
      assertTrue(
        f3.getMessage().contains("Invalid DFDL Entity (%#xTT;) found in \"%#xTT;\"")
      ) // Verify fails: Hexadecimal Code Point

      val f4 = intercept[Exception] { { er.replaceAll("%SomeInvalidName;") } }
      assertTrue(
        f4.getMessage()
          .contains("Invalid DFDL Entity (%SomeInvalidName;) found in \"%SomeInvalidName;\"")
      ) // Verify fails: Dfdl Entity
    }
  }

  @Test def testAll() = {
    EntityReplacer { er =>
      val testString = new StringBuilder
      testString.append("Text#%%Text")
      testString.append("Text%#x0042;Text%%#x000D")
      testString.append("Text%#65;Text")
      testString.append("%#rFF; %#rFA;")
      testString.append("Text%SP;%BEL;Text")
      testString.append("Text%NL;Text")

      val solutionString = new StringBuilder
      solutionString.append("Text#%Text")
      solutionString.append("TextBText%#x000D")
      solutionString.append("TextAText")
      solutionString.append("\u00FF \u00FA")
      solutionString.append("Text \u0007Text")
      solutionString.append("Text%NL;Text")

      val resultString = er.replaceAll(testString.toString(), forUnparse = true)

      assertEquals(solutionString.toString(), resultString)
    }
  }

  @Test def testMultiplePercents() = {
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

      assertEquals(solution1, { er.replaceAll(testString1, None, forUnparse = true) })

      val f1 = intercept[Exception] {
        {
          er.replaceAll(testString2, None)
        }
      }
      assertTrue(
        f1.getMessage()
          .contains("Invalid DFDL Entity (%Text1) found in \"%Text1%%%%%%%%%%Text2\"")
      )

      val f2 = intercept[Exception] { { er.replaceAll(testString3, None) } }
      assertTrue(
        f2.getMessage().contains("Invalid DFDL Entity (%) found in \"Text1%%%%%%%%%%Text2%\"")
      )

      val f3 = intercept[Exception] { { er.replaceAll(testString4) } }
      assertTrue(
        f3.getMessage()
          .contains("Invalid DFDL Entity (%Text1) found in \"%Text1%%%%%%%%%%Text2%\"")
      )

      assertEquals(solution5, { er.replaceAll(testString5, None, forUnparse = true) })

      val f4 = intercept[Exception] { { er.replaceAll(testString6) } }
      assertTrue(
        f4.getMessage()
          .contains("Invalid DFDL Entity (%Text1) found in \"%Text1%%%%%%%%%%Text2%%\"")
      )

      val f5 = intercept[Exception] { { er.replaceAll(testString7) } }
      assertTrue(
        f5.getMessage().contains("Invalid DFDL Entity (%) found in \"%%Text1%%%%%%%%%%Text2%\"")
      )

      assertEquals(solution8, { er.replaceAll(testString8, None, forUnparse = true) })
      assertEquals(solution9, { er.replaceAll(testString9, None, forUnparse = true) })
    }
  }

  @Test def testMultipleSemicolons() = {
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

  @Test def testSemiColonSP() = {
    EntityReplacer { er =>
      val testString = ";%SP;"
      assertEquals("; ", { er.replaceAll(testString) })
    }
  }

}
