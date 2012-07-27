package daffodil.dsom

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import java.util.regex.Pattern
import java.util.regex.Matcher

class TestEntityReplacer extends JUnit3Suite {

  def testEmptyString = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll(""), "")
  }

  def testEscapeScheme = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll("Text%Text"), "Text%Text") // Works basic case
    assertEquals(e.replaceAll("Text%%Text"), "Text%Text") // Works basic case
    assertEquals(e.replaceAll("Text%%#%%Text"), "Text%#%Text") // Works multiple
    assertEquals(e.replaceAll("Text%%%#%%Text"), "Text%%#%Text") // Works multiple
    assertEquals(e.replaceAll("Text%%#65;Text"), "Text%AText") // Works multiple
  }
  
  def testEntityReplacement = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll("Text%NUL;Text"), "Text\u0000Text") // Works basic case
    assertEquals(e.replaceAll("Text%NUL;%NULText"), "Text\u0000%NULText") // Works basic case
    assertEquals(e.replaceAll("Text%NUL;%BEL;Text"), "Text\u0000\u0007Text") // Works basic case
  }

  def testHexadecimalCodePointReplacement = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll("Text%#x0000;Text"), "Text\u0000Text") // Works basic case
    assertEquals(e.replaceAll("Text%#x0000;Text%#x000D;"), "Text\u0000Text\u000D") // Works multiple hex
    assertEquals(e.replaceAll("Text%#x0000;Text%#x000D"), "Text\u0000Text%#x000D") // Works one proper, one improper
  }

  def testDecimalCodePointReplacement = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll("Text%#65;Text"), "TextAText") // Works basic case
    assertEquals(e.replaceAll("Text%#0000000000065;Text"), "TextAText") // Works basic case w/ padding
    assertEquals(e.replaceAll("Text%#65;Text%#66;"), "TextATextB") // Works multiple
    assertEquals(e.replaceAll("Text%#65;Text%#000000000066;"), "TextATextB") // Works multiple w/ padding
    assertEquals(e.replaceAll("Text%#65;Text%#66"), "TextAText%#66") // Works one proper, one improper
  }

  def testRawByteReplacement = {
    val e = new EntityReplacer
    assertEquals(e.replaceAll("%#rFF;"), "%#rFF;")
    assertEquals(e.replaceAll("%#rFF;", false), "%#rFF;")
    assertEquals(e.replaceAll("%#rFF", true), "%#rFF")
    assertEquals(e.replaceAll("%#rFF;", true), "1515")
    assertEquals(e.replaceAll("%#rFF; %#rFA;", true), "1515 1510")
    assertEquals(e.replaceAll("%#rFF; %#rFA", true), "1515 %#rFA")
  }

  def testAll = {
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
