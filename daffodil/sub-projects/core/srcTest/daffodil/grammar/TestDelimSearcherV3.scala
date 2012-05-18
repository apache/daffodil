package daffodil.grammar

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.dsom.Compiler
import scala.util.logging.ConsoleLogger
import java.nio.CharBuffer

class TestDelimSearcherV3 extends JUnit3Suite {

  def testSingleSeparatorChar = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",")
    assert(ds.delimiters.length == 1) // Verified delimiter was added
    val str1 = "abc,def"
    val str2 = "abc,def,"
    val str3 = ",abc,def"
    val str4 = "abc,,def"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)

    // Round 2 - Expect: EOF, def, 6
    val (state1a, result1a, endPos1a, endPosDelim1a) = ds.search(cb, endPos1 + 1)
    assertEquals(ds.SearchResult.EOF, state1a)
    assertEquals("def", result1a)
    assertEquals(6, endPos1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state2, result2, endPos2, endPosDelim2) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)

    // Round 2 - Expect: FullMatch, def, 7
    val (state2a, result2a, endPos2a, endPosDelim2a) = ds.search(cb, endPos2 + 1)
    assertEquals(ds.SearchResult.FullMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(7, endPos2a)

    // Round 3 - Expect: EOF, EMPTY, 7
    val (state2b, result2b, endPos2b, endPosDelim2b) = ds.search(cb, endPos2a + 1)
    assertEquals(ds.SearchResult.EOF, state2b)
    assertEquals("", result2b)
    assertEquals(7, endPos2b)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0
    val (state3, result3, endPos3, endPosDelim3) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state3)
    assertEquals("", result3)
    assertEquals(0, endPos3)

    // Round 2 - Expect: FullMatch, abc, 4
    val (state3a, result3a, endPos3a, endPosDelim3a) = ds.search(cb, endPos3 + 1)
    assertEquals(ds.SearchResult.FullMatch, state3a)
    assertEquals("abc", result3a)
    assertEquals(4, endPos3a)

    // Round 3 - Expect: EOF, def, 7
    val (state3b, result3b, endPos3b, endPosDelim3b) = ds.search(cb, endPos3a + 1)
    assertEquals(ds.SearchResult.EOF, state3b)
    assertEquals("def", result3b)
    assertEquals(7, endPos3b)

    // STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 0
    val (state4, result4, endPos4, endPosDelim4) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state4)
    assertEquals("abc", result4)
    assertEquals(3, endPos4)

    // Round 2 - Expect: FullMatch, EMPTY, 4
    val (state4a, result4a, endPos4a, endPosDelim4a) = ds.search(cb, endPos4 + 1)
    assertEquals(ds.SearchResult.FullMatch, state4a)
    assertEquals("", result4a)
    assertEquals(4, endPos4a)

    // Round 3 - Expect: EOF, def, 7
    val (state4b, result4b, endPos4b, endPosDelim4b) = ds.search(cb, endPos4a + 1)
    assertEquals(ds.SearchResult.EOF, state4b)
    assertEquals("def", result4b)
    assertEquals(7, endPos4b)
  }

  def testSingleSeparatorCharClassNL = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("%NL;")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc\ndef"
    val str2 = "abc\ndef\n"
    val str3 = "\nabc\rdef"
    val str4 = "abc\n\rdef"
    val str5 = "abc\rdef\r\nhij"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)

    // Round 2 - Expect: EOF, def, 6
    val (state1a, result1a, endPos1a, endPosDelim1a) = ds.search(cb, endPos1 + 1)
    assertEquals(ds.SearchResult.EOF, state1a)
    assertEquals("def", result1a)
    assertEquals(6, endPos1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state2, result2, endPos2, endPosDelim2) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)

    // Round 2 - Expect: FullMatch, def, 7
    val (state2a, result2a, endPos2a, endPosDelim2a) = ds.search(cb, endPos2 + 1)
    assertEquals(ds.SearchResult.FullMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(7, endPos2a)

    // Round 3 - Expect: EOF, EMPTY, 7
    val (state2b, result2b, endPos2b, endPosDelim2b) = ds.search(cb, endPos2a + 1)
    assertEquals(ds.SearchResult.EOF, state2b)
    assertEquals("", result2b)
    assertEquals(7, endPos2b)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0
    val (state3, result3, endPos3, endPosDelim3) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state3)
    assertEquals("", result3)
    assertEquals(0, endPos3)

    // Round 2 - Expect: FullMatch, abc, 4
    val (state3a, result3a, endPos3a, endPosDelim3a) = ds.search(cb, endPos3 + 1)
    assertEquals(ds.SearchResult.FullMatch, state3a)
    assertEquals("abc", result3a)
    assertEquals(4, endPos3a)

    // Round 3 - Expect: EOF, def, 7
    val (state3b, result3b, endPos3b, endPosDelim3b) = ds.search(cb, endPos3a + 1)
    assertEquals(ds.SearchResult.EOF, state3b)
    assertEquals("def", result3b)
    assertEquals(7, endPos3b)

    // STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 0
    val (state4, result4, endPos4, endPosDelim4) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state4)
    assertEquals("abc", result4)
    assertEquals(3, endPos4)

    // Round 2 - Expect: FullMatch, EMPTY, 4
    val (state4a, result4a, endPos4a, endPosDelim4a) = ds.search(cb, endPos4 + 1)
    assertEquals(ds.SearchResult.FullMatch, state4a)
    assertEquals("", result4a)
    assertEquals(4, endPos4a)

    // Round 3 - Expect: EOF, def, 7
    val (state4b, result4b, endPos4b, endPosDelim4b) = ds.search(cb, endPos4a + 1)
    assertEquals(ds.SearchResult.EOF, state4b)
    assertEquals("def", result4b)
    assertEquals(7, endPos4b)
    
    // STR5 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str5.length() + 1)
    cb.put(str5)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state5, result5, endPos5, endPosDelim5) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state5)
    assertEquals("abc", result5)
    assertEquals(3, endPos5)
    assertEquals(3, endPosDelim5)
    
    // Round 2 - Expect: FullMatch, def, 7, 8
    val (state5a, result5a, endPos5a, endPosDelim5a) = ds.search(cb, endPosDelim5 + 1)
    assertEquals(ds.SearchResult.FullMatch, state5a)
    assertEquals("def", result5a)
    assertEquals(7, endPos5a)
    assertEquals(8, endPosDelim5a)
    
    // Round 3 - Expect: EOF, hij, 7, 8
    val (state5b, result5b, endPos5b, endPosDelim5b) = ds.search(cb, endPosDelim5a + 1)
    assertEquals(ds.SearchResult.EOF, state5b)
    assertEquals("hij", result5b)
    assertEquals(11, endPos5b)
    assertEquals(11, endPosDelim5b)
  }

  def testSingleSeparatorCharClassWSP = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("%WSP;")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc\tdef"
    val str2 = "abc\tdef\t"
    val str3 = "\tabc\tdef"
    val str4 = "abc\t\tdef"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)

    // Round 2 - Expect: EOF, def, 6
    val (state1a, result1a, endPos1a, endPosDelim1a) = ds.search(cb, endPos1 + 1)
    assertEquals(ds.SearchResult.EOF, state1a)
    assertEquals("def", result1a)
    assertEquals(6, endPos1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state2, result2, endPos2, endPosDelim2) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)

    // Round 2 - Expect: FullMatch, def, 7
    val (state2a, result2a, endPos2a, endPosDelim2a) = ds.search(cb, endPos2 + 1)
    assertEquals(ds.SearchResult.FullMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(7, endPos2a)

    // Round 3 - Expect: EOF, EMPTY, 7
    val (state2b, result2b, endPos2b, endPosDelim2b) = ds.search(cb, endPos2a + 1)
    assertEquals(ds.SearchResult.EOF, state2b)
    assertEquals("", result2b)
    assertEquals(7, endPos2b)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0
    val (state3, result3, endPos3, endPosDelim3) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state3)
    assertEquals("", result3)
    assertEquals(0, endPos3)

    // Round 2 - Expect: FullMatch, abc, 4
    val (state3a, result3a, endPos3a, endPosDelim3a) = ds.search(cb, endPos3 + 1)
    assertEquals(ds.SearchResult.FullMatch, state3a)
    assertEquals("abc", result3a)
    assertEquals(4, endPos3a)

    // Round 3 - Expect: EOF, def, 7
    val (state3b, result3b, endPos3b, endPosDelim3b) = ds.search(cb, endPos3a + 1)
    assertEquals(ds.SearchResult.EOF, state3b)
    assertEquals("def", result3b)
    assertEquals(7, endPos3b)

    // STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 0
    val (state4, result4, endPos4, endPosDelim4) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state4)
    assertEquals("abc", result4)
    assertEquals(3, endPos4)

    // Round 2 - Expect: FullMatch, EMPTY, 4
    val (state4a, result4a, endPos4a, endPosDelim4a) = ds.search(cb, endPos4 + 1)
    assertEquals(ds.SearchResult.FullMatch, state4a)
    assertEquals("", result4a)
    assertEquals(4, endPos4a)

    // Round 3 - Expect: EOF, def, 7
    val (state4b, result4b, endPos4b, endPosDelim4b) = ds.search(cb, endPos4a + 1)
    assertEquals(ds.SearchResult.EOF, state4b)
    assertEquals("def", result4b)
    assertEquals(7, endPos4b)
  }

  def testSingleSeparatorCharClassWSP_Plus = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("%WSP+;")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc\tdef"
    val str2 = "abc\tdef\t"
    val str3 = "\tabc\tdef"
    val str4 = "abc\t\tdef"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)

    // Round 2 - Expect: EOF, def, 6
    val (state1a, result1a, endPos1a, endPosDelim1a) = ds.search(cb, endPos1 + 1)
    assertEquals(ds.SearchResult.EOF, state1a)
    assertEquals("def", result1a)
    assertEquals(6, endPos1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state2, result2, endPos2, endPosDelim2) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)

    // Round 2 - Expect: FullMatch, def, 7
    val (state2a, result2a, endPos2a, endPosDelim2a) = ds.search(cb, endPos2 + 1)
    assertEquals(ds.SearchResult.FullMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(7, endPos2a)

    // Round 3 - Expect: EOF, EMPTY, 7
    val (state2b, result2b, endPos2b, endPosDelim2b) = ds.search(cb, endPos2a + 1)
    assertEquals(ds.SearchResult.EOF, state2b)
    assertEquals("", result2b)
    assertEquals(7, endPos2b)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0
    val (state3, result3, endPos3, endPosDelim3) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state3)
    assertEquals("", result3)
    assertEquals(0, endPos3)

    // Round 2 - Expect: FullMatch, abc, 4
    val (state3a, result3a, endPos3a, endPosDelim3a) = ds.search(cb, endPos3 + 1)
    assertEquals(ds.SearchResult.FullMatch, state3a)
    assertEquals("abc", result3a)
    assertEquals(4, endPos3a)

    // Round 3 - Expect: EOF, def, 7
    val (state3b, result3b, endPos3b, endPosDelim3b) = ds.search(cb, endPos3a + 1)
    assertEquals(ds.SearchResult.EOF, state3b)
    assertEquals("def", result3b)
    assertEquals(7, endPos3b)
    //val str4 = "abc\t\tdef"
    // STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state4, result4, endPos4, endPosDelim4) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state4)
    assertEquals("abc", result4)
    assertEquals(3, endPos4)
    assertEquals(4, endPosDelim4)

    // Round 2 - Expect: EOF, def, 7
    val (state4a, result4a, endPos4a, endPosDelim4a) = ds.search(cb, endPosDelim4 + 1)
    assertEquals(ds.SearchResult.EOF, state4a)
    assertEquals("def", result4a)
    assertEquals(7, endPos4a)
  }

  def testGetConsecutiveWSPList = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val str1 = "\t\t\tA\t\t\tB\tC\t\t"
    var cb: CharBuffer = CharBuffer.allocate(str1.length + 1)
    cb.put(str1)
    cb.flip()

    val list1 = ds.getConsecutiveWSPList(cb)
    assertEquals(4, list1.length)
  }

  def testSingleSeparatorCharClassWSP_Star = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("%WSP*;")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc\tdef"
    val str2 = "abc\tdef\t"
    val str3 = "\tabc\tdef"
    val str4 = "abc\t\tdef"
    val str5 = "abcdef"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)

    // Round 2 - Expect: EOF, def, 6
    val (state1a, result1a, endPos1a, endPosDelim1a) = ds.search(cb, endPos1 + 1)
    assertEquals(ds.SearchResult.EOF, state1a)
    assertEquals("def", result1a)
    assertEquals(6, endPos1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state2, result2, endPos2, endPosDelim2) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)

    // Round 2 - Expect: FullMatch, def, 7
    val (state2a, result2a, endPos2a, endPosDelim2a) = ds.search(cb, endPos2 + 1)
    assertEquals(ds.SearchResult.FullMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(7, endPos2a)

    // Round 3 - Expect: EOF, EMPTY, 7
    val (state2b, result2b, endPos2b, endPosDelim2b) = ds.search(cb, endPos2a + 1)
    assertEquals(ds.SearchResult.EOF, state2b)
    assertEquals("", result2b)
    assertEquals(7, endPos2b)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0
    val (state3, result3, endPos3, endPosDelim3) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state3)
    assertEquals("", result3)
    assertEquals(0, endPos3)

    // Round 2 - Expect: FullMatch, abc, 4
    val (state3a, result3a, endPos3a, endPosDelim3a) = ds.search(cb, endPos3 + 1)
    assertEquals(ds.SearchResult.FullMatch, state3a)
    assertEquals("abc", result3a)
    assertEquals(4, endPos3a)

    // Round 3 - Expect: EOF, def, 7
    val (state3b, result3b, endPos3b, endPosDelim3b) = ds.search(cb, endPos3a + 1)
    assertEquals(ds.SearchResult.EOF, state3b)
    assertEquals("def", result3b)
    assertEquals(7, endPos3b)

    // STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state4, result4, endPos4, endPosDelim4) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state4)
    assertEquals("abc", result4)
    assertEquals(3, endPos4)
    assertEquals(4, endPosDelim4)

    // Round 2 - Expect: EOF, def, 7
    val (state4a, result4a, endPos4a, endPosDelim4a) = ds.search(cb, endPosDelim4 + 1)
    assertEquals(ds.SearchResult.EOF, state4a)
    assertEquals("def", result4a)
    assertEquals(7, endPos4a)

    // STR5 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str5.length() + 1)
    cb.put(str5)
    cb.flip()

    // Round 1 - Expect: EOF, abcdef, 5, 5
    val (state5, result5, endPos5, endPosDelim5) = ds.search(cb)
    assertEquals(ds.SearchResult.EOF, state5)
    assertEquals("abcdef", result5)
    assertEquals(5, endPos5)
    assertEquals(5, endPosDelim5)
  }

  def testMultipleSeparatorsDifferent = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",")
    ds.addDelimiter("/")
    assert(ds.delimiters.length == 2) // Verified delimiter was added

    val str1 = "abc,def/efgh"
    val str2 = ",,//"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)

    // Round 2 - Expect: FullMatch, def, 7
    val (state1a, result1a, endPos1a, endPosDelim1a) = ds.search(cb, endPos1 + 1)
    assertEquals(ds.SearchResult.FullMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(7, endPos1a)

    // Round 3 - Expect: EOF, efgh, 11
    val (state1b, result1b, endPos1b, endPosDelim1b) = ds.search(cb, endPos1a + 1)
    assertEquals(ds.SearchResult.EOF, state1b)
    assertEquals("efgh", result1b)
    assertEquals(11, endPos1b)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0
    val (state2, result2, endPos2, endPosDelim2) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state2)
    assertEquals("", result2)
    assertEquals(0, endPos2)

    // Round 2 - Expect: FullMatch, EMPTY, 1
    val (state2a, result2a, endPos2a, endPosDelim2a) = ds.search(cb, endPos2 + 1)
    ds.printMatchStruct
    assertEquals(ds.SearchResult.FullMatch, state2a)
    assertEquals("", result2a)
    assertEquals(1, endPos2a)

    // Round 3 - Expect: FullMatch, EMPTY, 2
    val (state2b, result2b, endPos2b, endPosDelim2b) = ds.search(cb, endPos2a + 1)
    assertEquals(ds.SearchResult.FullMatch, state2b)
    assertEquals("", result2b)
    assertEquals(2, endPos2b)

    // Round 4 - Expect: FullMatch, EMPTY, 3
    val (state2c, result2c, endPos2c, endPosDelim2c) = ds.search(cb, endPos2b + 1)
    ds.printMatchStruct
    assertEquals(ds.SearchResult.FullMatch, state2c)
    assertEquals("", result2c)
    assertEquals(3, endPos2c)

    // Round 5 - Expect: EOF, EMPTY, 3
    val (state2d, result2d, endPos2d, endPosDelim2d) = ds.search(cb, endPos2c + 1)
    ds.printMatchStruct
    assertEquals(ds.SearchResult.EOF, state2d)
    assertEquals("", result2d)
    assertEquals(3, endPos2d)
  }

  def testMultipleCharacterDelimiters = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(":,:")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc:,:def:,:efgh"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 3)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)

    // Round 2 - Expect: FullMatch, def, 9
    val (state1a, result1a, endPos1a, endPosDelim1a) = ds.search(cb, endPos1 + 3)
    assertEquals(ds.SearchResult.FullMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(9, endPos1a)

    // Round 3 - Expect: EOF, efgh, 15
    val (state1b, result1b, endPos1b, endPosDelim1b) = ds.search(cb, endPos1a + 3)
    assertEquals(ds.SearchResult.EOF, state1b)
    assertEquals("efgh", result1b)
    assertEquals(15, endPos1b)
  }

  def testMultipleCharacterDelimitersWithCharacterClasses = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",%WSP;%NL;")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc,\n\ndef,\n\nefgh"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 3)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)

    // Round 2 - Expect: FullMatch, def, 9
    val (state1a, result1a, endPos1a, endPosDelim1a) = ds.search(cb, endPos1 + 3)
    assertEquals(ds.SearchResult.FullMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(9, endPos1a)

    // Round 3 - Expect: EOF, efgh, 15
    val (state1b, result1b, endPos1b, endPosDelim1b) = ds.search(cb, endPos1a + 3)
    assertEquals(ds.SearchResult.EOF, state1b)
    assertEquals("efgh", result1b)
    assertEquals(15, endPos1b)
  }

  def testMultipleCharacterDelimitersWithCharacterClassesRepeatedWSP = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",%WSP;%WSP;")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc,\n\ndef,\n\nefgh"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 3)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)

    // Round 2 - Expect: FullMatch, def, 9
    val (state1a, result1a, endPos1a, endPosDelim1a) = ds.search(cb, endPos1 + 3)
    assertEquals(ds.SearchResult.FullMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(9, endPos1a)

    // Round 3 - Expect: EOF, efgh, 15
    val (state1b, result1b, endPos1b, endPosDelim1b) = ds.search(cb, endPos1a + 3)
    assertEquals(ds.SearchResult.EOF, state1b)
    assertEquals("efgh", result1b)
    assertEquals(15, endPos1b)
  }

  def testReductionOfCharClasses = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("%WSP;%WSP*;") // 0 -> WSP+
    ds.addDelimiter("%WSP;%WSP;") // 1 -> WSP WSP
    ds.addDelimiter("%WSP+;%WSP+;") // 2 -> WSP+
    ds.addDelimiter("%WSP;%WSP;%WSP*;") // 3 -> WSP+
    ds.addDelimiter("%WSP*;") // 4 -> WSP*
    ds.addDelimiter("%WSP;%WSP;%WSP*;") // 5 -> WSP+
    assert(ds.delimiters.length == 6) // Verified delimiter was added

    val d0 = ds.delimiters(0)
    val d1 = ds.delimiters(1)
    val d2 = ds.delimiters(2)
    val d3 = ds.delimiters(3)
    val d4 = ds.delimiters(4)
    val d5 = ds.delimiters(5)
    assertEquals(1, d0.delimBuf.length)
    assertEquals(2, d1.delimBuf.length)
    assertEquals(1, d2.delimBuf.length)
    assertEquals(1, d3.delimBuf.length)
    assertEquals(1, d4.delimBuf.length)
    assertEquals(1, d5.delimBuf.length)

    val d0n0 = d0.delimBuf(0)
    assertEquals("WSP+Delim", d0n0.typeName)

    val d1n0 = d1.delimBuf(0)
    val d1n1 = d1.delimBuf(1)
    assertEquals("WSPDelim", d1n0.typeName)
    assertEquals("WSPDelim", d1n1.typeName)

    val d2n0 = d2.delimBuf(0)
    assertEquals("WSP+Delim", d2n0.typeName)

    val d3n0 = d3.delimBuf(0)
    assertEquals("WSP+Delim", d3n0.typeName)

    val d4n0 = d4.delimBuf(0)
    assertEquals("WSP*Delim", d4n0.typeName)

    val d5n0 = d5.delimBuf(0)
    assertEquals("WSP+Delim", d5n0.typeName)

    ds.printDelimStruct
  }

  def testLongestMatch = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("}")
    ds.addDelimiter("}}}")
    ds.addDelimiter("%WSP;")
    ds.addDelimiter("%WSP;:::")
    assert(ds.delimiters.length == 4) // Verified delimiter was added

    val str1 = "abc}}}def"
    val str2 = "abc :::def"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 5
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(5, endPosDelim1)

    // Round 2 - Expect: EOF, def, 8
    val (state1a, result1a, endPos1a, endPosDelim1a) = ds.search(cb, endPosDelim1 + 1)
    assertEquals(ds.SearchResult.EOF, state1a)
    assertEquals("def", result1a)
    assertEquals(8, endPos1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 6
    val (state2, result2, endPos2, endPosDelim2) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)
    assertEquals(6, endPosDelim2)

    // Round 2 - Expect: EOF, def, 9, 9
    val (state2a, result2a, endPos2a, endPosDelim2a) = ds.search(cb, endPosDelim2 + 1)
    assertEquals(ds.SearchResult.EOF, state2a)
    assertEquals("def", result2a)
    assertEquals(9, endPos2a)
    assertEquals(9, endPosDelim2a)
  }

  def testPartialMatchAtEndNonResume = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("}}}")
    ds.addDelimiter("%NL;")
    assert(ds.delimiters.length == 2) // Verified delimiter was added

    val str1 = "abcdef}"
    val str2 = "}}ghi"
    val str3 = "abcdef\r"
    val str4 = "\nghi"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: PartialMatch, abcdef, 6, 6
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.PartialMatch, state1)
    assertEquals("abcdef", result1)
    assertEquals(6, endPos1)
    assertEquals(6, endPosDelim1)

    // STR1 + STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str1.length() + str2.length() + 1)
    cb.put(str1 + str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abcdef, 6, 8
    val (state2, result2, endPos2, endPosDelim2) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state2)
    assertEquals("abcdef", result2)
    assertEquals(6, endPos2)
    assertEquals(8, endPosDelim2)

    // Round 2 - Expect: EOF, ghi, 11, 11
    val (state2a, result2a, endPos2a, endPosDelim2a) = ds.search(cb, endPosDelim2 + 1)
    assertEquals(ds.SearchResult.EOF, state2a)
    assertEquals("ghi", result2a)
    assertEquals(11, endPos2a)
    assertEquals(11, endPosDelim2a)
    
    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: PartialMatch, abcdef, 6, 6
    val (state3, result3, endPos3, endPosDelim3) = ds.search(cb)
    assertEquals(ds.SearchResult.PartialMatch, state3)
    assertEquals("abcdef", result3)
    assertEquals(6, endPos3)
    assertEquals(6, endPosDelim3)
    
    // STR3 + STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + str4.length() + 1)
    cb.put(str3 + str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abcdef, 6, 7
    val (state4, result4, endPos4, endPosDelim4) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state4)
    assertEquals("abcdef", result4)
    assertEquals(6, endPos4)
    assertEquals(7, endPosDelim4)
  }
  
  def testTwoPartialMatchAtEndNonResume1a = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("}}}")
    ds.addDelimiter("::")
    assert(ds.delimiters.length == 2) // Verified delimiter was added

    val str1 = "abcdef:}"
    val str2 = "}}ghi"
    
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: PartialMatch, abcdef, 7, 7
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.PartialMatch, state1)
    assertEquals("abcdef:", result1)
    assertEquals(7, endPos1)
    assertEquals(7, endPosDelim1)

    // STR1 + STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb = CharBuffer.allocate(str1.length() + str2.length() + 1)
    cb.put(str1 + str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abcdef:, 7, 9
    val (state2, result2, endPos2, endPosDelim2) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state2)
    assertEquals("abcdef:", result2)
    assertEquals(7, endPos2)
    assertEquals(9, endPosDelim2)
  }
  
  def testTwoPartialMatchAtEndNonResume1b = {
    // Differs from 1A in that the order of delimiters is reversed
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("::")
    ds.addDelimiter("}}}")
    assert(ds.delimiters.length == 2) // Verified delimiter was added

    val str1 = "abcdef:}"
    val str2 = "}}ghi"
    
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: PartialMatch, abcdef, 7, 7
    val (state1, result1, endPos1, endPosDelim1) = ds.search(cb)
    assertEquals(ds.SearchResult.PartialMatch, state1)
    assertEquals("abcdef:", result1)
    assertEquals(7, endPos1)
    assertEquals(7, endPosDelim1)

    // STR1 + STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb = CharBuffer.allocate(str1.length() + str2.length() + 1)
    cb.put(str1 + str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abcdef:, 7, 9
    val (state2, result2, endPos2, endPosDelim2) = ds.search(cb)
    assertEquals(ds.SearchResult.FullMatch, state2)
    assertEquals("abcdef:", result2)
    assertEquals(7, endPos2)
    assertEquals(9, endPosDelim2)
  }

}