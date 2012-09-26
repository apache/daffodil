package daffodil.grammar

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import scala.util.logging.ConsoleLogger
import java.nio.CharBuffer
import scala.collection.mutable.Queue
import stringsearch.delimiter._
import stringsearch.DelimSearcherV3._
import stringsearch.constructs._
import org.junit.Test

class TestDelimSearcherV3 extends JUnitSuite {

  /*
   * HELPER FUNCTIONS
   * */
  @Test def testGetCRLFList = {
    // FullMatch - \r\n
    // PartialMatch - \r (At End of Buffer)

    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val str1 = "\nabc\rdef\r\rhij\r\r\nklm\r" // A FullMatch and a PartialMatch
    val str2 = "\na\rb\nc\n" // No matches
    val str3 = ""
    var cb = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    val crlf1 = ds.getCRLFList(cb)
    assertEquals(2, crlf1.length)
    assertTrue(crlf1.contains((14, 15)))
    assertTrue(crlf1.contains((19, -1)))

    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    val crlf2 = ds.getCRLFList(cb)
    assertEquals(0, crlf2.length)

    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    val crlf3 = ds.getCRLFList(cb)
    assertEquals(0, crlf3.length)
  }

  @Test def testCrlfContains = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val str1 = "\nabc\rdef\r\rhij\r\r\nklm\r" // A FullMatch and a PartialMatch
    val str2 = "\na\rb\nc\n" // No matches
    var cb = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    val crlf1 = ds.getCRLFList(cb)
    assertEquals(2, crlf1.length)
    assertTrue(crlf1.contains((14, 15)))
    assertTrue(crlf1.contains((19, -1)))

    val d: stringsearch.delimiter.Delimiter = new stringsearch.delimiter.Delimiter

    val (state1a, beg1a, end1a) = d.crlfContains(crlf1, 14)
    assertEquals(CRLFState.Exists, state1a)
    assertEquals(14, beg1a)
    assertEquals(15, end1a)

    val (state1a1, beg1a1, end1a1) = d.crlfContains(crlf1, 15)
    assertEquals(CRLFState.Exists, state1a1)
    assertEquals(14, beg1a1)
    assertEquals(15, end1a1)

    val (state1b, beg1b, end1b) = d.crlfContains(crlf1, 19)
    assertEquals(CRLFState.Partial, state1b)
    assertEquals(19, beg1b)
    assertEquals(-1, end1b)

    val (state1b1, beg1b1, end1b1) = d.crlfContains(crlf1, 20)
    assertEquals(CRLFState.NotFound, state1b1)
    assertEquals(-1, beg1b1)
    assertEquals(-1, end1b1)

    val (state1c, beg1c, end1c) = d.crlfContains(crlf1, 5)
    assertEquals(CRLFState.NotFound, state1c)
    assertEquals(-1, beg1c)
    assertEquals(-1, end1c)

    val (state1d, beg1d, end1d) = d.crlfContains(crlf1, -1)
    assertEquals(CRLFState.NotFound, state1d)
    assertEquals(-1, beg1d)
    assertEquals(-1, end1d)
  }

  @Test def testGetConsecutiveWSPList = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val str1 = "\t\t\tA\t\t\tB\tC\t\t"
    val str2 = ""
    var cb: CharBuffer = CharBuffer.allocate(str1.length + 1)
    cb.put(str1)
    cb.flip()

    val list1 = ds.getConsecutiveWSPList(cb)
    assertEquals(4, list1.length)

    cb = CharBuffer.allocate(str2.length + 1)
    cb.put(str2)
    cb.flip()

    val list2 = ds.getConsecutiveWSPList(cb)
    assertEquals(0, list2.length)
  }

  @Test def testGetPrefixedDelims = {
    // Example string: "a}}}b"
    //                  01234
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val prefix = new stringsearch.delimiter.Delimiter
    prefix("}")

    prefix.fullMatches.add((1, 1))
    prefix.fullMatches.add((2, 2))
    prefix.fullMatches.add((3, 3))

    val delimsQ: Queue[stringsearch.delimiter.Delimiter] = new Queue[stringsearch.delimiter.Delimiter]

    val delim0 = new stringsearch.delimiter.Delimiter
    delim0("}}}")
    delim0.fullMatches.add((1, 3))

    val delim1 = new stringsearch.delimiter.Delimiter
    delim1("}}")
    delim1.fullMatches.add((1, 2))

    val delim2 = new stringsearch.delimiter.Delimiter
    delim2("}}}}")

    delimsQ += prefix
    delimsQ += delim0
    delimsQ += delim1
    delimsQ += delim2

    val list0 = ds.getPrefixedDelims(prefix, delimsQ.toList)

    // println(list0)

    assertEquals(2, list0.length)
    assertTrue(list0.filter(x => x._1 == 1 && x._2 == 2 && x._3.delimiterStr == delim1.delimiterStr).length > 0)
    assertTrue(list0.filter(x => x._1 == 1 && x._2 == 3 && x._3.delimiterStr == delim0.delimiterStr).length > 0)
  }

  @Test def testGetLongestMatch = {
    // Example string: "a}}}b"
    //                  01234
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val prefix = new stringsearch.delimiter.Delimiter
    prefix("}")

    prefix.fullMatches.add((1, 1))
    prefix.fullMatches.add((2, 2))
    prefix.fullMatches.add((3, 3))

    val delimsQ0: Queue[stringsearch.delimiter.Delimiter] = new Queue[stringsearch.delimiter.Delimiter]

    val delim0 = new stringsearch.delimiter.Delimiter
    delim0("}}}")
    delim0.fullMatches.add((1, 3))

    val delim1 = new stringsearch.delimiter.Delimiter
    delim1("}}")
    delim1.fullMatches.add((1, 2))

    val delim2 = new stringsearch.delimiter.Delimiter
    delim2("}}}}")

    delimsQ0 += prefix
    delimsQ0 += delim0
    delimsQ0 += delim1
    delimsQ0 += delim2

    // Prefixed result: Delimiter found at position (1,1) is the
    // start of a longer delimiter found at position (1,3)
    val result0 = ds.getLongestMatch(delimsQ0.toList)
    assertEquals((1, 3), (result0._1, result0._2))

    // Example Str: "a}b}}c"
    //               012345
    val delimsQ1: Queue[stringsearch.delimiter.Delimiter] = new Queue[stringsearch.delimiter.Delimiter]

    val delim3 = new stringsearch.delimiter.Delimiter
    delim3("}")
    delim3.fullMatches.add((1, 1))
    delim3.fullMatches.add((3, 3))
    delim3.fullMatches.add((4, 4))

    val delim4 = new stringsearch.delimiter.Delimiter
    delim4("}}")
    delim4.fullMatches.add((3, 4))

    delimsQ1 += delim3
    delimsQ1 += delim4

    // Non-prefixed result: The first delimiter found at position (1,1)
    // is not the start of a longer delimiter.
    val result1 = ds.getLongestMatch(delimsQ1.toList)
    assertEquals((1, 1), (result1._1, result1._2))

    // Test no matches
    val delimsQ2: Queue[stringsearch.delimiter.Delimiter] = new Queue[stringsearch.delimiter.Delimiter]
    val result2 = ds.getLongestMatch(delimsQ2.toList)
    assertEquals((-1, -1), (result2._1, result2._2))
  }

  @Test def testFindCharClasses = {
    // Expected to return first CharClass in string or None
    val d: stringsearch.delimiter.Delimiter = new stringsearch.delimiter.Delimiter with ConsoleLogger

    val str0 = "abcdef" // No CharClass
    val str1 = "%WSP;" // Single CharClass
    val str2 = "%WSP;%WSP+;" // Two CharClass, WSPDelim is first one
    val str3 = "%WSP+;" // Single CharClass
    val str4 = "%WSP*;" // Single CharClass
    val str5 = "%NL;" // Single CharClass
    val str6 = "" // Empty String - No CharClass
    val str7 = "%NL%WSP;" // First valid CharClass is WSP
    val str8 = "%;%WSP;;" // First valid CharClass is WSP
    val str9 = "%WSP,;" // No valid CharClass
    val str10 = "%WSP *;" // No valid CharClass

    val res0 = d.findCharClasses(str0)
    val res1 = d.findCharClasses(str1)
    val res2 = d.findCharClasses(str2)
    val res3 = d.findCharClasses(str3)
    val res4 = d.findCharClasses(str4)
    val res5 = d.findCharClasses(str5)
    val res6 = d.findCharClasses(str6)
    val res7 = d.findCharClasses(str7)
    val res8 = d.findCharClasses(str8)
    val res9 = d.findCharClasses(str9)
    val res10 = d.findCharClasses(str10)

    assertEquals((-1, None), res0)

    assertEquals(5, res1._1)
    res1._2 match {
      case Some(x: stringsearch.delimiter.WSPDelim) => assertTrue(true)
      case None => assertTrue(false)
      case _ => assertTrue(false)
    }

    assertEquals(5, res2._1)
    res2._2 match {
      case Some(x: stringsearch.delimiter.WSPDelim) => assertTrue(true)
      case None => assertTrue(false)
      case _ => assertTrue(false)
    }

    assertEquals(6, res3._1)
    res3._2 match {
      case Some(x: stringsearch.delimiter.WSPPlusDelim) => assertTrue(true)
      case None => assertTrue(false)
      case _ => assertTrue(false)
    }

    assertEquals(6, res4._1)
    res4._2 match {
      case Some(x: stringsearch.delimiter.WSPStarDelim) => assertTrue(true)
      case None => assertTrue(false)
      case _ => assertTrue(false)
    }

    assertEquals(4, res5._1)
    res5._2 match {
      case Some(x: stringsearch.delimiter.NLDelim) => assertTrue(true)
      case None => assertTrue(false)
      case _ => assertTrue(false)
    }

    assertEquals(-1, res6._1)
    res6._2 match {
      case None => assertTrue(true)
      case _ => assertTrue(false)
    }

    assertEquals(5, res7._1)
    res7._2 match {
      case Some(x: stringsearch.delimiter.WSPDelim) => assertTrue(true)
      case None => assertTrue(false)
      case _ => assertTrue(false)
    }

    assertEquals(5, res8._1)
    res8._2 match {
      case Some(x: stringsearch.delimiter.WSPDelim) => assertTrue(true)
      case None => assertTrue(false)
      case _ => assertTrue(false)
    }

    assertEquals(-1, res9._1)
    res9._2 match {
      case None => assertTrue(true)
      case _ => assertTrue(false)
    }

    assertEquals(-1, res10._1)
    res10._2 match {
      case None => assertTrue(true)
      case _ => assertTrue(false)
    }
  }

  @Test def testGetReducedDelim = {
    val d: stringsearch.delimiter.Delimiter = new stringsearch.delimiter.Delimiter

    val res1 = d.getReducedDelim(0, 0, 0)
    val res2 = d.getReducedDelim(0, 0, 1)
    val res3 = d.getReducedDelim(0, 1, 0)
    val res4 = d.getReducedDelim(0, 1, 1)
    val res5 = d.getReducedDelim(1, 0, 0)
    val res6 = d.getReducedDelim(1, 0, 1)
    val res7 = d.getReducedDelim(1, 1, 0)
    val res8 = d.getReducedDelim(1, 1, 1)
    val res9 = d.getReducedDelim(100, 0, 0) /* Multiple WSP when not accompanied
    											by WSP+ or WSP* handled outside of
    											getReducedDelim */

    res1 match {
      case Some(_) => assertTrue(false)
      case None => assertTrue(true)
    }

    res2 match {
      case Some(x: WSPStarDelim) => assertTrue(true)
      case None => assertTrue(false)
    }

    res3 match {
      case Some(x: WSPPlusDelim) => assertTrue(true)
      case None => assertTrue(false)
    }

    res4 match {
      case Some(x: WSPPlusDelim) => assertTrue(true)
      case None => assertTrue(false)
    }

    res5 match {
      case Some(x: WSPDelim) => assertTrue(true)
      case None => assertTrue(false)
    }

    res6 match {
      case Some(x: WSPPlusDelim) => assertTrue(true)
      case None => assertTrue(false)
    }

    res7 match {
      case Some(x: WSPPlusDelim) => assertTrue(true)
      case None => assertTrue(false)
    }

    res8 match {
      case Some(x: WSPPlusDelim) => assertTrue(true)
      case None => assertTrue(false)
    }

    res9 match {
      case Some(_) => assertTrue(false)
      case None => assertTrue(true)
    }
  }

  @Test def testReduceDelimBuf = {
    val d: Delimiter = new Delimiter

    // WSP WSP* => WSP+
    val db0: Queue[DelimBase] = new Queue[DelimBase]
    db0 += new WSPDelim()
    db0 += new WSPStarDelim()

    // WSP WSP+ => WSP+
    val db1: Queue[DelimBase] = new Queue[DelimBase]
    db1 += new WSPDelim()
    db1 += new WSPPlusDelim()

    // WSP* WSP+ => WSP+
    val db2: Queue[DelimBase] = new Queue[DelimBase]
    db2 += new WSPStarDelim()
    db2 += new WSPPlusDelim()

    // WSP* WSP* => WSP*
    val db3: Queue[DelimBase] = new Queue[DelimBase]
    db3 += new WSPStarDelim()
    db3 += new WSPStarDelim()

    // WSP+ WSP+ => WSP+
    val db4: Queue[DelimBase] = new Queue[DelimBase]
    db4 += new WSPPlusDelim()
    db4 += new WSPPlusDelim()

    // WSP WSP => WSP WSP
    // Here we should note that WSP WSP WSP is NOT equivalent to WSP+
    // as WSP+ would imply that WSP WSP WSP WSP is also valid when in fact
    // it may not be.
    val db5: Queue[DelimBase] = new Queue[DelimBase]
    db5 += new WSPDelim()
    db5 += new WSPDelim()

    // WSP* => WSP*
    val db6: Queue[DelimBase] = new Queue[DelimBase]
    db6 += new WSPStarDelim()

    // WSP+ => WSP+
    val db7: Queue[DelimBase] = new Queue[DelimBase]
    db7 += new WSPPlusDelim()

    // WSP => WSP
    val db8: Queue[DelimBase] = new Queue[DelimBase]
    db8 += new WSPDelim()

    // WSP WSP WSP* NL WSP+ WSP* => WSP+ NL WSP+
    val db9: Queue[DelimBase] = new Queue[DelimBase]
    db9 += new WSPDelim()
    db9 += new WSPDelim()
    db9 += new WSPStarDelim()
    db9 += new NLDelim()
    db9 += new WSPPlusDelim()
    db9 += new WSPStarDelim()

    val res0 = d.reduceDelimBuf(db0.toArray)
    val res1 = d.reduceDelimBuf(db1.toArray)
    val res2 = d.reduceDelimBuf(db2.toArray)
    val res3 = d.reduceDelimBuf(db3.toArray)
    val res4 = d.reduceDelimBuf(db4.toArray)
    val res5 = d.reduceDelimBuf(db5.toArray)
    val res6 = d.reduceDelimBuf(db6.toArray)
    val res7 = d.reduceDelimBuf(db7.toArray)
    val res8 = d.reduceDelimBuf(db8.toArray)
    val res9 = d.reduceDelimBuf(db9.toArray)

    assertEquals(1, res0.length)
    assertTrue(res0(0).isInstanceOf[WSPPlusDelim])
    assertEquals(1, res1.length)
    assertTrue(res1(0).isInstanceOf[WSPPlusDelim])
    assertEquals(1, res2.length)
    assertTrue(res2(0).isInstanceOf[WSPPlusDelim])
    assertEquals(1, res3.length)
    assertTrue(res3(0).isInstanceOf[WSPStarDelim])
    assertEquals(1, res4.length)
    assertTrue(res4(0).isInstanceOf[WSPPlusDelim])
    assertEquals(2, res5.length)
    assertTrue(res5(0).isInstanceOf[WSPDelim])
    assertTrue(res5(1).isInstanceOf[WSPDelim])
    assertEquals(1, res6.length)
    assertTrue(res6(0).isInstanceOf[WSPStarDelim])
    assertEquals(1, res7.length)
    assertTrue(res7(0).isInstanceOf[WSPPlusDelim])
    assertEquals(1, res8.length)
    assertTrue(res8(0).isInstanceOf[WSPDelim])
    assertEquals(3, res9.length)
    assertTrue(res9(0).isInstanceOf[WSPPlusDelim])
    assertTrue(res9(1).isInstanceOf[NLDelim])
    assertTrue(res9(2).isInstanceOf[WSPPlusDelim])
  }

  @Test def testBuildDelimBuf = {
    val d: Delimiter = new Delimiter
    val str0 = ""
    val str1 = "abc"
    val str2 = "a%NL;a"
    val str3 = "c%NLb"
    val str4 = "a%WSP;b"
    val str5 = "%WSP;%WSP;"
    val str6 = ",%WSP;%WSP*;"

    val res0 = d.buildDelimBuf(str0) // Empty String
    val res1 = d.buildDelimBuf(str1) // CharDelim CharDelim CharDelim
    val res2 = d.buildDelimBuf(str2) // CharDelim NLDelim CharDelim
    val res3 = d.buildDelimBuf(str3) // CharDelim CharDelim CharDelim CharDelim CharDelim
    val res4 = d.buildDelimBuf(str4) // CharDelim WSPDelim CharDelim
    val res5 = d.buildDelimBuf(str5) // WSPDelim WSPDelim
    val res6 = d.buildDelimBuf(str6) // CharDelim WSPPlusDelim

    assertEquals(0, res0.length)

    assertEquals(3, res1.length)
    assertTrue(res1(0).isInstanceOf[CharDelim])
    assertTrue(res1(1).isInstanceOf[CharDelim])
    assertTrue(res1(2).isInstanceOf[CharDelim])

    assertEquals(3, res2.length)
    assertTrue(res2(0).isInstanceOf[CharDelim])
    assertTrue(res2(1).isInstanceOf[NLDelim])
    assertTrue(res2(2).isInstanceOf[CharDelim])

    assertEquals(5, res3.length)
    assertTrue(res3(0).isInstanceOf[CharDelim])
    assertTrue(res3(1).isInstanceOf[CharDelim])
    assertTrue(res3(2).isInstanceOf[CharDelim])
    assertTrue(res3(3).isInstanceOf[CharDelim])
    assertTrue(res3(4).isInstanceOf[CharDelim])

    assertEquals(3, res4.length)
    assertTrue(res4(0).isInstanceOf[CharDelim])
    assertTrue(res4(1).isInstanceOf[WSPDelim])
    assertTrue(res4(2).isInstanceOf[CharDelim])

    assertEquals(2, res5.length)
    assertTrue(res5(0).isInstanceOf[WSPDelim])
    assertTrue(res5(1).isInstanceOf[WSPDelim])

    assertEquals(2, res6.length)
    assertTrue(res6(0).isInstanceOf[CharDelim])
    assertTrue(res6(1).isInstanceOf[WSPPlusDelim])
  }

  @Test def testBuildDelimRegEx = {
    val ds: DelimSearcher = new DelimSearcher with ConsoleLogger

    ds.addDelimiter("") // Empty String
    ds.addDelimiter("abc") // abc
    ds.addDelimiter("a%NL;a") // a(\\n\\r|\\n|\\r)a
    ds.addDelimiter("c%NLb") // c%NLb
    ds.addDelimiter("a%WSP;b") // a\\sb
    ds.addDelimiter("%WSP;%WSP;") // \\s\\s
    ds.addDelimiter(",%WSP;%WSP*;") // ,\\s\\* => ,\\s+
    ds.addDelimiter("+^$") // \\+\\^\\$
    ds.addDelimiter("%WSP*;") // \\s*

    val res0 = ds.delimiters(0).buildDelimRegEx()
    val res1 = ds.delimiters(1).buildDelimRegEx()
    val res2 = ds.delimiters(2).buildDelimRegEx()
    val res3 = ds.delimiters(3).buildDelimRegEx()
    val res4 = ds.delimiters(4).buildDelimRegEx()
    val res5 = ds.delimiters(5).buildDelimRegEx()
    val res6 = ds.delimiters(6).buildDelimRegEx()
    val res7 = ds.delimiters(7).buildDelimRegEx()
    val res8 = ds.delimiters(8).buildDelimRegEx()

    assertEquals("", res0)
    assertEquals("abc", res1)
    assertEquals("a(\\r\\n|\\n|\\r|\\u0085|\\u2028)a", res2)
    assertEquals("c%NLb", res3)
    assertEquals("a(\\s|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" + 
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" + 
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)b", res4)
    assertEquals("(\\s|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" + 
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" + 
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)(\\s|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" + 
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" + 
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)", res5)
    assertEquals(",(\\s|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" + 
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" + 
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)+", res6)
    assertEquals("\\+\\^\\$", res7)
    assertEquals("(\\s|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" + 
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" + 
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)*", res8)
  }

  /*
   * TEST STATE COVERAGE
   * */
  @Test def testNLStates = {
    // Exercises crlfContains and getCRLFList during execution
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("%NL;")
    ds.enableStateTrace
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "a\r\nb" // NL - CRLF Exists
    val str2 = "a\rb" // NL 
    val str3 = "ab\r" // NL - CRLF Partial
    val str4 = "a\nb" // NL
    val str5 = "ab\n" // NL
    val str6 = "abc" // NL - Not Matched

    var cb = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    val crlf1 = ds.getCRLFList(cb)
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("a", result1)
    assertEquals(1, endPos1)
    assertEquals(2, endPosDelim1)
    assertTrue(crlf1.contains((1, 2)))

    // Verify expected states encountered
    val d1: Delimiter = ds.delimiters(0)
    val trace1 = d1.stateTrace.map(x => x._1)
    assertEquals(3, trace1.length)
    assertEquals(SearchState.NLNoMatch, trace1(0))
    assertEquals(SearchState.NLCrlfExists, trace1(1))
    assertEquals(SearchState.NLNoMatch, trace1(2))

    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    val crlf2 = ds.getCRLFList(cb)
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("a", result2)
    assertEquals(1, endPos2)
    assertEquals(1, endPosDelim2)
    assertEquals(0, crlf2.length)

    // Verify expected states encountered
    val d2: Delimiter = ds.delimiters(0)
    val trace2 = d2.stateTrace.map(x => x._1)
    assertEquals(3, trace2.length)
    assertEquals(SearchState.NLNoMatch, trace2(0))
    assertEquals(SearchState.NLCrlfNotFound, trace2(1))
    assertEquals(SearchState.NLNoMatch, trace2(2))

    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    val crlf3 = ds.getCRLFList(cb)
    val (state3, result3, endPos3, endPosDelim3, _) = ds.search(cb)
    assertEquals(SearchResult.PartialMatch, state3)
    assertEquals("ab", result3)
    assertEquals(2, endPos3)
    assertEquals(2, endPosDelim3)
    assertTrue(crlf3.contains((2, -1)))

    // Verify expected states encountered
    val d3: Delimiter = ds.delimiters(0)
    val trace3 = d3.stateTrace.map(x => x._1)
    assertEquals(3, trace3.length)
    assertEquals(SearchState.NLNoMatch, trace3(0))
    assertEquals(SearchState.NLNoMatch, trace3(1))
    assertEquals(SearchState.NLCrlfPartial, trace3(2))

    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    val crlf4 = ds.getCRLFList(cb)
    val (state4, result4, endPos4, endPosDelim4, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state4)
    assertEquals("a", result4)
    assertEquals(1, endPos4)
    assertEquals(1, endPosDelim4)
    assertEquals(0, crlf4.length)

    // Verify expected states encountered
    val d4: Delimiter = ds.delimiters(0)
    val trace4 = d4.stateTrace.map(x => x._1)
    assertEquals(3, trace4.length)
    assertEquals(SearchState.NLNoMatch, trace4(0))
    assertEquals(SearchState.NLCrlfNotFound, trace4(1))
    assertEquals(SearchState.NLNoMatch, trace4(2))

    cb = CharBuffer.allocate(str5.length() + 1)
    cb.put(str5)
    cb.flip()

    val crlf5 = ds.getCRLFList(cb)
    val (state5, result5, endPos5, endPosDelim5, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state5)
    assertEquals("ab", result5)
    assertEquals(2, endPos5)
    assertEquals(2, endPosDelim5)
    assertEquals(0, crlf5.length)

    // Verify expected states encountered
    val d5: Delimiter = ds.delimiters(0)
    val trace5 = d5.stateTrace.map(x => x._1)
    assertEquals(3, trace5.length)
    assertEquals(SearchState.NLNoMatch, trace5(0))
    assertEquals(SearchState.NLNoMatch, trace5(1))
    assertEquals(SearchState.NLCrlfNotFound, trace5(2))

    cb = CharBuffer.allocate(str6.length() + 1)
    cb.put(str6)
    cb.flip()

    val crlf6 = ds.getCRLFList(cb)
    val (state6, result6, endPos6, endPosDelim6, _) = ds.search(cb)
    assertEquals(SearchResult.NoMatch, state6)
    assertEquals("abc", result6)
    assertEquals(2, endPos6)
    assertEquals(2, endPosDelim6)
    assertEquals(0, crlf6.length)

    // Verify expected states encountered
    val d6: Delimiter = ds.delimiters(0)
    val trace6 = d6.stateTrace.map(x => x._1)
    assertEquals(3, trace6.length)
    assertEquals(SearchState.NLNoMatch, trace6(0))
    assertEquals(SearchState.NLNoMatch, trace6(1))
    assertEquals(SearchState.NLNoMatch, trace6(2))
  }

  // State Coverage
  @Test def testCharMightBeStartOfNextDelimiter = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",..")
    ds.enableStateTrace
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "a,,..b"
    var cb = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("a,", result1)
    assertEquals(2, endPos1)
    assertEquals(4, endPosDelim1)

    // Verify expected states encountered
    val d1: Delimiter = ds.delimiters(0)
    val trace1 = d1.stateTrace.map(x => x._1)
    // println(trace1)
    assertEquals(7, trace1.length)
    assertEquals(SearchState.NoMatch, trace1(0))
    assertEquals(SearchState.OtherMatch, trace1(1))
    assertEquals(SearchState.OtherNoMatch, trace1(2))
    assertEquals(SearchState.OtherMatch, trace1(3))
    assertEquals(SearchState.OtherMatch, trace1(4))
    assertEquals(SearchState.OtherMatch, trace1(5))
    assertEquals(SearchState.NoMatch, trace1(6))
  }

  // State Coverage
  @Test def testWSPPlus = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("%WSP+;")
    ds.enableStateTrace
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "a\t\t\tb"
    var cb = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("a", result1)
    assertEquals(1, endPos1)
    assertEquals(3, endPosDelim1)

    // Verify expected states encountered
    val d1: Delimiter = ds.delimiters(0)
    val trace1 = d1.stateTrace.map(x => x._1)
    assertEquals(5, trace1.length)
    assertEquals(SearchState.WSPPlusNoMatch, trace1(0))
    assertEquals(SearchState.WSPPlusMatch, trace1(1))
    assertEquals(SearchState.WSPModeAndSpace, trace1(2))
    assertEquals(SearchState.WSPModeAndSpace, trace1(3))
    assertEquals(SearchState.WSPPlusNoMatch, trace1(4))

    val str2 = "ab"
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.NoMatch, state2)
    assertEquals("ab", result2)
    assertEquals(1, endPos2)
    assertEquals(1, endPosDelim2)

    // Verify expected states encountered
    val d2: Delimiter = ds.delimiters(0)
    val trace2 = d2.stateTrace.map(x => x._1)
    assertEquals(2, trace2.length)
    assertEquals(SearchState.WSPPlusNoMatch, trace2(0))
    assertEquals(SearchState.WSPPlusNoMatch, trace2(1))
  }

  // State Coverage
  @Test def testWSPStar = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("%WSP*;")
    ds.enableStateTrace
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "a\t\t\tb"
    var cb = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("a", result1)
    assertEquals(1, endPos1)
    assertEquals(3, endPosDelim1)

    // Verify expected states encountered
    val d1: Delimiter = ds.delimiters(0)
    val trace1 = d1.stateTrace.map(x => x._1)
    assertEquals(5, trace1.length)
    assertEquals(SearchState.WSPStarNoMatch, trace1(0))
    assertEquals(SearchState.WSPStarMatch, trace1(1))
    assertEquals(SearchState.WSPModeAndSpace, trace1(2))
    assertEquals(SearchState.WSPModeAndSpace, trace1(3))
    assertEquals(SearchState.WSPStarNoMatch, trace1(4))

    val str2 = "ab"
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.NoMatch, state2)
    assertEquals("ab", result2)
    assertEquals(1, endPos2)
    assertEquals(1, endPosDelim2)

    // Verify expected states encountered
    val d2: Delimiter = ds.delimiters(0)
    val trace2 = d2.stateTrace.map(x => x._1)
    assertEquals(2, trace2.length)
    assertEquals(SearchState.WSPStarNoMatch, trace2(0))
    assertEquals(SearchState.WSPStarNoMatch, trace2(1))
  }

  // State Coverage
  @Test def testWSP = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("%WSP;")
    ds.enableStateTrace
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "a\t\t\tb"
    var cb = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("a", result1)
    assertEquals(1, endPos1)
    assertEquals(1, endPosDelim1)

    // Verify expected states encountered
    val d1: Delimiter = ds.delimiters(0)
    val trace1 = d1.stateTrace.map(x => x._1)
    assertEquals(5, trace1.length)
    assertEquals(SearchState.WSPNoMatch, trace1(0))
    assertEquals(SearchState.WSPMatch, trace1(1))
    assertEquals(SearchState.WSPMatch, trace1(2))
    assertEquals(SearchState.WSPMatch, trace1(3))
    assertEquals(SearchState.WSPNoMatch, trace1(4))

    val str2 = "ab"
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.NoMatch, state2)
    assertEquals("ab", result2)
    assertEquals(1, endPos2)
    assertEquals(1, endPosDelim2)

    // Verify expected states encountered
    val d2: Delimiter = ds.delimiters(0)
    val trace2 = d2.stateTrace.map(x => x._1)
    assertEquals(2, trace2.length)
    assertEquals(SearchState.WSPNoMatch, trace2(0))
    assertEquals(SearchState.WSPNoMatch, trace2(1))
  }

  // State Coverage
  @Test def testGetIntoSpaceState = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",")
    ds.enableStateTrace
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "a\t\t\tb"
    var cb = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Verify expected vs actual result
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.NoMatch, state1)
    assertEquals("a\t\t\tb", result1)
    assertEquals(4, endPos1)
    assertEquals(4, endPosDelim1)

    // Verify expected states encountered
    val d: Delimiter = ds.delimiters(0)
    val trace = d.stateTrace.map(x => x._1)
    assertEquals(5, trace.length)
    assertEquals(SearchState.NoMatch, trace(0))
    assertEquals(SearchState.SpaceAndNotWSPMode, trace(1))
    assertEquals(SearchState.SpaceAndNotWSPMode, trace(2))
    assertEquals(SearchState.SpaceAndNotWSPMode, trace(3))
    assertEquals(SearchState.NoMatch, trace(4))
  }

  @Test def testGetIntoOtherState = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",")
    assert(ds.delimiters.length == 1) // Verified delimiter was added
    ds.enableStateTrace

    val str1 = "a,b"
    var cb = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Verify expected result vs actual
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("a", result1)
    assertEquals(1, endPos1)
    assertEquals(1, endPosDelim1)

    // Verify expected states encountered
    val d: Delimiter = ds.delimiters(0)
    val trace = d.stateTrace.map(x => x._1)
    assertEquals(SearchState.NoMatch, trace(0))
    assertEquals(SearchState.OtherMatch, trace(1))
    assertEquals(SearchState.NoMatch, trace(2))
  }

  /*
   * TEST SEARCH
   * */
  @Test def testSingleSeparatorChar = {
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

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(3, endPosDelim1)

    // Round 2 - Expect: EOF, def, 6, 6
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 1)
    assertEquals(SearchResult.NoMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(6, endPos1a)
    assertEquals(6, endPosDelim1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)
    assertEquals(3, endPosDelim2)

    // Round 2 - Expect: FullMatch, def, 7, 7
    val (state2a, result2a, endPos2a, endPosDelim2a, _) = ds.search(cb, endPos2 + 1)
    assertEquals(SearchResult.FullMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(7, endPos2a)
    assertEquals(7, endPosDelim2a)

    // Round 3 - Expect: EOF, EMPTY, 7, 7
    val (state2b, result2b, endPos2b, endPosDelim2b, _) = ds.search(cb, endPos2a + 1)
    assertEquals(SearchResult.NoMatch, state2b)
    assertEquals("", result2b)
    assertEquals(7, endPos2b)
    assertEquals(7, endPosDelim2b)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0, 0
    val (state3, result3, endPos3, endPosDelim3, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state3)
    assertEquals("", result3)
    assertEquals(0, endPos3)
    assertEquals(0, endPosDelim3)

    // Round 2 - Expect: FullMatch, abc, 4, 4
    val (state3a, result3a, endPos3a, endPosDelim3a, _) = ds.search(cb, endPos3 + 1)
    assertEquals(SearchResult.FullMatch, state3a)
    assertEquals("abc", result3a)
    assertEquals(4, endPos3a)
    assertEquals(4, endPosDelim3a)

    // Round 3 - Expect: EOF, def, 7, 7
    val (state3b, result3b, endPos3b, endPosDelim3b, _) = ds.search(cb, endPos3a + 1)
    assertEquals(SearchResult.NoMatch, state3b)
    assertEquals("def", result3b)
    assertEquals(7, endPos3b)
    assertEquals(7, endPosDelim3b)

    // STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state4, result4, endPos4, endPosDelim4, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state4)
    assertEquals("abc", result4)
    assertEquals(3, endPos4)
    assertEquals(3, endPosDelim4)

    // Round 2 - Expect: FullMatch, EMPTY, 4, 4
    val (state4a, result4a, endPos4a, endPosDelim4a, _) = ds.search(cb, endPos4 + 1)
    assertEquals(SearchResult.FullMatch, state4a)
    assertEquals("", result4a)
    assertEquals(4, endPos4a)
    assertEquals(4, endPosDelim4a)

    // Round 3 - Expect: EOF, def, 7, 7
    val (state4b, result4b, endPos4b, endPosDelim4b, _) = ds.search(cb, endPos4a + 1)
    assertEquals(SearchResult.NoMatch, state4b)
    assertEquals("def", result4b)
    assertEquals(7, endPos4b)
    assertEquals(7, endPosDelim4b)
  }

  @Test def testSingleSeparatorCharClassNL = {
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

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(3, endPosDelim1)

    // Round 2 - Expect: EOF, def, 6, 6
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 1)
    assertEquals(SearchResult.NoMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(6, endPos1a)
    assertEquals(6, endPosDelim1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)
    assertEquals(3, endPosDelim2)

    // Round 2 - Expect: FullMatch, def, 7, 7
    val (state2a, result2a, endPos2a, endPosDelim2a, _) = ds.search(cb, endPos2 + 1)
    assertEquals(SearchResult.FullMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(7, endPos2a)
    assertEquals(7, endPosDelim2a)

    // Round 3 - Expect: EOF, EMPTY, 7, 7
    val (state2b, result2b, endPos2b, endPosDelim2b, _) = ds.search(cb, endPos2a + 1)
    assertEquals(SearchResult.NoMatch, state2b)
    assertEquals("", result2b)
    assertEquals(7, endPos2b)
    assertEquals(7, endPosDelim2b)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0, 0
    val (state3, result3, endPos3, endPosDelim3, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state3)
    assertEquals("", result3)
    assertEquals(0, endPos3)
    assertEquals(0, endPosDelim3)

    // Round 2 - Expect: FullMatch, abc, 4, 4
    val (state3a, result3a, endPos3a, endPosDelim3a, _) = ds.search(cb, endPos3 + 1)
    assertEquals(SearchResult.FullMatch, state3a)
    assertEquals("abc", result3a)
    assertEquals(4, endPos3a)
    assertEquals(4, endPosDelim3a)

    // Round 3 - Expect: EOF, def, 7, 7
    val (state3b, result3b, endPos3b, endPosDelim3b, _) = ds.search(cb, endPos3a + 1)
    assertEquals(SearchResult.NoMatch, state3b)
    assertEquals("def", result3b)
    assertEquals(7, endPos3b)
    assertEquals(7, endPosDelim3b)

    // STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state4, result4, endPos4, endPosDelim4, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state4)
    assertEquals("abc", result4)
    assertEquals(3, endPos4)
    assertEquals(3, endPosDelim4)

    // Round 2 - Expect: FullMatch, EMPTY, 4, 4
    val (state4a, result4a, endPos4a, endPosDelim4a, _) = ds.search(cb, endPos4 + 1)
    assertEquals(SearchResult.FullMatch, state4a)
    assertEquals("", result4a)
    assertEquals(4, endPos4a)
    assertEquals(4, endPosDelim4a)

    // Round 3 - Expect: EOF, def, 7, 7
    val (state4b, result4b, endPos4b, endPosDelim4b, _) = ds.search(cb, endPos4a + 1)
    assertEquals(SearchResult.NoMatch, state4b)
    assertEquals("def", result4b)
    assertEquals(7, endPos4b)
    assertEquals(7, endPosDelim4b)

    // STR5 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str5.length() + 1)
    cb.put(str5)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state5, result5, endPos5, endPosDelim5, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state5)
    assertEquals("abc", result5)
    assertEquals(3, endPos5)
    assertEquals(3, endPosDelim5)

    // Round 2 - Expect: FullMatch, def, 7, 8
    val (state5a, result5a, endPos5a, endPosDelim5a, _) = ds.search(cb, endPosDelim5 + 1)
    assertEquals(SearchResult.FullMatch, state5a)
    assertEquals("def", result5a)
    assertEquals(7, endPos5a)
    assertEquals(8, endPosDelim5a)

    // Round 3 - Expect: EOF, hij, 7, 8
    val (state5b, result5b, endPos5b, endPosDelim5b, _) = ds.search(cb, endPosDelim5a + 1)
    assertEquals(SearchResult.NoMatch, state5b)
    assertEquals("hij", result5b)
    assertEquals(11, endPos5b)
    assertEquals(11, endPosDelim5b)
  }

  @Test def testSingleSeparatorCharClassWSP = {
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

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(3, endPosDelim1)

    // Round 2 - Expect: EOF, def, 6, 6
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 1)
    assertEquals(SearchResult.NoMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(6, endPos1a)
    assertEquals(6, endPosDelim1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)
    assertEquals(3, endPosDelim2)

    // Round 2 - Expect: FullMatch, def, 7, 7
    val (state2a, result2a, endPos2a, endPosDelim2a, _) = ds.search(cb, endPos2 + 1)
    assertEquals(SearchResult.FullMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(7, endPos2a)
    assertEquals(7, endPosDelim2a)

    // Round 3 - Expect: EOF, EMPTY, 7, 7
    val (state2b, result2b, endPos2b, endPosDelim2b, _) = ds.search(cb, endPos2a + 1)
    assertEquals(SearchResult.NoMatch, state2b)
    assertEquals("", result2b)
    assertEquals(7, endPos2b)
    assertEquals(7, endPosDelim2b)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0, 0
    val (state3, result3, endPos3, endPosDelim3, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state3)
    assertEquals("", result3)
    assertEquals(0, endPos3)
    assertEquals(0, endPosDelim3)

    // Round 2 - Expect: FullMatch, abc, 4, 4
    val (state3a, result3a, endPos3a, endPosDelim3a, _) = ds.search(cb, endPos3 + 1)
    assertEquals(SearchResult.FullMatch, state3a)
    assertEquals("abc", result3a)
    assertEquals(4, endPos3a)
    assertEquals(4, endPosDelim3a)

    // Round 3 - Expect: EOF, def, 7, 7
    val (state3b, result3b, endPos3b, endPosDelim3b, _) = ds.search(cb, endPos3a + 1)
    assertEquals(SearchResult.NoMatch, state3b)
    assertEquals("def", result3b)
    assertEquals(7, endPos3b)
    assertEquals(7, endPosDelim3b)

    // STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state4, result4, endPos4, endPosDelim4, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state4)
    assertEquals("abc", result4)
    assertEquals(3, endPos4)
    assertEquals(3, endPosDelim4)

    // Round 2 - Expect: FullMatch, EMPTY, 4, 4
    val (state4a, result4a, endPos4a, endPosDelim4a, _) = ds.search(cb, endPos4 + 1)
    assertEquals(SearchResult.FullMatch, state4a)
    assertEquals("", result4a)
    assertEquals(4, endPos4a)
    assertEquals(4, endPosDelim4a)

    // Round 3 - Expect: EOF, def, 7, 7
    val (state4b, result4b, endPos4b, endPosDelim4b, _) = ds.search(cb, endPos4a + 1)
    assertEquals(SearchResult.NoMatch, state4b)
    assertEquals("def", result4b)
    assertEquals(7, endPos4b)
    assertEquals(7, endPosDelim4b)
  }

  @Test def testSingleSeparatorCharClassWSP_Plus = {
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

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(3, endPosDelim1)

    // Round 2 - Expect: EOF, def, 6, 6
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 1)
    assertEquals(SearchResult.NoMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(6, endPos1a)
    assertEquals(6, endPosDelim1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)
    assertEquals(3, endPosDelim2)

    // Round 2 - Expect: FullMatch, def, 7, 7
    val (state2a, result2a, endPos2a, endPosDelim2a, _) = ds.search(cb, endPos2 + 1)
    assertEquals(SearchResult.FullMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(7, endPos2a)
    assertEquals(7, endPosDelim2a)

    // Round 3 - Expect: EOF, EMPTY, 7, 7
    val (state2b, result2b, endPos2b, endPosDelim2b, _) = ds.search(cb, endPos2a + 1)
    assertEquals(SearchResult.NoMatch, state2b)
    assertEquals("", result2b)
    assertEquals(7, endPos2b)
    assertEquals(7, endPosDelim2b)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0, 0
    val (state3, result3, endPos3, endPosDelim3, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state3)
    assertEquals("", result3)
    assertEquals(0, endPos3)
    assertEquals(0, endPosDelim3)

    // Round 2 - Expect: FullMatch, abc, 4, 4
    val (state3a, result3a, endPos3a, endPosDelim3a, _) = ds.search(cb, endPos3 + 1)
    assertEquals(SearchResult.FullMatch, state3a)
    assertEquals("abc", result3a)
    assertEquals(4, endPos3a)
    assertEquals(4, endPosDelim3a)

    // Round 3 - Expect: EOF, def, 7, 7
    val (state3b, result3b, endPos3b, endPosDelim3b, _) = ds.search(cb, endPos3a + 1)
    assertEquals(SearchResult.NoMatch, state3b)
    assertEquals("def", result3b)
    assertEquals(7, endPos3b)
    assertEquals(7, endPosDelim3b)

    // STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 4
    val (state4, result4, endPos4, endPosDelim4, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state4)
    assertEquals("abc", result4)
    assertEquals(3, endPos4)
    assertEquals(4, endPosDelim4)

    // Round 2 - Expect: EOF, def, 7, 7
    val (state4a, result4a, endPos4a, endPosDelim4a, _) = ds.search(cb, endPosDelim4 + 1)
    assertEquals(SearchResult.NoMatch, state4a)
    assertEquals("def", result4a)
    assertEquals(7, endPos4a)
    assertEquals(7, endPosDelim4a)
  }

  @Test def testSingleSeparatorCharClassWSP_Star = {
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

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(3, endPosDelim1)

    // Round 2 - Expect: EOF, def, 6, 6
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 1)
    assertEquals(SearchResult.NoMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(6, endPos1a)
    assertEquals(6, endPosDelim1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)
    assertEquals(3, endPosDelim2)

    // Round 2 - Expect: FullMatch, def, 7, 7
    val (state2a, result2a, endPos2a, endPosDelim2a, _) = ds.search(cb, endPos2 + 1)
    assertEquals(SearchResult.FullMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(7, endPos2a)
    assertEquals(7, endPosDelim2a)

    // Round 3 - Expect: EOF, EMPTY, 7, 7
    val (state2b, result2b, endPos2b, endPosDelim2b, _) = ds.search(cb, endPos2a + 1)
    assertEquals(SearchResult.NoMatch, state2b)
    assertEquals("", result2b)
    assertEquals(7, endPos2b)
    assertEquals(7, endPosDelim2b)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0, 0
    val (state3, result3, endPos3, endPosDelim3, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state3)
    assertEquals("", result3)
    assertEquals(0, endPos3)
    assertEquals(0, endPosDelim3)

    // Round 2 - Expect: FullMatch, abc, 4, 4
    val (state3a, result3a, endPos3a, endPosDelim3a, _) = ds.search(cb, endPos3 + 1)
    assertEquals(SearchResult.FullMatch, state3a)
    assertEquals("abc", result3a)
    assertEquals(4, endPos3a)
    assertEquals(4, endPosDelim3a)

    // Round 3 - Expect: EOF, def, 7, 7
    val (state3b, result3b, endPos3b, endPosDelim3b, _) = ds.search(cb, endPos3a + 1)
    assertEquals(SearchResult.NoMatch, state3b)
    assertEquals("def", result3b)
    assertEquals(7, endPos3b)
    assertEquals(7, endPosDelim3b)

    // STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str4.length() + 1)
    cb.put(str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 4
    val (state4, result4, endPos4, endPosDelim4, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state4)
    assertEquals("abc", result4)
    assertEquals(3, endPos4)
    assertEquals(4, endPosDelim4)

    // Round 2 - Expect: EOF, def, 7, 7
    val (state4a, result4a, endPos4a, endPosDelim4a, _) = ds.search(cb, endPosDelim4 + 1)
    assertEquals(SearchResult.NoMatch, state4a)
    assertEquals("def", result4a)
    assertEquals(7, endPos4a)
    assertEquals(7, endPosDelim4a)

    // STR5 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str5.length() + 1)
    cb.put(str5)
    cb.flip()

    // Round 1 - Expect: EOF, abcdef, 5, 5
    val (state5, result5, endPos5, endPosDelim5, _) = ds.search(cb)
    assertEquals(SearchResult.NoMatch, state5)
    assertEquals("abcdef", result5)
    assertEquals(5, endPos5)
    assertEquals(5, endPosDelim5)
  }

  @Test def testMultipleSeparatorsDifferent = {
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

    // Round 1 - Expect: FullMatch, abc, 3, 3
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(3, endPosDelim1)

    // Round 2 - Expect: FullMatch, def, 7, 7
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 1)
    assertEquals(SearchResult.FullMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(7, endPos1a)
    assertEquals(7, endPosDelim1a)

    // Round 3 - Expect: EOF, efgh, 11, 11
    val (state1b, result1b, endPos1b, endPosDelim1b, _) = ds.search(cb, endPos1a + 1)
    assertEquals(SearchResult.NoMatch, state1b)
    assertEquals("efgh", result1b)
    assertEquals(11, endPos1b)
    assertEquals(11, endPosDelim1b)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, EMPTY, 0, 0
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("", result2)
    assertEquals(0, endPos2)
    assertEquals(0, endPosDelim2)

    // Round 2 - Expect: FullMatch, EMPTY, 1, 1
    val (state2a, result2a, endPos2a, endPosDelim2a, _) = ds.search(cb, endPos2 + 1)
    ds.printMatchStruct
    assertEquals(SearchResult.FullMatch, state2a)
    assertEquals("", result2a)
    assertEquals(1, endPos2a)
    assertEquals(1, endPosDelim2a)

    // Round 3 - Expect: FullMatch, EMPTY, 2, 2
    val (state2b, result2b, endPos2b, endPosDelim2b, _) = ds.search(cb, endPos2a + 1)
    assertEquals(SearchResult.FullMatch, state2b)
    assertEquals("", result2b)
    assertEquals(2, endPos2b)
    assertEquals(2, endPosDelim2b)

    // Round 4 - Expect: FullMatch, EMPTY, 3, 3
    val (state2c, result2c, endPos2c, endPosDelim2c, _) = ds.search(cb, endPos2b + 1)
    ds.printMatchStruct
    assertEquals(SearchResult.FullMatch, state2c)
    assertEquals("", result2c)
    assertEquals(3, endPos2c)
    assertEquals(3, endPosDelim2c)

    // Round 5 - Expect: EOF, EMPTY, 3, 3
    val (state2d, result2d, endPos2d, endPosDelim2d, _) = ds.search(cb, endPos2c + 1)
    ds.printMatchStruct
    assertEquals(SearchResult.NoMatch, state2d)
    assertEquals("", result2d)
    assertEquals(3, endPos2d)
    assertEquals(3, endPosDelim2d)
  }

  @Test def testMultipleCharacterDelimiters = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(":,:")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc:,:def:,:efgh"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 5
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(5, endPosDelim1)

    // Round 2 - Expect: FullMatch, def, 9, 11
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 3)
    assertEquals(SearchResult.FullMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(9, endPos1a)
    assertEquals(11, endPosDelim1a)

    // Round 3 - Expect: EOF, efgh, 15, 15
    val (state1b, result1b, endPos1b, endPosDelim1b, _) = ds.search(cb, endPos1a + 3)
    assertEquals(SearchResult.NoMatch, state1b)
    assertEquals("efgh", result1b)
    assertEquals(15, endPos1b)
    assertEquals(15, endPosDelim1b)
  }

  @Test def testMultipleCharacterDelimitersWithCharacterClasses = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",%WSP;%NL;")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc,\n\ndef,\n\nefgh"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 5
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(5, endPosDelim1)

    // Round 2 - Expect: FullMatch, def, 9, 11
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 3)
    assertEquals(SearchResult.FullMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(9, endPos1a)
    assertEquals(11, endPosDelim1a)

    // Round 3 - Expect: EOF, efgh, 15, 15
    val (state1b, result1b, endPos1b, endPosDelim1b, _) = ds.search(cb, endPos1a + 3)
    assertEquals(SearchResult.NoMatch, state1b)
    assertEquals("efgh", result1b)
    assertEquals(15, endPos1b)
    assertEquals(15, endPosDelim1b)
  }

  @Test def testWSPPlusNL = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",%WSP+;%NL;")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc,\n\ndef,\n\nefgh"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 5
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(5, endPosDelim1)

    // Round 2 - Expect: FullMatch, def, 9, 11
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 3)
    assertEquals(SearchResult.FullMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(9, endPos1a)
    assertEquals(11, endPosDelim1a)

    // Round 3 - Expect: EOF, efgh, 15, 15
    val (state1b, result1b, endPos1b, endPosDelim1b, _) = ds.search(cb, endPos1a + 3)
    assertEquals(SearchResult.NoMatch, state1b)
    assertEquals("efgh", result1b)
    assertEquals(15, endPos1b)
    assertEquals(15, endPosDelim1b)
  }

  @Test def testWSPStarNL = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",%WSP*;%NL;")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc,\n\ndef,\n\nefgh"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 1)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 5
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(5, endPosDelim1)

    // Round 2 - Expect: FullMatch, def, 9, 11
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 3)
    assertEquals(SearchResult.FullMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(9, endPos1a)
    assertEquals(11, endPosDelim1a)

    // Round 3 - Expect: EOF, efgh, 15, 15
    val (state1b, result1b, endPos1b, endPosDelim1b, _) = ds.search(cb, endPos1a + 3)
    assertEquals(SearchResult.NoMatch, state1b)
    assertEquals("efgh", result1b)
    assertEquals(15, endPos1b)
    assertEquals(15, endPosDelim1b)
  }

  @Test def testMultipleCharacterDelimitersWithCharacterClassesRepeatedWSP = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",%WSP;%WSP;")
    assert(ds.delimiters.length == 1) // Verified delimiter was added

    val str1 = "abc,\n\ndef,\n\nefgh"

    // STR1 >>>>>>>>>>>>>>>>>>>>>>>>
    var cb: CharBuffer = CharBuffer.allocate(str1.length() + 3)
    cb.put(str1)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 5
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(5, endPosDelim1)

    // Round 2 - Expect: FullMatch, def, 9, 12
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPos1 + 3)
    assertEquals(SearchResult.FullMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(9, endPos1a)
    assertEquals(11, endPosDelim1a)

    // Round 3 - Expect: EOF, efgh, 15, 15
    val (state1b, result1b, endPos1b, endPosDelim1b, _) = ds.search(cb, endPos1a + 3)
    assertEquals(SearchResult.NoMatch, state1b)
    assertEquals("efgh", result1b)
    assertEquals(15, endPos1b)
    assertEquals(15, endPosDelim1b)
  }

  @Test def testReductionOfCharClasses = {
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
  }

  @Test def testLongestMatch = {
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
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state1)
    assertEquals("abc", result1)
    assertEquals(3, endPos1)
    assertEquals(5, endPosDelim1)

    // Round 2 - Expect: EOF, def, 8
    val (state1a, result1a, endPos1a, endPosDelim1a, _) = ds.search(cb, endPosDelim1 + 1)
    assertEquals(SearchResult.NoMatch, state1a)
    assertEquals("def", result1a)
    assertEquals(8, endPos1a)
    assertEquals(8, endPosDelim1a)

    // STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str2.length() + 1)
    cb.put(str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abc, 3, 6
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("abc", result2)
    assertEquals(3, endPos2)
    assertEquals(6, endPosDelim2)

    // Round 2 - Expect: EOF, def, 9, 9
    val (state2a, result2a, endPos2a, endPosDelim2a, _) = ds.search(cb, endPosDelim2 + 1)
    assertEquals(SearchResult.NoMatch, state2a)
    assertEquals("def", result2a)
    assertEquals(9, endPos2a)
    assertEquals(9, endPosDelim2a)
  }

  @Test def testPartialMatchAtEndNonResume = {
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
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.PartialMatch, state1)
    assertEquals("abcdef", result1)
    assertEquals(6, endPos1)
    assertEquals(6, endPosDelim1)

    // STR1 + STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str1.length() + str2.length() + 1)
    cb.put(str1 + str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abcdef, 6, 8
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("abcdef", result2)
    assertEquals(6, endPos2)
    assertEquals(8, endPosDelim2)

    // Round 2 - Expect: EOF, ghi, 11, 11
    val (state2a, result2a, endPos2a, endPosDelim2a, _) = ds.search(cb, endPosDelim2 + 1)
    assertEquals(SearchResult.NoMatch, state2a)
    assertEquals("ghi", result2a)
    assertEquals(11, endPos2a)
    assertEquals(11, endPosDelim2a)

    // STR3 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + 1)
    cb.put(str3)
    cb.flip()

    // Round 1 - Expect: PartialMatch, abcdef, 6, 6
    val (state3, result3, endPos3, endPosDelim3, _) = ds.search(cb)
    assertEquals(SearchResult.PartialMatch, state3)
    assertEquals("abcdef", result3)
    assertEquals(6, endPos3)
    assertEquals(6, endPosDelim3)

    // STR3 + STR4 >>>>>>>>>>>>>>>>>>>>>>>>
    cb.clear()
    cb = CharBuffer.allocate(str3.length() + str4.length() + 1)
    cb.put(str3 + str4)
    cb.flip()

    // Round 1 - Expect: FullMatch, abcdef, 6, 7
    val (state4, result4, endPos4, endPosDelim4, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state4)
    assertEquals("abcdef", result4)
    assertEquals(6, endPos4)
    assertEquals(7, endPosDelim4)
  }

  @Test def testTwoPartialMatchAtEndNonResume1a = {
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
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.PartialMatch, state1)
    assertEquals("abcdef:", result1)
    assertEquals(7, endPos1)
    assertEquals(7, endPosDelim1)

    // STR1 + STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb = CharBuffer.allocate(str1.length() + str2.length() + 1)
    cb.put(str1 + str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abcdef:, 7, 9
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("abcdef:", result2)
    assertEquals(7, endPos2)
    assertEquals(9, endPosDelim2)
  }

  @Test def testTwoPartialMatchAtEndNonResume1b = {
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
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb)
    assertEquals(SearchResult.PartialMatch, state1)
    assertEquals("abcdef:", result1)
    assertEquals(7, endPos1)
    assertEquals(7, endPosDelim1)

    // STR1 + STR2 >>>>>>>>>>>>>>>>>>>>>>>>
    cb = CharBuffer.allocate(str1.length() + str2.length() + 1)
    cb.put(str1 + str2)
    cb.flip()

    // Round 1 - Expect: FullMatch, abcdef:, 7, 9
    val (state2, result2, endPos2, endPosDelim2, _) = ds.search(cb)
    assertEquals(SearchResult.FullMatch, state2)
    assertEquals("abcdef:", result2)
    assertEquals(7, endPos2)
    assertEquals(9, endPosDelim2)
  }

  @Test def testUnrecognizedCharClass = {
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter(",%WSP%WSP;")
    ds.addDelimiter("%WSP,;")
    assert(ds.delimiters.length == 2) // Verified delimiter was added

    val d0 = ds.delimiters(0)
    val d1 = ds.delimiters(1)

    assertEquals(6, d0.delimBuf.length)
    assertEquals(6, d1.delimBuf.length)
  }

  @Test def testCorrectParsingMultipleSequence = {
    //val data: String = "abcde|fghij|klmno::pqrst|uvwzy|z]"

    // The purpose of this test is to verify that :: is found before | here
    // as this data represents two sequences terminated by either :: or ].
    val data: String = "klmno::pqrst|uvwzy|z]"
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger
    assertEquals(0, ds.delimiters.length) // Verified contained no delimiters
    ds.addDelimiter("|")
    ds.addDelimiter("::")
    ds.addDelimiter(";")
    ds.addDelimiter("]")
    assert(ds.delimiters.length == 4) // Verified delimiter was added  

    var cb: CharBuffer = CharBuffer.allocate(data.length() + 1)
    cb.put(data)
    cb.flip()

    val (state, result, endPos, endPosDelim, _) = ds.search(cb)

    assertEquals("klmno", result)
  }

  @Test def testComputeEscapeCharacterListSameChar = {
    val no_escapes: String = "a,b,c,d,e,f"
    val escapes: String = "a/,b,c/,d,e/,f" // results in escapes affecting char pos 2, 7, 12
    val escapeEscapes: String = "a//,b,c//,d,e/,f" // results in an escape affecting char pos 14
    val tricky1: String = "a///,b,c" // results in an escape at char pos 3 affecting char pos 4
    val tricky2: String = "a////,b,c" // results in no escapes

    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val cb0: CharBuffer = CharBuffer.allocate(no_escapes.length() + 1)
    cb0.put(no_escapes)
    cb0.flip()

    val cb1: CharBuffer = CharBuffer.allocate(escapes.length() + 1)
    cb1.put(escapes)
    cb1.flip()

    val cb2: CharBuffer = CharBuffer.allocate(escapeEscapes.length() + 1)
    cb2.put(escapeEscapes)
    cb2.flip()

    val cb3: CharBuffer = CharBuffer.allocate(tricky1.length() + 1)
    cb3.put(tricky1)
    cb3.flip()

    val cb4: CharBuffer = CharBuffer.allocate(tricky2.length() + 1)
    cb4.put(tricky2)
    cb4.flip()
    
    ds.setEscapeScheme(EscapeSchemeKind.Character, "/", "/", "", "")

    val res0 = ds.getEscapeCharacterList(cb0)
    assertEquals(List.empty, res0)

    val res1 = ds.getEscapeCharacterList(cb1)
    assertEquals(List(1, 6, 11), res1)

    // getEscapeEscapeCharacterList returns Tuple(ListA, ListB)
    //	Where
    //		A is the list of escapeEscapeCharacters
    //		B is the list of escaped escapeEscapeCharacters
    //
    // Here we should have the second list populated because we are attempting
    // to escape a character that matches both the escapeCharacter and the
    // escapeEscapeCharacter
    val res2_0 = ds.getEscapeEscapeCharacterList(cb2)
    assertEquals((List(13), List((1 -> 2), (7 -> 8))), res2_0)

    val res2_1 = ds.getEscapeCharacterList(cb2)
    assertEquals(List(13), res2_1)

    val res3 = ds.getEscapeCharacterList(cb3)
    assertEquals(List(3), res3)

    val res4 = ds.getEscapeCharacterList(cb4)
    assertEquals(List.empty, res4)
  }

  @Test def testComputeEscapeCharacterListDiffChar = {
    val no_escapes: String = "a,b,c,d,e,f"
    val escapes: String = "a/,b,c/,d,e/,f" // results in escapes affecting char pos 2, 7, 12
    val escapeEscapes: String = "a:/,b,c:/,d,e/,f" // results in an escape affecting char pos 14
    val tricky1: String = "a::/,b,c" // results in an escape at char pos 3 affecting char pos 4
    val tricky2: String = "a:/:/,b,c" // results in no escapes

    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val cb0: CharBuffer = CharBuffer.allocate(no_escapes.length() + 1)
    cb0.put(no_escapes)
    cb0.flip()

    val cb1: CharBuffer = CharBuffer.allocate(escapes.length() + 1)
    cb1.put(escapes)
    cb1.flip()

    val cb2: CharBuffer = CharBuffer.allocate(escapeEscapes.length() + 1)
    cb2.put(escapeEscapes)
    cb2.flip()

    val cb3: CharBuffer = CharBuffer.allocate(tricky1.length() + 1)
    cb3.put(tricky1)
    cb3.flip()

    val cb4: CharBuffer = CharBuffer.allocate(tricky2.length() + 1)
    cb4.put(tricky2)
    cb4.flip()

    ds.setEscapeScheme(EscapeSchemeKind.Character, "/", ":", "", "")

    val res0 = ds.getEscapeCharacterList(cb0)
    assertEquals(List.empty, res0)

    val res1 = ds.getEscapeCharacterList(cb1)
    assertEquals(List(1, 6, 11), res1)

    // getEscapeEscapeCharacterList returns Tuple(ListA, ListB)
    //	Where
    //		A is the list of escapeEscapeCharacters
    //		B is the list of escaped escapeEscapeCharacters
    //
    // Here the second list is empty because the escapeCharacter and
    // escapeEscapeCharacter are different
    val res2_0 = ds.getEscapeEscapeCharacterList(cb2)
    assertEquals((List(1, 7), List.empty), res2_0)

    val res2_1 = ds.getEscapeCharacterList(cb2)
    assertEquals(List(13), res2_1)

    val res3 = ds.getEscapeCharacterList(cb3)
    assertEquals(List(3), res3)

    val res4 = ds.getEscapeCharacterList(cb4)
    assertEquals(List.empty, res4)
  }

  @Test def testComputeEscapeBlockStartList = {
    val no_escapes: String = "a,b,c,d,e,f"
    val escapes: String = "text/*more*/text"

    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val cb0: CharBuffer = CharBuffer.allocate(no_escapes.length() + 1)
    cb0.put(no_escapes)
    cb0.flip()

    val cb1: CharBuffer = CharBuffer.allocate(escapes.length() + 1)
    cb1.put(escapes)
    cb1.flip()

    ds.setEscapeScheme(EscapeSchemeKind.Block, "/", ":", "/*", "*/")

    val res0 = ds.getEscapeBlockStartList(cb0)
    // println(res0)
    assertEquals(List.empty, res0)

    val res1 = ds.getEscapeBlockStartList(cb1)
    // println(res1)
    assertEquals(List((4, 5)), res1)
  }

  @Test def testComputeEscapeBlockEndList = {
    val no_escapes: String = "a,b,c,d,e,f"
    val escapes: String = "text/*more*/text"
    val escapes2: String = "text\"more\"text\"!"

    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val cb0: CharBuffer = CharBuffer.allocate(no_escapes.length() + 1)
    cb0.put(no_escapes)
    cb0.flip()

    val cb1: CharBuffer = CharBuffer.allocate(escapes.length() + 1)
    cb1.put(escapes)
    cb1.flip()

    val cb2: CharBuffer = CharBuffer.allocate(escapes2.length() + 1)
    cb2.put(escapes2)
    cb2.flip()

    ds.setEscapeScheme(EscapeSchemeKind.Block, "/", ":", "/*", "*/")

    val res0 = ds.getEscapeBlockEndList(cb0)
    assertEquals(List.empty, res0)

    val res1 = ds.getEscapeBlockEndList(cb1)
    assertEquals(List((10, 11)), res1)

    ds.setEscapeScheme(EscapeSchemeKind.Block, "/", ":", "\"", "\"")

    val res2 = ds.getEscapeBlockEndList(cb2)
    assertEquals(List((4, 4), (9, 9), (14, 14)), res2)

  }

  @Test def testComputeEscapeBlockListDiffStartEndChars = {
    val no_escapes: String = "a,b,c,d,e,f"
    val block: String = "text/*more*/text"
    val block2: String = "text/*mo/*re*/text"
    val block3: String = "text/*mo/*r*/e*/text"
    val block4: String = "text/*moretext"
    val block5: String = "text/*more*/text/*"

    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val cb0: CharBuffer = CharBuffer.allocate(no_escapes.length() + 1)
    cb0.put(no_escapes)
    cb0.flip()

    val cb1: CharBuffer = CharBuffer.allocate(block.length() + 1)
    cb1.put(block)
    cb1.flip()

    val cb2: CharBuffer = CharBuffer.allocate(block2.length() + 1)
    cb2.put(block2)
    cb2.flip()

    val cb3: CharBuffer = CharBuffer.allocate(block3.length() + 1)
    cb3.put(block3)
    cb3.flip()

    val cb4: CharBuffer = CharBuffer.allocate(block4.length() + 1)
    cb4.put(block4)
    cb4.flip()

    val cb5: CharBuffer = CharBuffer.allocate(block5.length() + 1)
    cb5.put(block5)
    cb5.flip()

    ds.setEscapeScheme(EscapeSchemeKind.Block, "/", ":", "/*", "*/")

    val res0 = ds.getEscapeBlocks(cb0)
    assertEquals(List.empty, res0)

    val res1 = ds.getEscapeBlocks(cb1)
    assertEquals(List((4, 11)), res1)

    val res2 = ds.getEscapeBlocks(cb2)
    assertEquals(List((4, 13)), res2)

    val res3 = ds.getEscapeBlocks(cb3)
    assertEquals(List((4, 12)), res3)

    val res4 = ds.getEscapeBlocks(cb4)
    assertEquals(List((4, -1)), res4)

    val res5 = ds.getEscapeBlocks(cb5)
    assertEquals(List((4, 11), (16, -1)), res5)
  }

  @Test def testComputeEscapeBlockListSameStartEndChars = {
    val no_escapes: String = "a,b,c,d,e,f"
    val block: String = "text\"more\"text"
    val block2: String = "text\"mo\"re\"text"
    val block3: String = "text\"mo\"r\"e\"text"
    val block4: String = "text\"moretext"
    val block5: String = "text\"more\"text\""

    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val cb0: CharBuffer = CharBuffer.allocate(no_escapes.length() + 1)
    cb0.put(no_escapes)
    cb0.flip()

    val cb1: CharBuffer = CharBuffer.allocate(block.length() + 1)
    cb1.put(block)
    cb1.flip()

    val cb2: CharBuffer = CharBuffer.allocate(block2.length() + 1)
    cb2.put(block2)
    cb2.flip()

    val cb3: CharBuffer = CharBuffer.allocate(block3.length() + 1)
    cb3.put(block3)
    cb3.flip()

    val cb4: CharBuffer = CharBuffer.allocate(block4.length() + 1)
    cb4.put(block4)
    cb4.flip()

    val cb5: CharBuffer = CharBuffer.allocate(block5.length() + 1)
    cb5.put(block5)
    cb5.flip()

    ds.setEscapeScheme(EscapeSchemeKind.Block, "/", ":", "\"", "\"")

    val res0 = ds.getEscapeBlocks(cb0)
    assertEquals(List.empty, res0)

    val res1 = ds.getEscapeBlocks(cb1)
    assertEquals(List((4, 9)), res1)

    val res2 = ds.getEscapeBlocks(cb2)
    assertEquals(List((4, 7), (10, -1)), res2)

    val res3 = ds.getEscapeBlocks(cb3)
    assertEquals(List((4, 7), (9, 11)), res3)

    val res4 = ds.getEscapeBlocks(cb4)
    assertEquals(List((4, -1)), res4)

    val res5 = ds.getEscapeBlocks(cb5)
    assertEquals(List((4, 9), (14, -1)), res5)
  }
  
  @Test def testComputeEscapeBlockListEscapes = {
    val block: String = "text\"mo:\"re\"text"
      
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val cb0: CharBuffer = CharBuffer.allocate(block.length() + 1)
    cb0.put(block)
    cb0.flip()

    ds.setEscapeScheme(EscapeSchemeKind.Block, "/", ":", "\"", "\"")

    val res0 = ds.getEscapeBlocks(cb0)
    assertEquals(List((4,11)), res0)
  }
  
  @Test def testSearchEscapeCharacter = {
    val block: String = "text/,more,text"
      
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val cb0: CharBuffer = CharBuffer.allocate(block.length() + 1)
    cb0.put(block)
    cb0.flip()

    ds.setEscapeScheme(EscapeSchemeKind.Character, "/", ":", "", "")

    val res0 = ds.getEscapeCharacterList(cb0)
    // println("res0 " + res0)
    assertEquals(List(4), res0)
    
    ds.addDelimiter(",")
    
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb0)
    assertEquals("text/,more", result1)
  }
  
  @Test def testSearchEscapeBlock = {
    val block: String = "text/*,more,*/text,yay"
      
    val ds: stringsearch.DelimSearcherV3.DelimSearcher = new stringsearch.DelimSearcherV3.DelimSearcher with ConsoleLogger

    val cb0: CharBuffer = CharBuffer.allocate(block.length() + 1)
    cb0.put(block)
    cb0.flip()

    ds.setEscapeScheme(EscapeSchemeKind.Block, "/", ":", "/*", "*/")

    val res0 = ds.getEscapeBlocks(cb0)
    // println("res0 " + res0)
    assertEquals(List((4,13)), res0)
    
    ds.addDelimiter(",")
    
    val (state1, result1, endPos1, endPosDelim1, _) = ds.search(cb0)
    assertEquals("text/*,more,*/text", result1)
  }

}
