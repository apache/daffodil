package daffodil.processors.input

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import scala.util.parsing.combinator._
import java.io.StringReader
import org.junit.Test
import daffodil.exceptions.Assert

class TestRegex extends JUnitSuite with RegexParsers {

  /**
   * Scala combinator parsers can do longest match via the '|||' combinator
   * Let's be sure we get longest match.
   */

  override def skipWhitespace = skipWS
  var skipWS = false // assign this to turn on/off whitespace skipping.

  /**
   * First let's avoid everything about whitespace and use _ (underscores)
   * instead.
   */

  def delim0 = """_*+;_*+""" // greedy whitespace consumption

  def anyUntil(delim : String) = {
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

  @Test def testParsingDelims() {
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

  @Test def testRegexNoWSLongestMatch() {
    skipWS = true // this is the default setting for scala comb. parsers, but we have no ws so it doesn't matter really.
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
  def endWS = """\s*end\s*""".r // note use \s for whiteSpace. \w is "word character" not whitespace
  def endingWS = """\s*ending\s*""".r
  // def delimWS = (log(endWS)("end") ||| log(endingWS)("ending"))
  def delimWS = (endWS ||| endingWS)

  def t2 = delimWS ~! "more"

  val rdr2 = new StringReader("   ending   more")

  lazy val parsed2 = parseAll(t2, rdr2)

  @Test def testRegexWSLongestMatch() {
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
  @Test def testRegexToMatchOneDelimiterWithEscapeChars() {

    /**
     * tester regexps and postprocessing algorithms are different
     * depending on whether you have escapeBlock or escapeCharacter type
     * escaping.
     */
    def tester(escapeEscape : String, escape : String, delim : String, padChar : String) = {
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
          """(?<!(?<!%1$s)%2$s)(%3$s)""" + // unescaped delimiter [delim]
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
      def test(x : String) = x match {
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
      def removeActiveEscapes(str : String) : String = {
        // if ends with SE (an escaped escape), then change to end with just E
        // because the S was active.
        val str1 = if (str.endsWith(escapeEscape + escape)) {
          str.slice(0, str.length() - 2) + escape
        } else str
        // if contains ED, replace with just D
        val str2 = removeActiveEscapes1(str1)
        str2
      }

      def removeActiveEscapes1(str : String) : String = {
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
    val test = tester("""S""", """E""", """\_*D\_*""", """P""") // put any regex in there for the delimiter

    assertEquals(Some(("before", "D", "after")), test("PPPbeforePPPDafter"))

    // Notice how (.*?) is non-greedy matching. SO we don't get "before_", we get "before" 
    assertEquals(Some(("before", "_D_", "after")), test("PPPbefore_D_after"))

    assertEquals(Some(("beforeE", "__D_", "after")), test("PPPbeforeSE__D_after"))

    assertEquals(Some(("beforeDstillBefore", "D", "after")), test("PPPbeforeEDstillBeforePPPDafter"))

    // In the test below. Note that S does NOT escape D.
    assertEquals(Some(("beforeS", "D", "after")), test("PPPbeforeSDafter"))

    // In the test below note that SE is just data because D doesn't follow E
    assertEquals(Some(("beforeSEstillBefore", "D", "after")), test("PPPbeforeSEstillBeforePPPDafter"))

    // to keep out of slash hell, just pretend RN is a \r\n, and N is a \n, and _ is a whitespace.
    //                 escEsc     esc          delim           padChar
    val test2 = tester("""S""", """E""", """(?>RN|\_*N\_*)""", """P""")
    // in the above note careful use of ?> in delim, which is disjunction with no backtracking.

    assertEquals(Some(("before", "RN", "after")), test2("PPPbeforePPPRNafter")) // works because (.*?) is lazy not greedy

    assertEquals(Some(("beforeRNstillBefore", "__N_", "after")), test2("PPPbeforeRENstillBeforePPP__N_after"))
  }

  /**
   * We assume here that a blockStart must be the very first thing in the string, and a blockEnd the
   * very last before the delimiter. For example, if , is delimiter, and [ and ] are block start and end:
   * <pre>
   * [aaa,bbb],...
   * </pre>is allowed but not
   * <pre> aaa[,]bbb,...
   * </pre>
   *
   * We also assume that you can escape the block start or block end.
   * Or, if you choose not to use the block escapes, you can still escape the delimiter.
   * <p>
   * Also illustrates how one would add padChar absorbing into the mix on the left, right, or both
   *
   */
  @Test def testRegexToMatchOneDelimiterWithBlockEscapesAndPaddingCharacters() {

    def tester(bStart : String, bEnd : String, escapeEscape : String, escape : String, delim : String, padChar : String) = {
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

      def test(x : String) = x match {
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
      def removeActiveEscapesUnblocked(str : String) : String = {
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

      def removeActiveEscapesUnblocked1(str : String) : String = {
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
      def removeActiveEscapesBlocked(str : String) : String = {
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

      def removeActiveEscapesBlocked1(str : String) : String = {
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
    assertEquals(Some(("beforeDstillBefore", "D", "after")), test("PPPbeforeEDstillBeforePPPDafter"))

    // with blockstart/end
    assertEquals(Some(("beforeDstillBefore", "D", "after")), test("PPPTbeforeDstillBeforeNPPPDafter"))

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")), test("PPPTbeforeEDstillBeforeSEDstillBeforeNPPPDafter"))
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(Some(("beforeDstillBeforeNstillBefore", "D", "after")), test("PPPTbeforeDstillBeforeENstillBeforeNPPPDafter"))

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(Some(("beforeDstillBeforeTstillBeforeE", "D", "after")), test("PPPTbeforeDstillBeforeTstillBeforeSENPPPDafter"))

    // with blockstart, but escape it so it's not really a block.
    assertEquals(Some(("Tbefore", "D", "afterNstillafter")), test("PPPETbeforePPPDafterNstillafter"))
  }

}