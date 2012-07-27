package daffodil.processors.input

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

class TestRegex extends JUnit3Suite {

  def testRegexToMatchOneDelimiterWithEscapeChars() {

    /**
     * tester regexps and postprocessing algorithms are different
     * depending on whether you have escapeBlock or escapeCharacter type
     * escaping.
     */
    def tester(escapeEscape : String, escape : String, delim : String) = {
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
      val str = """(.*?)(?=(?<!(?<!%1$s)%2$s)%3$s)(%3$s)(.*)"""
      val ContentPattern = str.format(escapeEscape, escape, delim).r
      println("Pattern = " + ContentPattern.pattern)

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
          println("'%s' parsed to b = '%s', d = '%s', a = '%s'".
            format(x, before, delim, after))
          val before1 = removeActiveEscapes(before)
          Some((before1, delim, after))
        }
        case z => { println("no match: " + z); None }
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

    //                escEsc     esc       delim
    val test = tester("""S""", """E""", """\_*D\_*""") // put any regex in there for the delimiter

    assertEquals(Some(("before", "D", "after")), test("beforeDafter"))

    // Notice how (.*?) is non-greedy matching. SO we don't get "before_", we get "before" 
    assertEquals(Some(("before", "_D_", "after")), test("before_D_after"))

    assertEquals(Some(("beforeE", "__D_", "after")), test("beforeSE__D_after"))

    assertEquals(Some(("beforeDstillBefore", "D", "after")), test("beforeEDstillBeforeDafter"))

    // In the test below. Note that S does NOT escape D.
    assertEquals(Some(("beforeS", "D", "after")), test("beforeSDafter"))

    // In the test below note that SE is just data because D doesn't follow E
    assertEquals(Some(("beforeSEstillBefore", "D", "after")), test("beforeSEstillBeforeDafter"))

    // to keep out of slash hell, just pretend RN is a \r\n, and N is a \n, and _ is a whitespace.
    //                 escEsc     esc          delim
    val test2 = tester("""S""", """E""", """(?>RN|\_*N\_*)""")
    // in the above note careful use of ?> in delim, which is disjunction with no backtracking.

    assertEquals(Some(("before", "RN", "after")), test2("beforeRNafter")) // works because (.*?) is lazy not greedy

    assertEquals(Some(("beforeRNstillBefore", "__N_", "after")), test2("beforeRENstillBefore__N_after"))
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
   *
   */
  def testRegexToMatchOneDelimiterWithBlockEscapes() {

    def tester(bStart : String, bEnd : String, escapeEscape : String, escape : String, delim : String) = {
      val str = """(?>""" +
        // First alternative is if the start is an unescaped Block start. In that case you must
        // have an unescaped block end followed directly by the delimiter as the termination. 
        // The delimiter may not be escaped in this case.
        """(?<!(?<!%1$s)%2$s)(?:%4$s(.*)(?=(?<!(?<!%1$s)?!%2$s)%5$s)%5$s)(%3$s)(.*)""" +
        """|""" +
        // Second alternative is if the start is NOT a block start, in which case we are looking 
        // for an unescaped delimiter at the end.
        """(?<!%4$s)(.*)(?=(?:(?!%1$s)(?!%2$s))(?:%3$s))(%3$s)(.*)""" +
        """)"""

      val ContentPattern = str.format(escapeEscape, escape, delim, bStart, bEnd).r
      println("Pattern = " + ContentPattern.pattern)

      // used to cleanup active escape characters 
      val EDSplit = """(.*)%1$s(%2$s)(.*)""".format(escape, delim).r
      val ENSplit = """(.*)%1$s(%2$s)(.*)""".format(escape, bEnd).r

      def test(x : String) = x match {
        case ContentPattern(before, delim, after, null, null, null) => {
          val before1 = removeActiveEscapesBlocked(before)
          println("'%s' parsed to b = '%s', d = '%s', a = '%s'".
            format(x, before1, delim, after))
          Some((before1, delim, after))
        }
        case ContentPattern(null, null, null, before, delim, after) => {
          val before1 = removeActiveEscapesUnblocked(before)
          println("'%s' parsed to b = '%s', d = '%s', a = '%s'".
            format(x, before1, delim, after))
          Some((before1, delim, after))
        }
        case z => { println("no match: " + z); None }
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

    //                blkStart blkEnd   escEsc   esc      delim
    val test = tester("""T""", """N""", """S""", """E""", """D""")

    // This particular regex lets you escape either the blockstart or blockend, or the delimiter

    // no blockstart/end
    assertEquals(Some(("before", "D", "after")), test("beforeDafter"))

    // no blockstart/end, but escape the delimiter
    assertEquals(Some(("beforeDstillBefore", "D", "after")), test("beforeEDstillBeforeDafter"))

    // with blockstart/end
    assertEquals(Some(("beforeDstillBefore", "D", "after")), test("TbeforeDstillBeforeNDafter"))

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")), test("TbeforeEDstillBeforeSEDstillBeforeNDafter"))
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(Some(("beforeDstillBeforeNstillBefore", "D", "after")), test("TbeforeDstillBeforeENstillBeforeNDafter"))

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(Some(("beforeDstillBeforeTstillBeforeE", "D", "after")), test("TbeforeDstillBeforeTstillBeforeSENDafter"))

    // with blockstart, but escape it so it's not really a block.
    assertEquals(Some(("Tbefore", "D", "afterNstillafter")), test("ETbeforeDafterNstillafter"))
  }

}