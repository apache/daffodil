package daffodil.processors.input


import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

class TestRegex extends JUnit3Suite {
  
def testRegexToMatchOneDelimiterWithEscapeChars() {

    def tester(escapeEscape : String, escape : String, delim : String) = {
      val str = """(.*)(?=(?:(?!%1$s)(?!%2$s))(?:%3$s))(%3$s)(.*)"""
      val ContentPattern = str.format(escapeEscape, escape, delim).r
      println("Pattern = " + ContentPattern.pattern)

      /**
       * Really really we want to use the java APIs that let us construct a matcher from
       * a pattern (which 'compiles' the pattern), then use that over and over.
       *
       * This is really just about figuring out the right regex, and putting
       * groups inside it that let us get at what we need.
       */
      def test(x : String) = x match {
        case ContentPattern(before, delim, after) => {
          println("'%s' parsed to b = '%s', d = '%s', a = '%s'".
            format(x, before, delim, after))
          Some((before, delim, after))
        }
        case z => { println("no match: " + z); None }
      }
      test _
    }

    val test = tester("""S""", """E""", """\_*D\_*""") // put any regex in there for the delimiter

    assertEquals(Some(("before", "D", "after")), test("beforeDafter"))
    assertEquals(Some(("before_", "D_", "after")), test("before_D_after"))
    assertEquals(Some(("beforeSE__", "D_", "after")), test("beforeSE__D_after"))
    assertEquals(Some(("beforeEDstillBefore", "D", "after")), test("beforeEDstillBeforeDafter"))

    // just pretend RN is a CRLF, and N is a NL, and _ is a whitespace.
    val test2 = tester("""S""", """E""", """RN|\_*N\_*""") // alternatives still cause issues with longest match

    assertEquals(Some(("beforeR", "N", "after")), test2("beforeRNafter")) // see this is shortest match!
    assertEquals(Some(("beforeERNstillBefore__", "N_", "after")), test2("beforeERNstillBefore__N_after"))
  }

def testRegexToMatchOneDelimiterWithBlockEscapes() {

    def tester(bStart : String, bEnd : String, escapeEscape : String, escape : String, delim : String) = {
      val str = """(?:(?:(?!%3$s)(?!%4$s))(?:%1$s(.*)(?=(?:(?!%3$s)(?!%4$s))(?:%2$s))%2$s)|(?:(?!%1$s)(.*)))(?=(?:(?!%3$s)(?!%4$s))(?:%5$s))(%5$s)(.*)"""
      val ContentPattern = str.format(bStart, bEnd, escapeEscape, escape, delim).r
      println("Pattern = " + ContentPattern.pattern)
      def test(x : String) = x match {
        case ContentPattern(blocked, nonBlocked, delim, after) => {
          println("'%s' parsed to blocked = '%s', nonBlocked = '%s', d = '%s', a = '%s'".
            format(x, blocked, nonBlocked, delim, after))
          Some((blocked, nonBlocked, delim, after))
        }
        case z => { println("no match: " + z); None }
      }
      test _
    }

    val test = tester("""T""", """N""", """S""", """E""", """D""") 

    // This particular regex lets you escape either the blockstart or blockend, or the delimiter
    
    // no blockstart/end
    assertEquals(Some((null, "before", "D", "after")), test("beforeDafter"))
    
    // no blockstart/end, but escape the delimiter
    assertEquals(Some((null, "beforeEDstillBefore", "D", "after")), test("beforeEDstillBeforeDafter"))
    // TODO FIXME - in the above result, the 'E' shouldn't be there as it escapes the delimiter D
    // thereby making the D ordinary payload. 
    
    // with blockstart/end
    assertEquals(Some(("beforeDstillBefore", null, "D", "after")), test("TbeforeDstillBeforeNDafter"))
    
    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(Some(("beforeEDstillBeforeSEDstillBefore", null, "D", "after")), test("TbeforeEDstillBeforeSEDstillBeforeNDafter"))
    
    // with blockstart/end, escape the first block end
    assertEquals(Some(("beforeDstillBeforeENstillBefore", null, "D", "after")), test("TbeforeDstillBeforeENstillBeforeNDafter"))
    // TODO FIXME - in the above the 'E' shouldn't be there, as it escapes the first 'N' 
    
    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(Some(("beforeDstillBeforeTstillBeforeSE", null, "D", "after")), test("TbeforeDstillBeforeTstillBeforeSENDafter"))
    // TODO FIXME - in the above result, the 'S' second char from the end
    // shouldn't be there. It escaped the 'E' escape, so that isn't 
    // an escape character, thereby activating the block end 'N'
    
    // Note: Making the regex even more complex so as to get the active escape characters (either 'S' or 'E')
    // out of the result is probably unwise. 
    // Better to check for this situation exactly with a special check. 
    
    // with blockstart, but escape it so it's not really a block.
    assertEquals(Some((null, "ETbefore", "D", "afterNstillafter")), test("ETbeforeDafterNstillafter"))
    // TODO FIXME - the 'E' shouldn't be there. Active escape character.
  }


}