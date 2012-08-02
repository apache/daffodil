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
    val test2 = tester("""S""", """E""", """RN|\_*N\_*""") // alternatives without shared prefix are ok.

    assertEquals(Some(("beforeR", "N", "after")), test2("beforeRNafter"))
    assertEquals(Some(("beforeERNstillBefore__", "N_", "after")), test2("beforeERNstillBefore__N_after"))
  }


}