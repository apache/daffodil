package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.exceptions.SavesErrorsAndWarnings
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils
import java.util.regex.PatternSyntaxException

/**
 * The purpose of this checker is to examine a regex and look for a situation
 * one can run into if you try to use the free-form regex syntax, which allows
 * a regex to be spread out over multiple lines with comments intersperced.
 *
 * Trouble is, such regex must be wrapped with CDATA bracketing, otherwise it
 * will all end up on one line, and the comments are from a # to end of line, so
 * this breaks the regex, and very commonly the regex will begin with a comment
 * and so the effective regex is one that matches anything at all, but is
 * completely legal as a regex, which makes it very very painful to debug.
 */
object PatternChecker {
  def checkPattern(pattern: String,
    context: SavesErrorsAndWarnings): Unit = {
    try {
      val pat = Pattern.compile(pattern)
      val m1 = pat.matcher("")
      val m2 = pat.matcher("\uFFFE") // obscure enough? 
      if (m1.matches() && m2.lookingAt() && m2.group().length() == 0) {
        // the pattern will match with zero length, anything or nothing
        // This is a flawed pattern for an assert and dubious 
        // generally. The pattern should have to match something.
        val needCDATA =
          if (pattern.startsWith("(?x)") &&
            !pattern.contains("\n") &&
            pattern.contains("#")) {
            // it's free form regex notation
            // it's all on one line,
            // and it contains a comment (# to end of line)
            // Almost guaranteed you are missing a CDATA wrapper.
            "\nMissing <![CDATA[...]]> around the regular expression." +
              "\nThis is required for free-form regular expression syntax with comments."
          } else ""
        context.SDW("Regular expression pattern '%s'.\n" +
          "This pattern will match with zero length, so it can always match.%s", pattern, needCDATA)
      }
    } catch {
      case e: PatternSyntaxException => {
        println(e.getMessage)
        context.SDE("Invalid regular expression pattern '%s'.\nReason: %s.", pattern, DiagnosticUtils.getSomeMessage(e).get)
      }
    }
  }
}