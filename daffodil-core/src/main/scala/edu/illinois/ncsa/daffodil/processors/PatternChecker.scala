package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils
import java.util.regex.PatternSyntaxException

object PatternChecker {
  def checkPattern(pattern: String,
    context: {
      def SDE(string: String, others: Any*): Nothing
      def SDW(string: String, others: Any*): Unit
    }): Unit = {
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