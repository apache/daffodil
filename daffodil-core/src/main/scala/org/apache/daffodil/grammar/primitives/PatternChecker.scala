/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.exceptions.SavesErrorsAndWarnings
import java.util.regex.Pattern
import org.apache.daffodil.util.Misc
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
        context.SDE("Invalid regular expression pattern '%s'.\nReason: %s.", pattern, Misc.getSomeMessage(e).get)
      }
    }
  }
}
