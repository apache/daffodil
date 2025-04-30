/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.grammar.primitives

import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException

import org.apache.daffodil.lib.exceptions.SavesErrorsAndWarnings
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.util.Misc

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
  def checkPattern(pattern: String, context: SavesErrorsAndWarnings): Unit = {
    try {
      val pat = Pattern.compile(pattern)
      val m1 = pat.matcher("")
      val m2 = pat.matcher("\uFFFE") // obscure enough?
      if (m1.matches() && m2.lookingAt() && m2.group().length() == 0) {
        // the pattern will match with zero length, anything or nothing
        // This is a flawed pattern for an assert and dubious
        // generally. The pattern should have to match something.
        val needCDATA =
          if (
            pattern.startsWith("(?x)") &&
            !pattern.contains("\n") &&
            pattern.contains("#")
          ) {
            // it's free form regex notation
            // it's all on one line,
            // and it contains a comment (# to end of line)
            // Almost guaranteed you are missing a CDATA wrapper.
            "\nMissing <![CDATA[...]]> around the regular expression." +
              "\nThis is required for free-form regular expression syntax with comments."
          } else ""
        context.SDW(
          WarnID.RegexPatternZeroLength,
          "Regular expression pattern '%s'.\n" +
            "This pattern will match with zero length, so it can always match.%s",
          pattern,
          needCDATA
        )
      }
    } catch {
      case e: PatternSyntaxException => {
        context.SDE(
          "Invalid regular expression pattern '%s'.\nReason: %s.",
          pattern,
          Misc.getSomeMessage(e).get
        )
      }
    }
  }
}
