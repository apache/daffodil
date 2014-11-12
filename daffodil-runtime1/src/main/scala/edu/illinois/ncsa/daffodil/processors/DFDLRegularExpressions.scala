package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextStringJustification

object DFDLRegularExpressions {

  def getEscapeRegEx(escape: String, escapeEscape: String, delim: String,
    dotMatchesAll: Boolean = true) = {
    val str =
      (if (dotMatchesAll) """(?s)""" else """""") +
        """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
        """(?:(?<!(?:(?<!%1$s)%2$s))(%3$s))""" + // unescaped delimiter [delim]
        """(.*)""" // trailing stuff [after]
    val contentPattern = str.format(escapeEscape, escape, delim).r
    contentPattern
  }

  def getEscapeRegExWithPadding(escape: String, escapeEscape: String, delim: String,
    padChar: String, justification: TextStringJustification,
    dotMatchesAll: Boolean = true) = {
    val str =
      (if (dotMatchesAll) """(?s)""" else """""") +
        (if (justification == TextStringJustification.Right
          || justification == TextStringJustification.Center)
          """(?:%4$s*)""" // trim pad char. aka right or center justified.
        else """""") +
        """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
        (if (justification == TextStringJustification.Left
          || justification == TextStringJustification.Center)
          """(?:%4$s*)""" // trim pad char. aka left or center justified.
        else """""") +
        """(?:(?<!(?:(?<!%1$s)%2$s))(%3$s))""" + // unescaped delimiter [delim]
        """(.*)""" // trailing stuff [after]
    val contentPattern = str.format(escapeEscape, escape, delim, padChar).r
    contentPattern
  }

  def getSameEscapeRegEx(escape: String, delim: String,
    dotMatchesAll: Boolean = true) = {
    val str =
      (if (dotMatchesAll) """(?s)""" else """""") +
        """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
        """(?:""" +
        """(?<!%1$s)((?:%1$s%1$s)*)(%2$s)""" + // unescaped delimiter [delim] which is delim preceded by NOT an odd number of escapes.
        """)""" +
        """(.*)""" // trailing stuff [after]
    val contentPattern = str.format(escape, delim).r
    contentPattern
  }

  def getSameEscapeRegExWithPadding(escape: String, delim: String,
    padChar: String, justification: TextStringJustification,
    dotMatchesAll: Boolean = true) = {
    val str =
      (if (dotMatchesAll) """(?s)""" else """""") +
        (if (justification == TextStringJustification.Right
          || justification == TextStringJustification.Center)
          """(?<!%1$s)((?:%1$s%1$s)*)(?:%3$s*)""" // trim unescaped pad char. aka right or center justified.
        else """""") +
        """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
        """(?:""" +
        (if (justification == TextStringJustification.Left
          || justification == TextStringJustification.Center)
          """(?<!%1$s)((?:%1$s%1$s)*)(?:%3$s*)(?<!%1$s)(%2$s)""" + // trim unescaped pad char. aka left or center justified. Then delim. (and if pad is size zero, then delimiter must be unescaped.)
          """|""" // OR
        else """""") +
        """(?<!%1$s)((?:%1$s%1$s)*)(%2$s)""" + // unescaped delimiter [delim] which is delim preceded by NOT an odd number of escapes.
        """)""" +
        """(.*)""" // trailing stuff [after]
    val contentPattern = str.format(escape, delim, padChar).r
    //System.err.println(contentPattern)
    contentPattern
  }

  def getEscapeBlockRegEx(bStart: String, bEnd: String, escapeEscape: String,
    escape: String, delim: String, dotMatchesAll: Boolean = true) = {
    // Based upon assumption that String.format ignores extra arguments for which there
    // are no specifiers
    //
    val str =
      (if (dotMatchesAll) """(?s)""" else """""") +
        """(?>""" +
        // First alternative is if the start is an unescaped Block start. In that case you must
        // have an unescaped block end followed directly by an unescaped delimiter as the termination.
        """(?<!(?<!%1$s)%2$s)%4$s""" + // unescaped block start
        """(.*)""" + // captured content (before)
        """(?<!(?<!%1$s)%2$s)%5$s""" + // unescaped block end
        """(?<!(?<!%1$s)%2$s)(%3$s)""" + // unescaped delimiter (delim)
        """(.*)""" + // trailing stuff (after)
        """|""" +
        // Second alternative is if the start is NOT a block start, in which case we are looking 
        // for an unescaped delimiter at the end.
        """(?<!%4$s)""" + // not a block start
        """(.*?)""" + // lazy content so it won't absorb the padChars (before2)
        """(?<!(?<!%1$s)%2$s)(%3$s)""" + // unescaped delimiter (delim2)
        """(.*)""" + // trailing stuff (after2)
        """)"""
    val contentPattern = str.format(escapeEscape, escape, delim, bStart, bEnd).r
    contentPattern
  }

  def getEscapeBlockRegExWithPadding(bStart: String, bEnd: String, escapeEscape: String,
    escape: String, padChar: String, delim: String, justification: TextStringJustification,
    dotMatchesAll: Boolean = true) = {
    // Based upon assumption that String.format ignores extra arguments for which there
    // are no specifiers
    //
    val str =
      (if (dotMatchesAll) """(?s)""" else """""") +
        """(?>""" +
        // First alternative is if the start is an unescaped Block start. In that case you must
        // have an unescaped block end followed directly by an unescaped delimiter as the termination.
        (if (justification == TextStringJustification.Right
          || justification == TextStringJustification.Center)
          """(?:%6$s*)""" //+ // trim padChar off on the left. Aka right or center justified.
        else """""") +
        """(?<!(?<!%1$s)%2$s)%4$s""" + // unescaped block start
        """(.*)""" + // captured content (before)
        """(?<!(?<!%1$s)%2$s)%5$s""" + // unescaped block end
        (if (justification == TextStringJustification.Left
          || justification == TextStringJustification.Center)
          """(?:%6$s*)""" //+ // trim padChar off on the right. aka left or center justified.
        else """""") +
        """(?<!(?<!%1$s)%2$s)(%3$s)""" + // unescaped delimiter (delim)
        """(.*)""" + // trailing stuff (after)
        """|""" +
        // Second alternative is if the start is NOT a block start, in which case we are looking 
        // for an unescaped delimiter at the end.
        (if (justification == TextStringJustification.Right
          || justification == TextStringJustification.Center)
          """(?:%6$s*)""" //+ // trim padChar off. Aka right or center justified.
        else """""") +
        """(?<!%4$s)""" + // not a block start
        """(.*?)""" + // lazy content so it won't absorb the padChars (before2)
        (if (justification == TextStringJustification.Left
          || justification == TextStringJustification.Center)
          """(?:%6$s*)""" //+ // trim padChar off. aka left or center justified.
        else """""") +
        """(?<!(?<!%1$s)%2$s)(%3$s)""" + // unescaped delimiter (delim2)
        """(.*)""" + // trailing stuff (after2)
        """)"""
    val contentPattern = str.format(escapeEscape, escape, delim, bStart, bEnd, padChar).r
    contentPattern
  }
}