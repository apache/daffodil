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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.lib.schema.annotation.props.gen.TextStringJustification

object DFDLRegularExpressions {

  def getEscapeRegEx(
    escape: String,
    escapeEscape: String,
    delim: String,
    dotMatchesAll: Boolean = true
  ) = {
    val str =
      (if (dotMatchesAll) """(?s)""" else """""") +
        """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
        """(?:(?<!(?:(?<!%1$s)%2$s))(%3$s))""" + // unescaped delimiter [delim]
        """(.*)""" // trailing stuff [after]
    val contentPattern = str.format(escapeEscape, escape, delim).r
    contentPattern
  }

  def getEscapeRegExWithPadding(
    escape: String,
    escapeEscape: String,
    delim: String,
    padChar: String,
    justification: TextStringJustification,
    dotMatchesAll: Boolean = true
  ) = {
    val str =
      (if (dotMatchesAll) """(?s)""" else """""") +
        (if (
           justification == TextStringJustification.Right
           || justification == TextStringJustification.Center
         )
           """(?:%4$s*)""" // trim pad char. aka right or center justified.
         else """""") +
        """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
        (if (
           justification == TextStringJustification.Left
           || justification == TextStringJustification.Center
         )
           """(?:%4$s*)""" // trim pad char. aka left or center justified.
         else """""") +
        """(?:(?<!(?:(?<!%1$s)%2$s))(%3$s))""" + // unescaped delimiter [delim]
        """(.*)""" // trailing stuff [after]
    val contentPattern = str.format(escapeEscape, escape, delim, padChar).r
    contentPattern
  }

  def getSameEscapeRegEx(escape: String, delim: String, dotMatchesAll: Boolean = true) = {
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

  def getSameEscapeRegExWithPadding(
    escape: String,
    delim: String,
    padChar: String,
    justification: TextStringJustification,
    dotMatchesAll: Boolean = true
  ) = {
    val str =
      (if (dotMatchesAll) """(?s)""" else """""") +
        (if (
           justification == TextStringJustification.Right
           || justification == TextStringJustification.Center
         )
           """(?<!%1$s)((?:%1$s%1$s)*)(?:%3$s*)""" // trim unescaped pad char. aka right or center justified.
         else """""") +
        """(.*?)""" + // content [before]. lazy so it won't absorb pad characters
        """(?:""" +
        (if (
           justification == TextStringJustification.Left
           || justification == TextStringJustification.Center
         )
           """(?<!%1$s)((?:%1$s%1$s)*)(?:%3$s*)(?<!%1$s)(%2$s)""" + // trim unescaped pad char. aka left or center justified. Then delim. (and if pad is size zero, then delimiter must be unescaped.)
             """|""" // OR
         else """""") +
        """(?<!%1$s)((?:%1$s%1$s)*)(%2$s)""" + // unescaped delimiter [delim] which is delim preceded by NOT an odd number of escapes.
        """)""" +
        """(.*)""" // trailing stuff [after]
    val contentPattern = str.format(escape, delim, padChar).r
    contentPattern
  }

  def getEscapeBlockRegEx(
    bStart: String,
    bEnd: String,
    escapeEscape: String,
    escape: String,
    delim: String,
    dotMatchesAll: Boolean = true
  ) = {
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

  def getEscapeBlockRegExWithPadding(
    bStart: String,
    bEnd: String,
    escapeEscape: String,
    escape: String,
    padChar: String,
    delim: String,
    justification: TextStringJustification,
    dotMatchesAll: Boolean = true
  ) = {
    // Based upon assumption that String.format ignores extra arguments for which there
    // are no specifiers
    //
    val str =
      (if (dotMatchesAll) """(?s)""" else """""") +
        """(?>""" +
        // First alternative is if the start is an unescaped Block start. In that case you must
        // have an unescaped block end followed directly by an unescaped delimiter as the termination.
        (if (
           justification == TextStringJustification.Right
           || justification == TextStringJustification.Center
         )
           """(?:%6$s*)""" // + // trim padChar off on the left. Aka right or center justified.
         else """""") +
        """(?<!(?<!%1$s)%2$s)%4$s""" + // unescaped block start
        """(.*)""" + // captured content (before)
        """(?<!(?<!%1$s)%2$s)%5$s""" + // unescaped block end
        (if (
           justification == TextStringJustification.Left
           || justification == TextStringJustification.Center
         )
           """(?:%6$s*)""" // + // trim padChar off on the right. aka left or center justified.
         else """""") +
        """(?<!(?<!%1$s)%2$s)(%3$s)""" + // unescaped delimiter (delim)
        """(.*)""" + // trailing stuff (after)
        """|""" +
        // Second alternative is if the start is NOT a block start, in which case we are looking
        // for an unescaped delimiter at the end.
        (if (
           justification == TextStringJustification.Right
           || justification == TextStringJustification.Center
         )
           """(?:%6$s*)""" // + // trim padChar off. Aka right or center justified.
         else """""") +
        """(?<!%4$s)""" + // not a block start
        """(.*?)""" + // lazy content so it won't absorb the padChars (before2)
        (if (
           justification == TextStringJustification.Left
           || justification == TextStringJustification.Center
         )
           """(?:%6$s*)""" // + // trim padChar off. aka left or center justified.
         else """""") +
        """(?<!(?<!%1$s)%2$s)(%3$s)""" + // unescaped delimiter (delim2)
        """(.*)""" + // trailing stuff (after2)
        """)"""
    val contentPattern = str.format(escapeEscape, escape, delim, bStart, bEnd, padChar).r
    contentPattern
  }
}
