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
package org.apache.daffodil.lib.util

/**
 * A abstract base for Remappers which convert strings.
 *
 * The public interface is just `def remap(s: String): String`.
 *
 * There are protected methods that implementations must provide.
 *
 * Contains shared implementation methods also.
 *
 * NOTE: This is inner loop stuff. Keep it and derived classes lean and fast.
 * Use a java-like coding style. While loops, not map/flatmap/etc. avoid tuples.
 */
trait CharacterSetRemapper {

  /**
   * Remaps characters. Provides the previous and following characters since some remappings
   * require this context for Surrogate Pairs, CRLF->LF etc.
   *
   * Plays a trick with negating the return value in order to avoid having to
   * return more than one value, which is potentially less efficient.
   *
   * @param prev The character prior to the one being considered. (Needed for surrogates)
   * @param curr The character under consideration for remapping.
   * @param next The next character afterwards. (Needed for surrogates and CRLF pairs)
   * @return The remapped character (as an Int) or that same remapped character Int
   *         value negated, which signals that curr+next was remapped to a single character.
   *         Such as is needed if CRLF is remapped to just LF.
   */
  protected def remap(prev: Char, curr: Char, next: Char): Int

  /**
   * Remaps the string. Returns the original string object if no remapping is required.
   *
   * Because of surrogate pairs, and the difference between 16-bit string codepoints
   * and real character codes, lots of things that traverse strings need
   * to consider either the codepoint after (if current is a leading surrogate)
   * or codepoint before (if current is a trailing surrogate).
   *
   * This is not the only kind of character set remapping. In particular this is
   * restricted to replace 1 character with 0 or 1 character in the remapped string.
   * Other character set remappers can convert 1 character into 0 to many characters
   * or even change from characters to bytes. Simple 1 to 1 remappings can be done
   * with just a map, and 1 to N remappings can be done with flatmap if no context is
   * needed for surrogates or CRLFs.
   *
   * See XMLUtils.walkUnicodeString for a more general kind of remapping that can
   * replace 1 character with N as well as being context sensitive about adjacent
   * characters before and after.
   *
   * This algorithm uses a StringBuilder which is not synchronized
   * so it is noticably faster than StringBuffer, and since the StringBuilder
   * is local to the function, we don't have to worry about any threading issues.
   * This makes for a noticeable speed increase.
   *
   * This remapper is called for every piece of string data, both when parsing
   * and when unparsing. Is very important for it to be high performance.
   * Hence, this very Java loop-oriented coding style,
   * avoiding map, or returning tuples or any other potentially inefficient scala-isms.
   */
  final def remap(s: String): String = {

    val len = s.length
    if (len == 0) return s

    // Use a java StringBuilder because it has an
    // append(charsequence, start end) method which lets us easily copy
    // a prefix of the string into the stringbuilder.
    // scala StringBuilder doesn't have this method.
    var sb: java.lang.StringBuilder = null // created only if remapping proves to be needed

    def isRemapNeeded = sb ne null
    var pos = 0;
    var prev = 0.toChar
    var curr = s(0).toChar
    var next = 0.toChar
    var newCurr: Int = 0 // positive normally, but will be negative if we're to skip a char

    while (pos < len) {
      next = if (pos + 1 < len) s(pos + 1) else 0.toChar
      //
      // sign of newCurr is negative if we're to skip 1 character
      // such as if the prior iteration collapsed a CRLF to just LF.
      //
      if (newCurr >= 0) {
        // don't skip any character
        newCurr = remap(prev, curr, next)
        if (!isRemapNeeded && newCurr != curr) {
          // we have just hit our first character that
          // needs remapping.
          // This block happens only once.
          sb = new java.lang.StringBuilder(s.length)
          sb.append(s, 0, pos)
          // Now we have a string builder, and can proceed as
          // if we always had one accumulating the characters
        }
        if (isRemapNeeded) {
          // something in the string needed remapping, so
          // now we have to always append characters to the
          // string builder.
          //
          // if newCurr is negative, it's still the replacement
          // remapped character code, just negated to indicate need to skip
          val c = (if (newCurr < 0) -newCurr else newCurr).toChar
          sb.append(c)
        }
      } else {
        // Skip a character
        newCurr = -newCurr // flip it so we only skip once
      }
      prev = curr
      curr = next
      pos += 1
    }

    val res = if (isRemapNeeded) sb.toString else s
    res
  }
}
