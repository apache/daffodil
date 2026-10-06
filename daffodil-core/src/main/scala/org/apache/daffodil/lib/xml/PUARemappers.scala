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
package org.apache.daffodil.lib.xml

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.CharacterSetRemapper
import org.apache.daffodil.runtime1.infoset.InvalidInfosetException

/**
 * Represents backslash, CR, and LF characters as the two character sequences
 * \\, \r, and \n so that the text contains no line ending characters and the
 * conversion is exactly invertible.
 */
object LineEndingEscaper {

  private def isEscapable(c: Char): Boolean = c == '\\' || c == '\r' || c == '\n'

  def escape(s: String): String = {
    val len = s.length
    var pos = 0
    while (pos < len && !isEscapable(s.charAt(pos))) {
      pos += 1
    }
    if (pos == len) {
      s
    } else {
      val sb = new java.lang.StringBuilder(len)
      sb.append(s, 0, pos)
      while (pos < len) {
        val c = s.charAt(pos)
        c match {
          case '\\' => sb.append("\\\\")
          case '\r' => sb.append("\\r")
          case '\n' => sb.append("\\n")
          case _ => sb.append(c)
        }
        pos += 1
      }
      sb.toString
    }
  }

  /**
   * Inverse of escape. A backslash that is not followed by backslash, 'r', or 'n'
   * is invalid, as is a backslash at the end of the string.
   */
  def unescape(s: String): String = {
    val len = s.length
    var pos = s.indexOf('\\')
    if (pos < 0) {
      s
    } else {
      val sb = new java.lang.StringBuilder(len)
      sb.append(s, 0, pos)
      while (pos < len) {
        val c = s.charAt(pos)
        if (c == '\\') {
          if (pos + 1 >= len) {
            throw new InvalidInfosetException(
              "Line ending escape style: string ends with an incomplete backslash escape."
            )
          }
          s.charAt(pos + 1) match {
            case '\\' => sb.append('\\')
            case 'r' => sb.append('\r')
            case 'n' => sb.append('\n')
            case other =>
              throw new InvalidInfosetException(
                "Line ending escape style: invalid escape sequence '\\%c'.".format(other)
              )
          }
          pos += 2
        } else {
          sb.append(c)
          pos += 1
        }
      }
      sb.toString
    }
  }
}

/**
 * Remaps illegal XML chars to the Unicode Private use Area (PUA), and optionally CR also to the PUA.
 *
 * The Unicode PUA is a set of characters reserved for application-specific uses.
 * Daffodil is one of many tools that use the PUA so as to preserve characters XML doesn't support.
 *
 * Handles unpaired Unicode surrogate code points properly (remaps them).
 *
 * Legal XML v1.0 chars are #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
 *
 * Normally XML also remaps CRLF to LF and CR (isolated) to LF, but this is problematic
 * when data must be preserved perfectly so we have options to turn that off.
 *
 * We also can check and error if the string contains conflicting PUA characters to begin with.
 *
 * See https://daffodil.apache.org/infoset/, specifically the section "XML Illegal Characters", for
 * more discussion.
 */
final class RemapXMLIllegalCharToPUA(checkForExistingPUA: Boolean, replaceCRWithLF: Boolean)
  extends CharacterSetRemapper {

  /**
   * Remaps to PUA. Note return is negated char code of replacement char if we're to skip a character
   */
  override protected def remap(prev: Char, curr: Char, next: Char): Int = {
    val res: Int = curr match {
      case 0x9 => curr
      case 0xa => curr
      case 0xd =>
        if (next == 0xa) {
          // CRLF case.
          if (replaceCRWithLF)
            -0xa // CRLF => LF, standard XML behavior. Note negated.
          else
            0xe00d // remap CR to preserve it. Leave LF alone.
        } else {
          // isolated CR case
          if (replaceCRWithLF)
            0xa // isolated CR => LF, standard XML behavior. Note NOT negated.
          else
            0xe00d // remap isolated CR to preserve it.
        }
      case _ if (curr < 0x20) => curr + 0xe000 // ascii c0 controls
      // no remapping for the so called C1 controls (0x80-0x9F) Those are not XML illegal.
      case _ if Character.isSurrogate(curr) => {
        if (
          (Character.isHighSurrogate(curr) && Character.isLowSurrogate(next)) ||
          (Character.isLowSurrogate(curr) && Character.isHighSurrogate(prev))
        ) {
          // well formed surrogate pairs are preserved
          curr
        } else {
          // curr is an isolated surrogate, so to preserve we must remap to PUA
          curr + 0x1000
        }
      }
      case _ if (curr >= 0xe000 && curr <= 0xf8ff) => { // Unicode PUA is E000 to F8FF.
        if (checkForExistingPUA)
          throw new RemapPUACharDetected(curr)
        else curr
      }
      case _ if (curr < 0xfffe) => curr
      // 0xFFFE and 0xFFFF are regular Unicode chars, but XML illegal.
      // (XML only allows up to 0xFFFD)
      // They can't remap into the PUA by the basic techniques of adding
      // 0xE000 or 0x1000 like with control chars or unpaired surrogate code points.
      // So we just pick two adhoc, but recognizable, PUA code points to use by subtracting
      // 0x0F00 from them.
      case 0xfffe =>
        0xf0fe // U+FFFE is not a legal XML char. Can't remap to PUA the regular way.
      case 0xffff => 0xf0ff // U+FFFF is not a legal XML char
      case bad =>
        // $COVERAGE-OFF$
        // This is a final class, so this only gets called with characters by the
        // base class remap(s: String) method. Those chars are only
        // taken from Scala/Java strings, hence, the char codes cannot be beyond 0xFFFF
        Assert.impossibleCase(
          "Scala/Java character code cannot be beyond 0xFFFF but was 0x%40X".format(bad)
        )
      // $COVERAGE-ON$
    }
    res
  }

}

class RemapPUACharDetected(val char: Char)
  extends Exception(
    "Pre-existing Private Use Area (PUA) character found in data: U+%04X.".format(char.toInt)
  )

/**
 * Reverse of the RemapXMLIllegalCharToPUA mapping.
 */
final class RemapPUAToXMLIllegalChar() extends CharacterSetRemapper {

  /**
   * This direction of remapping is simpler. No context characters are needed, and
   * it never returns a negated character code.
   */
  override protected def remap(prevIgnored: Char, c: Char, nextIgnored: Char): Int = {
    val res: Int = c match {
      case _ if (c >= 0xe000 && c <= 0xe01f) => c - 0xe000 // Ascii c0 controls
      case _ if (c >= 0xe800 && c <= 0xefff) =>
        c - 0x1000 // isolated remapped surrogate codepoints
      case 0xf0fe => 0xfffe // FFFE is illegal in XML
      case 0xf0ff => 0xffff // FFFF is illegal in XML
      case _ => c
    }
    res
  }
}
