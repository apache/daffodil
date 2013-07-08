package edu.illinois.ncsa.daffodil.util

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

import java.io.FileNotFoundException
import java.io.FileInputStream
import java.io.InputStream
import java.io.File
import java.net.URL
import java.net.URI
import java.io.ByteArrayInputStream
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import java.io.ByteArrayOutputStream

/**
 * Various reusable utilities that I couldn't easily find a better place for.
 */
object Misc {

  def getNameFromClass(obj: Object): String = {
    if (obj == null) return "null"
    // val hexHash = obj.hashCode.formatted("%x")
    val nonPackageClassName = obj.getClass.getName.split("""\.""").toList.reverse.head
    val nonDollarsParts = nonPackageClassName.split("""\$""").toList.reverse
    val nonNumericParts = nonDollarsParts.filterNot { _.matches("""\d*""") }
    val nameToken = nonNumericParts.head
    nameToken // + "@" + hexHash
  }

  def stripQuotes(s: String) = {
    val stripFirst = if (s.startsWith("\"")) s.substring(1) else s
    val stripLast = if (stripFirst.endsWith("\"")) stripFirst.substring(0, stripFirst.length - 1) else stripFirst
    stripLast
  }
  /**
   * Takes care of using the resource built-in to the jar, or
   * if we're just running interactively in eclipse or Intellij, doesn't use the jar.
   * But those put the same things on the classpath.
   *
   * resourcePath argument is relative to the classpath root.
   */

  def getResourceOption(resourcePath: String): (Option[URI], String) = {
    // more time is wasted by people forgetting that the initial "/" is needed
    // to get classpath relative behavior... Let's make sure there is a leading "/"
    val resPath = if (resourcePath.startsWith("/")) resourcePath else "/" + resourcePath
    var res = this.getClass.getResource(resPath)
    if (res == null) (None, resPath)
    else (Some(res.toURI), resPath)
  }

  lazy val classPath = {
    val props = System.getProperties()
    val cp = props.getProperty("java.class.path", null)
    val lines = cp.split(":").toSeq
    lines
  }

  def getRequiredResource(resourcePath: String): URI = {
    getResourceOption(resourcePath) match {
      case (None, resPath) => {
        val msg = "Required resource " + resPath + " was not found.\nClasspath is " +
          (if (classPath.length == 0) "empty."
          else ": " + classPath.mkString("\n"))
        System.err.println(msg)
        throw new Exception(msg)
      }
      case (Some(res), _) => res
    }
  }

  def initialUpperCase(s: String): String = s.head.toUpper + s.substring(1)
  def initialLowerCase(s: String): String = s.head.toLower + s.substring(1)

  /**
   * Returns a tuple with the primary version number in the first slot
   * the build hash in the second slot.
   */
  def getDaffodilVersion: Tuple2[String, String] = {
    val implVersion = this.getClass.getPackage.getImplementationVersion
    if (implVersion == null) {
      ("", "")
    } else {
      val VersionRegex = """(.+)-(.+)""".r
      implVersion match {
        case VersionRegex(v, b) => (v, b)
      }
    }
  }

  /**
   * Requires an even number of nibbles.
   */
  def hex2Bytes(hex: String): Array[Byte] = {
    (for { i <- 0 to hex.length - 1 by 2 if i > 0 || !hex.startsWith("0x") }
      yield hex.substring(i, i + 2))
      .map(Integer.parseInt(_, 16).toByte).toArray
  }

  def hex2Bits(hex: String): String = {
    val nums = hex.map { ch => Integer.parseInt(ch.toString, 16) }
    val bits = nums map { java.lang.Long.toString(_, 2) }
    val paddedBits = bits map { "%4s".format(_).replaceAll(" ", "0") }
    val res = paddedBits.mkString
    res
  }

  def cvtByte(b: Byte): String = {
    (if ((b & 0xff) < 0x10) "0" else "") + java.lang.Long.toString(b & 0xff, 16)
  }

  def bytes2Hex(bytes: Array[Byte]): String = {
    "0x" + bytes.map(cvtByte(_)).mkString.toUpperCase
  }

  def bits2Bytes(bits: String): Array[Byte] = {
    (for { i <- 0 to bits.length - 1 by 8 }
      yield bits.substring(i, math.min(bits.length, i + 8)))
      .map(Integer.parseInt(_, 2).toByte).toArray
  }

  def bytes2Bits(bytes: Array[Byte]): String = {
    def cvtByte(b: Byte) = {
      val indexes = (0 to 7).reverse
      val bits = indexes.map { index => (b >> index) & 0x01 }
      bits
    }
    val converted = bytes.flatMap { cvtByte(_) }
    val res = converted.mkString
    res
  }

  // Moved here from Compiler object.

  def stringToReadableByteChannel(s: String): ReadableByteChannel = {
    val bytes = s.getBytes("utf-8") // never use default charset. NEVER.
    byteArrayToReadableByteChannel(bytes)
  }

  def stringToWritableByteChannel(s: String): WritableByteChannel = {
    val size = s.length() // TODO: get byte count by encoding
    byteArrayToWritableByteChannel(size)
  }

  def byteArrayToReadableByteChannel(bytes: Array[Byte]): ReadableByteChannel = {
    val inputStream = new ByteArrayInputStream(bytes);
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def byteArrayToWritableByteChannel(size: Int): WritableByteChannel = {
    val outputStream = new ByteArrayOutputStream(size);
    val wbc = java.nio.channels.Channels.newChannel(outputStream);
    wbc
  }

  def fileToReadableByteChannel(file: java.io.File): ReadableByteChannel = {
    val inputStream = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  /**
   * This function creates a representation of data which doesn't
   * contain any whitespace characters that jump around the screen.
   * It replaces those with characters that have a simple glyph.
   *
   * The point of this is when you display the stream of data for
   * debugging, or for a diagnostic message,
   * the characters which control position like CR, LF, FF,
   * VT, HT, BS, etc. all make it hard to figure out what is going on.
   * Replacing these with the picture characters (designed for this purpose)
   * in the unicode x2400 block helps.
   */
  def remapControlsAndLineEndingsToVisibleGlyphs(s: String) = {
    def remap(c: Char) = {
      val URC = 0x2426 // Unicode control picture character for substutition (also looks like arabic q-mark)
      val code = c.toInt match {
        //
        // C0 Control pictures
        case n if (n <= 0x1F) => n + 0x2400
        case 0x20 => 0x2423 // For space we use the SP we use the ␣ (Unicode OPEN BOX)
        case 0x7F => 0x2421 // DEL pic isn't at 0x247F, it's at 0x2421
        //
        // Unicode separators & joiners
        case 0x00A0 => URC // no-break space
        case 0x200B => URC // zero width space
        case 0x2028 => URC // line separator 
        case 0x2029 => URC // paragraph separator 
        case 0x200C => URC // zero width non-joiner
        case 0x200D => URC // zero width joiner
        case 0x2060 => URC // word joiner
        // bi-di controls
        case 0x200E | 0x200F => URC
        case b if (b >= 0x202A && b <= 0x202E) => URC
        // byte order mark
        case 0xFFFE => URC // ZWNBS aka Byte Order Mark
        case 0xFFFF => URC // non-character FFFF
        // we assume surrogate codepoints all have a glyph (depends on font used of course)
        //
        // TODO: this could go on and on. There's a flock of 'space' characters (EM SPACE)
        // all over the place in Unicode. 
        // 
        // TODO: combining characters,
        // all whitespace, zero-width, and combining/joining characters would be 
        // represented by a separate glyph-character.
        //
        // Probably could be done by checking the character against some
        // unicode regex character classes like \p{M} which is the class 
        // of combining mark characters
        //
        //
        // Special case - if incoming character is one of the glyph
        // characters we're remapping onto, then change to URC
        //
        case n if (n > 0x2400 && n < 0x2423) => URC
        case _ => c
      }
      code.toChar
    }
    s.map { remap(_) }.mkString
  }

}
