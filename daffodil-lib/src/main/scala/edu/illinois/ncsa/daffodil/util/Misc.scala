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
   * Returns a tuple with the primary version number in the first slot and
   * the build number in the second slot.
   */
  def getDaffodilVersion: Tuple2[String, String] = {
    val implVersion = this.getClass.getPackage.getImplementationVersion
    if (implVersion == null) {
      ("", "")
    } else {
      val index = implVersion.indexOf("-")
      if (index > 0 && index < implVersion.length()) {
        (implVersion.substring(0, index), implVersion.substring(index + 1))
      } else {
        (implVersion, "")
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

}
