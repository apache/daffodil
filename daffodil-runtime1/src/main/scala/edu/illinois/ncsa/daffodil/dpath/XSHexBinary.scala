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

package edu.illinois.ncsa.daffodil.dpath

trait HexBinaryKind {

  /**
   * http://travisdazell.blogspot.com/2012/11/converting-hex-string-to-byte-array-in.html
   */
  protected def hexStringToByteArray(str: String): Array[Byte] = {
    val len = str.length

    if ((len % 2) != 0)
      throw new NumberFormatException("Failed to evaluate expression: A hexBinary value must contain an even number of characters.")

    val arr = new Array[Byte](len / 2)
    var i = 0
    while (i < len) {
      val upper = Character.digit(str.charAt(i), 16)
      val lower = Character.digit(str.charAt(i + 1), 16)

      if (upper == -1)
        throw new NumberFormatException("Failed to evaluate expression: Invalid hexadecimal digit '%c' at index %d of '%s'".format(str.charAt(i), i, str))
      if (lower == -1)
        throw new NumberFormatException("Failed to evaluate expression: Invalid hexadecimal digit '%c' at index %d of '%s'".format(str.charAt(i + 1), i + 1, str))

      val byte = (upper << 4) + (lower)
      arr(i / 2) = byte.asInstanceOf[Byte]
      i += 2
    }
    return arr
  }

  protected def reduce(numeric: Any): Array[Byte] = {
    val res: Array[Byte] = numeric match {
      case b: Byte => HexBinaryConversions.toByteArray(b)
      case s: Short if (s <= Byte.MaxValue && s >= Byte.MinValue) => reduce(s.toByte)
      case s: Short => HexBinaryConversions.toByteArray(s)
      case i: Int if (i <= Short.MaxValue && i >= Short.MinValue) => reduce(i.toShort)
      case i: Int => HexBinaryConversions.toByteArray(i)
      case l: Long if (l <= Int.MaxValue && l >= Int.MinValue) => reduce(l.toInt)
      case l: Long => HexBinaryConversions.toByteArray(l)
      case bi: BigInt if (bi.isValidLong) => reduce(bi.toLong)
      case bd: BigDecimal if (bd.isValidLong) => reduce(bd.toLong)
      case str: String => reduce(BigInt(str))
      case _ => throw new NumberFormatException("%s could not fit into a long".format(numeric.toString))
    }
    res
  }

  /**
   * http://javarevisited.blogspot.com/2013/03/convert-and-print-byte-array-to-hex-string-java-example-tutorial.html
   */
  protected def bytesToHexString(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append("%02X".format(b & 0xFF))
    }
    return sb.toString
  }
}

case class XSHexBinary(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) with HexBinaryKind {
  val name = "XSHexBinary"

  override def computeValue(a: Any, dstate: DState): Any = {
    // Check for:
    // 1. Even number of characters
    // 2. Valid hex (0-9 A-F)
    val array = a match {
      case s: String => hexStringToByteArray(s)
      case hb: Array[Byte] => hb
      case x => throw new NumberFormatException("%s cannot be cast to dfdl:hexBinary\ndfdl:hexBinary received an unrecognized type! Must be String or HexBinary.".format(x.toString))
    }
    array
  }
}
