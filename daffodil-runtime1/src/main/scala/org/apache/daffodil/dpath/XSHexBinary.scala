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

package org.apache.daffodil.dpath

import java.math.{ BigInteger => JBigInt, BigDecimal => JBigDecimal }
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValueByteArray

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
      //
      // Note -128 to 255 are converted to bytes. This means signed or unsigned bytes
      // will stay at size 1 byte. This prevents bytes like 0xD0 from turning
      // into 00D0 and occupying 4 characters instead of two.
      //
      case s: Short if (s <= 255 && s >= Byte.MinValue) => reduce(s.toByte)
      case s: Short => HexBinaryConversions.toByteArray(s)
      case i: Int if (i <= Short.MaxValue && i >= Short.MinValue) => reduce(i.toShort)
      case i: Int => HexBinaryConversions.toByteArray(i)
      case l: Long if (l <= Int.MaxValue && l >= Int.MinValue) => reduce(l.toInt)
      case l: Long => HexBinaryConversions.toByteArray(l)
      case bi: JBigInt if (bi.bitLength <= 63) => reduce(bi.longValue())
      case bi: JBigInt => bi.toByteArray()
      case bd: JBigDecimal if (try { bd.toBigIntegerExact(); true } catch { case e: ArithmeticException => false }) => reduce(bd.toBigIntegerExact())
      case str: String => reduce(new JBigInt(str))
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
  extends FNOneArg(recipe, argType) {
  val name = "XSHexBinary"

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueByteArray = {
    StringToHexBinary.computeValue(a, dstate)
  }
}
