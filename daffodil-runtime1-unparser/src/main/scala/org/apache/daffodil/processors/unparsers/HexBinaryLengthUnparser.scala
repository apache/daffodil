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

package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.infoset.RetryableException
import org.apache.daffodil.processors.UnparseTargetLengthInBitsEv
import org.apache.daffodil.exceptions.Assert

abstract class HexBinaryUnparserBase(override val context: ElementRuntimeData)
  extends PrimUnparser {

  override lazy val runtimeDependencies = Nil

  protected def getLengthInBits(state: UState): Long

  override final def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.asInstanceOf[Array[Byte]]
    val lengthInBits = getLengthInBits(state)

    val lengthInBytes = (lengthInBits + 7) / 8
    if (value.length > lengthInBytes) {
      UnparseError(One(context.schemaFileLocation), One(state.currentLocation), "Data length %d bits exceeds explicit length value: %d bits", value.length * 8, lengthInBits)
    }

    val bitsFromValueToPut =
      if (lengthInBytes > value.size) {
        // the length to put is larger than the number of available bytes in the
        // array. So put the whole array and we'll add fill bytes later
        value.size * 8
      } else {
        // the length to put is either everything or some fragment of the last
        // byte, so put the length in bits. putByte will deal with the fragment byte
        Assert.invariant(lengthInBytes == value.size)
        lengthInBits
      }

    val dos = state.dataOutputStream

    // put the hex binary array
    if (bitsFromValueToPut > 0) {
      val ret = dos.putByteArray(value, bitsFromValueToPut.toInt, state)
      if (!ret) {
        UnparseError(One(context.schemaFileLocation), One(state.currentLocation), "Failed to write %d hexBinary bits", bitsFromValueToPut)
      }
    }

    // calculate the skip bits
    val nFillBits = lengthInBits - bitsFromValueToPut
    if (nFillBits > 0) {
      val ret = dos.skip(nFillBits, state)
      if (!ret) {
        UnparseError(Nope, One(state.currentLocation), "Failed to skip %d bits.", nFillBits)
      }
    }
  }
}

final class HexBinaryMinLengthInBytesUnparser(minLengthInBytes: Long, erd: ElementRuntimeData)
  extends HexBinaryUnparserBase(erd) {

  override def getLengthInBits(state: UState): Long = {
    val len = state.currentNode.get.asSimple.dataValue.asInstanceOf[Array[Byte]].length * 8
    val min = minLengthInBytes * 8
    scala.math.max(len, min)
  }
}

final class HexBinarySpecifiedLengthUnparser(erd: ElementRuntimeData, val lengthEv: UnparseTargetLengthInBitsEv)
  extends HexBinaryUnparserBase(erd) {

  override def getLengthInBits(state: UState): Long = {
    val l: Long = try {
      lengthEv.evaluate(state).getULong.toLong
    } catch {
      case e: RetryableException => {
        val bytes = state.currentInfosetNode.asSimple.dataValue.asInstanceOf[Array[Byte]]
        val len = bytes.length * 8
        len
      }
    }
    l
  }
}
