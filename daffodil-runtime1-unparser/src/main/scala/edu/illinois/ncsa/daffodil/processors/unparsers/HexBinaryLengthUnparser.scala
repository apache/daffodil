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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.Evaluatable
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import edu.illinois.ncsa.daffodil.processors.FillByteEv
import java.lang.{ Long => JLong }

abstract class HexBinaryLengthInBytesUnparser(erd: ElementRuntimeData, fillByteEv: FillByteEv)
  extends PrimUnparserObject(erd) {

  protected def getLength(state: UState): Long

  final def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.asInstanceOf[Array[Byte]]
    val minLengthInBytes =
      if (erd.minLength.isEmpty) 0 else erd.minLength.get.intValue()
    val lengthInBytes = getLength(state)
    val lengthForFill = math.max(minLengthInBytes, lengthInBytes)

    if (value.length > lengthForFill) {
      UnparseError(One(erd.schemaFileLocation), One(state.currentLocation), "Data length %d exceeds explicit length value: %d", value.length, lengthInBytes)
    }

    val dos = state.dataOutputStream

    val ret = dos.putBytes(value)
    if (ret != value.length) {
      UnparseError(One(erd.schemaFileLocation), One(state.currentLocation), "Expected to write %d hexBinary bytes, but wrote %d.", value.length, ret)
    }

    val nFillBytes = lengthForFill - value.length
    if (nFillBytes > 0) {
      val fillByte = fillByteEv.evaluate(state)
      dos.setFillByte(fillByte) // TODO: PEFORMANCE: this and many other settings should be set via a changeFillByte processor, so that it is always pre-set???
      val ret = dos.skip(nFillBytes * 8)
      if (!ret) {
        UnparseError(Nope, One(state.currentLocation), "Failed to skip %d bytes.", nFillBytes)
      }
    }
  }
}

final class HexBinaryFixedLengthInBytesUnparser(nBytes: Long, erd: ElementRuntimeData, fillByteEv: FillByteEv)
  extends HexBinaryLengthInBytesUnparser(erd, fillByteEv) {

  override def getLength(state: UState): Long = nBytes
}

final class HexBinaryDelimitedMinLengthInBytesUnparser(minLengthInBytes: Long, erd: ElementRuntimeData, fillByteEv: FillByteEv)
  extends HexBinaryLengthInBytesUnparser(erd, fillByteEv) {

  override def getLength(state: UState): Long = {
    state.currentNode.get.asSimple.dataValue.asInstanceOf[Array[Byte]].length
  }
}

final class HexBinaryVariableLengthInBytesUnparser(erd: ElementRuntimeData, val lengthEv: Evaluatable[JLong], fillByteEv: FillByteEv)
  extends HexBinaryLengthInBytesUnparser(erd, fillByteEv) {

  override def getLength(state: UState): Long = {
    val lengthAsAnyRef = lengthEv.evaluate(state)
    val l = AsIntConverters.asLong(lengthAsAnyRef)
    l
  }
}
