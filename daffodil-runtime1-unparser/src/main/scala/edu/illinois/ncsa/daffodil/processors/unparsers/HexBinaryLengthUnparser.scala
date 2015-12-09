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

import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.dsom.TypeConversions
import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.Assert

abstract class HexBinaryLengthInBytesUnparser(erd: ElementRuntimeData)
  extends PrimUnparser(erd) {

  protected def getLength(state: UState): Long

  final def unparse(state: UState): Unit = {

    val event: InfosetEvent = { Assert.invariant(state.inspect); state.inspectAccessor }
    val node = event.node.asSimple
    val value = node.dataValue.asInstanceOf[Array[Byte]]

    val dos = state.dataOutputStream

    val ret = dos.putBytes(value)
    if (ret != value.length) {
      UnparseError(Nope, One(state.currentLocation), "Expected to write %d hexBinary bytes, but wrote %d.", value.length, ret)
    }

    val minLengthInBytes = getLength(state)
    val nFillBytes = minLengthInBytes - value.length
    if (nFillBytes > 0) {
      dos.setFillByte(erd.fillByteValue)
      val ret = dos.skip(nFillBytes * 8)
      if (!ret) {
        UnparseError(Nope, One(state.currentLocation), "Failed to skip %d bytes.", nFillBytes)
      }
    }
  }
}

final class HexBinaryFixedLengthInBytesUnparser(nBytes: Long, erd: ElementRuntimeData)
  extends HexBinaryLengthInBytesUnparser(erd) {

  override def getLength(state: UState): Long = nBytes
}

final class HexBinaryVariableLengthInBytesUnparser(erd: ElementRuntimeData, length: CompiledExpression)
  extends HexBinaryLengthInBytesUnparser(erd) {

  override def getLength(state: UState): Long = {
    val lengthAsAny = length.evaluate(state)
    val l = TypeConversions.convertToLong(lengthAsAny, state)
    l
  }
}
