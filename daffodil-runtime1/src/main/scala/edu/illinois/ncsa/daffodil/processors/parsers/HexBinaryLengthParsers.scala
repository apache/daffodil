/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.dsom.ConstantExpression
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.io.DataStreamCommon
import edu.illinois.ncsa.daffodil.exceptions.Assert

abstract class HexBinaryLengthInBytesParser(erd: ElementRuntimeData)
  extends PrimParser(erd) {

  protected def getLength(pstate: PState): Long

  private val zeroLengthArray = new Array[Byte](0)

  final def parse(start: PState): Unit = {

    val nBytes = getLength(start)
    if (nBytes == 0) {
      val currentElement = start.simpleElement
      currentElement.setDataValue(zeroLengthArray)
      return
    }
    DataStreamCommon.withLocalByteBuffer { lbb =>
      val bb = lbb.getBuf(nBytes)
      val mLen = start.dataInputStream.fillByteBuffer(bb)
      if (!mLen.isDefined) {
        PE(start, "%s - Insufficient Bits in field: IndexOutOfBounds: wanted %s byte(s), but found only %s available.",
          parserName, nBytes, 0)
      } else {
        val nBytesFound = mLen.get
        if (nBytesFound < nBytes) {
          PE(start, "%s - Insufficient Bits in field: IndexOutOfBounds: wanted %s byte(s), but found only %s available.",
            parserName, nBytes, nBytesFound)
        } else {
          Assert.invariant(nBytesFound == nBytes)
          val currentElement = start.simpleElement
          val ba = new Array[Byte](nBytes.toInt)
          bb.flip
          bb.get(ba) // no exception should be thrown ever here.
          currentElement.setDataValue(ba)
        }
      }
    }
  }

}

final class HexBinaryFixedLengthInBytesParser(nBytes: Long, erd: ElementRuntimeData)
  extends HexBinaryLengthInBytesParser(erd) {

  def getLength(pstate: PState): Long = nBytes

}

final class HexBinaryVariableLengthInBytesParser(erd: ElementRuntimeData, override val length: CompiledExpression)
  extends HexBinaryLengthInBytesParser(erd)
  with HasVariableLength {
}
