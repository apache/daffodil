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

abstract class HexBinaryLengthInBytesParser(erd: ElementRuntimeData)
  extends PrimParser(erd) {

  protected def parserName: String

  protected def getLength(pstate: PState): Long

  final def parse(start: PState): Unit = {

    log(LogLevel.Debug, "Parsing starting at bit position: %s", start.bitPos)

    val nBytes = getLength(start)
    log(LogLevel.Debug, "Explicit length %s", nBytes)

    if (start.bitPos % 8 != 0) {
      PE(start, "%s - not byte aligned.", parserName)
      return
    }

    val bytes = start.inStream.getBytes(start.bitPos, nBytes)
    val currentElement = start.simpleElement
    currentElement.setDataValue(bytes)

    if (bytes.length != nBytes)
      PE(start, "%s - Insufficient Bits in field: IndexOutOfBounds: wanted %s byte(s), but found only %s available.",
        parserName, nBytes, bytes.length)
    else
      start.setPos(start.bitPos + (bytes.length * 8), -1, Nope)
  }

}

final class HexBinaryFixedLengthInBytesParser(nBytes: Long, erd: ElementRuntimeData)
  extends HexBinaryLengthInBytesParser(erd) {

  def parserName = "HexBinaryFixedLengthInBytes"

  def getLength(pstate: PState): Long = nBytes

}

final class HexBinaryFixedLengthInBitsParser(nBits: Long, erd: ElementRuntimeData)
  extends HexBinaryLengthInBytesParser(erd) {

  def parserName = "HexBinaryFixedLengthInBits"

  def getLength(pstate: PState): Long = {
    val nBytes = scala.math.ceil(nBits / 8).toLong
    nBytes
  }
}

final class HexBinaryVariableLengthInBytesParser(erd: ElementRuntimeData, override val length: CompiledExpression)
  extends HexBinaryLengthInBytesParser(erd)
  with HasVariableLength {

  def parserName = "HexBinaryVariableLengthInBytes"
}
