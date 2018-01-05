/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.dfa

import edu.illinois.ncsa.daffodil.equality.ViewEqual
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.processors.DelimiterIterator
import edu.illinois.ncsa.daffodil.util.Pool
import edu.illinois.ncsa.daffodil.util.Poolable
import edu.illinois.ncsa.daffodil.io.FormatInfo

private[dfa] object TLRegistersPool extends ThreadLocal[RegistersPool] {
  override def initialValue = new RegistersPool()

  def pool() = this.get

  def getFromPool(requestorID: String) =
    pool.getFromPool(requestorID)

  def returnToPool(r: Registers) = pool.returnToPool(r)
}

private[dfa] class RegistersPool() extends Pool[Registers] {
  override def allocate = new Registers()
}

// This is the block of mutable things
// including the source of characters.
class Registers() extends Poolable with Serializable {

  var dataInputStream: DataInputStream = null
  var numCharsRead: Int = 0
  var numCharsReadUntilDelim: Int = 0
  var numCharsDropped: Int = 0
  var data0: Char = DFA.EndOfDataChar // current character
  var data1: Char = DFA.EndOfDataChar // next (lookahead 1) character
  var matchStartPos: Int = 0
  var matchedAtLeastOnce: Boolean = false // WSPStar WSPPlus

  var charIterator: DataInputStream.CharIterator = null //dataInputStream.asIteratorChar

  var actionNum: Int = 0
  var state: Int = -1
  var nextState: Int = -1 // used to determine the next state to go to, should only be used when status == StateKind.Parsing
  var status: StateKind.Value = StateKind.Parsing
  var delimitersIter: DelimiterIterator = null

  /**
   * Very important. We don't want to create these
   * over and over. We want to use one and reset it
   * and use it again.
   * <p>
   * So this is set up so you must construct it,
   * and then reset before first use. I.e.,
   * reset() is also init().
   */
  def reset(finfo: FormatInfo, input: DataInputStream, delimIter: DelimiterIterator, m: DataInputStream.MarkPos = DataInputStream.MarkPos.NoMarkPos) {
    dataInputStream = input
    if (m !=#= DataInputStream.MarkPos.NoMarkPos)
      dataInputStream.resetPos(m)
    resetChars(finfo)
    resultString.clear()
    delimString.clear()
    numCharsRead = 0
    numCharsReadUntilDelim = 0
    numCharsDropped = 0
    Registers.this.matchStartPos = matchStartPos
    matchedAtLeastOnce = false
    actionNum = 0
    state = 0
    nextState = 0
    status = StateKind.Parsing
    delimitersIter = delimIter
  }

  def resetChars(finfo: FormatInfo) {
    charIterator = dataInputStream.asIteratorChar
    charIterator.setFormatInfo(finfo)
    data0 = charIterator.peek()
    data1 = charIterator.peek2()
  }

  /**
   * Use to set reader position to resume from.
   */
  //  def setResume(resumeReader: DFDLCharReader) {
  //    Registers.this.reader = resumeReader
  //  }
  //
  //  def resumeForUnparse(resumeReader: DFDLCharReader) {
  //    Registers.this.reader = resumeReader
  //    data0 = nextChar()
  //    data1 = nextChar()
  //  }

  /**
   * Copy an existing Registers' reader position information
   * to this Registers.
   *
   * This allows this Registers to pick-up where the other
   * left off.
   */
  def copy1(that: Registers) {
    Registers.this.dataInputStream = that.dataInputStream
    Registers.this.data0 = that.data0
    Registers.this.data1 = that.data1
    Registers.this.matchStartPos = that.numCharsReadUntilDelim
    resultString.clear
    delimString.clear
    numCharsRead = 0
    numCharsReadUntilDelim = 0
    numCharsDropped = 0
    matchedAtLeastOnce = false
    status = StateKind.Parsing
  }

  /**
   * Consumes one character (in the sense of advancing the data stream past the bits of that character's code points.
   * Then peeks ahead one further, and returns the peeked character.
   */
  def nextChar() = {
    if (charIterator.hasNext) charIterator.next()
    charIterator.peek()
  }

  val resultString: StringBuilder = new StringBuilder()
  val delimString: StringBuilder = new StringBuilder()

  /**
   * When we advance, data0 is known to be consumed into either
   * the field, or delimiter being accumulated, or it is dropped.
   *
   * The new data0 and data1 are effectively peek() and peek2() past
   * the character that was just consumed, and that's an invariant
   * here. I.e., data0 and data1 are always two characters lookahead
   * into the data stream.
   */
  def advance() = {
    data0 = charIterator.peek()
    data1 = charIterator.peek2()
  }

  def commitOneChar {
    if (charIterator.hasNext) charIterator.next()
  }

  def appendToField(c: Char): Unit = {
    commitOneChar
    resultString.append(c)
    incCharsRead()
    incCharsReadUntilDelim()
  }

  def appendToField(cs: CharSequence): Unit = {
    var i: Int = 0
    val nChars = cs.length()
    while (i < nChars) {
      i += 1
      commitOneChar
    }
    resultString.append(cs)
    incCharsRead(nChars)
    incCharsReadUntilDelim(nChars)
  }

  def appendToDelim(c: Char): Unit = {
    commitOneChar
    delimString.append(c)
    incCharsRead()
  }

  def dropChar(c: Char): Unit = {
    commitOneChar
    incCharsRead()
    incCharsReadUntilDelim()
    incCharsDropped()
  }

  def incCharsRead(incr: Int = 1): Unit = numCharsRead += incr
  def incCharsReadUntilDelim(incr: Int = 1): Unit = numCharsReadUntilDelim += incr
  def incCharsDropped(incr: Int = 1): Unit = numCharsDropped += incr

  override def toString(): String = {
    "<Registers field='%s' delimiter='%s' numCharsRead='%d' />".format(resultString.toString, delimString.toString, numCharsRead)
  }

}
