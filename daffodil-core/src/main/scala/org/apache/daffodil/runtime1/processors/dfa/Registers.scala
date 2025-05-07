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

package org.apache.daffodil.runtime1.processors.dfa

import org.apache.daffodil.io.DataInputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.lib.equality.ViewEqual
import org.apache.daffodil.lib.util.Pool
import org.apache.daffodil.lib.util.Poolable
import org.apache.daffodil.runtime1.processors.DelimiterIterator

class RegistersPool() extends Pool[Registers] {
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

  var charIterator: DataInputStream.CharIterator = null // dataInputStream.asIteratorChar

  var actionNum: Int = 0
  var state: Int = -1
  var nextState: Int =
    -1 // used to determine the next state to go to, should only be used when status == StateKind.Parsing
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
  def reset(
    finfo: FormatInfo,
    input: DataInputStream,
    delimIter: DelimiterIterator,
    m: DataInputStream.MarkPos = DataInputStream.MarkPos.NoMarkPos
  ): Unit = {
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

  def resetChars(finfo: FormatInfo): Unit = {
    charIterator = dataInputStream.asIteratorChar
    charIterator.setFormatInfo(finfo)
    data0 = charIterator.peek()
    data1 = charIterator.peek2()
  }

  /**
   * Copy an existing Registers' reader position information
   * to this Registers.
   *
   * This allows this Registers to pick-up where the other
   * left off.
   */
  def copy1(that: Registers): Unit = {
    Registers.this.dataInputStream = that.dataInputStream
    Registers.this.data0 = that.data0
    Registers.this.data1 = that.data1
    Registers.this.matchStartPos = that.numCharsReadUntilDelim
    resultString.clear()
    delimString.clear()
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

  def commitOneChar(): Unit = {
    if (charIterator.hasNext) charIterator.next()
  }

  def appendToField(c: Char): Unit = {
    commitOneChar()
    resultString.append(c)
    incCharsRead()
    incCharsReadUntilDelim()
  }

  def appendToField(cs: CharSequence): Unit = {
    var i: Int = 0
    val nChars = cs.length()
    while (i < nChars) {
      i += 1
      commitOneChar()
    }
    resultString.append(cs)
    incCharsRead(nChars)
    incCharsReadUntilDelim(nChars)
  }

  def appendToDelim(c: Char): Unit = {
    commitOneChar()
    delimString.append(c)
    incCharsRead()
  }

  def dropChar(c: Char): Unit = {
    commitOneChar()
    incCharsRead()
    incCharsReadUntilDelim()
    incCharsDropped()
  }

  def incCharsRead(incr: Int = 1): Unit = numCharsRead += incr
  def incCharsReadUntilDelim(incr: Int = 1): Unit = numCharsReadUntilDelim += incr
  def incCharsDropped(incr: Int = 1): Unit = numCharsDropped += incr

  override def toString(): String = {
    "<Registers field='%s' delimiter='%s' numCharsRead='%d' />".format(
      resultString.toString,
      delimString.toString,
      numCharsRead
    )
  }

}
