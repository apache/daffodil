package edu.illinois.ncsa.daffodil.processors.dfa

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.equality._

// This is the block of mutable things
// including the source of characters.
class Registers(val delimiters: Seq[DFADelimiter]) extends Serializable {

  var dataInputStream: DataInputStream = null
  var numCharsRead: Int = 0
  var numCharsReadUntilDelim: Int = 0
  var numCharsDropped: Int = 0
  var data0: Char = DFA.EndOfDataChar // current character
  var data1: Char = DFA.EndOfDataChar // next (lookahead 1) character
  var matchStartPos: Int = 0
  var matchedAtLeastOnce: Boolean = false // WSPStar WSPPlus

  def charIterator = dataInputStream.asIteratorChar

  /**
   * Very important. We don't want to create these
   * over and over. We want to use one and reset it
   * and use it again.
   * <p>
   * So this is set up so you must construct it,
   * and then reset before first use. I.e.,
   * reset() is also init().
   */
  def reset(input: DataInputStream, m: DataInputStream.MarkPos = DataInputStream.MarkPos.NoMarkPos) {
    dataInputStream = input
    if (m !=:= DataInputStream.MarkPos.NoMarkPos) dataInputStream.resetPos(m)
    resetChars
    resultString.clear()
    delimString.clear()
    numCharsRead = 0
    numCharsReadUntilDelim = 0
    numCharsDropped = 0
    Registers.this.matchStartPos = matchStartPos
    matchedAtLeastOnce = false
  }

  def resetChars {
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
  }

  /**
   * Consumes one character (in the sense of advancing the data stream past the bits of that character's code points.
   * Then peeks ahead one further, and returns the peeked character.
   */
  def nextChar() = {
    if (charIterator.hasNext) charIterator.next()
    charIterator.peek()
  }

  val charsReadUntilDelim: StringBuilder = new StringBuilder()
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
    charsReadUntilDelim.append(c)
    resultString.append(c)
    incCharsRead
    incCharsReadUntilDelim
  }

  def appendToDelim(c: Char): Unit = {
    commitOneChar
    delimString.append(c)
    incCharsRead
  }

  def dropChar(c: Char): Unit = {
    commitOneChar
    charsReadUntilDelim.append(c)
    incCharsRead
    incCharsReadUntilDelim
    incCharsDropped
  }

  def incCharsRead(): Unit = numCharsRead += 1
  def incCharsReadUntilDelim(): Unit = numCharsReadUntilDelim += 1
  def incCharsDropped: Unit = numCharsDropped += 1

  override def toString(): String = {
    "<Registers field='%s' delimiter='%s' numCharsRead='%d' />".format(resultString.toString, delimString.toString, numCharsRead)
  }

}