package edu.illinois.ncsa.daffodil.processors.dfa

import edu.illinois.ncsa.daffodil.processors.DFDLCharReader

// This is the block of mutable things
// including the source of characters.
class Registers(val delimiters: Seq[DFADelimiter]) extends Serializable {

  // Very very loosely bind this whole system
  // to scala's Reader[Char] because that 
  // is too inefficient. (It allocates).
  // We really just need a 
  // get next char function.
  private var reader: DFDLCharReader = null;

  var numCharsRead: Int = 0
  var numCharsReadUntilDelim: Int = 0
  var data0: Char = DFA.EndOfDataChar // current character
  var data1: Char = DFA.EndOfDataChar // next (lookahead 1) character
  var matchStartPos: Int = 0
  var matchedAtLeastOnce: Boolean = false // WSPStar WSPPlus

  /**
   * Very important. We don't want to create these
   * over and over. We want to use one and reset it
   * and use it again.
   * <p>
   * So this is set up so you must construct it,
   * and then reset before first use. I.e.,
   * reset() is also init().
   */
  def reset(reader: DFDLCharReader, matchStartPos: Int) {
    Registers.this.reader = reader
    data0 = nextChar()
    data1 = nextChar()
    resultString.clear()
    delimString.clear()
    numCharsRead = 0
    numCharsReadUntilDelim = 0
    Registers.this.matchStartPos = matchStartPos
    matchedAtLeastOnce = false
  }

  /**
   * Use to set reader position to resume from.
   */
  def setResume(resumeReader: DFDLCharReader) {
    Registers.this.reader = resumeReader
  }

  def resumeForUnparse(resumeReader: DFDLCharReader) {
    Registers.this.reader = resumeReader
    data0 = nextChar()
    data1 = nextChar()
  }

  /**
   * Copy an existing Registers' reader position information
   * to this Registers.
   *
   * This allows this Registers to pick-up where the other
   * left off.
   */
  def copy(that: Registers) {
    Registers.this.reader = that.reader
    Registers.this.data0 = that.data0
    Registers.this.data1 = that.data1
    Registers.this.matchStartPos = that.numCharsReadUntilDelim
    resultString.clear
    delimString.clear
    numCharsRead = 0
    numCharsReadUntilDelim = 0
    matchedAtLeastOnce = false
  }

  def getReader: DFDLCharReader = reader

  // returns -1 for EOF (or maybe 26.toChar aka ^Z)
  def nextChar() = {
    val res = reader.first
    reader = reader.rest
    res
  }

  val charsReadUntilDelim: StringBuilder = new StringBuilder()
  val resultString: StringBuilder = new StringBuilder()
  val delimString: StringBuilder = new StringBuilder()

  def advance() = {
    data0 = data1
    data1 = nextChar()
  }

  def appendToField(c: Char): Unit = {
    charsReadUntilDelim.append(c)
    resultString.append(c)
    incCharsRead
    incCharsReadUntilDelim
  }

  def appendToDelim(c: Char): Unit = {
    delimString.append(c)
    incCharsRead
  }

  def dropChar(c: Char): Unit = {
    charsReadUntilDelim.append(c)
    incCharsRead
    incCharsReadUntilDelim
  }

  def incCharsRead(): Unit = numCharsRead += 1
  def incCharsReadUntilDelim(): Unit = numCharsReadUntilDelim += 1

  override def toString(): String = {
    "<Registers field='%s' delimiter='%s' numCharsRead='%d' />".format(resultString.toString, delimString.toString, numCharsRead)
  }

}