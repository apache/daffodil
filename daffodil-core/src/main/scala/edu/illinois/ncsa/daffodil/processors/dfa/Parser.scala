package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

trait Parser extends Serializable {
  def parse(input: DFDLCharReader): Maybe[ParseResult]
}

trait DelimitedParser extends Parser with HasLongestMatch {
  def parse(input: DFDLCharReader): Maybe[ParseResult] = parse(input, false)
  def parse(input: DFDLCharReader, isDelimiterRequired: Boolean): Maybe[ParseResult]
}

trait HasLongestMatch {

  /**
   * This function takes in a list of matches (in an ArrayBuffer for constant
   * append and random access) and returns the match that starts earliest in
   * the data. If multiple matches start at the same point in the data, the
   * match with the longer delimiter length is used as a tie breaker
   */
  protected def longestMatch(matches: ArrayBuffer[(DFADelimiter, Registers)]): Maybe[(DFADelimiter, Registers)] = {
    val len = matches.length
    if (len == 0) return Nope
    if (len == 1) return Some(matches(0))

    // these variables hold the Match/Registers of the match that starts earliest in
    // the data, using longest delimiter length as a tie breaker. Assume the
    // first match is the earliest longest to begin.
    var firstLongestMatchSoFar = matches(0)
    var (_, firstLongestRegSoFar) = firstLongestMatchSoFar

    var currIndex = 1 // skip the zeroth match since we assumed it was the first longest
    while (currIndex < len) {
      val currMatch = matches(currIndex)
      val (_, currReg) = currMatch
      if (currReg.matchStartPos < firstLongestRegSoFar.matchStartPos ||
         (currReg.matchStartPos == firstLongestRegSoFar.matchStartPos && currReg.delimString.length > firstLongestRegSoFar.delimString.length)) {
        firstLongestRegSoFar = currReg
        firstLongestMatchSoFar = currMatch
      }

      currIndex += 1
    }

    One(firstLongestMatchSoFar)
  }
}

class ParseResult(val field: Maybe[String],
  val matchedDelimiterValue: Maybe[String],
  val originalDelimiterRep: String,
  val numCharsRead: Int,
  val numBits: Int,
  val next: DFDLCharReader) {

  private val format = "<DFAParseResult field='%s' foundDelimiter='%s' searchedFor='%s' readerAtEnd='%s' readerPos='%s'/>"
  private val readerFormat = "readerAtEnd='%s' pos='%s'"

  private lazy val fieldStr = field.getOrElse("NOT-FOUND")
  private lazy val matchedDelimStr = matchedDelimiterValue.getOrElse("NOT-FOUND")
  private lazy val originalDelimiterStr = originalDelimiterRep
  private lazy val readerAtEnd = next.atEnd
  private lazy val pos = next.pos

  private lazy val formattedString = format.format(fieldStr, matchedDelimStr, originalDelimiterStr, readerAtEnd, pos)

  override def toString(): String = formattedString
}
