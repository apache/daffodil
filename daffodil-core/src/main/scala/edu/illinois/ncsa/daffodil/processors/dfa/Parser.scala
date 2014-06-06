package edu.illinois.ncsa.daffodil.processors.dfa

import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

trait Parser {
  def parse(input: DFDLCharReader): Maybe[ParseResult]
}

trait DelimitedParser extends Parser with HasLongestMatch {
  def parse(input: DFDLCharReader): Maybe[ParseResult] = parse(input, false)
  def parse(input: DFDLCharReader, isDelimiterRequired: Boolean): Maybe[ParseResult]
}

trait HasLongestMatch {
  protected def longestMatch(matches: Seq[(DFADelimiter, Registers)]): Maybe[(DFADelimiter, Registers)] = {
    if (matches.isEmpty) return Nope

    val (minD, minR: Registers) = matches.minBy { case (d, r) => r.matchStartPos }
    val theFirstLongestMatch = {
      val minValue = matches.filter { case (d: DFADelimiter, r: Registers) => r.matchStartPos == minR.matchStartPos }
      minValue.maxBy { _._2.delimString.length }
    }
    One(theFirstLongestMatch)
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
