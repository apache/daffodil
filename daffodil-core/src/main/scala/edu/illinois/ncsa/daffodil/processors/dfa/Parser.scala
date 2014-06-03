package edu.illinois.ncsa.daffodil.processors.dfa

import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader

trait Parser {
  def parse(input: DFDLCharReader): Option[ParseResult]
}

trait DelimitedParser extends Parser with HasLongestMatch {
  def parse(input: DFDLCharReader): Option[ParseResult] = parse(input, false)
  def parse(input: DFDLCharReader, isDelimiterRequired: Boolean): Option[ParseResult]
}

trait HasLongestMatch {
  protected def longestMatch(matches: Seq[(DFADelimiter, Registers)]): Option[(DFADelimiter, Registers)] = {
    if (matches.isEmpty) return None

    val (minD, minR: Registers) = matches.minBy { case (d, r) => r.matchStartPos }
    val theFirstLongestMatch = {
      val minValue = matches.filter { case (d: DFADelimiter, r: Registers) => r.matchStartPos == minR.matchStartPos }
      minValue.maxBy { _._2.delimString.length }
    }
    Some(theFirstLongestMatch)
  }
}

class ParseResult(val field: Option[String],
  val matchedDelimiterValue: Option[String],
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
