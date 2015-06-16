package edu.illinois.ncsa.daffodil.processors.dfa

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import scala.collection.mutable.ArrayBuffer
import edu.illinois.ncsa.daffodil.processors.RuntimeData

/**
 * Parent class of all DFA text parsers.
 */
abstract class Unparser extends Serializable {
  def name: String
  def info: String

  def context: RuntimeData

  override def toString(): String = name + "(context='" + context + "', " + info + ")"
}

abstract class DelimitedUnparser extends Unparser {
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
