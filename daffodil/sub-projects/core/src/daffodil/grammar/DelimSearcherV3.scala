package stringsearch.DelimSearcherV3

import java.util.regex.Pattern
import java.util.regex.Matcher
import scala.collection.mutable.Queue
import scala.collection.mutable.LinkedList
import scala.util.control.Breaks
import java.nio.CharBuffer
import scala.collection.mutable.Set
import stringsearch.delimiter._
import scala.util.logging.ConsoleLogger
import scala.util.logging.Logged

class DelimSearcher extends Logged {

  var delimiters: List[DelimNode] = List.empty[DelimNode]

  def addDelimiter(strDelim: String) = {
    val d = new DelimNode with ConsoleLogger
    d(strDelim)
    delimiters ++= List[DelimNode] { d }
  }

  // Prints structure of Delimiters
  //
  def printDelimStruct = {
    var i = 0
    delimiters foreach {
      node =>
        val sb = new StringBuilder
        log("====\nDELIM: " + i + "\n====")
        node.delimBuf foreach {
          dBase =>
            dBase match {
              case dc: CharDelim => sb.append("\t" + dc.char)
              case cc: CharacterClass => cc match {
                case nl: NLDelim => sb.append("\tNL")
                case wsp: WSPDelim => sb.append("\tWSP")
                case wsp: WSPPlusDelim => sb.append("\tWSP+")
                case wsp: WSPStarDelim => sb.append("\tWSP*")
                case _ => sb.append("\tUnrec CharClass")
              }
              case _ => sb.append("\tUnrec")
            }
        }
        i += 1
        log(sb.toString() + "\n")
    }
    log("====\n")
  }

  // Prints structure of matches per delimiter, if any
  //
  def printMatchStruct = {
    delimiters foreach {
      node => node.print
    }
  }

  object SearchResult extends Enumeration {
    type SearchResult = Value
    val FullMatch, PartialMatch, EOF = Value
  }
  import SearchResult._

  // Don't use this!  Old Code!
  //
  def search(input: CharBuffer, startPos: Int = 0, clearState: Boolean = true): (SearchResult, String, Int) = {
    if (clearState) {
      clear
    }
    var matched: Boolean = false
    var partialMatched: Boolean = false
    var EOF: Boolean = false
    var endPos: Int = -1

    val crlfList: List[(Int, Int)] = getCRLFList(input)
    val wspList: List[(Int, Int)] = getConsecutiveWSPList(input)

    while (!matched && !EOF && !partialMatched) {
      // Search entire CharBuffer for first delimiter
      // if a full match is not found, was there a partial
      // match at the end of the CharBuffer?  If so, we need more data.
      // If not, repeat above with second delimiter.
      delimiters foreach {
        node =>
          {

            node.search(input, startPos, crlfList, wspList)

            log("NODE: " + node.print)

            matched = node.fullMatches.size > 0
            partialMatched = node.partialMatches.size > 0

            if (matched) {
              // Full match

              val firstMatch = node.fullMatches.toList.sortBy(_._1).head
              endPos = firstMatch._1
              log("ENDPOS: " + endPos)
              log("ENDCHAR: " + input.toString().charAt(endPos))
            } else if (partialMatched) {
              val sortedMatches = node.partialMatches.toList.sortBy(_._1)
              endPos = sortedMatches(sortedMatches.length - 1)._1
            }
          }
      } // end-delimiters-for-each
      EOF = true
    }

    if (matched) {
      log("FULL MATCH")
      return (SearchResult.FullMatch, input.subSequence(0, endPos).toString(), endPos)
    } else if (partialMatched) {
      log("PARTIAL MATCH")
      return (SearchResult.PartialMatch, input.subSequence(0, endPos).toString(), endPos)
    } else {
      log("NO MATCH")
      return (SearchResult.EOF, input.toString(), input.length())
    }
  }

  // Searches a CharBuffer by iterating through each delimiter and seeing if the CharBuffer contains
  // text that matches the delimiter either fully or partially.
  //
  // Returns:
  //	SearchResult	- FullMatch, PartialMatch, EOF
  //	String			- Data field or full CharBufer
  //	Int				- The position of the delimiter if found, or the end position of the CharBuffer
  //
  // Resume functionality is broken when it comes to CRLF always leave clearState equal to true
  // until this is rectified.
  //
  def search2(input: CharBuffer, startPos: Int = 0, clearState: Boolean = true): (SearchResult, String, Int, Int) = {
    if (clearState) {
      clear
    }
    var matched: Boolean = false
    var partialMatched: Boolean = false
    var EOF: Boolean = false
    var endPos: Int = -1

    val crlfList: List[(Int, Int)] = getCRLFList(input)
    val wspList: List[(Int, Int)] = getConsecutiveWSPList(input)

    delimiters foreach { node => node.search(input, startPos, crlfList, wspList) }

    val delimsWithFullMatches = delimiters.filter(node => node.fullMatches.size > 0)

    matched = delimsWithFullMatches.length > 0

    val delimsWithPartialMatches = delimiters.filter(node => node.partialMatches.size > 0)

    partialMatched = delimsWithPartialMatches.length > 0

    if (matched) {
      val longestMatch = getLongestMatch(delimsWithFullMatches)
      endPos = longestMatch._1
      log("FULL MATCH! String: " + input.subSequence(startPos, endPos).toString() + " StartPos: " + startPos + " EndPos: " + endPos)
      return (SearchResult.FullMatch, input.subSequence(startPos, endPos).toString(), endPos, longestMatch._2)
    } else if (partialMatched) {
      // We only care if a partial match occurred at the end of the CharBuffer
      val lastChar = input.length() - 1
      val lastPartial = delimsWithPartialMatches.flatMap(x => x.partialMatches.filter(y => y._1 == lastChar || y._2 == lastChar))
      if (lastPartial.size > 0) {
        endPos = lastPartial(lastPartial.size - 1)._2
      } else {
        endPos = lastChar
      }
      log("PARTIAL MATCH! String: " + input.subSequence(startPos, endPos).toString() + " StartPos: " + startPos + " EndPos: " + endPos)
      return (SearchResult.PartialMatch, input.subSequence(startPos, endPos).toString(), endPos, endPos)
    } else {
      endPos = input.length() - 1
      log("EOF! String: " + input.toString().substring(startPos) + " StartPos: " + startPos + " EndPos: " + endPos)
      return (SearchResult.EOF, input.toString().substring(startPos), endPos, endPos)
    }
  }

  // Used to retrieve the longest matching delimiter in the case
  // that this delimiter is contained within another fully matching
  // delimiter
  //
  // Ex. Given "abc:::def" and separators ":", ":::"
  // 	The separator ":" would match here three times.
  //	The separator ":::" would match once.
  //
  // Visually, we can see that the matching separator should be ":::"
  // but the first separator matched is ":".  In this case, since ":"
  // is contained within ":::", we want to return ":::" as the longest match.
  //
  // Assumes that delimiter list is in order of 
  // precedence (innermost separator first).
  //
  // DFDL Spec Rules Satisfied: 12.3.2 DFDLV1.0
  // 	When two delimiters have a common prefix, the longest
  // 	delimiter has precedence.
  //
  //	When two delimiters have exactly the same value, the innermost
  //	(most deeply nested) delimiter has precedence.
  //
  def getLongestMatch(matchedDelims: List[DelimNode]): (Int, Int) = {
    if (matchedDelims.length == 0) {
      return (-1, -1)
    }

    // Retrieve the list of delimiters prefixed by matchedDelims(0) and sort them
    // by length
    val result = getPrefixedDelims(matchedDelims(0), matchedDelims).sortBy(x => x._2 - x._1)

    log("Prefixed Delims: " + result)

    if (result.size > 0) {
      return result(result.size - 1)
    }
    // A prefixed delimiter was not found, return the first full match
    val fullMatch = matchedDelims(0).fullMatches.toList.sortBy(x => x._1).head
    fullMatch
  }

  // Retrieve all of the delimiters prefixed by the prefix DelimNode but
  // do not equal the prefix.
  //
  def getPrefixedDelims(prefix: DelimNode, delimsWithFullMatches: List[DelimNode]): List[(Int, Int)] = {
    val q = new Queue[(Int, Int)]

    // Head should always return the first element in the list
    // according to Scala 2.7.4 api
    val firstFullMatch = prefix.fullMatches.toList.sortBy(_._1).head

    delimsWithFullMatches.filter(x => x != prefix).foreach(delimNode => {
      // check that the firstFullMatch is a prefix but is not the same value
      val prefixedDelims = delimNode.fullMatches.filter(x => firstFullMatch._1 == x._1 && firstFullMatch._2 != x._2).toList
      if (prefixedDelims.length > 0) {
        q.enqueue(prefixedDelims(0))
      }
    })
    q.toList
  }

  def clear = {
    log("clear initiated!")
    delimiters.foreach(x => x.clear)
    log("clear completed!")
  }

  // Creates a list of all carriage returns (CR) followed
  // by a line feed (LF)
  //
  def getCRLFList(input: CharBuffer): List[(Int, Int)] = {
    val nl = new NLDelim
    val q = new Queue[(Int, Int)]

    for (i <- 0 until input.length()) {
      val char: Char = input.charAt(i)

      if (nl.CR == char) {
        // Found a CR, is LF next?
        val nextI = i + 1
        if (nextI < input.length()) {
          val nextChar: Char = input.charAt(nextI)
          if (nl.LF == nextChar) {
            q += (i -> nextI)
          }
        } else {
          // At end of buffer, need more info
          q += (i -> -1)
        }
      }
    }
    val crlfList = q.toList.sortBy(x => x._1)
    log("getCRLFList - crlfList:\t" + crlfList)
    crlfList
  }

  def getConsecutiveWSPList(input: CharBuffer): List[(Int, Int)] = {
    val wsp = new WSPDelim
    val q = new Queue[(Int, Int)]

    var start: Int = -1
    var end: Int = -1

    for (i <- 0 until input.length()) {
      val char: Char = input.charAt(i)

      if (wsp.checkMatch(char)) {
        // Found a WSP, is another WSP next?

        if (start == -1) {
          start = i
        }
        val nextI = i + 1
        if (nextI >= input.length()) {
          // At end of buffer, need more info
          q += (start -> -1)
        }
      } else {
        if (start != -1) {
          q += (start -> (i - 1))
        }
        start = -1
        end = -1
      }
    }
    q.toList
  }
  
}