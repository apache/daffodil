package stringsearch.DelimSearcherV3

import java.util.regex.Pattern
import java.util.regex.Matcher
import scala.collection.mutable.Queue
import scala.collection.mutable.LinkedList
import scala.util.control.Breaks
import java.nio.CharBuffer
import scala.collection.mutable.Set
import stringsearch.delimiter._

class DelimSearcher {

  var delimiters: List[DelimNode] = List.empty[DelimNode]

  def addDelimiter(strDelim: String) = {
    val d = new DelimNode
    d(strDelim)
    delimiters ++= List[DelimNode] { d }
  }

  // Prints structure of Delimiters
  //
  def printDelimStruct = {
    var i = 0
    delimiters foreach {
      node =>
        println("====\nDELIM: " + i + "\n====")
        node.delimBuf foreach {
          dBase =>
            dBase match {
              case dc: CharDelim => print("\t" + dc.char)
              case cc: CharacterClass => cc match {
                case nl: NLDelim => print("\tNL")
                case wsp: WSPDelim => print("\tWSP")
                case wsp: WSPPlusDelim => print("\tWSP+")
                case wsp: WSPStarDelim => print("\tWSP*")
                case _ => print("\tUnrec CharClass")
              }
              case _ => print("\tUnrec")
            }
        }
        i += 1
        println("\n")
    }
    println("====\n")
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

  def search(input: CharBuffer, startPos: Int = 0, clearState: Boolean = true): (SearchResult, String, Int) = {
    if (clearState) {
      clear
    }
    var matched: Boolean = false
    var partialMatched: Boolean = false
    var EOF: Boolean = false
    var endPos: Int = -1

    val crlfList: List[(Int, Int)] = getCRLFList(input)

    while (!matched && !EOF && !partialMatched) {
      // Search entire CharBuffer for first delimiter
      // if a full match is not found, was there a partial
      // match at the end of the CharBuffer?  If so, we need more data.
      // If not, repeat above with second delimiter.
      delimiters foreach {
        node =>
          {
            node.search(input, startPos, crlfList)

            matched = node.fullMatches.size > 0
            partialMatched = node.partialMatches.size > 0

            if (matched) {
              // Full match
              val firstMatch = node.fullMatches.toList.sortBy(_._1).head
              endPos = firstMatch._1
              println("ENDPOS: " + endPos)
            } else if (partialMatched) {
              val sortedMatches = node.partialMatches.toList.sortBy(_._1)
              endPos = sortedMatches(sortedMatches.length - 1)._1
            }
          }
      } // end-delimiters-for-each
      EOF = true
    }

    if (matched) {
      println("FULL MATCH")
      return (SearchResult.FullMatch, input.subSequence(0, endPos).toString(), endPos)
    } else if (partialMatched) {
      println("PARTIAL MATCH")
      return (SearchResult.PartialMatch, input.subSequence(0, endPos).toString(), endPos)
    } else {
      println("NO MATCH")
      return (SearchResult.EOF, input.toString(), input.length())
    }
  }

  def clear = {
    delimiters.foreach(x => x.clear)
  }

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
    q.toList.sortBy(x => x._1)
  }
}



