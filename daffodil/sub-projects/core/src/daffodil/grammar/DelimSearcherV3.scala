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

import stringsearch.constructs.EscapeSchemeKind.EscapeSchemeKind
import stringsearch.constructs.EscapeSchemeKind
import stringsearch.constructs.SearchResult
import stringsearch.constructs.SearchResult._
import stringsearch.constructs.EscapeScheme._

import daffodil.util._

class DelimSearcher extends Logging {

  // A list of delimiters, each delimiter is represented as a Delimiter object
  var delimiters: List[Delimiter] = List.empty[Delimiter]
  
  var escapeSchemeKind: EscapeSchemeKind = EscapeSchemeKind.None
  var esCharacter: String = ""
  var esEsCharacter: String = ""
  var esBlockStart: String = ""
  var esBlockEnd: String = ""

  def addDelimiter(strDelim: String) = {
    val d = new Delimiter
    d(strDelim)
    delimiters ++= List[Delimiter] { d }
  }
  
  def addSeparator(strDelim: String) = {
    val d = new Separator with Logging
    d(strDelim)
    delimiters ++= List[Delimiter] { d }
  }
  
  def addTerminator(strDelim: String) = {
    val d = new Terminator with Logging
    d(strDelim)
    delimiters ++= List[Delimiter] { d }
  }

  // Prints structure of Delimiters
  //
  def printDelimStruct = {
    var i = 0
    delimiters foreach {
      node =>
        val sb = new StringBuilder
        log(Debug("====\nDELIM: " + i + "\n===="))
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
        log(Debug(sb.toString() + "\n"))
    }
    log(Debug("====\n"))
  }

  // Prints structure of matches per delimiter, if any
  //
  def printMatchStruct = {
    delimiters foreach {
      node => node.print
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
  def search(input: CharBuffer, startPos: Int = 0, clearState: Boolean = true): (SearchResult, String, Int, Int, Delimiter) = {
    //setLoggingLevel(LogLevel.Debug)
    
    var matched: Boolean = false
    var partialMatched: Boolean = false
    var EOF: Boolean = false
    var endPos: Int = -1

    if (clearState) {
      clear
    }
    if (input.toString().length() == 0) {
      log(Debug("EOF! String: was EMPTY!! StartPos: " + startPos + " EndPos: " + endPos))
      return (SearchResult.NoMatch, input.toString(), -1, -1, null)
    }
    if (startPos < 0) {
      endPos = input.length() - 1
      log(Debug("EOF! String: " + input.toString().substring(startPos) + " StartPos: " + startPos + " EndPos: " + endPos))
      return (SearchResult.NoMatch, input.toString().substring(0), endPos, endPos, null)
    }

    val crlfList: List[(Int, Int)] = getCRLFList(input)
    val wspList: List[(Int, Int)] = getConsecutiveWSPList(input)
    val escapeCharList: List[(Int)] = getEscapeCharacterList(input)
    val (escapeEscapeCharList, _) = this.getEscapeEscapeCharacterList(input)
    val escapeBlockList: List[(Int, Int)] = getEscapeBlocks(input)
    
    if (this.escapeSchemeKind == EscapeSchemeKind.Block){
      delimiters foreach { node => node.searchWithEscapeSchemeBlock(input, startPos, crlfList, wspList,
          escapeEscapeCharList, escapeBlockList) }
    }else if (this.escapeSchemeKind == EscapeSchemeKind.Character) {
    	delimiters foreach { node => node.searchWithEscapeSchemeCharacter(input, startPos, crlfList, wspList,
          escapeCharList) }
    } else {
      delimiters foreach { node => node.search(input, startPos, crlfList, wspList) }
    }

    val delimsWithFullMatches = delimiters.filter(node => node.fullMatches.size > 0)

    matched = delimsWithFullMatches.length > 0

    val delimsWithPartialMatches = delimiters.filter(node => node.partialMatches.size > 0)

    partialMatched = delimsWithPartialMatches.length > 0

    if (matched) {
      val longestMatch = getLongestMatch(delimsWithFullMatches, startPos)
      endPos = longestMatch._1
      log(Debug("FULL MATCH! String: " + input.subSequence(startPos, endPos).toString() + " StartPos: " + startPos + " EndPos: " + endPos))
      return (SearchResult.FullMatch, input.subSequence(startPos, endPos).toString(), endPos, longestMatch._2, longestMatch._3)
    } else if (partialMatched) {
      // We only care if a partial match occurred at the end of the CharBuffer
      val lastChar = input.length() - 1
      val lastPartial = delimsWithPartialMatches.flatMap(x => x.partialMatches.filter(y => y._1 == lastChar || y._2 == lastChar))
      if (lastPartial.size > 0) {
        endPos = lastPartial(lastPartial.size - 1)._2
      } else {
        endPos = lastChar
      }
      log(Debug("PARTIAL MATCH! String: " + input.subSequence(startPos, endPos).toString() + " StartPos: " + startPos + " EndPos: " + endPos))
      return (SearchResult.PartialMatch, input.subSequence(startPos, endPos).toString(), endPos, endPos, null)
    } else {
      endPos = input.length() - 1
      log(Debug("EOF! String: " + input.toString().substring(startPos) + " StartPos: " + startPos + " EndPos: " + endPos))
      return (SearchResult.NoMatch, input.toString().substring(startPos), endPos, endPos, null)
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
  def getLongestMatchOld(matchedDelims: List[Delimiter]): (Int, Int, Delimiter) = {
    if (matchedDelims.length == 0) {
      return (-1, -1, null)
    }

    // Retrieve the list of delimiters prefixed by matchedDelims(0) and sort them
    // by length
    val result = getPrefixedDelims(matchedDelims(0), matchedDelims).sortBy(x => x._2 - x._1)

    log(Debug("Prefixed Delims: " + result))

    if (result.size > 0) {
      return result(result.size - 1)
    }
    // A prefixed delimiter was not found, return the first full match
    val fullMatch = matchedDelims(0).fullMatches.toList.sortBy(x => x._1).head
    log(Debug("Matched Delims: %s %s", matchedDelims(0), matchedDelims(0).fullMatches.toList))
    (fullMatch._1, fullMatch._2, matchedDelims(0))
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
  def getLongestMatch(matchedDelims: List[Delimiter], startPos: Int = 0): (Int, Int, Delimiter) = {
    if (matchedDelims.length == 0) {
      return (-1, -1, null)
    }

    val firstFullMatch = getFirstFullMatch(matchedDelims, startPos)

    if (firstFullMatch == null) {
      return (-1, -1, null)
    }

    log(Debug("First Full Match: " + firstFullMatch))

    // Retrieve the list of delimiters prefixed by matchedDelims(0) and sort them
    // by length
    val result = getPrefixedDelims(firstFullMatch, matchedDelims).sortBy(x => x._2 - x._1)

    log(Debug("Prefixed Delims: " + result))

    if (result.size > 0) {
      return result(result.size - 1)
    }
    // A prefixed delimiter was not found, return the first full match
    firstFullMatch
  }

  def getFirstFullMatch(matchedDelims: List[Delimiter], startPos: Int = 0): (Int, Int, Delimiter) = {
    var contenders: List[(Int, Int, Delimiter)] = List.empty

    matchedDelims.foreach {
      x =>
        {
          val sortedMatches = x.fullMatches.toList.sortBy(c => (c._1, c._2))
          if (sortedMatches.length > 0) {
            val firstSortedMatch = List((sortedMatches(0)._1, sortedMatches(0)._2, x))
            contenders ++= firstSortedMatch
          }
        }
    }

    val sortedContenders = contenders.sortBy(x => (x._1, x._2))

    if (sortedContenders.length > 0) { return sortedContenders(0) }
    null
  }

  // Retrieve all of the delimiters prefixed by the prefix Delimiter but
  // do not equal the prefix.
  //
  def getPrefixedDelims(prefix: Delimiter, delimsWithFullMatches: List[Delimiter]): List[(Int, Int, Delimiter)] = {
    val q = new Queue[(Int, Int, Delimiter)]

    // Head should always return the first element in the list
    // according to Scala 2.7.4 api
    val firstFullMatch = prefix.fullMatches.toList.sortBy(_._1).head

    delimsWithFullMatches.filter(x => x != prefix).foreach(Delimiter => {
      // check that the firstFullMatch is a prefix but is not the same value
      val prefixedDelims = Delimiter.fullMatches.filter(x => firstFullMatch._1 == x._1 && firstFullMatch._2 != x._2).toList
      if (prefixedDelims.length > 0) {
        q.enqueue((prefixedDelims(0)._1, prefixedDelims(0)._2, Delimiter))
      }
    })
    q.toList
  }

  // Retrieve all of the delimiters prefixed by the prefix Delimiter but
  // do not equal the prefix.
  //
  def getPrefixedDelims(prefix: (Int, Int, Delimiter), delimsWithFullMatches: List[Delimiter]): List[(Int, Int, Delimiter)] = {
    val q = new Queue[(Int, Int, Delimiter)]

    val firstFullMatch = (prefix._1, prefix._2)

    delimsWithFullMatches.filter(x => x != prefix).foreach(Delimiter => {
      // check that the firstFullMatch is a prefix but is not the same value
      val prefixedDelims = Delimiter.fullMatches.filter(x => firstFullMatch._1 == x._1 && firstFullMatch._2 != x._2).toList
      if (prefixedDelims.length > 0) {
        q.enqueue((prefixedDelims(0)._1, prefixedDelims(0)._2, Delimiter))
      }
    })
    q.toList
  }

  def clear = {
    log(Debug("clear initiated!"))
    delimiters.foreach(x => x.clear)
    log(Debug("clear completed!"))
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
    log(Debug("getCRLFList - crlfList:\t" + crlfList))
    crlfList
  }

  // Creates a list of consecutive white space
  // This will allow us to easily tell the start and end of
  // WSP+ and WSP* characters.
  //
  // Returns a list of (start:Int, end:Int)
  // end will be -1 if we're at the end of buffer and this character is
  // and/or was preceded by a whitespace.
  //
  def getConsecutiveWSPList(input: CharBuffer): List[(Int, Int)] = {
    val wsp = new WSPDelim
    val q = new Queue[(Int, Int)]
    var start: Int = -1

    for (i <- 0 until input.length()) {
      val char: Char = input.charAt(i)

      if (wsp.checkMatch(char)) {
        // Found a WSP, is another WSP next?

        if (start == -1) {
          // No immediately preceding WSP
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
      }
    }
    val wspList = q.toList
    log(Debug("getWSPList - wspList:\t" + wspList))
    wspList
  }

  def enableStateTrace = {
    this.delimiters.foreach(d => d.stateTraceEnabled = true)
  }

  def disableStateTrace = {
    this.delimiters.foreach(d => d.stateTraceEnabled = false)
  }

  def setEscapeScheme(pEscapeKind: EscapeSchemeKind, pEsChar: String, pEsEsChar: String, pBlockStart: String, pBlockEnd: String) = {
    this.escapeSchemeKind = pEscapeKind
    this.esCharacter = pEsChar
    this.esEsCharacter = pEsEsChar
    this.esBlockStart = pBlockStart
    this.esBlockEnd = pBlockEnd
  }
  
  def setEscapeScheme(esObj: EscapeSchemeObj) = {
    this.escapeSchemeKind = esObj.escapeSchemeKind
    this.esCharacter = esObj.escapeCharacter
    this.esEsCharacter = esObj.escapeEscapeCharacter
    this.esBlockStart = esObj.escapeBlockStart
    this.esBlockEnd = esObj.escapeBlockEnd
  }

  def getCharacterList(input: CharBuffer, character: Char): List[Int] = {
    val q = new Queue[Int]

    for (i <- 0 until input.length()) {
      val char: Char = input.charAt(i)

      if (character == char) {
        q += (i)
      }
    }
    val list = q.toList.sortBy(x => x)
    list
  }

  // Returns a list of valid escapeCharacters positions who have not been escaped
  //
  def getEscapeCharacterList(input: CharBuffer): List[Int] = {
    if (this.escapeSchemeKind == EscapeSchemeKind.None || this.esCharacter.length() == 0) { return List.empty }

    val (escapeEscapeChars, escapedEscapeEscapeChars) = getEscapeEscapeCharacterList(input)

    val esCharList = getCharacterList(input, this.esCharacter.charAt(0))
    val result = new Queue[Int]

    // Removed escaped escapeChars from the list
    val allEscapeChars = esCharList.filter(x => escapedEscapeEscapeChars.filter(z => x >= z._1 && x <= z._2).length == 0)

    allEscapeChars.foreach { x =>
      {
        val prevI = x - 1
        if (prevI >= 0) {
          // Does the previous index exist in the escapeEscapeChars list?
          escapeEscapeChars.find(y => y == prevI) match {
            case None => result += x // Was not escaped, append to Queue
            case Some(_) => // Was escaped, do not append to Queue
          }
        }
      }
    }
    val escapeCharList = result.toList
    log(Debug("getEscapeCharacterList - EscapeCharacterList:\t" + escapeCharList))
    escapeCharList
  }

  // Returns a Tuple2 of (A, B)
  // Where
  //	A is a List of escapeEscapeCharacters not escaped by themselves
  //	B is a List of escaped escapeEscapeCharacters
  //
  def getEscapeEscapeCharacterList(input: CharBuffer): (List[Int], List[(Int, Int)]) = {
    if (this.escapeSchemeKind == EscapeSchemeKind.None || this.esEsCharacter.length() == 0) { return (List.empty, List.empty) }

    val esEsCharList = getCharacterList(input, this.esEsCharacter.charAt(0))
    val escapeEscapeQ = new Queue[Int]
    val escapedEscapeEscapeQ = new Queue[(Int, Int)]

    val theEsEsChar: Char = this.esEsCharacter.charAt(0)

    var i: Int = 0

    while (i < input.length()) {
      val char: Char = input.charAt(i)
      var escaped: Boolean = false

      if (char == theEsEsChar) {
        // Found escapeEscapeCharacter
        val nextI = i + 1
        if (nextI < input.length()) {
          val nextChar: Char = input.charAt(nextI)
          if (nextChar == theEsEsChar) {
            // We will escape the following escapeEscapeCharacter
            // do not append these, instead move past them.
            escapedEscapeEscapeQ += (i -> nextI)
            i += 1
            escaped = true
          }
        }
        if (!escaped) {
          escapeEscapeQ += i
        }
      }
      i += 1
    }
    val escapeEscapeList = escapeEscapeQ.toList.sortBy(x => x)
    val escapedEscapeEscapeList = escapedEscapeEscapeQ.toList.sortBy(x => x._1)
    log(Debug("getEscapeCharacterList - EscapeEscapeList:\t" + escapeEscapeList 
        + "\nEscapedEscapeEscapeList:\t" + escapedEscapeEscapeList))
    (escapeEscapeList, escapedEscapeEscapeList)
  }

  def getEscapeBlockList(input: CharBuffer, block: String): List[(Int, Int)] = {
    if (this.escapeSchemeKind == EscapeSchemeKind.None || this.escapeSchemeKind == EscapeSchemeKind.Character || block.length() == 0) { return List.empty }

    var i: Int = 0
    val blockLength = block.length()
    val inputLength = input.length()

    val q = new Queue[(Int, Int)]

    while (i < inputLength) {
      if (((i + (blockLength - 1)) < inputLength) && input.toString().regionMatches(false, i, block, 0, blockLength)) {
        q += (i -> (i + (blockLength - 1)))
      }
      i += 1
    }
    val escapeBlockList = q.toList
    log(Debug("getEscapeBlockList - EscapeBlockList:\t" + escapeBlockList))
    escapeBlockList
  }

  def getEscapeBlockStartList(input: CharBuffer): List[(Int, Int)] = {
    if (this.escapeSchemeKind == EscapeSchemeKind.None || this.escapeSchemeKind == EscapeSchemeKind.Character) { return List.empty }

    getEscapeBlockList(input, this.esBlockStart)
  }

  def getEscapeBlockEndList(input: CharBuffer): List[(Int, Int)] = {
    if (this.escapeSchemeKind == EscapeSchemeKind.None || this.escapeSchemeKind == EscapeSchemeKind.Character) { return List.empty }

    getEscapeBlockList(input, this.esBlockEnd)
  }

  // Returns a List of Tuple(StartIdx, EndIdx)
  //	Each Tuple represents the Start and End of an EscapeBlock
  //
  def getEscapeBlocks(input: CharBuffer): List[(Int, Int)] = {
    if(this.escapeSchemeKind != EscapeSchemeKind.Block){return List.empty}
    
    val (escapeEscapeList, _) = this.getEscapeEscapeCharacterList(input)
    val startList = getEscapeBlockStartList(input)
    val endList = getEscapeBlockEndList(input)
    val q = new Queue[(Int, Int)]	// Result

    val sLength = startList.length
    val eLength = endList.length
    var sIdx = 0	// Index for startList
    var eIdx = 0	// Index for endList

    if (sLength == 0) {
      return List.empty
    }
    if (sLength > 0 && eLength == 0) {
      val start = startList(sIdx)
      return List((start._1 -> -1))
    }

    // We have at least 1 item in startList and endList
    while (sIdx < sLength) {
      val start = startList(sIdx)
      var repeat: Boolean = false

      // Does this value already occur within a block?
      val qHasStart = q.filter(x => start._1 >= x._1 && start._2 <= x._2).length >= 1
     
      if (!qHasStart) {
        if (eIdx < eLength) {
          val end = endList(eIdx)

          if (end._1 > start._2 && end._2 > start._2) {
        	// This value is not a duplicate of start
            
            // Was this value escaped by the escapeEscapeCharacter?
            val isEscaped:Boolean = escapeEscapeList.filter(x => x == (end._1 - 1)).length >= 1
         
            if (!isEscaped){
              q += (start._1 -> end._2)
            }
            else {
              // This value was escaped,
              // Advance eIdx only!
              repeat = true
            }
            
          } else {
            // This value has already been added!
            // Advance eIdx only!
            repeat = true
          }
          eIdx += 1

        } else {
          // ran out of items in endList
          q += (start._1 -> -1)
          sIdx = sLength // Break this loop
        }
      }
      if (!repeat) { sIdx += 1 }
    }
    q.toList
  }

}
