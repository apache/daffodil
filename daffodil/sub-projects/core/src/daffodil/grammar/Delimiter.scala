package stringsearch.delimiter
import java.nio.CharBuffer
import java.util.regex.Pattern
import scala.util.control.Breaks
import java.util.regex.Matcher
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.util.logging.Logged

import stringsearch.constructs._
import stringsearch.constructs.CRLFState._
import stringsearch.constructs.SearchState._

class Separator extends Delimiter {
  override def toString(): String = {
    return "Separator[" + delimiterStr + "]"
  }
  
  override def typeDef = { DelimiterType.Separator }
}

class Terminator extends Delimiter{
  override def toString(): String = {
    return "Terminator[" + delimiterStr + "]"
  }
  
  override def typeDef = { DelimiterType.Terminator }
}

// A Delimiter represents a delimiter where a delimiter can be one or more
// characters long.
//
class Delimiter extends Logged {
  var stateTraceEnabled: Boolean = false
  var delimiterStr: String = "" // String representation of delimiter Ex. "%WSP;,%WSP*;"

  var delimBuf: Array[DelimBase] = Array.empty[DelimBase] /* Buffer where each cell (DelimBase) represents a character
		  												     in the delimiter string */

  var fullMatches: Set[(Int, Int)] = Set.empty[(Int, Int)] // (Start, End) of full matching delimiter
  var partialMatches: Set[(Int, Int)] = Set.empty[(Int, Int)] // (Start, End) of partial matching delimiter

  // Pre-compiled RegEx patterns for finding character classes
  lazy val NL = Pattern.compile("%(NL);", Pattern.MULTILINE)
  lazy val WSP = Pattern.compile("%(WSP);", Pattern.MULTILINE)
  lazy val WSP_Plus = Pattern.compile("%(WSP\\+);", Pattern.MULTILINE)
  lazy val WSP_Star = Pattern.compile("%(WSP\\*);", Pattern.MULTILINE)
  
  def typeDef = { DelimiterType.Delimiter }

  override def toString(): String = {
    return "Delimiter[" + delimiterStr + "]"
  }

  // Must call to create the necessary structures
  //
  def apply(pDelimiter: String) = {
    delimiterStr = pDelimiter
    delimBuf = buildDelimBuf(delimiterStr)
  }

  // Denotes whether or not this Delimiter (entire delimiter)
  // was matched completely in order.
  //
  def isMatched: Boolean = {
    return fullMatches.size > 0
  }

  // Populates the delimBuf object with an object
  // representation of the characters within the delimiter
  // string.
  //
  def buildDelimBuf(delimStr: String): Array[DelimBase] = {
    val q: Queue[DelimBase] = new Queue[DelimBase]()
    var inc = 0
    val loop = new Breaks

    var newIdx = 0 // index within delimBuf array

    var numCharClass: Int = 0

    loop.breakable {
      for (i <- 0 until delimStr.length()) {
        val idx = i + inc // Advances cursor past the Character Class

        if (idx >= delimStr.length()) {
          // ran off end of delimiter string, break!
          loop.break()
        }

        val c: Char = delimStr.charAt(idx)

        if (c == '%') {
          // Possible character class, check patterns

          // According to JavaDoc, split will always return at least
          // one result even if there is no match.
          val split = delimStr.substring(idx + 1).split("%")
          log("buildDelimBuf - SPLIT on %: " + split.toSeq.toString)

          val subStr: String = "%" + split(0)
          val (matchLength, delimObj) = findCharClasses(subStr)

          if (matchLength != -1) {
            // Have a match, add the object
            val obj = delimObj.get
            obj.index = newIdx // Index within delimBuf Array
            q += obj
            inc += matchLength - 1 // advance cursor past the Character Class
            newIdx += 1
            numCharClass += 1
          } else {
            // Not a CharClass or unrecognized,
            // therefore treat as a CharDelim
            val obj = new CharDelim(c)
            obj.index = newIdx // Index within delimBuf Array
            newIdx += 1
            q += obj
          }

        } else {
          // A CharDelim
          val obj = new CharDelim(c)
          obj.index = newIdx // Index within delimBuf Array
          newIdx += 1
          q += obj
        }
      } // END for-loop
    } // END loop-breakable
    var resDelimBuf: Array[DelimBase] = null
    if (numCharClass > 1) {
      // More than one Char Class, reduction possible!
      log("buildDelimBuf - Reduction of delimBuf possible!")
      log("buildDelimBuf - Before Reduction:\t" + printDelimBufStr)
      resDelimBuf = reduceDelimBuf(q.toArray[DelimBase])
    } else {
      // No need to reduce
      resDelimBuf = q.toArray[DelimBase]
    }
    log("buildDelimBuf - Result:\t" + printDelimBufStr(resDelimBuf))
    resDelimBuf
  }

  def printDelimBufStr(delims: Array[DelimBase]): String = {
    val sb = new StringBuilder
    var idx: Int = 0
    delims.foreach(x => {
      sb.append("\t" + idx + ":" + x.toString())
      idx += 1
    })
    sb.toString()
  }

  // Reset the state for the delimBuf
  //
  def resetDelimBuf = {
    processPartials

    for (i <- 0 until delimBuf.length) {
      delimBuf(i).clear
    }
    //TODO: Is there ever a time when we would want to reset delimIdx separately?
    resetDelim
  }

  // Reduces complicated delimiters containing consecutive WSP, WSP* and WSP+
  // character classes.
  //
  // Ex. %WSP;%WSP*;%NL;%WSP+;%WSP*
  // 	can be reduced to: %WSP+;%NL;%WSP+;
  //
  // TODO: Maybe should have an error message for the example.  What did they mean?
  // Problem because NL characters are in WSP.  Possible to consume the expected NL
  // and thus the rest of the delimiter may not match.
  //
  // Here we should note that %WSP;%WSP;%WSP; is NOT equivalent to %WSP+;
  // as WSP+ would imply that %WSP;%WSP;%WSP;%WSP; is also valid when in fact
  // it may not be.
  //
  def reduceDelimBuf(delims: Array[DelimBase]): Array[DelimBase] = {

    val q: Queue[DelimBase] = new Queue[DelimBase]()

    // Counters to keep track of WSP,+,* objects
    var numWSP: Int = 0
    var numWSP_Plus: Int = 0
    var numWSP_Star: Int = 0

    var idx: Int = 0 // To index the resultant array

    delims.foreach(delim => {
      delim match {
        case wsp: WSPDelim => numWSP += 1
        case wsp: WSPPlusDelim => numWSP_Plus += 1
        case wsp: WSPStarDelim => numWSP_Star += 1
        case _ => {
          // We've reached a non WSP delimiter, check if we've
          // previously encountered any WSP delimiter objects and
          // return the equivalent representation (if any)
          val result = getReducedDelim(numWSP, numWSP_Plus, numWSP_Star)

          result match {
            case Some(x) => {
              // WSP exists and an equivalent representation was found
              x.index = idx // Set the delimiter's index
              q += x
              idx += 1
            }
            case None => {
              // Reduction not possible, but did we come across
              // more than one WSP?

              var i = 0
              while (i < numWSP) {
                val wsp = new WSPDelim
                wsp.index = idx
                q += wsp
                idx += 1
                i += 1
              }
            }
          }

          // Set the delimiter's index, needed to
          // update the delimBuf individual node (DelimBase) state later
          delim.index = idx
          q += delim
          idx += 1

          // Reset counters
          numWSP = 0
          numWSP_Plus = 0
          numWSP_Star = 0
        }
      }
    }) // end-for-each

    // Check for leftovers in case the delimiter
    // ends in spaces
    val result = getReducedDelim(numWSP, numWSP_Plus, numWSP_Star)

    result match {
      case Some(x) => {
        x.index = idx
        q += x
      }
      case None => {
        // Reduction not possible, but did we come across
        // more than one WSP?

        var i = 0
        while (i < numWSP) {
          val wsp = new WSPDelim
          wsp.index = idx
          q += wsp
          idx += 1
          i += 1
        }
      }
    }

    q.toArray[DelimBase]
  }

  // Based upon what WSP delimiters were encountered,
  // determine the equivalent representation (if any) and return it.
  //
  def getReducedDelim(numWSP: Int, numWSP_Plus: Int, numWSP_Star: Int): Option[DelimBase] = {
    // 				TRUTH TABLE
    //		WSP		WSP+	WSP*	RESULT
    // 1	0		0		0		NONE
    // 2	0		0		1		WSP*
    // 3	0		1		0		WSP+
    // 4	0		1		1		WSP+
    // 5	1		0		0		WSP
    // 6	1		0		1		WSP+
    // 7	1		1		0		WSP+
    // 8	1		1		1		WSP+
    if (numWSP_Plus != 0) {
      // Case: 3, 4, 7, 8
      return Some(new WSPPlusDelim())
    } else if (numWSP != 0 && numWSP_Plus == 0 && numWSP_Star != 0) { // WSP+ == 0
      // Case: 6
      return Some(new WSPPlusDelim())
    } else if (numWSP == 0 && numWSP_Plus == 0 && numWSP_Star != 0) {
      // Case: 2
      return Some(new WSPStarDelim())
    } else if (numWSP == 1 && numWSP_Plus == 0 && numWSP_Star == 0) {
      // Case: 5
      return Some(new WSPDelim())
    }
    None
  }

  // Creates a RegEx representation of the delimiter.
  // Important for comparing the actual delimiter against
  // the data returned.
  // Ex. separator = "%WSP*;,%WSP*;"
  //	delimiter retrieved from data: ", "
  // There is no way that the separator text can equate to the data
  // when character classes are involved, RegEx allows us to determine
  // if the delimiter/data was in the expected format.
  //
  def buildDelimRegEx(delimiterBuf: Array[DelimBase] = delimBuf): String = {
    var sb: StringBuilder = new StringBuilder
    delimiterBuf foreach {
      delim =>
        {
          delim match {
            case nl: NLDelim => { sb.append("(\\r\\n|\\n|\\r)") }
            case wsp: WSPDelim => { sb.append("\\s") } // Single space
            case wsp: WSPPlusDelim => { sb.append("\\s+") } // One or more spaces
            case wsp: WSPStarDelim => { sb.append("\\s*") } // None or more spaces
            case char: CharDelim => { // Some character
              char.char match {
                case '[' => sb.append("\\[")
                case '\\' => sb.append("\\\\")
                case '^' => sb.append("\\^")
                case '$' => sb.append("\\$")
                case '.' => sb.append("\\.")
                case '|' => sb.append("\\|")
                case '?' => sb.append("\\?")
                case '*' => sb.append("\\*")
                case '+' => sb.append("\\+")
                case '(' => sb.append("\\(")
                case ')' => sb.append("\\)")
                case '{' => sb.append("\\{")
                case '}' => sb.append("\\}")
                case x => sb.append(x)
              }
            }
          }
        }
    }
    sb.toString()
  }

  // Returns the first character class in the String
  // or None if one is not found
  //
  def findCharClasses(str: String): (Int, Option[DelimBase]) = {
    log("findCharClasses(\"" + str + "\")")
    val mNL: Matcher = NL.matcher(str)
    val mWSP: Matcher = WSP.matcher(str)
    val mWSP_Plus: Matcher = WSP_Plus.matcher(str)
    val mWSP_Star: Matcher = WSP_Star.matcher(str)
    var length: Int = -1

    val classList: scala.collection.mutable.Map[String, (Int, Int)] = scala.collection.mutable.Map.empty

    if (mNL.find()) {
      classList += ("NL" -> (mNL.start() -> mNL.end()))
    }

    if (mWSP.find()) {
      classList += ("WSP" -> (mWSP.start() -> mWSP.end()))
    }

    if (mWSP_Plus.find()) {
      classList += ("WSP+" -> (mWSP_Plus.start() -> mWSP_Plus.end()))
    }

    if (mWSP_Star.find()) {
      classList += ("WSP*" -> (mWSP_Star.start() -> mWSP_Star.end()))
    }

    if (classList.size > 0) {
      val minItem = classList.minBy(x => x._2._1)
      length = minItem._2._2 - minItem._2._1
      val result = minItem._1 match {
        case "NL" => (length, Some(new NLDelim()))
        case "WSP" => (length, Some(new WSPDelim()))
        case "WSP+" => (length, Some(new WSPPlusDelim()))
        case "WSP*" => (length, Some(new WSPStarDelim()))
      }
      log("findCharClasses - result: " + result)
      return result
    }
    log("findCharClasses - result: " + (-1, None))
    (-1, None) // Unrecognized CharClass
  }

  def addFullMatch(startPos: Int, endPos: Int) = {
    // TODO: How do we make sure these are sorted without calling sort repeatedly?
    val x: (Int, Int) = (startPos, endPos)
    fullMatches += x
  }

  def addPartialMatch(startPos: Int, endPos: Int) = {
    // TODO: How do we make sure these are sorted without calling sort repeatedly?
    val x: (Int, Int) = (startPos, endPos)
    partialMatches += x
  }

  // Need to keep track of partial matches as this will
  // allow us to determine 'longest matches' later.
  //
  // Will also allows us to determine if a match spans
  // CharBuffers.
  //
  def processPartials = {
    val unMatched = delimBuf.filter(x => x.isMatched == false)
    val matches = delimBuf.filter(x => x.isMatched == true)

    log("\t\t\t\t\tprocessPartials: Length: " + delimBuf.length + " Matches: " + matches.length)

    if (unMatched.length > 0 && matches.length > 0) {
      val startPos: Int = matches(0).charPos
      val endPos: Int = matches(matches.length - 1).charPos
      addPartialMatch(startPos, endPos)
      log("\t\t\t\t\tprocessPartials - PartialMatch: StartPos=" + startPos + ", EndPos=" + endPos)
    }
  }

  // Determine if the CRLF list contains a CRLF at the specified
  // character position.
  //
  def crlfContains(crlfList: List[(Int, Int)], charPos: Int): (CRLFState, Int, Int) = {
    val exists = crlfList.filter(x => charPos >= x._1 && charPos <= x._2)
    val partial = crlfList.filter(x => charPos == x._1 && x._2 == -1)

    log("\t\t\t\t\tcrlfContains - exists:\t" + exists)
    log("\t\t\t\t\tcrlfContains - partial:\t" + partial)

    var result: (CRLFState, Int, Int) = null

    if (exists.length > 0) {
      result = (CRLFState.Exists, exists(0)._1, exists(0)._2)
    } else if (partial.length > 0) {
      result = (CRLFState.Partial, partial(0)._1, partial(0)._2)
    } else {
      result = (CRLFState.NotFound, -1, -1)
    }
    log("\t\t\t\t\tcrlfContains - result:\t" + result)
    result
  }

  // WARNING:	Do not alter the initial values of these variables.
  //
  // Variables used in the iteration contained
  // within the search method.  Part of state-machine.
  var wspMode: Boolean = false // True - Expecting Multiple Whitespace, False - Not Expecting Multiple Whitespace
  var delimIdx: Int = 0 // Index of delimBuf
  var charIdx: Int = 0 // Index of CharBuffer (Input String to search)
  var delimMatched: Boolean = false // Value indicating if a delimiter was fully matched

  val stateTrace: Queue[(SearchState, String)] = new Queue

  def addState(state: SearchState, msg: String) = {
    if (stateTraceEnabled) { stateTrace += (state -> msg) }
  }

  def advanceDelim = {
    if (!delimMatched) {
      delimIdx += 1

      if (delimIdx >= delimBuf.length) {
        delimIdx = 0
      }
    }
  }

  def resetDelim = {
    // If we are resetting the delimIdx then
    // an expected match was not found.
    // 
    delimIdx = 0
  }

  def advanceChar = {
    charIdx += 1
  }

  // Update the delimBuf array with the correct state
  // 
  def update(delim: DelimBase, charPosIn: Int, isMatched: Boolean, charPosEnd: Int = -1) = {
    delim.isMatched = isMatched
    delim.charPos = charPosIn

    if (charPosEnd == -1) {
      delim.charPosEnd = charPosIn
    } else {
      delim.charPosEnd = charPosEnd
    }
    delimBuf.update(delim.index, delim)
    processDelimBuf
  }

  // Resets all state-machine data
  //
  def clear = {
    resetDelimBuf
    this.fullMatches.clear()
    this.partialMatches.clear()
    this.delimIdx = 0
    this.charIdx = 0
    this.delimMatched = false
    this.stateTrace.clear()
  }

  // WARNING:	This method is a state-machine!
  //			Alteration may have undesirable consequences.
  //
  // This method iterates over the CharBuffer and delimBuf arrays.
  // Iteration is controlled by: advanceChar, advanceDelim, resetDelim
  //
  def search(input: CharBuffer, charPosIn: Int, crlfList: List[(Int, Int)], wspList: List[(Int, Int)]) = {
    val x = new WSPBase()
    charIdx = charPosIn
    log("SEARCH: charPosIn: " + charPosIn + " Delimiter: " + delimiterStr)
    while (charIdx < input.length() && charIdx > -1) {
      // This loop shall allow us to control when we
      // move on to check the next character via
      // advanceChar method.
      //
      // We separately iterate through the delimBuf array
      // via advanceDelim and resetDelim methods.

      log("\n\tDELIM_BUF: " + this.printDelimBufStr)

      val char: Char = input.charAt(charIdx)
      val delim: DelimBase = delimBuf(delimIdx)
      val matched: Boolean = delim.checkMatch(char)
      val isSpace: Boolean = {
        x.checkMatch(char)
      }
      delimMatched = false

      val header: String = "\tCHAR_IDX: " + charIdx + " DELIMBUF_IDX: " + delimIdx

      // If we are in WSP mode and this is a space, ignore it if we're not at the
      // beginning of the CharBuffer
      //
      if (wspMode && isSpace && charIdx != charPosIn && !delim.isInstanceOf[NLDelim]) {
        // We have already satisfied the WSP* or WSP+ delimiter.
        // Skip this space by advancing to the next character.
        log(header + "\tWspMode && isSpace, we have already satisfied the WSP* or WSP+ delimiter.  Skip spaces!")
        addState(SearchState.WSPModeAndSpace, header + "\tWspMode && isSpace, we have already satisfied the WSP* or WSP+ delimiter.  Skip spaces!")
        advanceChar
      } else {

        delim match {
          case nl: NLDelim if matched => {
            // Expected and found a newline
            log(header + "\tNL and isMatched" + " '" + char + "' " + char.toInt)
            val subheader = header + "\tNL and isMatched" + " '" + char + "' " + char.toInt + "\n"
            wspMode = false
            val (state, startPos, endPos) = crlfContains(crlfList, charIdx)

            state match {
              case Exists => {
                // CRLF found
                log("\t\t\t\t\tNL and CRLF" + " '" + char + "' d" + char.toInt)
                update(delim, charIdx, true, endPos)
                advanceDelim
                advanceChar
                advanceChar // Advance past LF portion of CRLF
                addState(SearchState.NLCrlfExists, subheader + "\t\t\t\t\tNL and CRLF" + " '" + char + "' d" + char.toInt)
              }
              case Partial => {
                // CR occurred at end of CharBuffer, could possibly be a CRLF
                log("\t\t\t\t\tNL and CR but might be start of CRLF" + " '" + char + "' d" + char.toInt)
                wspMode = false

                // TODO: I don't think we want to reset here, what if we want to maintain state? Blows up if we comment out.
                resetDelimBuf
                addPartialMatch(startPos, charIdx)
                advanceChar
                addState(SearchState.NLCrlfPartial, subheader + "\t\t\t\t\tNL and CR but might be start of CRLF" + " '" + char + "' d" + char.toInt)
              }
              case NotFound => {
                // CR by itself or some other NL character.
                log("\t\t\t\t\tNL " + " '" + char + "' d" + char.toInt)
                update(delim, charIdx, true)
                advanceDelim
                advanceChar
                addState(SearchState.NLCrlfNotFound, subheader + "\t\t\t\t\tNL " + " '" + char + "' d" + char.toInt)
              }
            }
          }
          case nl: NLDelim if !matched => {
            // Expected a newline but it was not found
            log(header + "\tNL and !matched" + " '" + char + "' d" + char.toInt)
            if (delimIdx == 0) {
              advanceChar
            }
            resetDelimBuf
            //advanceChar
            wspMode = false
            addState(SearchState.NLNoMatch, header + "\tNL and !matched" + " '" + char + "' d" + char.toInt)
          }
          case wspP: WSPPlusDelim if isSpace => {
            // We're looking for 1 or more spaces
            // and found at least 1.
            log(header + "\tWSPPlus and isSpace" + " '" + char + "' d" + char.toInt)
            val filteredList = wspList.filter(x => x._1 == charIdx)
            if (filteredList.length > 0) {
              // Consecutive white spaces found
              update(delim, charIdx, true, filteredList(0)._2)
            } else {
              update(delim, charIdx, true)
            }
            wspMode = true // ignore any further whitespace until encounter non-whitespace char
            advanceDelim
            advanceChar
            addState(SearchState.WSPPlusMatch, header + "\tWSPPlus and isSpace" + " '" + char + "' d" + char.toInt)
          }
          case wspS: WSPStarDelim if isSpace => {
            // We're looking for 0 or more spaces
            // and found at least 1.
            log(header + "\tWSPStar and isSpace" + " '" + char + "' d" + char.toInt)
            val filteredList = wspList.filter(x => x._1 == charIdx)
            if (filteredList.length > 0) {
              // Consecutive white spaces found
              update(delim, charIdx, true, filteredList(0)._2)
            } else {
              update(delim, charIdx, true)
            }
            wspMode = true // ignore any further whitespace until encounter non-whitespace char
            advanceDelim
            advanceChar
            addState(SearchState.WSPStarMatch, header + "\tWSPStar and isSpace" + " '" + char + "' d" + char.toInt)
          }
          case wspP: WSPPlusDelim if !isSpace => {
            // We're looking for 1 or more spaces
            // and did not find one.
            log(header + "\tWSPPlus and !isSpace" + " '" + char + "' d" + char.toInt)
            if (delimIdx == 0) {
              advanceChar
            }
            resetDelimBuf
            //advanceChar
            wspMode = false
            addState(SearchState.WSPPlusNoMatch, header + "\tWSPPlus and !isSpace" + " '" + char + "' d" + char.toInt)
          }
          case wspS: WSPStarDelim if !isSpace => {
            // We're looking for 0 or more spaces
            // and did not find one.
            // This is OK, we've satisfied this delim.
            // Advance to next delim and char
            log(header + "\tWSPStar and !isSpace" + " '" + char + "' d" + char.toInt)
            //resetDelimBuf
            update(delim, -1, true)
            advanceChar
            advanceDelim
            wspMode = false
            addState(SearchState.WSPStarNoMatch, header + "\tWSPStar and !isSpace" + " '" + char + "' d" + char.toInt)
          }
          case wsp: WSPDelim if isSpace => {
            // Expected and found a space
            log(header + "\tWSP and isSpace" + " '" + char + "' d" + char.toInt)
            update(delim, charIdx, true)
            wspMode = false
            advanceDelim
            advanceChar
            addState(SearchState.WSPMatch, header + "\tWSP and isSpace" + " '" + char + "' d" + char.toInt)
          }
          case wsp: WSPDelim if !isSpace => {
            // Expected and did not find a space
            log(header + "\tWSP and !isSpace" + " '" + char + "' d" + char.toInt)
            if (delimIdx == 0) {
              advanceChar
            }
            resetDelimBuf
            wspMode = false
            addState(SearchState.WSPNoMatch, header + "\tWSP and !isSpace" + " '" + char + "' d" + char.toInt)
          }
          case space if isSpace => {
            // Ignore if we're in wspMode
            // otherwise, we weren't expecting
            // to find a space. Reset.
            if (!wspMode) {
              log(header + "\t!WSPMode and isSpace" + " '" + char + "' d" + char.toInt)
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              addState(SearchState.SpaceAndNotWSPMode, header + "\t!WSPMode and isSpace" + " '" + char + "' d" + char.toInt)
            } else {
              // Shouldn't ever get here initial check for wspMode and isSpace should prevent
              // it.
              log(header + "\tWSPMode and isSpace" + " '" + char + "' d" + char.toInt)
              //advanceDelim
              advanceChar
              addState(SearchState.SpaceAndWSPMode, header + "\tWSPMode and isSpace" + " '" + char + "' d" + char.toInt)
            }
          }
          case other if matched => {
            log(header + "\tChar and matched" + " '" + char + "' d" + char.toInt)
            update(delim, charIdx, true)
            wspMode = false
            advanceDelim
            advanceChar
            addState(SearchState.OtherMatch, header + "\tChar and matched" + " '" + char + "' d" + char.toInt)
          }
          case other if !matched && delimBuf(0).checkMatch(char) => {
            log(header + "\tChar and !matched but might be start of next delimiter" + " '" + char + "' d" + char.toInt)
            wspMode = false
            if (delimIdx == 0) {
              advanceChar
            }
            resetDelimBuf
            addState(SearchState.OtherNoMatch, header + "\tChar and !matched but might be start of next delimiter" + " '" + char + "' d" + char.toInt)
          }
          case _ => {
            log(header + "\tNo Match!" + " '" + char + "' d" + char.toInt)
            if (delimIdx == 0) {
              advanceChar
            }
            resetDelimBuf
            wspMode = false
            addState(SearchState.NoMatch, header + "\tNo Match!" + " '" + char + "' d" + char.toInt)
            //advanceChar
          }
        } // end-delim-match
      } // end-if
    } // end-while
    processDelimBuf
    processPartials
    log("END SEARCH DELIM: " + delimiterStr + "\n")
  }

  def isCharBlockEscaped(charPos: Int, escapeBlockList: List[(Int, Int)]): Boolean = {
    val isEscaped: Boolean = escapeBlockList.filter(x => charPos >= x._1 && charPos <= x._2).length >= 1
    isEscaped
  }

  def isCharEscaped(charPos: Int, escapeCharList: List[Int]): Boolean = {
    val isEscaped: Boolean = escapeCharList.filter(x => (charPos-1) == x).length >= 1
    isEscaped
  }

  // WARNING:	This method is a state-machine!
  //			Alteration may have undesirable consequences.
  //
  // This method iterates over the CharBuffer and delimBuf arrays.
  // Iteration is controlled by: advanceChar, advanceDelim, resetDelim
  //
  def searchWithEscapeSchemeBlock(input: CharBuffer, charPosIn: Int,
    crlfList: List[(Int, Int)], wspList: List[(Int, Int)],
    escapeEscapeChar: List[(Int)], escapeBlockList: List[(Int, Int)]) = {
    val x = new WSPBase()
    var isEscapeMode: Boolean = false

    charIdx = charPosIn
    log("SEARCH_EscapeSchemeBlock: charPosIn: " + charPosIn + " Delimiter: " + delimiterStr)
    while (charIdx < input.length() && charIdx > -1) {
      // This loop shall allow us to control when we
      // move on to check the next character via
      // advanceChar method.
      //
      // We separately iterate through the delimBuf array
      // via advanceDelim and resetDelim methods.

      log("\n\tDELIM_BUF: " + this.printDelimBufStr)

      val char: Char = input.charAt(charIdx)
      val delim: DelimBase = delimBuf(delimIdx)
      val matched: Boolean = delim.checkMatch(char)
      val isSpace: Boolean = {
        x.checkMatch(char)
      }
      delimMatched = false

      val header: String = "\tCHAR_IDX: " + charIdx + " DELIMBUF_IDX: " + delimIdx

      // Is this char within an Escape Block?
      if (isCharBlockEscaped(charIdx, escapeBlockList)) {
        log(header + "\tEncountered Block Escape!")
        advanceChar
        resetDelimBuf // TODO: Is this correct? Does an escape require a reset of DelimSearch?
      } else {
        // If we are in WSP mode and this is a space, ignore it if we're not at the
        // beginning of the CharBuffer
        //
        if (wspMode && isSpace && charIdx != charPosIn && !delim.isInstanceOf[NLDelim]) {
          // We have already satisfied the WSP* or WSP+ delimiter.
          // Skip this space by advancing to the next character.
          log(header + "\tWspMode && isSpace, we have already satisfied the WSP* or WSP+ delimiter.  Skip spaces!")
          addState(SearchState.WSPModeAndSpace, header + "\tWspMode && isSpace, we have already satisfied the WSP* or WSP+ delimiter.  Skip spaces!")
          advanceChar
        } else {

          delim match {
            case nl: NLDelim if matched => {
              // Expected and found a newline
              log(header + "\tNL and isMatched" + " '" + char + "' " + char.toInt)
              val subheader = header + "\tNL and isMatched" + " '" + char + "' " + char.toInt + "\n"
              wspMode = false
              val (state, startPos, endPos) = crlfContains(crlfList, charIdx)

              state match {
                case Exists => {
                  // CRLF found
                  log("\t\t\t\t\tNL and CRLF" + " '" + char + "' d" + char.toInt)
                  update(delim, charIdx, true, endPos)
                  advanceDelim
                  advanceChar
                  advanceChar // Advance past LF portion of CRLF
                  addState(SearchState.NLCrlfExists, subheader + "\t\t\t\t\tNL and CRLF" + " '" + char + "' d" + char.toInt)
                }
                case Partial => {
                  // CR occurred at end of CharBuffer, could possibly be a CRLF
                  log("\t\t\t\t\tNL and CR but might be start of CRLF" + " '" + char + "' d" + char.toInt)
                  wspMode = false

                  // TODO: I don't think we want to reset here, what if we want to maintain state? Blows up if we comment out.
                  resetDelimBuf
                  addPartialMatch(startPos, charIdx)
                  advanceChar
                  addState(SearchState.NLCrlfPartial, subheader + "\t\t\t\t\tNL and CR but might be start of CRLF" + " '" + char + "' d" + char.toInt)
                }
                case NotFound => {
                  // CR by itself or some other NL character.
                  log("\t\t\t\t\tNL " + " '" + char + "' d" + char.toInt)
                  update(delim, charIdx, true)
                  advanceDelim
                  advanceChar
                  addState(SearchState.NLCrlfNotFound, subheader + "\t\t\t\t\tNL " + " '" + char + "' d" + char.toInt)
                }
              }
            }
            case nl: NLDelim if !matched => {
              // Expected a newline but it was not found
              log(header + "\tNL and !matched" + " '" + char + "' d" + char.toInt)
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              //advanceChar
              wspMode = false
              addState(SearchState.NLNoMatch, header + "\tNL and !matched" + " '" + char + "' d" + char.toInt)
            }
            case wspP: WSPPlusDelim if isSpace => {
              // We're looking for 1 or more spaces
              // and found at least 1.
              log(header + "\tWSPPlus and isSpace" + " '" + char + "' d" + char.toInt)
              val filteredList = wspList.filter(x => x._1 == charIdx)
              if (filteredList.length > 0) {
                // Consecutive white spaces found
                update(delim, charIdx, true, filteredList(0)._2)
              } else {
                update(delim, charIdx, true)
              }
              wspMode = true // ignore any further whitespace until encounter non-whitespace char
              advanceDelim
              advanceChar
              addState(SearchState.WSPPlusMatch, header + "\tWSPPlus and isSpace" + " '" + char + "' d" + char.toInt)
            }
            case wspS: WSPStarDelim if isSpace => {
              // We're looking for 0 or more spaces
              // and found at least 1.
              log(header + "\tWSPStar and isSpace" + " '" + char + "' d" + char.toInt)
              val filteredList = wspList.filter(x => x._1 == charIdx)
              if (filteredList.length > 0) {
                // Consecutive white spaces found
                update(delim, charIdx, true, filteredList(0)._2)
              } else {
                update(delim, charIdx, true)
              }
              wspMode = true // ignore any further whitespace until encounter non-whitespace char
              advanceDelim
              advanceChar
              addState(SearchState.WSPStarMatch, header + "\tWSPStar and isSpace" + " '" + char + "' d" + char.toInt)
            }
            case wspP: WSPPlusDelim if !isSpace => {
              // We're looking for 1 or more spaces
              // and did not find one.
              log(header + "\tWSPPlus and !isSpace" + " '" + char + "' d" + char.toInt)
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              //advanceChar
              wspMode = false
              addState(SearchState.WSPPlusNoMatch, header + "\tWSPPlus and !isSpace" + " '" + char + "' d" + char.toInt)
            }
            case wspS: WSPStarDelim if !isSpace => {
              // We're looking for 0 or more spaces
              // and did not find one.
              // This is OK, we've satisfied this delim.
              // Advance to next delim and char
              log(header + "\tWSPStar and !isSpace" + " '" + char + "' d" + char.toInt)
              //resetDelimBuf
              update(delim, -1, true)
              advanceChar
              advanceDelim
              wspMode = false
              addState(SearchState.WSPStarNoMatch, header + "\tWSPStar and !isSpace" + " '" + char + "' d" + char.toInt)
            }
            case wsp: WSPDelim if isSpace => {
              // Expected and found a space
              log(header + "\tWSP and isSpace" + " '" + char + "' d" + char.toInt)
              update(delim, charIdx, true)
              wspMode = false
              advanceDelim
              advanceChar
              addState(SearchState.WSPMatch, header + "\tWSP and isSpace" + " '" + char + "' d" + char.toInt)
            }
            case wsp: WSPDelim if !isSpace => {
              // Expected and did not find a space
              log(header + "\tWSP and !isSpace" + " '" + char + "' d" + char.toInt)
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              wspMode = false
              addState(SearchState.WSPNoMatch, header + "\tWSP and !isSpace" + " '" + char + "' d" + char.toInt)
            }
            case space if isSpace => {
              // Ignore if we're in wspMode
              // otherwise, we weren't expecting
              // to find a space. Reset.
              if (!wspMode) {
                log(header + "\t!WSPMode and isSpace" + " '" + char + "' d" + char.toInt)
                if (delimIdx == 0) {
                  advanceChar
                }
                resetDelimBuf
                addState(SearchState.SpaceAndNotWSPMode, header + "\t!WSPMode and isSpace" + " '" + char + "' d" + char.toInt)
              } else {
                // Shouldn't ever get here initial check for wspMode and isSpace should prevent
                // it.
                log(header + "\tWSPMode and isSpace" + " '" + char + "' d" + char.toInt)
                //advanceDelim
                advanceChar
                addState(SearchState.SpaceAndWSPMode, header + "\tWSPMode and isSpace" + " '" + char + "' d" + char.toInt)
              }
            }
            case other if matched => {
              log(header + "\tChar and matched" + " '" + char + "' d" + char.toInt)
              update(delim, charIdx, true)
              wspMode = false
              advanceDelim
              advanceChar
              addState(SearchState.OtherMatch, header + "\tChar and matched" + " '" + char + "' d" + char.toInt)
            }
            case other if !matched && delimBuf(0).checkMatch(char) => {
              log(header + "\tChar and !matched but might be start of next delimiter" + " '" + char + "' d" + char.toInt)
              wspMode = false
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              addState(SearchState.OtherNoMatch, header + "\tChar and !matched but might be start of next delimiter" + " '" + char + "' d" + char.toInt)
            }
            case _ => {
              log(header + "\tNo Match!" + " '" + char + "' d" + char.toInt)
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              wspMode = false
              addState(SearchState.NoMatch, header + "\tNo Match!" + " '" + char + "' d" + char.toInt)
              //advanceChar
            }
          } // end-delim-match
        } // end-if
      }// end-if

    } // end-while
    processDelimBuf
    processPartials
    log("END SEARCH_EscapeSchemeBlock: " + delimiterStr + "\n")
  }
  
  // WARNING:	This method is a state-machine!
  //			Alteration may have undesirable consequences.
  //
  // This method iterates over the CharBuffer and delimBuf arrays.
  // Iteration is controlled by: advanceChar, advanceDelim, resetDelim
  //
  def searchWithEscapeSchemeCharacter(input: CharBuffer, charPosIn: Int,
    crlfList: List[(Int, Int)], wspList: List[(Int, Int)],
    escapeCharList: List[(Int)]) = {
    val x = new WSPBase()
    var isEscapeMode: Boolean = false

    charIdx = charPosIn
    log("SEARCH_EscapeSchemeCharacter: charPosIn: " + charPosIn + " Delimiter: " + delimiterStr)
    while (charIdx < input.length() && charIdx > -1) {
      // This loop shall allow us to control when we
      // move on to check the next character via
      // advanceChar method.
      //
      // We separately iterate through the delimBuf array
      // via advanceDelim and resetDelim methods.

      log("\n\tDELIM_BUF: " + this.printDelimBufStr)

      val char: Char = input.charAt(charIdx)
      val delim: DelimBase = delimBuf(delimIdx)
      val matched: Boolean = delim.checkMatch(char)
      val isSpace: Boolean = {
        x.checkMatch(char)
      }
      delimMatched = false

      val header: String = "\tCHAR_IDX: " + charIdx + " DELIMBUF_IDX: " + delimIdx

      // Is this char within an Escape Block?
      if (isCharEscaped(charIdx, escapeCharList)) {
        log(header + "\tEncountered Character Escape!")
        advanceChar
        resetDelimBuf // TODO: Is this correct? Does an escape require a reset of DelimSearch?
      } else {
        // If we are in WSP mode and this is a space, ignore it if we're not at the
        // beginning of the CharBuffer
        //
        if (wspMode && isSpace && charIdx != charPosIn && !delim.isInstanceOf[NLDelim]) {
          // We have already satisfied the WSP* or WSP+ delimiter.
          // Skip this space by advancing to the next character.
          log(header + "\tWspMode && isSpace, we have already satisfied the WSP* or WSP+ delimiter.  Skip spaces!")
          addState(SearchState.WSPModeAndSpace, header + "\tWspMode && isSpace, we have already satisfied the WSP* or WSP+ delimiter.  Skip spaces!")
          advanceChar
        } else {

          delim match {
            case nl: NLDelim if matched => {
              // Expected and found a newline
              log(header + "\tNL and isMatched" + " '" + char + "' " + char.toInt)
              val subheader = header + "\tNL and isMatched" + " '" + char + "' " + char.toInt + "\n"
              wspMode = false
              val (state, startPos, endPos) = crlfContains(crlfList, charIdx)

              state match {
                case Exists => {
                  // CRLF found
                  log("\t\t\t\t\tNL and CRLF" + " '" + char + "' d" + char.toInt)
                  update(delim, charIdx, true, endPos)
                  advanceDelim
                  advanceChar
                  advanceChar // Advance past LF portion of CRLF
                  addState(SearchState.NLCrlfExists, subheader + "\t\t\t\t\tNL and CRLF" + " '" + char + "' d" + char.toInt)
                }
                case Partial => {
                  // CR occurred at end of CharBuffer, could possibly be a CRLF
                  log("\t\t\t\t\tNL and CR but might be start of CRLF" + " '" + char + "' d" + char.toInt)
                  wspMode = false

                  // TODO: I don't think we want to reset here, what if we want to maintain state? Blows up if we comment out.
                  resetDelimBuf
                  addPartialMatch(startPos, charIdx)
                  advanceChar
                  addState(SearchState.NLCrlfPartial, subheader + "\t\t\t\t\tNL and CR but might be start of CRLF" + " '" + char + "' d" + char.toInt)
                }
                case NotFound => {
                  // CR by itself or some other NL character.
                  log("\t\t\t\t\tNL " + " '" + char + "' d" + char.toInt)
                  update(delim, charIdx, true)
                  advanceDelim
                  advanceChar
                  addState(SearchState.NLCrlfNotFound, subheader + "\t\t\t\t\tNL " + " '" + char + "' d" + char.toInt)
                }
              }
            }
            case nl: NLDelim if !matched => {
              // Expected a newline but it was not found
              log(header + "\tNL and !matched" + " '" + char + "' d" + char.toInt)
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              //advanceChar
              wspMode = false
              addState(SearchState.NLNoMatch, header + "\tNL and !matched" + " '" + char + "' d" + char.toInt)
            }
            case wspP: WSPPlusDelim if isSpace => {
              // We're looking for 1 or more spaces
              // and found at least 1.
              log(header + "\tWSPPlus and isSpace" + " '" + char + "' d" + char.toInt)
              val filteredList = wspList.filter(x => x._1 == charIdx)
              if (filteredList.length > 0) {
                // Consecutive white spaces found
                update(delim, charIdx, true, filteredList(0)._2)
              } else {
                update(delim, charIdx, true)
              }
              wspMode = true // ignore any further whitespace until encounter non-whitespace char
              advanceDelim
              advanceChar
              addState(SearchState.WSPPlusMatch, header + "\tWSPPlus and isSpace" + " '" + char + "' d" + char.toInt)
            }
            case wspS: WSPStarDelim if isSpace => {
              // We're looking for 0 or more spaces
              // and found at least 1.
              log(header + "\tWSPStar and isSpace" + " '" + char + "' d" + char.toInt)
              val filteredList = wspList.filter(x => x._1 == charIdx)
              if (filteredList.length > 0) {
                // Consecutive white spaces found
                update(delim, charIdx, true, filteredList(0)._2)
              } else {
                update(delim, charIdx, true)
              }
              wspMode = true // ignore any further whitespace until encounter non-whitespace char
              advanceDelim
              advanceChar
              addState(SearchState.WSPStarMatch, header + "\tWSPStar and isSpace" + " '" + char + "' d" + char.toInt)
            }
            case wspP: WSPPlusDelim if !isSpace => {
              // We're looking for 1 or more spaces
              // and did not find one.
              log(header + "\tWSPPlus and !isSpace" + " '" + char + "' d" + char.toInt)
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              //advanceChar
              wspMode = false
              addState(SearchState.WSPPlusNoMatch, header + "\tWSPPlus and !isSpace" + " '" + char + "' d" + char.toInt)
            }
            case wspS: WSPStarDelim if !isSpace => {
              // We're looking for 0 or more spaces
              // and did not find one.
              // This is OK, we've satisfied this delim.
              // Advance to next delim and char
              log(header + "\tWSPStar and !isSpace" + " '" + char + "' d" + char.toInt)
              //resetDelimBuf
              update(delim, -1, true)
              advanceChar
              advanceDelim
              wspMode = false
              addState(SearchState.WSPStarNoMatch, header + "\tWSPStar and !isSpace" + " '" + char + "' d" + char.toInt)
            }
            case wsp: WSPDelim if isSpace => {
              // Expected and found a space
              log(header + "\tWSP and isSpace" + " '" + char + "' d" + char.toInt)
              update(delim, charIdx, true)
              wspMode = false
              advanceDelim
              advanceChar
              addState(SearchState.WSPMatch, header + "\tWSP and isSpace" + " '" + char + "' d" + char.toInt)
            }
            case wsp: WSPDelim if !isSpace => {
              // Expected and did not find a space
              log(header + "\tWSP and !isSpace" + " '" + char + "' d" + char.toInt)
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              wspMode = false
              addState(SearchState.WSPNoMatch, header + "\tWSP and !isSpace" + " '" + char + "' d" + char.toInt)
            }
            case space if isSpace => {
              // Ignore if we're in wspMode
              // otherwise, we weren't expecting
              // to find a space. Reset.
              if (!wspMode) {
                log(header + "\t!WSPMode and isSpace" + " '" + char + "' d" + char.toInt)
                if (delimIdx == 0) {
                  advanceChar
                }
                resetDelimBuf
                addState(SearchState.SpaceAndNotWSPMode, header + "\t!WSPMode and isSpace" + " '" + char + "' d" + char.toInt)
              } else {
                // Shouldn't ever get here initial check for wspMode and isSpace should prevent
                // it.
                log(header + "\tWSPMode and isSpace" + " '" + char + "' d" + char.toInt)
                //advanceDelim
                advanceChar
                addState(SearchState.SpaceAndWSPMode, header + "\tWSPMode and isSpace" + " '" + char + "' d" + char.toInt)
              }
            }
            case other if matched => {
              log(header + "\tChar and matched" + " '" + char + "' d" + char.toInt)
              update(delim, charIdx, true)
              wspMode = false
              advanceDelim
              advanceChar
              addState(SearchState.OtherMatch, header + "\tChar and matched" + " '" + char + "' d" + char.toInt)
            }
            case other if !matched && delimBuf(0).checkMatch(char) => {
              log(header + "\tChar and !matched but might be start of next delimiter" + " '" + char + "' d" + char.toInt)
              wspMode = false
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              addState(SearchState.OtherNoMatch, header + "\tChar and !matched but might be start of next delimiter" + " '" + char + "' d" + char.toInt)
            }
            case _ => {
              log(header + "\tNo Match!" + " '" + char + "' d" + char.toInt)
              if (delimIdx == 0) {
                advanceChar
              }
              resetDelimBuf
              wspMode = false
              addState(SearchState.NoMatch, header + "\tNo Match!" + " '" + char + "' d" + char.toInt)
              //advanceChar
            }
          } // end-delim-match
        } // end-if
      }// end-if

    } // end-while
    processDelimBuf
    processPartials
    log("END SEARCH_EscapeSchemeCharacter: " + delimiterStr + "\n")
  }

  // Determines if all characters within a delimBuf were
  // satisfied/matched in order.  
  //
  // If they were then the full delimiter
  // has been matched and the location within CharBuf is stored in FullMatch list.
  //
  def processDelimBuf = {
    if (delimBuf.length > 0) {
      val allMatched = delimBuf.filter(x => x.isMatched == false).length == 0
      if (allMatched) {
        delimMatched = true
        val startPos = delimBuf(0).charPos
        val end = delimBuf(delimBuf.length - 1)
        var endPos: Int = end.charPosEnd

        // CRLF will use charPosEnd as end
        if (end.charPosEnd != -1) {
          endPos = end.charPosEnd
        }
        log("\t\t\t\t\tprocessDelimBuf - FullMatch: StartPos=" + startPos + ", EndPos=" + endPos)

        if (startPos != -1 && endPos != -1) {
          addFullMatch(startPos, endPos)
        }
        resetDelimBuf
      }
    }
  }

  def printDelimBuf = {
    delimBuf foreach {
      x => x.print
    }
  }

  def printDelimBufStr = {
    val sb = new StringBuilder
    var idx: Int = 0
    delimBuf foreach {
      x =>
        {
          sb.append("\t" + idx + "_" + x.toString())
          idx += 1
        }
    }
    sb.append("\n")
    sb.toString()
  }

  def print = {
    //log("\n====\nDelimiter: " + delimiterStr)
    log("\n====\n" + this.toString())
    log("FULL MATCHES: ")
    fullMatches.toList.sortBy(_._1) foreach {
      x => log("\t" + x._1 + "\t" + x._2)
    }
    log("PARTIAL MATCHES: ")
    partialMatches.toList.sortBy(_._1) foreach {
      x => log("\t" + x._1 + "\t" + x._2)
    }
    log("\n====\n")
  }
}

abstract class DelimBase extends Base with Logged {
  def typeName: String
  def print
  def printStr: String
  override def toString(): String = {
    return typeName
  }
}

trait Base {
  var isMatched: Boolean = false
  var index: Int = -1
  var charPos: Int = -1
  var charPosEnd: Int = -1

  def clear = {
    isMatched = false
    charPos = -1
    charPosEnd = -1
  }

  def checkMatch(charIn: Char): Boolean
}

class CharDelim(val char: Char) extends DelimBase {
  def checkMatch(charIn: Char): Boolean = {
    val matched = charIn == char
    matched
  }

  lazy val typeName = "CharDelim"
  def print = {
    log("\t\t\t" + typeName + ": '" + char + "' d" + char.toInt + " isMatched: " + isMatched.toString())
  }

  def printStr = {
    val res = typeName + "(" + char + ")"
    res
  }

  override def toString(): String = {
    return typeName + "[" + char + "]"
  }
}

trait CharacterClass {
  def convertUnicodeToChar(unicode: String): Char = {
    val c: Char = Integer.parseInt(unicode.substring(2), 16).asInstanceOf[Char]
    c
  }
}

class NLDelim extends DelimBase with CharacterClass {
  lazy val typeName = "NLDelim"

  lazy val LF: Char = { convertUnicodeToChar("\\u000A") }
  lazy val CR: Char = { convertUnicodeToChar("\\u000D") }
  lazy val NEL: Char = { convertUnicodeToChar("\\u0085") }
  lazy val LS: Char = { convertUnicodeToChar("\\u2028") }

  def checkMatch(charIn: Char): Boolean = {
    charIn match {
      case LF | CR | NEL | LS => isMatched = true
      case _ => isMatched = false
    }
    isMatched
  }

  def print = {
    log("\t\t\t" + typeName + ": NL" + " isMatched: " + isMatched.toString())
  }
  def printStr = {
    val res = typeName
    res
  }
}

trait WSP extends CharacterClass {
  lazy val CTRL0: Char = { convertUnicodeToChar("\\u0009") }
  lazy val CTRL1: Char = { convertUnicodeToChar("\\u000A") }
  lazy val CTRL2: Char = { convertUnicodeToChar("\\u000B") }
  lazy val CTRL3: Char = { convertUnicodeToChar("\\u000C") }
  lazy val CTRL4: Char = { convertUnicodeToChar("\\u000D") }

  lazy val SPACE: Char = { convertUnicodeToChar("\\u0020") }

  lazy val NEL: Char = { convertUnicodeToChar("\\u0085") }

  lazy val NBSP: Char = { convertUnicodeToChar("\\u00A0") }

  lazy val OGHAM: Char = { convertUnicodeToChar("\\u1680") }
  lazy val MONG: Char = { convertUnicodeToChar("\\u180E") }

  lazy val SP0: Char = { convertUnicodeToChar("\\u2000") }
  lazy val SP1: Char = { convertUnicodeToChar("\\u2001") }
  lazy val SP2: Char = { convertUnicodeToChar("\\u2002") }
  lazy val SP3: Char = { convertUnicodeToChar("\\u2003") }
  lazy val SP4: Char = { convertUnicodeToChar("\\u2004") }
  lazy val SP5: Char = { convertUnicodeToChar("\\u2005") }
  lazy val SP6: Char = { convertUnicodeToChar("\\u2006") }
  lazy val SP7: Char = { convertUnicodeToChar("\\u2007") }
  lazy val SP8: Char = { convertUnicodeToChar("\\u2008") }
  lazy val SP9: Char = { convertUnicodeToChar("\\u2009") }
  lazy val SP10: Char = { convertUnicodeToChar("\\u200A") }

  lazy val LSP: Char = { convertUnicodeToChar("\\u2028") }
  lazy val PSP: Char = { convertUnicodeToChar("\\u2029") }
  lazy val NARROW: Char = { convertUnicodeToChar("\\u202F") }
  lazy val MED: Char = { convertUnicodeToChar("\\u205F") }
  lazy val IDE: Char = { convertUnicodeToChar("\\u3000") }
}

class WSPBase extends DelimBase with WSP {
  lazy val typeName = "WSPBase"
  def checkMatch(charIn: Char): Boolean = {
    charIn match {
      case CTRL0 | CTRL1 | CTRL2 | CTRL3 | CTRL4 => isMatched = true
      case SPACE | NEL | NBSP | OGHAM | MONG => isMatched = true
      case SP0 | SP1 | SP2 | SP3 | SP4 | SP5 | SP6 | SP7 | SP8 | SP9 | SP10 => isMatched = true
      case LSP | PSP | NARROW | MED | IDE => isMatched = true
      case _ => isMatched = false
    }
    isMatched
  }
  def print = {
    log("\t\t\t" + typeName + ": WSPBase" + " isMatched: " + isMatched.toString())
  }
  def printStr = {
    val res = typeName
    res
  }
}

class WSPDelim extends WSPBase with WSP {
  override lazy val typeName = "WSPDelim"
  override def print = {
    log("\t\t\t" + typeName + ": WSP" + " isMatched: " + isMatched.toString())
  }
  override def printStr = {
    val res = typeName
    res
  }
}

class WSPPlusDelim extends WSPBase with WSP {
  override lazy val typeName = "WSP+Delim"
  override def print = {
    log("\t\t\t" + typeName + ": WSP+" + " isMatched: " + isMatched.toString())
  }
  override def printStr = {
    val res = typeName
    res
  }
}

class WSPStarDelim extends WSPBase with WSP {
  override lazy val typeName = "WSP*Delim"
  override def print = {
    log("\t\t\t" + typeName + ": WSP*" + " isMatched: " + isMatched.toString())
  }
  override def printStr = {
    val res = typeName
    res
  }
}