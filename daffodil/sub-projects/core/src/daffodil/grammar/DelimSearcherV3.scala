package stringsearch.DelimSearcherV3
import java.util.regex.Pattern
import java.util.regex.Matcher
import scala.collection.mutable.Queue
import scala.collection.mutable.LinkedList
import scala.util.control.Breaks
import java.nio.CharBuffer
import scala.collection.mutable.Set

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
    if (clearState){
      clear
    }
    var matched: Boolean = false
    var partialMatched: Boolean = false
    var EOF: Boolean = false
    var endPos: Int = -1

    while (!matched && !EOF && !partialMatched) {
      // Search entire CharBuffer for first delimiter
      // if a full match is not found, was there a partial
      // match at the end of the CharBuffer?  If so, we need more data.
      // If not, repeat above with second delimiter.
      delimiters foreach {
        node =>
          {
            node.search(input, startPos)
            
            matched = node.fullMatches.size > 0
            partialMatched = node.partialMatches.size > 0
         
            if (matched) {
              // Full match
              val firstMatch = node.fullMatches.toList.sortBy(_._1).head
              endPos = firstMatch._1
            } else if (partialMatched) {
              val sortedMatches = node.partialMatches.toList.sortBy(_._1)
              endPos = sortedMatches(sortedMatches.length -1)._1
            }
          }
      } // end-delimiters-for-each
      EOF = true
    }
    
    if (matched) {
      return (SearchResult.FullMatch, input.subSequence(0, endPos).toString(), endPos)
    } else if (partialMatched) {
      return (SearchResult.PartialMatch, input.subSequence(0, endPos).toString(), endPos)
    } else {
      return (SearchResult.EOF, input.toString(), endPos)
    }
  }

  def clear = {
    delimiters.foreach(x => x.clear)
  }
}

class DelimNode {
  var delimiter: String = ""
  var indexes: Set[(Int, Int)] = Set.empty[(Int, Int)] // (Start, End) of full matching delimiter
  var delimBuf: Array[DelimBase] = Array.empty[DelimBase] // Buffer where each node (DelimBase) represents a character
  // in the delimiter string

  var fullMatches: Set[(Int, Int)] = Set.empty[(Int, Int)]
  var partialMatches: Set[(Int, Int)] = Set.empty[(Int, Int)]

  lazy val NL = Pattern.compile("%(NL);", Pattern.MULTILINE)
  lazy val WSP = Pattern.compile("%(WSP);", Pattern.MULTILINE)
  lazy val WSP_Plus = Pattern.compile("%(WSP\\+);", Pattern.MULTILINE)
  lazy val WSP_Star = Pattern.compile("%(WSP\\*);", Pattern.MULTILINE)

  def apply(pDelimiter: String) = {
    delimiter = pDelimiter
    buildDelimBuf
  }

  def isMatched: Boolean = {
    return indexes.size > 0
  }

  // Populates the delimBuf object with an object
  // representation of the characters within the delimiter
  // string.
  //
  def buildDelimBuf = {
    val q: Queue[DelimBase] = new Queue[DelimBase]()
    var inc = 0
    val loop = new Breaks

    var newIdx = 0 // index within delimBuf array

    var numCharClass: Int = 0

    loop.breakable {
      for (i <- 0 until delimiter.length()) {
        val idx = i + inc // Advances cursor past the Character Class

        if (idx >= delimiter.length()) {
          // ran off end of delimiter string, break!
          loop.break()
        }

        val c: Char = delimiter.charAt(idx)

        if (c == '%') {
          // Possible character class, check patterns
          val subStr: String = delimiter.substring(idx)
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

    if (numCharClass > 1) {
      // More than one Char Class, reduction possible!
      delimBuf = reduceDelimBuf(q.toArray[DelimBase])
    } else {
      // No need to reduce
      delimBuf = q.toArray[DelimBase]
    }
  }

  // Reduces complicated delimiters containing WSP, WSP* and WSP+
  // character classes.
  //
  // Ex. %WSP;%WSP*;%NL;%WSP+;%WSP*
  // 	can be reduced to: %WSP+;%NL;%WSP+;
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
            case None => // Do Nothing
          }

          delim.index = idx // Set the delimiter's index
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
      case None => // Nothing
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
    } else if (numWSP != 0 && numWSP_Plus == 0 && numWSP_Star == 0) {
      // Case: 5
      return Some(new WSPDelim())
    }
    None
  }

  def resetDelimBuf = {
    processPartials

    for (i <- 0 until delimBuf.length) {
      delimBuf(i).clear
    }
  }
  
  def processPartials = {
    val unMatched = delimBuf.filter(x => x.isMatched == false)
    val matches = delimBuf.filter( x => x.isMatched == true)

    if (unMatched.length > 0 && matches.length > 0) {
      val startPos: Int = matches(0).charPos
      val endPos: Int = matches(matches.length - 1).charPos
      addPartialMatch(startPos, endPos)
    }
  }

  def clear = {
    resetDelimBuf
    indexes.clear
    fullMatches.clear()
    partialMatches.clear()
    this.delimIdx = 0
    this.charIdx = 0
  }

  def findCharClasses(str: String): (Int, Option[DelimBase]) = {
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
      return result
    }

    (-1, None) // Unrecognized CharClass
  }

  def add(startPos: Int, endPos: Int) = {
    // TODO: Should make sure indexes gets sorted by startPos
    val x: (Int, Int) = (startPos, endPos)
    indexes += x
  }

  def addFullMatch(startPos: Int, endPos: Int) = {
    val x: (Int, Int) = (startPos, endPos)
    fullMatches += x
  }

  def addPartialMatch(startPos: Int, endPos: Int) = {
    val x: (Int, Int) = (startPos, endPos)
    partialMatches += x
  }

  // Update the delimBuf array with the correct state
  // 
  def update(delim: DelimBase, charPosIn: Int, isMatched: Boolean) = {
    delim.isMatched = isMatched
    delim.charPos = charPosIn
    delimBuf.update(delim.index, delim)
    processDelimBuf
  }

  var lastCharSpace: Boolean = false
  var spaceStart: Int = -1
  var wspMode: Boolean = false
  var delimIdx: Int = 0
  var charIdx: Int = 0

  def resetSpaceVars = {
    lastCharSpace = false
    spaceStart = -1
  }

  def advanceDelim = {
    delimIdx += 1
    
    if (delimIdx >= delimBuf.length){
      delimIdx = 0
    }
  }
  
  def resetDelim = {
    delimIdx = 0
  }
  
  def advanceChar = {
    charIdx += 1
  }
  
  def search(input: CharBuffer, charPosIn: Int) = {
    val x = new WSPBase()
    charIdx = charPosIn
    
    while (charIdx < input.length()){
      // This loop shall allow us to control when we
      // move on to check the next character via
      // advanceChar method.
      //
      // We separately iterate through the delimBuf array
      // via advanceDelim and resetDelim methods.
      
      val char: Char = input.charAt(charIdx)
      val delim: DelimBase = delimBuf(delimIdx)
      val matched: Boolean = delim.checkMatch(char)
      val isSpace: Boolean = {
        x.checkMatch(char)
      }
      
      // if we are in WSP mode and this is a space, ignore it
      if (wspMode && isSpace) {
        // We have already satisfied the WSP* or WSP+ delimiter.
        // Skip this space by advancing to the next character.
        println("wspMode && isSpace, we have already satisfied the WSP* or WSP+ delimiter.  Skip spaces!")
        advanceChar
      } else {
        println("CHAR_IDX: " + charIdx + " DELIMBUF_IDX: " + delimIdx)
        delim match {
          case nl: NLDelim if matched => {
            // Expected and found a newline
            println("NL and isMatched" + " '" + char + "' " + char.toInt)
            update(delim, charIdx, true)
            wspMode = false
            advanceDelim
            advanceChar
          }
          case nl: NLDelim if !matched => {
            // Expected a newline but it was not found
            println("NL and !matched" + " '" + char + "' " + char.toInt)
            resetDelimBuf
            wspMode = false
            resetDelim
          }
          case wspP: WSPPlusDelim if isSpace => {
            // We're looking for 1 or more spaces
            // and found at least 1.
            println("WSPPlus and isSpace" + " '" + char + "' " + char.toInt)
            update(delim, charIdx, true)
            wspMode = true
            advanceDelim
            advanceChar
          }
          case wspS: WSPStarDelim if isSpace => {
            // We're looking for 0 or more spaces
            // and found at least 1.
            println("WSPStar and isSpace" + " '" + char + "' " + char.toInt)
            update(delim, charIdx, true)
            wspMode = true
            advanceDelim
            advanceChar
          }
          case wspP: WSPPlusDelim if !isSpace => {
            // We're looking for 1 or more spaces
            // and did not find one.
            println("WSPPlus and !isSpace" + " '" + char + "' " + char.toInt)
            resetDelimBuf
            wspMode = false
            resetDelim
          }
          case wspS: WSPStarDelim if !isSpace => {
            // We're looking for 0 or more spaces
            // and did not find one.
            // This is OK, Ignore (mark true) and move onto
            // next delim
            println("WSPStar and !isSpace" + " '" + char + "' " + char.toInt)
            update(delim, charIdx, true)
            wspMode = false
            advanceDelim
          }
          case wsp: WSPDelim if isSpace => {
            // Expected and found a space
            println("WSP and isSpace" + " '" + char + "' " + char.toInt)
            update(delim, charIdx, true)
            wspMode = false
            advanceDelim
            advanceChar
          }
          case wsp: WSPDelim if !isSpace => {
            // Expected and did not find a space
            println("WSP and !isSpace" + " '" + char + "' " + char.toInt)
            resetDelimBuf
            wspMode = false
            resetDelim
          }
          case space if isSpace => {
            // Ignore if we're in wspMode
            // otherwise, we weren't expecting
            // to find a space. Reset.
            if (!wspMode) {
              println("!WSPMode and isSpace" + " '" + char + "' " + char.toInt)
              resetDelimBuf
              resetDelim
            } else {
              println("WSPMode and isSpace" + " '" + char + "' " + char.toInt)
              advanceDelim
              advanceChar
            }
          }
          case other if matched => {
            println("Char and matched" + " '" + char + "' " + char.toInt)
            update(delim, charIdx, true)
            wspMode = false
            advanceDelim
            advanceChar
          }
          case other if !matched && delimiter.startsWith(char.toString()) => {
            println("Char and !matched but might be start of next delimiter" + " '" + char + "' " + char.toInt)
            wspMode = false
            resetDelimBuf
            resetDelim
          }
          case _ => {
            println("Default Case" + " '" + char + "' " + char.toInt)
            resetDelimBuf
            wspMode = false
            resetDelim
            advanceChar
          }
        } // end-delim-match
      }
    }
    processDelimBuf
    processPartials
  }

  // Determines if all characters within a delimBuf were
  // satisfied/matched in order.  
  //
  // If they were then the full delimiter
  // has been matched and the location within CharBuf is stored in indexes.
  //
  def processDelimBuf = {
    if (delimBuf.length > 0) {
      val allMatched = delimBuf.filter(x => x.isMatched == false).length == 0
      if (allMatched) {

        val startPos = delimBuf(0).charPos
        val endPos = delimBuf(delimBuf.length - 1).charPos
        add(startPos, endPos) // Legacy, remove when done
        addFullMatch(startPos, endPos)
        resetDelimBuf
      }
    }
  }

  def print = {
    println("\n====\nDelimiter: " + delimiter)
    println("FULL MATCHES: ")
    fullMatches.toList.sortBy(_._1) foreach {
      x => println("\t" + x._1 + "\t" + x._2)
    }
    println("PARTIAL MATCHES: ")
    partialMatches.toList.sortBy(_._1) foreach {
      x => println("\t" + x._1 + "\t" + x._2)
    }
    println("\n====\n")
  }
}

abstract class DelimBase extends Base {
  def typeName: String
  def print
}

trait Base {
  var isMatched: Boolean = false
  var index: Int = -1
  var charPos: Int = -1

  def clear = {
    isMatched = false
    charPos = -1
  }

  def checkMatch(charIn: Char): Boolean
}

class CharDelim(val char: Char) extends DelimBase {
  def checkMatch(charIn: Char): Boolean = {
    this.isMatched = charIn == char
    isMatched
  }

  lazy val typeName = "CharDelim"
  def print = {
    println(typeName + ": '" + char + "' d" + char.toInt)
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
    println(typeName + ": NL")
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
    println(typeName + ": WSPBase")
  }
}

class WSPDelim extends WSPBase with WSP {
  override lazy val typeName = "WSPDelim"
  override def print = {
    println(typeName + ": WSP")
  }
}

class WSPPlusDelim extends WSPBase with WSP {
  override lazy val typeName = "WSP+Delim"
  override def print = {
    println(typeName + ": WSP+")
  }
}

class WSPStarDelim extends WSPBase with WSP {
  override lazy val typeName = "WSP*Delim"
  override def print = {
    println(typeName + ": WSP*")
  }
}