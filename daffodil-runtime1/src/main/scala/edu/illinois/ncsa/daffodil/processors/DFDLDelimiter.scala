

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.processors

import java.util.regex.Matcher
import java.util.regex.Pattern

import scala.collection.mutable.Queue
import scala.util.control.Breaks
import edu.illinois.ncsa.daffodil.util.Enum


object DelimiterType extends Enum {
  sealed abstract trait Type extends EnumValueType
  case object Separator extends Type
  case object Terminator extends Type
  case object NotDelimited extends Type
}

object DelimiterLocation extends Enum {
  sealed abstract trait Type extends EnumValueType
  case object Local extends Type
  case object Remote extends Type
}

class Delimiter {
  var delimiterStr: String = "" // String representation of delimiter Ex. "%WSP;,%WSP*;"

  var delimBuf: Array[DelimBase] = Array.empty[DelimBase] /* Buffer where each cell (DelimBase) represents a character
		  												     in the delimiter string */

  var delimRegExParseDelim: String = "" // Regex to actually parse the entire delimiter

  // Pre-compiled RegEx patterns for finding character classes
  lazy val NL = Pattern.compile("%(NL);", Pattern.MULTILINE)
  lazy val WSP = Pattern.compile("%(WSP);", Pattern.MULTILINE)
  lazy val WSP_Plus = Pattern.compile("%(WSP\\+);", Pattern.MULTILINE)
  lazy val WSP_Star = Pattern.compile("%(WSP\\*);", Pattern.MULTILINE)

  override def toString(): String = {
    return "Delimiter[" + delimiterStr + "]"
  }

  // Must call to create the necessary structures
  //
  def compile(pDelimiter: String): Unit = {
    delimiterStr = pDelimiter
    delimBuf = buildDelimBuf(delimiterStr)

    delimRegExParseDelim = this.delimRegexParseDelim(delimBuf)
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
  def delimRegexParseDelim(delimiterBuf: Array[DelimBase] = delimBuf): String = {
    var sb: StringBuilder = new StringBuilder
    delimiterBuf foreach {
      delim =>
        {
          delim match {
            case nl: NLDelim => {
              sb.append("(?>" + // Eliminates needles backtracking. Atomic group of
                "(\\r\\n)|" + // CRLF
                "((?<!\\r)\\n)|" + // LF not preceded by CR
                "(\\r(?!\\n))|" + // CR not followed by LF
                "\\u0085|\\u2028)")
            }
            case wsp: WSPDelim => {
              sb.append("(\\s|\\u0020|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" +
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" +
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)")
            } // Single space
            case wsp: WSPPlusDelim => {
              sb.append("(\\s|\\u0020|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" +
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" +
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)+")
            } // One or more spaces
            case wsp: WSPStarDelim => {
              sb.append("(\\s|\\u0020|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" +
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" +
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)*")
            } // None or more spaces
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
      resDelimBuf = reduceDelimBuf(q.toArray[DelimBase])
    } else {
      // No need to reduce
      resDelimBuf = q.toArray[DelimBase]
    }
    resDelimBuf
  }
}

abstract class DelimBase extends Base {
  def typeName: String
  def print
  def printStr: String
  def allChars: Seq[Char]
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
  lazy val allChars: Seq[Char] = Seq(char)
  def checkMatch(charIn: Char): Boolean = {
    val matched = charIn == char
    matched
  }

  lazy val typeName = "CharDelim"
  def print = {
    //log(LogLevel.Debug, "\t\t\t" + typeName + ": '" + char + "' d" + char.toInt + " isMatched: " + isMatched.toString()))
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

trait NL extends CharacterClass {
  lazy val LF: Char = { convertUnicodeToChar("\\u000A") }
  lazy val CR: Char = { convertUnicodeToChar("\\u000D") }
  lazy val NEL: Char = { convertUnicodeToChar("\\u0085") }
  lazy val LS: Char = { convertUnicodeToChar("\\u2028") }
  
  lazy val allChars: Seq[Char] = Seq(LF, CR, NEL, LS)
  lazy val allCharsNotCR: Seq[Char] = Seq(NEL, LS, LF)
}

class NLDelim extends DelimBase with NL {
  lazy val typeName = "NLDelim"
  
  def checkMatch(charIn: Char): Boolean = {
    charIn match {
      case LF | CR | NEL | LS => isMatched = true
      case _ => isMatched = false
    }
    isMatched
  }
  
  def isNLNotCR(charIn: Char): Boolean = {
    charIn match {
      case LF | NEL | LS => true
      case _ => false
    }
  }
  
  def isCR(charIn: Char): Boolean = {
    charIn match {
      case CR => true
      case _ => false
    }
  }
  
  def isLF(charIn: Char): Boolean = {
    charIn match {
      case LF => true
      case _ => false
    }
  }

  def print = {
    //log(LogLevel.Debug, "\t\t\t" + typeName + ": NL" + " isMatched: " + isMatched.toString()))
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
  
  lazy val allChars: Seq[Char] = Seq(CTRL0, CTRL1, CTRL2, CTRL3, CTRL4, SPACE, NEL, 
      NBSP, OGHAM, MONG, SP0, SP1, SP2, SP3, SP4, SP5, SP6, SP7, SP8, SP9, SP10,
      LSP, PSP, NARROW, MED, IDE)
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
    //log(LogLevel.Debug, "\t\t\t" + typeName + ": WSPBase" + " isMatched: " + isMatched.toString()))
  }
  def printStr = {
    val res = typeName
    res
  }
}

class WSPDelim extends WSPBase with WSP {
  override lazy val typeName = "WSPDelim"
  override def print = {
    //log(LogLevel.Debug, "\t\t\t" + typeName + ": WSP" + " isMatched: " + isMatched.toString()))
  }
  override def printStr = {
    val res = typeName
    res
  }
}

class WSPPlusDelim extends WSPBase with WSP {
  override lazy val typeName = "WSP+Delim"
  override def print = {
    //log(LogLevel.Debug, "\t\t\t" + typeName + ": WSP+" + " isMatched: " + isMatched.toString()))
  }
  override def printStr = {
    val res = typeName
    res
  }
}

class WSPStarDelim extends WSPBase with WSP {
  override lazy val typeName = "WSP*Delim"
  override def print = {
    //log(LogLevel.Debug, "\t\t\t" + typeName + ": WSP*" + " isMatched: " + isMatched.toString()))
  }
  override def printStr = {
    val res = typeName
    res
  }
}
