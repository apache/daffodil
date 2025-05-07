/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.runtime1.processors

import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.collection.mutable.Queue

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Enum
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._

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

  var delimBuf: Array[DelimBase] =
    Array.empty[DelimBase] /* Buffer where each cell (DelimBase) represents a character
		  												     in the delimiter string */

  var delimRegExParseDelim: String = "" // Regex to actually parse the entire delimiter

  // Pre-compiled RegEx patterns for finding character classes
  //
  // You might thing those would be gone by now, but they actually can't be. They have
  // to be removed only when the char class entities have been removed.
  // Otherwise you can artificially create a char class entity.
  //
  private lazy val NL = Pattern.compile("(NL);", Pattern.MULTILINE)
  private lazy val WSP = Pattern.compile("(WSP);", Pattern.MULTILINE)
  private lazy val WSP_Plus = Pattern.compile("(WSP\\+);", Pattern.MULTILINE)
  private lazy val WSP_Star = Pattern.compile("(WSP\\*);", Pattern.MULTILINE)
  private lazy val ES = Pattern.compile("(ES);", Pattern.MULTILINE)

  override def toString(): String = {
    return "Delimiter[" + delimiterStr + "]"
  }

  // Must call to create the necessary structures
  //
  def compileDelimiter(pDelimiter: String, ignoreCase: Boolean): Unit = {
    delimiterStr = pDelimiter
    delimBuf = buildDelimBuf(delimiterStr, ignoreCase)

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

          if (result.isDefined) {
            val x = result.get
            // WSP exists and an equivalent representation was found
            x.index = idx // Set the delimiter's index
            q += x
            idx += 1
          } else {
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

    if (result.isDefined) {
      val x = result.get
      x.index = idx
      q += x
    } else {
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

    q.toArray[DelimBase]
  }

  // Based upon what WSP delimiters were encountered,
  // determine the equivalent representation (if any) and return it.
  //
  def getReducedDelim(numWSP: Int, numWSP_Plus: Int, numWSP_Star: Int): Maybe[DelimBase] = {
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
      return One(new WSPPlusDelim())
    } else if (numWSP != 0 && numWSP_Plus == 0 && numWSP_Star != 0) { // WSP+ == 0
      // Case: 6
      return One(new WSPPlusDelim())
    } else if (numWSP == 0 && numWSP_Plus == 0 && numWSP_Star != 0) {
      // Case: 2
      return One(new WSPStarDelim())
    } else if (numWSP == 1 && numWSP_Plus == 0 && numWSP_Star == 0) {
      // Case: 5
      return One(new WSPDelim())
    }
    Nope
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
    val sb: StringBuilder = new StringBuilder
    delimiterBuf.foreach { delim =>
      {
        delim match {
          case nl: NLDelim => {
            sb.append(
              "(?>" + // Eliminates needles backtracking. Atomic group of
                "(\\r\\n)|" + // CRLF
                "((?<!\\r)\\n)|" + // LF not preceded by CR
                "(\\r(?!\\n))|" + // CR not followed by LF
                "\\u0085|\\u2028)"
            )
          }
          case wsp: WSPDelim => {
            sb.append(
              "(\\s|\\u0020|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" +
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" +
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)"
            )
          } // Single space
          case wsp: WSPPlusDelim => {
            sb.append(
              "(\\s|\\u0020|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" +
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" +
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)+"
            )
          } // One or more spaces
          case wsp: WSPStarDelim => {
            sb.append(
              "(\\s|\\u0020|\\u0009|\\u000A|\\u000B|\\u000C|\\u000D|\\u0085" +
                "|\\u00A0|\\u1680|\\u180E|\\u2000|\\u2001|\\u2002|\\u2003|\\u2004|\\u2005|\\u2006|" +
                "\\u2007|\\u2008|\\u2009|\\u200A|\\u2028|\\u2029|\\u202F|\\u205F|\\u3000)*"
            )
          } // None or more spaces
          case ws: ESDelim => // noop
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
  def findCharClasses(str: String): (Int, Maybe[DelimBase]) = {
    val mNL: Matcher = NL.matcher(str)
    val mWSP: Matcher = WSP.matcher(str)
    val mWSP_Plus: Matcher = WSP_Plus.matcher(str)
    val mWSP_Star: Matcher = WSP_Star.matcher(str)
    val mES: Matcher = ES.matcher(str)

    var length: Int = -1

    val classList: scala.collection.mutable.Map[String, (Int, Int)] =
      scala.collection.mutable.Map.empty

    if (mNL.find()) {
      classList += ("NL" -> (mNL.start() -> mNL.end()))
    }

    if (mES.find()) {
      classList += ("ES" -> (mES.start() -> mES.end()))
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
        case "NL" => (length, One(new NLDelim()))
        case "WSP" => (length, One(new WSPDelim()))
        case "WSP+" => (length, One(new WSPPlusDelim()))
        case "WSP*" => (length, One(new WSPStarDelim()))
        case "ES" => (length, One(new ESDelim()))
      }
      return result
    }
    (-1, Nope) // Unrecognized CharClass
  }

  // Populates the delimBuf object with an object
  // representation of the characters within the delimiter
  // string.
  //
  def buildDelimBuf(delimStr: String, ignoreCase: Boolean): Array[DelimBase] = {
    val q: Queue[DelimBase] = new Queue[DelimBase]()

    var newIdx = 0 // index within delimBuf array

    var numCharClass: Int = 0

    var idx = 0 // index in the delimStr
    while (idx < delimStr.length()) {

      val c: Char = delimStr.charAt(idx)

      if (c == '%') {
        // is there another character after this?
        if ((idx + 1) == delimStr.length) {
          // last character in delimStr was a single isolated '%'.
          // This shouldn't happen
          Assert.invariantFailed(
            "delimStr should not end in an isolated single %. DelimStr = " + delimStr
          )
        }
        if (delimStr.charAt(idx + 1) == '%') {
          // double percent. We want only a single one. And we
          // want it treated as a character delimiter, not the introduction of
          // a char class entity.
          val obj = new CharDelim(c, ignoreCase)
          obj.index = newIdx // Index within delimBuf Array
          newIdx += 1
          q += obj
          idx += 2 // move past the "%%"
        } else {
          // Possible character class, check patterns

          val (matchLength, delimObj) = {
            val split = delimStr.substring(idx + 1).split("%")
            if (split.length == 0)
              (-1, Nope) // no match, don't need the delimObj
            else
              findCharClasses(split(0))
          }
          if (matchLength != -1) {
            // Have a match, add the object
            val obj = delimObj.get
            obj.index = newIdx // Index within delimBuf Array
            q += obj
            idx += matchLength + 1 // advance cursor past the Character Class ( + 1 for the % sign )
            newIdx += 1
            numCharClass += 1
          } else {
            // Not a CharClass or unrecognized,
            // therefore treat as a CharDelim (TODO: Why? Seems like the throw below commented out is more correct.)
            //
            // This shouldn't happen. We should be getting '%%' i.e, escaped percent signs, not isolated ones.
            // And we shouldn't be getting %ES; or %WSP*; "alone" for delimiters.
            //
            // JIRA DFDL-1475 is the ticket for this poor diagnostic.
            //
            // It isn't clear to me how you issue a diagnostic from here. But clearly one wants to be able to.
            //
            // NOTE: if you throw here, instead of creating CharDelim, many regression tests break.
            // So undertake as an isolated activity and really figure it out. It's not going to be a small thing to just
            // do as part of a large commit.
            //
            // throw new EntitySyntaxException("delimStr has '%' at index " + idx + " that is not escaped (not '%%') and doesn't introduce an allowed char class entity: " + delimStr)
            val obj = new CharDelim(c, ignoreCase)
            obj.index = newIdx // Index within delimBuf Array
            newIdx += 1
            q += obj
            idx += 1
          }
        }
      } else {
        // c was not a '%'
        // Treat as a CharDelim
        val obj = new CharDelim(c, ignoreCase)
        obj.index = newIdx // Index within delimBuf Array
        newIdx += 1
        q += obj
        idx += 1
      }
    }

    // filter out any %ES; delims, they do not have any effect
    val delimsNoES = q.filterNot(_.isInstanceOf[ESDelim]).toArray

    val resDelimBuf: Array[DelimBase] =
      if (delimsNoES.length == 0) {
        // if the delimiter was just one or more ES's, then make the delim buf
        // a single ES
        Array(new ESDelim)
      } else if (numCharClass > 1) {
        // More than one Char Class, reduction possible!
        reduceDelimBuf(delimsNoES)
      } else {
        // No need to reduce
        delimsNoES
      }
    resDelimBuf
  }
}

abstract class DelimBase extends Base {
  def typeName: String
  def printStr: String
  def allChars: Seq[Char]
  override def toString(): String = {
    return typeName
  }
  def unparseValue(outputNewLine: String): String
}

trait Base {
  var isMatched: Boolean = false
  var index: Int = -1
  var charPos: Int = -1
  var charPosEnd: Int = -1

  def clear() = {
    isMatched = false
    charPos = -1
    charPosEnd = -1
  }

  def checkMatch(charIn: Char): Boolean
}

class CharDelim(val char: Char, ignoreCase: Boolean) extends DelimBase {
  lazy val allChars: Seq[Char] = Seq(char)
  def checkMatch(charIn: Char): Boolean = {
    if (charIn == char) true
    else if (ignoreCase && (charIn.toUpper == char.toUpper || charIn.toLower == char.toLower))
      true
    else false
  }

  lazy val typeName = "CharDelim"

  def printStr = {
    val res = typeName + "(" + char + ")"
    res
  }

  override def toString(): String = {
    return typeName + "[" + char + "]"
  }

  def unparseValue(outputNewLine: String): String = char.toString
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

  def printStr = {
    val res = typeName
    res
  }

  def unparseValue(outputNewLine: String) = {
    outputNewLine
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

  lazy val allChars: Seq[Char] = Seq(
    CTRL0,
    CTRL1,
    CTRL2,
    CTRL3,
    CTRL4,
    SPACE,
    NEL,
    NBSP,
    OGHAM,
    MONG,
    SP0,
    SP1,
    SP2,
    SP3,
    SP4,
    SP5,
    SP6,
    SP7,
    SP8,
    SP9,
    SP10,
    LSP,
    PSP,
    NARROW,
    MED,
    IDE
  )
}

abstract class WSPBase extends DelimBase with WSP {
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

  def printStr = {
    val res = typeName
    res
  }
}

class WSPDelim extends WSPBase with WSP {
  override lazy val typeName = "WSPDelim"

  override def printStr = {
    val res = typeName
    res
  }
  def unparseValue(outputNewLine: String): String = SPACE.toString
}

class WSPPlusDelim extends WSPBase with WSP {
  override lazy val typeName = "WSP+Delim"

  override def printStr = {
    val res = typeName
    res
  }
  def unparseValue(outputNewLine: String): String = SPACE.toString
}

class WSPStarDelim extends WSPBase with WSP {
  override lazy val typeName = "WSP*Delim"

  override def printStr = {
    val res = typeName
    res
  }

  def unparseValue(outputNewLine: String): String = ""
}

/**
 * This delimiter matches an empty space which effectively is always a
 * successful match without moving the character position.
 *
 * This is useful since it allows us to use the exact same logic for
 * consuming/matching %ES; as all other delimiters. This is necessary in the
 * case when dfdl:terminator has %ES; when dfdl:lengthKind != "delimited".
 * Being able to use the standard delimiter scanning for this makes our lives
 * much easier and removes lots of special casing. This class establishes the
 * invariant that there is always a DelimBase object used when "scanning", even
 * when we're "scanning for nothing".
 */
class ESDelim extends DelimBase {
  override def checkMatch(charIn: Char): Boolean =
    Assert.impossible("We should never ask if a character matches an %ES;")
  override def allChars: Seq[Char] = Seq.empty
  override def printStr: String = typeName
  override def typeName: String = "ES"
  override def unparseValue(outputNewLine: String): String = ""
}
