package edu.illinois.ncsa.daffodil.dsom

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

import java.util.regex.Matcher
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.lang.Byte
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
 * Replace character entities, as well as hex/decimal numeric character entities by their unicode codepoint values.
 *
 * Deals with self-escaping of the '%' which introduces a DFDL character entity.
 *
 * Does not deal with raw %#rHH; entities. Those have to be preserved because they get interpreted differently
 * depending on how the string literal is used. Similarly the character class entities like %WSP*; which are used
 * to provide pattern match literals (like delimiters).
 *
 */
class EntityReplacer {

  val dfdlEntityName = "NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC[1-4]|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL|NBSP|NEL|LS"
  val dfdlCharClassEntityName = "NL|WSP|WSP\\*|WSP\\+"

  val entityCharacterUnicode: List[(String, String, Pattern)] =
    List(
      ("NUL", "\u0000", Pattern.compile("%" + "NUL" + ";", Pattern.MULTILINE)),
      ("SOH", "\u0001", Pattern.compile("%" + "SOH" + ";", Pattern.MULTILINE)),
      ("STX", "\u0002", Pattern.compile("%" + "STX" + ";", Pattern.MULTILINE)),
      ("ETX", "\u0003", Pattern.compile("%" + "ETX" + ";", Pattern.MULTILINE)),
      ("EOT", "\u0004", Pattern.compile("%" + "EOT" + ";", Pattern.MULTILINE)),
      ("ENQ", "\u0005", Pattern.compile("%" + "ENQ" + ";", Pattern.MULTILINE)),
      ("ACK", "\u0006", Pattern.compile("%" + "ACK" + ";", Pattern.MULTILINE)),
      ("BEL", "\u0007", Pattern.compile("%" + "BEL" + ";", Pattern.MULTILINE)),
      ("BS", "\u0008", Pattern.compile("%" + "BS" + ";", Pattern.MULTILINE)),
      ("HT", "\u0009", Pattern.compile("%" + "HT" + ";", Pattern.MULTILINE)),
      ("LF", "\u000A", Pattern.compile("%" + "LF" + ";", Pattern.MULTILINE)),
      ("VT", "\u000B", Pattern.compile("%" + "VT" + ";", Pattern.MULTILINE)),
      ("FF", "\u000C", Pattern.compile("%" + "FF" + ";", Pattern.MULTILINE)),
      ("CR", "\u000D", Pattern.compile("%" + "CR" + ";", Pattern.MULTILINE)),
      ("SO", "\u000E", Pattern.compile("%" + "SO" + ";", Pattern.MULTILINE)),
      ("SI", "\u000F", Pattern.compile("%" + "SI" + ";", Pattern.MULTILINE)),
      ("DLE", "\u0010", Pattern.compile("%" + "DLE" + ";", Pattern.MULTILINE)),
      ("DC1", "\u0011", Pattern.compile("%" + "DC1" + ";", Pattern.MULTILINE)),
      ("DC2", "\u0012", Pattern.compile("%" + "DC2" + ";", Pattern.MULTILINE)),
      ("DC3", "\u0013", Pattern.compile("%" + "DC3" + ";", Pattern.MULTILINE)),
      ("DC4", "\u0014", Pattern.compile("%" + "DC4" + ";", Pattern.MULTILINE)),
      ("NAK", "\u0015", Pattern.compile("%" + "NAK" + ";", Pattern.MULTILINE)),
      ("SYN", "\u0016", Pattern.compile("%" + "SYN" + ";", Pattern.MULTILINE)),
      ("ETB", "\u0017", Pattern.compile("%" + "ETB" + ";", Pattern.MULTILINE)),
      ("CAN", "\u0018", Pattern.compile("%" + "CAN" + ";", Pattern.MULTILINE)),
      ("EM", "\u0019", Pattern.compile("%" + "EM" + ";", Pattern.MULTILINE)),
      ("SUB", "\u001A", Pattern.compile("%" + "SUB" + ";", Pattern.MULTILINE)),
      ("ESC", "\u001B", Pattern.compile("%" + "ESC" + ";", Pattern.MULTILINE)),
      ("FS", "\u001C", Pattern.compile("%" + "FS" + ";", Pattern.MULTILINE)),
      ("GS", "\u001D", Pattern.compile("%" + "GS" + ";", Pattern.MULTILINE)),
      ("RS", "\u001E", Pattern.compile("%" + "RS" + ";", Pattern.MULTILINE)),
      ("US", "\u001F", Pattern.compile("%" + "US" + ";", Pattern.MULTILINE)),
      ("SP", "\u0020", Pattern.compile("%" + "SP" + ";", Pattern.MULTILINE)),
      ("DEL", "\u007F", Pattern.compile("%" + "DEL" + ";", Pattern.MULTILINE)),
      ("NBSP", "\u00A0", Pattern.compile("%" + "NBSP" + ";", Pattern.MULTILINE)),
      ("NEL", "\u0085", Pattern.compile("%" + "NEL" + ";", Pattern.MULTILINE)),
      ("LS", "\u2028", Pattern.compile("%" + "LS" + ";", Pattern.MULTILINE)))

  val escapeReplacements: List[(String, String, Pattern)] = List(("%", "\u0025", Pattern.compile("%%", Pattern.MULTILINE)))

  val charEntityPattern = Pattern.compile("%(" + dfdlEntityName + ");", Pattern.MULTILINE)
  val hexPattern = Pattern.compile("%#x[0-9a-fA-F]+;", Pattern.MULTILINE)
  val decPattern = Pattern.compile("%#[0-9]+;", Pattern.MULTILINE)
  val bytePattern = Pattern.compile("%#r[0-9a-fA-F]{2};", Pattern.MULTILINE)
  val charClassEntityPattern = Pattern.compile("%(" + dfdlCharClassEntityName + ");", Pattern.MULTILINE)

  def hasDfdlEntity(input: String): Boolean = {
    if (hasDfdlCharEntity(input) ||
      hasDecimalCodePoint(input) ||
      hasHexCodePoint(input) ||
      hasByteCodePoint(input) ||
      hasDfdlCharClassEntity(input)) {
      return true
    }
    false
  }

  def hasDfdlCharClassEntity(input: String): Boolean = {
    val p: Pattern = charClassEntityPattern
    val m: Matcher = p.matcher(input)
    m.find()
  }

  def hasDfdlCharEntity(input: String): Boolean = {
    val p: Pattern = charEntityPattern
    val m: Matcher = p.matcher(input)
    m.find()
  }

  def hasDecimalCodePoint(input: String): Boolean = {
    val p: Pattern = decPattern
    val m: Matcher = p.matcher(input)
    m.find()
  }

  def hasHexCodePoint(input: String): Boolean = {
    val p: Pattern = hexPattern
    val m: Matcher = p.matcher(input)
    m.find()
  }

  def hasByteCodePoint(input: String): Boolean = {
    val p: Pattern = bytePattern
    val m: Matcher = p.matcher(input)
    m.find()
  }

  def replaceHex(input: String, prefix: String): String = {
    var res: String = input

    // While we have Hex Code Points in the string
    // Find and replace with their character equivalents.
    while (hasHexCodePoint(res)) {
      val p: Pattern = hexPattern
      var m: Matcher = p.matcher(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace(prefix, "").replace(";", "")
        val intStr = Integer.parseInt(trimmedStr, 16)
        val newChar = intStr.toChar.toString
        // Special case here
        // $ is used by replaceAll to refer to prior groups
        // so $ must be escaped into \$
        val newCharNotDollar = if (newChar == "$") """\$""" else newChar
        res = res.replaceAll(rawStr, newCharNotDollar)
        m = p.matcher(res) // update Matcher
      }
    }

    res
  }

  def replaceDecimal(input: String, prefix: String): String = {
    var res: String = input

    // While we have Decimal Code Points in the string
    // Find and replace with their character equivalents.
    while (hasDecimalCodePoint(res)) {
      val p: Pattern = decPattern
      var m: Matcher = p.matcher(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace(prefix, "").replace(";", "")
        val intStr = Integer.parseInt(trimmedStr, 10)

        res = res.replaceAll(rawStr, intStr.asInstanceOf[Char].toString())
        m = p.matcher(res) // update Matcher
      }
    }

    res
  }

  def replaceBytes(input: String, prefix: String): String = {
    var res: String = input

    // While we have Hex or Decimal Code Points in the string
    // Find and replace with their character equivalents.
    while (hasByteCodePoint(res)) {
      val p: Pattern = bytePattern
      var m: Matcher = p.matcher(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace(prefix, "").replace(";", "")
        val byteStr0: Byte = Byte.parseByte(trimmedStr.substring(0, 1), 16)
        val byteStr1: Byte = Byte.parseByte(trimmedStr.substring(1), 16)

        res = res.replaceAll(rawStr, byteStr0.asInstanceOf[Char].toString() + byteStr1.asInstanceOf[Char].toString())
        m = p.matcher(res) // update Matcher
      }
    }

    res
  }

  def replaceByte(input: String): String = {
    replaceBytes(input, "%#r")
  }

  def replaceHex(input: String): String = {
    replaceHex(input, "%#x")
  }

  def replaceDecimal(input: String): String = {
    replaceDecimal(input, "%#")
  }

  def replaceDfdlEntity(input: String): String = {
    replace(input, entityCharacterUnicode)
  }

  def replaceEscapes(input: String): String = {
    replace(input, escapeReplacements)
  }

  def replaceAll(input: String, shouldReplaceByte: Boolean = false): String = {
    var res: String = input

    if (shouldReplaceByte) { res = replaceByte(input) }
    res = replaceHex(res)
    res = replaceDecimal(res)
    res = replace(res, entityCharacterUnicode)
    res = replace(res, escapeReplacements)

    res
  }

  // Replacement helper function
  // 	entity = what you are replacing (informational only)
  // 	unicode = what you are replacing it with
  // 	pattern = what you are replacing via RegEx match on this pattern
  private def replace(input: String, chars: List[(String, String, Pattern)]): String = {
    var res: String = input
    chars.foreach {
      case (entity, unicode, pattern) => {
        val m: Matcher = pattern.matcher(res)
        res = m.replaceAll(unicode)
      }
    }
    res
  }

}

object EntityReplacer extends EntityReplacer

abstract class StringLiteralBase(rawArg: String) {
  val xmlEntityPattern = new Regex("""&(quot|amp|apos|lt|gt);""", "entity")
  val raw: String = {
    val res = xmlEntityPattern.replaceAllIn(rawArg, m => {
      val sb = scala.xml.Utility.unescape(m.group("entity"), new StringBuilder())
      // There really is no possibility for null to come back as we've made
      // sure to only include valid xml entities in the xmlEntityPattern.
      if (sb == null) {
        Assert.impossible("Failed to replace an xml entity (%s) when converting String Literals.".format(m.group("entity")))
      } else { sb.toString() }
    })
    res
  }
  def cooked: String
}

/**
 * String values in the infoset, string results of DFDL's xpath-like expressions are of this kind.
 *
 *  This is the kind of string literal you can use within an expression.
 */
class StringValueAsLiteral(rawArg: String, context: ThrowsSDE)
  extends StringLiteralBase(rawArg) {
  def cooked = EntityReplacer.replaceAll(raw)
}

class SingleCharacterLiteral(rawArg: String, context: ThrowsSDE)
  extends StringValueAsLiteral(rawArg, context) {
  context.schemaDefinition(cooked.length == 1, "Length of string must be exactly 1 character.")
}

class SingleCharacterLiteralES(rawArg: String, context: ThrowsSDE)
  extends StringValueAsLiteral(rawArg, context) {
  context.schemaDefinition(cooked.length() == 1 || cooked.length() == 0, "Length of string must be exactly 1 character or be empty.")
}

class OneDelimiterLiteral(rawArg: String, context: ThrowsSDE)
  extends StringLiteralBase(rawArg) {
  def cooked = EntityReplacer.replaceAll(raw)
  // deal with raw bytes entities
  // deal with character class entities

  /**
   *  return a regex matcher that matches this individual delimiter
   */
  def matcher = {
    Assert.notYetImplemented()
  }

}

class ListOfStringValueAsLiteral(rawArg: String, context: ThrowsSDE) {
  def cooked = {
    val list = rawArg.split("\\s").toList
    val cookedList: ListBuffer[String] = ListBuffer.empty
    list.foreach(x => {
      val l = new StringValueAsLiteral(x, context)
      cookedList += l.cooked
    })
    cookedList.toList
  }
}

class ListOfSingleCharacterLiteral(rawArg: String, context: ThrowsSDE) {
  def cooked = {
    val list = rawArg.split("\\s")
    val cookedList: ListBuffer[String] = ListBuffer.empty
    list.foreach(x => {
      val l = new SingleCharacterLiteral(x, context)
      cookedList += l.cooked
    })
    cookedList.toList
  }
}
