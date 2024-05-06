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

package org.apache.daffodil.lib.cookers

import java.lang.{ Byte => JByte, Character => JChar }
import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.collection.mutable.ListBuffer

import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.XMLUtils

class EntitySyntaxException(msg: String) extends Exception(msg)

/**
 * Replace character entities, as well as hex/decimal numeric character entities by their unicode codepoint values.
 *
 * Deals with self-escaping of the '%' which introduces a DFDL character entity.
 *
 * Note that %#rHH; Byte Value Entities (aka raw), have to be preserved because they get interpreted differently
 * depending on how the string literal is used. Similarly the character class entities like %WSP*; which are used
 * to provide pattern match literals (like delimiters).
 *
 */
final class EntityReplacer {

  val dfdlEntityName =
    "NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC[1-4]|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL|NBSP|NEL|LS"
  val dfdlCharClassEntityName = "NL|WSP|WSP\\*|WSP\\+|ES"

  val entityCharacterUnicode: List[(String, String, Matcher)] =
    List(
      ("NUL", "\u0000", Pattern.compile("%" + "NUL" + ";", Pattern.MULTILINE).matcher("")),
      ("SOH", "\u0001", Pattern.compile("%" + "SOH" + ";", Pattern.MULTILINE).matcher("")),
      ("STX", "\u0002", Pattern.compile("%" + "STX" + ";", Pattern.MULTILINE).matcher("")),
      ("ETX", "\u0003", Pattern.compile("%" + "ETX" + ";", Pattern.MULTILINE).matcher("")),
      ("EOT", "\u0004", Pattern.compile("%" + "EOT" + ";", Pattern.MULTILINE).matcher("")),
      ("ENQ", "\u0005", Pattern.compile("%" + "ENQ" + ";", Pattern.MULTILINE).matcher("")),
      ("ACK", "\u0006", Pattern.compile("%" + "ACK" + ";", Pattern.MULTILINE).matcher("")),
      ("BEL", "\u0007", Pattern.compile("%" + "BEL" + ";", Pattern.MULTILINE).matcher("")),
      ("BS", "\u0008", Pattern.compile("%" + "BS" + ";", Pattern.MULTILINE).matcher("")),
      ("HT", "\u0009", Pattern.compile("%" + "HT" + ";", Pattern.MULTILINE).matcher("")),
      ("LF", "\u000A", Pattern.compile("%" + "LF" + ";", Pattern.MULTILINE).matcher("")),
      ("VT", "\u000B", Pattern.compile("%" + "VT" + ";", Pattern.MULTILINE).matcher("")),
      ("FF", "\u000C", Pattern.compile("%" + "FF" + ";", Pattern.MULTILINE).matcher("")),
      ("CR", "\u000D", Pattern.compile("%" + "CR" + ";", Pattern.MULTILINE).matcher("")),
      ("SO", "\u000E", Pattern.compile("%" + "SO" + ";", Pattern.MULTILINE).matcher("")),
      ("SI", "\u000F", Pattern.compile("%" + "SI" + ";", Pattern.MULTILINE).matcher("")),
      ("DLE", "\u0010", Pattern.compile("%" + "DLE" + ";", Pattern.MULTILINE).matcher("")),
      ("DC1", "\u0011", Pattern.compile("%" + "DC1" + ";", Pattern.MULTILINE).matcher("")),
      ("DC2", "\u0012", Pattern.compile("%" + "DC2" + ";", Pattern.MULTILINE).matcher("")),
      ("DC3", "\u0013", Pattern.compile("%" + "DC3" + ";", Pattern.MULTILINE).matcher("")),
      ("DC4", "\u0014", Pattern.compile("%" + "DC4" + ";", Pattern.MULTILINE).matcher("")),
      ("NAK", "\u0015", Pattern.compile("%" + "NAK" + ";", Pattern.MULTILINE).matcher("")),
      ("SYN", "\u0016", Pattern.compile("%" + "SYN" + ";", Pattern.MULTILINE).matcher("")),
      ("ETB", "\u0017", Pattern.compile("%" + "ETB" + ";", Pattern.MULTILINE).matcher("")),
      ("CAN", "\u0018", Pattern.compile("%" + "CAN" + ";", Pattern.MULTILINE).matcher("")),
      ("EM", "\u0019", Pattern.compile("%" + "EM" + ";", Pattern.MULTILINE).matcher("")),
      ("SUB", "\u001A", Pattern.compile("%" + "SUB" + ";", Pattern.MULTILINE).matcher("")),
      ("ESC", "\u001B", Pattern.compile("%" + "ESC" + ";", Pattern.MULTILINE).matcher("")),
      ("FS", "\u001C", Pattern.compile("%" + "FS" + ";", Pattern.MULTILINE).matcher("")),
      ("GS", "\u001D", Pattern.compile("%" + "GS" + ";", Pattern.MULTILINE).matcher("")),
      ("RS", "\u001E", Pattern.compile("%" + "RS" + ";", Pattern.MULTILINE).matcher("")),
      ("US", "\u001F", Pattern.compile("%" + "US" + ";", Pattern.MULTILINE).matcher("")),
      ("SP", "\u0020", Pattern.compile("%" + "SP" + ";", Pattern.MULTILINE).matcher("")),
      ("DEL", "\u007F", Pattern.compile("%" + "DEL" + ";", Pattern.MULTILINE).matcher("")),
      ("NBSP", "\u00A0", Pattern.compile("%" + "NBSP" + ";", Pattern.MULTILINE).matcher("")),
      ("NEL", "\u0085", Pattern.compile("%" + "NEL" + ";", Pattern.MULTILINE).matcher("")),
      ("LS", "\u2028", Pattern.compile("%" + "LS" + ";", Pattern.MULTILINE).matcher(""))
    )

  val charClassReplacements: List[(String, String, Matcher)] =
    List(
      ("WSP", "\u0020", Pattern.compile("%" + "WSP" + ";", Pattern.MULTILINE).matcher("")),
      ("WSP*", "", Pattern.compile("%" + "WSP\\*" + ";", Pattern.MULTILINE).matcher("")),
      ("WSP+", "\u0020", Pattern.compile("%" + "WSP\\+" + ";", Pattern.MULTILINE).matcher("")),
      ("ES", "", Pattern.compile("%" + "ES" + ";", Pattern.MULTILINE).matcher(""))
    )

  val escapeReplacements: List[(String, String, Matcher)] = List(
    ("%", "\u0025", Pattern.compile("%%", Pattern.MULTILINE).matcher(""))
  )

  val charEntityPattern =
    Pattern.compile("%(" + dfdlEntityName + ");", Pattern.MULTILINE).matcher("")
  val hexPattern = Pattern.compile("%#x[0-9a-fA-F]+;", Pattern.MULTILINE).matcher("")
  val decPattern = Pattern.compile("%#[0-9]+;", Pattern.MULTILINE).matcher("")
  val bytePattern = Pattern.compile("%#r[0-9a-fA-F]{2};", Pattern.MULTILINE).matcher("")
  val charClassEntityPattern =
    Pattern.compile("%(" + dfdlCharClassEntityName + ");", Pattern.MULTILINE).matcher("")

  val charEntityRegex = ("(%(?:" + dfdlEntityName + ");)(.*)").r
  val hexRegex = "(%#x[0-9a-fA-F]+;)(.*)".r
  val decRegex = "(%#[0-9]+;)(.*)".r
  val byteRegex = "(%#r[0-9a-fA-F]{2};)(.*)".r
  val charClassEntityRegex = ("(%(?:" + dfdlCharClassEntityName + ");)(.*)").r
  val dfdlEntityRegex = "(%[^%]*?;)(.*)".r

  def hasDfdlEntity(input: String): Boolean = {
    if (
      hasDfdlCharEntity(input) ||
      hasDecimalCodePoint(input) ||
      hasHexCodePoint(input) ||
      hasByteCodePoint(input) ||
      hasDfdlCharClassEntity(input)
    ) {
      return true
    }
    false
  }

  private def isMatched(input: String, m: Matcher): Boolean = {
    m.reset(input)
    m.find()
  }

  def hasDfdlCharClassEntity(input: String): Boolean = isMatched(input, charClassEntityPattern)
  def hasDfdlCharEntity(input: String): Boolean = isMatched(input, charEntityPattern)
  def hasDecimalCodePoint(input: String): Boolean = isMatched(input, decPattern)
  def hasHexCodePoint(input: String): Boolean = isMatched(input, hexPattern)
  def hasByteCodePoint(input: String): Boolean = isMatched(input, bytePattern)

  private def replaceEntityWithChar(input: String, entity: String, newChar: Char): String = {
    val replacement =
      if (newChar =#= '%') {
        // Some character entities are not replaced in this EntityReplacer,
        // such as double percents or character classes (NL, WSP, etc.). If
        // this character entity results in a percent character (e.g. %#x25;),
        // we must replace it with an escaped percent to be handled later.
        "%%"
      } else {
        // This character might mean something special to the replaceAll method
        // we are about to use (e.g. a dollar sign for regex group references).
        // To be safe, call quoteReplaement which escapes any characters that
        // mean something special in a replacement string so they are replaced
        // with the literal value.
        Matcher.quoteReplacement(newChar.toString)
      }
    input.replaceAll(entity, replacement)
  }

  def replaceHex(input: String, prefix: String): String = {
    var res: String = input

    // While we have Hex Code Points in the string
    // Find and replace with their character equivalents.
    while (hasHexCodePoint(res)) {
      val m = hexPattern
      m.reset(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace(prefix, "").replace(";", "")
        val intStr = Integer.parseInt(trimmedStr, 16)
        val newChar = intStr.toChar
        res = replaceEntityWithChar(res, rawStr, newChar)
      }
    }
    res
  }

  def replaceDecimal(input: String, prefix: String): String = {
    var res: String = input

    // While we have Decimal Code Points in the string
    // Find and replace with their character equivalents.
    while (hasDecimalCodePoint(res)) {
      val m = decPattern
      m.reset(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace(prefix, "").replace(";", "")
        val intStr = Integer.parseInt(trimmedStr, 10)
        val newChar = intStr.toChar
        res = replaceEntityWithChar(res, rawStr, newChar)
      }
    }

    res
  }

  def replaceBytes(input: String, prefix: String): String = {
    var res: String = input

    // While we have Raw Byte entities in the string
    // Find and replace with their character equivalents.
    while (hasByteCodePoint(res)) {
      val m = bytePattern
      m.reset(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace(prefix, "").replace(";", "")
        val upperNibble: Int = JByte.parseByte(trimmedStr.substring(0, 1), 16) << 4
        val lowerNibble: Byte = JByte.parseByte(trimmedStr.substring(1, 2), 16)
        val byteStr: Int = upperNibble | lowerNibble
        val newChar = byteStr.toChar
        res = replaceEntityWithChar(res, rawStr, newChar)
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

  /**
   * Replace entities, except for %NL; with their unparse equivalents
   * %NL; must be done separately because it is replaced with dfdl:outputNewline
   * which can be computed at runtime.
   */
  def replaceCharClassForUnparse(input: String): String = {
    replace(input, charClassReplacements)
  }

  val markerForNL = "\uFFFC__NL_ENTITY__\uFFFC"
  private val markerForDoublePercent = "\uFFFC__DOUBLE_PERCENT__\uFFFC"

  // double percent, but not triple (or longer). Must be viewed as pairs So that %%%CR; is %% followed by %CR;
  private val DPMatcher = Pattern.compile("(?<!%)%%", Pattern.MULTILINE).matcher("")
  private val markerForDPMatcher =
    Pattern.compile(markerForDoublePercent, Pattern.MULTILINE).matcher("")

  private val NLMatcher = Pattern.compile("%NL;", Pattern.MULTILINE).matcher("")
  private val markerForNLMatcher = Pattern.compile(markerForNL, Pattern.MULTILINE).matcher("")

  /**
   * Replaces all the entities, including the char class entities.
   *
   * Special treatment for the NL entity, and double percent.
   * These are replaced with unique marker strings. (Which cannot appear in the data - this is checked)
   *
   * The returned value has the NL entities replaced by the markerforNL
   */
  def replaceForUnparse(raw: String): String = {
    markerForDPMatcher.reset(raw)
    Assert.usage(!markerForDPMatcher.find(), "string cannot contain " + markerForDPMatcher)
    markerForNLMatcher.reset(raw)
    Assert.usage(!markerForNLMatcher.find(), "string cannot contain " + markerForNL)

    DPMatcher.reset(raw)
    val dpMarked = DPMatcher.replaceAll(markerForDoublePercent)
    val forUnp = replaceAll(dpMarked, None, true)
    NLMatcher.reset(forUnp)
    val nlMarked = NLMatcher.replaceAll(markerForNL)
    val s2 = nlMarked.replaceAll(
      markerForDoublePercent,
      "%"
    ) // put back double percents. Leaving the markers for NL
    s2
  }

  private def stripLeadingPercent(s: String) = if (s.startsWith("%")) s.substring(1) else s

  /**
   * forUnparse controls whether the character class entities are replaced by their unparse-time equivalents.
   * E.g., %WSP+; is replaced by a  single space. The NL entity is NOT replaced since that needs outputNewLine which
   * can be an expression itself.
   *
   * allowByteEntity controls whether the raw byte entity ie., %#rHH; is allowed or not. Several DFDL properties disallow
   * this form, but allow the others.
   */
  private def replaceEntity(
    proposedEntity: String,
    orig: String,
    context: Option[ThrowsSDE],
    forUnparse: Boolean,
    allowByteEntity: Boolean
  ): String = {
    val result = proposedEntity match {
      case charClassEntityRegex(_, _) => {
        if (forUnparse) {
          replaceCharClassForUnparse(proposedEntity)
        } else {
          proposedEntity // WSP, WSP+/*, NL, etc. Don't get replaced for parsing
        }
      }
      case hexRegex(entity, rest) => { replaceHex(proposedEntity) }
      case decRegex(entity, rest) => { replaceDecimal(proposedEntity) }
      case byteRegex(entity, rest) => {
        if (allowByteEntity) replaceByte(proposedEntity)
        else {
          val msg = "DFDL Byte Entity (%%%s) is not allowed, but was found in \"%s\""
          context.map { _.SDE(msg, stripLeadingPercent(proposedEntity), orig) }.getOrElse {
            throw new EntitySyntaxException(
              msg.format(stripLeadingPercent(proposedEntity), orig)
            )
          }
        }
      }
      case charEntityRegex(entity, rest) => { replace(proposedEntity, entityCharacterUnicode) }
      case dfdlEntityRegex(invalidEntity, rest) => {
        // Because we didn't match any of the previously acceptable formats
        // this must be an invalid entity since it's still in the generic
        // %<something>; dfdl entity format.
        val msg = "Invalid DFDL Entity (%%%s) found in \"%s\""
        context.map { _.SDE(msg, stripLeadingPercent(invalidEntity), orig) }.getOrElse {
          throw new EntitySyntaxException(msg.format(stripLeadingPercent(invalidEntity), orig))
        }
      }
      case nonEntity => nonEntity
    }
    result
  }

  private def errBadEntityLonePercent(
    ent: String,
    orig: String,
    context: Maybe[ThrowsSDE]
  ): Unit = {
    val msg =
      "Invalid DFDL Entity (%s) found in \"%s\". If a single percent was intended instead of a DFDL Entity, it must be self escaped (%%%%)."
        .format(ent, orig)
    if (context.isDefined) context.get.SDE(msg)
    else throw new EntitySyntaxException(msg)
  }

  private def errBadEntityNoSemi(ent: String, orig: String, context: Maybe[ThrowsSDE]): Unit = {
    val msg =
      "Invalid DFDL Entity (%%%s) found in \"%s\". Missing semicolon at end of entity name?"
        .format(stripLeadingPercent(ent), orig)
    if (context.isDefined) context.get.SDE(msg)
    else throw new EntitySyntaxException(msg)
  }

  /**
   * There are no '%%' in the input
   */
  private def process(
    input: String,
    orig: String,
    context: Option[ThrowsSDE],
    forUnparse: Boolean,
    allowByteEntities: Boolean = true
  ): String = {
    Assert.usage(!input.contains("%%"))
    if (!input.contains("%")) { return input }

    if (input.equals("%")) {
      errBadEntityLonePercent(input, orig, context)
    }

    val tokens = input.split("""%""") ++ (if (input.endsWith("%")) List("") else Nil)
    // we must have a minimum of 2 tokens here. The first token never was for an entity. Only 2nd to last ones.
    val startingToken = tokens.head
    val possibleEntityTokens = tokens.tail

    val tokens2 = possibleEntityTokens.map(tok => {
      Assert.invariant(!tok.contains("%"))
      //
      // If the token doesn't contain a ";", then it has to be malformed entity situation
      // as we had an un-escaped % followed by stuff, and no subsequent ";"
      if (!tok.contains(";")) errBadEntityNoSemi(tok, orig, context)
      //
      val semiPos = tok.indexOf(";")
      val possibleEntityName = tok.substring(0, semiPos)
      val afterEntity = tok.substring(semiPos + 1)
      // Now replace the entity appropriately. This will throw errors on malformed entities.
      val newEntity = replaceEntity(
        "%" + possibleEntityName + ";",
        orig,
        context,
        forUnparse,
        allowByteEntities
      )
      val res = newEntity + afterEntity
      res
    })
    val output = startingToken + tokens2.mkString
    output
  }

  private def hasDoublePercentEnding(input: String): Boolean = {
    if (input == "" || input.length() < 2) false
    else {
      if ((input.charAt(input.length() - 2) == '%') && (input.last == '%')) true
      else false
    }
  }

  /**
   * Replaces all valid dfdl entities with their appropriate values.
   *
   * However, because when parsing we don't replace entities like %WSP+; and such (because we
   * have to generate a recognizer/DFA off of those, we CANNOT replace %% by % or we risk turning
   * %%WSP+; into %WSP+; which would then be recognized as a charClassEntity, but it was double-escaped, so
   * should NOT be recognized as one.
   *
   * For unparse, we can just replace everything, including the %% by %. The resulting string contains
   * no entities at all.
   */
  def replaceAll(
    input: String,
    context: Option[ThrowsSDE] = None,
    forUnparse: Boolean = false,
    allowByteEntities: Boolean = true
  ): String = {
    if (!input.contains("%")) { return input } // No entities, no replacement.

    val startOfPossibleEntity = input.indexOf("%")
    val inputUntilPossibleEntity = input.substring(0, startOfPossibleEntity)
    val inputWithPossibleEntity = input.substring(startOfPossibleEntity)

    if (!inputWithPossibleEntity.contains("%%")) {
      // No escaped percents, just process
      val processedInput =
        process(inputWithPossibleEntity, input, context, forUnparse, allowByteEntities)
      val fullResult = inputUntilPossibleEntity + processedInput
      return fullResult
    }

    // We have escaped percent literals, we need to also determine if we ended
    // in an escaped percent.  If so, we'll need to append it to the result.
    val endedWithDoublePercent = hasDoublePercentEnding(inputWithPossibleEntity)
    val splitByDoublePercent =
      inputWithPossibleEntity.split("%%") // Effectively removes escaped percents

    // Below we process each token and at the end call mkString to add back in
    // the escaped % literals if necessary. This works automatically except in the case where a
    // double percent occurred at the end of the input.
    val replaced = splitByDoublePercent.map { token =>
      process(token, input, context, forUnparse)
    }
    val replacementForDoublePercents =
      if (forUnparse) "%" else "%%"
    val recomposedWithLiteralPercents =
      inputUntilPossibleEntity + replaced
        .mkString(replacementForDoublePercents) + (if (endedWithDoublePercent)
                                                     replacementForDoublePercents
                                                   else "")
    recomposedWithLiteralPercents
  }

  // Replacement helper function
  //   entity = what you are replacing (informational only)
  //   unicode = what you are replacing it with
  //   pattern = what you are replacing via RegEx match on this pattern
  private def replace(input: String, chars: List[(String, String, Matcher)]): String = {
    var res: String = input
    chars.foreach {
      case (entity, unicode, m) => {
        m.reset(res)
        res = m.replaceAll(unicode)
      }
    }
    res
  }

}

import org.apache.daffodil.lib.util.OnStack

object EntityReplacer extends OnStack(new EntityReplacer)

/**
 * We refer to the process of checking and replacing entities within
 * DFDL string literals as "cooking" them.
 *
 * They start raw. Thawing replaces XML literals like &quot;
 * Cooking replaces DFDL literals, as appropriate depending on
 *
 * * forUnparse - things like WSP+ just turn into a single space when unparsing. For parsing
 * they get processed later into a lexical analyzer/DFA part.
 * * Which property the cooking is for: This determines also error checking of which kinds of
 * entites (raw aka byte, character entities, or character-class entities).
 * * Of the character class entities, which specifically are allowed or disallowed.
 *
 * The taxonomy of trait mixins and classes combine to implement the right combination of
 * the above.
 */

trait StringLiteralCookerMixin extends Converter[String, String] {

  override protected def convert(b: String, context: ThrowsSDE, forUnparse: Boolean) =
    cook(b, context, forUnparse)

  def cook(raw: String, context: ThrowsSDE, forUnparse: Boolean): String
}

/**
 * See description of StringLiteralCookerMixin for details on raw, thaw, and cook phases.
 */
trait ListStringLiteralCookerMixin extends Converter[String, List[String]] {

  override protected def convert(b: String, context: ThrowsSDE, forUnparse: Boolean) =
    cook(b, context, forUnparse)

  protected def cook(raw: String, context: ThrowsSDE, forUnparse: Boolean): List[String]
}

abstract class UpperCaseToken(propNameArg: String = null)
  extends AutoPropNameBase(propNameArg)
  with Converter[String, String] {

  override protected def convert(b: String, context: ThrowsSDE, forUnparse: Boolean) =
    cook(b, context, forUnparse)

  def cook(raw: String, context: ThrowsSDE, forUnparse: Boolean): String = raw.trim.toUpperCase
}

/**
 * String values in the infoset, string results of DFDL's xpath-like expressions are of this kind.
 *
 *  This is the kind of string literal you can use within an expression.
 */
sealed abstract class StringLiteralBase(
  propNameArg: String,
  protected val allowByteEntities: Boolean
) extends AutoPropNameBase(propNameArg)
  with StringLiteralCookerMixin {

  /**
   * Thawed means XML entities have been replaced. So &amp; is &, etc.
   */
  private def thaw(raw: String) = XMLUtils.unescape(raw)

  private val whitespaceMatcher = """.*(\s+).*""".r

  def cook(raw: String, context: ThrowsSDE, forUnparse: Boolean): String = {
    val hasWhitespace: Boolean = raw match {
      case whitespaceMatcher(_) => true
      case _ => false
    }
    context.schemaDefinitionWhen(
      hasWhitespace,
      "For %s, the string (%s) must not contain any whitespace. Use DFDL Entities for whitespace characters.",
      propName,
      raw
    )
    testRaw(raw, context)
    val thawed = thaw(raw)
    testThawed(thawed, context)
    val cooked = EntityReplacer { e =>
      e.replaceAll(thawed, Some(context), forUnparse, allowByteEntities)
    }
    testCooked(cooked, context)
    cooked
  }

  protected def testRaw(raw: String, context: ThrowsSDE): Unit = {
    // do nothing
  }
  protected def testThawed(thawed: String, context: ThrowsSDE): Unit = {
    // do nothing
  }
  protected def testCooked(cooked: String, context: ThrowsSDE): Unit = {
    // do nothing
  }
}

class StringLiteral(pn: String, allowByteEntities: Boolean)
  extends StringLiteralBase(pn, allowByteEntities)

sealed trait NonEmptyMixin { self: StringLiteralBase =>

  protected def testCooked(cooked: String, context: ThrowsSDE): Unit = {
    context.schemaDefinitionUnless(cooked.length > 0, "Cannot be an empty string.")
  }
}

sealed trait SingleCharacterMixin { self: StringLiteralBase =>

  override protected def testCooked(cooked: String, context: ThrowsSDE): Unit = {
    context.schemaDefinitionUnless(
      cooked.length == 1 ||
        cooked =:= "%%",
      "For property dfdl:%s the length of string must be exactly 1 character.",
      propName
    )
  }
}

class SingleCharacterLiteral(pn: String, allowByteEntities: Boolean)
  extends StringLiteralBase(pn, allowByteEntities)
  with SingleCharacterMixin

trait DisallowedCharClassEntitiesMixin {

  protected def propName: String
  protected def disallowedCharClassEntities: Seq[String]

  /**
   * The raw string to test is supplied as the first parameter
   * A list of disallowed character class entities is supplied as the 2nd parameter
   * This list is added to the regex string below
   * Ex regex: (?:^|[^%])(?:%%)*(%(?:NL|ES);)
   * (?:^|[^%]) look for the beginning of the string or a non-% character
   * (?:%%)* look for an even number of %'s
   * (%(?:NL|ES);) look for any single % left after previous match along with and NL or ES followed by a ;
   *
   */
  private lazy val disallowedRegex = {
    val disallowedCharClassMatch = disallowedCharClassEntities.map(Pattern.quote).mkString("|")
    val regexString = "(?:^|[^%])(?:%%)*(%(?:" + disallowedCharClassMatch + ");)"
    regexString.r
  }

  protected def testRaw(raw: String, context: ThrowsSDE): Unit = {
    val matchedGroups = disallowedRegex.findAllMatchIn(raw).map { _.group(1) }
    context.schemaDefinitionUnless(
      matchedGroups.isEmpty,
      "Property dfdl:%s contains disallowed character class(es): %s",
      propName,
      matchedGroups.mkString(", ")
    )
  }
}

class StringLiteralNoCharClassEntities(pn: String, allowByteEntities: Boolean)
  extends StringLiteralBase(pn, allowByteEntities)
  with DisallowedCharClassEntitiesMixin {

  override val disallowedCharClassEntities = Seq("NL", "ES", "WSP", "WSP+", "WSP*")
  override def testRaw(raw: String, context: ThrowsSDE) =
    super[DisallowedCharClassEntitiesMixin].testRaw(raw, context)
}

class SingleCharacterLiteralNoCharClassEntities(pn: String, allowByteEntities: Boolean)
  extends StringLiteralBase(pn, allowByteEntities)
  with DisallowedCharClassEntitiesMixin
  with SingleCharacterMixin {

  override val disallowedCharClassEntities = Seq("NL", "ES", "WSP", "WSP+", "WSP*")
  override protected def testRaw(raw: String, context: ThrowsSDE) =
    super.testRaw(raw, context)
}

class StringLiteralNonEmptyNoCharClassEntitiesNoByteEntities(pn: String = null)
  extends StringLiteralBase(pn, false)
  with DisallowedCharClassEntitiesMixin
  with NonEmptyMixin {

  override val disallowedCharClassEntities = Seq("NL", "ES", "WSP", "WSP+", "WSP*")
  override def testRaw(raw: String, context: ThrowsSDE) =
    super[DisallowedCharClassEntitiesMixin].testRaw(raw, context)

  override protected def testCooked(cooked: String, context: ThrowsSDE) =
    super[NonEmptyMixin].testCooked(cooked, context)
}

class StringLiteralNoCharClassEntitiesNoByteEntities(pn: String = null)
  extends StringLiteralNoCharClassEntities(pn, allowByteEntities = false)

class SingleCharacterLiteralNoCharClassEntitiesNoByteEntities(pn: String = null)
  extends SingleCharacterLiteralNoCharClassEntities(pn, allowByteEntities = false)

class SingleCharacterLiteralNoCharClassEntitiesWithByteEntities(pn: String = null)
  extends SingleCharacterLiteralNoCharClassEntities(pn, allowByteEntities = true)

class StringLiteralESEntityWithByteEntities(pn: String)
  extends StringLiteralNoCharClassEntities(pn, allowByteEntities = true)
  with DisallowedCharClassEntitiesMixin {

  override val disallowedCharClassEntities = Seq("NL", "WSP", "WSP+", "WSP*")
  override def testRaw(raw: String, context: ThrowsSDE) =
    super[DisallowedCharClassEntitiesMixin].testRaw(raw, context)
}

class SingleCharacterLiteralESEntityWithByteEntities(pn: String = null)
  extends StringLiteralESEntityWithByteEntities(pn)
  with SingleCharacterMixin

class SingleCharacterLiteralOrEmptyString(pn: String, allowByteEntities: Boolean)
  extends StringLiteralBase(pn, allowByteEntities)
  with SingleCharacterMixin

sealed abstract class AutoPropNameBase(propNameArg: String) extends Serializable {

  private lazy val autoPropName = Misc.stripSuffix(
    Misc.toInitialLowerCaseUnlessAllUpperCase(Misc.getNameFromClass(this)),
    "Cooker"
  )

  final protected lazy val propName: String =
    if (propNameArg eq null) autoPropName else propNameArg
}

sealed abstract class ListOfStringLiteralBase(
  propNameArg: String,
  protected val allowByteEntities: Boolean
) extends AutoPropNameBase(propNameArg)
  with ListStringLiteralCookerMixin {

  private lazy val olc = oneLiteralCooker

  protected def cook(raw: String, context: ThrowsSDE, forUnparse: Boolean): List[String] = {

    // ignore leading, trailing, and repeating whitespae
    val rawList = raw.split("\\s").filterNot(_ == "").toList

    val cooked = {
      val cookedList: ListBuffer[String] = ListBuffer.empty
      rawList.foreach(x => {
        val cooked = olc.cook(x, context, forUnparse)
        cookedList += cooked
      })
      cookedList.toList
    }
    testCooked(cooked, context)
    cooked
  }

  protected def oneLiteralCooker: StringLiteralBase

  protected def testCooked(cooked: List[String], context: ThrowsSDE): Unit = {
    // do nothing
  }
}

sealed trait ListOfSingleCharacterMixin { self: ListOfStringLiteralBase =>

  def cookCharacters(raw: String, context: ThrowsSDE, forUnparse: Boolean) =
    cook(raw, context, forUnparse).map { s => JChar.valueOf(s(0)) }
}

class ListOfStringLiteral(pn: String, allowByteEntities: Boolean)
  extends ListOfStringLiteralBase(pn, allowByteEntities) {

  override protected val oneLiteralCooker: StringLiteralBase =
    new StringLiteral(propName, allowByteEntities)
}

class NonEmptyListOfStringLiteral(pn: String, allowByteEntities: Boolean)
  extends ListOfStringLiteral(pn, allowByteEntities) {

  override def testCooked(cookedList: List[String], context: ThrowsSDE) = {
    context.schemaDefinitionUnless(
      cookedList.length > 0,
      "Property dfdl:%s cannot be empty string. Use dfdl:%s='%%ES;' for empty string.",
      propName,
      propName
    )
  }
}

trait ListOfStringOneOrMoreLiteral { self: ListOfStringLiteralBase =>

  override protected def testCooked(cooked: List[String], context: ThrowsSDE): Unit = {
    context.schemaDefinitionUnless(
      cooked.length > 0,
      "Property %s cannot be empty string.",
      propName
    )
  }
}

class ListOfSingleCharacterLiteral(pn: String, allowByteEntities: Boolean)
  extends ListOfStringLiteralBase(pn, allowByteEntities)
  with ListOfSingleCharacterMixin {

  override protected val oneLiteralCooker =
    new SingleCharacterLiteral(propName, allowByteEntities)
}

class ListOfSingleCharacterLiteralNoCharClassEntitiesNoByteEntities(pn: String = null)
  extends ListOfStringLiteralBase(pn, false)
  with ListOfSingleCharacterMixin {

  override protected val oneLiteralCooker =
    new SingleCharacterLiteralNoCharClassEntitiesNoByteEntities(propName)
}

class StringLiteralNonEmpty(pn: String, allowByteEntities: Boolean)
  extends StringLiteralBase(pn, allowByteEntities)
  with NonEmptyMixin {

  override def testCooked(cooked: String, context: ThrowsSDE): Unit = {
    super.testCooked(cooked, context)
  }
}

class ListOfStringLiteralNonEmptyNoCharClassEntitiesNoByteEntities(pn: String = null)
  extends ListOfStringLiteralBase(pn, false) {

  override protected val oneLiteralCooker =
    new StringLiteralNonEmpty(propName, allowByteEntities = false)
      with DisallowedCharClassEntitiesMixin {

      override val disallowedCharClassEntities = Seq("NL", "ES", "WSP", "WSP+", "WSP*")
      override def testRaw(raw: String, context: ThrowsSDE) =
        super[DisallowedCharClassEntitiesMixin].testRaw(raw, context)
    }
}

class ListOfStringLiteralNoCharClass_NL_ES_EntitiesNoByteEntities(pn: String = null)
  extends ListOfStringLiteralBase(pn, false) {

  override protected val oneLiteralCooker =
    new StringLiteral(propName, allowByteEntities = false)
      with DisallowedCharClassEntitiesMixin {

      override val disallowedCharClassEntities = Seq("NL", "ES")
      override def testRaw(raw: String, context: ThrowsSDE) =
        super[DisallowedCharClassEntitiesMixin].testRaw(raw, context)
    }
}

class SingleCharacterLineEndingOrCRLF_NoCharClassEntitiesNoByteEntities(pn: String = null)
  extends StringLiteral(pn, allowByteEntities = false)
  with DisallowedCharClassEntitiesMixin {

  private val validNLs: List[Char] = List('\u000A', '\u000D', '\u0085', '\u2028')

  override val disallowedCharClassEntities = Seq("NL", "ES", "WSP", "WSP+", "WSP*")
  override def testRaw(raw: String, context: ThrowsSDE) =
    super[DisallowedCharClassEntitiesMixin].testRaw(raw, context)

  /**
   * Check that length is 1 (single char) except for CRLF case, and that it's a line ending char.
   */
  override protected def testCooked(cooked: String, context: ThrowsSDE): Unit = {
    context.schemaDefinitionUnless(
      cooked.length == 1 || cooked =:= "\r\n",
      "For property dfdl:%s, the length of string must be exactly 1 character, except for CRLF case when it can be 2 characters.",
      propName
    )
    context.schemaDefinitionUnless(
      validNLs.contains(cooked(0)),
      "'%s' is not a valid new line character for dfdl:%s",
      cooked,
      propName
    )
  }
}

class NonEmptyListOfStringLiteralCharClass_ES_WithByteEntities(pn: String)
  extends ListOfStringLiteralBase(pn, true) {

  override protected val oneLiteralCooker =
    new StringLiteral(propName, allowByteEntities = true)
      with DisallowedCharClassEntitiesMixin {

      override val disallowedCharClassEntities = Seq("NL", "WSP", "WSP+", "WSP*")
      override def testRaw(raw: String, context: ThrowsSDE) =
        super[DisallowedCharClassEntitiesMixin].testRaw(raw, context)
    }

  override def testCooked(cookedList: List[String], context: ThrowsSDE) = {
    context.schemaDefinitionUnless(
      cookedList.length > 0,
      "Property dfdl:%s cannot be empty string. Use dfdl:%s='%%ES;' for empty string.",
      propName,
      propName
    )
  }
}

class DelimiterCookerNoES(pn: String) extends DelimiterCooker(pn) {

  override def oneDelimiterLiteralCooker: StringLiteralBase =
    new StringLiteralNoCharClassEntities(propName, true) with DisallowedCharClassEntitiesMixin {

      // Disallow "%ES" in the string raw. Disallow "%WSP*" when it is
      // the only value in the string raw
      override val disallowedCharClassEntities = Seq("ES")
      override def testRaw(raw: String, context: ThrowsSDE) = {
        context.schemaDefinitionUnless(
          raw != "%WSP*;",
          """For dfdl:%s the WSP* entity cannot appear on it's own when dfdl:lengthKind="delimited".""",
          propName
        )
        super[DisallowedCharClassEntitiesMixin].testRaw(raw, context)
      }
    }
}

class DelimiterCookerNoSoleES(pn: String) extends DelimiterCooker(pn) {

  override def oneDelimiterLiteralCooker: StringLiteralBase =
    new StringLiteralBase(propName, true) {

      override def testRaw(raw: String, context: ThrowsSDE): Unit = {
        // "Neither %ES or %WSP*;" can appear on their own when lenghtKind is delimited.
        context.schemaDefinitionUnless(
          raw != "%ES;",
          """For dfdl:%s the ES entity cannot appear on its own when dfdl:lengthKind="delimited".""",
          propName
        )
        context.schemaDefinitionUnless(
          raw != "%WSP*;",
          """For dfdl:%s the WSP* entity cannot appear on its own when dfdl:lengthKind="delimited".""",
          propName
        )
      }
    }
}

class DelimiterCooker(pn: String) extends ListOfStringLiteralBase(pn, true) {

  def oneDelimiterLiteralCooker: StringLiteralBase = new StringLiteral(pn, true)

  // zero length allowed for constants
  private val constantCooker = new ListOfStringLiteral(propName, true) {
    override val oneLiteralCooker = oneDelimiterLiteralCooker
  }

  private val runtimeCooker = new ListOfStringLiteral(propName, true)
    with ListOfStringOneOrMoreLiteral {
    override val oneLiteralCooker = oneDelimiterLiteralCooker
  }

  override def convertRuntime(
    b: String,
    context: ThrowsSDE,
    forUnparse: Boolean
  ): List[String] =
    runtimeCooker.convertRuntime(b, context, forUnparse)

  override def convertConstant(
    b: String,
    context: ThrowsSDE,
    forUnparse: Boolean
  ): List[String] =
    constantCooker.convertConstant(b, context, forUnparse)

  /**
   * Overriding as def since this is a usage error to call.
   */
  override protected def oneLiteralCooker: StringLiteralBase =
    Assert.usageError("not to be used.")
  override protected def cook(
    raw: String,
    context: ThrowsSDE,
    forUnparse: Boolean
  ): List[String] = Assert.usageError("not to be used")
}
