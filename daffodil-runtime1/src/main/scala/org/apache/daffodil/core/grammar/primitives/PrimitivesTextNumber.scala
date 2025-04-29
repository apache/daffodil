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

package org.apache.daffodil.core.grammar.primitives

import org.apache.daffodil.core.dsom._
import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.core.grammar.Terminal
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.cookers.EntityReplacer
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.TextNumberCheckPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.TextNumberRounding
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.MaybeDouble
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.processors.Delimiter
import org.apache.daffodil.runtime1.processors.TextNumberFormatEv
import org.apache.daffodil.runtime1.processors.parsers.ConvertTextCombinatorParser
import org.apache.daffodil.runtime1.processors.parsers.ConvertTextStandardNumberParser
import org.apache.daffodil.runtime1.processors.parsers.Parser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser
import org.apache.daffodil.unparsers.runtime1.ConvertTextCombinatorUnparser
import org.apache.daffodil.unparsers.runtime1.ConvertTextNumberUnparser

import com.ibm.icu.impl.number.AffixPatternProvider
import com.ibm.icu.impl.number.Padder.PadPosition
import com.ibm.icu.impl.number.PatternStringParser
import com.ibm.icu.impl.number.PatternStringParser.ParsedPatternInfo
import com.ibm.icu.text.DecimalFormat

case class ConvertTextCombinator(e: ElementBase, value: Gram, converter: Gram)
  extends Terminal(e, !(value.isEmpty || converter.isEmpty)) {

  lazy val parser =
    new ConvertTextCombinatorParser(e.termRuntimeData, value.parser, converter.parser)

  override lazy val unparser =
    new ConvertTextCombinatorUnparser(e.termRuntimeData, value.unparser, converter.unparser)
}

/**
 * This is a separate object so that we can easily unit test these subtle regular
 * expressions.
 */
private[primitives] object TextNumberPatternUtils {

  //  DFDL v1.0 Spec says
  //  It is a Schema Definition Error if any symbols other than "0", "1" through "9" or #
  //  are used in the vpinteger region of the pattern.
  //
  // The prefix and suffix chars can surround the vpinteger region, and there can be
  // a positive and a negative pattern.

  /**
   * The prefix and suffix are quoted any single character
   * or a single non-special character (excepting + and - which are allowed
   * though they appear in the table of special characters for textNumberFormat).
   *
   * By default these regex will not allow line-ending chars either.
   */
  val prefixChars = """(?:'.'|[^0-9#PVE\.\,\;\*\@\'])?""" // same for suffix.

  /**
   * sharps and at least 1 digit before the V.
   */
  val beforeVChars = """#*[0-9]+"""

  /**
   * one or more digits after the V. (No trailing sharps)
   */
  val afterVChars = """[0-9]+"""

  /**
   * The negative pattern must be a valid pattern, but
   * does NOT have to match the positive one, as it is
   * always ignored.
   *
   * The shortest possible match is just a # or just a digit.
   * If a V appears it must have a digit on either side.
   */
  val negVNumberChars = """#|#+|#*[0-9]+(?:V[0-9]+)?"""

  /**
   * A regex which matches textNumberPatterns that legally use the V
   * (implied decimal point) character.
   */
  lazy val vRegexStandard = {

    //
    // Each of the capture groups is defined here in order
    //
    val posPrefix = s"""($prefixChars)"""
    val posBeforeV = s"""($beforeVChars)"""
    val posAfterV = s"""($afterVChars)"""
    val posSuffix = posPrefix
    val negPrefix = posPrefix
    val negNumber = s"""($negVNumberChars)"""
    val negSuffix = posPrefix
    // don't forget the ^ and $ (start of data, end of data) because we want
    // the match to consume all the characters, starting at the beginning.
    val vPattern =
      s"""^${posPrefix}${posBeforeV}V${posAfterV}${posSuffix}""" +
        s"""(?:;${negPrefix}${negNumber}${negSuffix})?$$"""
    val re = vPattern.r
    re
  }

  /*
   * There are two regex used for 'P' pattern case. One if the P chars
   * appear to the left of the digits, the other for P chars on the right
   * of the digits.
   *
   * When P is on left of the digits, there must be 1 or more digits.
   * When P is on right of the digits there must be 1 or more digits, and there may be sharps.
   */
  val afterPChars = """[0-9]+""" // P is left, digits are after
  val beforePChars = """#*[0-9]+""" // P is right, digits are before (sharps before the digits).

  /**
   * For P patterns, with P on the left, the negative pattern can be just # or just a digit
   * or can be P chars followed by one or more digits.
   */
  val negPOnLeftNumberChars = "#|P*[0-9]+"

  /**
   * For P patterns, with P on the right, the negative patterncan be just # or just a digit
   * or can be sharps, then digits, then P chars.
   */
  val negPOnRightNumberChars = "#|#*[0-9]+P*"

  /**
   * A regex which matches textNumberPatterns that legally use the P
   * (implied decimal point) character, on the LEFT of the digits.
   */
  lazy val pOnLeftRegexStandard = {
    //
    // Each of the capture groups is defined here in order
    //
    val posPrefix = s"""($prefixChars)"""
    val posPs = s"""(P+)"""
    val posAfterP = s"""($afterPChars)"""
    val posSuffix = posPrefix
    val negPrefix = posPrefix
    val negPNumber = s"""($negPOnLeftNumberChars)"""
    val negSuffix = posPrefix
    // don't forget the ^ and $ (start of data, end of data) because we want
    // the match to consume all the characters, starting at the beginning.
    val vPattern =
      s"""^${posPrefix}$posPs${posAfterP}${posSuffix}(?:;${negPrefix}${negPNumber}${negSuffix})?$$"""
    val re = vPattern.r
    re
  }

  /**
   * A regex which matches textNumberPatterns that legally use the P
   * (implied decimal point) character, on the RIGHT of the digits.
   */
  lazy val pOnRightRegexStandard = {
    //
    // Each of the capture groups is defined here in order
    //
    val posPrefix = s"""($prefixChars)"""
    val posBeforeP = s"""($beforePChars)"""
    val posPs = s"""(P+)"""
    val posSuffix = posPrefix
    val negPrefix = posPrefix
    val negPNumber = s"""($negPOnRightNumberChars)"""
    val negSuffix = posPrefix
    // don't forget the ^ and $ (start of data, end of data) because we want
    // the match to consume all the characters, starting at the beginning.
    val vPattern =
      s"""^${posPrefix}${posBeforeP}$posPs${posSuffix}(?:;${negPrefix}${negPNumber}${negSuffix})?$$"""
    val re = vPattern.r
    re
  }

  lazy val vRegexZoned = {
    // Note: for zoned, can only have a positive pattern
    // Prefix or suffix can only be '+' character, to
    // indicate leading or trailing sign, and can only
    // one of those.
    //
    // Also we're not allowing the # character, since I think that
    // makes no sense for zoned with virtual decimal point.
    //
    val prefixChars = """\+?""" // only + for prefix/suffix
    val aroundVChars = """[0-9]+"""
    //
    // capture groups are defined here in sequence
    //
    val prefix = s"""($prefixChars)"""
    val beforeV = s"""($aroundVChars)"""
    val afterV = beforeV
    val suffix = prefix
    val vPattern =
      s"""^${prefix}${beforeV}V${afterV}${suffix}$$""" // only positive pattern allowed
    val re = vPattern.r
    re
  }

  /**
   * Checks a pattern for suitability with V (virtual decimal point) in the pattern
   * in the context of zoned textNumberRep.
   *
   * This method is here in this object for unit testing purposes.
   *
   * @param patternStripped the dfdl:textNumberPattern pattern - with all quoting removed.
   * @return None if the pattern is illegal syntax for use with V (virtual decimal point)
   *         otherwise Some(N) where N is the number of digits to the right of the V character.
   */
  def textNumber_V_DecimalVirtualPointForZoned(patternStripped: String): Option[Int] = {
    val r = TextNumberPatternUtils.vRegexZoned
    r.findFirstMatchIn(patternStripped) match {
      // note: cannot have both a prefix and suffix. Only one of them.
      case Some(r(pre, _, afterV, suf)) if (pre.length + suf.length <= 1) =>
        Some(afterV.length)
      case _ => None
    }
  }

  lazy val pOnLeftRegexZoned = {
    // Note: for zoned, can only have a positive pattern
    // Prefix or suffix can only be '+' character, to
    // indicate leading or trailing sign, and can only
    // one of those.
    //
    // Also we're not allowing the # character, since I think that
    // makes no sense for zoned with virtual decimal point.
    //
    val prefixChars = """\+?""" // only + for prefix/suffix
    //
    // capture groups are defined here in sequence
    //
    val prefix = s"""($prefixChars)"""
    val ps = s"""(P+)"""
    val afterP = s"""($afterPChars)"""
    val suffix = prefix
    val vPattern = s"""^${prefix}$ps${afterP}${suffix}$$""" // only positive pattern allowed
    val re = vPattern.r
    re
  }

  lazy val pOnRightRegexZoned = {
    // Note: for zoned, can only have a positive pattern
    // Prefix or suffix can only be '+' character, to
    // indicate leading or trailing sign, and can only
    // one of those.
    //
    // Also we're not allowing the # character, since I think that
    // makes no sense for zoned with virtual decimal point.
    //
    val prefixChars = """\+?""" // only + for prefix/suffix
    //
    // capture groups are defined here in sequence
    //
    val prefix = s"""($prefixChars)"""
    val beforeP = s"""($beforePChars)"""
    val ps = s"""(P+)"""
    val suffix = prefix
    val vPattern = s"""^${prefix}${beforeP}$ps${suffix}$$""" // only positive pattern allowed
    val re = vPattern.r
    re
  }

  /**
   * The apos character (') is the quoting character in ICU patterns (and DFDL textNumberPattern).
   * This removes any unquoted P or V characters (which are not implemented by ICU)
   * leaving a pattern string that is suitable for use with ICU.
   * @param pattern the textNumberPattern string
   * @return the pattern string with all unquoted P and unquoted V removed.
   */
  def removeUnquotedPV(pattern: String): String = {
    val regex = "'.*?'|.".r
    val res = regex
      .findAllIn(pattern)
      .filterNot(_ == "P")
      .filterNot(_ == "V")
      .mkString("")
    res
  }
}

trait ConvertTextNumberMixin {

  def e: ElementBase

  /**
   * Convenience. The original value of textNumberPattern
   */
  @inline
  final protected def pattern = e.textNumberPattern

  /**
   * Analogous to the property dfdl:binaryDecimalVirtualPoint
   *
   * Value is 0 if there is no virtual decimal point.
   *
   * Value is the number of digits to the right of the 'V' for V patterns.
   *
   * For P patterns, the value is the number of P characters when
   * the P chars are on the right of the digits, or it is negated if the
   * P chars are on the left of the digits.
   *
   * Examples:
   *
   * "000V00" returns 2, so text "12345" yields 123.45
   *
   * "PP000" returns 5, so text "123" yields 0.00123
   *
   * "000PP" returns -2 so text "123" yields 12300.0
   */
  protected def textDecimalVirtualPointFromPattern: Int

  /**
   * The pattern with escaped characters removed.
   * This can be reliably searched for pattern characters,
   * but cannot be used as an operational pattern given that
   * potentially lots of things have been removed from it.
   */
  final protected lazy val patternWithoutEscapedChars = {
    // note: tick == apos == ' == single quote.
    // First remove entirely all escaped ticks ie., ''
    val noEscapedTicksRegex = """''""".r
    val patternNoEscapedTicks = noEscapedTicksRegex.replaceAllIn(pattern, "")
    // Next remove all tick-escaped characters entirely
    val noQuotedRegex = """'[^']+'""".r
    val res = noQuotedRegex.replaceAllIn(patternNoEscapedTicks, "")
    // the remaining string contains only pattern special characters
    // and regular non-pattern characters
    res
  }

  protected final lazy val hasV = patternWithoutEscapedChars.contains("V")
  protected final lazy val hasP = patternWithoutEscapedChars.contains("P")

  /**
   * Analogous to the property dfdl:binaryDecimalVirtualPoint
   *
   * Value is 0 if there is no virtual decimal point.
   *
   * Value is the number of digits to the right of the 'V' for V patterns.
   *
   * For P patterns, the value is the number of P characters when
   * the P chars are on the right of the digits, or it is negated if the
   * P chars are on the left of the digits.
   *
   * Examples:
   *
   * "000V00" returns 2, so text "12345" yields 123.45
   *
   * "PP000" returns 2, so text "123" yields 0.00123
   *
   * "000PP" returns -2 so text "123" yields 12300.0
   */
  final lazy val textDecimalVirtualPoint: Int = {
    lazy val virtualPoint = textDecimalVirtualPointFromPattern
    if (hasV) {
      //
      // check for things incompatible with "V"
      //
      Assert.invariant(virtualPoint >= 1) // if this fails the regex is broken.
      virtualPoint
    } else if (hasP) {
      //
      // check for things incompatible with "P"
      //
      Assert.invariant(virtualPoint != 0) // if this fails the regex is broken.
      virtualPoint
    } else {
      0 // no virtual point since we have neither V nor P in the pattern.
    }
  }

  //
  // We need 2 patterns
  // 1. The original textNumberPattern string from the schema
  // 2. Same but with the P or V removed.
  //
  // We need to use the original textNumberPattern in diagnostic messages
  // since that's what the schema author wrote and expects to see.
  // But if the pattern has P or V, then we need ICU at runtime to use the original
  // but with the P or V removed since the P and V don't actually represent anything in the data.
  final protected lazy val runtimePattern = {
    if (hasV || hasP) {
      val patternWithoutPV = TextNumberPatternUtils.removeUnquotedPV(pattern)
      patternWithoutPV
    } else pattern
  }

  /**
   * Validates the textNumberPattern using ICU's PatternStringParser. Although this class is
   * public, it is not part of the ICU API, so it maybe not be stable. However, this is what
   * DecimalFormat uses internally to parse patterns and extract information for initialization,
   * and that likely won't change significantly. Plus, by using this class instead of parsing
   * with DeciamlFormat we can return the ParsedPatternInfo to give callers raw access to what
   * was in the pattern without having to parse it ourselves. This can be useful for additional
   * validation or logic using parts of the pattern ICU might normally ignore.
   */
  final protected def checkPatternWithICU(e: ElementBase): ParsedPatternInfo = {
    try {
      val patternToCheck = if (hasV || hasP) runtimePattern else pattern
      val parsedPatternInfo = PatternStringParser.parseToPatternInfo(patternToCheck)
      parsedPatternInfo
    } catch {
      case ex: IllegalArgumentException =>
        if (hasV || hasP) {
          // we don't know what the diagnostic message will say here
          // since it is from the ICU library.
          // That library has only seen the pattern with the P and V
          // removed. The messages might show that pattern which would
          // confuse the schema author since that's not the pattern
          // they wrote.
          //
          // So we try to explain....
          e.SDE(
            """Invalid textNumberPattern.
              | The errors are about the pattern with the P (decimal scaling position)
              | and V (virtual decimal point) characters removed: %s""".stripMargin,
            ex
          )
        } else {
          e.SDE("Invalid textNumberPattern: %s", ex)
        }
    }
  }
}

case class ConvertTextStandardNumberPrim(e: ElementBase)
  extends Terminal(e, true)
  with ConvertTextNumberMixin {

  final override protected lazy val textDecimalVirtualPointFromPattern: Int = {
    if (hasV) {
      val r = TextNumberPatternUtils.vRegexStandard
      r.findFirstMatchIn(patternWithoutEscapedChars) match {
        case Some(r(_, beforeV, afterV, _, _, negNum, _)) => {
          checkPosNegNumPartSyntax(beforeV + "V" + afterV, negNum)
          afterV.length
        }
        case Some(_) | None =>
          e.SDE(
            s"""The dfdl:textNumberPattern '%s' contains 'V' (virtual decimal point).
               | Other than the sign indicators, it can contain only
               | '#', then digits 0-9 then 'V' then digits 0-9.
               | The positive part of the dfdl:textNumberPattern is mandatory.""".stripMargin(
              '|'
            ),
            pattern
          )
      }
    } else if (hasP) {
      val rr = TextNumberPatternUtils.pOnRightRegexStandard
      val rl = TextNumberPatternUtils.pOnLeftRegexStandard
      val rightMatch = rr.findFirstMatchIn(patternWithoutEscapedChars)
      val leftMatch = rl.findFirstMatchIn(patternWithoutEscapedChars)
      (leftMatch, rightMatch) match {
        case (None, None) =>
          e.SDE(
            """The dfdl:textNumberPattern '%s' contains 'P' (virtual decimal point positioners).
            |However, it did not match the allowed syntax which allows the sign indicator
            |plus digits on only one side of the P symbols.""".stripMargin,
            pattern
          )
        case (Some(rl(_, ps, digits, _, negPre, negNum, _)), None) => {
          checkPosNegNumPartSyntax(ps + digits, negNum)
          ps.length + digits.length
        }
        case (None, Some(rr(_, digits, ps, _, negPre, negNum, _))) => {
          checkPosNegNumPartSyntax(digits + ps, negNum)
          -ps.length // negate value.
        }
        case _ =>
          Assert.invariantFailed(
            "Should not match both left P and right P regular expressions."
          )
      }
    } else {
      0 // there is no V nor P, so there is no virtual decimal point scaling involved.
    }
  }

  /**
   * Check for common errors in the pattern matches for 'P' patterns.
   *
   * @param posNum positive number pattern part (combined ps and digits on left or right)
   * @param negNum negative number pattern part
   */
  private def checkPosNegNumPartSyntax(posNum: String, negNum: String): Unit = {
    if (negNum ne null) {
      // there is a negative part
      if (negNum.length == 1) {
        Assert.invariant(
          negNum.matches("#|[0-9]")
        ) // must be # or digit or wouldn't have matched.
        // do nothing. This is ok. (The negNum is ignored anyway)
      } else {
        // we issue a warning if there is a negNum pattern part and it is not
        // identical to the positive part.
        e.schemaDefinitionWarningUnless(
          WarnID.TextNumberPatternWarning,
          posNum == negNum,
          s"""The negative numeric part of the textNumberPattern: '${negNum}' is being ignored.
             | It should either be just '#' or should exactly
             | match the numeric part of the positive pattern,
             | which is '${posNum}'.""".stripMargin
        )
      }
    }
  }

  val zeroRepsRaw = e.textStandardZeroRep.filter { _ != "" }
  val zeroRepsRegex = zeroRepsRaw.map { zr =>
    val d = new Delimiter()
    d.compileDelimiter(zr, e.ignoreCaseBool)
    // add '^' and '$' to require the regular expression to match the entire
    // string as a zero rep instead of just part of it
    val ignoreCaseStr = if (e.ignoreCaseBool) "(?i)" else ""
    val regex = (ignoreCaseStr + "^" + d.delimRegExParseDelim + "$").r
    regex
  }
  val zeroRepUnparse: Maybe[String] = zeroRepsRaw.headOption.map { zr =>
    EntityReplacer { _.replaceForUnparse(zr) }
  }

  lazy val textNumberFormatEv: TextNumberFormatEv = {
    val primNumeric = e.primType.asInstanceOf[PrimType.PrimNumeric]

    if (textDecimalVirtualPoint > 0) {
      e.schemaDefinitionUnless(
        !primNumeric.isInteger,
        "The dfdl:textNumberPattern has a virtual decimal point 'V', but the type is an integer-only type: %s." ++
          "The type must be xs:decimal, xs:double, or xs:float",
        e.primType.globalQName.toPrettyString
      )

      e.schemaDefinitionUnless(
        e.textStandardBase == 10,
        "The dfdl:textNumberPattern 'V' (virtual decimal point) requires that " +
          "dfdl:textStandardBase is 10, but its value was %s",
        e.textStandardBase
      )
    }

    e.schemaDefinitionWhen(
      pattern.startsWith(";"),
      "The positive part of the dfdl:textNumberPattern is required. The dfdl:textNumberPattern cannot begin with ';'."
    )
    val parsedPatternInfo = checkPatternWithICU(e)

    val (roundingIncrement: MaybeDouble, roundingMode) =
      e.textNumberRounding match {
        case TextNumberRounding.Explicit =>
          (MaybeDouble(e.textNumberRoundingIncrement), One(e.textNumberRoundingMode))
        case TextNumberRounding.Pattern => (MaybeDouble.Nope, Nope)
      }

    // If the pattern contains any of these characters, we need to set both group and decimal
    // separators, even if the pattern doesn't contain the associated character. This is because
    // even when the pattern does not contain the grouping/decimal separators, ICU stills seems
    // to take the separators into account. And since ICU provides default values based on
    // locales, not setting them can cause subtle locale related bugs. We also must required
    // separators when parsing is lax, which parses decimals and grouping separators regardless
    // of the pattern.
    val requireDecGroupSeps =
      patternWithoutEscapedChars.contains(",") || patternWithoutEscapedChars.contains(".") ||
        patternWithoutEscapedChars.contains("E") || patternWithoutEscapedChars.contains("@") ||
        (e.textNumberCheckPolicy eq TextNumberCheckPolicy.Lax)

    val decSep =
      if (requireDecGroupSeps) {
        One(e.textStandardDecimalSeparatorEv)
      } else {
        Nope
      }

    val groupSep =
      if (requireDecGroupSeps) {
        One(e.textStandardGroupingSeparatorEv)
      } else {
        Nope
      }

    // ICU does not have a way to set the pad position to after an affix if the positive pattern
    // does not have that affix. For example, "* 0", will always have a pad position of
    // BEFORE_PREFIX, with no way to set it to AFTER_PREFIX because the positive pattern has no
    // prefix. In cases where formats do not have a postive affix but want to specify the pad
    // position to AFTER, we allow them to do so in the negative pattern. For example, a pattern
    // of "* 0;-* 0" will have a pad position of AFTER_PREFIX. ICU normally ignores the negative
    // pattern for pad position. Note that we require the pad char to be defined on the same
    // affix or else it is ignored.
    val posPadLoc = parsedPatternInfo.positive.paddingLocation
    val negPadLoc =
      if (parsedPatternInfo.negative != null) parsedPatternInfo.negative.paddingLocation
      else null
    val posPrefix = parsedPatternInfo.getString(AffixPatternProvider.FLAG_POS_PREFIX)
    val posSuffix = parsedPatternInfo.getString(AffixPatternProvider.FLAG_POS_SUFFIX)
    val icuPadPosition =
      (posPadLoc, negPadLoc, posPrefix, posSuffix) match {
        case (PadPosition.BEFORE_PREFIX, PadPosition.AFTER_PREFIX, "", _) =>
          MaybeInt(DecimalFormat.PAD_AFTER_PREFIX)
        case (PadPosition.BEFORE_SUFFIX, PadPosition.AFTER_SUFFIX, _, "") =>
          MaybeInt(DecimalFormat.PAD_AFTER_SUFFIX)
        case _ => MaybeInt.Nope
      }

    val ev = new TextNumberFormatEv(
      e.tci,
      decSep,
      groupSep,
      One(e.textStandardExponentRepEv),
      One(e.textStandardInfinityRep),
      One(e.textStandardNaNRep),
      e.textNumberCheckPolicy,
      runtimePattern,
      e.textNumberRounding,
      roundingMode,
      roundingIncrement,
      zeroRepsRaw,
      icuPadPosition,
      e.primType
    )
    ev.compile(tunable)
    ev
  }

  lazy val parser: Parser =
    new ConvertTextStandardNumberParser(
      textNumberFormatEv,
      zeroRepsRegex,
      e.elementRuntimeData,
      textDecimalVirtualPoint
    )

  override lazy val unparser: Unparser =
    new ConvertTextNumberUnparser(
      textNumberFormatEv,
      zeroRepUnparse,
      e.elementRuntimeData,
      textDecimalVirtualPoint
    )
}
