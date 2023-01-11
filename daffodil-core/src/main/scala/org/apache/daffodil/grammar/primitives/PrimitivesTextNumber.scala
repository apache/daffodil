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

package org.apache.daffodil.grammar.primitives

import com.ibm.icu.text.DecimalFormat
import org.apache.daffodil.cookers.EntityReplacer
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.Delimiter
import org.apache.daffodil.processors.parsers.ConvertTextCombinatorParser
import org.apache.daffodil.processors.parsers.ConvertTextStandardNumberParser
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.processors.TextNumberFormatEv
import org.apache.daffodil.processors.unparsers.ConvertTextCombinatorUnparser
import org.apache.daffodil.processors.unparsers.ConvertTextNumberUnparser
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.schema.annotation.props.gen.TextNumberRounding
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeDouble

case class ConvertTextCombinator(e: ElementBase, value: Gram, converter: Gram)
  extends Terminal(e, !(value.isEmpty || converter.isEmpty)) {

  lazy val parser = new ConvertTextCombinatorParser(e.termRuntimeData, value.parser, converter.parser)

  override lazy val unparser = new ConvertTextCombinatorUnparser(e.termRuntimeData, value.unparser, converter.unparser)
}

/**
 * This is a separate object so that we can easily unit test these subtle regular
 * expressions.
 */
private[primitives]
object TextNumberPatternUtils {

  //  DFDL v1.0 Spec says
  //  It is a Schema Definition Error if any symbols other than "0", "1" through "9" or #
  //  are used in the vpinteger region of the pattern.
  //
  // The prefix and suffix chars can surround the vpinteger region, and there can be
  // a positive and a negative pattern.
  //
  //
  //
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
     val negBeforeV = posBeforeV
     val negAfterV = posAfterV
     val negSuffix = posPrefix
     // don't forget the ^ and $ (start of data, end of data) because we want
     // the match to consume all the characters, starting at the beginning.
    val vPattern=
     s"""^${posPrefix}${posBeforeV}V${posAfterV}${posSuffix}""" ++
       s"""(?:;${negPrefix}${negBeforeV}(?:V${negAfterV}${negSuffix})?)?$$"""
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
    val negPs = posPs
    val negAfterP = posAfterP
    val negSuffix = posPrefix
    // don't forget the ^ and $ (start of data, end of data) because we want
    // the match to consume all the characters, starting at the beginning.
    val vPattern =
    s"""^${posPrefix}$posPs${posAfterP}${posSuffix}(?:;${negPrefix}$negPs${negAfterP}${negSuffix})?$$"""
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
    val negBeforeP = posBeforeP
    val negPs = posPs
    val negSuffix = posPrefix
    // don't forget the ^ and $ (start of data, end of data) because we want
    // the match to consume all the characters, starting at the beginning.
    val vPattern =
    s"""^${posPrefix}${posBeforeP}$posPs${posSuffix}(?:;${negPrefix}${negBeforeP}$negPs${negSuffix})?$$"""
    val re = vPattern.r
    re
  }

  lazy val vRegexZoned= {
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
    val vPattern = s"""^${prefix}${beforeV}V${afterV}${suffix}$$""" // only positive pattern allowed
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
  def removeUnquotedPV(pattern: String) : String = {
    //    val uniqueQQString = "alsdflslkjskjslkkjlkkjfppooiipsldsflj" // FIXME: use split-based technique.
    //    // A single regex that matches an unquoted character
    //    // where the quoting char is self quoting requires
    //    // a zero-width look behind that matches a potentially
    //    // unbounded number of quote chars. That's not allowed.
    //    //
    //    // Consider ''''''P. We want a regex that matches only the P here
    //    // because it is preceded by an even number of quotes.
    //    //
    //    // I did some tests, and the formulations you think might work
    //    // such as from stack-overflow, don't work.
    //    // (See test howNotToUseRegexLookBehindWithReplaceAll)
    //    //
    //    // So we use this brute force technique of replacing all ''
    //    // first, so we have only single quotes to deal with.
    //    pattern.replaceAll("''", uniqueQQString).
    //      replaceAll("(?<!')P", "").
    //      replaceAll("(?<!')V", "").
    //      replaceAll(uniqueQQString, "''")
    //  }
    val regex = "'.*?'|.".r
    val res = regex.findAllIn(pattern)
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
   * "000.00" returns 2, so text "12345" yields 123.45
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
  final protected lazy val patternNoQuoted = {
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

  protected final lazy val hasV = patternNoQuoted.contains("V")
  protected final lazy val hasP = patternNoQuoted.contains("P")

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
   * "000.00" returns 2, so text "12345" yields 123.45
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
      // check for things incompatible with "V"
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
    }
    else pattern
  }

  final protected def checkPatternWithICU(e: ElementBase) = {
    // Load the pattern to make sure it is valid
    try {
      if (hasV || hasP) {
        new DecimalFormat(runtimePattern)
      } else {
        new DecimalFormat(pattern)
      }
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
              | and V (virtual decimal point) characters removed: %s""".stripMargin, ex)
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
      r.findFirstMatchIn(patternNoQuoted) match {
        case Some(r(_, _, afterV, _, _, _, _, _)) => afterV.length
        case None =>
          e.SDE(
            s"""The dfdl:textNumberPattern '%s' contains 'V' (virtual decimal point).
               | Other than the sign indicators, it can contain only
               | '#', then digits 0-9 then 'V' then digits 0-9.
               | The positive part of the dfdl:textNumberPattern is mandatory.""".stripMargin('|'),
            pattern)
      }
    } else if (hasP) {
      val rr = TextNumberPatternUtils.pOnRightRegexStandard
      val rl = TextNumberPatternUtils.pOnLeftRegexStandard
      val rightMatch = rr.findFirstMatchIn(patternNoQuoted)
      val leftMatch = rl.findFirstMatchIn(patternNoQuoted)
      (leftMatch, rightMatch) match {
        case (None, None) => e.SDE(
        """The dfdl:textNumberPattern '%s' contains 'P' (virtual decimal point positioners).
            |However, it did not match the allowed syntax which allows the sign indicator
            |plus digits on only one side of the P symbols.""".stripMargin,
          pattern)
        case (Some(rl(_, ps, digits, _, negPre, negPs, negDigits, _)), None) => {
          checkPPPMatchSyntax(ps, digits, negPre, negPs, negDigits)
          ps.length + digits.length
        }
        case (None, Some(rr(_, digits, ps, _, negPre, negDigits, negPs, _))) => {
          checkPPPMatchSyntax(ps, digits, negPre, negPs, negDigits)
          - ps.length // negate value.
        }
        case _ => Assert.invariantFailed("Should not match both left P and right P regular expressions.")
      }
    } else {
      0 // there is no V nor P, so there is no virtual decimal point scaling involved.
    }
  }

  /**
   * Check for common errors in the pattern matches for 'P' patterns.
   *
   * @param ps        match string for positive pattern P characters
   * @param digits    match string for positive pattern digits (and sharps)
   * @param negPre    negative prefix match (null if there is no negative pattern at all)
   * @param negPs     match string for negative pattern P character (or null)
   * @param negDigits match string for negative pattern P digits (and sharps)
   */
  private def checkPPPMatchSyntax(ps: String, digits: String, negPre: String, negPs: String, negDigits: String): Unit = {
    Assert.invariant(ps.length >= 1)
    digits.length >= 1
    if (negPre ne null) {
      e.schemaDefinitionUnless(ps == negPs,
        """In the dfdl:textNumberPattern '%s' the P Symbols do not match
          | between positive and negative patterns""".stripMargin,
        pattern)
      e.schemaDefinitionUnless(digits == negDigits,
        """In the dfdl:textNumberPattern '%s' the '#' and digit symbols do not match
          | between positive and negative patterns""".stripMargin,
        pattern)
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

    if (textDecimalVirtualPoint > 0) {
      e.primType match {
        case PrimType.Double | PrimType.Float | PrimType.Decimal => // ok
        case _ => e.SDE("The dfdl:textNumberPattern has a virtual decimal point 'V', but the type is an integer-only type: %s." ++
          "The type must be xs:decimal, xs:double, or xs:float", e.primType.globalQName.toPrettyString)

      }
      e.schemaDefinitionUnless(e.textStandardBase == 10,
        "The dfdl:textNumberPattern 'V' (virtual decimal point) requires that " +
          "dfdl:textStandardBase is 10, but its value was %s",
        e.textStandardBase)
    }

    e.schemaDefinitionWhen(pattern.startsWith(";"), "The positive part of the dfdl:textNumberPattern is required. The dfdl:textNumberPattern cannot begin with ';'.")
    checkPatternWithICU(e)

    val (roundingIncrement: MaybeDouble, roundingMode) =
      e.textNumberRounding match {
        case TextNumberRounding.Explicit => (MaybeDouble(e.textNumberRoundingIncrement), One(e.textNumberRoundingMode))
        case TextNumberRounding.Pattern => (MaybeDouble.Nope, Nope)
      }

    val (infRep, nanRep) = e.primType match {
      case PrimType.Double | PrimType.Float => (One(e.textStandardInfinityRep), One(e.textStandardNaNRep))
      case _ => (Nope, Nope)
    }

    val isInt = e.primType match {
      case PrimType.Double | PrimType.Float | PrimType.Decimal => false
      case _ => true
    }

    // If the pattern contains any of these characters, we need to set both
    // group and decimal separators, even if the pattern doesn't contain the
    // associated character. This is because even when the pattern does not
    // contain the grouping/decimal separators, ICU stills seems to take the
    // separators into account. And since ICU provides default values based on
    // locales, not setting them can cause subtle locale related bugs. We must
    // also require the separators if the prim type is not an integer type,
    // since ICU will use them even if the pattern does not specify them.
    val requireDecGroupSeps =
      patternNoQuoted.contains(",") || patternNoQuoted.contains(".") ||
      patternNoQuoted.contains("E") || patternNoQuoted.contains("@") ||
      !isInt

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

    val ev = new TextNumberFormatEv(
      e.tci,
      decSep,
      groupSep,
      One(e.textStandardExponentRepEv),
      infRep,
      nanRep,
      e.textNumberCheckPolicy,
      runtimePattern,
      e.textNumberRounding,
      roundingMode,
      roundingIncrement,
      zeroRepsRaw,
      isInt,
      e.primType)
    ev.compile(tunable)
    ev
  }

  lazy val parser: Parser =
    new ConvertTextStandardNumberParser(textNumberFormatEv, zeroRepsRegex, e.elementRuntimeData, textDecimalVirtualPoint)

  override lazy val unparser: Unparser =
    new ConvertTextNumberUnparser(textNumberFormatEv, zeroRepUnparse, e.elementRuntimeData, textDecimalVirtualPoint)
}
