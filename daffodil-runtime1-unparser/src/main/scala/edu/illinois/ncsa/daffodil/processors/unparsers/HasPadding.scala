package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.util.Maybe

/**
 * When dfdl:lengthKind is 'explicit' (and dfdl:length is an expression),
 * 'delimited', 'prefixed', 'pattern' the data value is padded to the length given
 * by the XSD minLength facet for type 'xs:string' or
 * dfdl:textOutputMinLength property for other types.
 *
 *
 * dfdl:textOutputMinLength:
 * Only used when dfdl:textPadKind is 'padChar' and dfdl:lengthKind is
 * 'delimited', 'prefixed', 'pattern', 'explicit' (when dfdl:length is an expression)
 * or 'endOfParent', and type is not xs:string
 * Specifies the minimum content length during unparsing for simple types
 * that do not allow the XSD minLength facet to be specified.
 * For dfdl:lengthKind 'delimited', 'pattern' and 'endOfParent' the length units
 * are always characters, for other dfdl:lengthKinds the length units are
 * specified by the dfdl:lengthUnits property.
 * If dfdl:textOutputMinLength is zero or less than the length of the
 * representation text then no padding occurs.
 */
trait HasPadding {

  def pad: Maybe[Char]
  def padToLength: Int

  def addRightPadding(str: String): String = {
    val inputLength = str.length

    val res =
      if (!pad.isDefined || padToLength < inputLength || padToLength == 0) { str }
      else {
        val numCharsToAdd = padToLength - inputLength
        addRightPadding(str, numCharsToAdd)
      }
    res
  }

  def addLeftPadding(str: String): String = {
    val inputLength = str.length

    val res =
      if (!pad.isDefined || padToLength < inputLength || padToLength == 0) { str }
      else {
        val numCharsToAdd = padToLength - inputLength
        addLeftPadding(str, numCharsToAdd)
      }
    res
  }

  def addPadding(str: String): String = {
    val inputLength = str.length

    val res = if (!pad.isDefined || padToLength < inputLength || padToLength == 0) { str }
    else {
      val numCharsToAddTotal = padToLength - inputLength
      val numAddToLeft = Math.ceil(numCharsToAddTotal / 2).toInt
      val numAddToRight = numCharsToAddTotal - numAddToLeft

      addRightPadding(addLeftPadding(str, numAddToLeft), numAddToRight)
    }

    res
  }
  
  private def append(sb: StringBuilder, numCharsToAppend: Int): StringBuilder = {
    for (i <- 1 to numCharsToAppend) { sb.append(pad.get) }
    sb
  }

  private def addRightPadding(str: String, numCharsToPad: Int): String = {
    val sb = append(new StringBuilder(str), numCharsToPad)
    sb.toString
  }
  private def addLeftPadding(str: String, numCharsToPad: Int): String = {
    val sb = append(new StringBuilder, numCharsToPad)
    sb.append(str)
    sb.toString
  }

}