package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.TextJustificationType

trait HasPadding {
  def justificationTrim: TextJustificationType.Type
  def padChar: String

  def removeRightPadding(str: String): String = str.reverse.dropWhile(c => c == padChar.charAt(0)).reverse
  def removeLeftPadding(str: String): String = str.dropWhile(c => c == padChar.charAt(0))
  def removePadding(str: String): String = removeRightPadding(removeLeftPadding(str))

  def trimByJustification(str: String): String = {
    val result = justificationTrim match {
      case TextJustificationType.None => str
      case TextJustificationType.Right => removeLeftPadding(str)
      case TextJustificationType.Left => removeRightPadding(str)
      case TextJustificationType.Center => removePadding(str)
    }
    result
  }
}