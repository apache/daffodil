package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.XMLUtils

object DPathUtil {

  /**
   * Whether a string is a DFDL expression (an XPath expression surrounded by brackets).
   *
   * This function does not verify a string conforms to the DFDL subset of XPath
   */
  def isExpression(expression: String): Boolean = {
    val trimmed = expression.trim
    trimmed.startsWith("{") && trimmed.endsWith("}") &&
      (trimmed(1) != '{')
  }
  /**
   * Returns the XPath expression contained in a DFDL expression (an XPath expression surrounded by brackets).
   *
   * @param expression a valid DFDL expression
   */
  def getExpression(expression: String): String = {
    val v = expression.trim
    v.substring(1, v.length - 1)
  }

}
