package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.XMLUtils

object DPathUtil {

  /**
   * Whether a string is a DFDL expression (an XPath expression surrounded by brackets).
   *
   * This function does not verify a string conforms to the DFDL subset of XPath
   */
  def isExpression(expression: String): Boolean =
    expression.startsWith("{") && expression.endsWith("}") &&
      (expression(1) != '{')

  /**
   * Returns the XPath expression contained in a DFDL expression (an XPath expression surrounded by brackets).
   *
   * @param expression a valid DFDL expression
   */
  def getExpression(expression: String): String = {
    val v = expression.trim
    v.substring(1, v.length - 1)
  }

  // TODO: When we get rid of this deprecated thing, part of that is part of getting
  // rid of the other QName oriented code that is also deprecated. There
  // should be no use of this "expandedTypeName" stuff anymore.
  @deprecated("use PrimType and/or NodeInfo instead.", "2014-09-17")
  def convertTypeString(expandedTypeName: String): NodeInfo.Kind = {
    Assert.usage(expandedTypeName != null)
    expandedTypeName match {
      case XMLUtils.XSD_STRING => NodeInfo.String
      case XMLUtils.XSD_BYTE => NodeInfo.Byte
      case XMLUtils.XSD_SHORT => NodeInfo.Short
      case XMLUtils.XSD_INT => NodeInfo.Int
      case XMLUtils.XSD_LONG => NodeInfo.Long
      case XMLUtils.XSD_UNSIGNED_BYTE => NodeInfo.UnsignedByte
      case XMLUtils.XSD_UNSIGNED_SHORT => NodeInfo.UnsignedShort
      case XMLUtils.XSD_UNSIGNED_INT => NodeInfo.UnsignedInt
      case XMLUtils.XSD_DOUBLE => NodeInfo.Double
      case XMLUtils.XSD_FLOAT => NodeInfo.Double
      case XMLUtils.XSD_BOOLEAN => NodeInfo.Boolean
      case XMLUtils.XSD_HEX_BINARY => NodeInfo.HexBinary
      case XMLUtils.XSD_DATE => NodeInfo.Date
      case XMLUtils.XSD_DATE_TIME => NodeInfo.DateTime
      case XMLUtils.XSD_TIME => NodeInfo.Time
      case XMLUtils.XSD_NON_NEGATIVE_INTEGER => NodeInfo.NonNegativeInteger
      case XMLUtils.XSD_INTEGER => NodeInfo.Integer
      case XMLUtils.XSD_UNSIGNED_LONG => NodeInfo.UnsignedLong
      case XMLUtils.XSD_DECIMAL => NodeInfo.Decimal
    }
  }

}
