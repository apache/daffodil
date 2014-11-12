package edu.illinois.ncsa.daffodil.dpath

object NodeInfoUtils {
  /**
   * When operating on two operands, this computes the type to which
   * they are mutually converted before the operation. Such as if you
   * add an Int and a Double, the Int is converted to Double before adding, and
   * the result type is Double.
   */
  def generalize(aExpr: Expression, bExpr: Expression): NodeInfo.Kind = {
    val a = aExpr.inherentType
    val b = bExpr.inherentType
    if (a == b) a
    else if (a.isSubtypeOf(b)) b
    else if (b.isSubtypeOf(a)) a
    else
      (a, b) match {
        case (s: NodeInfo.String.Kind, _) => NodeInfo.String
        case (_, s: NodeInfo.String.Kind) => NodeInfo.String
        case (NodeInfo.Float, NodeInfo.Double) => NodeInfo.Double
        case (NodeInfo.Double, NodeInfo.Float) => NodeInfo.Double
        case (NodeInfo.Decimal, NodeInfo.Double) => NodeInfo.Decimal
        case (NodeInfo.Double, NodeInfo.Decimal) => NodeInfo.Decimal
        case (NodeInfo.Boolean, bt: NodeInfo.Numeric.Kind) => bt
        case (bt: NodeInfo.Numeric.Kind, NodeInfo.Boolean) => bt
        case (it: NodeInfo.Long.Kind, NodeInfo.ArrayIndex) => NodeInfo.ArrayIndex
        case _ => aExpr.SDE("Static type error: expressions '%s' and '%s' have incompatible types %s and %s.", aExpr.text, bExpr.text, a, b)
      }
  }
}