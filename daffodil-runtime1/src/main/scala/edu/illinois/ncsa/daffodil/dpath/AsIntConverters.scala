package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc

object AsIntConverters {
  /**
   * Parsers don't always insert the smallest numeric type into the infoset.
   * Sometimes we get a BigInt when an Int would have sufficed, but the
   * parsers don't always do that. This is a workaround. Really the parsers
   * should be inserting the *right thing* into the infoset.
   */
  def asInt(n: Any): Int = {
    val value = n match {
      case b: Byte => b.toInt
      case s: Short => s.toInt
      case i: Int => i
      case l: Long => l.toInt
      case bi: BigInt => bi.toInt
      case _ => Assert.invariantFailed("Unsupported conversion to Int. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }
  def asByte(n: Any): Byte = {
    val value = n match {
      case b: Byte => b
      case s: Short => s.toByte
      case i: Int => i.toByte
      case l: Long => l.toByte
      case bi: BigInt => bi.toByte
      case _ => Assert.invariantFailed("Unsupported conversion to Byte. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }
  def asShort(n: Any): Short = {
    val value = n match {
      case b: Byte => b.toShort
      case s: Short => s
      case i: Int => i.toShort
      case l: Long => l.toShort
      case bi: BigInt => bi.toShort
      case _ => Assert.invariantFailed("Unsupported conversion to Short. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asLong(n: Any): Long = {
    val value = n match {
      case b: Byte => b.toLong
      case s: Short => s.toLong
      case i: Int => i.toLong
      case l: Long => l
      case bi: BigInt => bi.toLong
      case jbi: java.math.BigInteger => jbi.longValue()
      case _ => Assert.invariantFailed("Unsupported conversion to Long. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asBigInt(n: Any): BigInt = {
    val value = n match {
      case b: Byte => BigInt(b)
      case s: Short => BigInt(s)
      case i: Int => BigInt(i)
      case l: Long => BigInt(l)
      case bi: BigInt => bi
      case _ => Assert.invariantFailed("Unsupported conversion to BigInt. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asFloat(n: Any): Float = {
    val value = n match {
      case f: Float => f
      case d: Double => d.toFloat
      case b: Byte => b.toFloat
      case s: Short => s.toFloat
      case i: Int => i.toFloat
      case l: Long => l.toFloat
      case bi: BigInt => bi.toFloat
      case bd: BigDecimal => bd.toFloat
      case _ => Assert.invariantFailed("Unsupported conversion to Float. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asDouble(n: Any): Double = {
    val value = n match {
      case f: Float => f.toDouble
      case d: Double => d
      case b: Byte => b.toDouble
      case s: Short => s.toDouble
      case i: Int => i.toDouble
      case l: Long => l.toDouble
      case bi: BigInt => bi.toDouble
      case bd: BigDecimal => bd.toDouble
      case _ => Assert.invariantFailed("Unsupported conversion to Double. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asBigDecimal(n: Any): BigDecimal = {
    val value = n match {
      //
      // Not converting Float to string first causes precision issues 
      // that round-half-to-even doesn't resolve correctly.  BigDecimal.valueOf(3.455) turns into 3.454999. 
      // HALF_EVEN rounding mode would round this to 3.45 rather than the desired 3.46.
      case f: Float => BigDecimal(f.toString)
      case d: Double => BigDecimal.valueOf(d)
      case b: Byte => BigDecimal.valueOf(b)
      case s: Short => BigDecimal.valueOf(s)
      case i: Int => BigDecimal.valueOf(i)
      case l: Long => BigDecimal.valueOf(l)
      case bi: BigInt => BigDecimal(bi)
      case bd: BigDecimal => bd
      case _ => Assert.invariantFailed("Unsupported conversion to BigDecimal. %s of type %s".format(
        n, Misc.getNameFromClass(n)))
    }
    value
  }

  def asBoolean(n: Any): Boolean = {
    n.asInstanceOf[Boolean]
  }
}
