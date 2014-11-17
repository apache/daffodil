package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.processors._
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.OnStack
import edu.illinois.ncsa.daffodil.util.PreSerialization
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import scala.math.BigDecimal.RoundingMode
import edu.illinois.ncsa.daffodil.util.Bits
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import java.text.ParsePosition
import com.ibm.icu.util.DFDLCalendar
import com.ibm.icu.util.SimpleTimeZone
import com.ibm.icu.util.TimeZone
import java.nio.ByteBuffer
import com.ibm.icu.util.DFDLDateTime
import com.ibm.icu.util.DFDLDate
import com.ibm.icu.util.DFDLTime
import AsIntConverters._

case object BooleanToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = if (asBoolean(a) == true) 1L else 0L
}

case object BooleanToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = if (asBoolean(a) == true) "true" else "false"
}

case object DateTimeToDate extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case dt: DFDLDateTime => dt.toDate
      case _ => throw new NumberFormatException("xs:dateTime expected but an invalid type was received.")
    }
  }
}
case object DateTimeToTime extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case dt: DFDLDateTime => dt.toTime
      case _ => throw new NumberFormatException("xs:dateTime expected but an invalid type was received.")
    }
  }
}
case object DateToDateTime extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case d: DFDLDate => d.toDateTime
      case _ => throw new NumberFormatException("xs:date expected but an invalid type was received.")
    }
  }
}
case object DecimalToInteger extends Converter {
  override def computeValue(a: Any, dstate: DState) = asBigDecimal(a).toBigInt()
}
case object DecimalToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigDecimal(a)
    if (res < Long.MinValue || res > Long.MaxValue) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    res.toLong
  }
}
case object DecimalToDouble extends Converter {
  override def computeValue(a: Any, dstate: DState) = asBigDecimal(a).toDouble
}
case object DecimalToNonNegativeInteger extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigDecimal(a)
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))
    res.toBigInt()
  }
}
case object DecimalToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigDecimal(a).toBigInt
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))

    if (res > NodeInfo.UnsignedLong.Max) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}
case object DoubleToDecimal extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigDecimal(asDouble(a))
}
case object DoubleToFloat extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val d = asDouble(a)
    val res = d.toFloat
    res
  }
}
case object DoubleToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigDecimal(asDouble(a))
    if (res < Long.MinValue || res > Long.MaxValue) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    res.toLong
  }
}
case object DoubleToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = BigDecimal(asDouble(a)).toBigInt
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned long.".format(res))
    if (res > NodeInfo.UnsignedLong.Max) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}
case object FloatToDouble extends Converter {
  override def computeValue(a: Any, dstate: DState) = asFloat(a).toDouble
}
case object IntegerToDecimal extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigDecimal(asBigInt(a))
}
case object IntegerToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigInt(a)
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned long.".format(res))
    if (res > NodeInfo.UnsignedLong.Max) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}
case object LongToBoolean extends Converter {
  override def computeValue(a: Any, dstate: DState) = if (asLong(a) == 0) false else true
}
case object LongToByte extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val l = asLong(a)
    if (l > Byte.MaxValue || l < Byte.MinValue) throw new NumberFormatException("Value %s out of range for Byte type.".format(l))
    l.toByte
  }
}
case object LongToDecimal extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigDecimal(asLong(a))
}
case object LongToDouble extends Converter {
  override def computeValue(a: Any, dstate: DState) = asLong(a).toDouble
}
case object LongToFloat extends Converter {
  override def computeValue(a: Any, dstate: DState) = asLong(a).toFloat
}
case object LongToInt extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val l = asLong(a)
    if (l > Int.MaxValue || l < Int.MinValue) throw new NumberFormatException("Value %s out of range for Int type.".format(l))
    l.toInt
  }
}

case object LongToInteger extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigInt(asLong(a))
}

case object LongToShort extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val l = asLong(a)
    if (l > Short.MaxValue || l < Short.MinValue) throw new NumberFormatException("Value %s out of range for Short type.".format(l))
    l.toShort
  }
}

case object LongToArrayIndex extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asLong(a)
    val upperLimit = DaffodilTunableParameters.maxOccursBounds
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an array index.".format(res))
    if (res > upperLimit) throw new NumberFormatException("Value %s out of range for an array index\nThe current (tunable) maximum is %s.".format(res, upperLimit))
    res
  }
}
case object LongToUnsignedByte extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asLong(a)
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned byte.".format(res))
    if (res > 255) throw new NumberFormatException("Value %s out of range for unsigned byte.".format(res))
    res.toShort
  }
}
case object LongToUnsignedInt extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asLong(a)
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned int.".format(res))
    if (res > 0xFFFFFFFFL) throw new NumberFormatException("Value %s out of range for unsigned int.".format(res))
    res
  }
}
case object LongToUnsignedShort extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asLong(a)
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned short.".format(res))
    if (res > 65535) throw new NumberFormatException("Value %s out of range for unsigned short.".format(res))
    res.toInt
  }
}

case object LongToNonNegativeInteger extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = BigInt(asLong(a))
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))
    res
  }
}

case object LongToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = BigInt(asLong(a))
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to a non-negative integer.".format(res))
    else res
  }
}

case object StringToBoolean extends Converter {
  override def computeValue(a: Any, dstate: DState) = if (a.asInstanceOf[String].length == 0) false else true
}
case object StringToDecimal extends Converter {
  override def computeValue(a: Any, dstate: DState) = BigDecimal(a.asInstanceOf[String])
}
case object StringToDouble extends Converter {
  override def computeValue(a: Any, dstate: DState) = a.asInstanceOf[String].toDouble
}
case object StringToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res =
      try {
        a.asInstanceOf[String].toLong
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type long: " + nfe.getMessage())
          throw e
        }
      }
    res
  }
}
case object StringToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = BigInt(a.asInstanceOf[String])
    if (res < 0) throw new NumberFormatException("Negative value %s cannot be converted to an unsigned long.".format(res))
    if (res > NodeInfo.UnsignedLong.Max) throw new NumberFormatException("Value %s out of range for UnsignedLong type.".format(res))
    else res
  }
}

/**
 * Summary: Computes the effective boolean value of the sequence $arg. 
 *
 * If $arg is the empty sequence, fn:boolean returns false.
 * 
 * If $arg is a sequence whose first item is a node, fn:boolean returns true.
 * 
 * If $arg is a singleton value of type xs:boolean or a derived from 
 * xs:boolean, fn:boolean returns $arg.
 * 
 * If $arg is a singleton value of type xs:string or a type derived from 
 * xs:string, xs:anyURI or a type derived from xs:anyURI or xs:untypedAtomic, 
 * fn:boolean returns false if the operand value has zero length; otherwise 
 * it returns true.
 * 
 * If $arg is a singleton value of any numeric type or a type derived 
 * from a numeric type, fn:boolean returns false if the operand value 
 * is NaN or is numerically equal to zero; otherwise it returns true.
 * 
 * In all other cases, fn:boolean raises a type error [err:FORG0006].
 */
case object FNToBoolean extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = a match {
      case b: Boolean => b
      case s: String => if (s.length == 0) false else true
      case d: Double => if (d.isNaN() || d == 0) false else true
      case f: Float => if (f.isNaN() || f == 0) false else true
      //
      // BigDecimal does not have a representation for NaN or Infinite
      case bd: BigDecimal => if (bd.compare(java.math.BigDecimal.ZERO) == 0) false else true
      case b: Byte => if (b == 0) false else true
      case s: Short => if (s == 0) false else true
      case i: Int => if (i == 0) false else true
      case l: Long => if (l == 0) false else true
      case bi: BigInt => if (bi.compare(java.math.BigInteger.ZERO) == 0) false else true
      // TODO: Once sequences are supported, fill in these case statements
      //case s: Sequence if s.length == 0 => false
      //case s: Sequence if s(0) == Node => true
      case _ => throw new NumberFormatException("Invalid argument type.")
    }
    res
  }
}
