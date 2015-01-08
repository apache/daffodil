package edu.illinois.ncsa.daffodil.dpath

import scala.math.BigDecimal.long2bigDecimal
import scala.math.BigInt.long2bigInt

import AsIntConverters.asBigDecimal
import AsIntConverters.asBigInt
import AsIntConverters.asInt
import AsIntConverters.asLong
import edu.illinois.ncsa.daffodil.util.Misc

case object NumericToString extends ToString
case object DateTimeToString extends ToString
case object HexBinaryToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val bytes = a.asInstanceOf[Array[Byte]]
    val hex = Misc.bytes2Hex(bytes)
    hex
  }
}
case object HexStringToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res =
      try {
        val str = a.asInstanceOf[String]
        java.lang.Long.parseLong(str, 16)
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type long: " + nfe.getMessage())
          throw e
        }
      }
    res
  }
}
case object HexStringToUnsignedLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res =
      try {
        val str = a.asInstanceOf[String]
        BigInt(str, 16)
      } catch {
        case nfe: NumberFormatException => {
          val e = new NumberFormatException("Cannot convert to type unsignedLong: " + nfe.getMessage())
          throw e
        }
      }
    res
  }
}
case object BigIntToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigInt(a)
    if (res < Long.MinValue || res > Long.MaxValue) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    res.toLong
  }
}
case object IntToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = asInt(a).toLong
}
case object UnsignedLongToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    val res = asBigDecimal(a)
    if (res < Long.MinValue || res > Long.MaxValue) throw new NumberFormatException("Value %s out of range for Long type.".format(res))
    res.toLong
  }
}
case object UnsignedIntToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    // Assert.invariant(a.isInstanceOf[Long])
    asLong(a)
  }
}
case object ArrayIndexToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = {
    asLong(a)
  }
}
case object ShortToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = asInt(a).toLong
}
case object UnsignedShortToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = asInt(a).toLong
}
case object ByteToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = asInt(a).toLong
}
case object UnsignedByteToLong extends Converter {
  override def computeValue(a: Any, dstate: DState) = asInt(a).toLong
}