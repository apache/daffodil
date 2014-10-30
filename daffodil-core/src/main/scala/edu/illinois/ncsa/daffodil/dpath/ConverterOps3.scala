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