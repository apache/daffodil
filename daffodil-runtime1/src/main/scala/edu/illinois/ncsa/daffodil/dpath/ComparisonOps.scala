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
import com.ibm.icu.util.SimpleTimeZone
import com.ibm.icu.util.TimeZone
import java.nio.ByteBuffer
import AsIntConverters._
import edu.illinois.ncsa.daffodil.calendar.DFDLDateTime
import edu.illinois.ncsa.daffodil.calendar.DFDLTime
import edu.illinois.ncsa.daffodil.calendar.DFDLDate

case object LT_Boolean extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = {
    val b1 = v1.asInstanceOf[Boolean]
    val b2 = v2.asInstanceOf[Boolean]

    val res = (!b1 && b2)
    res
  }
}
case object GT_Boolean extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = {
    val b1 = v1.asInstanceOf[Boolean]
    val b2 = v2.asInstanceOf[Boolean]

    val res = (b1 && !b2)
    res
  }
}
case object LE_Boolean extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { !GT_Boolean.operate(v1, v2) }
}
case object GE_Boolean extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { !LT_Boolean.operate(v1, v2) }
}

case object LT_Date extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLDate] < v2.asInstanceOf[DFDLDate] }
}
case object GT_Date extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLDate] > v2.asInstanceOf[DFDLDate] }
}
case object LE_Date extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLDate] <= v2.asInstanceOf[DFDLDate] }
}
case object GE_Date extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLDate] >= v2.asInstanceOf[DFDLDate] }
}
case object LT_Time extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLTime] < v2.asInstanceOf[DFDLTime] }
}
case object GT_Time extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLTime] > v2.asInstanceOf[DFDLTime] }
}
case object LE_Time extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLTime] <= v2.asInstanceOf[DFDLTime] }
}
case object GE_Time extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLTime] >= v2.asInstanceOf[DFDLTime] }
}
case object LT_DateTime extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLDateTime] < v2.asInstanceOf[DFDLDateTime] }
}
case object GT_DateTime extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLDateTime] > v2.asInstanceOf[DFDLDateTime] }
}
case object LE_DateTime extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLDateTime] <= v2.asInstanceOf[DFDLDateTime] }
}
case object GE_DateTime extends CompareOpBase {
  def operate(v1: Any, v2: Any): Boolean = { v1.asInstanceOf[DFDLDateTime] >= v2.asInstanceOf[DFDLDateTime] }
}

case object LT_String extends StringCompareOp {
  def operate(v1: Any, v2: Any): Boolean = {
    val res = compare(v1, v2) < 0
    res
  }
}
case object GT_String extends StringCompareOp {
  def operate(v1: Any, v2: Any): Boolean = {
    val res = compare(v1, v2) > 0
    res
  }
}
case object LE_String extends StringCompareOp {
  def operate(v1: Any, v2: Any): Boolean = {
    val res = compare(v1, v2) <= 0
    res
  }
}
case object GE_String extends StringCompareOp {
  def operate(v1: Any, v2: Any): Boolean = {
    val res = compare(v1, v2) >= 0
    res
  }
}
case object LT_Decimal extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigDecimal(v1) < asBigDecimal(v2) }
}
case object GT_Decimal extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigDecimal(v1) > asBigDecimal(v2) }
}
case object LE_Decimal extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigDecimal(v1) <= asBigDecimal(v2) }
}
case object GE_Decimal extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigDecimal(v1) >= asBigDecimal(v2) }
}
case object LT_Integer extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) < asBigInt(v2) }
}
case object GT_Integer extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) > asBigInt(v2) }
}
case object LE_Integer extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) <= asBigInt(v2) }
}
case object GE_Integer extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) >= asBigInt(v2) }
}
case object LT_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) < asBigInt(v2) }
}
case object GT_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) > asBigInt(v2) }
}
case object LE_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) <= asBigInt(v2) }
}
case object GE_NonNegativeInteger extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) >= asBigInt(v2) }
}
case object LT_UnsignedLong extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) < asBigInt(v2) }
}
case object GT_UnsignedLong extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) > asBigInt(v2) }
}
case object LE_UnsignedLong extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) <= asBigInt(v2) }
}
case object GE_UnsignedLong extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asBigInt(v1) >= asBigInt(v2) }
}
case object LT_Long extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) < asLong(v2) }
}
case object GT_Long extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) > asLong(v2) }
}
case object LE_Long extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) <= asLong(v2) }
}
case object GE_Long extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) >= asLong(v2) }
}
case object LT_UnsignedInt extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) < asLong(v2) }
}
case object GT_UnsignedInt extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) > asLong(v2) }
}
case object LE_UnsignedInt extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) <= asLong(v2) }
}
case object GE_UnsignedInt extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asLong(v1) >= asLong(v2) }
}
case object LT_Int extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) < asInt(v2) }
}
case object GT_Int extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) > asInt(v2) }
}
case object LE_Int extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) <= asInt(v2) }
}
case object GE_Int extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) >= asInt(v2) }
}
case object LT_UnsignedShort extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) < asInt(v2) }
}
case object GT_UnsignedShort extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) > asInt(v2) }
}
case object LE_UnsignedShort extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) <= asInt(v2) }
}
case object GE_UnsignedShort extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asInt(v1) >= asInt(v2) }
}
case object LT_Short extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) < asShort(v2) }
}
case object GT_Short extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) > asShort(v2) }
}
case object LE_Short extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) <= asShort(v2) }
}
case object GE_Short extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) >= asShort(v2) }
}
case object LT_UnsignedByte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) < asShort(v2) }
}
case object GT_UnsignedByte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) > asShort(v2) }
}
case object LE_UnsignedByte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) <= asShort(v2) }
}
case object GE_UnsignedByte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asShort(v1) >= asShort(v2) }
}
case object LT_Byte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asByte(v1) < asByte(v2) }
}
case object GT_Byte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asByte(v1) > asByte(v2) }
}
case object LE_Byte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asByte(v1) <= asByte(v2) }
}
case object GE_Byte extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asByte(v1) >= asByte(v2) }
}
case object LT_Float extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asFloat(v1) < asFloat(v2) }
}
case object GT_Float extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asFloat(v1) > asFloat(v2) }
}
case object LE_Float extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asFloat(v1) <= asFloat(v2) }
}
case object GE_Float extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asFloat(v1) >= asFloat(v2) }
}
case object LT_Double extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asDouble(v1) < asDouble(v2) }
}
case object GT_Double extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asDouble(v1) > asDouble(v2) }
}
case object LE_Double extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asDouble(v1) <= asDouble(v2) }
}
case object GE_Double extends NumberCompareOp {
  def operate(v1: Any, v2: Any): Boolean = { asDouble(v1) >= asDouble(v2) }
}
