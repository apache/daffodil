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

trait HexBinaryKind {

  private val conversionErrMsg: String = "%s could not be represented as a long."

  /**
   * http://travisdazell.blogspot.com/2012/11/converting-hex-string-to-byte-array-in.html
   */
  protected def hexStringToByteArray(str: String): Array[Byte] = {
    val len = str.length

    if ((len % 2) != 0)
      throw new NumberFormatException("Failed to evaluate expression: A hexBinary value must contain an even number of characters.")

    val arr = new Array[Byte](len / 2)
    var i = 0
    while (i < len) {
      val upper = Character.digit(str.charAt(i), 16)
      val lower = Character.digit(str.charAt(i + 1), 16)

      if (upper == -1)
        throw new NumberFormatException("Failed to evaluate expression: Invalid hexadecimal digit '%c' at index %d of '%s'".format(str.charAt(i), i, str))
      if (lower == -1)
        throw new NumberFormatException("Failed to evaluate expression: Invalid hexadecimal digit '%c' at index %d of '%s'".format(str.charAt(i + 1), i + 1, str))

      val byte = (upper << 4) + (lower)
      arr(i / 2) = byte.asInstanceOf[Byte]
      i += 2
    }
    return arr
  }

  protected def reduce(numeric: Any): Array[Byte] = {
    val res: Array[Byte] = numeric match {
      case b: Byte => HexBinaryConversions.toByteArray(b)
      case s: Short if (s <= Byte.MaxValue && s >= Byte.MinValue) => reduce(s.toByte)
      case s: Short => HexBinaryConversions.toByteArray(s)
      case i: Int if (i <= Short.MaxValue && i >= Short.MinValue) => reduce(i.toShort)
      case i: Int => HexBinaryConversions.toByteArray(i)
      case l: Long if (l <= Int.MaxValue && l >= Int.MinValue) => reduce(l.toInt)
      case l: Long => HexBinaryConversions.toByteArray(l)
      case bi: BigInt if (bi.isValidLong) => reduce(bi.toLong)
      case bd: BigDecimal if (bd.isValidLong) => reduce(bd.toLong)
      case str: String => reduce(BigInt(str))
      case _ => throw new NumberFormatException("%s could not fit into a long".format(numeric.toString))
    }
    res
  }

  /**
   * http://javarevisited.blogspot.com/2013/03/convert-and-print-byte-array-to-hex-string-java-example-tutorial.html
   */
  protected def bytesToHexString(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append("%02X".format(b & 0xFF))
    }
    return sb.toString
  }
}

case class XSHexBinary(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) with HexBinaryKind {
  val name = "XSHexBinary"

  override def computeValue(a: Any, dstate: DState): Any = {
    // Check for:
    // 1. Even number of characters
    // 2. Valid hex (0-9 A-F)
    val array = a match {
      case s: String => hexStringToByteArray(s)
      case hb: Array[Byte] => hb
      case x => throw new NumberFormatException("%s cannot be cast to dfdl:hexBinary\ndfdl:hexBinary received an unrecognized type! Must be String or HexBinary.".format(x.toString))
    }
    array
  }
}