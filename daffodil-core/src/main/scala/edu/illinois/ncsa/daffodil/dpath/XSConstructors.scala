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

case class XSInt(recipe: CompiledDPath) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    recipe.run(dstate)
    val basicValue = dstate.currentValue
    val value = asInt(basicValue)
    dstate.setCurrentValue(value)
  }
}

case class XSString(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = {
    val res: Any = value match {
      case hb: Array[Byte] => HexBinaryToString.computeValue(hb, dstate)
      case _ => value.toString
    }
    res
  }
}

case class XSDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSDateTime"

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case _: DFDLTime => throw new NumberFormatException("Casting from xs:time to xs:dateTime can never succeed.")
      case _ => StringToDateTime.computeValue(a, dstate)
    }
    result
  }
}

case class XSDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSDate"

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case _: DFDLTime => throw new NumberFormatException("Casting from xs:time to xs:date can never succeed.")
      case _ => StringToDate.computeValue(a, dstate)
    }
    result
  }
}

case class XSTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  val name = "XSTime"

  override def computeValue(a: Any, dstate: DState): Any = {
    val result = a match {
      case _: DFDLDate => throw new NumberFormatException("Casting from xs:date to xs:time can never succeed")
      case _ => StringToTime.computeValue(a, dstate)
    }
    result
  }
}