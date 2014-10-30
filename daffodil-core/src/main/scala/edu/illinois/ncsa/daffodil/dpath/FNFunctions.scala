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

case class FNAbs(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = argType match {
    case _: NodeInfo.UnsignedNumeric.Kind => value
    case NodeInfo.Decimal => asBigDecimal(value).abs
    case NodeInfo.Float => asFloat(value).abs
    case NodeInfo.Double => asDouble(value).abs
    case NodeInfo.Long => asLong(value).abs
    case NodeInfo.Int => asInt(value).abs
    case NodeInfo.Integer => asBigInt(value).abs
    case NodeInfo.Short => asShort(value).abs
    case NodeInfo.Byte => asByte(value).abs
    case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function abs.", argType))
  }
}

case class FNStringLength(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[String].length.toLong
}

case class FNLowerCase(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[String].toLowerCase
}

case class FNUpperCase(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: Any, dstate: DState) = str.asInstanceOf[String].toUpperCase
}

case class FNConcat(recipes: List[CompiledDPath]) extends FNArgsList(recipes) {
  override def computeValue(values: List[Any], dstate: DState) = values.mkString
}

// No such function in DFDL v1.0
// But leave this here because we probably will want to add it as a 
// daffodil extension function and then eventually hope it gets into DFDL1.1
//case class FNStringJoin(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
//  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {
//    val values = arg1.asInstanceOf[List[String]]
//    val sep = arg2.asInstanceOf[String]
//    values.mkString(sep)
//  }
//}

case class FNSubstring2(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {
    val sourceString = arg1.asInstanceOf[String]
    val startPos = asDouble(arg2)
    sourceString.substring(startPos.toInt - 1)
  }
}

case class FNSubstring3(recipes: List[CompiledDPath]) extends FNThreeArgs(recipes) {
  override def computeValue(arg1: Any, arg2: Any, arg3: Any, dstate: DState) = {
    val sourceString = arg1.asInstanceOf[String]
    val startPos = asDouble(arg2).toInt - 1 //adjust to zero-based
    val length = asDouble(arg3).toInt
    sourceString.substring(startPos, length)
  }
}

case class FNDateTime(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
  val name = "FNDateTime"

  private def calendarToDFDLDateTime(calendar: Calendar, formatString: String, dstate: DState, fncName: String, toType: String): DFDLCalendar = {
    try {
      val cal = new DFDLDateTime(calendar)
      return cal
    } catch {
      case ex: java.lang.IllegalArgumentException =>
        dstate.pstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, calendar.toString, toType, ex.getMessage())
    }
  }
  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {

    val dateCal = arg1.asInstanceOf[DFDLCalendar].getCalendar
    val timeCal = arg2.asInstanceOf[DFDLCalendar].getCalendar

    val year = dateCal.get(Calendar.YEAR)
    val day = dateCal.get(Calendar.DAY_OF_MONTH)
    val month = dateCal.get(Calendar.MONTH)
    val dateTZ = dateCal.getTimeZone()

    val timeTZ = timeCal.getTimeZone()

    val newCal: Calendar = timeCal.clone().asInstanceOf[Calendar]
    newCal.set(Calendar.YEAR, year)
    newCal.set(Calendar.DAY_OF_MONTH, day)
    newCal.set(Calendar.MONTH, month)

    /**
     * http://www.w3.org/TR/xpath-functions/#func-dateTime
     *
     * The timezone of the result is computed as follows:
     *
     * If neither argument has a timezone, the result has no timezone.
     * If exactly one of the arguments has a timezone, or if both arguments
     * 	have the same timezone, the result has this timezone.
     * If the two arguments have different timezones, an error
     * 	is raised:[err:FORG0008]
     */

    (dateTZ, timeTZ) match {
      case (null, null) => newCal.setTimeZone(null)
      case (tz, null) => newCal.setTimeZone(tz)
      case (null, tz) => newCal.setTimeZone(tz)
      case (tz0, tz1) if tz0 != tz1 => dstate.pstate.SDE("The two arguments to fn:dateTime have inconsistent timezones")
      case (tz0, tz1) => newCal.setTimeZone(tz0)
    }

    val finalCal = calendarToDFDLDateTime(newCal, "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx", dstate, name, "DateTime")
    finalCal
  }
}

case class FNRoundHalfToEven(recipeNum: CompiledDPath, recipePrecision: CompiledDPath)
  extends RecipeOpWithSubRecipes(recipeNum, recipePrecision) {

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    recipeNum.run(dstate)
    val unrounded = dstate.currentValue
    dstate.setCurrentNode(savedNode)
    recipePrecision.run(dstate)
    val precision = dstate.intValue
    val bd = unrounded match {
      case s: String => BigDecimal(s) // TODO: Remove eventually. Holdover from JDOM where everything is a string.
      case l: Long => BigDecimal.valueOf(l)
      case f: Float => BigDecimal.valueOf(f)
      case d: Double => BigDecimal.valueOf(d)
      case bd: BigDecimal => bd
      case _ => Assert.invariantFailed("not a number")
    }
    val value = {
      val rounded = bd.setScale(precision, BigDecimal.RoundingMode.HALF_EVEN)
      rounded
    }
    dstate.setCurrentValue(value)
  }
}

case class FNNot(recipe: CompiledDPath, argType: NodeInfo.Kind = null) extends FNOneArg(recipe, NodeInfo.Boolean) {
  override def computeValue(value: Any, dstate: DState) = !(value.asInstanceOf[Boolean])
}

case class FNEndsWith(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
  override def computeValue(arg1: Any, arg2: Any, dstate: DState) = {
    val arg1s = arg1.asInstanceOf[String]
    val arg2s = arg2.asInstanceOf[String]
    val res = arg1s.endsWith(arg2s)
    res
  }
}

case class FNNilled(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, NodeInfo.Nillable) {
  override def computeValue(value: Any, dstate: DState) = value.asInstanceOf[DIElement].isNilled
}

case class FNExists(recipe: CompiledDPath, argType: NodeInfo.Kind) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    dstate.fnExists() // hook so we can insist this is non-constant at compile time.
    val exists =
      try {
        recipe.run(dstate)
        true
      } catch {
        // catch exceptions indicating a node (or value) doesn't exist.
        //
        // if you reach into an array location that doesn't exist
        case e: java.lang.IndexOutOfBoundsException => false
        // if you reach into a child element slot that isn't filled
        case e: InfosetNoSuchChildElementException => false
        // if something else goes wrong while evaluating the
        // expression (fn:exist can be called on expressions that are
        // not necessarily nodes.They can be simple value expressions)
        case e: java.lang.IllegalStateException => false
        case e: java.lang.NumberFormatException => false
        case e: java.lang.IllegalArgumentException => false
        case e: java.lang.ArithmeticException => false
        case e: ProcessingError => false
      }
    dstate.setCurrentValue(exists)
  }

  override def toXML = toXML(recipe.toXML)

}

case class FNCeiling(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = argType match {

    case NodeInfo.Decimal => {
      val bd = asBigDecimal(value).setScale(0, RoundingMode.CEILING)
      bd.round(bd.mc)
    }
    case NodeInfo.Float => asFloat(value).ceil
    case NodeInfo.Double => asDouble(value).ceil
    case _: NodeInfo.Numeric.Kind => value
    case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function ceiling.", argType))
  }
}

case class FNFloor(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = argType match {

    case NodeInfo.Decimal => {
      val bd = asBigDecimal(value).setScale(0, RoundingMode.FLOOR)
      bd.round(bd.mc)
    }
    case NodeInfo.Float => asFloat(value).floor
    case NodeInfo.Double => asDouble(value).floor
    case _: NodeInfo.Numeric.Kind => value
    case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function floor.", argType))
  }
}

case class FNRound(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: Any, dstate: DState) = argType match {

    case NodeInfo.Decimal => {
      val bd = asBigDecimal(value)
      bd.round(bd.mc)
    }
    case NodeInfo.Float => asFloat(value).round
    case NodeInfo.Double => asDouble(value).round
    case _: NodeInfo.Numeric.Kind => value
    case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function round.", argType))
  }
}

trait FNFromDateTimeKind {
  def fieldName: String
  def field: Int
}

abstract class FNFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case dt: DFDLDateTime => dt.getField(field)
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-dateTime only accepts xs:dateTime.")
    }
  }
}

abstract class FNFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case d: DFDLDate => d.getField(field)
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-date only accepts xs:date.")
    }
  }
}

abstract class FNFromTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: Any, dstate: DState) = {
    a match {
      case t: DFDLTime => t.getField(field)
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-time only accepts xs:time.")
    }
  }
}

case class FNYearFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "year"
  val field = Calendar.YEAR
}
case class FNMonthFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "month"
  val field = Calendar.MONTH
  override def computeValue(a: Any, dstate: DState) = super.computeValue(a, dstate).asInstanceOf[Int] + 1 // JAN 0
}
case class FNDayFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "day"
  val field = Calendar.DAY_OF_MONTH
}
case class FNHoursFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "hours"
  val field = Calendar.HOUR_OF_DAY
}
case class FNMinutesFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "minutes"
  val field = Calendar.MINUTE
}
case class FNSecondsFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "seconds"
  val field = Calendar.SECOND
}

case class FNYearFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDate(recipe, argType) {
  val fieldName = "year"
  val field = Calendar.YEAR
}
case class FNMonthFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDate(recipe, argType) {
  val fieldName = "month"
  val field = Calendar.MONTH
  override def computeValue(a: Any, dstate: DState) = super.computeValue(a, dstate).asInstanceOf[Int] + 1 // JAN 0
}
case class FNDayFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDate(recipe, argType) {
  val fieldName = "day"
  val field = Calendar.DAY_OF_MONTH
}
case class FNHoursFromTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromTime(recipe, argType) {
  val fieldName = "hours"
  val field = Calendar.HOUR_OF_DAY
}
case class FNMinutesFromTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromTime(recipe, argType) {
  val fieldName = "minutes"
  val field = Calendar.MINUTE
}
case class FNSecondsFromTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromTime(recipe, argType) {
  val fieldName = "seconds"
  val field = Calendar.SECOND
}