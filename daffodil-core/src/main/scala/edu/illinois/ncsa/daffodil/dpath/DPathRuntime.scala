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

class CompiledDPath(val ops: RecipeOp*) extends Serializable {

  def this(ops: List[RecipeOp]) = this(ops.toArray: _*)

  override def toString = toXML.toString

  def toXML = <CompiledDPath>{ ops.map { _.toXML } }</CompiledDPath>

  def runExpression(pstate: PState) {
    val dstate = pstate.dstate
    dstate.setCurrentNode(pstate.infoset.asInstanceOf[DINode])
    dstate.setVMap(pstate.variableMap)
    dstate.setPState(pstate)
    dstate.resetValue
    run(dstate)
  }

  /**
   * Used at compilation time to evaluate expressions to determine
   * if they are constant valued.
   *
   * TODO: constnat folding really should operate on sub-expressions of expressions
   * so that part of an expression can be constant, not necessarily the whole thing.
   */
  def runExpressionForConstant(sfl: SchemaFileLocation): Option[Any] = {

    //
    // we use a special dummy dstate here that errors out via throw
    // if the evaluation tries to get a processor state or node.
    //
    val dstate = new DStateForConstantFolding
    val isConstant: Boolean =
      try {
        run(dstate)
        // it ran, so must have produced a constant value
        val v = dstate.currentValue
        Assert.invariant(v != null)
        // the only way dstate can have a value is if setCurrentValue was called
        // so this is redundant. Remove?
        // dstate.setCurrentValue(v) // side effect nulls out the current node
        true
      } catch {
        // 
        // We use IllegalStateException to indicate that the DState was manipulated 
        // in a way that is not consistent with a constant expression. Such as trying to do 
        // anything with the infoset other than saving and restoring current position in the infoset. 
        case e: java.lang.IllegalStateException =>
          false // useful place for breakpoint
        // if the expression is all literals, but illegal such as xs:int("foobar") then 
        // all the pieces are constant, but evaluating will throw NumberFormatException
        // or dfdl:length='{ 5 / 0 }' - contrived yes, but in larger expressions misakes like this
        // are typically typographical errors so it is good to pick them up here.
        case e: java.lang.NumberFormatException => throw new SchemaDefinitionError(Some(sfl), None, e.getMessage)
        case e: java.lang.IndexOutOfBoundsException => false
        case e: java.lang.IllegalArgumentException => false
        case e: SchemaDefinitionDiagnosticBase => throw new SchemaDefinitionError(Some(sfl), None, e.getMessage)
        case e: ProcessingError => throw new SchemaDefinitionError(Some(sfl), None, e.getMessage)
      }
    val res =
      if (isConstant) Some(dstate.currentValue) else None
    res
  }

  def run(dstate: DState) {
    var i = 0
    // Assert.invariant(ops.length > 0)
    while (i < ops.length) {
      val op = ops(i)
      op.run(dstate)
      i = i + 1
    }
  }
}

abstract class RecipeOp
  extends Serializable {
  import AsIntConverters._

  def run(dstate: DState): Unit

  protected def subRecipes: Seq[CompiledDPath] = Nil

  protected def toXML(s: String): scala.xml.Node = toXML(new scala.xml.Text(s))

  protected def toXML(children: scala.xml.Node*): scala.xml.Node = toXML(children.toSeq)

  protected def toXML(children: scala.xml.NodeSeq): scala.xml.Node = {
    val name = Misc.getNameFromClass(this)
    scala.xml.Elem(null, name, scala.xml.Null, scala.xml.TopScope, children.isEmpty, children: _*)
  }

  /**
   * default behavior is inherited and it displays a RecipeOp assuming there are no children
   * to display. Override and make it call one of the above toXML methods if
   * there are children to display.
   */
  def toXML: scala.xml.Node = toXML(scala.xml.NodeSeq.Empty)

}

abstract class RecipeOpWithSubRecipes(recipes: List[CompiledDPath]) extends RecipeOp {

  override def subRecipes: List[CompiledDPath] = recipes

  def this(recipes: CompiledDPath*) = this(recipes.toList)

}

case class VRef(qn: RefQName, context: ThrowsSDE)
  extends RecipeOp {

  val expName = qn.toExpandedName

  override def run(dstate: DState) {
    Assert.invariant(dstate.vmap != null)
    val (res, newVMap) = dstate.vmap.readVariable(expName, context)
    dstate.setVMap(newVMap)
    dstate.setCurrentValue(res)
  }

  override def toXML = toXML("$" + qn.toPrettyString)

}

case class Literal(v: Any) extends RecipeOp {
  override def run(dstate: DState) {
    dstate.setCurrentValue(v)
  }
  override def toXML = toXML(v.toString)
}

case class IF(predRecipe: CompiledDPath, thenPartRecipe: CompiledDPath, elsePartRecipe: CompiledDPath)
  extends RecipeOpWithSubRecipes(predRecipe, thenPartRecipe, elsePartRecipe) {

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    predRecipe.run(dstate)
    val predValue = dstate.currentValue.asInstanceOf[Boolean]
    dstate.setCurrentNode(savedNode)
    if (predValue) {
      thenPartRecipe.run(dstate)
    } else {
      elsePartRecipe.run(dstate)
    }
    // should have a value now. IF-Then-Else is always
    // evaluated for a value.
    Assert.invariant(dstate.currentValue != null)
  }

  override def toXML =
    <if>
      <pred>{ predRecipe.toXML }</pred>
      <then>{ thenPartRecipe.toXML }</then>
      <else>{ elsePartRecipe.toXML }</else>
    </if>
}

trait BinaryOpMixin { self: RecipeOp =>
  def op: String
  def left: CompiledDPath
  def right: CompiledDPath
  override def subRecipes: Seq[CompiledDPath] = Seq(left, right)

  override def toXML: scala.xml.Node = toXML(new scala.xml.Text(op), left.toXML, right.toXML)
}

case class NumberCompareOperator(cop: NumberCompareOp, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp with BinaryOpMixin {

  override def op = Misc.getNameFromClass(cop)

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue
    val result = cop.operate(leftValue, rightValue)
    dstate.setCurrentValue(result)
  }
}

<<<<<<< Upstream, based on origin/serialization-with-dpath4
trait NumberCompareOp {
  /**
   * It is such a pain that there is no scala.math.Number base class above
   * all the numeric types.
   */
  def operate(v1: Any, v2: Any): Boolean
}

abstract class CompareOp
  extends RecipeOp with BinaryOpMixin {

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue
    // Now reset back to the original node to evaluate the right
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue
    val result = compare(op, leftValue, rightValue)
    dstate.setCurrentValue(result)
  }

  def compare(op: String, v1: Any, v2: Any): Boolean
}

case class EqualityCompareOp(op: String, left: CompiledDPath, right: CompiledDPath)
  extends CompareOp {

  def compare(op: String, v1: Any, v2: Any): Boolean = {
    (op, v1, v2) match {
      case ("=", a, b) => a == b
      case ("eq", a, b) => a == b
      case ("!=", a, b) => a != b
      case ("ne", a, b) => a != b
      case _ => Assert.notYetImplemented("operator " + op +
        " on types " + Misc.getNameFromClass(v1) + ", " +
        Misc.getNameFromClass(v2))
    }
  }
}

case class BooleanOp(op: String, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp with BinaryOpMixin {
  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue.asInstanceOf[Boolean] // convertToBoolean(dstate.currentValue, dstate.pstate)
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue.asInstanceOf[Boolean] // convertToBoolean(dstate.currentValue, dstate.pstate)
    val result = operateBoolean(op, leftValue, rightValue)
    dstate.setCurrentValue(result)
  }

  def operateBoolean(op: String, left: Boolean, right: Boolean): Boolean = {
    (op, left, right) match {
      case ("and", v1, v2) => v1 && v2
      case ("or", v1, v2) => v1 || v2
      case _ => Assert.usageError("Not a boolean op")
    }
  }
}
case class NegateOp(recipe: CompiledDPath) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    recipe.run(dstate)
    val value = dstate.currentValue match {
      case i: Int => i * -1
      case l: Long => l * (-1L)
      case d: Double => d * 1.0
      case bi: BigInt => bi * -1
      case bd: BigDecimal => bd * -1
      case _ => Assert.invariantFailed("not a number: " + dstate.currentValue.toString)
    }
    dstate.setCurrentValue(value)
  }

  override def toXML: scala.xml.Node = <Negate>{ recipe.toXML }</Negate>
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

trait SubstringKind {

  protected def substr(sourceString: String, startPos: Int, endPos: Int): String = {
    //
    // Essentially we want to return all characters whose indices >= startingPos 
    // and indices < endPos
    //
    if (startPos >= endPos) return ""
    if (startPos >= sourceString.length) return ""
    if (endPos > sourceString.length) return sourceString.substring(startPos)
    //
    // Note: The documentation of substring says endIndex is exclusive.
    // Yet it seems to behave inclusively here.
    //
    sourceString.substring(startPos, endPos)
  }

  def substring(sourceString: String, startingLoc: Double, length: Double): String = {
    val result =
      if (startingLoc.isNaN() || length.isNaN()) { "" }
      else if (startingLoc.isNegInfinity && length.isNegInfinity) { "" }
      else if (startingLoc.isNegInfinity && length.isPosInfinity) { "" }
      else if (startingLoc.isPosInfinity && length.isNegInfinity) { "" }
      else if (startingLoc.isPosInfinity && length.isPosInfinity) { "" }
      else if (startingLoc.isNegInfinity) {
        val sp = 0
        val ep = length.round.toInt - 1 // adjust to zero-based
        substr(sourceString, sp, ep)
      } else if (length.isPosInfinity) {
        val rounded = startingLoc.round.toInt
        val sp =
          if (rounded <= 0) 0
          else rounded - 1 // adjust to zero-based
        val ep = sourceString.length() // Pos Infinity for length, so result is whole string from startLoc
        substr(sourceString, sp, ep)
      } else {
        val rounded = startingLoc.round.toInt
        val sp =
          if (rounded <= 0) 0
          else rounded - 1 // adjust to zero-based
        val ep = rounded + length.round.toInt - 1 // startLoc + len yields endLoc, adust to zero-based
        substr(sourceString, sp, ep)
      }
    result
  }
}

case class FNSubstring2(recipes: List[CompiledDPath])
  extends FNTwoArgs(recipes)
  with SubstringKind {
  /**
   * The two argument version of the function assumes that $length is infinite
   * and returns the characters in $sourceString whose position $p obeys:
   *
   * fn:round($startingLoc) <= $p < fn:round(INF)
   */
  override def computeValue(arg1: Any, arg2: Any, dstate: DState): Any = {
    val sourceString = arg1.asInstanceOf[String]
    val startingLoc = asDouble(arg2)
    val length = Double.PositiveInfinity

    val res =
      if (startingLoc.isNegInfinity) sourceString
      else substring(sourceString, startingLoc, length)
    res
  }
}

case class FNSubstring3(recipes: List[CompiledDPath])
  extends FNThreeArgs(recipes)
  with SubstringKind {

  /**
   * More specifically, the three argument version of the function returns the
   * characters in $sourceString whose position $p obeys:
   *
   * fn:round($startingLoc) <= $p < fn:round($startingLoc) + fn:round($length)
   *
   * See: http://www.w3.org/TR/xpath-functions/#func-substring
   */
  override def computeValue(arg1: Any, arg2: Any, arg3: Any, dstate: DState): Any = {
    val sourceString = arg1.asInstanceOf[String]
    val startingLoc = asDouble(arg2)
    val length = asDouble(arg3)

    substring(sourceString, startingLoc, length)
  }
}

abstract class FNOneArg(recipe: CompiledDPath, argType: NodeInfo.Kind) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    recipe.run(dstate)
    val arg = dstate.currentValue
    dstate.setCurrentValue(computeValue(arg, dstate))
  }

  override def toXML = toXML(recipe.toXML)

  def computeValue(str: Any, dstate: DState): Any
}

abstract class FNTwoArgs(recipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val List(recipe1, recipe2) = recipes

    val savedNode = dstate.currentNode
    dstate.resetValue()
    recipe1.run(dstate)
    val arg1 = dstate.currentValue

    dstate.setCurrentNode(savedNode)
    recipe2.run(dstate)
    val arg2 = dstate.currentValue

    dstate.setCurrentValue(computeValue(arg1, arg2, dstate))
  }

  def computeValue(arg1: Any, arg2: Any, dstate: DState): Any

  override def toXML = toXML(recipes.map { _.toXML })
}

abstract class FNThreeArgs(recipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val List(recipe1, recipe2, recipe3) = recipes

    val savedNode = dstate.currentNode
    recipe1.run(dstate)
    val arg1 = dstate.currentValue

    dstate.setCurrentNode(savedNode)
    recipe2.run(dstate)
    val arg2 = dstate.currentValue

    dstate.setCurrentNode(savedNode)
    recipe3.run(dstate)
    val arg3 = dstate.currentValue
    dstate.setCurrentValue(computeValue(arg1, arg2, arg3, dstate))
  }

  def computeValue(arg1: Any, arg2: Any, arg3: Any, dstate: DState): Any

  override def toXML = toXML(recipes.map { _.toXML })
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

abstract class FNArgsList(recipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val savedNode = dstate.currentNode

    // FIXME: rewrite to use an OnStack ListBuffer, and
    // a while loop with index vs. the foreach. 
    val args: List[Any] = {
      val list = new ListBuffer[Any]

      recipes.foreach { recipe =>
        recipe.run(dstate)
        list += dstate.currentValue
        dstate.setCurrentNode(savedNode)
      }
      list.toList
    }

    dstate.setCurrentValue(computeValue(args, dstate))
  }

  def computeValue(args: List[Any], dstate: DState): Any
}

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
      case _ => ToDateTime.computeValue(a, dstate)
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
      case _ => ToDate.computeValue(a, dstate)
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
      case _ => ToTime.computeValue(a, dstate)
    }
    result
  }
}

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

=======
>>>>>>> c1133d4 Second round code review changes (minor), plus decomposed the giant DPathRuntime.scala file into useful sub-modules. 
case class NumericOperator(nop: NumericOp, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp with BinaryOpMixin {

  override def op = Misc.getNameFromClass(nop)

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue
    val result = nop.operate(leftValue, rightValue)
    dstate.setCurrentValue(result)
  }
}

trait NumericOp {
  /**
   * It is such a pain that there is no scala.math.Number base class above
   * all the numeric types.
   */
  def operate(v1: Any, v2: Any): Any
}

abstract class Converter extends RecipeOp {

  def typeNames = {
    // This is a total hack. Grab the type names of this converter
    // by spliting the class name at the "To" in the middle.
    val names = Misc.getNameFromClass(this).split("To").toList
    Assert.invariant(names.length == 2)
    val List(fromTypeName, toTypeName) = names
    (fromTypeName, toTypeName)
  }

  override def run(dstate: DState) {
    val arg = dstate.currentValue
    val res =
      try {
        computeValue(arg, dstate)
      } catch {
        case e: NumberFormatException => {
          val (fromTypeName, toTypeName) = typeNames
          val msg =
            if (e.getMessage() != null && e.getMessage() != "") e.getMessage()
            else "No other details are available."
          val err = new NumberFormatException("Cannot convert '%s' from %s type to %s (%s).".format(arg.toString, fromTypeName, toTypeName, msg))
          throw err
        }
      }
    dstate.setCurrentValue(res)
  }

  def computeValue(str: Any, dstate: DState): Any
}

trait ToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = a.toString
}

case object StringToNonEmptyString extends RecipeOp {
  override def run(dstate: DState) {
    val current = dstate.currentValue.asInstanceOf[String]
    if (current.length == 0)
      throw new IllegalArgumentException("String value may not be empty.")
  }
}

