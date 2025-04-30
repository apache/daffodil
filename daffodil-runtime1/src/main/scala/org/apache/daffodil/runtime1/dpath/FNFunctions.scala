/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.runtime1.dpath

import java.lang.{ Byte => JByte }
import java.lang.{ Double => JDouble }
import java.lang.{ Float => JFloat }
import java.lang.{ Integer => JInt }
import java.lang.{ Long => JLong }
import java.lang.{ Number => JNumber }
import java.lang.{ Short => JShort }
import java.math.RoundingMode
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.lib.calendar.DFDLCalendar
import org.apache.daffodil.lib.calendar.DFDLDate
import org.apache.daffodil.lib.calendar.DFDLDateTime
import org.apache.daffodil.lib.calendar.DFDLTime
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.exceptions.UnsuppressableException
import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.Numbers.asBigDecimal
import org.apache.daffodil.lib.util.Numbers.asBigInt
import org.apache.daffodil.lib.util.Numbers.asBoolean
import org.apache.daffodil.lib.util.Numbers.asByte
import org.apache.daffodil.lib.util.Numbers.asDouble
import org.apache.daffodil.lib.util.Numbers.asFloat
import org.apache.daffodil.lib.util.Numbers.asInt
import org.apache.daffodil.lib.util.Numbers.asLong
import org.apache.daffodil.lib.util.Numbers.asShort
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBigDecimal
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBigInt
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBool
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueCalendar
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueLong
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueNumber
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueString
import org.apache.daffodil.runtime1.infoset.InfosetArrayIndexOutOfBoundsException
import org.apache.daffodil.runtime1.infoset.InfosetNoSuchChildElementException
import org.apache.daffodil.runtime1.infoset.InfosetNodeNotFinalException
import org.apache.daffodil.runtime1.infoset.InfosetWrongNodeType
import org.apache.daffodil.runtime1.infoset.RetryableException
import org.apache.daffodil.runtime1.processors.ProcessingError
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

import com.ibm.icu.util.Calendar
import com.ibm.icu.util.TimeZone

case class FNAbs(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(v: DataValuePrimitive, dstate: DState) = {
    val value = asBigDecimal(v.getAnyRef).abs
    argType match {
      case _: NodeInfo.UnsignedNumeric.Kind => value
      case NodeInfo.Decimal => value
      case NodeInfo.Float => asFloat(v.getAnyRef).floatValue().abs
      case NodeInfo.Double => asDouble(v.getAnyRef).doubleValue().abs
      case NodeInfo.Long => asLong(value)
      case NodeInfo.Int => asInt(value)
      case NodeInfo.Integer => asBigInt(value)
      case NodeInfo.Short => asShort(value)
      case NodeInfo.Byte => asByte(value)
      case _ =>
        Assert.invariantFailed(
          String.format("Type %s is not a valid type for function abs.", argType)
        )
    }
  }
}

case class FNStringLength(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(str: DataValuePrimitive, dstate: DState): DataValueLong =
    str.getString.length.toLong
}

case class FNLowerCase(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(str: DataValuePrimitive, dstate: DState): DataValueString =
    str.getString.toLowerCase
}

case class FNUpperCase(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(str: DataValuePrimitive, dstate: DState): DataValueString =
    str.getString.toUpperCase
}

case class FNConcat(recipes: List[CompiledDPath]) extends FNArgsList(recipes) {
  override def computeValue(
    values: List[DataValuePrimitive],
    dstate: DState
  ): DataValueString = {
    val ans = new StringBuilder()
    for (i <- 0 to values.length - 1) {
      ans.append(values(i).getAnyRef.toString())
    }
    ans.toString()
  }
}

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

  protected def substr(sourceString: String, startPos: Int): String = {
    //
    // Essentially we want to return all characters whose indices >= startingPos
    //
    if (startPos >= sourceString.length) return ""
    //
    // Note: The documentation of substring says endIndex is exclusive.
    // Yet it seems to behave inclusively here.
    //
    sourceString.substring(startPos)
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
        val ep =
          sourceString
            .length() // Pos Infinity for length, so result is whole string from startLoc
        substr(sourceString, sp, ep)
      } else {
        val rounded = startingLoc.round.toInt
        val sp =
          if (rounded <= 0) 0
          else rounded - 1 // adjust to zero-based
        val ep =
          rounded + length.round.toInt - 1 // startLoc + len yields endLoc, adust to zero-based
        substr(sourceString, sp, ep)
      }
    result
  }

  def substring(sourceString: String, startingLoc: Double): String = {
    val result =
      if (startingLoc.isNaN()) { "" }
      else if (startingLoc.isPosInfinity) { "" }
      else if (startingLoc.isNegInfinity) {
        val sp = 0
        substr(sourceString, sp)
      } else {
        val rounded = startingLoc.round.toInt
        val sp =
          if (rounded <= 0) 0
          else rounded - 1 // adjust to zero-based
        substr(sourceString, sp)
      }
    result
  }
}

case class FNSubstring2(recipes: List[CompiledDPath])
  extends FNTwoArgs(recipes)
  with SubstringKind {

  /**
   * The two argument version of the function assumes that \$length is infinite
   * and returns the characters in \$sourceString whose position \$p obeys:
   *
   * fn:round(\$startingLoc) <= \$p < fn:round(INF)
   */
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValueString = {
    val sourceString = arg1.getString
    val startingLoc = asDouble(arg2.getAnyRef).doubleValue()
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
   * characters in \$sourceString whose position \$p obeys:
   *
   * fn:round(\$startingLoc) <= \$p < fn:round(\$startingLoc) + fn:round(\$length)
   *
   * See: http://www.w3.org/TR/xpath-functions/#func-substring
   */
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    arg3: DataValuePrimitive,
    dstate: DState
  ): DataValueString = {
    val sourceString = arg1.getString
    val startingLoc = asDouble(arg2.getAnyRef)
    val length = asDouble(arg3.getAnyRef)

    substring(sourceString, startingLoc, length)
  }
}

case class FNSubstringBefore(recipes: List[CompiledDPath])
  extends FNTwoArgs(recipes)
  with SubstringKind {

  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValueString = {
    val sourceString: String = arg1.getString
    val searchString: String = arg2.getString

    val res =
      if (searchString == null || searchString == "") ""
      else {
        val index = sourceString.indexOf(searchString)
        if (index < 0) ""
        else substr(sourceString, 0, index)
      }
    res
  }
}

case class FNSubstringAfter(recipes: List[CompiledDPath])
  extends FNTwoArgs(recipes)
  with SubstringKind {

  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValueString = {
    val sourceString: String = arg1.getString
    val searchString: String = arg2.getString

    val res =
      if (searchString == null || searchString == "") sourceString
      else {
        val index = sourceString.indexOf(searchString)
        if (index < 0) ""
        else substr(sourceString, index + searchString.length())
      }
    res
  }
}

case class FNDateTime(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
  val name = "FNDateTime"

  private def calendarToDFDLDateTime(
    calendar: Calendar,
    hasTZ: Boolean,
    dstate: DState,
    fncName: String,
    toType: String
  ): DFDLCalendar = {
    try {
      val cal = DFDLDateTime(calendar, hasTZ)
      return cal
    } catch {
      case ex: java.lang.IllegalArgumentException =>
        dstate.SDE(
          "Conversion Error: %s failed to convert \"%s\" to %s. Due to %s",
          fncName,
          calendar.toString,
          toType,
          ex.getMessage()
        )
    }
  }
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValueCalendar = {
    val dateCalendar = arg1.getCalendar
    val timeCalendar = arg2.getCalendar

    val dateCal = dateCalendar.calendar
    val timeCal = timeCalendar.calendar

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
     *   have the same timezone, the result has this timezone.
     * If the two arguments have different timezones, an error
     *   is raised:[err:FORG0008]
     */
    var hasTZ: Boolean = true

    (dateCalendar.hasTimeZone, timeCalendar.hasTimeZone) match {
      case (false, false) => {
        // No concept of 'no time zone' so we will use TimeZone.UNKNOWN_ZONE instead
        // this will behave just like GMT/UTC
        //
        newCal.setTimeZone(TimeZone.UNKNOWN_ZONE)
        hasTZ = false
      }
      case (true, false) => newCal.setTimeZone(dateTZ)
      case (false, true) => newCal.setTimeZone(timeTZ)
      case (true, true) if dateTZ != timeTZ =>
        dstate.SDE("The two arguments to fn:dateTime have inconsistent timezones")
      case (true, true) => newCal.setTimeZone(dateTZ)
    }

    val finalCal = calendarToDFDLDateTime(newCal, hasTZ, dstate, name, "DateTime")
    finalCal
  }
}

case class FNRoundHalfToEven(recipeNum: CompiledDPath, recipePrecision: CompiledDPath)
  extends RecipeOpWithSubRecipes(recipeNum, recipePrecision) {

  override def run(dstate: DState): Unit = {
    val savedNode = dstate.currentNode
    recipeNum.run(dstate)
    val unrounded = dstate.currentValue
    dstate.setCurrentNode(savedNode)
    recipePrecision.run(dstate)
    val precision = dstate.intValue
    val bd = unrounded.getAnyRef match {
      case s: String =>
        new JBigDecimal(
          s
        ) // TODO: Remove eventually. Holdover from JDOM where everything is a string.
      case l: JLong => JBigDecimal.ONE
      case f: JFloat => JBigDecimal.valueOf(f.toDouble)
      case d: JDouble => JBigDecimal.valueOf(d)
      case bd: JBigDecimal => bd
      case _ => Assert.invariantFailed("not a number")
    }
    val value = {
      val rounded = bd.setScale(precision, RoundingMode.HALF_EVEN)
      rounded
    }
    dstate.setCurrentValue(value)
  }
}

trait FNRoundHalfToEvenKind {

  /**
   * For arguments of type xs:float and xs:double, if the argument is NaN,
   * positive or negative zero, or positive or negative infinity, then the
   * result is the same as the argument.
   *
   * In all other cases, the argument is cast to xs:decimal, the function
   * is applied to this xs:decimal value, and the resulting xs:decimal is
   * cast back to xs:float or xs:double as appropriate to form the function
   * result.
   *
   * If the resulting xs:decimal value is zero, then positive or negative
   * zero is returned according to the sign of the original argument.
   *
   * NOTE: Java does not appear to make a distinction between pos or neg zero.
   */
  def compute(value: DataValuePrimitive, precision: Int): DataValuePrimitive = {
    // We should only receive 'Numeric' types here which are either
    // xs:double, xs:float, xs:decimal, xs:integer or a sub-type thereof.
    //
    // The conversions code should be enforcing this, if not we
    // have a serious issue.
    //
    def roundIt = {
      val unroundedValue: DataValueBigDecimal = unrounded(value)
      val roundedValue = toBaseNumericType(round(unroundedValue, precision), value)
      roundedValue
    }

    val result: DataValuePrimitive = value.getAnyRef match {
      case f: JFloat
          if (f.isNaN() || f == 0 || f.floatValue().isPosInfinity || f
            .floatValue()
            .isNegInfinity) =>
        f
      case d: JDouble
          if (d.isNaN() || d == 0 || d.doubleValue().isPosInfinity || d
            .doubleValue()
            .isNegInfinity) =>
        d
      //
      // This used to be a single big case like:
      // case _:Float | _: Double | ... | _: Short => ....
      // but unfortunately, the scala compiler spit out a warning (about analyzing cases)
      // so this is equivalent, but avoids the warning.
      //
      case _: JFloat => roundIt
      case _: JDouble => roundIt
      case _: JBigDecimal => roundIt
      case _: JBigInt => roundIt
      case _: JLong => roundIt
      case _: JInt => roundIt
      case _: JByte => roundIt
      case _: JShort => roundIt
      case _ =>
        Assert.invariantFailed(
          "Unrecognized numeric type. Must be xs:float, xs:double, xs:decimal, xs:integer or a type derived from these."
        )
    }
    result
  }

  private def unrounded(value: DataValuePrimitive): DataValueBigDecimal = {
    val result = value.getAnyRef match {
      //
      // Not converting Float to string first causes precision issues
      // that round-half-to-even doesn't resolve correctly.  BigDecimal.valueOf(3.455) turns into 3.454999.
      // HALF_EVEN rounding mode would round this to 3.45 rather than the desired 3.46.
      //
      // The solution is to do BigDecimal(float.toString).  This has been corrected in asBigDecimal.
      //
      // NOTE:
      // Any change in how asBigDecimal handles Float
      // will affect the correctness of this rounding operation.
      //
      case _: JFloat | _: JDouble | _: JBigDecimal => asBigDecimal(value.getAnyRef)
      case _: JBigInt | _: JLong | _: JInt | _: JByte | _: JShort =>
        asBigDecimal(value.getAnyRef)
      case _ =>
        Assert.usageError(
          "Received a type other than xs:decimal, xs:double, xs:float, xs:integer or any of its sub-types."
        )
    }
    result
  }

  private def round(value: DataValueBigDecimal, precision: Int): DataValueBigDecimal = {
    val rounded: JBigDecimal = value.getBigDecimal.setScale(precision, RoundingMode.HALF_EVEN)
    rounded
  }

  /**
   * If the type of \$arg is one of the four numeric types xs:float, xs:double,
   * xs:decimal or xs:integer the type of the result is the same as the type of \$arg.
   *
   * If the type of \$arg is a type derived from one of the numeric types, the
   * result is an instance of the base numeric type.
   */
  private def toBaseNumericType(
    value: DataValueBigDecimal,
    origValue: DataValuePrimitive
  ): DataValuePrimitive = {
    val result: DataValuePrimitive = origValue.getAnyRef match {
      case _: JFloat => value.getBigDecimal.floatValue() // xs:float
      case _: JDouble => value.getBigDecimal.doubleValue() // xs:double
      case _: JBigDecimal => value
      case _: JBigInt => value.getBigDecimal.toBigInteger()
      case _: JLong | _: JInt | _: JByte | _: JShort =>
        value.getBigDecimal.toBigInteger() // xs:integer
      case _ =>
        Assert.usageError(
          "Received a type other than xs:decimal, xs:double, xs:float, xs:integer or any of its sub-types."
        )
    }
    result
  }

}

/**
 * The value returned is the nearest (that is, numerically closest) value
 * to \$arg that is a multiple of ten to the power of minus \$precision.
 *
 * If two such values are equally near (e.g. if the fractional part
 * in \$arg is exactly .500...), the function returns the one whose least
 * significant digit is even.
 *
 * This particular function expects a single argument which is a Numeric.
 * \$precision is assumed 0.
 */
case class FNRoundHalfToEven1(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNRoundHalfToEvenKind {

  override def computeValue(value: DataValuePrimitive, dstate: DState) = {
    val roundedValue = compute(value, 0)
    roundedValue
  }
}

/**
 * The value returned is the nearest (that is, numerically closest) value
 * to \$arg that is a multiple of ten to the power of minus \$precision.
 *
 * If two such values are equally near (e.g. if the fractional part
 * in \$arg is exactly .500...), the function returns the one whose least
 * significant digit is even.
 *
 * This particular function expects a two arguments: \$arg and \$precision.
 */
case class FNRoundHalfToEven2(recipes: List[CompiledDPath])
  extends FNTwoArgs(recipes)
  with FNRoundHalfToEvenKind {

  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ) = {
    val precision = asInt(arg2.getAnyRef)
    val roundedValue = compute(arg1, precision)
    roundedValue
  }
}

case class FNNot(recipe: CompiledDPath, argType: NodeInfo.Kind = null)
  extends FNOneArg(recipe, NodeInfo.Boolean) {
  override def computeValue(value: DataValuePrimitive, dstate: DState): DataValueBool = {
    val bool = asBoolean(FNToBoolean.computeValue(value, dstate).getAnyRef)
    !bool
  }
}

case class FNNilled(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends RecipeOpWithSubRecipes(recipe) {

  override def run(dstate: DState): Unit = {
    // FNNilled wants to use FNOneArg. However, the run() method in FNOneArg
    // attempts to get currentValue of the argument, and then it calls
    // computeValue() passing in the value. However, with FNNilled, we cannot
    // call currentValue because nilled elements do not have a value, causing
    // an exception to be thrown. Instead, we override the run() method to run
    // the recipe and then determine if the resulting node is nilled, avoiding
    // ever trying to get the currentValue.
    recipe.run(dstate)
    val nilled = dstate.currentNode.asInstanceOf[DIElement].isNilled
    dstate.setCurrentValue(nilled)
  }
}

trait ExistsKind {

  def exists(recipe: CompiledDPath, dstate: DState): Boolean = {
    dstate.fnExists() // hook so we can insist this is non-constant at compile time.

    val res =
      try {
        recipe.run(dstate)
        true
      } catch {
        case ue: UnsuppressableException => throw ue
        case th: Throwable => handleThrow(th, dstate)
      }
    res
  }

  private def doesntExist(dstate: DState) = {
    false
  }

  /**
   * In blocking mode, fn:exist can only answer false if the
   * element/array is closed meaning no more children can be added.
   *
   * If a child is present, then it can answer true regardless
   * of closed/not, but if child is absent, we might still be
   * appending to the array, or adding children to the complex
   * element.
   *
   */
  private def ifClosedDoesntExist(dstate: DState, th: Throwable): Boolean = {
    Assert.invariant(dstate.currentNode ne null)
    val res = dstate.mode match {
      case UnparserNonBlocking => {
        // we are evaluating an expression while unparsing in non blocking mode.
        // This means this throwable must have come from an evaluatable using
        // only backwards references. Backwards references are known to be
        // final (even if isFinal isn't set yet), so because an exception was
        // thrown, this element must not exist.
        false
      }
      case UnparserBlocking => {
        // we are evaluating an expression while unparsing in blocking mode.
        // This means we don't know if this exception means the element does
        // not exist or just hasn't been created in the infoset yet. If the
        // current node is final, then we are sure it doesn't exist. Otherwise
        // we throw the exception which triggers suspension logic to retry
        // later
        dstate.currentNode match {
          case c: DINode if (c.isFinal) => false
          case _ => throw th
        }
      }
      case _: ParserMode => {
        false
      }
    }
    res
  }

  private def handleThrow(th: Throwable, dstate: DState) = {
    //
    // Some errors we never suppress. They're always thrown
    // to enable blocking, or detect errors where something would
    // have blocked but didn't.
    //
    dstate.mode match {
      case UnparserNonBlocking => // ok, fall through
      case UnparserBlocking => {
        th match {
          case u: UnsuppressableException => throw u
          case r: RetryableException => // ok fall through
          case _ => throw th
        }
      }
      case _: ParserMode => // ok
    }
    //
    // If we fall through to here, then we might suppress the error
    // and just report that the node doesn't exist.
    //
    th match {
      case e: InfosetNodeNotFinalException => {
        Assert.invariant(dstate.mode eq UnparserBlocking)
        throw e
      }
      //
      // catch exceptions indicating a node (or value) doesn't exist.
      // if you reach into a child element slot that isn't filled
      case e: InfosetNoSuchChildElementException => ifClosedDoesntExist(dstate, th)
      case e: InfosetArrayIndexOutOfBoundsException => ifClosedDoesntExist(dstate, th)

      // Note that in debugger, expressions are sometimes evaluated in contexts
      // where they don't make sense.
      // But we must rethrow this or it will get suppressed when it is a real error.
      case e: InfosetWrongNodeType => throw th
      //
      // other processing errors indicate it doesn't exist
      //
      case e: ProcessingError => doesntExist(dstate)
      case _ => throw th
    }
  }

}

case class FNExists(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends RecipeOpWithSubRecipes(recipe)
  with ExistsKind {
  override def run(dstate: DState): Unit = {
    val res = exists(recipe, dstate)
    dstate.setCurrentValue(res)
  }

  override def toXML = toXMLVarargs(recipe.toXML)

}

case class FNEmpty(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends RecipeOpWithSubRecipes(recipe)
  with ExistsKind {
  override def run(dstate: DState): Unit = {
    val res = exists(recipe, dstate)
    dstate.setCurrentValue(!res)
  }

  override def toXML = toXMLVarargs(recipe.toXML)

}

/**
 * Returns the local part of the name of \$arg as an xs:string
 * that will either be the zero-length string or will have the
 * lexical form of an xs:NCName
 *
 * If the argument is omitted, it defaults to the context item (.).
 * The behavior of the function if the argument is omitted is
 * exactly the same as if the context item had been passed as
 * the argument.
 *
 * This function is called when 0 arguments are provided.  We
 * treat this as if the argument passed was "." to denote self.
 */
case class FNLocalName0() extends RecipeOp {
  override def run(dstate: DState): Unit = {
    // Same as using "." to denote self.
    val localName = dstate.currentElement.name

    if (localName.contains(":"))
      throw new IllegalArgumentException(
        "fn:local-name failed. " + localName + " is not a valid NCName as it contains ':'."
      )

    dstate.setCurrentValue(localName)
  }
}

/**
 * Returns the local part of the name of \$arg as an xs:string
 * that will either be the zero-length string or will have the
 * lexical form of an xs:NCName
 */
case class FNLocalName1(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(value: DataValuePrimitive, dstate: DState) = {
    Assert.usageError(
      "not to be called. DPath compiler should be answering this without runtime calls."
    )
  }

  override def run(dstate: DState): Unit = {
    // Save off original node, which is the original
    // element/node that calls fn:local-name
    val savedNode = dstate.currentNode

    // Execute the recipe/expression which should
    // return a node/element whose local-name we want
    recipe.run(dstate)

    val localName = dstate.currentElement.name
    dstate.setCurrentNode(savedNode)

    if (localName.contains(":"))
      throw new IllegalArgumentException(
        "fn:local-name failed. " + localName + " is not a valid NCName as it contains ':'."
      )

    dstate.setCurrentValue(localName)
  }
}

/**
 * Returns the namespace URI of the expanded-QName of the context
 * node as an xs:string value.
 *
 * If the context node is in no namespace, then the function returns
 * the zero-length xs:string value instead.
 *
 * The following error may be raised:
 * - If the context node is not an element, type error [err:XPTY004]
 *
 * This function is called when 0 arguments are provided.  We
 * treat this as if the argument passed was "." to denote self.
 */
case class FNNamespaceUri0() extends RecipeOp {
  override def run(dstate: DState): Unit = {
    // Insist this is non-constant at compile time (to avoid a NPE)
    if (dstate.isCompile)
      throw new IllegalStateException()

    // Check that the context node is really present
    if (dstate.currentNode eq null)
      dstate.SDE("Context node for fn:namespace-uri is not an element")

    // Same as using "." to denote self.
    val value = dstate.currentNode.namedQName.namespace.optURI match {
      case Nope => ""
      case uri => uri.get.toString
    }
    dstate.setCurrentValue(value)
  }
}

/**
 * Returns the namespace URI of the expanded-QName of \$arg as an
 * xs:string value.
 *
 * If the element identified by \$arg is in no namespace, then the
 * function returns the zero-length xs:string value instead.
 *
 * The following error may be raised:
 * - If \$arg is not an element, type error [err:XPTY004]
 */
case class FNNamespaceUri1(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(value: DataValuePrimitive, dstate: DState) = {
    Assert.usageError(
      "not to be called. DPath compiler should be answering this without runtime calls."
    )
  }

  override def run(dstate: DState): Unit = {
    // Insist this is non-constant at compile time (to avoid a NPE)
    if (dstate.isCompile)
      throw new IllegalStateException()

    // Save original node that calls fn:namespace-uri and execute the
    // recipe/expression which should return a node/element whose
    // namespace-uri we want
    val savedNode = dstate.currentNode
    recipe.run(dstate)
    val resultNode = dstate.currentNode
    dstate.setCurrentNode(savedNode)

    // Check that the expression returned an element, not a value
    if (resultNode eq null)
      dstate.SDE("Argument %s for fn:namespace-uri is not an element", recipe)

    // Find and return the namespace-uri we want
    val value = resultNode.namedQName.namespace.optURI match {
      case Nope => ""
      case uri => uri.get.toString
    }
    dstate.setCurrentValue(value)
  }
}

case class FNCeiling(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(value: DataValuePrimitive, dstate: DState) = argType match {

    case NodeInfo.Decimal => {
      val bd = asBigDecimal(value.getAnyRef)
      bd.setScale(0, RoundingMode.CEILING)
    }
    case NodeInfo.Float => asFloat(value.getAnyRef).floatValue().ceil
    case NodeInfo.Double => asDouble(value.getAnyRef).doubleValue().ceil
    case _: NodeInfo.Numeric.Kind => value
    case _ =>
      Assert.invariantFailed(
        String.format("Type %s is not a valid type for function ceiling.", argType)
      )
  }
}

case class FNFloor(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(value: DataValuePrimitive, dstate: DState) = argType match {

    case NodeInfo.Decimal => {
      val bd = asBigDecimal(value.getAnyRef)
      bd.setScale(0, RoundingMode.FLOOR)
    }
    case NodeInfo.Float => asFloat(value.getAnyRef).floatValue().floor
    case NodeInfo.Double => asDouble(value.getAnyRef).doubleValue().floor
    case _: NodeInfo.Numeric.Kind => value
    case _ =>
      Assert.invariantFailed(
        String.format("Type %s is not a valid type for function floor.", argType)
      )
  }
}

case class FNRound(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {
  override def computeValue(value: DataValuePrimitive, dstate: DState) = {
    val res: DataValuePrimitive = argType match {
      case NodeInfo.Decimal => {
        val bd = asBigDecimal(value.getAnyRef)
        //
        // A MathContext object whose settings have
        // the values required for unlimited precision arithmetic.
        val mc = java.math.MathContext.UNLIMITED
        bd.setScale(0, RoundingMode.HALF_UP)
      }
      case NodeInfo.Float => {
        val f = asFloat(value.getAnyRef).floatValue()
        if (f.isPosInfinity || f.isNegInfinity) f
        else if (f.isNaN()) throw new NumberFormatException("fn:round received NaN")
        else f.round.toFloat
      }
      case NodeInfo.Double => {
        val d = asDouble(value.getAnyRef).doubleValue()
        if (d.isPosInfinity || d.isNegInfinity) d
        else if (d.isNaN()) throw new NumberFormatException("fn:round received NaN")
        else d.round.toDouble
      }
      case _: NodeInfo.Numeric.Kind => value
      case _ =>
        Assert.invariantFailed(
          String.format("Type %s is not a valid type for function round.", argType)
        )
    }
    res
  }
}

trait FNFromDateTimeKind {
  def fieldName: String
  def field: Int
}

abstract class FNFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueNumber = {
    a.getAnyRef match {
      case dt: DFDLDateTime => JBigInt.valueOf(dt.calendar.get(field))
      case _ =>
        throw new NumberFormatException(
          "fn:" + fieldName + "-from-dateTime only accepts xs:dateTime."
        )
    }
  }
}

abstract class FNFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueNumber = {
    a.getAnyRef match {
      case d: DFDLDate => JBigInt.valueOf(d.calendar.get(field))
      case _ =>
        throw new NumberFormatException("fn:" + fieldName + "-from-date only accepts xs:date.")
    }
  }
}

abstract class FNFromTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueNumber = {
    a.getAnyRef match {
      case t: DFDLTime => JBigInt.valueOf(t.calendar.get(field))
      case _ =>
        throw new NumberFormatException("fn:" + fieldName + "-from-time only accepts xs:time.")
    }
  }
}

case class FNYearFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "year"
  val field = Calendar.EXTENDED_YEAR
}
case class FNMonthFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDateTime(recipe, argType) {
  val fieldName = "month"
  val field = Calendar.MONTH
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = {
    super.computeValue(a, dstate).getBigInt.add(JBigInt.ONE) // JAN 0
  }
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

  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigDecimal = {
    //
    // It matters that val res is declared to be type JNumber.
    //
    // This preserves polyporphism.
    //
    // Without this, scala just combines the types of the arms of the if-then-else.
    // Witness:
    //     scala> val foo = if (true) 5 else 5.0
    //     foo: Double = 5.0
    //     scala> val foo: Any = if (true) 5 else 5.0
    //     foo: Any = 5
    //     scala> val bar : java.lang.Number = if (true) 5 else 5.0
    //     bar: Number = 5
    // It seems to infer type Double for foo, and that squashes the polymorphism of
    // the number type, unless you explicitly make the val have type Any or Number.
    //
    val res: JNumber = a.getAnyRef match {
      case dt: DFDLDateTime => {
        val seconds = dt.calendar.get(Calendar.SECOND)
        val frac = dt.calendar.get(Calendar.MILLISECOND)
        if (frac == 0) { seconds }
        else {
          val d = seconds + (frac / 1000.0)
          d
        }
      }
      case _ =>
        throw new NumberFormatException(
          "fn:" + fieldName + "-from-dateTime only accepts xs:dateTime."
        )
    }
    asBigDecimal(res)
  }
}

case class FNYearFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDate(recipe, argType) {
  val fieldName = "year"
  val field = Calendar.EXTENDED_YEAR
}
case class FNMonthFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNFromDate(recipe, argType) {
  val fieldName = "month"
  val field = Calendar.MONTH
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigInt = {
    super.computeValue(a, dstate).getBigInt.add(JBigInt.ONE) // JAN 0
  }
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
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueBigDecimal = {
    //
    // Please see the block comment in FNSecondsFromDateTime.computeValue
    //
    // It explains why val res below must be of type JNumber.
    //
    val res: JNumber = a.getAnyRef match {
      case dt: DFDLTime => {
        val seconds = dt.calendar.get(Calendar.SECOND)
        val frac = dt.calendar.get(Calendar.MILLISECOND)
        if (frac == 0) { seconds }
        else {
          val d = seconds + (frac / 1000.0)
          d
        }
      }
      case _ =>
        throw new NumberFormatException(
          "fn:" + fieldName + "-from-dateTime only accepts xs:dateTime."
        )
    }
    asBigDecimal(res)
  }
}

case class FNContains(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {

  /**
   * Returns an xs:boolean indicating whether or not the value of \$arg1 contains
   * (at the beginning, at the end, or anywhere within) \$arg2.
   */
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValueBool = {
    val sourceString = arg1.getString
    val valueString = arg2.getString

    // If the value of \$arg2 is the zero-length string, then the function returns true.
    if (valueString.isEmpty()) return true

    // If the value of \$arg1 is the zero-length string, the function returns false.
    if (sourceString.isEmpty()) return false

    val res = sourceString.contains(valueString)
    res
  }
}

case class FNStartsWith(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {

  /**
   *  Returns an xs:boolean indicating whether or not the
   *  value of \$arg1 starts with \$arg2.
   */
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValueBool = {
    val sourceString = arg1.getString
    val prefixString = arg2.getString

    // If the value of \$arg2 is the zero-length string, then the function returns true.
    if (prefixString.isEmpty()) return true

    // If the value of \$arg1 is the zero-length string and the value
    // of \$arg2 is not the zero-length string, then the function returns false.
    if (sourceString.isEmpty()) return false

    val res = sourceString.startsWith(prefixString)
    res
  }
}

case class FNEndsWith(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {

  /**
   * Returns an xs:boolean indicating whether or not the
   * value of \$arg1 ends with \$arg2.
   */
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValueBool = {
    val sourceString = arg1.getString
    val postfixString = arg2.getString

    // If the value of \$arg2 is the zero-length string, then the function returns true.
    if (postfixString.isEmpty()) return true

    // If the value of \$arg1 is the zero-length string and the value of \$arg2
    // is not the zero-length string, then the function returns false.
    if (sourceString.isEmpty()) return false

    val res = sourceString.endsWith(postfixString)
    res
  }
}

trait FNErrorException {
  self: Diagnostic =>
  def asDiagnostic = self
}

case class FNErrorFunctionException(
  schemaContext: Maybe[SchemaFileLocation],
  dataContext: Maybe[DataLocation],
  errorMessage: String
) extends ProcessingError("Expression Evaluation", schemaContext, dataContext, errorMessage)
  with FNErrorException

case class FNError(recipes: List[CompiledDPath]) extends FNArgsList(recipes) {
  override def computeValue(values: List[DataValuePrimitive], dstate: DState) = {
    val maybeSFL =
      if (dstate.runtimeData.isDefined) One(dstate.runtimeData.get.schemaFileLocation)
      else Nope

    val errorString =
      if (values.isEmpty) "http://www.w3.org/2005/xqt-errors#FOER0000"
      else values.mkString(". ")

    dstate.mode match {
      case UnparserNonBlocking | UnparserBlocking =>
        UnparseError(maybeSFL, dstate.contextLocation, errorString)
      case _: ParserMode => {
        val fe = new FNErrorFunctionException(maybeSFL, dstate.contextLocation, errorString)
        throw fe
      }
    }
  }
}
