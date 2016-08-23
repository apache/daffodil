/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.exceptions._
import com.ibm.icu.util.Calendar
import scala.math.BigDecimal.RoundingMode
import com.ibm.icu.util.TimeZone
import AsIntConverters._
import edu.illinois.ncsa.daffodil.calendar.DFDLDateTime
import edu.illinois.ncsa.daffodil.calendar.DFDLCalendar
import edu.illinois.ncsa.daffodil.calendar.DFDLTime
import edu.illinois.ncsa.daffodil.calendar.DFDLDate
import java.lang.{ Number => JNumber, Byte => JByte, Short => JShort, Integer => JInt, Long => JLong, Float => JFloat, Double => JDouble, Boolean => JBoolean }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }

case class FNAbs(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(v: AnyRef, dstate: DState) = {
    val value = asBigDecimal(v).abs
    argType match {
      case _: NodeInfo.UnsignedNumeric.Kind => value
      case NodeInfo.Decimal => value
      case NodeInfo.Float => asAnyRef(asFloat(v).floatValue().abs)
      case NodeInfo.Double => asAnyRef(asDouble(v).doubleValue().abs)
      case NodeInfo.Long => asLong(value)
      case NodeInfo.Int => asInt(value)
      case NodeInfo.Integer => asBigInt(value)
      case NodeInfo.Short => asShort(value)
      case NodeInfo.Byte => asByte(value)
      case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function abs.", argType))
    }
  }
}

case class FNStringLength(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: AnyRef, dstate: DState) = asAnyRef(str.asInstanceOf[String].length.toLong)
}

case class FNLowerCase(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: AnyRef, dstate: DState) = str.asInstanceOf[String].toLowerCase
}

case class FNUpperCase(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(str: AnyRef, dstate: DState) = str.asInstanceOf[String].toUpperCase
}

case class FNConcat(recipes: List[CompiledDPath]) extends FNArgsList(recipes) {
  override def computeValue(values: List[Any], dstate: DState) = values.mkString
}

// No such function in DFDL v1.0
// But leave this here because we probably will want to add it as a
// daffodil extension function and then eventually hope it gets into DFDL1.1
//case class FNStringJoin(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {
//  override def computeValue(arg1: AnyRef, arg2:AnyRef, dstate: DState) = {
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
  override def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): AnyRef = {
    val sourceString = arg1.asInstanceOf[String]
    val startingLoc = asDouble(arg2).doubleValue()
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
  override def computeValue(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, dstate: DState): AnyRef = {
    val sourceString = arg1.asInstanceOf[String]
    val startingLoc = asDouble(arg2)
    val length = asDouble(arg3)

    substring(sourceString, startingLoc, length)
  }
}

case class FNSubstringBefore(recipes: List[CompiledDPath])
  extends FNTwoArgs(recipes)
  with SubstringKind {

  override def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): AnyRef = {
    val sourceString: String = arg1.asInstanceOf[String]
    val searchString: String = arg2.asInstanceOf[String]

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

  override def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): AnyRef = {
    val sourceString: String = arg1.asInstanceOf[String]
    val searchString: String = arg2.asInstanceOf[String]

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

  private def calendarToDFDLDateTime(calendar: Calendar, hasTZ: Boolean, dstate: DState, fncName: String, toType: String): DFDLCalendar = {
    try {
      val cal = new DFDLDateTime(calendar, hasTZ)
      return cal
    } catch {
      case ex: java.lang.IllegalArgumentException =>
        dstate.SDE("Conversion Error: %s failed to convert \"%s\" to %s. Due to %s", fncName, calendar.toString, toType, ex.getMessage())
    }
  }
  override def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState) = {
    val dateCalendar = arg1.asInstanceOf[DFDLCalendar]
    val timeCalendar = arg2.asInstanceOf[DFDLCalendar]

    val dateCal = dateCalendar.getCalendar
    val timeCal = timeCalendar.getCalendar

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
      case (true, true) if dateTZ != timeTZ => dstate.SDE("The two arguments to fn:dateTime have inconsistent timezones")
      case (true, true) => newCal.setTimeZone(dateTZ)
    }

    val finalCal = calendarToDFDLDateTime(newCal, hasTZ, dstate, name, "DateTime")
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
      case l: JLong => BigDecimal.valueOf(l)
      case f: JFloat => BigDecimal.valueOf(f.toDouble)
      case d: JDouble => BigDecimal.valueOf(d)
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
  def compute(value: AnyRef, precision: Int) = {
    // We should only receive 'Numeric' types here which are either
    // xs:double, xs:float, xs:decimal, xs:integer or a sub-type thereof.
    //
    // The conversions code should be enforcing this, if not we
    // have a serious issue.
    //
    def roundIt = {
      val unroundedValue: JBigDecimal = unrounded(value)
      val roundedValue = toBaseNumericType(round(unroundedValue, precision), value)
      roundedValue
    }

    val result = value match {
      case f: JFloat if (f.isNaN() || f == 0 || f.floatValue().isPosInfinity || f.floatValue().isNegInfinity) => f
      case d: JDouble if (d.isNaN() || d == 0 || d.doubleValue().isPosInfinity || d.doubleValue().isNegInfinity) => d
      //
      // This used to be a single big case like:
      // case _:Float | _: Double | ... | _: Short => ....
      // but unfortunately, the scala compiler spit out a warning (about analyzing cases)
      // so this is equivalent, but avoids the warning.
      //
      case _: JFloat => roundIt
      case _: JDouble => roundIt
      case _: BigDecimal => roundIt
      case _: JBigDecimal => roundIt
      case _: BigInt => roundIt
      case _: JBigInt => roundIt
      case _: JLong => roundIt
      case _: JInt => roundIt
      case _: JByte => roundIt
      case _: JShort => roundIt
      case _ => Assert.invariantFailed("Unrecognized numeric type. Must be xs:float, xs:double, xs:decimal, xs:integer or a type derived from these.")
    }
    result
  }

  private def unrounded(value: AnyRef): java.math.BigDecimal = {
    val result = value match {
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
      case _: JFloat | _: JDouble | _: BigDecimal | _: JBigDecimal => asBigDecimal(value)
      case _: BigInt | _: JBigInt | _: JLong | _: JInt | _: JByte | _: JShort => asBigDecimal(value)
      case _ => Assert.usageError("Received a type other than xs:decimal, xs:double, xs:float, xs:integer or any of its sub-types.")
    }
    result
  }

  private def round(value: JBigDecimal, precision: Int): JBigDecimal = {
    val rounded = value.setScale(precision, JBigDecimal.ROUND_HALF_EVEN)
    rounded
  }

  /**
   * If the type of \$arg is one of the four numeric types xs:float, xs:double,
   * xs:decimal or xs:integer the type of the result is the same as the type of \$arg.
   *
   * If the type of \$arg is a type derived from one of the numeric types, the
   * result is an instance of the base numeric type.
   */
  private def toBaseNumericType(value: JBigDecimal, origValue: AnyRef): AnyRef = {
    val result = origValue match {
      case _: JFloat => value.floatValue() // xs:float
      case _: JDouble => value.doubleValue() // xs:double
      case _: BigDecimal => value // xs:decimal
      case _: JBigDecimal => value
      case _: BigInt => value.toBigInteger()
      case _: JBigInt => value.toBigInteger()
      case _: JLong | _: JInt | _: JByte | _: JShort => value.toBigInteger() // xs:integer
      case _ => Assert.usageError("Received a type other than xs:decimal, xs:double, xs:float, xs:integer or any of its sub-types.")
    }
    asAnyRef(result)
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

  override def computeValue(value: AnyRef, dstate: DState) = {
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

  override def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState) = {
    val precision = asInt(arg2)
    val roundedValue = compute(arg1, precision)
    roundedValue
  }
}

case class FNNot(recipe: CompiledDPath, argType: NodeInfo.Kind = null)
  extends FNOneArg(recipe, NodeInfo.Boolean) {
  override def computeValue(value: AnyRef, dstate: DState): JBoolean = {
    val bool = asBoolean(FNToBoolean.computeValue(value, dstate))
    !bool
  }
}

case class FNNilled(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, NodeInfo.Nillable) {
  override def computeValue(value: AnyRef, dstate: DState): JBoolean = value.asInstanceOf[DIElement].isNilled
}

trait ExistsKind {

  def exists(recipe: CompiledDPath, dstate: DState): Boolean = {
    dstate.fnExists() // hook so we can insist this is non-constant at compile time.

    val res = try {
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
        // we are evaluating the expression, and hoping it will evaluate
        // completely just to avoid the overhead of creating a suspension
        //
        // we will always re-throw in this case so that fn:exists doesn't
        // return false, resulting in the expression getting a value that
        // might, later, have returned true.
        //
        throw th
      }
      case UnparserBlocking => {
        dstate.currentNode match {
          case c: DIFinalizable if (c.isFinal) => false
          case _ => throw th
        }
      }
      case ParserNonBlocking => {
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
      case UnparserBlocking | UnparserNonBlocking => {
        th match {
          case u: UnsuppressableException => throw u
          case r: RetryableException => // ok fall through
          case _ => throw th
        }
      }
      case ParserNonBlocking => // ok
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
  override def run(dstate: DState) {
    val res = exists(recipe, dstate)
    dstate.setCurrentValue(res)
  }

  override def toXML = toXML(recipe.toXML)

}

case class FNEmpty(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends RecipeOpWithSubRecipes(recipe)
  with ExistsKind {
  override def run(dstate: DState) {
    val res = exists(recipe, dstate)
    dstate.setCurrentValue(!res)
  }

  override def toXML = toXML(recipe.toXML)

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
case class FNLocalName0(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    // Same as using "." to denote self.
    val localName = dstate.currentElement.name

    if (localName.contains(":"))
      throw new IllegalArgumentException("fn:local-name failed. " + localName + " is not a valid NCName as it contains ':'.")

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
  override def computeValue(value: AnyRef, dstate: DState) = {
    Assert.usageError("not to be called. DPath compiler should be answering this without runtime calls.")
  }

  override def run(dstate: DState) {
    // Save off original state, which is the original
    // element/node that calls inputValueCalc with fn:local-name
    //
    val origState = dstate

    // Execute the recipe/expression which should
    // return a node/element whose local-name we want.
    //
    recipe.run(dstate)

    val localName = dstate.currentElement.name

    if (localName.contains(":"))
      throw new IllegalArgumentException("fn:local-name failed. " + localName + " is not a valid NCName as it contains ':'.")

    // The original state contains the node/element upon which
    // fn:local-name was called.  This is where we should set
    // the value.
    //
    origState.setCurrentValue(localName)
  }
}

case class FNCeiling(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: AnyRef, dstate: DState) = argType match {

    case NodeInfo.Decimal => {
      val bd = asBigDecimal(value).setScale(0, RoundingMode.CEILING)
      bd.round(bd.mc)
    }
    case NodeInfo.Float => asAnyRef(asFloat(value).floatValue().ceil)
    case NodeInfo.Double => asAnyRef(asDouble(value).floatValue().ceil)
    case _: NodeInfo.Numeric.Kind => value
    case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function ceiling.", argType))
  }
}

case class FNFloor(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: AnyRef, dstate: DState) = argType match {

    case NodeInfo.Decimal => {
      val bd = asBigDecimal(value).setScale(0, RoundingMode.FLOOR)
      bd.round(bd.mc)
    }
    case NodeInfo.Float => asAnyRef(asFloat(value).floatValue().floor)
    case NodeInfo.Double => asAnyRef(asDouble(value).doubleValue().floor)
    case _: NodeInfo.Numeric.Kind => value
    case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function floor.", argType))
  }
}

case class FNRound(recipe: CompiledDPath, argType: NodeInfo.Kind) extends FNOneArg(recipe, argType) {
  override def computeValue(value: AnyRef, dstate: DState) = {
    val res = argType match {
      case NodeInfo.Decimal => {
        val bd = asBigDecimal(value)
        //
        // A MathContext object whose settings have
        // the values required for unlimited precision arithmetic.
        val mc = java.math.MathContext.UNLIMITED
        bd.round(mc)
      }
      case NodeInfo.Float => {
        val f = asFloat(value).floatValue()
        if (f.isPosInfinity || f.isNegInfinity) f
        else if (f.isNaN()) throw new NumberFormatException("fn:round received NaN")
        else f.round
      }
      case NodeInfo.Double => {
        val d = asDouble(value).doubleValue()
        if (d.isPosInfinity || d.isNegInfinity) d
        else if (d.isNaN()) throw new NumberFormatException("fn:round received NaN")
        else d.round
      }
      case _: NodeInfo.Numeric.Kind => value
      case _ => Assert.invariantFailed(String.format("Type %s is not a valid type for function round.", argType))
    }
    asAnyRef(res)
  }
}

trait FNFromDateTimeKind {
  def fieldName: String
  def field: Int
}

abstract class FNFromDateTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    a match {
      case dt: DFDLDateTime => asAnyRef(dt.getField(field))
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-dateTime only accepts xs:dateTime.")
    }
  }
}

abstract class FNFromDate(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    a match {
      case d: DFDLDate => asAnyRef(d.getField(field))
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-date only accepts xs:date.")
    }
  }
}

abstract class FNFromTime(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType)
  with FNFromDateTimeKind {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    a match {
      case t: DFDLTime => asAnyRef(t.getField(field))
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-time only accepts xs:time.")
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
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = asAnyRef(asInt(super.computeValue(a, dstate)).intValue() + 1) // JAN 0
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

  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
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
    val res: JNumber = a match {
      case dt: DFDLDateTime => {
        val seconds = dt.getField(Calendar.SECOND)
        val frac = dt.getField(Calendar.MILLISECOND)
        if (frac == 0) { seconds }
        else {
          val d = seconds + (frac / 1000.0)
          d
        }
      }
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-dateTime only accepts xs:dateTime.")
    }
    res
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
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = asAnyRef(asInt(super.computeValue(a, dstate)).intValue() + 1) // JAN 0
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
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = {
    //
    // Please see the block comment in FNSecondsFromDateTime.computeValue
    //
    // It explains why val res below must be of type JNumber.
    //
    val res: JNumber = a match {
      case dt: DFDLTime => {
        val seconds = dt.getField(Calendar.SECOND)
        val frac = dt.getField(Calendar.MILLISECOND)
        if (frac == 0) { seconds }
        else {
          val d = seconds + (frac / 1000.0)
          d
        }
      }
      case _ => throw new NumberFormatException("fn:" + fieldName + "-from-dateTime only accepts xs:dateTime.")
    }
    res
  }
}

case class FNContains(recipes: List[CompiledDPath])
  extends FNTwoArgs(recipes) {
  /**
   * Returns an xs:boolean indicating whether or not the value of \$arg1 contains
   * (at the beginning, at the end, or anywhere within) \$arg2.
   */
  override def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): JBoolean = {
    val sourceString = arg1.asInstanceOf[String]
    val valueString = arg2.asInstanceOf[String]

    // If the value of \$arg2 is the zero-length string, then the function returns true.
    if (valueString.isEmpty()) return true

    // If the value of \$arg1 is the zero-length string, the function returns false.
    if (sourceString.isEmpty()) return false

    val res = sourceString.contains(valueString)
    res
  }
}

case class FNStartsWith(recipes: List[CompiledDPath])
  extends FNTwoArgs(recipes) {
  /**
   *  Returns an xs:boolean indicating whether or not the
   *  value of \$arg1 starts with \$arg2.
   */
  override def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): JBoolean = {
    val sourceString = arg1.asInstanceOf[String]
    val prefixString = arg2.asInstanceOf[String]

    // If the value of \$arg2 is the zero-length string, then the function returns true.
    if (prefixString.isEmpty()) return true

    // If the value of \$arg1 is the zero-length string and the value
    // of \$arg2 is not the zero-length string, then the function returns false.
    if (sourceString.isEmpty()) return false

    val res = sourceString.startsWith(prefixString)
    res
  }
}

case class FNEndsWith(recipes: List[CompiledDPath])
  extends FNTwoArgs(recipes) {
  /**
   * Returns an xs:boolean indicating whether or not the
   * value of \$arg1 ends with \$arg2.
   */
  override def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): JBoolean = {
    val sourceString = arg1.asInstanceOf[String]
    val postfixString = arg2.asInstanceOf[String]

    // If the value of \$arg2 is the zero-length string, then the function returns true.
    if (postfixString.isEmpty()) return true

    // If the value of \$arg1 is the zero-length string and the value of \$arg2
    // is not the zero-length string, then the function returns false.
    if (sourceString.isEmpty()) return false

    val res = sourceString.endsWith(postfixString)
    res
  }
}
