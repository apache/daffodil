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

package org.apache.daffodil.processors.parsers

import com.ibm.icu.math.{BigDecimal => ICUBigDecimal}
import com.ibm.icu.text.DecimalFormat

import java.text.ParsePosition
import scala.util.matching.Regex
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.DataValue.DataValueNumber
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.TextNumberFormatEv

import java.lang.{Float => JFloat, Number => JNumber, Long => JLong, Double => JDouble}
import java.math.MathContext
import java.math.{BigDecimal => JBigDecimal}

case class ConvertTextCombinatorParser(
  rd: TermRuntimeData,
  valueParser: Parser,
  converterParser: Parser)
  extends CombinatorParser(rd) {

  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector(valueParser, converterParser)

  def parse(start: PState): Unit = {
    valueParser.parse1(start)
    if (start.processorStatus ne Success) {
      return
    }
    converterParser.parse1(start)
  }
}

trait TextDecimalVirtualPointMixin {
  def textDecimalVirtualPoint: Int

  final protected lazy val virtualPointScaleFactor = scala.math.pow(10.0, textDecimalVirtualPoint)

  final protected def applyTextDecimalVirtualPointForParse(num1: JNumber): JNumber = {
    if (textDecimalVirtualPoint == 0) num1
    else {
      // scale for virtual decimal point
      val scaledNum: JNumber = num1 match {
        // Empirically, in our test suite, we do get Long back here, so the runtime sometimes represents small integer
        // (or possibly even smaller decimal numbers with no fraction part) as Long.
        case l: JLong => {
          val scaled = JBigDecimal.valueOf(l).scaleByPowerOfTen(-textDecimalVirtualPoint)
          if (textDecimalVirtualPoint < 0) scaled.toBigIntegerExact()
          else scaled
        }
        case bd: JBigDecimal => bd.scaleByPowerOfTen(-textDecimalVirtualPoint)
        case f: JFloat => (f / virtualPointScaleFactor).toFloat
        case d: JDouble => (d / virtualPointScaleFactor).toDouble
        // $COVERAGE-OFF$
        case _ => badType(num1)
        // $COVERAGE-ON$
      }
      scaledNum
    }
  }

  // $COVERAGE-OFF$
  private def badType(num1: AnyRef) = {
    Assert.invariantFailed(
      s"""Number cannot be scaled for virtual decimal point,
         |because it is not a decimal, float, or double.
         |The type is ${num1.getClass.getSimpleName}.""".stripMargin)
  }
  // $COVERAGE-ON$


  /**
   * Always creates an integer from a JFloat, JDouble, or JBigDecimal
   * by scaling the argument by the virtual decimal point scaling factor.
   *
   * If the argument is some other JNumber, i.e., not a JFloat, JDouble, or JBigDecimal
   * it can only be an integer type. This method returns the argument value
   * only if no virtual decimal point scaling is needed.
   * Otherwise aborts since only JFloat, JDouble, or JDecimal can be scaled in this way.
   *
   * Floating point NaNs and Infinities are tolerated. (No scaling occurs.)
   *
   * @param valueAsAnyRef value to be scaled. Should be a JNumber. Aborts otherwise.
   * @return a JNumber of the same concrete type as the argument.
   */
  final protected def applyTextDecimalVirtualPointForUnparse(valueAsAnyRef: AnyRef) : JNumber = {
    val res: JNumber = valueAsAnyRef match {
      case jn: JNumber if (textDecimalVirtualPoint == 0) => jn
      // This is not perfectly symmetrical with the parse side equivalent.
      // Empirically in our test suite, we do not see JLong here.
      //
      // If the base 10 data is 1.23 and it becomes a float, that's 1.2300000190734863
      // That is to say, it isn't a perfect representation of 1.23, so we don't know for
      // certain that when we unscale it by the virtualPointScaleFactor that it will
      // become an integer. We just know it will be close to an integer.
      // The same holds for a decimal number that was created via math involving division.
      // In that case there may be fraction digits that will still exist even after we scale
      // the value so that it "should be" an integer.
      //
      // So if it is float/double/decimal after scaling we round it to be exactly an
      // integer.
      //
      case f: JFloat if f.isNaN || f.isInfinite => f
      case f: JFloat => (f * virtualPointScaleFactor).round.toFloat
      case d: JDouble if d.isNaN || d.isInfinite => d
      case d: JDouble => (d * virtualPointScaleFactor).round.toDouble
      case bd: JBigDecimal => bd.scaleByPowerOfTen(textDecimalVirtualPoint).round(MathContext.UNLIMITED)
      case n: JNumber =>
        // $COVERAGE-OFF$ // both badType and the next case are coverage-off
        badType(n)
      case _ => Assert.invariantFailed("Not a JNumber")
      // $COVERAGE-ON$
    }
    // Result type is same as argument type.
    Assert.invariant(res.getClass == valueAsAnyRef.getClass)
    res
  }
}

case class ConvertTextStandardNumberParser(
  textNumberFormatEv: TextNumberFormatEv,
  zeroRepsRegex: List[Regex],
  override val context: ElementRuntimeData,
  override val textDecimalVirtualPoint: Int)
  extends TextPrimParser
  with TextDecimalVirtualPointMixin {

  override lazy val runtimeDependencies = Vector(textNumberFormatEv)

  private val primNumeric = context.optPrimType.get.asInstanceOf[NodeInfo.PrimType.PrimNumeric]

  def parse(start: PState): Unit = {
    val node: DISimple = start.simpleElement
    val str = node.dataValueAsString

    Assert.invariant(str != null) // worst case it should be empty string. But not null.
    if (str == "") {
      PE(start, "Unable to parse %s from empty string", context.optPrimType.get.globalQName)
      return
    }

    // because of the way the zero rep regular expressions are generated, they
    // will match either all or none of 'str', never part of it. Thus,
    // findFirstIn() either matches and it's a zero rep, or it doesn't and it's
    // not a zero
    val numValue: DataValueNumber = zeroRepsRegex.find { _.findFirstIn(str).isDefined } match {
      case Some(_) => primNumeric.fromNumber(0)
      case None => {
        val df = textNumberFormatEv.evaluate(start).get
        val strCheckPolicy = if (df.isParseStrict) str else str.trim
        val pos = new ParsePosition(0)
        val icuNum: Number = df.parse(strCheckPolicy, pos)


        if (icuNum == null) {
          PE(start, "Unable to parse %s from text: %s",
            context.optPrimType.get.globalQName, str)
          return
        }

        // sometimes ICU will return their own custom BigDecimal, even if the
        // value could be represented as a BigInteger. We only want Java types,
        // so detect this and convert it to the appropriate type
        val num1 = icuNum match {
          case bd: ICUBigDecimal => {
            if (bd.scale == 0) bd.unscaledValue
            else bd.toBigDecimal
          }
          case _ => icuNum
        }

        val num2: JNumber = applyTextDecimalVirtualPointForParse(num1)

        // Verify that what was parsed was what was passed exactly in byte count.
        // Use pos to verify all characters consumed & check for errors!
        if (pos.getIndex != strCheckPolicy.length) {
          val isValid =
            if (df.getPadPosition == DecimalFormat.PAD_AFTER_SUFFIX) {
              // If the DecimalFormat pad position is PAD_AFTER_SUFFIX, ICU
              // does not update the parse position to be a the end of the
              // padding, but instead sets the position to the end of the
              // suffix. So we need to manually check to see if all characters
              // after the parse position are the pad character
              val padChar = df.getPadCharacter
              val afterPosition = str.substring(pos.getIndex)
              afterPosition.forall(_ == padChar)
            } else {
              // For all other padPositions, the parse position must be at the
              // end of the string. That's not the case here, so it's not valid
              false
            }
          if (!isValid) {
            PE(start, "Unable to parse %s from text: %s",
              context.optPrimType.get.globalQName, str)
            return
          }
        }

        val numValue: DataValueNumber = try {
          primNumeric.fromNumber(num2)
        } catch {
          case e: InvalidPrimitiveDataException => {
            PE(start, "%s", e.getMessage)
            return
          }
        }
        numValue
      }
    }

    node.overwriteDataValue(numValue)

  }
}
