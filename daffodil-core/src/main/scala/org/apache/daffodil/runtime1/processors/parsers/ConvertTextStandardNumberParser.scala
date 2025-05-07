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

package org.apache.daffodil.runtime1.processors.parsers

import java.lang.{ Double => JDouble, Float => JFloat, Long => JLong, Number => JNumber }
import java.math.MathContext
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInteger }
import java.text.ParsePosition
import scala.util.matching.Regex

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueNumber
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Success
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.TextNumberFormatEv

import com.ibm.icu.math.{ BigDecimal => ICUBigDecimal }
import com.ibm.icu.text.DecimalFormat

case class ConvertTextCombinatorParser(
  rd: TermRuntimeData,
  valueParser: Parser,
  converterParser: Parser
) extends CombinatorParser(rd) {

  override def runtimeDependencies = Vector()

  override def childProcessors = Vector(valueParser, converterParser)

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

  final protected lazy val virtualPointScaleFactor =
    scala.math.pow(10.0, textDecimalVirtualPoint)

  final protected def applyTextDecimalVirtualPointForParse(num: JNumber): JNumber = {
    if (textDecimalVirtualPoint == 0) num
    else {
      // scale for virtual decimal point
      val scaledNum: JNumber = num match {
        case l: JLong => {
          // For integers that can fit in a long, ICU will always return a long
          val scaled = JBigDecimal.valueOf(l).scaleByPowerOfTen(-textDecimalVirtualPoint)
          if (textDecimalVirtualPoint < 0) scaled.toBigIntegerExact()
          else scaled
        }
        case bi: JBigInteger => {
          // For numbers that cannot fit into a long but do not have a decimal, ICU will return
          // a BigDecimal which we converted to a BigInteger. Convert back to a BigDecimal so it
          // can be scaled
          val bd = new JBigDecimal(bi)
          bd.scaleByPowerOfTen(-textDecimalVirtualPoint)
        }
        case d: JDouble => {
          // ICU only returns doubles if they are -INF, INF, NaN, or negative zero, which do not
          // need scaling
          Assert.invariant(
            d.isNaN || d.isInfinite || JDouble.doubleToLongBits(d) == 0x8000000000000000L
          )
          d
        }
        // $COVERAGE-OFF$
        case _ => {
          // ICU returns either Long, Double (for inf/nan) or BigDecimal. BigDecimal should have
          // been converted to BigInteger if possible, and if not it should have created a PE
          // since we have a virtual point. If we see any other types, it means ICU returned
          // something unexpected or we didn't throw a PE correctly.
          Assert.invariantFailed(s"""
            |Number cannot be scaled for virtual decimal point,
            |expected integer type but got ${num.getClass.getSimpleName}.
            """.stripMargin)
        }
        // $COVERAGE-ON$
      }
      scaledNum
    }
  }

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
   * @param value value to be scaled
   * @return a JNumber of the same concrete type as the argument.
   */
  final protected def applyTextDecimalVirtualPointForUnparse(value: JNumber): JNumber = {
    val res: JNumber = value match {
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
      case bd: JBigDecimal => {
        bd.scaleByPowerOfTen(textDecimalVirtualPoint).round(MathContext.UNLIMITED)
      }
      // $COVERAGE-OFF$
      case _ => {
        Assert.invariantFailed(s"""
          |Number cannot be unscaled for virtual decimal point,
          |expected decimal, float, or double, but got
          |${value.getClass.getSimpleName}.""".stripMargin)
      }
      // $COVERAGE-ON$
    }
    // Result type is same as argument type.
    Assert.invariant(res.getClass == value.getClass)
    res
  }
}

case class ConvertTextStandardNumberParser(
  textNumberFormatEv: TextNumberFormatEv,
  zeroRepsRegex: List[Regex],
  override val context: ElementRuntimeData,
  override val textDecimalVirtualPoint: Int
) extends TextPrimParser
  with TextDecimalVirtualPointMixin {

  override def runtimeDependencies = Vector(textNumberFormatEv)

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
        val df = textNumberFormatEv.evaluate(start)
        val strToParse = if (df.isParseStrict) str else str.trim
        val pos = new ParsePosition(0)
        val icuNum: JNumber = df.parse(strToParse, pos) match {
          case null => {
            val infNaN: JDouble =
              if (df.isDecimalPatternMatchRequired) {
                // ICU failed to parse. But there is a bug in ICU4J (ICU-22303) that if there is
                // a decimal in the pattern and we've set that decimal to be required (due to
                // strict mode), then it will fail to parse Inf/NaN representations. As a
                // workaround, we clone the DecimalFormat, disable requiring the decimal, and
                // reparse. We only accept successful Inf/NaN parses though--everything else is
                // considered a parse error since it meant the decimal point was missing or
                // wasn't either inf/nan or a valid number. If ICU fixes this bug, we should
                // remove this infNan variable and its use, as it is likely pretty expensive to
                // clone, change a setting, and reparse. Fortunately, it is only in the error
                // case of strict parsing so should be rare.
                pos.setIndex(0)
                val newDF = df.clone().asInstanceOf[DecimalFormat]
                newDF.setDecimalPatternMatchRequired(false)
                newDF.parse(strToParse, pos) match {
                  case d: JDouble => {
                    Assert.invariant(d.isNaN || d.isInfinite)
                    d
                  }
                  case _ => null
                }
              } else {
                null
              }

            if (infNaN != null) {
              infNaN
            } else {
              PE(
                start,
                "Unable to parse %s from text: %s",
                context.optPrimType.get.globalQName,
                str
              )
              return
            }
          }
          case d: JDouble => {
            // ICU returns a Double only if it parsed NaN, Infinity, -Infinity, or negative
            // zero. We will later pass this value in primNumber.fromNumber, which will fail if
            // the primitive type does not allow NaN/Infinity
            Assert.invariant(
              d.isNaN || d.isInfinite || JDouble.doubleToLongBits(d) == 0x8000000000000000L
            )
            d
          }
          case bd: ICUBigDecimal => {
            // ICU will return their own custom BigDecimal if the value cannot fit in a Long and
            // isn't infinity/NaN. We only want Java types, so detect this and convert it to the
            // appropriate type. Additionally, due to ICU lax parsing, ICU could successfully
            // parse something with a non-zero fractional part even if the pattern does not
            // specify a decimal. So in cases where decimals are not allowed (e.g. integer
            // primitives, virtual decimal points), we create a PE.

            val fractionalPartMustBeZero = primNumeric.isInteger || textDecimalVirtualPoint > 0

            if (bd.scale == 0) bd.unscaledValue
            else if (!fractionalPartMustBeZero) {
              bd.toBigDecimal
            } else {
              PE(
                start,
                "Unable to parse %s from text: %s",
                context.optPrimType.get.globalQName,
                str
              )
              return
            }
          }
          case l: JLong => l
          // $COVERAGE-OFF$
          case num: JNumber => {
            Assert.invariantFailed(
              "ICU returned an unexpected type. Expected either Double, ICU BigDecimal, or Long, but got " + num.getClass.getName
            )
          }
          // $COVERAGE-ON$
        }

        // Verify that what was parsed was what was passed exactly in byte count.
        // Use pos to verify all characters consumed & check for errors!
        if (pos.getIndex != strToParse.length) {
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
            PE(
              start,
              "Unable to parse %s from text: %s",
              context.optPrimType.get.globalQName,
              str
            )
            return
          }
        }

        val num: JNumber = applyTextDecimalVirtualPointForParse(icuNum)

        val numValue: DataValueNumber =
          try {
            primNumeric.fromNumber(num)
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
