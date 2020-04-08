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

package org.apache.daffodil.processors

import java.math.RoundingMode
   
import scala.collection.mutable

import com.ibm.icu.text.DecimalFormat
import com.ibm.icu.text.DecimalFormatSymbols

import org.apache.daffodil.dsom._
import org.apache.daffodil.cookers.TextStandardGroupingSeparatorCooker
import org.apache.daffodil.cookers.TextBooleanFalseRepCooker
import org.apache.daffodil.cookers.TextStandardExponentRepCooker
import org.apache.daffodil.cookers.TextBooleanTrueRepCooker
import org.apache.daffodil.cookers.TextStandardDecimalSeparatorCooker
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeChar
import org.apache.daffodil.util.MaybeDouble
import org.apache.daffodil.schema.annotation.props.gen.TextNumberCheckPolicy
import org.apache.daffodil.schema.annotation.props.gen.TextNumberRounding
import org.apache.daffodil.schema.annotation.props.gen.TextNumberRoundingMode
import org.apache.daffodil.dpath.NodeInfo.PrimType

class TextStandardDecimalSeparatorEv(expr: CompiledExpression[String], tci: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TextStandardDecimalSeparatorCooker,
    tci)
  with InfosetCachedEvaluatable[List[String]] {
  override lazy val runtimeDependencies = Vector()
}

class TextStandardGroupingSeparatorEv(expr: CompiledExpression[String], tci: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    TextStandardGroupingSeparatorCooker,
    tci)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Vector()
}

class TextStandardExponentRepEv(expr: CompiledExpression[String], tci: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    TextStandardExponentRepCooker,
    tci)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Vector()
}

class TextNumberFormatEv(
  tci: DPathCompileInfo,
  decimalSepEv: Maybe[TextStandardDecimalSeparatorEv],
  groupingSepEv: Maybe[TextStandardGroupingSeparatorEv],
  exponentRepEv: Maybe[TextStandardExponentRepEv],
  infRep: Maybe[String],
  nanRep: Maybe[String],
  checkPolicy: TextNumberCheckPolicy,
  textNumberPattern: String,
  rounding: TextNumberRounding,
  roundingMode: Maybe[TextNumberRoundingMode],
  roundingIncrement: MaybeDouble,
  zeroRepsRaw: List[String],
  isInt: Boolean,
  primType: PrimType)
  extends Evaluatable[ThreadLocal[DecimalFormat]](tci)
  with InfosetCachedEvaluatable[ThreadLocal[DecimalFormat]] {

  override lazy val runtimeDependencies = (decimalSepEv.toList ++ groupingSepEv.toList ++ exponentRepEv.toList).toVector

  private def checkUnique(
    decimalSep: MaybeChar,
    groupingSep: MaybeChar,
    exponentRep: Maybe[String]): Unit = {

    val mm = new mutable.HashMap[String, mutable.Set[String]] with mutable.MultiMap[String, String]
    if (decimalSep.isDefined) mm.addBinding(decimalSep.get.toString, "textStandardDecimalSeparator")
    if (groupingSep.isDefined) mm.addBinding(groupingSep.get.toString, "textStandardGroupingSeparator")
    if (exponentRep.isDefined) mm.addBinding(exponentRep.get, "textStandardExponentRep")
    if (infRep.isDefined) mm.addBinding(infRep.get, "textStandardInfinityRep")
    if (nanRep.isDefined) mm.addBinding(nanRep.get, "textStandardNaNRep")
    zeroRepsRaw.foreach { zr => mm.addBinding(zr, "textStandardZeroRep") }

    val dupes = mm.filter { case (k, s) => s.size > 1 }
    val dupeStrings = dupes.map {
      case (k, s) =>
        "Non-distinct property '%s' found in: %s".format(k, s.mkString(", "))
    }
    tci.schemaDefinitionUnless(dupeStrings.size == 0, dupeStrings.mkString("\n"))
  }

  private def generateNumFormat(
    decimalSep: MaybeChar,
    groupingSep: MaybeChar,
    exponentRep: Maybe[String]): DecimalFormat = {

    val dfs = new DecimalFormatSymbols()

    if (decimalSep.isDefined) {
      dfs.setDecimalSeparator(decimalSep.get)
    }

    if (groupingSep.isDefined) {
      dfs.setGroupingSeparator(groupingSep.get)
    }

    // TODO: this is allowed to be case insenstive, ICU doesn't support that
    if (exponentRep.isDefined) {
      dfs.setExponentSeparator(exponentRep.get)
    }

    if (infRep.isDefined) {
      // TODO: this is allowed to be case insensitive, ICU doesn't support that
      dfs.setInfinity(infRep.get)
    }

    if (nanRep.isDefined) {
      // TODO: this is allowed to be case insensitive, ICU doesn't support that
      dfs.setNaN(nanRep.get)
    }

    val df = new DecimalFormat(textNumberPattern, dfs)

    val cp = checkPolicy match {
      case TextNumberCheckPolicy.Strict => true
      case TextNumberCheckPolicy.Lax => false
    }
    df.setParseStrict(cp)

    rounding match {
      case TextNumberRounding.Pattern => {
        df.setRoundingMode(RoundingMode.HALF_EVEN.ordinal())
      }
      case TextNumberRounding.Explicit => {
        val rm = roundingMode.get match {
          case TextNumberRoundingMode.RoundCeiling => RoundingMode.CEILING
          case TextNumberRoundingMode.RoundFloor => RoundingMode.FLOOR
          case TextNumberRoundingMode.RoundDown => RoundingMode.DOWN
          case TextNumberRoundingMode.RoundUp => RoundingMode.UP
          case TextNumberRoundingMode.RoundHalfEven => RoundingMode.HALF_EVEN
          case TextNumberRoundingMode.RoundHalfDown => RoundingMode.HALF_DOWN
          case TextNumberRoundingMode.RoundHalfUp => RoundingMode.HALF_UP
          case TextNumberRoundingMode.RoundUnnecessary => RoundingMode.UNNECESSARY
        }
        df.setRoundingMode(rm.ordinal())
        df.setRoundingIncrement(roundingIncrement.get)
      }
    }

    if (isInt) {
      df.setMaximumFractionDigits(0)
      df.setDecimalSeparatorAlwaysShown(false)
      df.setParseIntegerOnly(true)
    }

    df
  }

  override protected def compute(state: ParseOrUnparseState): ThreadLocal[DecimalFormat] = {

    val decimalSepList = if (decimalSepEv.isDefined) {
      val seps = decimalSepEv.get.evaluate(state)
      if (seps.length > 1) {
        // TODO: ICU only supports a single decimal separator
        tci.notYetImplemented("More than one textStandardDecimalSeparator")
      }
      MaybeChar(seps.head(0))
    } else {
      MaybeChar.Nope
    }

    val groupingSep = if (groupingSepEv.isDefined) {
      MaybeChar(groupingSepEv.get.evaluate(state)(0))
    } else {
      MaybeChar.Nope
    }

    val exponentRep = if (exponentRepEv.isDefined) {
      One(exponentRepEv.get.evaluate(state))
    } else {
      Nope
    }

    checkUnique(
      decimalSepList,
      groupingSep,
      exponentRep)

    val numFormat = new ThreadLocal[DecimalFormat] with Serializable {
      override def initialValue() = {
        generateNumFormat(
          decimalSepList,
          groupingSep,
          exponentRep)
      }
    }

    numFormat
  }
  
}


class TextBooleanTrueRepEv(exprT: CompiledExpression[String], falseRepEv: TextBooleanFalseRepEv, mustBeSameLength: Boolean, tci: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, List[String]](
    exprT,
    TextBooleanTrueRepCooker,
    tci)
  with InfosetCachedEvaluatable[List[String]] {
  override lazy val runtimeDependencies = Vector()

  override final protected def compute(state: ParseOrUnparseState): List[String] = {
    if (mustBeSameLength) {

      //All values of textBooleanTrueRep and textBooleanFalseRep must be equal in length
      val textBooleanTrueReps: List[String] = super.compute(state)
      val textBooleanFalseReps: List[String] = falseRepEv.evaluate(state)

      val trueLength = textBooleanTrueReps(0).length
      val falseLength = textBooleanFalseReps(0).length
      if (trueLength != falseLength ||
        textBooleanTrueReps.exists(x => x.length != trueLength) ||
        textBooleanFalseReps.exists(x => x.length != falseLength)) {
        tci.schemaDefinitionError("If dfdl:lengthKind is 'explicit' or 'implicit' and either dfdl:textPadKind or dfdl:textTrimKind  is 'none' then both dfdl:textBooleanTrueRep and dfdl:textBooleanFalseRep must have the same length.")
      }
      textBooleanTrueReps
    } else {
      super.compute(state)
    }
  }
}

class TextBooleanFalseRepEv(expr: CompiledExpression[String], tci: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TextBooleanFalseRepCooker,
    tci)
  with InfosetCachedEvaluatable[List[String]] {
  override lazy val runtimeDependencies = Vector()
}
