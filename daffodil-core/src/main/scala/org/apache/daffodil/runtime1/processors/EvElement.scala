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

package org.apache.daffodil.runtime1.processors

import java.lang.{ Long => JLong }

import org.apache.daffodil.lib.cookers.ChoiceDispatchKeyCooker
import org.apache.daffodil.lib.cookers.OutputNewLineCooker
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeJULong
import org.apache.daffodil.runtime1.dpath.EvalMode
import org.apache.daffodil.runtime1.dpath.UnparserBlocking
import org.apache.daffodil.runtime1.dsom._

sealed trait LengthEv extends Evaluatable[JLong]

class ExplicitLengthEv(expr: CompiledExpression[JLong], ci: DPathCompileInfo)
  extends EvaluatableExpression[JLong](expr, ci)
  with LengthEv
  with InfosetCachedEvaluatable[JLong] {
  override def runtimeDependencies = Vector()

  /**
   * Length is special. For the dfdl:length property, when it's an expression
   * we use blocking mode when unparsing.
   *
   * Most (all?) other Evaluatables for runtime-valued properties will use
   * UnparserNonBlocking when unparsing.
   */
  override protected final def maybeUseUnparserMode: Maybe[EvalMode] = Maybe(UnparserBlocking)

  override def compute(state: State): JLong = {
    val v: JLong = super.compute(state)
    if (v < 0) {
      state.SDE("dfdl:length expression result must be non-negative, but was: %d", v)
    }
    v
  }
}

class ImplicitLengthEv(lengthValue: Long, ci: DPathElementCompileInfo)
  extends Evaluatable[JLong](ci)
  with LengthEv
  with NoCacheEvaluatable[JLong] {

  override def runtimeDependencies = Vector()

  private val jLength = JLong.valueOf(lengthValue)

  override protected def compute(state: State): JLong = {
    jLength
  }
}

/*
 * We need Evaluatables that let us tease apart the different algorithmic cases
 * for determining length.
 *
 * Specified-length means elements with simple types with lengthKind 'implicit', or
 * lengthKind 'explicit' - for simple or complex types.
 *
 * There really are these cases for specified length unparsing:
 *
 * # length convertible to N bits. This case includes binary data as well as textual data
 * when the encoding is fixed width. In this case, even if the lengthUnits is
 * 'characters' we can convert to a number of bits.
 *
 * # length in variable-width characters, but lengthUnits is 'bits' or 'bytes'.
 * In this case, we must unparse only until we run out of space when space is
 * limited. Truncation, in the one case it is allowed (type string with
 * dfdl:truncateSpecifiedLenghtString='yes') requires care, as the number of
 * characters where to truncate isn't clear.
 *
 * # variable width characters, units are characters. In this case we need
 * entirely different algorithms as we must count the number of characters.
 */

/**
 * Used when lengthKind='explicit', computes the length in bits or Nope if
 * the length depends on encoding, and the encoding is variable width.
 *
 * Handles the case of LengthUnits.Characters by returning Nope if
 * the charset is variable width. Otherwise it computes the number of
 * bits using the width.
 *
 * Nope does NOT mean that we cannot yet compute the length expression. It
 * means the length depends on variable-width character encoding (and since
 * encoding is runtime-valued sometimes, we need an Ev for this.
 *
 * Sometimes a length cannot be computed yet - happens during unparsing, for
 * an element whose length expression refers back to an element with a
 * dfdl:outputValueCalc that has not yet been computed, or a variable that
 * has not yet been computed.
 *
 * In that case, the access to the infoset throws particular exceptions
 * descended from the RetryableException trait.
 */
sealed abstract class LengthInBitsEvBase(
  ci: DPathCompileInfo,
  val lengthUnits: LengthUnits,
  val lengthKind: LengthKind
) extends Evaluatable[MaybeJULong](ci)
  with InfosetCachedEvaluatable[MaybeJULong] {

  protected def maybeCharsetEv: Maybe[CharsetEv]
  protected def lengthInLengthUnits(state: ParseOrUnparseState): Long

  override protected def compute(state: ParseOrUnparseState): MaybeJULong = {
    val lengthV = lengthInLengthUnits(state)
    val bitsLength: Long =
      lengthUnits match {
        case LengthUnits.Bits => lengthV
        case LengthUnits.Bytes => lengthV * 8
        case LengthUnits.Characters => {
          val maybeWidth = maybeCharsetEv.get.evaluate(state).maybeFixedWidth
          if (maybeWidth.isEmpty)
            return MaybeJULong.Nope
          else {
            val width = maybeWidth.get
            lengthV * width
          }
        }
      }
    MaybeJULong(bitsLength)
  }

}

/**
 * Provides the length of the element, in bits, for Explicit or Implicit
 * length elements, including deals with LengthUnits.Characters and fixed
 * width encodings.
 *
 * Does NOT take into account minLength and maxLength or
 * textOutputMinLength for unparsing of Explicit length elements.
 * See ElementTargetLengthInBitsEv.
 */
class LengthInBitsEv(
  lengthUnits: LengthUnits,
  lengthKind: LengthKind,
  override val maybeCharsetEv: Maybe[CharsetEv],
  val lengthEv: LengthEv,
  ci: DPathCompileInfo
) extends LengthInBitsEvBase(ci, lengthUnits, lengthKind) {

  override def runtimeDependencies = maybeCharsetEv.toList :+ lengthEv

  override protected def lengthInLengthUnits(state: ParseOrUnparseState) =
    lengthEv.evaluate(state).longValue()
}

/**
 * Since the minimum text length might be specified in LengthUnits.Characters
 * for the minLength facet, and for the textOutputMinLength property, then
 * converting those to bits requires possibly not knowing the
 * encoding until runtime.
 *
 * Hence, we have to compute this similarly at runtime.
 */
class MinLengthInBitsEv(
  lengthUnits: LengthUnits,
  lengthKind: LengthKind,
  override val maybeCharsetEv: Maybe[CharsetEv],
  minLen: Long,
  ci: DPathCompileInfo
) extends LengthInBitsEvBase(ci, lengthUnits, lengthKind) {

  override def runtimeDependencies = maybeCharsetEv.toList

  override protected def lengthInLengthUnits(state: ParseOrUnparseState) = minLen
}

/**
 * Used for unparsing, to determine whether we have a target length
 * from the dfdl:length property and Explicit length, or for fixed length
 * from facets or textOutputMinLength.
 *
 * Evaluates to Nope if variable-width encoding prevents computing the
 * targetLengthInBits.
 *
 * Nope does NOT MEAN the length cannot be computed yet. It means variable-width
 * encoding prevents computing the target length in bits units.
 */
class UnparseTargetLengthInBitsEv(
  val lengthInBitsEv: LengthInBitsEv,
  minLengthInBitsEv: MinLengthInBitsEv,
  ci: DPathCompileInfo
) extends Evaluatable[MaybeJULong](ci)
  with InfosetCachedEvaluatable[MaybeJULong] {

  override def runtimeDependencies = Vector(this.lengthInBitsEv, this.minLengthInBitsEv)

  /**
   * Note: use of MaybeJULong type. New Maybe type added which can be stored in
   * a generic data structure. (I.e., a MaybeJULong is an AnyRef, not an AnyVal
   * like Long and MaybeULong).
   *
   * Ev values that are runtime dependent get stored in a generic cache of
   * AnyRef values, so this gives us the benefits of a maybe object on top of
   * the boxing that is required to store a number in in the cache.
   *
   */

  override protected def compute(state: ParseOrUnparseState): MaybeJULong = {
    val maybeLen = lengthInBitsEv.evaluate(state)
    val maybeMin = minLengthInBitsEv.evaluate(state)
    if (maybeLen.isEmpty) {
      MaybeJULong.Nope
    } else {
      Assert.invariant(maybeMin.isDefined)
      val lenInBits = maybeLen.get
      val minInBits = maybeMin.get
      val targetLenInBits = scala.math.max(lenInBits, minInBits)
      MaybeJULong(targetLenInBits)
    }
  }
}

class UnparseTargetLengthInCharactersEv(
  val lengthEv: LengthEv,
  val charsetEv: CharsetEv,
  minLen: Long,
  ci: DPathElementCompileInfo
) extends Evaluatable[MaybeJULong](ci)
  with InfosetCachedEvaluatable[MaybeJULong] {

  override def runtimeDependencies = Vector(this.lengthEv, charsetEv)

  /**
   * Note: use of MaybeJULong type. New Maybe type added which can be stored in
   * a generic data structure. (I.e., a MaybeJULong is an AnyRef, not an AnyVal
   * like Long and MaybeULong).
   *
   * Ev values that are runtime dependent get stored in a generic cache of
   * AnyRef values, so this gives us the benefits of a maybe object on top of
   * the boxing that is required to store a number in in the cache.
   *
   */

  override protected def compute(state: ParseOrUnparseState): MaybeJULong = {
    Assert.usage(
      charsetEv.evaluate(state).maybeFixedWidth.isEmpty
    ) // must be variable-width-chars
    val len = lengthEv.evaluate(state)
    val targetLen = scala.math.max(len, minLen)
    MaybeJULong(targetLen)
  }
}

class OccursCountEv(expr: CompiledExpression[JLong], ci: DPathElementCompileInfo)
  extends EvaluatableExpression[JLong](expr, ci)
  with InfosetCachedEvaluatable[JLong] {
  override def runtimeDependencies = Vector()
}

class OutputNewLineEv(expr: CompiledExpression[String], ci: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](expr, OutputNewLineCooker, ci)
  with InfosetCachedEvaluatable[String] {
  override def runtimeDependencies = Vector()
}

class ChoiceDispatchKeyEv(expr: CompiledExpression[String], ci: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](expr, ChoiceDispatchKeyCooker, ci)
  with InfosetCachedEvaluatable[String] {
  override def runtimeDependencies = Vector()
}
