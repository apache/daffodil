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

import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Numbers
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.unparsers.UState

/**
 * A TypeCalculator is a class that performs some kind of conversion from one DataValuePrimitive
 * to another DataValuePrimitive. In general, this is done to convert a parsed physical
 * representation from the data to a logical representation that goes in the infoset, and
 * reverse on unparse. The former is done in the inputTypeCalc functions, and the latter in the
 * outputTypeCalc functions. While these TypeCalculators are generic can in theory support
 * convertting between any types, their only use is converting from and int to a string for
 * parse, and the reverse for unparse.
 *
 * The different TypeCalculator implementations allow for different conversions based on
 * different properties provided in the schema (e.g. repType, enumerations, unions, repValues,
 * repValueRanges).
 */
abstract class TypeCalculator(val srcType: NodeInfo.Kind, val dstType: NodeInfo.Kind)
  extends Serializable {
  type Error = String

  /**
   * The inputTypeCalc function provides only the conversion logic, returning either the new
   * value or an error string. This allows it to be agnostic about how to handle the resulting
   * value or error, assuming the caller knows best. If a caller needs normalization and
   * errors to become ParseErrors, they should instead call inputTypeCalcParse.
   */
  def inputTypeCalc(
    x: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): Either[Error, DataValuePrimitiveNullable]

  /**
   * The outputTypeCalc function provides only the conversion logic, returning either the new
   * value or an error string. This allows it to be agnostic about how to handle the resulting
   * value or error, assuming the caller knows best. If a caller needs normalization and
   * errors to become UnparseErrors, they should instead call outputTypeCalcUnparse.
   */
  def outputTypeCalc(
    x: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): Either[Error, DataValuePrimitiveNullable]

  final def inputTypeCalcParse(
    pstate: PState,
    context: RuntimeData,
    x_in: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): DataValuePrimitiveNullable = {
    val x = normalizeArg(x_in, xType)
    val res = inputTypeCalc(x, xType) match {
      case Left(err) => {
        val diag = new ParseError(
          Maybe(context.schemaFileLocation),
          Maybe(pstate.currentLocation),
          err,
        )
        pstate.setFailed(diag)
        DataValue.NoValue
      }
      case Right(ans) => ans
    }
    res
  }

  final def outputTypeCalcUnparse(
    ustate: UState,
    context: RuntimeData,
    x_in: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): DataValuePrimitiveNullable = {
    val x = normalizeArg(x_in, xType)
    val res = outputTypeCalc(x, xType) match {
      case Left(err) => {
        val diag = new ParseError(
          Maybe(context.schemaFileLocation),
          Maybe(ustate.currentLocation),
          err,
        )
        ustate.setFailed(diag)
        DataValue.NoValue
      }
      case Right(ans) => ans
    }
    res
  }

  /*
   * In theory, this normalizeArg method should not be nessasary. We know at compile time what
   * types a given calculator is defined in terms of, so the compiler should insert any conversion
   * nessasary so that the type being passed in is what the calculator was defined in terms of.
   * In practice, we are not doing that. Instead, we convert all numeric types to JBigInt.
   */
  protected def normalizeArg(
    x: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): DataValuePrimitive = {
    if (xType == NodeInfo.String) {
      x
    } else {
      Numbers.asBigInt(x.getAnyRef)
    }
  }
}

/*
 * We define valueMap in terms of AnyRef instead of DataValuePrimitive, because HashMap is polymorphic,
 * so using DataValuePrimitive would trigger boxing
 */
class KeysetValueTypeCalculatorOrdered(
  valueMap: HashMap[DataValuePrimitive, DataValuePrimitive],
  rangeTable: Seq[(RangeBound, RangeBound, DataValuePrimitive)],
  unparseMap: HashMap[DataValuePrimitive, DataValuePrimitive],
  srcType: NodeInfo.Kind,
  dstType: NodeInfo.Kind,
) extends TypeCalculator(srcType, dstType) {

  override def inputTypeCalc(
    x: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): Either[Error, DataValuePrimitiveNullable] = {
    if (valueMap.contains(x)) {
      Right(valueMap.get(x).get)
    } else {
      val ans1: Option[(RangeBound, RangeBound, DataValuePrimitiveNullable)] = rangeTable.find({
        case (min, max, _) => {
          min.testAsLower(x) && max.testAsUpper(x)
        }
      })
      ans1 match {
        case None => {
          Left(s"Value ${x} not found in enumeration dfdlx:repValues")
        }
        case Some((_, _, v)) => Right(v)
      }
    }
  }

  override def outputTypeCalc(
    x: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): Either[Error, DataValuePrimitiveNullable] = {
    unparseMap.get(x) match {
      case Some(v) => Right(v)
      case None => Left(s"Value ${x} not found in enumeration")
    }
  }

}

class KeysetValueTypeCalculatorUnordered(
  valueMap: HashMap[DataValuePrimitive, DataValuePrimitive],
  unparseMap: HashMap[DataValuePrimitive, DataValuePrimitive],
  srcType: NodeInfo.Kind,
  dstType: NodeInfo.Kind,
) extends TypeCalculator(srcType, dstType) {

  override def inputTypeCalc(
    x: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): Either[Error, DataValuePrimitiveNullable] = {
    valueMap.get(x) match {
      case Some(a) => Right(a)
      case None => Left(s"Value ${x} not found in enumeration dfdlx:repValues")
    }
  }

  override def outputTypeCalc(
    x: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): Either[Error, DataValuePrimitiveNullable] = {
    unparseMap.get(x) match {
      case Some(v) => Right(v)
      case None => Left(s"Value ${x} not found in enumeration")
    }
  }

}

class IdentityTypeCalculator(srcType: NodeInfo.Kind) extends TypeCalculator(srcType, srcType) {
  override def inputTypeCalc(
    x: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): Either[Error, DataValuePrimitiveNullable] = Right(x)
  override def outputTypeCalc(
    x: DataValuePrimitive,
    xType: NodeInfo.Kind,
  ): Either[Error, DataValuePrimitiveNullable] = Right(x)
}

/*
 * Since we can inherit the restriction from xsd facets, we also need to be able to support an
 * aribitrary subset of: minInclusive, minExclusive, maxInclusive, and maxExclusive
 */
class RepValueSet(
  val valueSet: HashSet[DataValuePrimitive],
  val valueRanges: Set[(RangeBound, RangeBound)],
) extends Serializable {

  def merge(other: RepValueSet): RepValueSet = {
    val valueSet_ = valueSet ++ other.valueSet
    val valueRanges_ = valueRanges ++ other.valueRanges
    new RepValueSet(valueSet_, valueRanges_)
  }

  lazy val isEmpty: Boolean = valueSet.isEmpty && valueRanges.isEmpty

}

//TODO, many of the key/values we receive will be BigInt.
//We should check if we can safely convert them to Long

object RepValueSetCompiler {
  def compile(
    valueSet: Seq[DataValuePrimitive],
    valuesRanges: Seq[(RangeBound, RangeBound)],
  ): RepValueSet = {
    val hashSet = HashSet.empty ++ valueSet
    val rangeSet = Set.empty ++ valuesRanges.filter(x => x._1.isDefined || x._2.isDefined)
    new RepValueSet(hashSet, rangeSet)
  }
  def empty: RepValueSet = compile(Seq.empty, Seq.empty)
}

object TypeCalculatorCompiler {

  // mappings: [(keySet, canonicalKey, value)]
  def compileKeysetValue(
    mappings: Seq[(RepValueSet, DataValuePrimitive, DataValuePrimitive)],
    srcType: NodeInfo.Kind,
    dstType: NodeInfo.Kind,
  ): TypeCalculator = {
    Assert.invariant(!mappings.isEmpty)

    /*
     * We need to cast to HashMap, because the type of HashMap.++ returns a generic Map
     * HashMap.+ returns a HashMap, so we can avoid the case by doing the fold ourself
     */
    val valueMap: HashMap[DataValuePrimitive, DataValuePrimitive] =
      (HashMap.empty ++ mappings.flatMap(x => {
        val (keySet, _, value) = x
        keySet.valueSet.map((_, value))
      })).asInstanceOf[HashMap[DataValuePrimitive, DataValuePrimitive]]
    val rangeTable: Seq[(RangeBound, RangeBound, DataValuePrimitive)] = mappings.flatMap(x => {
      val (keySet, _, value) = x
      keySet.valueRanges.map({ case (min, max) => (min, max, value) })
    })
    val unparseMap: HashMap[DataValuePrimitive, DataValuePrimitive] =
      (HashMap.empty ++ mappings.map(x => {
        val (_, canonicalKey, value) = x
        (value, canonicalKey)
      })).asInstanceOf[HashMap[DataValuePrimitive, DataValuePrimitive]]

    /*
     * Type erasure makes dispatching based on if we have an ordered keyset or not difficult
     * Really, the problem is that we should not be using AnyRef at all,
     * but since the DPath library is based around AnyRef, we are stuck with it
     */
    rangeTable match {
      case Seq() =>
        new KeysetValueTypeCalculatorUnordered(valueMap, unparseMap, srcType, dstType)
      case _ => {
        new KeysetValueTypeCalculatorOrdered(valueMap, rangeTable, unparseMap, srcType, dstType)
      }
    }
  }

  def compileIdentity(srcType: NodeInfo.Kind): TypeCalculator = new IdentityTypeCalculator(
    srcType,
  )

}

object Range {
  type Range = (RangeBound, RangeBound)
  def inclusive(lower: DataValuePrimitiveNullable, upper: DataValuePrimitiveNullable): Range = {
    val lower_ = new RangeBound(lower, true)
    val upper_ = new RangeBound(upper, true)
    (lower_, upper_)
  }
}

/*
 * This would be a good example of why we might want DataValue to have a more rich
 * hierarchy. In theory, RangeBound could be defined in term of a DataValueNumeric type,
 * from which all of the numeric types extend. In practice, getting complicated DataValue hierarchies to work
 * turns out to be annoyingly finicky, and so may not be worth the benifit.
 *
 * Additionally, in the current implementation, all instance of RangeBound actually use JBigInt.
 * This is because most of the current type calculator implementation is based on casting all numeric types
 * to BigInt, instead of trying to keep track of what types we would be expecting at runtime.
 */
class RangeBound(val maybeBound: DataValuePrimitiveNullable, val isInclusive: Boolean)
  extends Serializable {

  lazy val isEmpty = maybeBound.isEmpty
  lazy val isDefined = maybeBound.isDefined

  override def toString(): String = {
    if (maybeBound.isDefined) {
      maybeBound.getAnyRef.toString + "_" + (if (isInclusive) "inclusive" else "exclusive")
    } else {
      "Nope"
    }
  }

  def intersectsWithOtherBounds(lower: RangeBound, upper: RangeBound): Boolean = {
    if (maybeBound.isEmpty) {
      false
    } else {
      val bound = maybeBound.getNonNullable
      val inBounds = lower.testAsLower(bound) && upper.testAsUpper(bound)
      val atBoundery = maybeBound == lower.maybeBound || maybeBound == upper.maybeBound
      inBounds && (isInclusive || !atBoundery)
    }
  }

  /*
   * It should be the case that x is either a JJBigInt, or a Long
   */

  def testAsLower(x: DataValuePrimitive): Boolean = {
    if (maybeBound.isEmpty) {
      true
    } else {
      val bound = maybeBound.getNonNullable
      if (isInclusive) {
        le(bound, x)
      } else {
        lt(bound, x)
      }
    }
  }
  def testAsUpper(x: DataValuePrimitive): Boolean = {
    if (maybeBound.isEmpty) {
      true
    } else {
      val bound = maybeBound.getNonNullable
      if (isInclusive) {
        le(x, bound)
      } else {
        lt(x, bound)
      }
    }
  }

  private def le(x: DataValuePrimitive, y: DataValuePrimitive): Boolean = {
    val x2 = Numbers.asBigInt(x.getAnyRef)
    val y2 = Numbers.asBigInt(y.getAnyRef)

    x2.compareTo(y2) <= 0
  }

  private def lt(x: DataValuePrimitive, y: DataValuePrimitive): Boolean = {
    val x2 = Numbers.asBigInt(x.getAnyRef)
    val y2 = Numbers.asBigInt(y.getAnyRef)

    x2.compareTo(y2) < 0
  }

}
