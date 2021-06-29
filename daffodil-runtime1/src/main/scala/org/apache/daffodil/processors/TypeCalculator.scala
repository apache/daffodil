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

import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import org.apache.daffodil.dpath.DState
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DataValue
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.parsers.ParseError
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.util.Delay
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.Numbers
import org.apache.daffodil.xml.QNameBase

abstract class TypeCalculator(val srcType: NodeInfo.Kind, val dstType: NodeInfo.Kind)
  extends Serializable {
  type Error = String

  def initialize(): Unit = {
    // base method does nothing
  }

  /*
   * We can be used from both a parser directly, and as part of a DPath expression.
   * There are 2 main differences that require handling these cases seperatly
   *
   * 1) A (un)parser expects errors to be reported via state.setFailed, while DPath expects subexpressions to report
   *     error by throwing
   * 2) DPathCompiledExpressions provides a top level interface to initiate evaluation which accepts a ParseOrUnparseState,
   *      and a second interface to evaluate subexpressions which accepts a DState
   */

  def inputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error])
  def outputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error])

  def inputTypeCalcParse(pstate: PState, context: RuntimeData, x_in: DataValuePrimitive, xType: NodeInfo.Kind): DataValuePrimitiveNullable = {
    val x = normalizeArg(x_in, xType)
    val (ans, err) = inputTypeCalc(x, xType)
    Assert.invariant(ans.isDefined ^ err.isDefined)

    if (err.isDefined) {
      val diag = new ParseError(Maybe(context.schemaFileLocation), Maybe(pstate.currentLocation), err.get)
      pstate.setFailed(diag)
    }

    ans
  }
  def outputTypeCalcUnparse(ustate: UState, context: RuntimeData, x_in: DataValuePrimitive, xType: NodeInfo.Kind): DataValuePrimitiveNullable = {
    val x = normalizeArg(x_in, xType)
    val (ans, err) = outputTypeCalc(x, xType)
    Assert.invariant(ans.isDefined ^ err.isDefined)

    if (err.isDefined) {
      val diag = new ParseError(Maybe(context.schemaFileLocation), Maybe(ustate.currentLocation), err.get)
      ustate.setFailed(diag)
    }

    //In event of an error, we still want to return Maybe.Nope, which happens
    //to be what ans would have
    ans
  }

  /*
   * It appears that dpath expressions actually throw errors.
   * See the definition of DAFError
   */
  def inputTypeCalcRun(dstate: DState, x_in: DataValuePrimitive, xType: NodeInfo.Kind): Unit = {
    val x = normalizeArg(x_in, xType)
    val context = dstate.runtimeData.get
    val (ans, err) = inputTypeCalc(x, xType)
    Assert.invariant(ans.isDefined ^ err.isDefined)

    if (err.isDefined) {
      val diag = new ParseError(Maybe(context.schemaFileLocation), dstate.contextLocation, err.get)
      throw diag
    }

    dstate.setCurrentValue(ans)

  }
  def outputTypeCalcRun(dstate: DState, x_in: DataValuePrimitive, xType: NodeInfo.Kind): Unit = {
    val x = normalizeArg(x_in, xType)
    val context = dstate.runtimeData.get
    val (ans, err) = outputTypeCalc(x, xType)
    Assert.invariant(ans.isDefined ^ err.isDefined)

    if (err.isDefined) {
      val diag = new ParseError(Maybe(context.schemaFileLocation), dstate.contextLocation, err.get)
      throw diag
    }

    dstate.setCurrentValue(ans)

  }

  def supportsParse: Boolean = true
  def supportsUnparse: Boolean = true

  /*
   * In theory, this normalizeArg method should not be nessasary. We know at compile time what
   * types a given calculator is defined in terms of, so the compiler should insert any conversion
   * nessasary so that the type being passed in is what the calculator was defined in terms of.
   * In practice, we are not doing that. Instead, we convert all numeric types to JBigInt.
   */
  protected def normalizeArg(x: DataValuePrimitive, xType: NodeInfo.Kind): DataValuePrimitive = {
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
class KeysetValueTypeCalculatorOrdered(valueMap: HashMap[DataValuePrimitive, DataValuePrimitive], rangeTable: Seq[(RangeBound, RangeBound, DataValuePrimitive)], unparseMap: HashMap[DataValuePrimitive, DataValuePrimitive],
  srcType: NodeInfo.Kind, dstType: NodeInfo.Kind)
  extends TypeCalculator(srcType, dstType) {

  override def inputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error]) = {
    if (valueMap.contains(x)) {
      (valueMap.get(x).get, Maybe.Nope)
    } else {
      val ans1: Option[(RangeBound, RangeBound, DataValuePrimitiveNullable)] = rangeTable.find({
        case (min, max, _) => {
          min.testAsLower(x) && max.testAsUpper(x)
        }
      })
      ans1 match {
        case None => {
          (DataValue.NoValue, One(s"Key ${x} not found in keyset-value mapping"))
        }
        case Some((_, _, v)) => (v, Maybe.Nope)
      }
    }
  }

  override def outputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error]) = {
    unparseMap.get(x) match {
      case Some(v) => (v, Maybe.Nope)
      case None => (DataValue.NoValue, One(s"Value ${x} not found in keyset-value mapping"))
    }
  }

}

class KeysetValueTypeCalculatorUnordered(valueMap: HashMap[DataValuePrimitive, DataValuePrimitive], unparseMap: HashMap[DataValuePrimitive, DataValuePrimitive], srcType: NodeInfo.Kind, dstType: NodeInfo.Kind)
  extends TypeCalculator(srcType, dstType) {

  override def inputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error]) = {
    if (valueMap.contains(x)) {
      valueMap.get(x) match {
        case Some(a) => (a, Maybe.Nope)
        case None => (DataValue.NoValue, One(s"Value ${x} not found in keyset-value mapping"))
      }
    } else {
      (DataValue.NoValue, One(s"Key ${x} not found in keyset-value mapping"))

    }
  }
  override def outputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error]) = {
    unparseMap.get(x) match {
      case Some(v) => (v, Maybe.Nope)
      case None => (DataValue.NoValue, One(s"Value ${x} not found in keyset-value mapping"))
    }
  }

}

class ExpressionTypeCalculator(
  private val maybeInputTypeCalcDelay: Delay[Maybe[CompiledExpression[AnyRef]]],
  private val maybeOutputTypeCalcDelay: Delay[Maybe[CompiledExpression[AnyRef]]],
  srcType: NodeInfo.Kind,
  dstType: NodeInfo.Kind)
  extends TypeCalculator(srcType, dstType) {

  /*
   * objects with Delay arguments for functional programming construction of
   * cyclic graphs, need a way to force the delays, resulting in an ordinary
   * (though cyclic) data structure.
   */
  final override def initialize(): Unit = {
    super.initialize()
    maybeInputTypeCalc
    maybeOutputTypeCalc
  }

  override def supportsParse = maybeInputTypeCalc.isDefined
  override def supportsUnparse = maybeOutputTypeCalc.isDefined

  /*
   * Compiling DPath expressions may need to evaluate typeCalculators in order to lookup their srcType and dstType.
   * To prevent circular dependencies, this means that the underlying expressions must be lazy.
   *
   * Since these fields must be lazy, we cannot use them to determine supportsParse or supportUnparse
   */
  lazy val maybeInputTypeCalc = maybeInputTypeCalcDelay.value
  lazy val maybeOutputTypeCalc = maybeOutputTypeCalcDelay.value

  //The class TypeValueCalc will verify that supports(Un)Parse is true when nessasary
  //Therefore, if we ever call the below functions, we know that the relevent Maybe object is defined.

  //TODO, pass x into DPath state

  override def inputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error]) =
    Assert.invariantFailed("inputTypeCalc not implemented on ExpressionTypeCalculator. Call the more specialized forms directly")
  override def outputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error]) =
    Assert.invariantFailed("outputTypeCalc not implemented on ExpressionTypeCalculator. Call the more specialized forms directly")

  override def inputTypeCalcParse(state: PState, context: RuntimeData, x: DataValuePrimitive, xType: NodeInfo.Kind): DataValuePrimitiveNullable = {
    val dstate = state.dState
    val oldRepValue = dstate.repValue
    val oldLogicalValue = dstate.logicalValue
    dstate.repValue = x
    dstate.logicalValue = DataValue.NoValue

    val ans = Maybe(maybeInputTypeCalc.get.evaluate(state))

    dstate.repValue = oldRepValue
    dstate.logicalValue = oldLogicalValue
    if (ans.isDefined) {
      DataValue.unsafeFromAnyRef(ans.get)
    } else {
      DataValue.NoValue;
    }
  }
  override def outputTypeCalcUnparse(state: UState, context: RuntimeData, x: DataValuePrimitive, xType: NodeInfo.Kind): DataValuePrimitiveNullable = {
    val dstate = state.dState
    val oldRepValue = dstate.repValue
    val oldLogicalValue = dstate.logicalValue
    dstate.repValue = DataValue.NoValue
    dstate.logicalValue = x

    val ans = maybeOutputTypeCalc.get.evaluate(state)

    dstate.repValue = oldRepValue
    dstate.logicalValue = oldLogicalValue
    DataValue.unsafeFromAnyRef(ans)
  }

  override def inputTypeCalcRun(dstate: DState, x: DataValuePrimitive, xType: NodeInfo.Kind): Unit = {
    val oldRepValue = dstate.repValue
    val oldLogicalValue = dstate.logicalValue
    dstate.repValue = x
    dstate.logicalValue = DataValue.NoValue

    maybeInputTypeCalc.get.run(dstate)

    dstate.repValue = oldRepValue
    dstate.logicalValue = oldLogicalValue

  }
  override def outputTypeCalcRun(dstate: DState, x: DataValuePrimitive, xType: NodeInfo.Kind): Unit = {
    val oldRepValue = dstate.repValue
    val oldLogicalValue = dstate.logicalValue
    dstate.repValue = DataValue.NoValue
    dstate.logicalValue = x

    maybeOutputTypeCalc.get.run(dstate)

    dstate.repValue = oldRepValue
    dstate.logicalValue = oldLogicalValue
  }
}

class IdentifyTypeCalculator(srcType: NodeInfo.Kind) extends TypeCalculator(srcType, srcType) {
  override def inputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error]) = (x, Maybe.Nope)
  override def outputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error]) = (x, Maybe.Nope)
}

class UnionTypeCalculator(subCalculators: Seq[(RepValueSet, RepValueSet, TypeCalculator)], srcType: NodeInfo.Kind, dstType: NodeInfo.Kind)
  extends TypeCalculator(srcType, dstType) {
  //TODO, it may be worth it to pre-compute a hash table for direct dispatch,
  //Similar to how keyset-value works
  override def inputTypeCalc(x: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error]) = {
    val subCalcSeq = subCalculators.filter(sub => sub._1.contains(x))
    Assert.invariant(subCalcSeq.length <= 1)
    if (subCalcSeq.isEmpty) {
      (DataValue.NoValue, One(s"Key ${x} does not match any component of this simpleType union"))
    } else {
      val subCalc = subCalcSeq.head._3
      subCalc.inputTypeCalc(x, xType)
    }
  }

  override def outputTypeCalc(x_in: DataValuePrimitive, xType: NodeInfo.Kind): (DataValuePrimitiveNullable, Maybe[Error]) = {
    val x = normalizeArg(x_in, xType)
    val subCalcSeq = subCalculators.filter(sub => sub._2.contains(x))
    Assert.invariant(subCalcSeq.length <= 1)
    if (subCalcSeq.isEmpty) {
      (DataValue.NoValue, One(s"Key ${x} does not match the logical values from any component of this union."))
    } else {
      val subCalc = subCalcSeq.head._3
      subCalc.outputTypeCalc(x, xType)
    }
  }

}

/*
 * Since we can inherit the restriction from xsd facets, we also need to be able to support an
 * aribitrary subset of: minInclusive, minExclusive, maxInclusive, and maxExclusive
 */
class RepValueSet(val valueSet: HashSet[DataValuePrimitive], val valueRanges: Set[(RangeBound, RangeBound)]) extends Serializable {
  def contains(x: DataValuePrimitive): Boolean = {
    val ans1 = valueSet.contains(x)
    if (ans1) {
      ans1
    } else {
      valueRanges.map({
        case (min, max) =>
          min.testAsLower(x) && max.testAsUpper(x)
      }).fold(false)(_ || _)
    }
  }

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
  def compile(valueSet: Seq[DataValuePrimitive], valuesRanges: Seq[(RangeBound, RangeBound)]): RepValueSet = {
    val hashSet = HashSet.empty ++ valueSet
    val rangeSet = Set.empty ++ valuesRanges.filter(x => x._1.isDefined || x._2.isDefined)
    new RepValueSet(hashSet, rangeSet)
  }
  def empty: RepValueSet = compile(Seq.empty, Seq.empty)
}

object TypeCalculatorCompiler {

  type TypeCalcMap = Map[QNameBase, TypeCalculator]

  // mappings: [(keySet, canonicalKey, value)]
  def compileKeysetValue(
    mappings: Seq[(RepValueSet, DataValuePrimitive, DataValuePrimitive)],
    srcType: NodeInfo.Kind,
    dstType: NodeInfo.Kind): TypeCalculator = {
    Assert.invariant(!mappings.isEmpty)

    /*
     * We need to cast to HashMap, because the type of HashMap.++ returns a generic Map
     * HashMap.+ returns a HashMap, so we can avoid the case by doing the fold ourself
     */
    val valueMap: HashMap[DataValuePrimitive, DataValuePrimitive] = (HashMap.empty ++ mappings.flatMap(x => {
      val (keySet, _, value) = x
      keySet.valueSet.map((_, value))
    })).asInstanceOf[HashMap[DataValuePrimitive, DataValuePrimitive]]
    val rangeTable: Seq[(RangeBound, RangeBound, DataValuePrimitive)] = mappings.flatMap(x => {
      val (keySet, _, value) = x
      keySet.valueRanges.map({ case (min, max) => (min, max, value) })
    })
    val unparseMap: HashMap[DataValuePrimitive, DataValuePrimitive] = (HashMap.empty ++ mappings.map(x => {
      val (_, canonicalKey, value) = x
      (value, canonicalKey)
    })).asInstanceOf[HashMap[DataValuePrimitive, DataValuePrimitive]]

    /*
     * Type erasure makes dispatching based on if we have an ordered keyset or not difficult
     * Really, the problem is that we should not be using AnyRef at all,
     * but since the DPath library is based around AnyRef, we are stuck with it
     */
    rangeTable match {
      case Seq() => new KeysetValueTypeCalculatorUnordered(valueMap, unparseMap, srcType, dstType)
      case _ => {
        new KeysetValueTypeCalculatorOrdered(valueMap, rangeTable, unparseMap, srcType, dstType)
      }
    }
  }

  /*
   * compileExpression is already a highly overloaded name from the DPath expression compiler.
   * While this technically overload that function, to avoid confusion, we are giving it a different
   * name entirely.
   */
  def compileTypeCalculatorFromExpression(
    optInputTypeCalc: => Option[CompiledExpression[AnyRef]],
    optOutputTypeCalc: => Option[CompiledExpression[AnyRef]],
    srcType: NodeInfo.Kind, dstType: NodeInfo.Kind): ExpressionTypeCalculator = {
    lazy val maybeInputType: Maybe[CompiledExpression[AnyRef]] = optInputTypeCalc.map(Maybe(_)).getOrElse(Maybe.Nope)
    lazy val maybeOutputType: Maybe[CompiledExpression[AnyRef]] = optOutputTypeCalc.map(Maybe(_)).getOrElse(Maybe.Nope)
    val tc =
      new ExpressionTypeCalculator(
        Delay('maybeInputType, this, maybeInputType),
        Delay('maybeOutputType, this, maybeOutputType),
        srcType,
        dstType)
    tc
  }
  def compileIdentity(srcType: NodeInfo.Kind): TypeCalculator = new IdentifyTypeCalculator(srcType)

  //subCalculators: Seq[(repValues, logicalValues, subCalc)]
  def compileUnion(subCalculators: Seq[(RepValueSet, RepValueSet, TypeCalculator)]): TypeCalculator = {
    //TODO, in some cases, it may be possible to merge some subCalculators
    //It may also be possible to compute a direct dispatch table
    val types = subCalculators.map(x => (x._3.srcType, x._3.dstType))
    val srcTypes = types.map(_._1)
    val dstTypes = types.map(_._2)
    val srcType = srcTypes.head
    val dstType = dstTypes.head
    Assert.invariant(srcTypes.map(_ == srcType).reduce(_ == _))
    Assert.invariant(dstTypes.map(_ == dstType).reduce(_ == _))
    new UnionTypeCalculator(subCalculators, srcType, dstType)
  }

}

object Range {
  type Range = (RangeBound, RangeBound)
  def inclusive(lower:DataValuePrimitiveNullable, upper:DataValuePrimitiveNullable):Range={
    val lower_ = new RangeBound(lower,true)
    val upper_ = new RangeBound(upper,true)
    (lower_,upper_)
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
