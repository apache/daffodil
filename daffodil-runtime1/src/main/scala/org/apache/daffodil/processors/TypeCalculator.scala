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
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.parsers.ParseError
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.PreSerialization
import org.apache.daffodil.util.RangeBound
import org.apache.daffodil.util.TransientParam
import org.apache.daffodil.xml.GlobalQName

abstract class TypeCalculator[A <: AnyRef, B <: AnyRef](val srcType: NodeInfo.Kind, val dstType: NodeInfo.Kind)
  extends PreSerialization {
  type Error = String

  /*
   * We can be used from both a parser directly, and as part of a DPath expression.
   * There are 2 main differences that require handling these cases seperatly
   *
   * 1) A (un)parser expects errors to be reported via state.setFailed, while DPath expects subexpressions to report
   *     error by throwing
   * 2) DPathCompiledExpressions provides a top level interface to initiate evaluation which accepts a ParseOrUnparseState,
   *      and a second interface to evaluate subexpressions which accepts a DState
   */

  def inputTypeCalc(x: A, xType: NodeInfo.Kind): (Maybe[B], Maybe[Error])
  def outputTypeCalc(x: B, xType: NodeInfo.Kind): (Maybe[A], Maybe[Error])

  def inputTypeCalcParse(pstate: PState, context: RuntimeData, x: A, xType: NodeInfo.Kind): Maybe[B] = {
    val (ans, err) = inputTypeCalc(x, xType)
    Assert.invariant(ans.isDefined ^ err.isDefined)

    if (err.isDefined) {
      val diag = new ParseError(Maybe(context.schemaFileLocation), Maybe(pstate.currentLocation), err.get)
      pstate.setFailed(diag)
    }

    //In event of an error, we still want to return Maybe.Nope, which happens
    //to be what ans would have
    ans
  }
  def outputTypeCalcUnparse(ustate: UState, context: RuntimeData, x: B, xType: NodeInfo.Kind): Maybe[A] = {
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
  def inputTypeCalcRun(dstate: DState, x: A, xType: NodeInfo.Kind): Unit = {
    val context = dstate.runtimeData.get
    val (ans, err) = inputTypeCalc(x, xType)
    Assert.invariant(ans.isDefined ^ err.isDefined)

    if (err.isDefined) {
      val diag = new ParseError(Maybe(context.schemaFileLocation), dstate.contextLocation, err.get)
      throw diag
      //      throw FNErrorFunctionException(Maybe(context.schemaFileLocation), dstate.contextLocation, err.get)
    }

    dstate.setCurrentValue(ans.get)

  }
  def outputTypeCalcRun(dstate: DState, x: B, xType: NodeInfo.Kind): Unit = {
    val context = dstate.runtimeData.get
    val (ans, err) = outputTypeCalc(x, xType)
    Assert.invariant(ans.isDefined ^ err.isDefined)

    if (err.isDefined) {
      val diag = new ParseError(Maybe(context.schemaFileLocation), dstate.contextLocation, err.get)
      throw diag
    }

    dstate.setCurrentValue(ans.get)

  }

  def supportsParse: Boolean = true
  def supportsUnparse: Boolean = true

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)
}

class KeysetValueTypeCalculatorOrdered[A <: AnyRef, B <: AnyRef](valueMap: HashMap[A, B], rangeTable: Seq[(RangeBound[A], RangeBound[A], B)], unparseMap: HashMap[B, A],
  srcType: NodeInfo.Kind, dstType: NodeInfo.Kind)
  extends TypeCalculator[A, B](srcType, dstType) {

  override def inputTypeCalc(x: A, xType: NodeInfo.Kind): (Maybe[B], Maybe[Error]) = {
    if (valueMap.contains(x)) {
      (One(valueMap.get(x).get), Maybe.Nope)
    } else {
      val ans1: Option[(RangeBound[A], RangeBound[A], B)] = rangeTable.find({
        case (min, max, _) => {
          min.testAsLower(x) && max.testAsUpper(x)
        }
      })
      ans1 match {
        case None => {
          (Maybe.Nope, One(s"Key ${x} not found in keyset-value mapping"))
        }
        case Some((_, _, v)) => (Maybe(v), Maybe.Nope)
      }
    }
  }

  override def outputTypeCalc(x: B, xType: NodeInfo.Kind): (Maybe[A], Maybe[Error]) = {
    unparseMap.get(x) match {
      case Some(v) => (Maybe(v), Maybe.Nope)
      case None => (Maybe.Nope, One(s"Value ${x} not found in keyset-value mapping"))
    }
  }

}

class KeysetValueTypeCalculatorUnordered[A <: AnyRef, B <: AnyRef](valueMap: HashMap[A, B], unparseMap: HashMap[B, A], srcType: NodeInfo.Kind, dstType: NodeInfo.Kind)
  extends TypeCalculator[A, B](srcType, dstType) {

  override def inputTypeCalc(x: A, xType: NodeInfo.Kind): (Maybe[B], Maybe[Error]) = {
    if (valueMap.contains(x)) {
      valueMap.get(x) match {
        case Some(a) => (Maybe(a), Maybe.Nope)
        case None => (Maybe.Nope, One(s"Value ${x} not found in keyset-value mapping"))
      }
    } else {
      (Maybe.Nope, One(s"Key ${x} not found in keyset-value mapping"))

    }
  }
  override def outputTypeCalc(x: B, xType: NodeInfo.Kind): (Maybe[A], Maybe[Error]) = {
    unparseMap.get(x) match {
      case Some(v) => (Maybe(v), Maybe.Nope)
      case None => (Maybe.Nope, One(s"Value ${x} not found in keyset-value mapping"))
    }
  }

}

class ExpressionTypeCalculator[A <: AnyRef, B <: AnyRef](
  @TransientParam maybeInputTypeCalcArg: => Maybe[CompiledExpression[B]],
  @TransientParam maybeOutputTypeCalcArg: => Maybe[CompiledExpression[A]],
  srcType: NodeInfo.Kind, dstType: NodeInfo.Kind,
  override val supportsParse: Boolean, override val supportsUnparse: Boolean)
  extends TypeCalculator[A, B](srcType, dstType) {

  /*
   * Compiling DPath expressions may need to evaluate typeCalculators in order to lookup their srcType and dstType.
   * To prevent circular dependencies, this means that the underlying expressions must be lazy.
   *
   * Since these fields must be lazy, we cannot use them to determine supportsParse or supportUnparse
   */

  lazy val maybeInputTypeCalc = maybeInputTypeCalcArg
  lazy val maybeOutputTypeCalc = maybeOutputTypeCalcArg

  override protected def preSerialization: Any = {
    super.preSerialization
    maybeInputTypeCalc
    maybeOutputTypeCalc
  }

  //The class TypeValueCalc will verify that supports(Un)Parse is true when nessasary
  //Therefore, if we ever call the below functions, we know that the relevent Maybe object is defined.

  //TODO, pass x into DPath state

  override def inputTypeCalc(x: A, xType: NodeInfo.Kind): (Maybe[B], Maybe[Error]) =
    Assert.invariantFailed("inputTypeCalc not implemented on ExpressionTypeCalculator. Call the more specialized forms directly")
  override def outputTypeCalc(x: B, xType: NodeInfo.Kind): (Maybe[A], Maybe[Error]) =
    Assert.invariantFailed("outputTypeCalc not implemented on ExpressionTypeCalculator. Call the more specialized forms directly")

  override def inputTypeCalcParse(state: PState, context: RuntimeData, x: A, xType: NodeInfo.Kind): Maybe[B] = {
    val dstate = state.dState
    val oldRepValue = dstate.repValue
    val oldLogicalValue = dstate.logicalValue
    dstate.repValue = One((x, xType))
    dstate.logicalValue = Maybe.Nope

    val ans = Maybe(maybeInputTypeCalc.get.evaluate(state))

    dstate.repValue = oldRepValue
    dstate.logicalValue = oldLogicalValue
    ans
  }
  override def outputTypeCalcUnparse(state: UState, context: RuntimeData, x: B, xType: NodeInfo.Kind): Maybe[A] = {
    val dstate = state.dState
    val oldRepValue = dstate.repValue
    val oldLogicalValue = dstate.logicalValue
    dstate.repValue = Maybe.Nope
    dstate.logicalValue = One((x, xType))

    val ans = Maybe(maybeOutputTypeCalc.get.evaluate(state))

    dstate.repValue = oldRepValue
    dstate.logicalValue = oldLogicalValue
    ans
  }

  override def inputTypeCalcRun(dstate: DState, x: A, xType: NodeInfo.Kind): Unit = {
    val oldRepValue = dstate.repValue
    val oldLogicalValue = dstate.logicalValue
    dstate.repValue = One((x, xType))
    dstate.logicalValue = Maybe.Nope

    maybeInputTypeCalc.get.run(dstate)

    dstate.repValue = oldRepValue
    dstate.logicalValue = oldLogicalValue

  }
  override def outputTypeCalcRun(dstate: DState, x: B, xType: NodeInfo.Kind): Unit = {
    val oldRepValue = dstate.repValue
    val oldLogicalValue = dstate.logicalValue
    dstate.repValue = Maybe.Nope
    dstate.logicalValue = One((x, xType))

    maybeOutputTypeCalc.get.run(dstate)

    dstate.repValue = oldRepValue
    dstate.logicalValue = oldLogicalValue
  }
}

class IdentifyTypeCalculator[A <: AnyRef](srcType: NodeInfo.Kind) extends TypeCalculator[A, A](srcType, srcType) {
  override def inputTypeCalc(x: A, xType: NodeInfo.Kind): (Maybe[A], Maybe[Error]) = (Maybe(x), Maybe.Nope)
  override def outputTypeCalc(x: A, xType: NodeInfo.Kind): (Maybe[A], Maybe[Error]) = (Maybe(x), Maybe.Nope)
}

class UnionTypeCalculator[A <: AnyRef, B <: AnyRef](subCalculators: Seq[(RepValueSet[A], RepValueSet[B], TypeCalculator[A, B])], srcType: NodeInfo.Kind, dstType: NodeInfo.Kind)
  extends TypeCalculator[A, B](srcType, dstType) {
  //TODO, it may be worth it to pre-compute a hash table for direct dispatch,
  //Similar to how keyset-value works
  override def inputTypeCalc(x: A, xType: NodeInfo.Kind): (Maybe[B], Maybe[Error]) = {
    val subCalcSeq = subCalculators.filter(sub => sub._1.contains(x))
    Assert.invariant(subCalcSeq.length <= 1)
    if (subCalcSeq.isEmpty) {
      (Maybe.Nope, One(s"Key ${x} does not match any component of this simpleType union"))
    } else {
      val subCalc = subCalcSeq.head._3
      subCalc.inputTypeCalc(x, xType)
    }
  }

  override def outputTypeCalc(x: B, xType: NodeInfo.Kind): (Maybe[A], Maybe[Error]) = {
    val subCalcSeq = subCalculators.filter(sub => sub._2.contains(x))
    Assert.invariant(subCalcSeq.length <= 1)
    if (subCalcSeq.isEmpty) {
      (Maybe.Nope, One(s"Key ${x} does not match the logical values from any component of this union."))
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
class RepValueSet[A <: AnyRef](val valueSet: HashSet[A], val valueRanges: Set[(RangeBound[A], RangeBound[A])]) extends Serializable {
  def contains(x: A): Boolean = {
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

  def merge(other: RepValueSet[A]): RepValueSet[A] = {
    val valueSet_ = valueSet ++ other.valueSet
    val valueRanges_ = valueRanges ++ other.valueRanges
    new RepValueSet(valueSet_, valueRanges_)
  }

  lazy val isEmpty: Boolean = valueSet.isEmpty && valueRanges.isEmpty

}

//TODO, many of the key/values we receive will be BigInt.
//We should check if we can safely convert them to Long

object RepValueSetCompiler {
  def compile[A <: AnyRef](valueSet: Seq[A], valuesRanges: Seq[(RangeBound[A], RangeBound[A])]): RepValueSet[A] = {
    val hashSet = HashSet.empty ++ valueSet
    val rangeSet = Set.empty ++ valuesRanges.filter(x => x._1.isDefined || x._2.isDefined)
    new RepValueSet(hashSet, rangeSet)
  }
  def empty[A <: AnyRef]: RepValueSet[A] = compile(Seq.empty, Seq.empty)
}

object TypeCalculatorCompiler {

  type TypeCalcMap = Map[GlobalQName, TypeCalculator[AnyRef, AnyRef]]

  // mappings: [(keySet, canonicalKey, value)]
  def compileKeysetValue[A <: AnyRef, B <: AnyRef](mappings: Seq[(RepValueSet[A], A, B)], srcType: NodeInfo.Kind, dstType: NodeInfo.Kind): TypeCalculator[A, B] = {
    Assert.invariant(!mappings.isEmpty)

    /*
     * We need to cast to HashMap, because the type of HashMap.++ returns a generic Map
     * HashMap.+ returns a HashMap, so we can avoid the case by doing the fold ourself
     */
    val valueMap: HashMap[A, B] = (HashMap.empty ++ mappings.flatMap(x => {
      val (keySet, _, value) = x
      keySet.valueSet.map((_, value))
    })).asInstanceOf[HashMap[A, B]]
    val rangeTable: Seq[(RangeBound[A], RangeBound[A], B)] = mappings.flatMap(x => {
      val (keySet, _, value) = x
      keySet.valueRanges.map({ case (min, max) => (min, max, value) })
    })
    val unparseMap: HashMap[B, A] = (HashMap.empty ++ mappings.map(x => {
      val (_, canonicalKey, value) = x
      (value, canonicalKey)
    })).asInstanceOf[HashMap[B, A]]

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
  def compileTypeCalculatorFromExpression[A <: AnyRef, B <: AnyRef](
    optInputTypeCalc: => Option[CompiledExpression[B]],
    optOutputTypeCalc: => Option[CompiledExpression[A]],
    srcType: NodeInfo.Kind, dstType: NodeInfo.Kind,
    supportsParse: Boolean, supportsUnparse: Boolean): ExpressionTypeCalculator[A, B] = {
    lazy val maybeInputType: Maybe[CompiledExpression[B]] = optInputTypeCalc.map(Maybe(_)).getOrElse(Maybe.Nope)
    lazy val maybeOutputType: Maybe[CompiledExpression[A]] = optOutputTypeCalc.map(Maybe(_)).getOrElse(Maybe.Nope)
    new ExpressionTypeCalculator(maybeInputType, maybeOutputType, srcType, dstType, supportsParse, supportsUnparse)
  }
  def compileIdentity[A <: AnyRef](srcType: NodeInfo.Kind): TypeCalculator[A, A] = new IdentifyTypeCalculator(srcType)

  //subCalculators: Seq[(repValues, logicalValues, subCalc)]
  def compileUnion[A <: AnyRef, B <: AnyRef](subCalculators: Seq[(RepValueSet[A], RepValueSet[B], TypeCalculator[A, B])]): TypeCalculator[A, B] = {
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
