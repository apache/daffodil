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

import scala.jdk.CollectionConverters._

import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.cookers.Converter
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MStackOfMaybe
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.GlobalQName
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.EvalMode
import org.apache.daffodil.runtime1.dpath.ExpressionEvaluationException
import org.apache.daffodil.runtime1.dpath.UnparserNonBlocking
import org.apache.daffodil.runtime1.dsom.CompiledExpression
import org.apache.daffodil.runtime1.dsom.ContentValueReferencedElementInfoMixin
import org.apache.daffodil.runtime1.dsom.DPathCompileInfo
import org.apache.daffodil.runtime1.dsom.DPathElementCompileInfo
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors.parsers.DoSDEMixin
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.unparsers.UState

/**
 * Generates unique int for use as key into EvalCache
 */
object EvalCache {

  private var nextInt = 0

  /**
   * Create boxed integer key, to be used in hashtable lookups of EvalCache.
   *
   * This is for speed (no hash-code computation walking over the structure, boxing and unboxing, etc.)
   *
   * Basically, we bypass calls to hashCode and equals here.
   */
  def generateKey: Int = synchronized {
    val n = nextInt
    nextInt += 1
    n
  }
}

trait InfosetCachedEvaluatable[T <: AnyRef] { self: Evaluatable[T] =>

  protected def getCachedOrComputeAndCache(state: State): T = {
    ci match {
      case eci: DPathElementCompileInfo if eci.isOutputValueCalc => {
        // we only care a about caching when an evaluatable is part of an
        // outputValueCalc since we need to maintain the state. Otherwise, it
        // is more memory efficient to just recalculate any evaluatables
        Assert.invariant(state.currentNode.isDefined)
        val cn = state.infoset
        val termNode = cn.asInstanceOf[DITerm]
        val cache = termNode.evalCache(state)
        val optHit = cache.get(this)
        if (optHit.isDefined) optHit.get
        else {
          val v = compute(state)
          cache.put(this, v)
          v
        }
      }
      case _ => {
        compute(state)
      }
    }
  }
}

/**
 * This is a method of caching that requires the parsers to create new cache
 * slots, evaluate, then invalidate the cache slots when it goes out of scope.
 * This differs from the InfosetCachedEvaluatable, which caches values in the
 * infoset and relies on backtracking and removal of infoset nodes to
 * invalidate the cache.
 *
 * The Evaluatable implementing this must have an MStack stored in the PState
 * or UState, and getCacheStack will be used to retrieve that stack. Before
 * calling evaluate for the first time, the parser must call newCache. All
 * other calls to evaluate will use this evaluated value. When the evaluatable
 * goes out of scope, the parser must then invalidate this cache by calling
 * invalidateCace.
 *
 * The motivating use case for this is for DFAFieldEV and EscapeEschemeEV.
 * DFAField is dependent on the escape scheme. The logical way to handle is to
 * make the EscapeSchemeEv a runtime dependent on the DFAFieldEv. This is
 * convenient since if the EscapeSchemeEv is constant, we can calcuclate the
 * DFAFieldEv at compile time.
 *
 * However, say the EscapeSchemeEv is not constant. The problem is that it
 * needs to be evaluated in a different scope than when the DFAFieldEv is
 * evaluated. So this manual caching thing is a way to force evaluation of the
 * Ev at the appropriate time (via the DynamicEscapeSchemeParser), and cache it
 * in the PState. Then when the DFAField is evaluated it can use that
 * cached value. Note that the problem with storing the cached value in the
 * infoset (ala InfosetCachedEvaluatable) is that the current infoset element
 * may have changed in between the time the EscapeSchemeEv was evaluated and
 * the time the DFAField is evaluated. So if we cached on the infoset, when the
 * DFAField asks for the EscapeSchemeEv, it will look at the currentInfoset
 * cache and it wouldn't be there, and then evaluate the escape scheme again in
 * the wrong scope.
 */
trait ManuallyCachedEvaluatable[T <: AnyRef] { self: Evaluatable[T] =>

  protected def getCacheStack(state: State): MStackOfMaybe[T]

  protected def getCachedOrComputeAndCache(state: State): T = {
    val cs = getCacheStack(state)
    val manualCache = cs.top
    if (manualCache.isDefined) {
      manualCache.get
    } else {
      val v = compute(state)
      cs.pop
      cs.push(One(v))
      v
    }
  }

  def newCache(state: State): Unit = {
    getCacheStack(state).push(Nope)
  }

  def invalidateCache(state: State): Unit = {
    getCacheStack(state).pop
  }
}

trait NoCacheEvaluatable[T <: AnyRef] { self: Evaluatable[T] =>
  protected def getCachedOrComputeAndCache(state: State): T = {
    compute(state)
  }

  /**
   * A NoCacheEvaluatable also never compiles into a constant.
   */
  final override protected def isNeverConstant = true
}

/**
 * Evaluatable - things that could be runtime-valued, but also could be compile-time constants
 * are instances of Ev.
 */
abstract class Evaluatable[+T <: AnyRef](
  protected val ci: DPathCompileInfo,
  qNameArg: NamedQName = null
) extends Serializable {

  type State = ParseOrUnparseState

  /**
   * Important - Evalutables MUST declare all evaluatables they depend on. But only those
   * they directly depend on. (Not the dependents of the dependents...)
   */
  def runtimeDependencies: Seq[Evaluatable[AnyRef]]

  /**
   * Been compiled yet?
   */
  private var isCompiled_ = false

  protected def isNeverConstant = false

  @inline final def isCompiled = isCompiled_

  @inline final def ensureCompiled(): Unit = {
    ci.initialize()
    if (!isCompiled)
      Assert.invariantFailed("not compiled Ev: " + this.qName)
  }

  /**
   * returns None if the value is not constant at compile time,
   * otherwise returns the constant value.
   */
  private def compileTimeEvaluate(state: CompileState): Maybe[T] = {
    if (isCompiled) throw new IllegalStateException("already compiled")

    // must assign before call to apply since that requires it to be compiled
    // (the first evaluation is compilation)
    isCompiled_ = true

    //
    // detonation chamber. Evaluate it. Did it blow up because it needs
    // data, not just static information, to succeed?
    //
    val result =
      try {
        val v = evaluate(state)
        One(v)
      } catch {
        //
        // Really what this should be catching are the special-purpose exceptions thrown
        // by the Infoset data-accessing API to indicate no data being found.
        //
        // However, for unparsing outputValueCalc this needs to distinguish whether we've determined
        // that the OVC expression is non-constant, from a failure at runtime.
        //
        // So non-constant is detected based on it failing by accessing either data or the infoset
        // when evaluated with a fake state that has neither. But SDE means there is something wrong
        // with the expression, so we can't absorb those.
        //
        // Thrown if we're trying to navigate from parent to child and the child doesn't exist.
        // or there is no data, etc.
        case _: ExpressionEvaluationException => Nope
        case _: InfosetException => Nope
        case _: VariableException => Nope
      }
    result
  }

  /**
   * Convenience method
   *
   * Use by way of
   *      override lazy val qName = dafName("foobar")
   */
  protected def dafName(local: String) = GlobalQName(Some("dafint"), local, XMLUtils.dafintURI)

  /**
   * Override if this evaluatable needs to use a different evaluate mode
   * for unparsing.
   *
   * The only example of this (as of this comment being written) is LengthEv
   */
  protected def maybeUseUnparserMode: Maybe[EvalMode] = Maybe(UnparserNonBlocking)

  override def toString = "(%s@%x, %s)".format(
    qName,
    this.hashCode(),
    (if (isConstant) "constant: " + constValue else "runtime")
  )

  def toBriefXML(depth: Int = -1): String = if (isConstant) constValue.toString else toString

  /**
   * QName to be associated with this evaluatable.
   *
   * If the Evaluatable is for a DFDL property this should be dfdl:propertyName e.g., dfdl:terminator, with
   * the prefix DFDL being the prefix associated with the DFDL namespace.
   *
   * If the Evaluatable is for some other computation, e.g., for the CharsetEncoder. Then daf:encoder or other
   * useful identifier.
   *
   * This QName is used in the XML representation of these Evaluatables, if they are to be displayed, which is
   * mostly for various debug purposes.
   */
  lazy val qName = if (qNameArg eq null) dafName(Misc.getNameFromClass(this)) else qNameArg

  protected def getCachedOrComputeAndCache(state: State): T

  protected def compute(state: State): T

  final def evaluate(state: State): T = {
    if (isConstant) constValue
    else {
      state match {
        case _: CompileState => {
          // Do not worry about caching at compile time. Just try to compute it
          // to see if it is constant. If it isn't constant, something will
          // throw. If it is constant, compilation will store the value in the
          // Evaluatable and the cache will never be used at runtime.
          compute(state)
        }
        case _ => {
          getCachedOrComputeAndCache(state)
        }
      }
    }
  }

  private var constValue_ : Maybe[AnyRef] = Nope

  /**
   * Preferred for use in the runtime.
   */
  @inline final def maybeConstant = Maybe.fromMaybeAnyRef[T](constValue_)
  @inline final def isConstant = constValue_.isDefined
  @inline final def constValue = maybeConstant.get

  /**
   * Schema compiler wants to use map call, so we need a scala option type
   * for that. So this variant supplies that.
   */
  @inline final def optConstant = {
    maybeConstant.toOption
  }

  /**
   * Compile an Ev to see if it is a constant at compilation time.
   * This is determined by actually evaluating it, passing a special CompileState
   * that errors out when data access to runtime-valued data is attempted.
   */
  final def compile(state: CompileState): Maybe[T] = {
    if (isNeverConstant) {
      isCompiled_ = true
      Nope
    } else {
      val y = compileTimeEvaluate(state)
      // just by getting here - and not throwing, we know it's a constant.
      constValue_ = y
      y
    }
  }

  final def compile(tunable: DaffodilTunables): Maybe[T] = {
    val compState = new CompileState(ci, Nope, tunable)
    compile(compState)
  }

  /**
   * Creates an XML-like string representation.
   *
   * Looks like an attribute definition, but the value part
   * may contain XML-element-like syntax. If so it is either escaped with
   * CDATA bracketing (preferred), or if there are quotes in it, then
   * it is escapified meaning &amp;quot; &amp;lt; etc.
   *
   * Avoids inserting single quotes (aka apos) so that those can be used
   * surrounding this at a higher level.
   */
  def toPseudoXML(cache: EvalCache): String = {
    val pseudoAttributeName = qName.toAttributeNameString
    val thing = cache.get(this)
    val it = if (thing.isDefined) thing.value else "Nope"
    val stringValueUnlimited = XMLUtils.remapXMLIllegalCharactersToPUA(it.toString())
    val truncated = if (stringValueUnlimited.length > 60) "...(truncated)" else ""
    val stringValue =
      stringValueUnlimited.substring(0, math.min(60, stringValueUnlimited.length)) + truncated
    Assert.usage(!stringValue.contains("]]>"))
    val pseudoXMLStringValue =
      if (stringValue.contains("<") && !stringValue.contains("\"")) {
        // looks like XML with child elements or other things that begin with "<", but no quotes
        "<![CDATA[" + stringValue + "]]>"
      } else {
        scala.xml.Utility.escape(stringValue)
      }
    val res = pseudoAttributeName + "=\"" + pseudoXMLStringValue + "\""
    res
  }

  protected def toSDE(e: Diagnostic, state: ParseOrUnparseState) = {
    state.SDE(e)
  }
}

final class EvalCache {
  private val ht =
    new java.util.LinkedHashMap[Evaluatable[
      AnyRef
    ], AnyRef] // linked so we can depend on order in unit tests.

  def get[T <: AnyRef](ev: Evaluatable[T]): Maybe[T] = {
    val got = ht.get(ev)
    val res = Maybe.WithNulls.toMaybe[T](got)
    res
  }

  def put[T <: AnyRef](ev: Evaluatable[T], thing: T): Unit = {
    if (thing.isInstanceOf[DINode])
      return // happens in the interactive debugger due to expression ".." being compiled & run.
    Assert.usage(!thing.isInstanceOf[DINode])
    Assert.usage(thing ne null)
    Assert.usage(ht.get(ev) eq null) // cannot be present already
    ht.put(ev, thing)
  }

  /**
   * Creates an XML-like string representation of each item in the cache.
   */
  def toPseudoXML(): String = {
    val strings = ht.asScala.map { case (k, _) => k.toPseudoXML(this) }
    val string = strings.mkString("\n")
    string
  }
}

trait ExprEvalMixin[T <: AnyRef]
  extends DoSDEMixin
  with ContentValueReferencedElementInfoMixin {

  protected def expr: CompiledExpression[T]

  protected def maybeUseUnparserMode: Maybe[EvalMode]

  final protected def eval(expr: CompiledExpression[T], state: ParseOrUnparseState): T = {
    val expressionResult: T =
      state match {
        case _: CompileState => expr.evaluate(state)
        case _: PState =>
          try {
            expr.evaluate(state)
          } catch {
            case vnv: VariableHasNoValue => doSDE(vnv, state)
          }
        case ustate: UState => {
          Assert.invariant(maybeUseUnparserMode.isDefined)
          val originalMode = ustate.dState.mode
          val unpMode = maybeUseUnparserMode.get
          try {
            ustate.dState.setMode(unpMode)
            expr.evaluate(state)
          } catch {
            case vnv: VariableHasNoValue =>
              if (unpMode eq UnparserNonBlocking)
                doSDE(vnv, state)
              else
                throw vnv
          } finally {
            ustate.dState.setMode(originalMode)
          }
        }
      }
    DataValue.assertValueIsNotDataValue(expressionResult)
    expressionResult
  }

  final def contentReferencedElementInfos = expr.contentReferencedElementInfos
  final def valueReferencedElementInfos = expr.valueReferencedElementInfos

}

/**
 * Use for expressions when what you want out really is just the string value
 * of the property.
 */
abstract class EvaluatableExpression[ExprType <: AnyRef](
  override val expr: CompiledExpression[ExprType],
  ci: DPathCompileInfo
) extends Evaluatable[ExprType](ci)
  with ExprEvalMixin[ExprType] {

  override def runtimeDependencies = Vector()

  override final def toBriefXML(depth: Int = -1) =
    "<EvaluatableExpression eName='" + ci.diagnosticDebugName + "' expr=" + expr
      .toBriefXML() + " />"

  override protected def compute(state: ParseOrUnparseState): ExprType = eval(expr, state)

}

/**
 * Use for expressions that produce results which have to subsequently be
 * converted into say, an enum value (so that "bigEndian" becomes ByteOrder.BigEndian)
 * or that have to be interpreted as DFDL string literals * of various kinds.
 *
 * See the cookers for string literals such as TextStandardInfinityRepCooker.
 */
trait EvaluatableConvertedExpressionMixin[ExprType <: AnyRef, +ConvertedType <: AnyRef]
  extends ExprEvalMixin[ExprType] { self: Evaluatable[ConvertedType] =>

  protected def converter: Converter[ExprType, ConvertedType]

  override def runtimeDependencies = Vector()

  override final def toBriefXML(depth: Int = -1) =
    if (this.isConstant) this.constValue.toString else expr.toBriefXML(depth)

  override protected def compute(state: ParseOrUnparseState): ConvertedType = {
    val expressionResult = eval(expr, state)
    val forUnparse = !state.isInstanceOf[PState]
    val converterResult = state match {
      //
      // API thought note: It was suggested that converter methods should take the state and do this dispatch themselves.
      // However, the converters are also used in many static-only situations where there is no state
      // to use. Seems easier to just separate here.
      //
      case cs: CompileState => converter.convertConstant(expressionResult, ci, forUnparse)
      case _ => converter.convertRuntime(expressionResult, ci, forUnparse)
    }
    converterResult
  }
}

abstract class EvaluatableConvertedExpression[ExprType <: AnyRef, +ConvertedType <: AnyRef](
  val expr: CompiledExpression[ExprType],
  val converter: Converter[ExprType, ConvertedType],
  ci: DPathCompileInfo
) extends Evaluatable[ConvertedType](ci)
  with EvaluatableConvertedExpressionMixin[ExprType, ConvertedType]
