/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.processors

import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.cookers.Converter
import org.apache.daffodil.xml.NamedQName
import org.apache.daffodil.xml.GlobalQName
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.exceptions.Assert
import scala.collection.JavaConversions._
import org.apache.daffodil.xml.scalaLib
import org.apache.daffodil.dpath.ExpressionEvaluationException
import org.apache.daffodil.xml._
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.dpath.EvalMode
import org.apache.daffodil.dpath.UnparserNonBlocking
import org.apache.daffodil.util.MStackOfMaybe
import org.apache.daffodil.dsom.ContentValueReferencedElementInfoMixin
import org.apache.daffodil.infoset._
import org.apache.daffodil.processors.parsers.DoSDEMixin
import org.apache.daffodil.processors.parsers.PState

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

/**
 * Potentially runtime-evaluated things are instances of this base.
 */

trait EvaluatableBase[+T <: AnyRef] extends Serializable {

  protected def rd: RuntimeData

  /**
   * Please override this.
   */
  def toBriefXML(depth: Int = -1) = toString

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
  lazy val qName: NamedQName = {
    val local = Misc.toInitialLowerCaseUnlessAllUpperCase(
      Misc.stripSuffix(Misc.getNameFromClass(this), "Ev"))
    GlobalQName(None, local, NoNamespace)
  }

  type State = ParseOrUnparseState

  /**
   * Important - Evalutables MUST declare all evaluatables they depend on. But only those
   * they directly depend on. (Not the dependents of the dependents...)
   */
  def runtimeDependencies: Seq[EvaluatableBase[AnyRef]]

  /**
   * The compute method is called at compilation time to determine if a static value can be returned.
   * If so the Ev will store and return that value every time evaluate is called. If not then this
   * will get called at runtime to compute the value.
   */
  protected def compute(state: State): T

  /**
   * The "apply" method is the "evaluate" routine. This and isConstant are the public operations on
   * this class.
   */
  def apply(state: State): T

  /**
   * The apply method is the "evaluate" function.
   */
  final def evaluate(state: State): T = apply(state)

  /**
   * If the object has been compiled, was it a constant or not?
   */
  def isConstant: Boolean

  def compile(): Option[T] = {
    val compState = new CompileState(rd, Nope)
    compile(compState)
  }
  /**
   * Been compiled yet?
   */
  var isCompiled = false
  final def ensureCompiled {
    if (!isCompiled)
      Assert.invariantFailed("not compiled Ev: " + this.qName)
  }

  /**
   * returns None if the value is not constant at compile time,
   * otherwise returns the constant value.
   */
  def compile(state: CompileState): Option[T] = {
    if (isCompiled) throw new IllegalStateException("already compiled")

    // must assign before call to apply since that requires it to be compiled
    // (the first evaluation is compilation)
    isCompiled = true

    //
    // detonation chamber. Evaluate it. Did it blow up because it needs
    // data, not just static information, to succeed?
    //
    val result = try {
      val v = apply(state)
      Some(v)
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
      case e: ExpressionEvaluationException => None
      case e: InfosetException => None
      case e: VariableException => None
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
}

trait InfosetCachedEvaluatable[T <: AnyRef] { self: Evaluatable[T] =>

  protected def getCachedOrComputeAndCache(state: State): T = {
    self.rd match {
      case erd: ElementRuntimeData if erd.outputValueCalcExpr.isDefined => {
        // we only care a about caching when an evaluatable is part of an
        // outputValueCalc since we need to maintain the state. Otherwise, it
        // is more memory efficient to just recalculate any evaluatables
        Assert.invariant(state.currentNode.isDefined)
        val cn = state.infoset
        val termNode = cn match {
          case t: DITerm => t
          case _ => Assert.invariantFailed("current node was not a term.")
        }
        val cache = termNode.evalCache(state)
        val optHit = cache.get(this)
        if (optHit.isDefined) optHit.get.asInstanceOf[T]
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

  def newCache(state: State) {
    getCacheStack(state).push(Nope)
  }

  def invalidateCache(state: State) {
    getCacheStack(state).pop
  }
}

trait NoCacheEvaluatable[T <: AnyRef] { self: Evaluatable[T] =>
  protected def getCachedOrComputeAndCache(state: State): T = {
    compute(state)
  }
}

/**
 * Evaluatable - things that could be runtime-valued, but also could be compile-time constants
 * are instances of Ev.
 */
abstract class Evaluatable[+T <: AnyRef](protected val rd: RuntimeData, qNameArg: NamedQName = null) extends EvaluatableBase[T] {

  /**
   * Override if this evaluatable needs to use a different evaluate mode
   * for unparsing.
   *
   * The only example of this (as of this comment being written) is LengthEv
   */
  protected def maybeUseUnparserMode: Maybe[EvalMode] = Maybe(UnparserNonBlocking)

  override def toString = "(%s@%x, %s)".format(qName, this.hashCode(), (if (constValue.isDefined) "constant: " + constValue.value else "runtime"))

  override def toBriefXML(depth: Int = -1): String = if (constValue.isDefined) constValue.value.toString else super.toString

  override lazy val qName = if (qNameArg eq null) dafName(Misc.getNameFromClass(this)) else qNameArg

  protected def getCachedOrComputeAndCache(state: State): T

  final override def apply(state: State): T = {
    ensureCompiled

    if (isConstant) constValue.get
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

  private var constValue_ : Option[AnyRef] = None
  protected def constValue = constValue_.asInstanceOf[Option[T]]

  def optConstant = {
    ensureCompiled
    constValue
  }

  def isConstant = {
    ensureCompiled
    constValue.isDefined
  }

  /**
   * Compile an Ev to see if it is a constant at compilation time.
   * This is determined by actually evaluating it, passing a special CompileState
   * that errors out when data access to runtime-valued data is attempted.
   */
  final override def compile(state: CompileState) = {
    val y = super.compile(state)
    constValue_ = y
    y
  }

  /**
   * Creates an XML-like string representation.
   *
   * Looks like an attribute definition, but the value part
   * may contain XML-element-like syntax. If so it is either escaped with
   * CDATA bracketing (preferred), or if there are quotes in it, then
   * it is escapified meaning &quot; &lt; etc.
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
    val stringValue = stringValueUnlimited.substring(0, math.min(60, stringValueUnlimited.length)) + truncated
    Assert.usage(!stringValue.contains("]]>"))
    val pseudoXMLStringValue =
      if (stringValue.contains("<") && !stringValue.contains("\"")) {
        // looks like XML with child elements or other things that begin with "<", but no quotes
        "<![CDATA[" + stringValue + "]]>"
      } else {
        scalaLib.Utility.escape(stringValue)
      }
    val res = pseudoAttributeName + "=\"" + pseudoXMLStringValue + "\""
    res
  }

  protected def toSDE(e: Diagnostic, state: ParseOrUnparseState) = {
    state.SDE(e)
  }
}

/**
 * Rv is a "runtime value" as in, it is *always* evaluated, never cached, never
 * even considered as a possibility for a constant.
 */

//abstract class Rv[+T <: AnyRef](rd: RuntimeData) extends EvaluatableBase[T](rd) {
//
//  final override def isConstant = false // this is always to be computed
//
//  final override def compile(state: CompileState) = {
//    //
//    // Just because this can't be constant ever, doesn't mean it doesn't
//    // call other things that might be folded into constants. So we have to call super.compile().
//    //
//    val y = super.compile(state)
//    if (y.isDefined) {
//      // println("Compiling Done for %s.".format(qName))
//    } else {
//      // interesting that we got a value back from compiling, but since this is a Rv we're going to
//      // disregard it anyway.
//    }
//    None // we never get a value back from compile().
//  }
//
//  final override def apply(state: State): T = {
//    if (!isCompiled) {
//      compileIt(state)
//    }
//    compute(state)
//  }
//}

final class EvalCache {
  private val ht = new java.util.LinkedHashMap[Evaluatable[AnyRef], AnyRef] // linked so we can depend on order in unit tests.

  def get[T <: AnyRef](ev: Evaluatable[T]): Maybe[T] = {
    val got = ht.get(ev)
    val res = Maybe.WithNulls.toMaybe[T](got)
    res
  }

  def put[T <: AnyRef](ev: Evaluatable[T], thing: T) {
    if (thing.isInstanceOf[DINode]) return // happens in the interactive debugger due to expression ".." being compiled & run.
    Assert.usage(!thing.isInstanceOf[DINode])
    Assert.usage(thing ne null)
    Assert.usage(ht.get(ev) eq null) // cannot be present already
    ht.put(ev, thing)
  }

  /**
   * Creates an XML-like string representation of each item in the cache.
   */
  def toPseudoXML(): String = {
    val strings = mapAsScalaMap(ht).map { case (k, _) => k.toPseudoXML(this) }
    val string = strings.mkString("\n")
    string
  }
}

trait ExprEvalMixin[T <: AnyRef]
  extends DoSDEMixin with ContentValueReferencedElementInfoMixin {

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
  override protected val expr: CompiledExpression[ExprType],
  rd: RuntimeData)
  extends Evaluatable[ExprType](rd)
  with ExprEvalMixin[ExprType] {

  override lazy val runtimeDependencies = Nil

  override final def toBriefXML(depth: Int = -1) = "<EvaluatableExpression eName='" + rd.diagnosticDebugName + "' expr=" + expr.toBriefXML() + " />"

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

  override lazy val runtimeDependencies = Nil

  override final def toBriefXML(depth: Int = -1) = if (this.isConstant) this.constValue.toString else expr.toBriefXML(depth)

  override protected def compute(state: ParseOrUnparseState): ConvertedType = {
    val expressionResult = eval(expr, state)
    val forUnparse = !state.isInstanceOf[PState]
    val converterResult = state match {
      //
      // API thought note: It was suggested that converter methods should take the state and do this dispatch themselves.
      // However, the converters are also used in many static-only situations where there is no state
      // to use. Seems easier to just separate here.
      //
      case cs: CompileState => converter.convertConstant(expressionResult, rd, forUnparse)
      case _ => converter.convertRuntime(expressionResult, rd, forUnparse)
    }
    converterResult
  }
}

abstract class EvaluatableConvertedExpression[ExprType <: AnyRef, +ConvertedType <: AnyRef](
  val expr: CompiledExpression[ExprType],
  val converter: Converter[ExprType, ConvertedType],
  rd: RuntimeData)
  extends Evaluatable[ConvertedType](rd)
  with EvaluatableConvertedExpressionMixin[ExprType, ConvertedType]

