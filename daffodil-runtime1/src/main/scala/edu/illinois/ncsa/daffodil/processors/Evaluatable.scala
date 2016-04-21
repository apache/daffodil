package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.dsom.Converter
import edu.illinois.ncsa.daffodil.xml.NamedQName
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.xml.scalaLib
import edu.illinois.ncsa.daffodil.dpath.ExpressionEvaluationException
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.processors.unparsers.UState

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
abstract class EvaluatableBase[+T <: AnyRef](trd: RuntimeData) extends Serializable {

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
    val compState = new CompileState(trd,
      Nope)
    compile(compState)
  }
  /**
   * Been compiled yet?
   */
  var isCompiled = false
  final def ensureCompiled {
    if (!isCompiled) compile() //throw new IllegalStateException("Not compiled")
  }

  /**
   * returns None if the value is not constant at compile time,
   * otherwise returns the constant value.
   */
  def compile(state: CompileState): Option[T] = {
    if (isCompiled) throw new IllegalStateException("already compiled")
    isCompiled = true // must assign before call to apply.
    // println("compiling " + this.qName)
    //
    // detonation chamber. Evaluate it. Did it blow up because it needs
    // data, not just static information, to succeed?
    //
    val x = try {
      apply(state)
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
      case e: ExpressionEvaluationException =>
        return None
      case e: InfosetException =>
        return None
      case e: VariableException =>
        return None
    }
    val y = Some(x.asInstanceOf[T])
    y
  }

  protected final def compileIt(state: State): Option[T] = {
    val res = state match {
      case cs: CompileState => compile(cs)
      case u: UState => {
        Assert.invariantFailed("Evalutable must be compiled at compile time. State indicates this is runtime.")
      }
      case p: PState => {
        // until such time as all parsers are converted to declare runtimeDependencies, this is going to fail
        // almost every parse. So we'll allow on-demand compile for now.
        Assert.invariantFailed("Evalutable " + this + " must be compiled at compile time. State indicates this is runtime.")
        //        (new Exception()).printStackTrace()
        //        compile()
      }
    }
    if (isConstant) res // propagate constant for compilation
    else
      None // not a constant
  }

  /**
   * Convenience method
   *
   * Use by way of
   *      override lazy val qName = dafName("foobar")
   */
  protected def dafName(local: String) = GlobalQName(Some("dafint"), local, XMLUtils.dafintURI)
}

/**
 * Evaluatable - things that could be runtime-valued, but also could be compile-time constants
 * are instances of Ev.
 */
abstract class Evaluatable[+T <: AnyRef](trd: RuntimeData, qNameArg: NamedQName = null) extends EvaluatableBase[T](trd) {

  override def toString = "(%s@%x, %s)".format(qName, this.hashCode(), (if (constValue.isDefined) "constant: " + constValue.value else "runtime"))

  def toBriefXML(depth: Int = -1): String = if (constValue.isDefined) constValue.value.toString else super.toString

  override lazy val qName = if (qNameArg eq null) dafName(Misc.getNameFromClass(this)) else qNameArg

  final override def apply(state: State): T = {
    if (!isCompiled) {
      //
      // Compiling the Evaluatable mutates state of the ERD/TRD objects which are shared across threads.
      //
      // In theory, all Evaluatables should be compiled by the compiler before the processors are handed off
      // but in case that doesn't happen, we can avoid difficult bugs by just insuring only one thread can
      // compile a Evaluatable.
      synchronized {
        if (!isCompiled) {
          //
          // Compile on-demand.
          //
          val v = compileIt(state)
          constValue_ = v
        }
      }
    }
    //
    // it's compiled. Is it already known to be a static constant
    //
    if (isConstant) constValue.get
    else {
      //
      // Have to evaluate.
      //
      // check cache, compute and cache if necessary
      //
      state match {
        case cs: CompileState => {
          //
          // we're compiling. If we get a value, it's our constant value.
          //
          val v = compute(cs)
          constValue_ = Some(v)
          v
        }
        case _ => {
          //
          // we're running. Check cache. If not then compute and cache it.
          //
          assert(state.currentNode.isDefined)
          val cn = state.currentNode.get
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
      }
    }
  }

  private var constValue_ : Option[AnyRef] = None
  protected def constValue = constValue_.asInstanceOf[Option[T]]

  def optConstant = { ensureCompiled; constValue }

  /**
   * Note that right now asking isConstant does't force compilation to happen. It just
   * checks and errors if not compiled.
   */
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

//abstract class Rv[+T <: AnyRef](trd: RuntimeData) extends EvaluatableBase[T](trd) {
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

/**
 * Use for expressions when what you want out really is just the string value
 * of the property.
 */
class EvaluatableExpression[+ExprType <: AnyRef](
  expr: CompiledExpression[ExprType],
  trd: RuntimeData)
  extends Evaluatable[ExprType](trd) {

  override lazy val runtimeDependencies = Nil

  override final def toBriefXML(depth: Int = -1) = if (this.isConstant) this.constValue.toString else expr.toBriefXML(depth)

  override protected def compute(state: ParseOrUnparseState): ExprType = {

    val expressionResult =
      try {
        expr.evaluate(state)
      } catch {
        case i: InfosetException => toSDE(i, state)
        case v: VariableException => toSDE(v, state)
      }
    expressionResult
  }

}

/**
 * Use for expressions that produce results which have to subsequently be
 * converted into say, an enum value (so that "bigEndian" becomes ByteOrder.BigEndian)
 * or that have to be interpreted as DFDL string literals * of various kinds.
 *
 * See the cookers for string literals such as TextStandardInfinityRepCooker.
 */
class EvaluatableConvertedExpression[+ExprType <: AnyRef, +ConvertedType <: AnyRef](
  expr: CompiledExpression[ExprType],
  converter: Converter[ExprType, ConvertedType],
  trd: TermRuntimeData)
  extends Evaluatable[ConvertedType](trd) {

  override lazy val runtimeDependencies = Nil

  override final def toBriefXML(depth: Int = -1) = if (this.isConstant) this.constValue.toString else expr.toBriefXML(depth)

  override protected def compute(state: ParseOrUnparseState): ConvertedType = {
    val expressionResult =
      try {
        expr.evaluate(state)
      } catch {
        case i: InfosetException => toSDE(i, state)
        case v: VariableException => toSDE(v, state)
      }
    val forUnparse = !state.isInstanceOf[PState]
    val converterResult = state match {
      //
      // API thought note: It was suggested that converter methods should take the state and do this dispatch themselves.
      // However, the converters are also used in many static-only situations where there is no state
      // to use. Seems easier to just separate here.
      //
      case cs: CompileState => converter.convertConstant(expressionResult, trd, forUnparse)
      case _ => converter.convertRuntime(expressionResult, trd, forUnparse)
    }
    converterResult
  }
}
