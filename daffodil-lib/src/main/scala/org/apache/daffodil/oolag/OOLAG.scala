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

package org.apache.daffodil.oolag

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Logging
import org.apache.daffodil.util._
import org.apache.daffodil.exceptions.UnsuppressableException
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.api.WithDiagnostics
import org.apache.daffodil.util.Misc
import org.apache.daffodil.exceptions.ThinThrowable
import org.apache.daffodil.util.Maybe._

/**
 * OOLAG = Object-oriented Lazy Attribute Grammars
 *
 * It's a collection of techniques for compilation/transformation
 * in a functional programming style. You can find an article about
 * Attribute Grammars on Wikipedia.
 */

object OOLAG extends Logging {

  private val indent_ = new ThreadLocal[Int] {
    override def initialValue = 0
  }
  final def indent = indent_.get()
  final def setIndent(i: Int) = indent_.set(i)

  /**
   * For when we can continue to gather more errors after one error
   * has occurred.
   *
   * The point of keepGoing: users don't have to build try/catches or
   * know about which exceptions being thrown are ones that should or
   * should not be caught. If the body fails, the alt is computed and
   * returned.
   *
   * If body fails with an OOLAG exception of the right kind, then
   * alt is evaluated and returned instead.
   */
  def keepGoing[T](alt: => T)(body: => T) = {
    try {
      body
    } catch {
      // We suppress errors here because a rethrow indicates that somebody else
      // has already recorded the exception in the diagnostics, and it was of the
      // kind that can be recorded and issued later as a compile-time diagnostic.
      case e: OOLAGRethrowException =>
        log(LogLevel.OOLAGDebug, "OOLAG.keepGoing is suppressing exception already recorded: %s", e)
        alt
    }
  }

  sealed abstract class Args
  case object OneArg extends Args
  case object ZeroArgs extends Args

  /**
   * Convenience class for implementing OOLAGHost trait.
   *
   *  Insures context is set by way of construction. Use the trait and
   *  implement the context yourself if you want to make use of setting the
   *  context after the objects are constructed.
   */
  abstract class OOLAGHostImpl private (
    oolagContextArg: OOLAGHost,
    final override val nArgs: Args)
    extends OOLAGHost {

    def this(oolagContext: OOLAGHost) = this(oolagContext, OneArg)
    def this() = this(null, ZeroArgs)

    final protected override def oolagContextViaArgs = {
      Option(oolagContextArg) // Option since Option(null) is None.
    }
  }

  /**
   * An OOLAGHost, or OOLAG for short, is a collection of OOLAGValues
   * or LVs for short.
   *
   * The way these are generally used now is like this
   * {{{
   *  def foo = LV('foo) {...calculation...}.value
   * }}}
   * or, if you would like a slot to show up in the debugger so you
   * can more easily see the value of the LV then you can
   * {{{
   * lazy val foo = LV('foo){...calculation...}.value
   * }}}
   * Why scala needs 'real' Lisp-style macros: Well wouldn't it be
   * nicer if I could write:
   * {{{
   *    defAttribute foo {... calculation ...}
   * }}}
   * and have that be equivalent the more verbose stuff above it?
   * But I digress...
   *
   * OOLAGHosts support a feature called requiredEvaluations.
   * This is a lazy function the arguments to which are only evaluated
   * when the isError summary operation is invoked.
   *
   * The requiredEvaluations function takes any number of arguments, they are
   * of type Any. Suppose we have the LV named 'foo' from above. If
   * we want to say that our particular OOLAGHost-derived class must
   * evaluate the 'foo' LV otherwise we don't know if it has errors or not,
   * then we write:
   * {{{
   *     requiredEvaluations(foo)
   * }}}
   * This goes at the top of the class definition. When isError is invoked
   * the value of 'foo' will be computed if it has not been attempted
   * already. This insures that the value exists for 'foo', or any errors/warnings
   * to be determined by its calculation have been recorded.
   */
  trait OOLAGHost
    extends Logging with WithDiagnostics
    with NamedMixinBase {

    protected def oolagContextViaArgs: Option[OOLAGHost] = None
    protected def nArgs: Args = OneArg

    private var oolagContextViaSet: Option[OOLAGHost] = None

    final def setOOLAGContext(oolagContextArg: OOLAGHost) {
      Assert.usage(nArgs == ZeroArgs, "Cannot set oolag context if it was provided as a constructor arg.")
      if (oolagContextViaSet != None)
        Assert.usageError("Cannot set oolag context more than once.")
      oolagContextViaSet = Option(oolagContextArg) // Option since Option(null) is None
      if (optOolagContext.isDefined) {
        oolagRoot.requiredEvalFunctions = this.requiredEvalFunctions ::: oolagRoot.requiredEvalFunctions
        this.requiredEvalFunctions = Nil
      }
    }

    final def hasOOLAGRootSetup: Boolean = {
      if (nArgs == OneArg) true
      else if (oolagContextViaSet.isDefined) true
      else
        false
    }

    /**
     * Used to check things that OOLAG doesn't protect against such as
     * index out of bounds.
     *
     * When we rationalized some of the try/catches so they catch less-broad
     * catagories of exceptions, well now some things are ambiguous failures.
     * If lazy val foo = schemas(0).schemaDocs(0) fails with index-out-of-range,
     * is that a programmer error, or just the computation failing because
     * there aren't any schemas, so the lazy val foo should fail, causing an
     * OOLAG capture of the error.
     *
     * In the past, OOLAG would have caught this indexing exception,
     * converted it to an SDE, and in the case where it was a programmer
     * mistake, it would have masked that mistake.
     *
     * Now we've flipped it. Now OOLAG won't catch this, but we need a way to
     * say explicitly, that we're assuming there are schemas, so schema(0)
     * should work, and if it doesn't, OOLAG should catch this and abandon
     * computing whatever attribute we were trying to compute, and put that
     * attribute in the 'already been tried' state.
     */
    final def assuming(pred: Boolean) = {
      if (!pred) throw AssumptionFailed
    }

    /**
     * Either we were called with a constructor arg that provides the oolagContext,
     * or we were constructed with no args, and in that case someone must call
     * setOOLAGContext before we access the oolagContext.
     */
    final lazy val optOolagContext = nArgs match {
      case ZeroArgs => {
        Assert.usage(oolagContextViaSet != None, "Must call setOOLAGContext before accessing when OOLAGHost is constructed with no args.")
        oolagContextViaSet
      }
      case OneArg => {
        val oc = oolagContextViaArgs
        Assert.invariant(oc.isEmpty || (oc.get ne null))
        oc
      }
    }

    final lazy val oolagContext = optOolagContext.get

    /**
     * My parent, unless I am the root and have no parent. In that
     * case this aborts with a usage error. Programmer should have
     * called isOOLAGRoot first.
     */
    protected final def oolagParent: OOLAGHost = {
      if (!isOOLAGRoot) oolagContext
      else Assert.usageError("parent of root OOLAGHost not allowed.")
    }

    /**
     * Take me to the root of this OOLAG nest.
     */
    private lazy val oolagRoot: OOLAGHost = {
      if (isOOLAGRoot) this
      else oolagContext.oolagRoot
    }

    private def isOOLAGRoot = {
      val res = optOolagContext.isEmpty
      res
    }

    lazy val path: String = {
      if (isOOLAGRoot) diagnosticDebugName else oolagParent.path + ":" + diagnosticDebugName
    }

    /**
     * Detect circular relationships among LVs.
     */

    // I don't like this var for all those thread-safety reasons, but otherwise we have to
    // carry a context list with us throughout the computation

    final var currentOVList: Seq[OOLAGValueBase] = Nil

    private val lvCache = new scala.collection.mutable.LinkedHashMap[Symbol, OOLAGValueBase]

    /**
     * The factory for OOLAGValues is LV.
     */
    protected final def LV[T](sym: Symbol)(body: => T): OOLAGValue[T] = {
      lvCache.get(sym).getOrElse {
        val lv = new OOLAGValue[T](this, sym.name, body)
        lvCache.put(sym, lv)
        lv
      }.asInstanceOf[OOLAGValue[T]]
    }

    /**
     * requiredEvaluations feature
     */
    private var requiredEvalCount = 0
    private val requiredEvalName = Misc.getNameFromClass(this) + "_requiredEvaluation_"

    private def accumulationPoint =
      if (this.hasOOLAGRootSetup) oolagRoot
      else this

    protected final def requiredEvaluations(arg: => Any) {
      val lv = LV(Symbol(requiredEvalName + requiredEvalCount)) {
        arg
      }
      requiredEvalCount += 1
      accumulationPoint.requiredEvalFunctions +:= lv
    }

    protected final def requiredEvaluations(lv: OOLAGValueBase) {
      accumulationPoint.requiredEvalFunctions +:= lv
    }

    private var requiredEvalFunctions: List[OOLAGValueBase] = Nil

    final def checkErrors: Unit = {
      Assert.usage(this.isOOLAGRoot || requiredEvalFunctions == Nil)
      while (oolagRoot.requiredEvalFunctions != Nil) {
        var evFuncs = oolagRoot.requiredEvalFunctions
        oolagRoot.requiredEvalFunctions = Nil
        while (evFuncs != Nil) {
          val lv = evFuncs.head
          evFuncs = evFuncs.tail
          OOLAG.keepGoing { () } {
            lv.valueAsAny // useful place for a breakpoint
          }
        }
      }
    }

    /**
     * Accumulate diagnostics on the root of the hierarchy.
     * (Likely the SchemaSet object.)
     */

    //TODO: why store these on all the objects if the
    //only one where they are used is the root object.
    private var errors_ : Seq[Diagnostic] = Nil
    private var warnings_ : Seq[Diagnostic] = Nil

    final def errors = oolagRoot.errors_
    final def warnings = oolagRoot.warnings_

    def getDiagnostics = diagnostics

    def warn(th: Diagnostic): Unit = {
      oolagRoot.oolagWarn(th)
    }

    def error(th: Diagnostic): Unit = {
      oolagRoot.oolagError(th)
    }

    /**
     * Implementor of OOLAGHost constructs Diagnostic objects
     * because it has other context (schema components)
     * needed in order to provide a good diagnostic.
     *
     * So we have this abstract method that is implemented
     * by things that can actually construct Diagnostic
     * objects (Anything that ThrowsSDE)
     */

    protected def oolagWarn(th: Diagnostic) {
      if (!warnings_.contains(th))
        warnings_ :+= th
    }

    protected def oolagError(th: Diagnostic) {
      if (!errors_.contains(th))
        errors_ :+= th
    }

    /**
     * Currently we depend on being able to evaluate these
     * repeatedly, and get different answers.
     *
     * because it forces evaluation of all the requiredEvaluations(...)
     * on all objects first, but that is only for the objects
     * that have been created at the time this is called.
     */

    def isError = {
      oolagRoot.checkErrors
      val errorCount = oolagRoot.errors.length
      errorCount > 0
    }

    def diagnostics = errors ++ warnings
  }

  /**
   * An OOLAG value is what would be called an "attribute" in attribute-grammar terminology.
   * It is evaluated lazily, once, and either produces a value, or causes an error, or
   * both produces a value and also creates zero or more warnings.
   *
   * It detects evaluation more than once, and disallows multiple evaluations.
   * It detects and disallows cyclic definitions (oolag value defined in terms of itself).
   * This does not prevent recursive definitions (different objects, but same oolag value 'name'),
   * just cyclic loops (exact same oolag value object).
   *
   * An OOLAG value is created as a val of an OOLAGHost class.
   */
  sealed abstract class OOLAGValueBase(
    val oolagContext: OOLAGHost,
    nameArg: String,
    body: => Any)
    extends Logging {

    Assert.usage(oolagContext != null)

    final lazy val name = nameArg

    private var alreadyTriedThis = false
    protected final def hasValue: Boolean = value_.isDefined
    private var value_ : Maybe[AnyRef] = Nope

    protected final def wasTried = alreadyTriedThis

    // private def warn(th: Diagnostic): Unit = oolagContext.warn(th)
    private def error(th: Diagnostic): Unit = oolagContext.error(th)

    private def thisThing = this.name + valuePart + "@" + this.hashCode()

    private def valuePart = {
      if (hasValue) "(" + toStringIfSimpleEnough(value_.get) + ")"
      else ""
    }

    private def toStringIfSimpleEnough(x: Any): String = {
      x match {
        case None => "None"
        case Some(y) => "Some(" + toStringIfSimpleEnough(y) + ")"
        case s: String => "'" + s + "'"
        case av: Long => av.toString
        case av: Int => av.toString
        case av: Short => av.toString
        case av: Byte => av.toString
        case av: scala.xml.Node => av.toString
        case av: Boolean => av.toString
        case _ => Misc.getNameFromClass(x)
      }
    }

    override def toString = thisThing

    private val catchMsg = "%s has no value due to %s."

    protected final def toss(th: Throwable) = {
      throw th
    }

    protected final def oolagCatch(th: Throwable): Nothing = {
      Assert.invariant(!hasValue)
      th match {
        case le: scala.Error => { // note that Exception does NOT inherit from Error
          log(LogLevel.Error, " " * indent + catchMsg, thisThing, le) // tell us which lazy attribute it was
          toss(le)
        }
        //
        // Don't catch RuntimeException as this makes using the debugger
        // to isolate bugs harder. You just end up converting them into unsuppressible,
        // but with loss of context.
        //
        case ue: IllegalArgumentException => {
          val ex = ue
          log(LogLevel.OOLAGDebug, " " * indent + catchMsg, this.getClass.getName, ex) // tell us which lazy attribute it was
          toss(ex)
        }
        case ue: UnsuppressableException => {
          val ex = ue
          log(LogLevel.OOLAGDebug, " " * indent + catchMsg, this.getClass.getName, ex) // tell us which lazy attribute it was
          toss(ex)
        }
        //
        // These are OOLAGs own Throwables.
        // ErrorAlreadyHandled means we are headed back to some top-level that
        // can tolerate errors and go on with compilation.
        case eah: ErrorAlreadyHandled => {
          log(LogLevel.OOLAGDebug, " " * indent + catchMsg, thisThing, eah)
          toss(eah)
        }
        //
        // Already tried means we got to this same OOLAG value evaluation again,
        // and as values, they can't behave differently, so the same error will
        // just get reported again.
        case at: AlreadyTried => {
          log(LogLevel.OOLAGDebug, " " * indent + "Caught %s", at)
          toss(at)
        }
        case AssumptionFailed => {
          toss(AssumptionFailed)
        }
        case e: Diagnostic => {
          // we threw, instead of producing a value
          //
          // Typically this will be for a Schema Definition Error
          //
          Assert.invariant(hasValue == false)
          Assert.invariant(alreadyTriedThis == true)

          log(LogLevel.OOLAGDebug, " " * indent + catchMsg, thisThing, e)
          error(e)
          //
          // Catch this if you can carry on with more error gathering
          // from other contexts. Otherwise just let it propagate.
          //
          toss(new ErrorAlreadyHandled(e, this))
        }
        case e @ ErrorsNotYetRecorded(diags) => {
          diags.foreach { error(_) }
          toss(new AlreadyTried(this))
        }
        case th => toss(th)
      }
    }

    private def initialize = {
      val now = oolagContext.currentOVList
      oolagContext.currentOVList = this +: oolagContext.currentOVList
      // log(LogLevel.OOLAGDebug, " " * indent + "push: " + thisThing))
      setIndent(indent + 2)
      now
    }

    protected final def oolagBefore(): Unit = {
      Assert.invariant(!hasValue)
      if (initialize.contains(this)) {
        // System.err.println("Circular OOLAG Value Definition")
        // This next println was causing problems because toString was
        // itself causing circular evaluation. The abort above
        // System.err.println("OOLAGValues (aka 'LVs') on stack are: " + currentOVList.mkString(", "))
        val c = CircularDefinition(this, oolagContext.currentOVList)
        log(LogLevel.OOLAGDebug, " " * indent + "LV: " + thisThing + " CIRCULAR")
        toss(c)
      }
      if (alreadyTriedThis) {
        log(LogLevel.OOLAGDebug, " " * indent + "LV: %s was tried and failed", thisThing)
        val e = AlreadyTried(this)
        toss(e)
      }
      alreadyTriedThis = true
      log(LogLevel.OOLAGDebug, " " * indent + "Evaluating %s", thisThing)
    }

    protected final def oolagAfterValue(res: AnyRef) {
      log(LogLevel.OOLAGDebug, " " * indent + "Evaluated %s", thisThing)
      value_ = Maybe(res)
    }

    protected final def oolagFinalize = {
      setIndent(indent - 2)
      log(LogLevel.OOLAGDebug, " " * indent + "pop:  " + thisThing)
      if (oolagContext.currentOVList.nonEmpty)
        oolagContext.currentOVList = oolagContext.currentOVList.tail
    }

    final def hasError = alreadyTriedThis && !hasValue

    /**
     * forces the value, then boolean result tells you if
     * a value was computed or errors, 1 or more, occurred
     * which prevent a value from being computed.
     */

    final def isError = {
      val res =
        if (alreadyTriedThis) !hasValue
        else {
          try {
            valueAsAny
            !hasValue
          } catch {
            case oe: OOLAGRethrowException => true
          }
        }
      if (res == true) {
        log(LogLevel.OOLAGDebug, " " * indent + "LV %s has an error", this)
      }
      res
    }

    final lazy val valueAsAny: Any = {
      if (hasValue) value_.get
      val res =
        try {
          oolagBefore
          val v = body // good place for a breakpoint
          oolagAfterValue(v.asInstanceOf[AnyRef])
          v
        } catch {
          case npe: NullPointerException => throw npe
          case s: scala.util.control.ControlThrowable => throw s
          case u: UnsuppressableException => throw u
          case e: Error => throw e
          case th: Throwable => oolagCatch(th)
        } finally {
          oolagFinalize
        }
      res
    }

    protected lazy val toOptionAny: Option[Any] = {
      if (wasTried) {
        if (hasValue) Some(value_.get)
        else None
      } else {
        val res = try {
          val v = valueAsAny
          Some(v)
        } catch {
          case e: OOLAGRethrowException => None
        }
        res
      }
    }
  }

  final class OOLAGValue[T](ctxt: OOLAGHost, nameArg: String, body: => T)
    extends OOLAGValueBase(ctxt, nameArg, body) {

    /**
     * Converts LV into an option type.
     *
     * If the logical value of the LV is already an option type,
     * keep in mind you have to flatten to get back to an Option\[T\].
     *
     * Use this if you are getting stack-overflows or circular value problems
     * caused by an SDE and the SDE infrastructure needing the value of some LV.
     */
    final lazy val toOption: Option[T] = toOptionAny.asInstanceOf[Option[T]]

    final lazy val value: T = valueAsAny.asInstanceOf[T]
  }
} // end object

/**
 * These exception types are all private because they are supposed
 * to be caught by wrappers defined here and contained by them.
 *
 * If they get thrown all the way to top level it's a bug.
 * If code someplace needs to see/understand them, then it
 * either needs to use a wrapper from here (e.g., see keepGoing(){})
 * or we need to evolve a new wrapper.
 */
// allow outside world to see this base, for maintainability catches
//
private[oolag] trait OOLAGException extends Exception with ThinThrowable {
  def lv: OOLAG.OOLAGValueBase
}

private[oolag] trait OOLAGRethrowException extends OOLAGException {
  def cause1: Option[Throwable]
}

/**
 * Used to carry diagnostics from one oolag context (DPath expression compiling)
 * to another (Schema Components). You throw this bag of diagnostics, and they
 * get recorded individually on the enclosing oolag context.
 */
case class ErrorsNotYetRecorded(diags: Seq[Diagnostic]) extends OOLAGRethrowException {
  Assert.invariant(diags.length > 0)
  def cause1: Option[Throwable] = Some(diags(0))
  def lv: OOLAG.OOLAGValueBase = null

  override def getMessage() = {
    if (diags.length == 1) diags(0).getMessage()
    else diags.map { Misc.getSomeMessage(_).get }.mkString("\n")
  }

}

private[oolag] case class AlreadyTried(val lv: OOLAG.OOLAGValueBase)
  extends OOLAGRethrowException {
  override def getMessage() = lv.toString
  override val cause1 = None
}

/**
 * Catch this if you can carry on with more evaluations after an
 * error has occurred. Otherwise just let it propagate.
 */
// I'd like this package private, but they leak out due to compile time errors
// that are not being seen until runtime.
final case class ErrorAlreadyHandled(val th: Diagnostic, lv: OOLAG.OOLAGValueBase)
  extends Exception(th) with OOLAGRethrowException {
  override val cause1 = Some(th)
}

private[oolag] case object AssumptionFailed extends OOLAGRethrowException {
  def lv: OOLAG.OOLAGValue[Any] = Assert.usageError("Should not call lv on AssumptionFailed.")
  def cause1: Option[Throwable] = None
}

final case class CircularDefinition(val lv: OOLAG.OOLAGValueBase, list: Seq[OOLAG.OOLAGValueBase]) extends Exception {
  override def getMessage() = {
    "OOLAG Cycle (of " + list.length + ") through " + list.mkString(", ")
  }
}
