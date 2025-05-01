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

package org.apache.daffodil.lib.oolag

import scala.collection.mutable

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThinException
import org.apache.daffodil.lib.exceptions.UnsuppressableException
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.iapi.WithDiagnostics
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util._

/**
 * OOLAG = Object-oriented Lazy Attribute Grammars
 *
 * It's a collection of techniques for compilation/transformation
 * in a functional programming style. You can find an article about
 * Attribute Grammars on Wikipedia.
 */

object OOLAG {

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
   *
   * Anyplace that calls keepGoing should have a comment saying why it
   * is using it. There should be very few places that use it.
   */
  def keepGoing[T](alt: => T)(body: => T) = {
    try {
      body
    } catch {
      // We suppress errors here because a rethrow indicates that somebody else
      // has already recorded the exception in the diagnostics, and it was of the
      // kind that can be recorded and issued later as a compile-time diagnostic.
      case e: OOLAGRethrowException =>
        Logger.log.trace(s"OOLAG.keepGoing is suppressing exception already recorded: ${e}")
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
    final override val nArgs: Args
  ) extends OOLAGHost {

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
   *  lazy val foo = LV(Symbol("foo")) {...calculation...}.value
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
   *     requiredEvaluationsAlways(foo)
   * }}}
   * This goes at the top of the class definition. When isError is invoked
   * the value of 'foo' will be computed if it has not been attempted
   * already. This insures that the value exists for 'foo', or any errors/warnings
   * to be determined by its calculation have been recorded.
   *
   * There is a variant called requiredEvaluationsIfActivated(...) which
   * is similar, but the evaluations only occur if the object is marked as active
   * via setRequiredEvaluationsActive().
   */
  trait OOLAGHost extends WithDiagnostics with NamedMixinBase {

    protected def oolagContextViaArgs: Option[OOLAGHost] = None
    protected def nArgs: Args = OneArg

    private var oolagContextViaSet: Option[OOLAGHost] = None

    final def setOOLAGContext(oolagContextArg: OOLAGHost): Unit = {
      Assert.usage(
        nArgs == ZeroArgs,
        "Cannot set oolag context if it was provided as a constructor arg."
      )
      if (oolagContextViaSet != None)
        Assert.usageError("Cannot set oolag context more than once.")
      oolagContextViaSet = Option(oolagContextArg) // Option since Option(null) is None
      centralizeEvalFunctionsWhenReady()
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
        Assert.usage(
          oolagContextViaSet != None,
          "Must call setOOLAGContext before accessing when OOLAGHost is constructed with no args."
        )
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

    /**
     * The factory for OOLAGValues is LV.
     */
    protected final def LV[T](sym: Symbol)(body: => T): OOLAGValue[T] = {
      val lv = new OOLAGValue[T](this, sym.name, body)
      lv
    }

    /*
     * requiredEvaluations feature
     *
     * An object either uses requiredEvaluationsAlways, or requiredEvaluationsIfActivated.
     *
     * requiredEvaluationsIfActivated is for objects in the abstract syntax tree (AST) where
     * the mere construction of them is not sufficient to know if the required evaluations
     * should occur. The objects may or may not ultimately be needed.
     *
     * requiredEvaluationsAlways is for top level things like the OOLAG Root object,
     * and objects where their construction is enough to know that they should have
     * these required evaluations occur.
     *
     * They are maintained as two separate lists of eval functions aka thunks to be run.
     *
     * If it uses requiredEvaluationsAlways, then the expressions will be evaluated
     * as part of compilation even if nothing else demands their values. Just constructing
     * the object queues these thunks for evaluation.
     *
     * If it uses requiredEvaluationsIfActivated, then the expresions are ignored
     * unless setRequiredEvaluationsActive() is called on the same object. After that
     * call, all expressions from calls to requiredEvaluationsIfActivated are queued for
     * evaluation, and any additional calls toe requiredEvaluationsIfActive will also be
     * queued for evaluation, and all will be evaluated even if nothing else demands their
     * values.
     *
     * If no call to setRequiredEvaluationsActive() occurs, then expression args for
     * requiredEvaluationsIfActivated will not be evaluated.
     */

    private var requiredEvalCount = 0 // used to generate unique names.
    private val requiredEvalName = Misc.getNameFromClass(this) + "_requiredEvaluation_"

    private var requiredEvalFunctions: List[OOLAGValueBase] =
      Nil // for evaluation unconditionally
    private var requiredEvalIfActivatedFunctions: List[OOLAGValueBase] =
      Nil // for evaluation only if activated.

    /**
     * Used to force evaluation for error checking unconditionally.
     *
     * Creating the object will queue up the expression for evaluation later
     * when checkErrors is called.
     *
     * This is for root-of-the-AST objects, where we don't conditionally construct them.
     * Rather, upon construction we know we positively ARE adding them to the AST.
     */
    protected final def requiredEvaluationsAlways(arg: => Any): Unit = {
      requiredEvaluationsAlways(thunk(arg))
    }

    /**
     * Unconditionally, evaluate the LV arg in order to insure all checks for this
     * object are performed.
     */
    private def requiredEvaluationsAlways(lv: OOLAGValueBase): Unit = {
      val accumPoint =
        if (this.hasOOLAGRootSetup)
          oolagRoot
        else
          this
      accumPoint.requiredEvalFunctions +:= lv
    }

    /**
     * Wraps an LV around an expression that is a non LV
     */
    private def thunk(arg: => Any): OOLAGValueBase = {
      val lv = LV(Symbol(requiredEvalName + requiredEvalCount)) {
        arg
      }
      requiredEvalCount += 1
      lv
    }

    /**
     * When it is possible to centralize (on the OOLAG Root object) all the
     * locally enqueued required evaluation thunks, do so. The root needs to be established first,
     * and the object activated if the requiredEvaluationsIfActive method is used.
     */
    private def centralizeEvalFunctionsWhenReady(): Unit = {
      if (optOolagContext.isDefined) {
        //
        // If the context is now available centralize the required eval functions that are always
        // to be evaluated.
        //
        oolagRoot.requiredEvalFunctions =
          this.requiredEvalFunctions ::: oolagRoot.requiredEvalFunctions
        this.requiredEvalFunctions = Nil
        //
        // If this object is activated, then also centralize the conditional eval functions.
        //
        if (requiredEvalStatus eq Active) {
          oolagRoot.requiredEvalFunctions =
            this.requiredEvalIfActivatedFunctions ::: oolagRoot.requiredEvalFunctions
          this.requiredEvalIfActivatedFunctions = Nil
        }
      }
    }

    private sealed trait ActivityStatus
    private case object Active extends ActivityStatus
    private case object Inactive extends ActivityStatus

    private var requiredEvalStatus: ActivityStatus = Inactive

    /**
     * Saves the arg expression, and insures it is evaluated later only
     * if setRequiredEvaluationActive() is called for this object.
     *
     * This is for use when constructing a abstract syntax tree (AST)
     * such as the DSOM tree (really a directed graph) when
     * the object creation itself is not enough to know if the object will or
     * will not be attached to the tree.
     *
     * The code constructing the AST must traverse the final tree and activate
     * the nodes that ultimately want all these things evaluated on them.
     *
     * If using this, you should know that the object is either discarded
     * (in which case these will never get evaluated), or somehow activated
     * in which case they will be evaluated later when checkErrors is called.
     */
    protected final def requiredEvaluationsIfActivated(arg: => Any): Unit = {
      requiredEvaluationsIfActivated(thunk(arg))
    }

    /**
     * Saves the arg LV, and insures it is evaluated later only if
     * setRequiredEvaluationActive() is called for this object.
     */
    private def requiredEvaluationsIfActivated(lv: OOLAGValueBase): Unit = {
      if (requiredEvalStatus eq Active)
        if (hasOOLAGRootSetup)
          oolagRoot.requiredEvalFunctions +:= lv // active. Rooted. Accumulate centrally.
        else
          requiredEvalIfActivatedFunctions +:= lv // active, but no root yet. Accumulate locally.
      else
        requiredEvalIfActivatedFunctions +:= lv // not active. Accumulate locally.
    }

    /**
     * Call to activate an object so that deferred requiredEvaluationsIfActivated
     * expressions are evaluated.
     *
     * Note that this can be called as part of a unconditional requiredEvaluationsAlways
     * expression/LV, and if so, all the conditional evaluations will be captured and
     * evaluated as part of the ongoing evaluation of requiredEvaluations expressions.
     */
    final def setRequiredEvaluationsActive(): Unit = setRequiredEvaluationsActiveOnceOnly
    private lazy val setRequiredEvaluationsActiveOnceOnly = {
      requiredEvalStatus = Active
      centralizeEvalFunctionsWhenReady()
    }

    /**
     * Evaluate all requiredEvaluation functions/expressions.
     *
     * Evaluates all currently activated (or always activated)
     * required evaluations, as well as those for any objects that
     * become activated during evaluation of required evaluations of
     * any object recursively.
     *
     * This is called only on the OOLAGRoot object. It depends on
     * all other object's required evaluations having been moved up
     * onto the root, which happens automatically via the calls to
     * centralizeEvalFunctionsWhenReady().
     */
    final def checkErrors(): Unit = {
      Assert.usage(this.isOOLAGRoot || requiredEvalFunctions == Nil)
      while (oolagRoot.requiredEvalFunctions != Nil) { // while there is an accumulated crop of eval functions
        // grab the current crop of eval functions
        var evFuncs = oolagRoot.requiredEvalFunctions
        // and clear the current accumulation point
        oolagRoot.requiredEvalFunctions = Nil
        // evaluate the current crop of eval functions.
        // this evaluation may cause new eval functions to be accumulated.
        // which will cause the outer loop to iterate again.
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
    private val errors_ = new mutable.LinkedHashSet[Diagnostic]()
    private val warnings_ = new mutable.LinkedHashSet[Diagnostic]()

    final def errors: Seq[Diagnostic] = oolagRoot.errors_.toSeq
    final def warnings: Seq[Diagnostic] = oolagRoot.warnings_.toSeq

    override def getDiagnostics: Seq[Diagnostic] = diagnostics

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

    protected def oolagWarn(th: Diagnostic): Unit = {
      if (!warnings_.contains(th))
        warnings_ += th
    }

    protected def oolagError(th: Diagnostic): Unit = {
      if (!errors_.contains(th))
        errors_ += th
    }

    /**
     * Forces evaluation of all the requiredEvaluationsAlways(...)
     * or requiredEvaluationsIfActivated(...)
     * on all objects first, but that is only for the objects
     * that have been created and activated at the time this is called.
     */
    def isError: Boolean = isErrorOnce
    private lazy val isErrorOnce: Boolean = {
      oolagRoot.checkErrors()
      val errorCount = oolagRoot.errors.size
      errorCount > 0
    }

    def diagnostics: Seq[Diagnostic] = (errors ++ warnings).toSeq
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
    bodyArg: => Any
  ) {

    // SCHEMA COMPILER PERFORMANCE INSTRUMENTATION
    // To get timing on every OOLAG LV, use these 3 lines to define the 'body' lazy var.
    // private lazy val bodyOnce = bodyArg
    // private lazy val timedBody = TimeTracker.track(name) { bodyOnce }
    // private lazy val body = timedBody
    // You will also need to call TimeTracker.logTimes() at the end of Compiler.compileSource.
    private lazy val body = bodyArg

    Assert.usage(oolagContext != null)

    final lazy val name = nameArg

    private var errorAlreadyHandled: Maybe[ErrorAlreadyHandled] = Nope
    private var alreadyTriedThis: Maybe[AlreadyTried] = Nope
    protected final def hasValue: Boolean = value_.isDefined
    private var value_ : Maybe[AnyRef] = Nope

    private final def wasTried = alreadyTriedThis.isDefined

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

    override def toString: String = thisThing

    protected final def toss(th: Throwable): Nothing = {
      throw th
    }

    protected final def oolagCatch(th: Throwable): Nothing = {
      Assert.invariant(!hasValue)
      th match {
        case le: scala.Error => { // note that Exception does NOT inherit from Error
          Logger.log.trace(
            s" " * indent + s"${thisThing} has no value due to ${le}"
          ) // tell us which lazy attribute it was
          toss(le)
        }
        //
        // Don't catch RuntimeException as this makes using the debugger
        // to isolate bugs harder. You just end up converting them into unsuppressible,
        // but with loss of context.
        //
        case ue @ (_: IllegalArgumentException | _: UnsuppressableException) => {
          val ex = ue
          Logger.log.trace(
            " " * indent + s"${this.getClass.getName} has no value due to ${ex}"
          ) // tell us which lazy attribute it was
          toss(ex)
        }
        //
        // These are OOLAGs own Throwables.
        // ErrorAlreadyHandled means we are headed back to some top-level that
        // can tolerate errors and go on with compilation.
        case eah: ErrorAlreadyHandled => {
          Logger.log.trace(
            s" " * indent + s"${thisThing} has no value due to ${eah}"
          ) // tell us which lazy attribute it was
          toss(eah)
        }
        //
        // Already tried means we got to this same OOLAG value evaluation again,
        // and as values, they can't behave differently, so the same error will
        // just get reported again.
        case at: AlreadyTried => {
          Logger.log.trace(" " * indent + s"Caught ${at}")
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
          Assert.invariant(alreadyTriedThis.isDefined)
          Assert.invariant(errorAlreadyHandled.isEmpty)

          Logger.log.trace(" " * indent + s"${thisThing} has no value due to ${e}")
          error(e)
          //
          // Catch this if you can carry on with more error gathering
          // from other contexts. Otherwise just let it propagate.
          //
          val eah = new ErrorAlreadyHandled(e, this)
          errorAlreadyHandled = One(eah) // in theory, saving this doesn't matter.
          toss(eah)
        }
        case e @ ErrorsNotYetRecorded(diags) => {
          Assert.invariant(alreadyTriedThis.isDefined)
          Assert.invariant(!hasValue)
          diags.foreach { error(_) }
          toss(alreadyTriedThis.get)
        }
        case th => toss(th)
      }
    }

    private def initialize = {
      val now = oolagContext.currentOVList
      oolagContext.currentOVList = this +: oolagContext.currentOVList
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
        Logger.log.trace(" " * indent + s"LV: ${thisThing} CIRCULAR")
        toss(c)
      }
      if (alreadyTriedThis.isDefined) {
        Logger.log.trace(" " * indent + s"LV: ${thisThing} was tried and failed")
        val e = alreadyTriedThis.get
        toss(e)
      }
      alreadyTriedThis = One(AlreadyTried(this))
      Logger.log.trace(" " * indent + s"Evaluating ${thisThing}")
    }

    protected final def oolagAfterValue(res: AnyRef): Unit = {
      Logger.log.trace(" " * indent + s"Evaluated ${thisThing}")
      value_ = Maybe(res)
    }

    protected final def oolagFinalize() = {
      setIndent(indent - 2)
      Logger.log.trace(" " * indent + s"pop: ${thisThing}")
      if (oolagContext.currentOVList.nonEmpty)
        oolagContext.currentOVList = oolagContext.currentOVList.tail
    }

    final def hasError = alreadyTriedThis.isDefined && !hasValue

    /**
     * forces the value, then boolean result tells you if
     * a value was computed or errors, 1 or more, occurred
     * which prevent a value from being computed.
     */

    final def isError = {
      val res =
        if (alreadyTriedThis.isDefined) !hasValue
        else {
          try {
            valueAsAny
            !hasValue
          } catch {
            case oe: OOLAGRethrowException => true
          }
        }
      if (res == true) {
        Logger.log.trace(" " * indent + s"LV ${this} has an error")
      }
      res
    }

    final lazy val valueAsAny: Any = {
      if (hasValue) value_.get
      else {
        // egad.... on 2024-12-19 this was always re-evaluating due to missing `else` keyword.
        // this should make a substantial difference in schema compilation time.
        val res =
          try {
            oolagBefore()
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
            oolagFinalize()
          }
        res
      }
    }

    protected lazy val toOptionAny: Option[Any] = {
      if (wasTried) {
        if (hasValue) Some(value_.get)
        else None
      } else {
        val res =
          try {
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
private[oolag] abstract class OOLAGException(th: Throwable) extends ThinException(th) {
  def lv: OOLAG.OOLAGValueBase
}

private[oolag] abstract class OOLAGRethrowException(th: Throwable) extends OOLAGException(th) {
  def cause1: Option[Throwable]

  def this() = this(null)
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
  extends OOLAGRethrowException(th) {
  override val cause1 = Some(th)
}

private[oolag] case object AssumptionFailed extends OOLAGRethrowException {
  def lv: OOLAG.OOLAGValue[Any] = Assert.usageError("Should not call lv on AssumptionFailed.")
  def cause1: Option[Throwable] = None
}

final case class CircularDefinition(
  val lv: OOLAG.OOLAGValueBase,
  list: Seq[OOLAG.OOLAGValueBase]
) extends Exception {
  override def getMessage() = {
    "OOLAG Cycle (of " + list.length + ") through " + list.mkString(", ")
  }
}
