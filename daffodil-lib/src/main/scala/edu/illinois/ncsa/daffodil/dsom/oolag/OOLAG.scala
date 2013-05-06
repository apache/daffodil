package edu.illinois.ncsa.daffodil.dsom.oolag

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.Glob
import edu.illinois.ncsa.daffodil.exceptions.Abort
import edu.illinois.ncsa.daffodil.exceptions.NotYetImplementedException
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.ExecutionMode

/**
 * OOLAG = Object-oriented Lazy Attribute Grammars
 *
 * It's a collection of techniques for compilation/transformation
 * in a functional programming style
 */

object OOLAG extends Logging {

  trait HasIsError {
    def isError: Boolean
  }

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
        log(OOLAGDebug("OOLAG.keepGoing is suppressing exception already recorded: %s", e))
        alt
    }
  }

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
  trait OOLAGException {
    self: Exception =>
    def lv: OOLAGValue
  }

  // private[oolag] 
  trait OOLAGRethrowException extends Exception with OOLAGException {
    self: Exception =>
    def cause: Option[Throwable]
  }

  // private [oolag] 
  case class AlreadyTried(val lv: OOLAGValue)
    extends Exception() with OOLAGRethrowException {
    override def getMessage() = lv.toString
    val cause = None
  }

  /**
   * Catch this if you can carry on with more evaluations after an
   * error has occurred. Otherwise just let it propagate.
   */
  // I'd like this package private, but they leak out due to compile time errors
  // that are not being seen until runtime.
  case class ErrorAlreadyHandled(val th: Diagnostic, lv: OOLAGValue)
    extends Exception(th) with OOLAGRethrowException {
    val cause = Some(th)
  }

  protected case class CircularDefinition(val lv: OOLAGValue, list: Seq[OOLAGValue]) extends Exception with OOLAGException {
    override def getMessage() = {
      "OOLAG Cycle (of " + list.length + ") through " + list.mkString(", ")
    }
  }

  trait OOLAGDiagnosticMixin extends DiagnosticImplMixin {
    override def isError = true
  }

  /**
   * I tried making a private singleton to use instead of null. I couldn't get
   * it to work right with the type checking. Not worth it. The root
   * has parent null.
   */
  private[oolag] val OOLAGRoot: OOLAGHost = null

  /**
   * An OOLAGHost, or OOLAG for short, is a collection of OOLAGValues
   * or LVs for short.
   *
   * The way these are generally used now is like this
   *
   *    lazy val foo = foo_.value
   *    private val foo_ = LV('foo) {...calculation...} // note val is preferred here
   *
   * Why scala needs 'real' Lisp-style macros: Well wouldn't it be
   * nicer if I could write:
   *
   *    defAttribute foo {... calculation ...}
   *
   * and have that be equivalent the more verbose stuff above it?
   * But I digress....
   *
   * The LVs (but not the values of the LV) are created
   * at OOLAGHost object creation time.
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
   *
   *     requiredEvaluations(foo)
   *
   * This goes at the top of the class definition. When isError is invoked
   * the value of 'foo' will be computed if it has not been attempted
   * already. This insures that the value exists for 'foo', or any errors/warnings
   * to be determined by its calculation have been recorded.
   */
  abstract class OOLAGHost(val oolagContext: OOLAGHost)
    extends HasIsError with Logging with WithDiagnostics {

    /**
     * My parent, unless I am the root and have no parent. In that
     * case this aborts with a usage error. Programmer should have
     * called isOOLAGRoot first.
     */
    protected def oolagParent: OOLAGHost = {
      if (!isOOLAGRoot) oolagContext
      else Assert.usageError("parent of root OOLAGHost not allowed.")
    }

    /**
     * Take me to the root of this OOLAG nest.
     */
    lazy val oolagRoot: OOLAGHost = {
      if (isOOLAGRoot) this
      else oolagContext.oolagRoot
    }

    def isOOLAGRoot = {
      val res = oolagContext == OOLAGRoot
      res
    }

    /**
     * For trace/debug output & diagnostic messages.
     */
    def prettyName = Misc.getNameFromClass(this)

    def path: String = {
      if (isOOLAGRoot) prettyName else oolagParent.path + ":" + prettyName
    }

    /**
     * Detect circular relationships among LVs.
     */

    // I don't like this var for all those thread-safety reasons, but otherwise we have to
    // carry a context list with us throughout the computation

    private var currentOVList_ : Seq[OOLAGValue] = Nil

    def currentOVList = currentOVList_

    private[oolag] def circularityDetector(ov: OOLAGValue)(body: => Any) = {
      if (currentOVList_.contains(ov)) {
        System.err.println("Circular OOLAG Value Definition")
        // This next println was causing problems because toString was
        // itself causing circular evaluation. The abort above 
        // System.err.println("OOLAGValues (aka 'LVs') on stack are: " + currentOVList.mkString(", "))
        throw CircularDefinition(ov, currentOVList_)
      }
      currentOVList_ = ov +: currentOVList_
      try {
        body
      } finally {
        currentOVList_ = currentOVList_.tail
      }
    }

    /**
     * The factory for OOLAGValues is LV.
     */
    protected def LV[T](sym: Symbol)(body: => T): LV[T] = {
      val lv = new LV[T](sym, this, body)
      lv
    }

    /**
     * requiredEvaluations feature
     */

    // TODO: only used on root object. 
    // Would be good to eliminate from all objects
    // as space savings. (In a really large schema, every slot
    // counts!)
    private var errorCheckList: Seq[ZList] = Nil

    // Intended to be used to help enforce that concrete OOLAGHost
    // classes have calls to requiredEvaluations(..) in them.
    // Check not yet implemented though.
    // private var wasErrorUnlessCalled: Boolean = false // ?? Using ??
    private[oolag] var rootErrorCheckList: Seq[OOLAGHost] = Nil

    def requiredEvaluations(str: ZList): Unit = {
      oolagRoot.errorCheckList :+= str
      // this OOLAGHost object added to list
      if (!oolagRoot.rootErrorCheckList.contains(this)) {
        oolagRoot.rootErrorCheckList :+= this
      }
      // wasErrorUnlessCalled = true
    }

    /**
     * Up to this many arguments. Add more if we need them.
     *
     * This is because Scala can't do by-name passing of varargs.
     */
    final def requiredEvaluations(
      a0: => Any = Nil,
      a1: => Any = Nil,
      a2: => Any = Nil,
      a3: => Any = Nil,
      a4: => Any = Nil,
      a5: => Any = Nil,
      a6: => Any = Nil,
      a7: => Any = Nil,
      a8: => Any = Nil,
      a9: => Any = Nil,
      a10: => Any = Nil,
      a11: => Any = Nil,
      a12: => Any = Nil,
      a13: => Any = Nil,
      a14: => Any = Nil): Unit = {

      // See Zlist.scala
      // The above args are all call-by name. These ZZ list cells are lazy.
      // So nothing is evaluated here. The argument expressions are just captured
      // for evaluation later.
      val zzz = ZZ(a0, ZZ(a1, ZZ(a2, ZZ(a3, ZZ(a4, ZZ(a5, ZZ(a6, ZZ(a7, ZZ(a8, ZZ(a9, ZZ(a10, ZZ(a11, ZZ(a12, ZZ(a13, ZZ(a14, ZEnd)))))))))))))))
      requiredEvaluations(zzz)
    }

    private def checkErrors {
      ExecutionMode.usingCompilerMode {
        while (oolagRoot.errorCheckList != Nil) {
          var ecl = oolagRoot.errorCheckList // grab current errorCheckList
          oolagRoot.errorCheckList = Nil // truncate it, so we can spot new entries  

          ecl.foreach { z =>
            var zAtStart = z // keep original ZList for display       
            var zlist = z
            while (zlist != ZEnd) {
              var next = zlist.tail
              OOLAG.keepGoing() {
                //
                // This use of an OOLAG value here insures that anything a user
                // puts inside of requiredEvaluations(...) if it throws, it does so
                // across the context of an OOLAG Value, which insures that 
                // Things are not circular, that things will accumulate SDEs and they
                // wont just unwind to top level, etc.
                //
                val requiredEvals = LV('requiredEvaluations) {
                  ExecutionMode.usingCompilerMode {
                    val h = zlist.head
                    // forces the evaluation of the next head
                    // we don't actually care what the value is, just that there is one.
                    // the point is to cause the error to be thrown, caught below, but
                    // reported on the OOLAGValue, where we pick it up later. 
                    // println("value was: " + h)
                  }
                }
                requiredEvals.value
              }
              zlist = next
            }
            // println("errorCheckList: " + str) // displays all the values
            // pick up any new ones that were placed 
            // there because new objects were created.
          }
          // println("Done with one round.")
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

    def errors = oolagRoot.errors_
    def warnings = oolagRoot.warnings_

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
    def rethrowAsDiagnostic(th: Throwable): Nothing

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

    //TODO: avoid having to store these on any object except
    // the root object. 
    def isError = {
      oolagRoot.checkErrors

      //      // runs through all the requiredEvaluations(...) forced 
      //      // evaluations, on the root object of course.
      //      oolagRoot.rootErrorCheckList.foreach { oh =>
      //        OOLAG.keepGoing() {
      //          oh.checkErrors
      //        }
      //      }

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
   * just cyclic loops (exact same oolag value).
   *
   * An OOLAG value is created as a val of an OOLAGHost class.
   */
  abstract class OOLAGValue(val oolagContext: OOLAGHost, nameArg: String)
    extends HasIsError
    with Logging {

    Assert.usage(oolagContext != null)

    lazy val name = nameArg
    protected def lazyBody: Any

    private var alreadyTriedThis = false
    private var hasValue_ = false

    private final def wasTried = alreadyTriedThis
    private final def hasValue = hasValue_

    private def warn(th: Diagnostic): Unit = oolagContext.warn(th)
    private def error(th: Diagnostic): Unit = oolagContext.error(th)

    override def toString =
      try {
        descrip
      } catch {
        case e: Exception => {
          // Assert.invariantFailed("Exception while creating string from OOLAG Host.")
          System.err.println("Exception while creating string from OOLAGValue.")
          System.err.println("Exception class: " + e.getClass().getName())
          "(((Exception in OOLAG Value toString)))"
        }
      }

    // OOLAG framework code has to be rather defensive. If anything goes wrong
    // computing the name, then the whole framework becomes a nightmare.

    private lazy val descrip =
      try {
        val cp = oolagContext.path
        val suffix = "@@" + name
        val res = cp + suffix
        res
      } catch {
        case e: CircularDefinition => {
          // we have a circularity in trying to come up with the description
          // This would be because something in context.path is causing an error
          // which then results in us needing to print out the context.path
          // resulting in this circularity.
          //
          // in this case, just the name will be the description
          "circ(" + name + ")"
        }
        case e: Exception => {
          val exc = e
          System.err.println("Exception in OOLAGValue '%s' while computing the name of an OOLAG Value.".format(name))
          System.err.println("Exception class: " + exc.getClass().getName())
          // System.err.println("The exception was %s".format(exc))
          "???@@" + name
        }
      }

    private val catchMsg = "%s has no value due to %s."

    /**
     * Centralize the tricky try/catch logic.
     */
    private def oolagTryCatch[T](body: => T)(oolagAction: Throwable => T)(oolagDiagnosticAction: (Diagnostic, OOLAGValue) => T): T = {
      try {
        body
      } catch {
        // Some kinds of errors/exceptions we always want thrown to top level.
        //
        // these first few cases are kept as separate code even though it does the same thing
        // so that one can easily put breakpoints here.
        // i.e., coding style for debug here.
        //
        case le: scala.Error => { // note that Exception does NOT inherit from Error 
          log(Error(catchMsg, descrip, le)) // tell us which lazy attribute it was
          throw le
        }
        case ex: java.lang.RuntimeException => {
          val re = ex // debugger won't let you see the exception without this.
          log(Error(catchMsg, descrip, re)) // tell us which lazy attribute it was
          error(new Exception(re) with OOLAGDiagnosticMixin)
          val ab = new Abort(re.toString)
          throw ab
        }
        case ue: UnsuppressableException => {
          val ex = ue
          log(OOLAGDebug(catchMsg, this.getClass.getName, ue)) // tell us which lazy attribute it was            
          throw ue
        }
        //
        // These are OOLAGs own Throwables. 
        // ErrorAlreadyHandled means we are headed back to some top-level that
        // can tolerate errors and go on with compilation.
        case eah: ErrorAlreadyHandled => {
          log(OOLAGDebug(catchMsg, descrip, eah))
          oolagAction(eah)
        }
        //
        // Already tried means we got to this same OOLAG value evaluation again, 
        // and as values, they can't behave differently, so the same error will
        // just get reported again.
        case at: AlreadyTried => {
          log(OOLAGDebug("Caught %s", at))
          oolagAction(at)
        }
        case e: Diagnostic => {
          // we threw, instead of producing a value
          //
          // Typically this will be for a Schema Definition Error
          // 
          Assert.invariant(hasValue == false)
          Assert.invariant(alreadyTriedThis == true)

          // accumulate the error object is up to provider
          // of the oolagDiagnosticAction. So don't error() here
          // error(e)
          log(OOLAGDebug(catchMsg, descrip, e))
          // 
          // Catch this if you can carry on with more error gathering
          // from other contexts. Otherwise just let it propagate.
          //
          oolagDiagnosticAction(e, this)
        }
        case unknown: Throwable => {
          val unknownException = unknown
          log(OOLAGDebug(catchMsg, descrip, unknownException))
          oolagContext.rethrowAsDiagnostic(unknown)
        }
      } finally {
        // nothing for now
      }
    }

    protected final def valueAsAny: Any = {
      if (oolagContext == null) {
        alreadyTriedThis = true
        val res = lazyBody
        hasValue_ = true
        res
      } else {
        // oolagContext.closeRegistration
        // registration of LVs is closed once any LV has been attempted to be evaluated.
        // that forces us to construct LVs properly (at construction time)
        // but evaluated them later (lazy eval time).
        if (hasValue_) {
          log(OOLAGDebug("LV: %s already has value: %s", descrip, lazyBody))
          lazyBody
        } else {
          val res = oolagContext.circularityDetector(this) {
            if (alreadyTriedThis) {
              log(OOLAGDebug("LV: %s was tried and failed", descrip))
              val e = AlreadyTried(this)
              throw e
            }
            alreadyTriedThis = true
            log(OOLAGDebug("Evaluating %s", descrip))

            oolagTryCatch {
              val res = lazyBody
              hasValue_ = true
              log(OOLAGDebug("Evaluated %s to %s.", descrip, res))
              res
            } { th => throw th } { (e, ov) =>
              {
                error(e)
                throw new ErrorAlreadyHandled(e, ov)
              }
            }
          }
          res
        }
      }
    }

    /**
     * use for things where you need something to put
     * into a diagnostic message.
     */
    protected final def valueAsAnyOrElse(thing: => Any): Any = {
      if (hasValue) valueAsAny
      else {
        val v = try {
          valueAsAny
        } catch {
          case _: Throwable => {
            // System.err.println("OOLAG valueOrElse failed. Substituting.")
            // Assert.abort("OOLAG valueOrElse failed.")
            thing
          }
        }
        v
      }
    }

    final def hasError = alreadyTriedThis && !hasValue

    /**
     * forces the value, then boolean result tells you if
     * a value was computed or errors, 1 or more, occurred
     * which prevent a value from being computed.
     */

    def isError = {
      val res = if (alreadyTriedThis) !hasValue
      else {
        oolagTryCatch[Boolean] {
          valueAsAny
          !hasValue
        } { th => true } { (e, ov) => true }
      }
      if (res == true) {
        log(OOLAGDebug("LV %s has an error", this))
      }
      res
    }

    //    /**
    //     * List containing value or Nil if there was an error.
    //     * If it's an option type, then Nil if None,
    //     * List of v if Some(v)
    //     */
    //    protected def toListAny = {
    //      val res =
    //        if (!hasValue && alreadyTriedThis) Nil
    //        else if (isError) Nil
    //        else valueAsAny match {
    //          case Some(dp) => List(dp)
    //          case None => Nil
    //          case _ => List(valueAsAny)
    //        }
    //      res
    //    }
  }

  /**
   * LV is the class people actually use when creating these
   * things.
   */
  protected class LV[T](val sym: Symbol, context: OOLAGHost, body: => T)
    extends OOLAGValue(context, sym.name) {

    final protected lazy val lazyBody = body

    final def value: T = {
      val res = valueAsAny
      res.asInstanceOf[T]
    }

    final def valueOrElse(thing: T): T = {
      val res = valueAsAnyOrElse(thing)
      res.asInstanceOf[T]
    }

    //    final def toList = {
    //      val res = toListAny
    //      res.asInstanceOf[List[T]]
    //    }
  }

  //  /**
  //   * Implicitly, an LV is convertable to its underlying type.
  //   */
  //  implicit def LV_T_to_T[T](lv : LV[T]) : T = lv.value
  //
  //  /**
  //   * Implicitly, if one LV is defined so it returns another LV, then
  //   * this converts to the underlying type you want.
  //   */
  //  implicit def LVLV_T_to_T[T](lv : LV[LV[T]]) : T = lv.value.value
  //
  //  /**
  //   * And that works 3 hops deep. Beyond that you have to call ".value" yourself.
  //   */
  //  implicit def LVLVLV_T_to_T[T](lv : LV[LV[LV[T]]]) : T = lv.value.value.value

} // end object
