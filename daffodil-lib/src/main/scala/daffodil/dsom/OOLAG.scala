package daffodil.dsom
import daffodil.exceptions.Assert
import daffodil.dsom.OOLAG.OOLAGValue
import daffodil.util.Logging
import daffodil.util._
import daffodil.util.Glob
import daffodil.exceptions.Abort
import daffodil.exceptions.NotYetImplementedException
import daffodil.exceptions.UnsuppressableException

/**
 * OOLAG = Object-oriented Lazy Attribute Grammars
 *
 * It's a collection of techniques for compilation/transformation
 * in a functional programming style
 */
object OOLAG {

  trait HasIsError {
    def isError: Boolean
  }

  trait OOLAGException {
    self: Exception =>
    def lv: OOLAGValue
  }

  trait OOLAGRethrowException extends OOLAGException {
    self: Exception =>
    def cause: Option[Throwable]
  }

  case class AlreadyTried(val lv: OOLAGValue) extends Exception() with OOLAGRethrowException {
    override def getMessage() = lv.toString
    val cause = None
  }

  /**
   * Catch this if you can carry on with more evaluations after an
   * error has occurred. Otherwise just let it propagate.
   */
  case class ErrorAlreadyHandled(val th: Throwable, lv: OOLAGValue)
    extends Exception(th) with OOLAGRethrowException {
    val cause = Some(th)
  }

  case class CircularDefinition(val lv: OOLAGValue) extends UnsuppressableException() {
    override def getMessage() = {
      val list = lv.context.currentOVList
      "OOLAG Cycle (of " + list.length + ") through " + list.mkString(", ")
    }
  }

  /**
   * An object that uses OOLAG values.
   */
  trait OOLAGHost extends Logging {
    def handleThrownError(th: Throwable, ov: OOLAGValue): Unit
    def handleWarning(e: OOLAGValue, th: Throwable): Unit
    def prettyName: String
    def path: String
    def LV: LVFactory

    // I don't like this for all those thread-safety reasons, but otherwise we have to
    // carry a context list with us throughout the computation

    var currentOVList: Seq[OOLAGValue] = Nil

    def circularityDetector(ov: OOLAGValue)(body: => Any) = {
      if (currentOVList.contains(ov)) {
        //        System.err.println("Circular OOLAG Value Definition")
        //        System.err.println("Attributes on stack are: " + currentOVList.mkString(", "))
        throw CircularDefinition(ov)
      }
      currentOVList = ov +: currentOVList
      try {
        body
      } finally {
        currentOVList = currentOVList.tail
      }
    }
  }

  /**
   * An OOLAG value is what would be called an "attribute" in attribute-grammar terminology.
   * It is evaluated lazily, once, and either produces a value, or causes an error.
   *
   * It detects evaluation more than once, and disallows multiple evaluations.
   * It detects and disallows cyclic definitions (oolag value defined in terms of itself). This does not
   * prevent recursive definitions (different objects, but same oolag value 'name'), just cyclic
   * loops (exact same oolag value).
   *
   * OOLAG values are implicitly converted to their underlying value type if used in a context
   * where that type is clear. There will be occasions however, where one must explicitly access
   * the value member.
   *
   * An OOLAG value is most commonly a lazy val of an OOLAG host class. If it is a regular val then it
   * is computed eagerly.
   */
  abstract class OOLAGValue(val context: OOLAGHost, val name: String, factory: LVFactory)
    extends HasIsError
    with Logging {
    protected var alreadyTriedThis = false
    protected var hasValue = false
    protected def lazyBody: Any

    final def warn(th: Throwable) = context.handleWarning(this, th)

    final override def toString =
      try {
        descrip
      } catch {
        case e: Exception => {
          // Assert.invariantFailed("Exception while creating string from OOLAG Host.")
          System.err.println("Exception while creating string from OOLAG Value.")
          "(((Exception in OOLAG Value toString)))"
        }
      }

    // OOLAG framework code has to be rather defensive. If anything goes wrong
    // computing the name, then the whole framework becomes a nightmare.

    private lazy val descrip =
      try {
        context.path + "@@" + name
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
          System.err.println("Exception in OOLAG Value '%s' while computing the name of an OOLAG Value.".format(name))
          "???@@" + name
        }
      }

    private lazy val catchMsg = "%s has no value due to %s)."

    final def valueAsAny: Any = {
      if (hasValue) {
        log(OOLAGDebug("LV: %s already has value: %s", descrip, lazyBody))
        return lazyBody
      }
      val res = context.circularityDetector(this) {
        if (alreadyTriedThis) {
          log(OOLAGDebug("LV: %s was tried and failed", descrip))
          val e = AlreadyTried(this)
          throw e
        }
        alreadyTriedThis = true
        log(OOLAGDebug("Evaluating %s", descrip))

        try {
          val res = lazyBody
          hasValue = true
          log(OOLAGDebug("Evaluated %s to %s.", descrip, res))
          res
        } catch {
          // Some kinds of errors/exceptions we always want thrown to top level.
          //
          // these first few cases are kept as separate code even though it does the same thing
          // so that one can easily put breakpoints here.
          // i.e., coding style for debug here.
          //
          case le: scala.Error => { // note that Exception does NOT inherit from Error 
            log(Error(catchMsg, descrip, le)) // tell us which lazy attribute it was (hard to get from a scala stack trace)
            throw le
          }
          case re: java.lang.RuntimeException => {
            log(Error(catchMsg, descrip, re)) // tell us which lazy attribute it was
            throw re
          }
          case ue: UnsuppressableException => {
            log(Error(catchMsg, descrip, ue)) // tell us which lazy attribute it was            
            throw ue
          }
          //
          // These are OOLAGs own Throwables. 
          // ErrorAlreadyHandled means we are headed back to some top-level that
          // can tolerate errors and go on with compilation.
          case eah: ErrorAlreadyHandled => {
            log(OOLAGDebug(catchMsg, descrip, eah))
            throw eah
          }
          //
          // Already tried means we got to this same OOLAG value evaluation again, 
          // and as values, they can't behave differently, so the same error will
          // just get reported again.
          case at: AlreadyTried => {
            log(OOLAGDebug("Caught %s", at))
            throw at
          }
          case e => {
            //
            // we threw, instead of producing a value
            //
            // Typically this will be for a Schema Definition Error
            // 
            Assert.invariant(hasValue == false)
            Assert.invariant(alreadyTriedThis == true)

            // let the host do what it wants with the situation
            context.handleThrownError(e, this)
            log(OOLAGDebug(catchMsg, descrip, e))
            // 
            // Catch this if you can carry on with more error gathering
            // from other contexts. Otherwise just let it propagate.
            //
            throw new ErrorAlreadyHandled(e, this)
          }
        } finally {
          // factory.name = null
        }
      }
      res
    }

    final def isError = {
      val res = if (alreadyTriedThis) !hasValue
      else try {
        valueAsAny
        !hasValue
      } catch {
        case e: OOLAGException => {
          log(OOLAGDebug("LV %s suppressed throw of %s", this, e))
          true
        }
      }
      if (res == true) {
        val x = 1
        log(OOLAGDebug("LV %s has an error", this))
      }
      res
    }

    protected def toListAny = {
      val res =
        if (!hasValue && alreadyTriedThis) Nil
        else if (isError) Nil
        else valueAsAny match {
          case Some(dp) => List(dp)
          case _ => List(valueAsAny)
        }
      res
    }
  }

  class LV[T](body: => T, context: OOLAGHost, sym: Symbol, factory: LVFactory)
    extends OOLAGValue(context, sym.name, factory) {
    final protected lazy val lazyBody = body
    final def value: T = {
      val res = valueAsAny
      res.asInstanceOf[T]
    }
    final def toList = {
      val res = toListAny
      res.asInstanceOf[List[T]]
    }
  }

  class LVFactory(context: OOLAGHost) {

    //
    // This is really what we need lisp-like macros for. 
    // We want to write:
    // 
    //   defAttribute foo = {... body calc ...}
    //
    // We want that to turn into 
    //
    //   private lazy val foo_ = new LV('foo) {... body calc ...} 
    //   lazy val foo = foo_.value
    //
    // Instead we have to pass the darn name.
    //
    // 
    /**
     * Don't get this unless you really need it, because it
     * uses stack traces, which are large and expensive.
     */
    //    private def LVName = {
    //      val ct = Thread.currentThread()
    //      val stArray = ct.getStackTrace() // EXPENSIVE
    //      val callingFrame = stArray(3) // The magic number - could change if Scala compilation scheme changes
    //      val method = callingFrame.getMethodName()
    //      val unqualifiedMethod = method //.split("\\$").reverse.head
    //      unqualifiedMethod
    //    }

    /**
     * State used to convey the name of the LV to the body code
     */
    // var name: String = null

    /**
     * We call this factory to obtain a lazy value (LV).
     * It is at that point that we can obtain the name
     * automatically.
     */
    def apply[T](sym: Symbol)(body: => T) = {
      // val n = LVName // TODO: do this conditionally based on trace request.
      new LV(body, context, sym, this)
    }

  }

  object LVFactory {
    def apply(context: OOLAGHost) = new LVFactory(context)
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

}