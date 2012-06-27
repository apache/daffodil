package daffodil.dsom
import daffodil.exceptions.Assert
import daffodil.dsom.OOLAG.OOLAGValue
import daffodil.util.Logging
import daffodil.util._
import daffodil.util.Glob

/**
 * OOLAG = Object-oriented Lazy Attribute Grammars
 *
 * It's a collection of techniques for compilation/transformation
 * in a functional programming style
 */
object OOLAG {

  trait HasIsError {
    def isError : Boolean
  }

  abstract class OOLAGException(msg : String) extends Exception(msg)

  abstract class OOLAGRethrowException(msg : String) extends OOLAGException(msg)
  case class AlreadyTried(lvName : String) extends OOLAGException("Already Tried " + lvName)
  case class ErrorAlreadyHandled(th : Throwable) extends OOLAGException("Error already handled: " + th)

  /**
   * An object that uses OOLAG values.
   */
  trait OOLAGHost extends Logging {
    def handleThrownError(e : OOLAGValue) : Unit
    def handleWarning(e : OOLAGValue, th : Throwable) : Unit
    def prettyName : String
    def path : String
    def LV : LVFactory
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
  abstract class OOLAGValue(val context : OOLAGHost, final val name : String, factory : LVFactory)
    extends HasIsError
    with Logging {
    protected var alreadyTriedThis = false
    protected var hasValue = false
    protected var throwable : Throwable = null
    protected def lazyBody : Any

    final def warn(th : Throwable) = context.handleWarning(this, th)

    final override def toString = descrip

    private lazy val descrip = context.path + "@@" + name
    private lazy val catchMsg = "Catch! So %s has no value. (Exc = %s)."

    final def valueAsAny = {
      if (hasValue) lazyBody
      else if (alreadyTriedThis) {
        throw AlreadyTried(name)
      } else {
        alreadyTriedThis = true
        log(Debug("Evaluating %s", descrip))
        factory.name = name // NOTE: Sequential. Not concurrent/thread safe.

        try {
          val res = lazyBody
          hasValue = true
          log(Debug("Evaluated %s to %s.", descrip, res))
          res
        } catch {
          case eah : ErrorAlreadyHandled => {
            log(Debug(catchMsg, descrip, eah))
            throw eah
          }
          case at : AlreadyTried => {
            log(Debug("Caught %s", at))
            throw at
          }
          case e => {
            // we threw, instead of producing a value
            Assert.invariant(hasValue == false)
            Assert.invariant(alreadyTriedThis == true)
            // save it
            throwable = e
            // let the host do what it wants with the situation
            context.handleThrownError(this)
            log(Debug(catchMsg, descrip, e))
            // 
            // Catch this if you can carry on with more error gathering
            // from other contexts. Otherwise just let it propagate.
            //
            throw new ErrorAlreadyHandled(e)
          }
        } finally {
          factory.name = null
        }
      }
    }

    final def isError = {
      if (alreadyTriedThis) !hasValue
      else try {
        valueAsAny
        !hasValue
      } catch {
        case e => { //: OOLAGException => {
          // throw already has been handled by context.
          true
        }
      }
    }

    final def thrown() = {
      Assert.usage(isError)
      throwable
    }

  }

  class LV[T](body : => T, context : OOLAGHost, name : String, factory : LVFactory)
    extends OOLAGValue(context, name, factory) {
    final protected lazy val lazyBody = body
    final def value : T = {
      val res = valueAsAny
      res.asInstanceOf[T]
    }
  }

  class LVFactory(context : OOLAGHost) {

    /**
     * Don't get this unless you really need it, because it
     * uses stack traces, which are large and expensive.
     */
    private def LVName = {
      val ct = Thread.currentThread()
      val stArray = ct.getStackTrace() // EXPENSIVE
      val callingFrame = stArray(3) // The magic number - could change if Scala compilation scheme changes
      val method = callingFrame.getMethodName()
      val unqualifiedMethod = method.split("\\$").reverse.head
      unqualifiedMethod
    }

    /**
     * State used to convey the name of the LV to the body code
     */
    var name : String = null

    /**
     * We call this factory to obtain a lazy value (LV).
     * It is at that point that we can obtain the name
     * automatically.
     */
    def apply[T](body : => T) = {
      val n = LVName // TODO: do this conditionally based on trace request.
      new LV(body, context, n, this)
    }

  }

  object LVFactory {
    def apply(context : OOLAGHost) = new LVFactory(context)
  }

  /**
   * Implicitly, an LV is convertable to its underlying type.
   */
  implicit def LV_T_to_T[T](lv : LV[T]) : T = lv.value

  /**
   * Implicitly, if one LV is defined so it returns another LV, then
   * this converts to the underlying type you want.
   */
  implicit def LVLV_T_to_T[T](lv : LV[LV[T]]) : T = lv.value.value

  /**
   * And that works 3 hops deep. Beyond that you have to call ".value" yourself.
   */
  implicit def LVLVLV_T_to_T[T](lv : LV[LV[LV[T]]]) : T = lv.value.value.value

}