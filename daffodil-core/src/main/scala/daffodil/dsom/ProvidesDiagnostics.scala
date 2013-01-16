package daffodil.dsom

import java.io._
import scala.xml.Node
import scala.xml.XML
import daffodil.api.DFDL
import daffodil.exceptions._
import daffodil.xml.XMLUtils
import daffodil.grammar._
import daffodil.processors._
import daffodil.util.Misc._
import daffodil.api.Diagnostic
import daffodil.util.Misc
import daffodil.api.WithDiagnostics
import daffodil.dsom.OOLAG._
import daffodil.util.Glob
import daffodil.util.Debug
import daffodil.util.Info
import daffodil.util.Compile

object DiagnosticUtils {
  /**
   * Java throwable/exception objects may or may not have a message. They are supposed to have a cause if they
   * don't have a message of their own, but might have neither, or might have both.
   *
   * This is too painful to deal with in code when you want to be generic about converting throws/exceptions
   * into diagnostic information.
   *
   * So we have a more uniform behavior. Never returns null. Always gets a message.
   * If the argument has none, but has a cause object, then it
   * gets the message from that, if that has no message, it chases further.
   * Ultimately, if there's no message, it just uses the innermost cause object's class name.
   */

  def getSomeMessage(th: Throwable): Some[String] = {
    val m = th.getMessage()
    val c = th.getCause()
    val res = (m, c) match {
      case (null, null) => th.getClass.getName
      case (m, null) => m
      case (null, c) => getSomeMessage(c).get
      case (m, c) => {
        val Some(cmsg) = getSomeMessage(c)
        cmsg + "(within " + m + ")"
      }
    }
    Some(res)
  }

  def getSomeCause(th: Throwable): Some[Throwable] = {
    val c = th.getCause()
    val res = c match {
      case null => th
      case _ => getSomeCause(c).get
    }
    Some(res)
  }
}

trait DiagnosticImplMixin extends Diagnostic { self: Throwable =>
  def getSomeCause() = DiagnosticUtils.getSomeCause(self)
  def getSomeMessage() = DiagnosticUtils.getSomeMessage(self)
}

class CapturedThrowDiagnostic(e: Throwable, context: Any) extends Throwable with DiagnosticImplMixin {
  Assert.invariant(!e.isInstanceOf[OOLAGRethrowException])
  def isError() = true
  def getSchemaLocations() = Nil
  def getDataLocations() = Nil
  override def getMessage() = "ERROR caught: " + e + ". Context at catch was: " + context
}

class Warning(e: Throwable) extends Throwable with DiagnosticImplMixin {
  Assert.invariant(!e.isInstanceOf[OOLAGRethrowException])
  def isError() = false
  def getSchemaLocations() = Nil
  def getDataLocations() = Nil
  override def getMessage() = "WARNING: " + e
}

/**
 * Implementation of OOLAG diagnostics gathering for Daffodil (daffodil being an application of OOLAG technique)
 *
 * This is for compilation. Not the runtime.
 */
trait DiagnosticsProviding extends OOLAGHost with HasIsError {
  type DiagnosticsList = Seq[HasIsError]

  lazy val LV = LVFactory(this)

  def handleThrownError(e: Throwable, lv: OOLAGValue) {
    log(Compile("Object %s recording error: %s", this, e))
    val diag = e match {
      case d: Diagnostic => d
      case _ => new CapturedThrowDiagnostic(e, lv.context) // no common trait to use....?
    }
    addDiagnostic(diag)
    // addDiagnosticChild(lv)
    lv.context match {
      case dp: Prod => {
        // dp.sc.addDiagnosticHost(dp) // TODO: determine is this really needed?
      }
      case _ => log(Compile("type of lv.context is %s", lv.context.getClass().getName()))
    }
  }

  def handleWarning(lv: OOLAGValue, th: Throwable) {
    val diag = new Warning(th)
    addDiagnostic(diag)
  }

  protected var localDiagnostics: Seq[Diagnostic] = Nil
  private var diagChildren: Seq[OOLAGValue] = Nil
  private var diagHosts: Seq[DiagnosticsProviding] = Nil

  def addDiagnostic(diag: Diagnostic) {
    if (!localDiagnostics.contains(diag)) {
      localDiagnostics = localDiagnostics :+ diag
      log(Compile("Adding Diagnostic: %s to %s", diag, path))
    } else {
      log(Compile("Already recorded diagnostic: %s to %s", diag, path))
    }
  }

  //  private def addDiagnosticChild(lv: OOLAGValue) {
  //    diagChildren = diagChildren :+ lv
  //    val lvmsg = lv.toString
  //    val thismsg = this.prettyName
  //    log(Compile("Adding Diagnostic child %s to %s.", lvmsg, thismsg))
  //  }
  //
  //  def addDiagnosticHost(h: DiagnosticsProviding) {
  //    diagHosts = diagHosts :+ h
  //    log(Compile("Adding Diagnostic host %s to %s.", h, this))
  //  }

  //  final lazy val hasDiagnostics = {
  //    getDiagnostics.size > 0
  //  }

  final lazy val getDiagnostics = getDiagnostics_.value
  private lazy val getDiagnostics_ = LV('getDiagnostics) {
    val res = diagnostics.toSeq // TODO: Sort into file/schema ordering so that earlier diagnostics are about
    // earlier parts of the schema.
    res
  }

  // Note: an error could occur when trying to construct this list in a derived class
  // hence, we don't use this member directly, rather we use diagnosticChildrenList
  protected def diagnosticChildren: DiagnosticsList

  // This is *always* a list of diagnostics, even if the diagnosticChildren member couldn't be evaluated.
  private lazy val diagnosticChildrenList = {
    val dc =
      try diagnosticChildren
      catch {
        case OOLAG.ErrorAlreadyHandled(th, lv) =>
          log(Compile("exception when just trying to get the list of diagnostic children: %s, in LV: %s", th, lv))
          // 
          // If we don't record this error now, we'll lose it, because we won't be able to gather it up
          // out of the list of diagnostic children because, well we weren't able to even construct that
          // list without an error.
          this.handleThrownError(th, lv)
          List(lv)
      }
    log(Compile("DC for %s is %s", this, dc))
    dc
  }

  lazy val isError = {
    log(Compile("checking %s for error", this))
    val dchildren = diagnosticChildrenList
    log(Compile("diagnosticChildren are: %s", dchildren))
    val bools = dchildren.map { dc =>
      try {
        dc.isError
      } catch {
        // We suppress errors here because a rethrow indicates that somebody else
        // has already recorded the exception in their diagnostics, and it was of the 
        // kind that can be recorded and issued later as a compile-time diagnostic.
        case e: OOLAGRethrowException => {
          log(Compile("isError is suppressing exception already recorded: %s", e))
          true
        }
      }
    }
    val childrenRes = bools.exists { x => x }
    // 
    // Don't forget to check the local diagnostic objects. It's not just about the 
    // children objects. The object itself might have accumulated the errors.
    //
    log(Compile("LD for %s are %s", this, localDiagnostics))
    val localRes =
      this.localDiagnostics.length > 0 &&
        this.localDiagnostics.exists { _.isError }
    val res = childrenRes || localRes
    if (res == true) {
      log(Compile("object %s had an error: %s", this, this.getDiagnostics))

    } else log(Compile("object %s ok", this))
    res
  }

  /**
   * Flat list of diagnostic objects.
   *
   * We try to avoid creating this every time a question is asked.
   */
  private lazy val diagnostics: Seq[Diagnostic] = {
    val dc = diagnosticChildrenList
    log(Compile("diagnosticChildren are: %s", dc))
    val dcDiags = dc.flatMap { dc =>
      dc match {
        case dp: DiagnosticsProviding => {
          try dp.diagnostics
          catch {
            // We suppress errors here because a rethrow indicates that somebody else
            // has already recorded the exception in their diagnostics, and it was of the 
            // kind that can be recorded and issued later as a compile-time diagnostic.
            case e: OOLAGRethrowException => {
              log(Compile("Diagnostics is suppressing exception already recorded: %s", e))
              Nil
            }
          }
        }
        case _ => {
          log(Compile("Diagnostics is ignoring: %s", dc))
          Nil
        }
      }
    }
    val ld = getLocalDiagnostics
    log(Compile("Object %s had localDiagnostics: %s", prettyName, ld))

    val res = dcDiags ++ ld
    log(Compile("Object %s had this list of diagnostics: %s", prettyName, res))
    res
  }

  /**
   * Override if there is a different way to obtain local diagnostic objects
   * than just those captured from throws
   */
  lazy val getLocalDiagnostics: Seq[Diagnostic] = {
    localDiagnostics.toSeq
  }

}

