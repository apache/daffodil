package daffodil.dsom

import java.io._
import scala.xml.Node
import scala.xml.XML
import daffodil.api.DFDL
import daffodil.exceptions._
import daffodil.util.Validator
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

class CapturedThrowDiagnostic(e : Throwable, context : Any) extends Diagnostic {
  Assert.invariant(!e.isInstanceOf[OOLAGRethrowException])
  def isError() = true
  def getSchemaLocations() = Nil
  def getDataLocations() = Nil
  def getMessage() = "ERROR caught: " + e + ". Context at catch was: " + context
}

class Warning(e : Throwable) extends Diagnostic {
  Assert.invariant(!e.isInstanceOf[OOLAGRethrowException])
  def isError() = false
  def getSchemaLocations() = Nil
  def getDataLocations() = Nil
  def getMessage() = "WARNING: " + e
}

/**
 * Implementation of OOLAG diagnostics gathering for Daffodil (daffodil being an application of OOLAG technique)
 *
 * This is for compilation. Not the runtime.
 */
trait DiagnosticsProviding extends OOLAGHost with HasIsError {
  type DiagnosticsList = Seq[HasIsError]

  lazy val LV = LVFactory(this)

  def handleThrownError(lv : OOLAGValue) {
    val e = lv.thrown
    log(Debug("Object %s recording error: %s", this, e))
    val diag = e match {
      case d : Diagnostic => d
      case _ => new CapturedThrowDiagnostic(e, lv.context) // no common trait to use....?
    }
    addDiagnostic(diag)
    addDiagnosticChild(lv)
    lv.context match {
      case dp : Prod => {
        dp.sc.addDiagnosticHost(dp) // TODO: determine is this really needed?
      }
      case _ => log(Debug("type of lv.context is %s", lv.context.getClass().getName()))
    }
  }

  def handleWarning(lv : OOLAGValue, th : Throwable) {
    val diag = new Warning(th)
    addDiagnostic(diag)
  }

  protected var localDiagnostics : Seq[Diagnostic] = Nil
  private var diagChildren : Seq[OOLAGValue] = Nil
  private var diagHosts : Seq[DiagnosticsProviding] = Nil

  def addDiagnostic(diag : Diagnostic) {
    localDiagnostics = localDiagnostics :+ diag
    log(Compile("Adding Diagnostic: %s to %s", diag, path))
  }

  private def addDiagnosticChild(lv : OOLAGValue) {
    diagChildren = diagChildren :+ lv
    val lvmsg = lv.toString
    val thismsg = this.prettyName
    log(Debug("Adding Diagnostic child %s to %s.", lvmsg, thismsg))
  }

  def addDiagnosticHost(h : DiagnosticsProviding) {
    diagHosts = diagHosts :+ h
    log(Debug("Adding Diagnostic host %s to %s.", h, this))
  }

  //  final lazy val hasDiagnostics = {
  //    getDiagnostics.size > 0
  //  }

  final lazy val getDiagnostics = getDiagnostics_.value
  private lazy val getDiagnostics_ = LV {
    val res = diagnostics.toSeq // TODO: Sort into file/schema ordering so that earlier diagnostics are about
    // earlier parts of the schema.
    res
  }

  def diagnosticChildren : DiagnosticsList

  lazy val isError = {
    log(Debug("checking %s for error", this))
    val dchildren = diagnosticChildren // force lazy value for sake of debugging
    log(Debug("diagnosticChildren are: %s", dchildren))
    val bools = dchildren.map { dc =>
      try {
        dc.isError
      } catch {
        // We suppress errors here because a rethrow indicates that somebody else
        // has already recorded the exception in their diagnostics, and it was of the 
        // kind that can be recorded and issued later as a compile-time diagnostic.
        case e : OOLAGRethrowException => {
          log(Debug("isError is suppressing exception already recorded: %s", e))
          true
        }
      }
    }
    val res = bools.exists { x => x }
    if (res == true) log(Debug("object %s had an error", this))
    res
  }

  /**
   * Flat list of diagnostic objects.
   *
   * We try to avoid creating this every time a question is asked.
   */
  private lazy val diagnostics : Seq[Diagnostic] = diagnostics_.value
  private lazy val diagnostics_ = LV {
    val dChildren = diagnosticChildren
    log(Debug("diagnosticChildren are: %s", dChildren))
    val dc = dChildren ++ diagHosts.toSeq
    val dcDiags = dc.flatMap { dc =>
      dc match {
        case dp : DiagnosticsProviding => {
          try dp.diagnostics
          catch {
            // We suppress errors here because a rethrow indicates that somebody else
            // has already recorded the exception in their diagnostics, and it was of the 
            // kind that can be recorded and issued later as a compile-time diagnostic.
            case e : OOLAGRethrowException => {
              log(Debug("Diagnostics is suppressing exception already recorded: %s", e))
              Nil
            }
          }
        }
        case _ => {
          log(Debug("Diagnostics is ignoring: %s", dc))
          Nil
        }
      }
    }
    val ld = getLocalDiagnostics
    log(Debug("Object %s had localDiagnostics: %s", prettyName, ld))

    val res = dcDiags ++ ld
    log(Debug("Object %s had this list of diagnostics: %s", prettyName, res))
    res
  }

  /**
   * Override if there is a different way to obtain local diagnostic objects
   * than just those captured from throws
   */
  lazy val getLocalDiagnostics : Seq[Diagnostic] = {
    localDiagnostics.toSeq
  }

}


