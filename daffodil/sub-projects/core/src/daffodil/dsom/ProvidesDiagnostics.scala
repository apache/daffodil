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

class CapturedThrowDiagnostic(e : Throwable, context : Any) extends Diagnostic {
  Assert.invariant(!e.isInstanceOf[OOLAGRethrowException])
  def isError() = true
  def getSchemaLocations() = Nil
  def getDataLocations() = Nil
  def getMessage() = "ERROR (Catch):" + e + ". Context at catch was: " + context
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
    log(Debug("thrown error caught: %s", e))
    val diag = e match {
      case d : Diagnostic => d
      case _ => new CapturedThrowDiagnostic(e, this) // no common trait to use....?
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

  private var localDiagnostics : Seq[Diagnostic] = Nil
  private var diagChildren : Seq[OOLAGValue] = Nil
  private var diagHosts : Seq[DiagnosticsProviding] = Nil

  private def addDiagnostic(diag : Diagnostic) {
    localDiagnostics = localDiagnostics :+ diag
    log(Debug("Adding Diagnostic: %s to %s", diag, path))
  }

  private def addDiagnosticChild(lv : OOLAGValue) {
    diagChildren = diagChildren :+ lv
    val lvmsg= lv.toString
    val thismsg = this.prettyName
    log(Debug("Adding Diagnostic child %s to %s.", lvmsg, thismsg))
  }

  def addDiagnosticHost(h : DiagnosticsProviding) {
    diagHosts = diagHosts :+ h 
    log(Debug("Adding Diagnostic host %s to %s.", h, this))
  }

  final lazy val hasDiagnostics = {
    getDiagnostics.size > 0
  }

  final lazy val getDiagnostics = getDiagnostics_.value
  private lazy val getDiagnostics_ = LV{
    val res = diagnostics.toSeq // TODO: Sort into file/schema ordering so that earlier diagnostics are about
    // earlier parts of the schema.
    res
  }

  def diagnosticChildren : DiagnosticsList

  lazy val isError = {
    diagnosticChildren.map { dc =>
      log(Debug("checking %s for error", dc))
      try dc.isError
      catch {
        case e : OOLAGException => true
      }
    }.exists { x => x }
  }

  /**
   * Flat list of diagnostic objects.
   *
   * We try to avoid creating this every time a question is asked.
   */
  private lazy val diagnostics : Seq[Diagnostic] = diagnostics_.value
  private lazy val diagnostics_ = LV {
    val dc = 
      try diagnosticChildren ++ diagHosts.toSeq
      catch { case e : OOLAGException => Nil } // TODO: ? how do we accumulate the error from children if we Nil here and therefore have no list of children?
    val dcDiags = dc.flatMap { dc =>
      dc match {
        case dp : DiagnosticsProviding => dp.diagnostics
          // try dp.diagnostics catch { case e => Nil }
        case _ => Nil
      }
    }
    val res = dcDiags ++ getLocalDiagnostics
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


