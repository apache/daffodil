package edu.illinois.ncsa.daffodil.dsom
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.HasIsError

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

trait DiagnosticImplMixin
  extends Diagnostic
  with HasIsError {
  def getSomeCause() = DiagnosticUtils.getSomeCause(this)
  def getSomeMessage() = DiagnosticUtils.getSomeMessage(this)
  def getLocationsInSchemaFiles(): Seq[LocationInSchemaFile] = Nil
  def getDataLocations(): Seq[DataLocation] = Nil
  def isError = true
}