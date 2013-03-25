package edu.illinois.ncsa.daffodil.dsom

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

import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import scala.xml.Node

class SchemaDefinitionError(schemaContext: Option[SchemaComponentBase],
                            annotationContext: Option[DFDLAnnotation],
                            kind: String,
                            args: Any*)
  extends SchemaDefinitionDiagnosticBase(schemaContext, annotationContext, kind, args: _*) {

  def this(sc: SchemaComponent, kind: String, args: Any*) = this(Some(sc), None, kind, args: _*)
  val diagnosticKind = "Error"
}

class SchemaDefinitionWarning(schemaContext: Option[SchemaComponentBase],
                              annotationContext: Option[DFDLAnnotation],
                              kind: String,
                              args: Any*)
  extends SchemaDefinitionDiagnosticBase(schemaContext, annotationContext, kind, args: _*) {

  def this(sc: SchemaComponent, kind: String, args: Any*) = this(Some(sc), None, kind, args: _*)

  override def isError = false
  val diagnosticKind = "Warning"
}

abstract class SchemaDefinitionDiagnosticBase(
  val schemaContext: Option[SchemaComponentBase],
  val annotationContext: Option[DFDLAnnotation],
  val kind: String,
  val args: Any*) extends Exception with DiagnosticImplMixin {

  def diagnosticKind: String
  override def getLocationsInSchemaFiles: Seq[LocationInSchemaFile] = schemaContext.toList

  // TODO: Alternate constructor that allows data locations.
  // Because some SDEs are caught only once Processing starts. 
  // They're still SDE but they will have data location information.

  override def toString = {
    //
    // It is important that this routine is robust. It is used to print error messages
    // so if something goes wrong in this, you run around in circles. I believe the 
    // stack-overaflow problems will be caught so long as one is running through lazy val aka 
    // OOLAG 'attributes' framework. 
    //
    val res = {
      //
      // Right here is where we would lookup the symbolic error kind id, and 
      // choose a locale-based message string.
      //
      // For now, we'll just do an automatic English message.
      //
      val msg =
        if (kind.contains("%"))
          kind.format(args: _*)
        else {
          val argsAsString = args.map { _.toString }.mkString(", ")
          (kind + "(%s)").format(argsAsString)
        }

      // this is where it gets kind of hairy. We're depending on fairly rich
      // attribute calculations in order to generate the context information 
      // in these diagnostic messages. Break any of that stuff, and suddenly 
      // you will get circularity errors from OOLAG.
      // beats a stack-overflow at least.
      val schContextLocDescription =
        schemaContext.map { " " + _.locationDescription }.getOrElse("")

      val annContextLocDescription =
        annotationContext.map { " " + _.locationDescription }.getOrElse("")

      val res = "Schema Definition " + diagnosticKind + ": " + msg +
        " Schema context: " + schemaContext.getOrElse("top level") + "." +
        // TODO: should be one or the other, never(?) both
        schContextLocDescription +
        annContextLocDescription

      res
    }
    res
  }

  override def getMessage = toString
}

trait ImplementsThrowsSDE
  extends ThrowsSDE { self: SchemaComponentBase =>

  val NoAnnotationContext: Option[DFDLAnnotation] = None
  /**
   * Override in derived classes to create a member that
   * is of the right type.
   */
  //val context: DiagnosticsProviding = parent

  /**
   * Centralize throwing for debug convenience
   */
  def toss(th: Throwable) = {
    println(th)
    throw th // good place for a breakpoint
  }

  def schemaComponent: SchemaComponentBase

  // TODO: create a trait to share various error stuff with DFDLAnnotation class.
  // Right now there is small code duplication since annotations aren't schema components.
  def SDE(id: String, args: Any*): Nothing = {
    val sde = new SchemaDefinitionError(Some(schemaComponent), NoAnnotationContext, id, args: _*)
    toss(sde)
  }

  def SDEButContinue(id: String, args: Any*): Unit = {
    val sde = new SchemaDefinitionError(Some(schemaComponent), NoAnnotationContext, id, args: _*)
    error(sde)
  }

  def SDW(id: String, args: Any*): Unit = {
    val sdw = new SchemaDefinitionWarning(Some(schemaComponent), NoAnnotationContext, id, args: _*)
    warn(sdw)
  }

  /**
   * Use for cases where it is an SDE because of something we've chosen
   * not to implement. Not merely short term (haven't coded it yet, but intending to),
   * more like things we've chosen to defer intentionally to some future release.
   */
  def subset(testThatWillThrowIfFalse: Boolean, msg: String, args: Any*) = {
    if (!testThatWillThrowIfFalse) subsetError(msg, args: _*)
  }

  def subsetError(msg: String, args: Any*) = {
    val msgTxt = msg.format(args: _*)
    SDE("Subset " + msgTxt)
  }

}
