/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.dsom

import org.xml.sax.SAXParseException
import org.apache.daffodil.xml.DaffodilXMLLoader
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.api._
import org.apache.daffodil.dsom.IIUtils._
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.oolag.OOLAG
import org.xml.sax.SAXException
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Misc

/**
 * represents one schema document file
 *
 * manages loading of it, and keeping track of validation errors
 */
final class DFDLSchemaFile(val sset: SchemaSet,
  schemaSourceArg: => DaffodilSchemaSource, // fileName, URL, or a scala.xml.Node
  val iiParent: IIBase,
  seenBeforeArg: IIMap)
  extends SchemaComponentImpl(<file/>, sset)
  with org.xml.sax.ErrorHandler {

  requiredEvaluations(isValid)

  private lazy val seenBefore = seenBeforeArg

  /**
   * Delegate back to the include or import that references us.
   *
   * This is the schema document we are contained in, not the one
   * we are referring to.
   */
  override lazy val schemaDocument = {
    // the one containing the reference to the file
    // Not the schema document in this file (that one is iiSchemaDocument).
    val res = iiParent.schemaDocument
    // the schemaDocument in this file is called iiSchemaDocument,
    // but you may only be interested in its XML characteristics (namespace
    // for example), in which case you want iiXMLSchemaDocument
    res
  }

  override lazy val diagnosticDebugName = schemaSource.uriForLoading.toString

  lazy val diagnosticChildren = Nil // no recursive descent. We just want the loader's validation errors.

  lazy val schemaSource = schemaSourceArg

  final override protected def enclosingComponentDef = None

  private var validationDiagnostics_ : Seq[Diagnostic] = Nil

  def validationDiagnostics = validationDiagnostics_

  def isValid: Boolean = {
    node // demanding this forces the load to happen
    val ld = validationDiagnostics
    // warnings won't stop things.
    // TODO: options to control when validation warnings
    // should be escalated to errors.
    val res = !ld.exists { d =>
      {
        val isE = d.isError
        isE
      }
    }
    res
  }

  def warning(exception: SAXParseException) = {
    val sdw = new SchemaDefinitionWarning(this.schemaFileLocation, "Warning loading schema due to %s", exception)
    warn(sdw)
    validationDiagnostics_ :+= sdw
  }

  def error(exception: SAXParseException) = {
    // val ex = exception
    val sde = new SchemaDefinitionError(this.schemaFileLocation, "Error loading schema due to %s", exception)
    error(sde)
    validationDiagnostics_ :+= sde
  }

  def fatalError(exception: SAXParseException) = {
    val sde = new SchemaDefinitionError(this.schemaFileLocation, "Fatal error loading schema due to %s", exception)
    validationDiagnostics_ :+= sde
    // parser throws out of fatalErrors.
  }

  private lazy val loader = {
    val ldr = new DaffodilXMLLoader(this)
    // val shouldValidate = sset.validateDFDLSchemas
    // ldr.setValidation(shouldValidate) // TODO: Validation not occurring JIRA DFDL-1473. Fix later.
    ldr
  }

  private def loadedNode = LV('loadedNode) {
    def die(e: Throwable) = {
      SDE("Error loading schema due to %s.", Misc.getSomeMessage(e).getOrElse("an unknown error."))
    }
    val node = try {
      log(LogLevel.Resolver, "Loading %s.", diagnosticDebugName)
      val node = loader.load(schemaSource)
      schemaDefinitionUnless(node != null, "No XML Node could be loaded from %s.", schemaSource)
      node
    } catch {
      case e: java.io.IOException => die(e)
      case e: SAXException => die(e)
    }
    node
  }.value

  lazy val node = loadedNode

  def iiXMLSchemaDocument = LV('iiXMLSchemaDocument) {
    val res = loadXMLSchemaDocument(seenBefore, Some(this))
    res
  }.value

  def iiSchemaDocument = LV('iiSchemaDocument) {
    val res = new SchemaDocument(iiXMLSchemaDocument)
    res
  }.value

  private def loadXMLSchemaDocument(before: IIMap, sf: Option[DFDLSchemaFile]) = {
    val sd = node match {
      case <schema>{ _* }</schema> if (NS(node.namespace) == XMLUtils.xsdURI) => {
        // top level is a schema.

        val sd = new XMLSchemaDocument(node, sset, Some(iiParent), sf, before, false)
        sd
      }
      case _ => {
        val ns = NS(node.namespace)
        schemaDefinitionError("The file %s did not contain a schema element as the document element. Found %s %s.", diagnosticDebugName, node.label, ns.explainForMsg)
      }
    }
    sd
  }

  lazy val seenAfter: IIMap = LV('seenAfter) {
    val res = OOLAG.keepGoing(seenBefore) {
      val aft = iiXMLSchemaDocument.seenAfter
      aft
    }
    res
  }.value
}
