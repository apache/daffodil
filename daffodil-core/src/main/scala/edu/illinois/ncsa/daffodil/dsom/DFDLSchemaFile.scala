package edu.illinois.ncsa.daffodil.dsom

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

import org.xml.sax.SAXParseException
import edu.illinois.ncsa.daffodil.xml.DaffodilXMLLoader
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.NS
import java.net.URL
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import java.io.File
import edu.illinois.ncsa.daffodil.dsom.IIUtils._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.HasIsError
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG
import java.net.URI
import edu.illinois.ncsa.daffodil.util.Misc
import org.xml.sax.SAXException
import edu.illinois.ncsa.daffodil.util.LogLevel
import org.xml.sax.InputSource

/**
 * represents one schema document file
 *
 * manages loading of it, and keeping track of validation errors
 */
class DFDLSchemaFile(val sset: SchemaSet,
  sourceOfSchema: => Any, // fileName, URL, or a scala.xml.Node
  val iiParent: IIBase,
  seenBeforeArg: IIMap)
  extends SchemaComponent(<file/>, sset)
  with org.xml.sax.ErrorHandler {

  requiredEvaluations(isValid)

  lazy val seenBefore = seenBeforeArg

  //  /**
  //   * Delegate back to the include or import that references us.
  //   * 
  //   * This is the schema document we are contained in, not the one
  //   * we are referring to.
  //   */
  // lazy val schemaDocument = Assert.invariantFailed("schemaDocument called on schemaFile. You wan't to call schemaDocument on the include/import object.")
  override lazy val schemaDocument = {
    // the one containing the reference to the file
    // Not the schema document in this file (that one is iiSchemaDocument).    
    val res = iiParent.schemaDocument
    // the schemaDocument in this file is called iiSchemaDocument,
    // but you may only be interested in its XML characteristics (namespace 
    // for example), in which case you want iiXMLSchemaDocument
    res
  }

  override lazy val prettyName = uri.toString

  lazy val diagnosticChildren = Nil // no recursive descent. We just want the loader's validation errors.

  lazy val fakeURI = new File("tempFile.xsd").toURI

  lazy val uri = sourceOfSchema match {
    case fn: String => new File(fn).toURI
    case uri: URI => uri
    case n: scala.xml.Node => fakeURI
    case _ => Assert.usageError("sourceOfSchema must be a fileName string, a URL or a schema node")
  }

  override lazy val enclosingComponent = None

  var validationDiagnostics_ : Seq[Diagnostic] = Nil

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
    val ex = exception
    val sde = new SchemaDefinitionError(this.schemaFileLocation, "Error loading schema due to %s", exception)
    error(sde)
    validationDiagnostics_ :+= sde
  }

  def fatalError(exception: SAXParseException) = {
    val sde = new SchemaDefinitionError(this.schemaFileLocation, "Fatal error loading schema due to %s", exception)
    validationDiagnostics_ :+= sde
    // parser throws out of fatalErrors.
  }

  lazy val loader = new DaffodilXMLLoader(this)
  lazy val resolver = schemaSet.resolver

  lazy val loadedNode = loadedNode_.value
  private val loadedNode_ = LV('loadedNode) {
    def die(e: Throwable) = {
      SDE("Error loading schema due to %s.", DiagnosticUtils.getSomeMessage(e).getOrElse("an unknown error."))
    }
    val node = try {
      log(LogLevel.Resolver, "Loading %s.", uri)
      loader.load(uri)
    } catch {
      case e: java.io.IOException => die(e)
      case e: SAXException => die(e)
    }
    node
  }

  lazy val node =
    sourceOfSchema match {
      case schemaNode: scala.xml.Node => schemaNode
      case _ => loadedNode
    }

  //  lazy val seenBeforePlusThis = {
  //    val res = if (ii.notSeenThisBefore) {
  //      seenBefore + ii.mapTuple
  //    } else seenBefore
  //    res
  //  }

  lazy val iiXMLSchemaDocument = iiXMLSchemaDocument_.value
  val iiXMLSchemaDocument_ = LV('iiXMLSchemaDocument) {
    val res = loadXMLSchemaDocument(seenBefore, Some(this))
    res
  }

  lazy val iiSchemaDocument = iiSchemaDocument_.value
  private val iiSchemaDocument_ = LV('iiSchemaDocument) {
    val res = new SchemaDocument(iiXMLSchemaDocument)
    res
  }

  def loadXMLSchemaDocument(before: IIMap, sf: Option[DFDLSchemaFile]) = {
    val sd = node match {
      case <schema>{ _* }</schema> if (NS(node.namespace) == XMLUtils.xsdURI) => {
        // top level is a schema. 

        val sd = new XMLSchemaDocument(node, sset, Some(iiParent), sf, before)
        sd
      }
      case _ => schemaDefinitionError("The file %s did not contain a schema element as the document element. Found %s in namespace %s.", uri, node.label, node.namespace)
    }
    sd
  }

  lazy val seenAfter: IIMap = seenAfter_.value
  private val seenAfter_ = LV('seenAfter) {
    val res = OOLAG.keepGoing(seenBefore) {
      val aft = iiXMLSchemaDocument.seenAfter
      aft
    }
    res
  }
}
