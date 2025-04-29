/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.dsom

import java.io.File
import scala.xml.Elem

import org.apache.daffodil.core.dsom.IIUtils._
import org.apache.daffodil.lib.iapi._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.exceptions.XercesSchemaFileLocation
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.DaffodilXMLLoader
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dsom._

import org.xml.sax.SAXParseException

class DFDLSchemaFileLoadErrorHandler(schemaFileLocation: SchemaFileLocation)
  extends org.xml.sax.ErrorHandler {

  private var loaderErrors_ : Seq[SAXParseException] = Nil
  private var loaderWarnings_ : Seq[SAXParseException] = Nil

  private def reset(): Unit = {
    loaderErrors_ = Nil
    loaderWarnings_ = Nil
  }

  private def loaderErrors = loaderErrors_
  private def loaderWarnings = loaderWarnings_

  private def loaderSDEs: Seq[Diagnostic] = loaderErrors.map { err =>
    val errMessage = err.getMessage
    // we create a new SchemaFileLocation (xsfl) because the Xerces error has line, column and file info that
    // the original schemaFileLocation that's passed in doesn't contain, so we can pass in this more
    // complete SchemaFileLocation
    val xsfl = new XercesSchemaFileLocation(err, schemaFileLocation)
    new SchemaDefinitionError(
      xsfl,
      "Error loading schema due to %s",
      errMessage
    )
  }

  private def loaderSDWs: Seq[Diagnostic] = loaderWarnings.map { w =>
    val warnMessage = w.getMessage
    // we create a new SchemaFileLocation (xsfl) because the Xerces error has line and column info that
    // the original schemaFileLocation that's passed in doesn't contain, so we can pass in this more
    // complete SchemaFileLocation
    val xsfl = new XercesSchemaFileLocation(w, schemaFileLocation)
    new SchemaDefinitionWarning(
      WarnID.XmlParser,
      xsfl,
      "Warning loading schema due to %s",
      warnMessage
    )
  }

  def loadingDiagnostics = loaderSDEs ++ loaderSDWs

  /**
   * Converts the accumulated SAXParseErrors into SDEs and SDWs
   * and escalates a file validation error to a thrown SDE
   * so that the enclosing LV gets a failure due to these errors.
   *
   * Note: A common pattern for these error handlers is to implement
   * the trait/interface for the error handler in the object itself,
   * that is without a separate error handler object. This causes
   * trouble because the validator caches this object, so if this object
   * is connected to a large data structure, that will cause heavy
   * memory usage/leaking.
   *
   * Similarly, this object does not store the context object to avoid
   * holding onto it and all the compilation-time data structures
   * reachable from it.
   *
   * @param context The DFDLSchemaFile object where we're loading
   */
  def handleLoadErrors(context: DFDLSchemaFile): Unit = {
    loaderSDEs.foreach { context.error(_) }
    loaderSDWs.foreach { context.warn(_) }
    val optErr = loaderSDEs.headOption
    reset()
    optErr.foreach {
      context.toss(_) // escalate to a thrown SDE.
    }
  }

  def warning(exception: SAXParseException) = {
    loaderWarnings_ :+= exception
  }

  def error(exception: SAXParseException) = {
    loaderErrors_ :+= exception
  }

  /**
   * Called on a fatal exception. The parser/validator throws the exception after
   * this call returns.
   */
  def fatalError(exception: SAXParseException) = error(
    exception
  ) // same as non-fatal exception.
}

/**
 * represents one schema document file
 *
 * manages loading of it, and keeping track of validation errors
 */
final class DFDLSchemaFile(
  val sset: SchemaSet,
  schemaSourceArg: => DaffodilSchemaSource, // fileName, URL, or a scala.xml.Node
  val iiParent: IIBase,
  seenBeforeArg: IIMap
) extends SchemaComponentImpl(<file/>, sset) {

  private lazy val seenBefore = seenBeforeArg

  /**
   * Delegate back to the include or import that references us.
   *
   * This is the schema document we are contained in, not the one
   * we are referring to.
   */
  override lazy val optSchemaDocument = {
    // the one containing the reference to the file
    // Not the schema document in this file (that one is iiSchemaDocument).
    val res = iiParent.optSchemaDocument
    // the schemaDocument in this file is called iiSchemaDocument,
    // but you may only be interested in its XML characteristics (namespace
    // for example), in which case you want iiXMLSchemaDocument
    res
  }

  override lazy val optXMLSchemaDocument = iiParent.optXMLSchemaDocument

  override lazy val uriString = schemaSource.uriForLoading.toString

  override lazy val diagnosticFile: File = schemaSource.diagnosticFile

  override protected lazy val diagnosticDebugNameImpl = diagnosticFile.getPath

  lazy val diagnosticChildren =
    Nil // no recursive descent. We just want the loader's validation errors.

  lazy val schemaSource = schemaSourceArg

  lazy val (node, validationDiagnostics, isValid) = {
    val res =
      try {
        Logger.log.debug(s"Loading ${diagnosticDebugName}.")
        //
        // We do not want to validate here ever, because we have to examine the
        // root xs:schema element of a schema to decide if it is a  DFDL schema
        // at all that we're even supposed to compile.
        //
        // need line numbers for diagnostics
        val node =
          try {
            loader.load(schemaSource, None, addPositionAttributes = true)
          } catch {
            case e: SAXParseException => {
              // the loader can throw these due to e.g., doctype disallowed which is fatal.
              // It would be redundant to record it again.
              // So we simply ignore it.
              errHandler.error(e)
              null // node is null in this case.
            }
          }
        errHandler.handleLoadErrors(this)
        Assert.invariant(node != null)
        (node, errHandler.loadingDiagnostics, true)
      } catch {
        case e: java.io.IOException =>
          SDE(
            "Error loading schema due to %s.",
            Misc.getSomeMessage(e).getOrElse("an unknown error.")
          )
      }
    res
  }

  lazy val isDFDLSchemaFile = iiXMLSchemaDocument.isDFDLSchema

  private lazy val errHandler = new DFDLSchemaFileLoadErrorHandler(schemaFileLocation)
  private lazy val loader = new DaffodilXMLLoader(errHandler)

  lazy val isValidAsCompleteDFDLSchema: Boolean = LV(Symbol("isValidAsCompleteDFDLSchema")) {
    try {
      loader.validateAsCompleteDFDLSchema(schemaSource)
    } catch {
      case e: org.xml.sax.SAXParseException =>
        errHandler.error(e) // accumulate with error handler.
    } finally {
      errHandler.handleLoadErrors(this)
    }
    true
  }.value

  lazy val iiXMLSchemaDocument = LV(Symbol("iiXMLSchemaDocument")) {
    val res = makeXMLSchemaDocument(seenBefore, Some(this))
    if (res.isDFDLSchema && sset.shouldValidateDFDLSchemas) {
      //
      // We validate DFDL schemas, only if validation is requested.
      // Some things, tests generally, want to turn this validation off.
      //
      try {
        loader.validateAsIndividualDFDLSchemaFile(schemaSource)
      } catch {
        // validateAsIndividualDFDLSchemaFile doesn't always capture all exceptions in the
        // loader's error handler. Even for fatal errors it sometimes
        // just throws them.
        case e: org.xml.sax.SAXParseException =>
          errHandler.error(e) // accumulate with error handler.
      } finally {
        errHandler.handleLoadErrors(this)
      }
    }
    res
  }.value

  lazy val iiSchemaDocument = {
    val res = SchemaDocument(iiXMLSchemaDocument)
    res
  }

  private def makeXMLSchemaDocument(
    before: IIMap,
    sf: Option[DFDLSchemaFile]
  ): XMLSchemaDocument = {
    val sd = node match {
      case Elem(_, "schema", _, _, _*) if (NS(node.namespace) == XMLUtils.xsdURI) => {
        val sd = XMLSchemaDocument(node, sset, Some(iiParent), sf, before, false)
        sd
      }
      case _ => {
        val ns = NS(node.namespace)
        schemaDefinitionError(
          "The file %s did not contain a schema element as the document element. Found %s %s.",
          diagnosticDebugName,
          node.label,
          ns.explainForMsg
        )
      }
    }
    sd
  }

  lazy val seenAfter: IIMap = {
    val aft = iiXMLSchemaDocument.seenAfter
    aft
  }
}
