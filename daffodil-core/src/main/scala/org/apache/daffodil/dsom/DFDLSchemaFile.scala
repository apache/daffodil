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

package org.apache.daffodil.dsom

import org.xml.sax.SAXParseException
import org.apache.daffodil.xml.DaffodilXMLLoader
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.api._
import org.apache.daffodil.dsom.IIUtils._
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.oolag.OOLAG
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Misc

/**
 * represents one schema document file
 *
 * manages loading of it, and keeping track of validation errors
 */
final class DFDLSchemaFile(
  val sset: SchemaSet,
  schemaSourceArg: => DaffodilSchemaSource, // fileName, URL, or a scala.xml.Node
  val iiParent: IIBase,
  seenBeforeArg: IIMap)
  extends SchemaComponentImpl(<file/>, sset)
  with org.xml.sax.ErrorHandler {

  requiredEvaluationsAlways(isValid)

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

  override lazy val xmlSchemaDocument = iiParent.xmlSchemaDocument

  override lazy val uriString = schemaSource.uriForLoading.toString

  override lazy val diagnosticDebugName = schemaSource.uriForLoading.toString

  lazy val diagnosticChildren = Nil // no recursive descent. We just want the loader's validation errors.

  lazy val schemaSource = schemaSourceArg

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
    val sde = new SchemaDefinitionError(this.schemaFileLocation, "Error loading schema due to %s", exception)
    error(sde)
    validationDiagnostics_ :+= sde
  }

  /**
   * Called on a fatal exception. The parser/validator throws the exception after
   * this call returns.
   */
  def fatalError(exception: SAXParseException) = error(exception) // same as non-fatal exception.

  private def loadedNode = LV('loadedNode) {
    def die(e: Throwable) = {
      SDE("Error loading schema due to %s.", Misc.getSomeMessage(e).getOrElse("an unknown error."))
    }
    val node = try {
      log(LogLevel.Resolver, "Loading %s.", diagnosticDebugName)
      val ldr = new DaffodilXMLLoader(this)
      //
      // We do not want to validate here ever, because we have to examine the
      // root xs:schema eleemnt of a schema to decide if it is a  DFDL schema
      // at all that we're even supposed to compile.
      //
      ldr.setValidation(false)
      val node = ldr.load(schemaSource)
      schemaDefinitionUnless(node != null, "Unable to load XML from %s.", diagnosticDebugName)
      node
    } catch {
      case e: java.io.IOException => die(e)
    }
    node
  }.value

  lazy val node = loadedNode

  lazy val isDFDLSchemaFile = iiXMLSchemaDocument.isDFDLSchema

  lazy val iiXMLSchemaDocument = LV('iiXMLSchemaDocument) {
    val res = loadXMLSchemaDocument(seenBefore, Some(this))
    if (res.isDFDLSchema && sset.validateDFDLSchemas) {
      //
      // We validate DFDL schemas, only if validation is requested.
      // Some things, tests generally, want to turn this validation off.
      //

      val ldr = new DaffodilXMLLoader(this)
      ldr.setValidation(true)
      try {
        ldr.load(schemaSource) // validate as XML file with XML Schema for DFDL Schemas
        ldr.validateSchema(schemaSource) // validate as XSD (catches UPA errors for example)
      } catch {
        // ok to absorb SAX Parse Exception as we've captured those errors in error handling
        // elsewhere.
        case _: org.xml.sax.SAXParseException => // ok

        //
        // Leaving this commented code in, to document that it
        // is a BAD IDEA to catch Exception. A more specific exception may be ok.
        // if you catch Exception, this will mask errors like Null Pointer Exceptions.
        //
        // This catch of Exception had been put here due to problems with circular definitions
        // occurring during issuing of an SDE. If the computation of the error object
        // such as the file name, proper schema object to blame, etc. if those can
        // themselves cause an SDE, then we end up in a circular definition.
        //
        // However, masking all Exceptions is NOT the right way to fix this.
        // Use of OOLAG LV's toOption method is a better way.
        //
        // case e: Exception =>
        //   Assert.invariantFailed("Unexpected exception type " + e)
      }
    }
    res
  }.value

  def iiSchemaDocument = LV('iiSchemaDocument) {
    val res = new SchemaDocument(iiXMLSchemaDocument)
    res
  }.value

  private def loadXMLSchemaDocument(before: IIMap, sf: Option[DFDLSchemaFile]): XMLSchemaDocument = {
    val sd = node match {
      case <schema>{ _* }</schema> if (NS(node.namespace) == XMLUtils.xsdURI) => {
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
