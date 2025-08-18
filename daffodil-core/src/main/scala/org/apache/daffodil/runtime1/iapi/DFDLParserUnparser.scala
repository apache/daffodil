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

package org.apache.daffodil.runtime1.iapi

import scala.jdk.CollectionConverters.*

import org.apache.daffodil.api
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.lib.externalvars.Binding
import org.apache.daffodil.lib.iapi.*
import org.apache.daffodil.runtime1.processors.Failure
import org.apache.daffodil.runtime1.processors.ProcessorResult
import org.apache.daffodil.runtime1.processors.Success
import org.apache.daffodil.runtime1.processors.VariableMap

/**
 * This file contains traits that define an abstract API that any DFDL processor
 * can satisfy. The intention is to allow test rigs and other systems to run tests
 * against multiple DFDL implementations for comparison purposes.
 */

/**
 * APIs for DFDL Parsing and Unparsing
 *
 * Assumed that applications will provide information not all at once, but will provide first:
 * <ol>
 * <li> Schema (which can be multiple schema files by way of include/import)
 * <li> XML Catalog, or other means by which include/import are found.
 * <li> Control settings/features for how they want the parser to behave.
 * <li> Error handlers.
 * <li> Root node and namespace
 * <li> Externally defined variables
 * </ol>
 * After that, the application will want to use one of two primary patterns:
 * <ul>
 * <li> Random access: one input source, evaluate many different paths
 * <li> Bulk Parsing: one path, many different input sources
 * </ul>
 * The output direction is symmetric.
 *
 * So the API provides a way to set the path specially, providing for schema-aware compilation of the XPath expression in the bulk processing case.
 *
 */
object DFDL {

  type Output = java.nio.channels.WritableByteChannel // binary output stream/buffer

  trait Compiler extends api.Compiler {

    /**
     * User must establish error handlers, setup features appropriately before using the data access objects.
     *
     * Other features are TBD, but may include
     *
     * <ul>
     * <li>validation checking of the data - whether to warn or error.
     * <li>schema validation - validation of the DFDL Schema itself
     * <li>error recovery information - paths to anchor elements or similar
     * <li>error and warning tolerances - how many before failing entirely, escalation for warnings
     * <li>debug modes & tracing modes
     * <li>amount of buffering to make available for backward reference (0 meaning no backward reference)
     * <li>amount of buffering to make available for lookahead/speculation - bounding this can make error detection/recovery faster
     * <li>grammar ambiguity policies - whether to escalate certain ambiguous use of delimiters to errors, or try to parse anyway
     * </ul>
     */

    /**
     * There are two places you can set the root element, or you can omit it.
     * You can set it here on the compiler. This saves compilation time, as
     * it will only compile things needed by your root element. (Good to make
     * tests run fast.) If you don't specify a root, then it compiles all top
     * level elements, anticipating that different uses of the compiled schema
     * might subsequently ask for different top level elements.
     *
     * If you don't specify a root here, then you can specify one later on the
     * ProcessorFactory object, but that won't save you as much compilation time.
     *
     * If you don't set a root at all, then it compiles all top level elements
     * and uses the first element in the first schema file as the root.
     *
     * When specifying a root element, you either specify its namespace, or
     * you can specify the empty string "" as the namespace which means "no namespace".
     * If you specify a root element name, but pass null as the namespace, then
     * it will search for a unique element with your root element name, and
     * if that is unambiguous, it will use it as the root.
     */

    /**
     * Compilation returns a [[ProcessorFactory]], which must be interrogated for diagnostics
     * to see if compilation was successful or not.
     */
    def compileSource(
      schemaSource: DaffodilSchemaSource,
      optRootNodeName: Option[String] = None,
      optRootNodeNamespace: Option[String] = None
    ): api.ProcessorFactory
  }

  /**
   * The point of [[ProcessorFactory]] is to allow compilation of the path expression
   * and/or generation of source code to process data matching the compiled schema
   */
  trait ProcessorFactory extends WithDiagnostics with api.ProcessorFactory

  trait DataProcessor extends api.DataProcessor with WithDiagnostics {
    def withExternalVariables(extVars: Seq[Binding]): DataProcessor
    def tunables: DaffodilTunables
    def variableMap: VariableMap
  }

  /**
   * Interface of a ContentHandler providing an additional function to the
   * UnparseResult. Used to unparse and infoset represented as SAX events from an
   * XMLReader to data.
   */
  trait DaffodilUnparseContentHandler extends api.DaffodilUnparseContentHandler {
    def enableResolutionOfRelativeInfosetBlobURIs(): Unit
  }

  /**
   * Thrown by the DaffodilUnparseContentHandler when an a Daffodil UnparseResult
   * fails because of an error. The DaffodilUnparseContentHandler.getUnparseResult
   * can be used to get the result and any diagnostics.
   */
  class DaffodilUnparseErrorSAXException(unparseResult: UnparseResult)
    extends api.exceptions.DaffodilUnparseErrorSAXException(
      unparseResult.getDiagnostics.asScala.mkString("\n")
    )

  /**
   * Thrown by the DaffodilUnparseConentHandler when an unexpected error
   * occurs, this usually represents a bug in Daffodil
   */
  class DaffodilUnhandledSAXException(description: String, cause: Exception)
    extends api.exceptions.DaffodilUnhandledSAXException(description, cause) {
    def this(msg: String) = this(msg, null)

    def this(cause: Exception) = this(null, cause)
  }

  trait ParseResult extends Result with api.ParseResult

  trait UnparseResult extends Result with api.UnparseResult {

    /**
     * Data is 'scannable' if it consists entirely of textual data, and that data
     * is all in the same encoding.
     */
    def isScannable: Boolean
    def encodingName: String
  }

  /**
   * Interface for Parse and Unparse states
   */
  trait State {

    /**
     * @return a processorResult detailing the status of the processor
     */
    def processorStatus: ProcessorResult

    /**
     * @return validation status
     */
    def validationStatus: Boolean

    /**
     * @return list of diagnostics
     */
    def diagnostics: Seq[Diagnostic]

    /**
     * @return current data location
     */
    def currentLocation: DataLocation
  }

  /**
   * Interface for Parse and Unparse results
   */
  abstract class Result extends api.Result with WithDiagnostics {
    def resultState: State
    var diagnostics: Seq[api.Diagnostic] = Nil

    private def resultStatusDiagnostics: Seq[api.Diagnostic] = {
      resultState.processorStatus match {
        case Failure(c) => List(c)
        case Success => Nil
      }
    }

    override def getDiagnostics: java.util.List[api.Diagnostic] = {
      (diagnostics ++ resultState.diagnostics ++ resultStatusDiagnostics).distinct.asJava
    }

    def addDiagnostic(d: api.Diagnostic): Unit = {
      diagnostics = d +: diagnostics
    }

    override def isError: Boolean = isProcessingError || isValidationError
    override def isProcessingError: Boolean = resultState.processorStatus != Success
    override def isValidationError: Boolean = !resultState.validationStatus
  }
}
