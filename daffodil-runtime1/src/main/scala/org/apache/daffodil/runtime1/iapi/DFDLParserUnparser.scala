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

import java.io.File

import org.apache.daffodil.api.MetadataHandler
import org.apache.daffodil.api.compiler.{ ProcessorFactory => JProcessorFactory }
import org.apache.daffodil.api.debugger.{ Debugger => JDebugger }
import org.apache.daffodil.api.exceptions.{
  DaffodilUnhandledSAXException => JDaffodilUnhandledSAXException
}
import org.apache.daffodil.api.exceptions.{
  DaffodilUnparseErrorSAXException => JDaffodilUnparseErrorSAXException
}
import org.apache.daffodil.api.infoset.{ InfosetInputter => JInfosetInputter }
import org.apache.daffodil.api.infoset.{ InfosetOutputter => JInfosetOutputter }
import org.apache.daffodil.api.validation.{ ValidationResult => JValidationResult }
import org.apache.daffodil.api.{ CodeGenerator => JCodeGenerator }
import org.apache.daffodil.api.{ Compiler => JCompiler }
import org.apache.daffodil.api.{ DaffodilParseXMLReader => JDaffodilParseXMLReader }
import org.apache.daffodil.api.{
  DaffodilUnparseContentHandler => JDaffodilUnparseContentHandler
}
import org.apache.daffodil.api.{ DataProcessor => JDataProcessor }
import org.apache.daffodil.api.{ Diagnostic => JDiagnostic }
import org.apache.daffodil.api.{ InputSourceDataInputStream => JInputSourceDataInputStream }
import org.apache.daffodil.api.{ ParseResult => JParseResult }
import org.apache.daffodil.api.{ Result => JResult }
import org.apache.daffodil.api.{ State => JState }
import org.apache.daffodil.api.{ UnparseResult => JUnparseResult }
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.externalvars.Binding
import org.apache.daffodil.lib.iapi._
import org.apache.daffodil.runtime1.processors.Failure
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

  trait Compiler extends JCompiler {

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
     * Compilation returns a [[org.apache.daffodil.api.compiler.ProcessorFactory]], which must be interrogated for diagnostics
     * to see if compilation was successful or not.
     */
    def compileSource(
      schemaSource: DaffodilSchemaSource,
      optRootNodeName: Option[String] = None,
      optRootNodeNamespace: Option[String] = None
    ): JProcessorFactory

    def reload(savedParser: File): JDataProcessor
  }

  /**
   * The point of [[ProcessorFactory]] is to allow compilation of the path expression
   * and/or generation of source code to process data matching the compiled schema
   */
  trait ProcessorFactory extends WithDiagnostics with JProcessorFactory {

    /**
     * Returns a [[org.apache.daffodil.api.CodeGenerator]] to generate code from a DFDL schema to parse or unparse data
     * @param language source language for generated code (you can use only "c" at this time)
     */
    def forLanguage(language: String): JCodeGenerator
  }

  /**
   * Source code generation and compilation is performed with a language-specific [[CodeGenerator]],
   * which must be interrogated for diagnostics to see if each call was successful or not.
   */
  trait CodeGenerator extends JCodeGenerator with WithDiagnostics

  trait DataProcessor extends JDataProcessor with WithDiagnostics {

    /**
     * Returns a data processor with all the same state, but the validation mode changed to that of the argument.
     *
     * Note that the default validation mode is "off", that is, no validation is performed.
     */
    def withTunable(name: String, value: String): DataProcessor
    def withTunables(tunables: Map[String, String]): DataProcessor
    def withExternalVariables(extVars: Map[String, String]): DataProcessor
    def withExternalVariables(extVars: File): DataProcessor
    def withExternalVariables(extVars: Seq[Binding]): DataProcessor
//    def withDebugger(dbg: AnyRef): DataProcessor
    def withDebugging(flag: Boolean): DataProcessor

    def save(output: DFDL.Output): Unit

    def walkMetadata(handler: MetadataHandler): Unit
    def tunables: DaffodilTunables
    def variableMap: VariableMap

    /**
     * Creates a new instance of XMLReader for SAX Parsing
     */
    def newXMLReaderInstance: DaffodilParseXMLReader

    /**
     * Creates a new instance of DaffodilUnparseContentHandler for SAX Unparsing
     */
    def newContentHandlerInstance(output: DFDL.Output): DaffodilUnparseContentHandler

    /**
     * Unparses (that is, serializes) data to the output, returns an object which contains any diagnostics.
     */
    def unparse(input: JInfosetInputter, output: DFDL.Output): UnparseResult

    /**
     * Returns an object which contains the result, and/or diagnostic information.
     */
    def parse(input: JInputSourceDataInputStream, output: JInfosetOutputter): ParseResult

    def withDebugger(dbg: JDebugger): DataProcessor
  }

  /**
   * Interface of an XMLReader providing additional parse functions to support that
   * data that Daffodil usually expects. Used to parse data to a infoset represent as
   * SAX events
   */
  trait DaffodilParseXMLReader extends JDaffodilParseXMLReader {
    def parse(is: java.io.InputStream): Unit
    def parse(in: InputSourceDataInputStream): Unit
    def parse(ab: Array[Byte]): Unit
    def parse(in: JInputSourceDataInputStream): Unit = {
      parse(in.asInstanceOf[InputSourceDataInputStream])
    }
  }

  /**
   * Interface of a ContentHandler providing an additional function to the
   * UnparseResult. Used to unparse and infoset represented as SAX events from an
   * XMLReader to data.
   */
  trait DaffodilUnparseContentHandler extends JDaffodilUnparseContentHandler {
    def getUnparseResult: UnparseResult
    def enableResolutionOfRelativeInfosetBlobURIs(): Unit
  }

  /**
   * Thrown by the DaffodilUnparseContentHandler when an a Daffodil UnparseResult
   * fails because of an error. The DaffodilUnparseContentHandler.getUnparseResult
   * can be used to get the result and any diagnostics.
   */
  class DaffodilUnparseErrorSAXException(unparseResult: UnparseResult)
    extends JDaffodilUnparseErrorSAXException(unparseResult.getDiagnostics.mkString("\n"))

  /**
   * Thrown by the DaffodilUnparseConentHandler when an unexpected error
   * occurs, this usually represents a bug in Daffodil
   */
  class DaffodilUnhandledSAXException(description: String, cause: Exception)
    extends JDaffodilUnhandledSAXException(description, cause) {
    def this(msg: String) = this(msg, null)

    def this(cause: Exception) = this(null, cause)
  }

  trait ParseResult extends Result with WithDiagnostics with JParseResult {
    def resultState: State
    val validationResult: Option[JValidationResult]
  }

  trait UnparseResult extends Result with JUnparseResult with WithDiagnostics {

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
  trait State extends JState

  /**
   * Interface for Parse and Unparse results
   */
  abstract class Result extends JResult {
    var diagnostics: Seq[JDiagnostic] = Nil

    private def resultStatusDiagnostics: Seq[Diagnostic] = {
      resultState.processorStatus match {
        case Failure(c) => List(c)
        case Success => Nil
      }
    }

    override def getDiagnostics: Seq[JDiagnostic] = {
      (diagnostics.toSet ++ resultState.diagnostics.toSet ++ resultStatusDiagnostics.toSet).toSeq
    }

    override def addDiagnostic(d: JDiagnostic): Unit = {
      diagnostics = d +: diagnostics
    }

    override def isError = isProcessingError || isValidationError
    override def isProcessingError = resultState.processorStatus != Success
    override def isValidationError = resultState.validationStatus != true
  }
}
