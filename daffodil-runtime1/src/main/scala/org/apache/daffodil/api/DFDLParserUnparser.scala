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

package org.apache.daffodil.api

import org.apache.daffodil.processors.ProcessorResult
import org.apache.daffodil.processors.Success
import java.io.File

import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.infoset.InfosetInputter
import org.apache.daffodil.infoset.InfosetOutputter
import org.apache.daffodil.processors.Failure
import org.apache.daffodil.io.InputSourceDataInputStream

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

  trait Compiler {

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
    @deprecated("Use arguments to compileSource, or compileFile method.", "2.6.0")
    def setDistinguishedRootNode(name: String, namespace: String): Unit

    @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
    def setExternalDFDLVariable(name: String, namespace: String, value: String): Unit

    /**
     * Compilation returns a parser factory, which must be interrogated for diagnostics
     * to see if compilation was successful or not.
     */
    def compileSource(
      schemaSource: DaffodilSchemaSource,
      optRootNodeName: Option[String] = None,
      optRootNodeNamespace: Option[String] = None): ProcessorFactory

    def reload(savedParser: File): DataProcessor
  }

  /**
   * The point of processor factory is to allow compilation of the path expression.
   */
  trait ProcessorFactory extends WithDiagnostics {

    /**
     * If you didn't set a root on the compiler, then get another
     * chance to specify one here.
     *
     * If you don't set a root at all it uses the first element in the
     * first schema document as the root.
     *
     * If you don't specify a namespace, or pass null, then it searches, and if
     * it is unambiguous, it will use the unique global element with
     * that name.
     *
     * Note: null used specifically here not an Option type, because this API
     * will shine through to a Java API.
     *
     * To explicitly specify that there is no-namespace, pass "" as
     * the namespace argument.
     */
    @deprecated("Use arguments to Compiler.compileSource or compileFile.", "2.6.0")
    def setDistinguishedRootNode(name: String, namespace: String = null): Unit

    def onPath(xpath: String): DataProcessor
  }

  trait DataProcessor extends WithDiagnostics {

    /**
     * Returns a data processor with all the same state, but the validation mode changed to that of the argument.
     *
     * Note that the default validation mode is "off", that is, no validation is performed.
     */
    def withValidationMode(mode:ValidationMode.Type): DataProcessor
    def withTunable(name: String, value: String): DataProcessor
    def withTunables(tunables: Map[String, String]): DataProcessor
    def withExternalVariables(extVars: Map[String, String]): DataProcessor
    def withExternalVariables(extVars: File): DataProcessor
    def withExternalVariables(extVars: Seq[Binding]): DataProcessor

    def validationMode: ValidationMode.Type

    def getTunables(): DaffodilTunables
    def save(output: DFDL.Output): Unit
    def variableMap: VariableMap

    @deprecated("Use withValidationMode.", "2.6.0")
    def setValidationMode(mode: ValidationMode.Type): Unit
    @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
    def setExternalVariables(extVars: Map[String, String]): Unit
    @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
    def setExternalVariables(extVars: File): Unit
    @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
    def setExternalVariables(extVars: File, tunable: DaffodilTunables): Unit
    @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
    def setExternalVariables(extVars: Seq[Binding]): Unit
    @deprecated("Use withTunables.", "2.6.0")
    def setTunable(tunable: String, value: String): Unit
    @deprecated("Use withTunables.", "2.6.0")
    def setTunables(tunables: Map[String,String]): Unit

    /**
     * Creates a new instance of XMLReader for SAX Parsing/Unparsing
     */
    def newXMLReaderInstance: DaffodilXMLReader

    /**
     * Unparses (that is, serializes) data to the output, returns an object which contains any diagnostics.
     */
    def unparse(input: InfosetInputter, output: DFDL.Output): UnparseResult

    /**
     * Returns an object which contains the result, and/or diagnostic information.
     */
    def parse(input: InputSourceDataInputStream, output: InfosetOutputter): ParseResult
  }

  trait DaffodilXMLReader extends org.xml.sax.XMLReader {
    def parse(is: java.io.InputStream): Unit
    def parse(in: InputSourceDataInputStream): Unit
    def parse(ab: Array[Byte]): Unit
  }

  trait ParseResult extends Result with WithDiagnostics {
    def resultState: State
  }

  trait UnparseResult extends Result with WithDiagnostics {
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
    def processorStatus: ProcessorResult
    def validationStatus: Boolean
    def diagnostics: Seq[Diagnostic]
    def currentLocation: DataLocation
  }

  /**
   * Interface for Parse and Unparse results
   */
  abstract class Result {
    def resultState: State
    var diagnostics: Seq[Diagnostic] = Nil

    private def resultStatusDiagnostics: Seq[Diagnostic] = {
      resultState.processorStatus match {
        case Failure(c) => List(c)
        case Success => Nil
      }
    }

    def getDiagnostics = {
      (diagnostics.toSet ++ resultState.diagnostics.toSet ++ resultStatusDiagnostics.toSet).toSeq
    }

    def addDiagnostic(d: Diagnostic): Unit = {
      diagnostics = d +: diagnostics
    }

    def isError = isProcessingError || isValidationError
    def isProcessingError = resultState.processorStatus != Success
    def isValidationError = resultState.validationStatus != true
  }
}
