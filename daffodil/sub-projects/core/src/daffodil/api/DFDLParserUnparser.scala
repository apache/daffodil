package daffodil.api

import scala.xml.Node

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
 * <p>
 * So the API provides a way to set the path specially, providing for schema-aware compilation of the XPath expression in the bulk processing case.
 *
 */
object DFDL {

  type Input = java.nio.channels.ReadableByteChannel // binary input stream/buffer
  type Output = java.nio.channels.WritableByteChannel // binary output stream/buffer

  trait Compiler {

    /**
     * User must establish error handlers, setup features appropriately before using the data access objects.
     *
     * Other features are TBD, but may include
     *
     * <ul>
     * <li>whether to error on NUL (\u0000) character code in string data. (Invalid in XML 1.0 and 1.1 syntax, but seemingly unchecked in many cases.)
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
     * If you don't set a root, it uses the first element in the schema as the root.
     */
    def setDistinguishedRootNode(name: String, namespace: String = "")
    def setExternalDFDLVariable(name: String, namespace: String, value: String)
    def setDebugging(flag: Boolean)

    /**
     * Compilation returns a parser factory, which must be interrogated for diagnostics
     * to see if compilation was successful or not.
     */
    def compile(schema: Node): ProcessorFactory
    def compile(schemaFileName: String): ProcessorFactory
    
    def reload(fileName: String): ProcessorFactory
  }

  /**
   * The point of processor factory is to allow compilation of the path expression.
   */
  trait ProcessorFactory extends WithDiagnostics {
    def onPath(xpath: String): DataProcessor
  }

  trait DataProcessor extends WithDiagnostics {
    def save(fileName: String): Unit
    
    /**
     * Unparses (that is, serializes) data to the output, returns an object which contains any diagnostics.
     */
    def unparse(output: Output, node: scala.xml.Node): UnparseResult
    
    /**
     * Returns an object which contains the result, and/or diagnostic information
     */   
    def parse(input : Input) : ParseResult 
  }
  
  trait ParseResult extends WithDiagnostics {
    def result : scala.xml.Node
  }
  
  trait UnparseResult extends WithDiagnostics 

  abstract class State {
    def currentLocation : DataLocation
  }
}

