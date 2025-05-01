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
package org.apache.daffodil.runtime1.layers.api;

import org.apache.daffodil.lib.util.SimpleNamedLoadableService;
import org.apache.daffodil.runtime1.layers.LayerRuntime;
import org.apache.daffodil.runtime1.layers.LayerUtils;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * This is the primary API class for writing layers.
 * <p>
 * All layers are derived from this class.
 * <p>
 * This class is used directly as a base class to define <i>transforming</i> layers.
 * To simplify the
 * definition of <i>checksum</i> layers, a specialized sub-class
 * {@link org.apache.daffodil.runtime1.layers.api.ChecksumLayer}
 * is also available.
 * Many of the requirements for layers, such as the naming conventions for layer variables,
 * are described here, but they apply equally to checksum layers.
 * <p>
 * Derived classes will be dynamically loaded by Java's Service Provider Interface (SPI) system.
 * The names of concrete classes derived from Layer are listed in
 * a metadata resource file
 * named for this class (that is, the file name is the fully-qualified class name of this class:
 * {@code resources/META-INF/services/org.apache.daffodil.runtime1.layers.api.Layer}).
 * This file contains lines where each line contains one fully qualified class name of a derived
 * layer class.
 * More than one line in the file denotes that the Jar file contains the definitions of more than one derived layer
 * class.
 * This file is incorporated in the compiled jar file for the derived {@code Layer} class
 * so that the class path can be searched and Jars containing layer classes can be dynamically loaded.
 * <p>
 * The SPI creates an instance the class by calling a default (no-arg) constructor, which should be
 * the only constructor.
 * <p>
 * Instances of derived layer classes can be stateful.
 * They are private to threads, and each time a layer
 * is encountered during parse/unparse, an instance is created for that situation.
 * <p>
 * Layer instances should not share mutable state (such as via singleton objects).
 * <p>
 * <h2>About Layer Variables</h2>
 * <p>
 * Layer logic may read and write DFDL variables.
 * These variables are associated with the layer implementation class by using
 * Java/Scala reflection to find matches (case-sensitive) between the names of DFDL variables and method
 * names and method arguments of the layer's Java/Scala code.
 * Hence, the layer's DFDL variables must have names suitable for use as Java/Scala
 * identifiers.
 * <p>
 * The layer namespace is used only for its layer.
 * All DFDL variables defined in that namespace are either used to pass parameters to the
 * layer code, or receive results (such as a checksum) back from the layer code.
 * This is enforced.
 * If a layer namespace contains a DFDL variable and there is no corresponding
 * usage of that variable name in the layer code (following the conventions below), then
 * it is a Schema Definition Error when the layer code is loaded.
 * <p>
 * A layer that does not define any DFDL variables does
 * not have to define a DFDL schema that defines the layer's target
 * namespace, but any layer that uses DFDL variables *must* define a
 * schema with the layer's namespace as its target namespace, and with the
 * variables declared in it (using {@code dfdl:defineVariable}).
 * </p>
 * Every DFDL Variable in the layer's target namespace is used either at the start of the
 * layer algorithm as a parameter to the layer or at the end of the layer algorithm where it is
 * assigned a return value (such as a checksum or flag) from the layer.
 * <p>
 * Variables being read must have values before being read by the layer, and this is true for both
 * parsing and unparsing.
 * This happens when the Daffodil processor begins parsing/unparsing the layered sequence.
 * When unparsing, variables being read cannot be forward-referencing to parts
 * of the DFDL infoset that have not yet been unparsed.
 * <p>
 * A layer that wants to read parameters declares a special setter named
 * {@code setLayerVariableParameters}
 * which has arguments where each has a name and type that match a corresponding
 * {@code dfdl:defineVariable} in the layer's target namespace.
 * <p>
 * For example, if the layer logic has a DFDL variable for a parameter named {@code direction}
 * of type {@code xs:string} and another DFDL variable for a parameter named {@code wordCount}
 * of type {@code xs:unsignedShort}
 * then the derived Layer class must have a {@code setLayerVariableParameters} with
 * arguments corresponding in name and type to these two variables.
 * This setter will be called passing the value of the variables immediately after the layer
 * instance is constructed.
 * The arguments to {@code setLayerVariableParameters} can be in any order:
 * <pre>
 *     void setLayerVariableParameters(String direction, int wordCount) {
 *         // usually this setter will assign the values to data members
 *         this.direction = direction;
 *         this.wordCount = wordCount;
 *     }
 * </pre>
 * Beside initializing local members, this setter is also an initializer for the layer class instance.
 * Any exception thrown becomes a Schema Definition Error.
 * <p>
 * If there are no parameter variables, then this setter, with no arguments, can be used purely for initialization.
 * <p>
 * A DFDL variable used to return a result from a layer must be undefined, since variables in DFDL are single-assignment.
 * Usually this means the use of the layer must be surrounded by a {@code dfdl:newVariableInstance}
 * annotation which creates a new instance of the layer result variable, over a limited scope
 * of use.
 * The variable is assigned by the layer, and it is then available for reading by the DFDL schema until
 * the end of the {@code dfdl:newVariableInstance} scope.
 * <p>
 * To return a value into a DFDL variable, the layer implementation defines a special recognizable
 * getter method.
 * The name of the getter is formed from prefixing the DFDL variable name with the string
 * "{@code getLayerVariableResult_}".
 * The return type of the getter must correspond to the type of the variable.
 * <p>
 * For example, a result value getter for a DFDL variable named {@code total} of type {@code xs:unsignedShort} would be:
 * <pre>
 * {@code
 *      int getLayerVariableResult_total() {
 *          // returns the value created by the layer's algorithm.
 *          // commonly this returns the value of a data member.
 *          return this.total;
 *      }
 * }
 * </pre>
 * Layers could have multiple result variables, but a single variable is most common, generally for returning checksums.
 * See the {@link ChecksumLayer} class, which is designed to facilitate creation of checksum/CRC/hash/digest layers.
 * <p>
 * The Java types to use for the setter arguments and getter return types correspond to the DFDL variable types
 * according to this table:
 * <table border="1">
 *     <tr>
 *         <th>DFDL Schema Type</th>
 *         <th>Java Type</th>
 *     </tr>
 *     <tr>
 *         <td>xs:byte</td>
 *         <td>byte</td>
 *     </tr>
 *     <tr>
 *         <td>xs:short</td>
 *         <td>short</td>
 *     </tr>
 *     <tr>
 *         <td>xs:int</td>
 *         <td>int</td>
 *     </tr>
 *     <tr>
 *         <td>xs:long</td>
 *         <td>long</td>
 *     </tr>
 *     <tr>
 *         <td>xs:integer</td>
 *         <td>java.math.BigInteger</td>
 *     </tr>
 *     <tr>
 *         <td>xs:decimal</td>
 *         <td>java.math.BigDecimal</td>
 *     </tr>
 *     <tr>
 *         <td>xs:unsignedInt</td>
 *         <td>long</td>
 *     </tr>
 *     <tr>
 *         <td>xs:unsignedByte</td>
 *         <td>short</td>
 *     </tr>
 *     <tr>
 *         <td>xs:unsignedShort</td>
 *         <td>int</td>
 *     </tr>
 *     <tr>
 *         <td>xs:unsignedLong</td>
 *         <td>java.math.BigInteger</td>
 *     </tr>
 *     <tr>
 *         <td>xs:nonNegativeInteger</td>
 *         <td>java.math.BigInteger</td>
 *     </tr>
 *     <tr>
 *         <td>xs:double</td>
 *         <td>double</td>
 *     </tr>
 *     <tr>
 *         <td>xs:float</td>
 *         <td>float</td>
 *     </tr>
 *     <tr>
 *         <td>xs:hexBinary</td>
 *         <td>byte[]</td>
 *     </tr>
 *     <tr>
 *         <td>xs:anyURI</td>
 *         <td>java.net.URI</td>
 *     </tr>
 *     <tr>
 *         <td>xs:boolean</td>
 *         <td>boolean</td>
 *     </tr>
 *     <tr>
 *         <td>xs:dateTime</td>
 *         <td>com.ibm.icu.util.ICUCalendar</td>
 *     </tr>
 *     <tr>
 *         <td>xs:date</td>
 *         <td>com.ibm.icu.util.ICUCalendar</td>
 *     </tr>
 *     <tr>
 *         <td>xs:time</td>
 *         <td>com.ibm.icu.util.ICUCalendar</td>
 *     </tr>
 *     <caption style="caption-side:bottom"></caption>
 * </table>
 * <p>
 * <h2>Layer Algorithm</h2>
 * <p>
 * The rest of the Layer class implements the layer decode/encode logic.
 * <p>
 * The actual algorithm of the layer is not implemented in methods of the derived layer class, but rather is
 * implemented in the layer's <i>input decoder</i> and <i>output encoder</i>.
 * These extend the {@code java.io.InputStream} and {@code java.io.OutputStream} base classes to actually handle the
 * data.
 * <p>
 * Every layer must implement the {@code wrapLayerInput} and {@code wrapLayerOutput}
 * methods, which provide the input decoder and output encoder instances to the Daffodil layer framework.
 * When parsing/unparsing, the derived Layer class itself is concerned with setup and tear-down of the
 * layer's input decoder and output encoder, with providing access to/from DFDL variables,
 * and with reporting errors effectively.
 * <p>
 * <h3> Layer Exception Handling </h3>
 * The method {@code setProcessingErrorException } allows the layer to specify that if the layer throws specific
 * exceptions or runtime exceptions that they are converted into processing errors.
 * This eliminates most need for layer code to contain try-catches.
 * For example:
 * <pre>
 * {@code
 *     setProcessingErrorException(IOException.class);
 * }
 * </pre>
 * informs the DFDL processor that an IOException thrown from the layer is to be treated as a processing error.
 * <p>
 * Unhandled exceptions thrown by the layer code are treated as fatal errors.
 */
public abstract class Layer implements SimpleNamedLoadableService {

  private final String localName;
  private final String targetNamespace;

  private LayerRuntime layerRuntime;

  /**
   * Constructs a new Layer object with the given layer name and namespace URI.
   *
   * @param localName       the local NCName of the layer. Must be usable as a Java identifier.
   * @param targetNamespace the namespace URI of the layer. Must obey URI syntax.
   * @throws IllegalArgumentException if arguments are null or do not obey required syntax.
   */
  public Layer(String localName, String targetNamespace) {

    LayerUtils.requireJavaIdCompatible(localName, "layerLocalName");
    LayerUtils.requireURICompatible(targetNamespace, "layerNamespace");

    this.localName = localName;
    this.targetNamespace = targetNamespace;
  }

  /**
   * For framework use.
   * <p>
   * This method and the string it returns are required by the SPI loader.
   *
   * @return A unique indentifier for the kind of layer. Contains both local and namespace components of the layer's complete name.
   */
  public final String name() {
    return LayerUtils.spiName(localName, targetNamespace);
  }

  /**
   * For framework use.
   */
  public final String localName() {
    return this.localName;
  }

  /**
   * For framework use.
   */
  public final String namespace() {
    return this.targetNamespace;
  }

  /**
   * For framework use.
   * <p>
   * Called by the execution framework to give the context for reporting errors.
   *
   * @param lr runtime data structure used by the framework
   */
  public final void setLayerRuntime(LayerRuntime lr) {
    this.layerRuntime = lr;
  }

  /**
   * For framework use.
   * <p>
   * Called by the execution framework to obtain the context for reporting errors.
   */
  public final LayerRuntime getLayerRuntime() {
    return layerRuntime;
  }

  /**
   * Use to report a processing error.
   * <p>
   * When parsing a processing error can cause backtracking so that the parse
   * can often recover from the error.
   * <p>
   * When unparsing a processing error is fatal.
   *
   * @param msg describes the error
   */
  public void processingError(String msg) {
    layerRuntime.processingError(msg);
  }

  /**
   * Use to report a processing error.
   * <p>
   * When parsing a processing error can cause backtracking so that the parse
   * can often recover from the error.
   * <p>
   * When unparsing a processing error is fatal.
   *
   * @param cause a throwable object that describes the error
   */
  public void processingError(Throwable cause) {
    layerRuntime.processingError(cause);
  }

  /**
   * Use to report a runtime schema definition error.
   * <p>
   * This indicates that the layer is unable to
   * work meaningfully because of the way it is configured.
   * That is, the schema itself is not well defined due to
   * the way the layer is configured.
   * <p>
   * For example suppose a layer had a DFDL variable for a parameter that is supposed
   * to be an integer between 1 and 10, and this parameter is generally provided as a constant value.
   * If the provided parameter variable value is 0, which is not meaningful, then a
   * Runtime Schema Definition Error is the right error to invoke.
   * <p>
   * This error type is always fatal whether parsing or unparsing.
   *
   * @param msg describes the error
   */
  public void runtimeSchemaDefinitionError(String msg) {
    layerRuntime.runtimeSchemaDefinitionError(msg);
  }

  /**
   * Use to report a runtime schema definition error.
   * <p>
   * This indicates that the layer is unable to
   * work meaningfully because of the way it is configured. The schema itself is not well defined due to
   * the way the layer is configured.
   * <p>
   * This error type is always fatal whether parsing or unparsing.
   * See {@code runtimeSchemaDefinitionError(String) } for discussion of situations where
   * a Runtime Schema Definition Error is appropriate.
   *
   * @param cause a throwable object that describes the error
   */
  public void runtimeSchemaDefinitionError(Throwable cause) {
    layerRuntime.runtimeSchemaDefinitionError(cause);
  }

  /**
   * Wraps a layer <i>input decoder</i> around an input stream.
   * <p>
   * This input decoder does the real parser-time work of the layer.
   * It implements {@code java.io.InputStream}, by
   * reading data from the given argument input stream, and decoding it.
   * <p>
   * Any exception thrown from this wrap method becomes a Schema Definition Error (fatal).
   * <p>
   * The input decoder generally uses a reference to this layer instance to report errors.
   * <p>
   * To have the Daffodil layer framework convert uncaught exceptions
   * thrown by the input decoder into processing errors automatically,
   * call the {@code setProcessingErrorException(Class)}.
   * Other kinds of input decoder processing errors can be signaled by calling the {@code processingError(String)} or
   * {@code processingError(Throwable)} methods.
   * Input decoder schema definition errors are more rare, but if needed the {@code runtimeSchemaDefinitionError(String)} or
   * {@code runtimeSchemaDefinitionError(Throwable)} methods may be called.
   *
   * @param jis The input stream to be wrapped.
   * @return An input stream with the layer's input decoder wrapped around it.
   */
  public abstract InputStream wrapLayerInput(InputStream jis) throws Exception;

  /**
   * Wraps a layer <i>output encoder</i> around an output stream.
   * <p>
   * The output encoder does the real unparse-time work of the layer.
   * It implements {@code java.io.OutputStream} by
   * accepting data via the usual output stream write calls, encoding this
   * data, and writing the encoded data to the argument output stream.
   * <p>
   * Any exception thrown from this wrap method becomes a Schema Definition Error (fatal).
   * <p>
   * The output encoder generally uses a reference to this layer to report errors.
   * <p>
   * To have the Daffodil layer framework convert uncaught exceptions
   * thrown by the output encoder into processing errors automatically,
   * call the {@code setProcessingErrorException(Class)}.
   * Other kinds of output encoder processing errors can be signaled by calling the {@code processingError(String)} or
   * {@code processingError(Throwable)} methods.
   * Output encoder schema definition errors are more rare, but if needed the {@code runtimeSchemaDefinitionError(String)} or
   * {@code runtimeSchemaDefinitionError(Throwable)} methods may be called.
   *
   * @param jos The output stream to be wrapped.
   * @return An output stream with the layer wrapped around it.
   */
  public abstract OutputStream wrapLayerOutput(OutputStream jos) throws Exception;

  private ArrayList<Class<? extends Exception>> peExceptions = new ArrayList<>();

  /**
   * Use to add an exception class to the list of exceptions that will be automatically converted
   * into processing errors.
   * <p>
   * The purpose of this is to allow one to use java/scala libraries that may throw
   * exceptions when encountering bad data. Such exceptions should be translated into
   * processing errors, which will allow the parser to backtrack and try other alternatives
   * which may work for that data.
   * By calling this method the layer framework implements the try-catch logic to capture
   * these exception types and convert them into processing errors.
   * This avoids a great deal of try-catch logic that would otherwise be required in
   * layer methods.
   * <p>
   * When considering whether a thrown Exception is to be converted to a processing error
   * RuntimeException classes are handled separately from Exception classes.
   * Hence calling
   * <pre>
   *     setProcessingErrorException(Exception.class);
   * </pre>
   * will NOT cause all RuntimeExceptions to also be converted into processing errors.
   * It will, however, cause all classes derived from Exception that are NOT RuntimeExceptions
   * to be caught and converted into Processing Errors.
   *
   * @param e the exception class to be added to the list of processing error exceptions
   */
  public final void setProcessingErrorException(Class<? extends Exception> e) {
    peExceptions.add(e);
  }

  /**
   * For framework use.
   */
  public final List<Class<? extends Exception>> getProcessingErrorExceptions() {
    return peExceptions;
  }

}
