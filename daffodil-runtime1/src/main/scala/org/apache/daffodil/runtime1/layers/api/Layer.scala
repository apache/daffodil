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
package org.apache.daffodil.runtime1.layers.api

import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.nio.charset.Charset
import java.util.Optional

import org.apache.daffodil.runtime1.api.DFDLPrimType
import org.apache.daffodil.runtime1.layers.LayerException

// TODO: Convert this whole file to Java ??

/**
 * Descriptor of the DFDL variables used by the layer.
 *
 * The names and types must match the dfdl:defineVariable definitions used in
 * an included/imported DFDL component schema which provides the layer definition for use
 * by other schemas.
 *
 * The first such variable is sometimes distinguished as the variable written to
 * as the single unique value of the layer, such as when the layer computes a
 * checksum. This is, however, just a convention used by various checksum layers
 * and classes/traits that support writing checksum layers.
 *
 * @param prefix preferred namespace prefix for the namespace of the variables
 * @param namespace namespace of the variables
 * @param variables list of pairs, each is the name (an NCName, that is without any
 *                  namespace prefix) and type of a variable
 */
final case class LayerVariables(
  prefix: String,
  namespace: String,
  variables: java.util.List[(String, DFDLPrimType)],
)

/**
 * This is the primary API class for writing layers.
 *
 * All layers are derived from this class, and must have no-args default constructors.
 *
 * Derived classes will be dynamically loaded by Java's SPI system.
 * The names of concrete classes derived from Layer are listed in a resources/M.services file
 * so that they can be found and dynamically loaded.
 *
 * The SPI creates an instance the class of which is used as a factory to create the
 * instances actually used by the Daffodil runtime. Compilation of the static information about
 * the layer occurs only once and is then shared by all runtime instances.
 *
 * Instances of derived layer classes can be stateful. They are private to threads, and each time a layer
 * is encountered during parse/unparse, an instance is created for that situation.
 *
 * All the static information about the layer is provided in the arguments.
 *
 * The rest of the Layer class implements the
 * layer decode/encode logic, which is done as part of deriving one's Layer class from the
 * Layer base class.
 *
 * About variables: Layer logic may read and write variables. Variables being read are parameters to
 * the layer algorithm. Variables being written are outputs (such as checksums) from the layer algorithm.
 * Variables being written must be undefined, since variables in DFDL are single-assignment.
 * Variables being read must be defined before being read by the layer, and this is true for both
 * parsing and unparsing. When unparsing, variables being read cannot be forward-referencing to parts
 * of the DFDL infoset that have not yet been unparsed.
 *
 * @param layerName the name that will appear in the DFDL schema to identify the layer
 * @param supportedLayerLengthKinds list of the layer length kinds the layer supports
 * @param supportedLayerLengthUnits list of the layer length units the layer supports
 * @param isRequiredLayerEncoding true if the layer is textual and so needs the layerEncoding property
 * @param optLayerVariables a LayerVariables structure describing the variables the layer accesses
 */
abstract class Layer(
  val layerName: String,
  val supportedLayerLengthKinds: java.util.List[JLayerLengthKind],
  val supportedLayerLengthUnits: java.util.List[JLayerLengthUnits],
  val isRequiredLayerEncoding: Boolean,
  val optLayerVariables: Optional[LayerVariables],
) {

  final def name(): String =
    layerName // name() method with empty args is required by SPI loader

  /**
   * Called exactly once when the schema is compiled to do extra checking that the layer is being used properly.
   * The thrown exception becomes a SchemaDefinitionError at schema compile time.
   *
   * Example checks are:
   * - layerEncoding is constant and is a single-byte charset
   * - layerLength, if constant, is within a maximum value range
   * - layerBoundaryMark string, if constant, is not too long and contains only allowed characters.
   * These things can be required to be constant by this check, or it can check their values for legality
   * if they happen to be constant. Since these are runtime-valued properties (can be expressions), then if the
   * layer allowed that, they must also be checked at runtime.
   *
   * You don't have to check that the variables are defined and declared in matching manner, that happens automatically.
   *
   * @throws LayerException on any error.
   *
   */
  @throws[LayerException]
  def check(layerPropertyInfo: LayerPropertyInfo): Unit = { /* nothing */ }
  // TODO: figure out what to throw. LayerCompilerError ??

  @throws[IOException]
  def wrapLayerDecoder(jis: InputStream, lr: LayerRuntime): InputStream

  @throws[IOException]
  def wrapLayerEncoder(jos: OutputStream, lr: LayerRuntime): OutputStream

  /**
   * Override if you need to customize layer length limiting behavior.
   * You will not override if you use standard explicit, implicit, or boundary-mark behaviors.
   *
   * @return a LayerLimiter or null to indicate the standard layer limiter implementation should be used.
   */
  def layerLimiter(layerPropertyInfo: LayerPropertyInfo): Optional[LayerLimiter] =
    Optional.empty()
}

/**
 * Layers that use the standard layerLengthKinds will not need to use this.
 *
 * But if they want to implement different behaviors for how the length is determined
 * then the layer must implement a LayerLimiter.
 *
 * Implementations of this trait must have a public zero-arg constructor which will be
 * used to allocate a per-thread per-layer instance of the class.
 *
 * That is, implementations of the class are free to be stateful, as their state
 * will be private to just that single invocation. 
 */
trait LayerLimiter {
  def wrapLayerLimitingInputStream(jis: InputStream, lr: LayerRuntime): InputStream
  def wrapLayerLimitingOutputStream(jos: OutputStream, lr: LayerRuntime): OutputStream
}

/**
 * Runtime information and stateful services available to the layer when
 * encoding/decoding the layer data to/from the input/output stream.
 *
 * Provides the ability to cause runtime processing errors, which can cause backtracking
 * when parsing, but are fatal when unparsing.
 *
 * Also provides the ability to cause runtime schema definition errors, which are always
 * fatal.
 *
 * This object contains the processor state, but hidden behind an API so that only relevant
 * aspects of the processor state are visible. 
 */
trait LayerRuntime extends LayerPropertyInfo {

  def layerLength(): Long

  def layerBoundaryMark(): String

  /**
   * @return the value of the layerEncoding property converted to a java.nio.charset.Charset.
   *         If defined as an expression, the expression is evaluated and the
   *         string it produces interpreted as the name of a charset encoding. If the layerEncoding property
   *         is not defined it is an error in the layer implementation reported as a schema definition error.
   *         If the layerEncoding string value cannot be converted to a charset, then it is a processing error.
   */
  def layerCharset: Charset

  /**
   * At runtime, for speed reasons, variables are accessed by index which is the zero-based position in
   * the LayerVariables variables list.
   * @param index zero-based index selecting the variable structure
   * @return a LayerVariable which allows setting and getting the variable value.
   */
  def variable(index: Int): LayerVariable

  /**
   * Used when parsing to cause a runtime ParseError from layer code. Parse errors can cause backtracking.
   * Used when unparsing to cause a runtime UnparseError (which is generally fatal) from layer code.
   *
   * @param msg text that will be included in any diagnostic output.
   * @return This method does not return. Control is thrown to the Daffodil runtime.
   */
  def processingError(msg: String): Nothing

  /**
   * Used when parsing to cause a runtime ParseError from layer code. Parse errors can cause backtracking.
   * Used when unparsing to cause a runtime UnparseError (which is generally fatal) from layer code.
   *
   * @param cause a throwable that is the cause of the error
   * @return This method does not return. Control is thrown to the Daffodil runtime.
   */
  def processingError(cause: Throwable): Nothing

  /**
   * Used when parsing or unparsing to indicate that the schema is not meaningful.
   *
   * An example of this is if a runtime-valued property (defined by an expression) such as layerEncoding
   * does not have a suitable value for the layer to use. For example, it may evaluate to the name of a valid
   * encoding, but the encoding might not be supported; some layers can only work with Single-Byte Character Sets.
   *
   * This method does not return. Control is thrown to the Daffodil runtime.
   * @param msg text that will be displayed as part of the diagnostic information.
   * @param args arguments substituted into the msg as in String.format
   */
  def runtimeSchemaDefinitionError(msg: String, args: AnyRef*): Nothing

  /**
   * Used when parsing or unparsing to indicate that the schema is not meaningful.
   *
   * An example of this is if a runtime-valued property (defined by an expression) such as layerEncoding
   * does not have a suitable value for the layer to use. For example, it may evaluate to the name of a valid
   * encoding, but the encoding might not be supported; some layers can only work with Single-Byte Character Sets.
   *
   * This method does not return. Control is thrown to the Daffodil runtime.
   * @param cause Exception object that captures the cause of the problem.
   */
  def runtimeSchemaDefinitionError(cause: Throwable): Nothing

  /**
   * @param variable the variable to get
   * @return the value of the variable as a string. If the variable has no value it is a processing error.
   */
  def getString(variable: LayerVariable): String

  /**
   * Sets the value of the variable. Note that variables in DFDL are _single assignment_.
   * So if a layer sets a variable more than once it is an error in the layer code
   * which is reported as a runtime schema definition error (fatal).
   * @param variable the variable to set
   * @param s the string value to which the variable will be set
   */
  def setString(variable: LayerVariable, s: String): Unit

  /**
   * @param variable the variable to get
   * @return the value of the variable as a signed Int. The variable must be of integer (or unsigned integer) type, and if it
   *         is not it is an error in the layer code which is reported as a runtime schema definition error. (This may
   *         mean the layer code definition of the variable, and the DFDL schema's dfdl:defineVariable disagree on the
   *         type.) No attempt is made to convert strings to integers. If the variable has no value it is a processing error.
   *         If the value is greater than Int.MaxValue then it is a processing error.
   */
  def getInt(variable: LayerVariable): Int

  /**
   * Sets the value of the variable. Note that variables in DFDL are _single assignment_.
   * So if a layer sets a variable more than once it is an error in the layer code
   * which is reported as a runtime schema definition error (fatal). The variable must be of
   * integer (or unsigned integer) type. No attempt is made to convert the argument into a string
   * if the variable type is not an integer type. This type error is reported as a runtime schema definition error.
   *
   * @param variable the variable to set
   * @param v the value to assign. If the type of the variable is unsignedInt, and this parameter is negative, it is
   *          a processing error.
   */
  def setInt(variable: LayerVariable, v: Int): Unit

}

/**
 * Variable values are obtained or set using LayerVariable objects.
 */
trait LayerVariable
