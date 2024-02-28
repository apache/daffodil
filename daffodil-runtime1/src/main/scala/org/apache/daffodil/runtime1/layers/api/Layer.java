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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * This is the primary API class for writing layers.
 * <p/>
 * All layers are derived from this class, and must have no-args default constructors.
 * <p/>
 * Derived classes will be dynamically loaded by Java's SPI system.
 * The names of concrete classes derived from Layer are listed in a resources/M.services file
 * so that they can be found and dynamically loaded.
 * <p/>
 * The SPI creates an instance the class of which is used as a factory to create the
 * instances actually used by the Daffodil runtime. Compilation of the static information about
 * the layer occurs only once and is then shared by all runtime instances.
 * <p/>
 * Instances of derived layer classes can be stateful. They are private to threads, and each time a layer
 * is encountered during parse/unparse, an instance is created for that situation.
 * <p/>
 * Layer instances should not share mutable state (such as via singleton objects)
 * <p/>
 * All the static information about the layer is provided in the arguments.
 * <p/>
 * The rest of the Layer class implements the
 * layer decode/encode logic, which is done as part of deriving one's Layer class from the
 * Layer base class.
 * <p/>
 * About variables: Layer logic may read and write variables. Variables being read are parameters to
 * the layer algorithm. Variables being written are outputs (such as checksums) from the layer algorithm.
 * Variables being written must be undefined, since variables in DFDL are single-assignment.
 * Variables being read must be defined before being read by the layer, and this is true for both
 * parsing and unparsing. When unparsing, variables being read cannot be forward-referencing to parts
 * of the DFDL infoset that have not yet been unparsed.
 * <p/>
 */
public abstract class Layer {

  protected final String layerName;

  public Layer(String layerName) {
    this.layerName = layerName;
  }

  public final String name() {
    return this.layerName; // name() method with empty args is required by SPI loader
  }

  /**
   * Called exactly once when the schema is compiled to do extra checking that the layer is being used properly.
   * The thrown exception becomes a SchemaDefinitionError at schema compile time.
   * <p/>
   * Example checks are:
   * - layerEncoding is constant and is a single-byte charset
   * - layerLength, if constant, is within a maximum value range
   * - layerBoundaryMark string, if constant, is not too long and contains only allowed characters.
   * These things can be required to be constant by this check, or it can check their values for legality
   * if they happen to be constant. Since these are runtime-valued properties (can be expressions), then if the
   * layer allowed that, they must also be checked at runtime.
   * <p/>
   * You don't have to check that the variables are defined and declared in matching manner, that happens automatically.
   */
  public void check(LayerCompileInfo layerPropertyInfo) throws LayerException { /* nothing */ }


  /**
   * Wraps a layer decoder around an input stream, using the provided LayerRuntime for runtime information and stateful services.
   *
   * @param jis The input stream to be wrapped.
   * @param lr The LayerRuntime object providing runtime information and stateful services.
   * @return An input stream with the layer decoder wrapped around it.
   */
  public InputStream wrapLayerDecoder(InputStream jis, LayerRuntime lr) throws IOException { return null; }

  /**
   * Wraps a layer encoder around an output stream, using the provided LayerRuntime for runtime information and stateful services.
   *
   * @param jos The output stream to be wrapped.
   * @param lr The LayerRuntime object providing runtime information and stateful services.
   * @return An output stream with the layer encoder wrapped around it.
   */
  public OutputStream wrapLayerEncoder(OutputStream jos, LayerRuntime lr) throws IOException { return null; }

}