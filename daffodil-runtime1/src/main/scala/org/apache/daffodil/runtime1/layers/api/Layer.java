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

import org.apache.daffodil.lib.xml.QName;
import org.apache.daffodil.runtime1.layers.LayerUtils;

import java.io.FilterInputStream;
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

  protected final String layerLocalName;
  protected final String layerNamespace;

  /**
   * Constructs a new Layer object with the given layer name and namespace.
   *
   * @param layerLocalName      the local NCName of the layer. Must be usable as a Java identifier.
   * @param layerNamespace the namespace of the layer. Must obey URI syntax.
   * @throws IllegalArgumentException if arguments are null or do not obey required syntax.
   */
  public Layer(String layerLocalName, String layerNamespace) {

    LayerUtils.requireJavaIdCompatible(layerLocalName, "layerLocalName");
    LayerUtils.requireURICompatible(layerNamespace, "layerNamespace");

    this.layerLocalName = layerLocalName;
    this.layerNamespace = layerNamespace;
  }

  /** The spiName of the Layer class.
   *
   * This method and the string it returns are required by the SPI loader.
   * @return A unique indentifier for the kind of layer. Contains both local and namespace components of the layer's complete name.
   */
  public final String name() { return LayerUtils.spiName(layerLocalName, layerNamespace); }

  public final String localName() { return this.layerLocalName; }
  public final String namespace() { return this.layerNamespace; }

  /**
   * Wraps a layer input interpreter around an input stream, using the provided LayerRuntime for runtime information and stateful services.
   *
   * @param jis The input stream to be wrapped.
   * @param lr The LayerRuntime object providing runtime information and stateful services.
   * @return An input stream with the layer wrapped around it.
   * throws javo.io.IOException
   */
  public abstract InputStream wrapLayerInput(InputStream jis, LayerRuntime lr) throws IOException;

  /**
   * Wraps a layer output interpreter around an output stream, using the provided LayerRuntime for runtime information and stateful services.
   *
   * @param jos The output stream to be wrapped.
   * @param lr The LayerRuntime object providing runtime information and stateful services.
   * @return An output stream with the layer wrapped around it.
   * throws javo.io.IOException
   */
  public abstract OutputStream wrapLayerOutput(OutputStream jos, LayerRuntime lr) throws IOException;

}