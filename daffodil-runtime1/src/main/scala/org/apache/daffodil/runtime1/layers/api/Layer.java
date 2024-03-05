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

import org.apache.daffodil.runtime1.layers.LayerUtils;

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
 * The SPI creates an instance the class by calling a default (no-arg) constructor.
 * Compilation of the static information about
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
 * About variables: Layer logic may read and write DFDL variables.
 * <p/>
 * Every DFDL Variable in the layer's targetNamespace is used either at the start of the
 * layer algorithm as a parameter to the layer or at the end of the layer algorithm it is
 * assigned as a return value (such as a checksum) from the layer.
 * <p/>
 * Variables being written must be undefined, since variables in DFDL are single-assignment.
 * <p/>
 * Variables being read must be defined before being read by the layer, and this is true for both
 * parsing and unparsing. When unparsing, variables being read cannot be forward-referencing to parts
 * of the DFDL infoset that have not yet been unparsed.
 * <p/>
 * A layer that wants to read parameters declares a second constructor. Each argument to this constructor
 * must have a name and type that match a corresponding dfdl:defineVariable in the layer's namespace.
 * <p/>
 * A layer that wants to return a value after the layer algorithm completes defines a special recognizable
 * getter method. The name of the getter is formed from prefixing the DFDL variable name with the string
 * 'getDFDLResultVariable_'. The return type of the getter must match the type of the variable.
 * <p/>
 * For example, a result value getter for a DFDL variable named 'checksum' of type xs:unsignedShort would be:
 * <pre>
 *      Int getDFDLResultVariable_checksum() {
 *          // returns the value created by the checksum algorithm.
 *      }
 * </pre>
 * <p/>
 * Initially, the types of DFDL variables layers can use are only
 * <ul>
 *     <li>xs:int</li>
 *     <li>xs:unsignedShort</li>
 *     <li>xs:string</li>
 * </ul>
 * In the Layer constructor, these DFDL types correspond to types java.lang.Integer (for both xs:int and xs:unsignedShort), and String.
 */
public abstract class Layer {

  private final String localName;
  private final String targetNamespace;

  /**
   * Constructs a new Layer object with the given layer name and namespace.
   *
   * @param localName      the local NCName of the layer. Must be usable as a Java identifier.
   * @param targetNamespace the namespace of the layer. Must obey URI syntax.
   * @throws IllegalArgumentException if arguments are null or do not obey required syntax.
   */
  public Layer(String localName, String targetNamespace) {

    LayerUtils.requireJavaIdCompatible(localName, "layerLocalName");
    LayerUtils.requireURICompatible(targetNamespace, "layerNamespace");

    this.localName = localName;
    this.targetNamespace = targetNamespace;
  }

  /** The spiName of the Layer class.
   *
   * This method and the string it returns are required by the SPI loader.
   * @return A unique indentifier for the kind of layer. Contains both local and namespace components of the layer's complete name.
   */
  public final String name() { return LayerUtils.spiName(localName, targetNamespace); }

  public final String localName() { return this.localName; }
  public final String namespace() { return this.targetNamespace; }

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