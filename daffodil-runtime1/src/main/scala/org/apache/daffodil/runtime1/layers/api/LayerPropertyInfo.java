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

import java.nio.charset.Charset;
import java.util.Optional;

/**
 * Provides contextual information for the layer's own static checking.
 */
public interface LayerPropertyInfo {

   // TODO: BitsCharset needs to be a trait that is part of the supported API

  /**
   * Retrieves the layerEncoding property as a java.nio.charset.Charset
   *
   * @return an Optional containing another Optional of type BitsCharset.
   *         The outer Optional will be empty if the layerEncoding property is not defined.
   *         The inner Optional will be empty if the layerEncoding property is not constant.
   */
  Optional<Optional<Charset>> optOptConstLayerCharset();

  /**
   * @return the layerLength property value if defined and a constant.
   */
  Optional<Optional<Long>> optOptConstLayerLength();

  /**
   * @return the layerLengthKind property value. This property must always be defined for layers so
   * it is a schema definition error if this is requested but the property is not defined.
   */
  JLayerLengthKind layerLengthKind();

  /**
   * @return the layerLengthUnits property value if defined.
   */
  Optional<JLayerLengthUnits> optLayerLengthUnits();

  /**
   * @return the layerBoundaryMark property value if defined and a constant.
   */
  Optional<Optional<String>> optOptConstLayerBoundaryMark();

  // TODO: decide whether to provide these more mundane aliases
//  boolean isDefinedLayerBoundaryMark();
//  boolean isConstLayerBoundaryMary();
//  String constLayerBoundaryMark();

  /**
   * Causes a Schema Definition Error when compiling the DFDL schema.
   *
   * @param msg  A string which is included with the diagnostic information.
   * @param args any number of arguments which are substituted into the msg via the same
   *             mechanism as String.format
   * @return This method does not return. It throws to the Daffodil schema compiler context.
   */
  void schemaDefinitionError(String msg, Object... args);

  /**
   * Causes a Schema Definition Warning when compiling the DFDL schema.
   *
   * These can be suppressed using the warn ID "layerCompileWarning"
   *
   * @param msg  A string which is included with the diagnostic information.
   * @param args any number of arguments which are substituted into the msg via the same
   *             mechanism as String.format
   */
  // TODO: Remove if not needed. Hard to implement, as the LayerRuntimeInfo object
  // Doesn't have compiler context for issuing warnings.
  //  void schemaDefinitionWarning(String msg, Object... args);
}