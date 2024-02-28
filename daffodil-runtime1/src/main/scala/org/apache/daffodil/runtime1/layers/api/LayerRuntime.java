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

/**
 * Runtime information and stateful services available to the layer when
 * encoding/decoding the layer data to/from the input/output stream.
 * <p/>
 * Provides the ability to cause runtime processing errors, which can cause backtracking
 * when parsing, but are fatal when unparsing.
 * <p/>
 * Also provides the ability to cause runtime schema definition errors, which are always
 * fatal.
 * <p/>
 * This object contains the processor state, but hidden behind an API so that only relevant
 * aspects of the processor state are visible.
 */
public interface LayerRuntime extends LayerCompileInfo {

  /**
   * Retrieves the Charset object associated with the given layer encoding from the LayerRuntime.
   *
   * @param layerEncoding the encoding of the layer
   * @return the Charset object associated with the layer encoding
   */
  Charset getCharset(String layerEncoding);

  /**
   * Throws a processing error with the specified message.
   *
   * @param msg the error message
   */
  void processingError(String msg);

  /**
   * Throws a processing error by providing the cause of the error.
   *
   * @param cause the Throwable object representing the cause of the error
   */
  void processingError(Throwable cause);

  /**
   * Throws a runtime schema definition error, which is always fatal.
   *
   * This method is used to report errors related to the schema definitions at runtime.
   *
   * @param msg  A string which describes the error.
   * @param args any number of arguments which are substituted into the msg via the same
   *             mechanism as String.format.
   */
  void runtimeSchemaDefinitionError(String msg, Object... args);

  /**
   * Throws a runtime schema definition error.
   * <p/>
   * This method is used to indicate a fatal schema definition error that occurs during runtime processing.
   *
   * @param cause the Throwable object representing the cause of the error
   */
  void runtimeSchemaDefinitionError(Throwable cause);

  /**
   * @param variable The LayerVariable object for which to retrieve the string value.
   * @return The string value associated with the given LayerVariable.
   */
  String getString(LayerVariable variable);

  /**
   * @param variable the LayerVariable to set the string value for
   * @param s the string value to be set
   */
  void setString(LayerVariable variable, String s);

  /**
   * @param variable The LayerVariable from which to retrieve the integer value.
   * @return The integer value associated with the provided LayerVariable.
   */
  int getInt(LayerVariable variable);

  /**
   * @param variable The LayerVariable to set the value for.
   * @param v The integer value to set.
   */
  void setInt(LayerVariable variable, int v);

  /**
   * @param variable The LayerVariable for which the long value is to be retrieved.
   * @return The long value associated with the given LayerVariable.
   */
  long getLong(LayerVariable variable);

  /**
   * @param variable the LayerVariable to set the value for
   * @param v the long value to set
   */
  void setLong(LayerVariable variable, long v);
}