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

import org.apache.daffodil.runtime1.layers.LayerRuntimeData;

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
public interface LayerRuntime {

  String layerName();

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

}