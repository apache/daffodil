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
public interface LayerRuntime extends LayerCompileInfo {

  Charset getCharset(String layerEncoding);

  void processingError(String msg);

  void processingError(Throwable cause);

  void runtimeSchemaDefinitionError(String msg, Object... args);

  void runtimeSchemaDefinitionError(Throwable cause);

  String getString(LayerVariable variable);

  void setString(LayerVariable variable, String s);

  int getInt(LayerVariable variable);

  void setInt(LayerVariable variable, int v);

  long getLong(LayerVariable variable);

  void setLong(LayerVariable variable, long v);
}