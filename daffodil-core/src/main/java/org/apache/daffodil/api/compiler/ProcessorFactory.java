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

package org.apache.daffodil.api.compiler;

import org.apache.daffodil.api.CodeGenerator;
import org.apache.daffodil.api.DataProcessor;
import org.apache.daffodil.api.WithDiagnostics;

/**
 * Factory to create {@link DataProcessor}s, used for parsing data
 */
public interface ProcessorFactory extends WithDiagnostics {
  /**
   * Get a new {@link ProcessorFactory} having a global element specified as the root of DFDL Schema to start parsing.
   *
   * @param name      name of the root node
   * @param namespace namespace of the root node. Set to empty string to specify
   *                  no namespace. Set to to NULL to figure out the namespace.
   */
  ProcessorFactory withDistinguishedRootNode(String name, String namespace);

  /**
   * Create a {@link org.apache.daffodil.api.DataProcessor}
   *
   * @param path path to an element to use as the parsing root, relative to the distinguished root node. Currently, must be set to "/"
   * @return {@link org.apache.daffodil.api.DataProcessor} used to parse data. Must check {@code DataProcessor.isError} before using it.
   */
  DataProcessor onPath(String path);
}
