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

package org.apache.daffodil.api;

import org.apache.daffodil.api.exceptions.InvalidParserException;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.channels.ReadableByteChannel;
import java.util.Map;

/**
 * Compile DFDL schemas into {@link ProcessorFactory}'s or
 * reload saved parsers into {@link DataProcessor}'s.
 * <p>
 * Use {@code Daffodil#compiler()} to get a compiler instance.
 */
public interface Compiler {
  /**
   * Compile DFDL schema file into a {@link ProcessorFactory}
   * <p>
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use {@code Compiler.compileSource} instead.
   *
   * @param schemaFile DFDL schema file used to create a {@link ProcessorFactory}.
   * @return {@link ProcessorFactory} used to create {@link DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileFile(File schemaFile) {
    return compileFile(schemaFile, null, null);
  }

  /**
   * Compile DFDL schema file into a {@link ProcessorFactory}
   * <p>
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use {@code Compiler.compileSource} instead.
   *
   * @param schemaFile DFDL schema file used to create a {@link ProcessorFactory}.
   * @param rootName   name of root element, or null to choose automatically from first element of schema.
   * @return {@link ProcessorFactory} used to create {@link DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileFile(File schemaFile, String rootName) {
    return compileFile(schemaFile, rootName, null);
  }

  /**
   * Compile DFDL schema file into a {@link ProcessorFactory}
   * <p>
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use {@code Compiler.compileSource} instead.
   *
   * @param schemaFile    DFDL schema file used to create a {@link ProcessorFactory}.
   * @param rootName      name of root element, or null to choose automatically from first element
   *                      of schema.
   * @param rootNamespace String of namespace of the root element, or null to infer automatically
   *                      when unambiguous. Pass "" (empty string) for No Namespace.*
   * @return {@link ProcessorFactory} used to create {@link DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  ProcessorFactory compileFile(File schemaFile, String rootName, String rootNamespace);

  /**
   * Compile DFDL schema source into a {@link ProcessorFactory}
   *
   * @param uri URI of DFDL schema file used to create a {@link ProcessorFactory}.
   * @return {@link ProcessorFactory} used to create {@link DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileSource(URI uri) {
    return compileSource(uri, null, null);
  }

  /**
   * Compile DFDL schema source into a {@link ProcessorFactory}
   *
   * @param uri      URI of DFDL schema file used to create a {@link ProcessorFactory}.
   * @param rootName name of root element, or null to choose automatically from first element of schema.
   * @return {@link ProcessorFactory} used to create {@link DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileSource(URI uri, String rootName) {
    return compileSource(uri, rootName, null);
  }

  /**
   * Compile DFDL schema source into a {@link ProcessorFactory}
   *
   * @param uri           URI of DFDL schema file used to create a {@link ProcessorFactory}.
   * @param rootName      name of root element, or null to choose automatically from first element of schema.
   * @param rootNamespace String of namespace of the root element, or null to infer automatically
   *                      when unambiguous. Pass "" (empty string) for No Namespace.
   * @return {@link ProcessorFactory} used to create {@link DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  ProcessorFactory compileSource(URI uri, String rootName, String rootNamespace);

  /**
   * Compile DFDL resource name into a {@link ProcessorFactory}
   *
   * @param name Resource name of a DFDL schema used to create a {@link ProcessorFactory}.
   * @return {@link ProcessorFactory} used to create {@link DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   * @throws IOException if resource cannot be found
   */
  default ProcessorFactory compileResource(String name) throws IOException {
    return compileResource(name, null, null);
  }

  /**
   * Compile DFDL resource name into a {@link ProcessorFactory}
   *
   * @param name     Resource name of a DFDL schema used to create a {@link ProcessorFactory}.
   * @param rootName name of root element, or null to choose automatically from first element
   *                 of schema.
   * @return {@link ProcessorFactory} used to create {@link DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   * @throws IOException if resource cannot be found
   */
  default ProcessorFactory compileResource(String name, String rootName) throws IOException {
    return compileResource(name, rootName, null);
  }

  /**
   * Compile DFDL resource name into a {@link ProcessorFactory}
   *
   * @param name          Resource name of a DFDL schema used to create a {@link ProcessorFactory}.
   * @param rootName      name of root element, or null to choose automatically from first element
   *                      of schema.
   * @param rootNamespace String of namespace of the root element, or null to infer automatically
   *                      when unambiguous. Pass "" (empty string) for No Namespace.
   * @return {@link ProcessorFactory} used to create {@link DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   * @throws IOException if resource cannot be found
   */
  ProcessorFactory compileResource(String name, String rootName, String rootNamespace) throws IOException;

  /**
   * Reload a saved parser from a file
   * <p>
   * To allow jar-file packaging, (where the savedParser might be part of a jar),
   * it is recommended to use the other version of {@code Compiler.reload(savedParser:java\.nio\.channels\.ReadableByteChannel)* Compiler.reload} where the argument is
   * a java.nio.channels.ReadableByteChannel for a saved parser.
   *
   * @param savedParser file of a saved parser, created with {@code DataProcessor.save}
   * @return {@link DataProcessor} used to parse data. Must check {@code DataProcessor.isError} before using it.
   * @throws InvalidParserException if the file is not a valid saved parser.
   */
  DataProcessor reload(File savedParser) throws InvalidParserException;

  /**
   * Reload a saved parser from a java.nio.channels.ReadableByteChannel
   *
   * @param savedParser java.nio.channels.ReadableByteChannel of a saved parser, created with {@code DataProcessor.save}
   * @return {@link DataProcessor} used to parse data. Must check {@code DataProcessor.isError} before using it.
   * @throws InvalidParserException if the file is not a valid saved parser.
   */
  DataProcessor reload(ReadableByteChannel savedParser) throws InvalidParserException;

  /**
   * Return a new {@link Compiler} with a specific Daffodil tunable parameter
   *
   * @param tunable name of the tunable parameter to set.
   * @param value   value of the tunable parameter to set
   * @return new {@link Compiler} with a specific Daffodil tunable parameter
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/#tunable-parameters'>Tunable Parameters</a> - list of tunables names of default values
   */
  Compiler withTunable(String tunable, String value);

  /**
   * Return a new {@link Compiler} with multiple tunable parameters
   *
   * @param tunables a map of key/value pairs, where the key is the tunable name and the value is the value to set it to
   * @return new {@link Compiler} with multiple tunable parameters
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/#tunable-parameters'>Tunable Parameters</a> - list of tunables names of default values
   */
  Compiler withTunables(Map<String, String> tunables);
}
