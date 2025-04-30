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

import org.apache.daffodil.api.compiler.ProcessorFactory;
import org.apache.daffodil.api.exceptions.InvalidParserException;
import scala.Option;
import scala.collection.immutable.Map;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.channels.ReadableByteChannel;

/**
 * Compile DFDL schemas into {@link org.apache.daffodil.api.compiler.ProcessorFactory}'s or
 * reload saved parsers into {@link org.apache.daffodil.api.DataProcessor}'s.
 * <p>
 * Do not use the Compiler constructor to create a Compiler. Instead, use {@code Daffodil#compiler()}.
 */
public interface Compiler {
  /**
   * Compile DFDL schema file into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   * <p>
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use {@code Compiler.compileSource} instead.
   *
   * @param schemaFile DFDL schema file used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileFile(File schemaFile) throws IOException {
    return compileFile(schemaFile, Option.empty(), Option.empty());
  }

  /**
   * Compile DFDL schema file into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   * <p>
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use {@code Compiler.compileSource} instead.
   *
   * @param schemaFile DFDL schema file used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @param rootName   name of root element, or null to choose automatically from first element of schema.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileFile(File schemaFile, String rootName) throws IOException {
    return compileFile(schemaFile, Option.apply(rootName), Option.empty());
  }

  /**
   * Compile DFDL schema file into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   * <p>
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use {@code Compiler.compileSource} instead.
   *
   * @param schemaFile  DFDL schema file used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @param optRootName Option for name of root element, or None to choose automatically from first element of schema.
   *                    Defaults to None.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileFile(File schemaFile, Option<String> optRootName) throws IOException {
    return compileFile(schemaFile, optRootName, Option.empty());
  }

  /**
   * Compile DFDL schema file into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   * <p>
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use {@code Compiler.compileSource} instead.
   *
   * @param schemaFile    DFDL schema file used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @param rootName      name of root element, or null to choose automatically from first element
   *                      of schema.
   * @param rootNamespace String of namespace of the root element, or null to infer automatically
   *                      when unambiguous. Pass "" (empty string) for No Namespace.*
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileFile(File schemaFile, String rootName, String rootNamespace) throws IOException {
    return compileFile(schemaFile, Option.apply(rootName), Option.apply(rootNamespace));
  }

  /**
   * Compile DFDL schema file into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   * <p>
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use {@code Compiler.compileSource} instead.
   *
   * @param schemaFile       DFDL schema file used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @param optRootName      Option for name of root element, or None to choose automatically from first element of schema.
   *                         Defaults to None.
   * @param optRootNamespace Option for string of namespace of the root element, or None to infer automatically when
   *                         unambiguous. Pass Some("") (empty string) for No Namespace. Defaults to None.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  ProcessorFactory compileFile(File schemaFile, Option<String> optRootName, Option<String> optRootNamespace) throws IOException;

  /**
   * Compile DFDL schema source into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   *
   * @param uri URI of DFDL schema file used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileSource(URI uri) throws IOException {
    return compileSource(uri, Option.empty(), Option.empty());
  }

  /**
   * Compile DFDL schema source into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   *
   * @param uri      URI of DFDL schema file used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @param rootName name of root element, or null to choose automatically from first element of schema.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileSource(URI uri, String rootName) throws IOException {
    return compileSource(uri, Option.apply(rootName), Option.empty());
  }

  /**
   * Compile DFDL schema source into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   *
   * @param uri         URI of DFDL schema file used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @param optRootName Option for name of root element, or None to choose automatically from first
   *                    element of schema. Defaults to None.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileSource(URI uri, Option<String> optRootName) throws IOException {
    return compileSource(uri, optRootName, Option.empty());
  }

  /**
   * Compile DFDL schema source into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   *
   * @param uri           URI of DFDL schema file used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @param rootName      name of root element, or null to choose automatically from first element of schema.
   * @param rootNamespace String of namespace of the root element, or null to infer automatically
   *                      when unambiguous. Pass "" (empty string) for No Namespace.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileSource(URI uri, String rootName, String rootNamespace) throws IOException {
    return compileSource(uri, Option.apply(rootName), Option.apply(rootNamespace));
  }

  /**
   * Compile DFDL schema source into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   *
   * @param uri              URI of DFDL schema file used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @param optRootName      Option for name of root element, or None to choose automatically from first
   *                         element of schema. Defaults to None.
   * @param optRootNamespace Option for string of namespace of the root element, or None to infer
   *                         automatically when unambiguous. Pass Some("") (empty string) for No Namespace.
   *                         Defaults to None.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  ProcessorFactory compileSource(URI uri, Option<String> optRootName, Option<String> optRootNamespace) throws IOException;

  /**
   * Compile DFDL resource name into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   *
   * @param name Resource name of a DFDL schema used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileResource(String name) throws IOException {
    return compileResource(name, Option.empty(), Option.empty());
  }

  /**
   * Compile DFDL resource name into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   *
   * @param name          Resource name of a DFDL schema used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @param rootName      name of root element, or null to choose automatically from first element
   *                      of schema.
   * @param rootNamespace String of namespace of the root element, or null to infer automatically
   *                      when unambiguous. Pass "" (empty string) for No Namespace.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  default ProcessorFactory compileResource(String name, String rootName, String rootNamespace) throws IOException {
    return compileResource(name, Option.apply(rootName), Option.apply(rootNamespace));
  }

  /**
   * Compile DFDL resource name into a {@link org.apache.daffodil.api.compiler.ProcessorFactory}
   *
   * @param name             Resource name of a DFDL schema used to create a {@link org.apache.daffodil.api.compiler.ProcessorFactory}.
   * @param optRootName      Option for name of root element, or None to choose automatically from first
   *                         element of schema. Defaults to None.
   * @param optRootNamespace Option for string of namespace of the root element, or None to infer
   *                         automatically when unambiguous. Pass Some("") (empty string) for No Namespace.
   *                         Defaults to None.
   * @return {@link org.apache.daffodil.api.compiler.ProcessorFactory} used to create {@link org.apache.daffodil.api.DataProcessor}(s). Must check {@code ProcessorFactory.isError} before using it.
   */
  ProcessorFactory compileResource(String name, Option<String> optRootName, Option<String> optRootNamespace) throws IOException;

  /**
   * Reload a saved parser from a file
   * <p>
   * To allow jar-file packaging, (where the savedParser might be part of a jar),
   * it is recommended to use the other version of {@code Compiler.reload(savedParser:java\.nio\.channels\.ReadableByteChannel)* Compiler.reload} where the argument is
   * a java.nio.channels.ReadableByteChannel for a saved parser.
   *
   * @param savedParser file of a saved parser, created with {@code DataProcessor.save}
   * @return {@link org.apache.daffodil.api.DataProcessor} used to parse data. Must check {@code DataProcessor.isError} before using it.
   * @throws org.apache.daffodil.api.exceptions.InvalidParserException if the file is not a valid saved parser.
   */
  DataProcessor reload(File savedParser) throws InvalidParserException;

  /**
   * Reload a saved parser from a java.nio.channels.ReadableByteChannel
   *
   * @param savedParser java.nio.channels.ReadableByteChannel of a saved parser, created with {@code DataProcessor.save}
   * @return {@link org.apache.daffodil.api.DataProcessor} used to parse data. Must check {@code DataProcessor.isError} before using it.
   * @throws org.apache.daffodil.api.exceptions.InvalidParserException if the file is not a valid saved parser.
   */
  DataProcessor reload(ReadableByteChannel savedParser) throws InvalidParserException;

  /**
   * Return a new {@link Compiler} with a specific Daffodil tunable parameter
   *
   * @param tunable name of the tunable parameter to set.
   * @param value   value of the tunable parameter to set
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/#tunable-parameters'>Tunable Parameters</a> - list of tunables names of default values
   */
  Compiler withTunable(String tunable, String value);

  /**
   * Return a new {@link Compiler} with multiple tunable parameters
   *
   * @param tunables a map of key/value pairs, where the key is the tunable name and the value is the value to set it to
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/#tunable-parameters'>Tunable Parameters</a> - list of tunables names of default values
   */
  Compiler withTunables(Map<String, String> tunables);

}