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

import org.apache.daffodil.api.debugger.Debugger;
import org.apache.daffodil.api.debugger.DebuggerRunner;
import org.apache.daffodil.api.debugger.InteractiveDebuggerRunner;
import org.apache.daffodil.api.debugger.InteractiveDebuggerRunnerFactory;
import org.apache.daffodil.api.debugger.TraceDebuggerRunner;
import org.apache.daffodil.api.exceptions.ExternalVariableException;
import org.apache.daffodil.api.infoset.InfosetInputter;
import org.apache.daffodil.api.infoset.InfosetOutputter;
import org.apache.daffodil.api.metadata.MetadataHandler;
import org.apache.daffodil.api.validation.ValidatorInitializationException;
import org.apache.daffodil.api.validation.ValidatorNotRegisteredException;
import org.apache.daffodil.core.dsom.ExpressionCompilers$;
import org.apache.daffodil.runtime1.debugger.InteractiveDebugger;

import java.io.File;
import java.io.Serializable;
import java.net.URI;
import java.nio.channels.WritableByteChannel;
import java.util.Map;

/**
 * Compiled version of a DFDL Schema, used to parse data and get the DFDL infoset
 */
public interface DataProcessor extends WithDiagnostics, Serializable {
  /**
   * Obtain a new {@link DataProcessor} instance with debugging enabled or disabled.
   * <p>
   * Before enabling, {@link DataProcessor#withDebugger} or {@link DataProcessor#withDebuggerRunner} must be called to obtain
   * a {@link DataProcessor} with a non-null debugger.
   *
   * @param flag true to enable debugging, false to disabled
   * @return a new {@link DataProcessor} instance with debugging enabled or disabled.
   */
  DataProcessor withDebugging(boolean flag);

  /**
   * Obtain a new {@link DataProcessor} with a specified debugger runner.
   *
   * @param dr debugger runner
   * @return a new {@link DataProcessor} with a specified debugger runner.
   */
  default DataProcessor withDebuggerRunner(DebuggerRunner dr) {
    Debugger dbg = null;
    InteractiveDebuggerRunner runner;
    if (dr instanceof TraceDebuggerRunner) {
      runner = InteractiveDebuggerRunnerFactory.newTraceDebuggerRunner(System.out);
    } else if (dr != null) {
      runner = InteractiveDebuggerRunnerFactory.get(dr);
    } else {
      runner = null;
    }

    if (runner != null) {
      dbg = new InteractiveDebugger(runner, ExpressionCompilers$.MODULE$);
    }
    return withDebugger(dbg);
  }

  /**
   * Obtain a new {@link DataProcessor} with a specified debugger.
   *
   * @param dbg debugger
   * @return a new {@link DataProcessor} with a specified debugger.
   */
  DataProcessor withDebugger(Debugger dbg);

  /**
   * Obtain a new {@link DataProcessor} with validation that does not require configuration.
   *
   * @param kind Kind of validation to use. Can be a custom validator name available via the
   *             {@link org.apache.daffodil.api.validation.ValidatorFactory} SPI or one of the built-in validators
   *             ("xerces", "daffodil", "off", "schematron")
   * @return a new {@link DataProcessor} with a specified validator.
   * @throws ValidatorNotRegisteredException if the validator cannot be found
   * @throws ValidatorInitializationException if initializing the validator fails
   */
  default DataProcessor withValidation(String kind) throws ValidatorNotRegisteredException, ValidatorInitializationException {
    return withValidation(kind, null);
  }

  /**
   * Obtain a new {@link DataProcessor} with validation using a URI for configuration.
   *
   * @param kind Kind of validation to use. Can be a custom validator name available via the
   *             {@link org.apache.daffodil.api.validation.ValidatorFactory} SPI or one of the built-in validators
   *             ("xerces", "daffodil", "off", "schematron")
   * @param config Absolute URI to use for validation configuration. If the URI ends with .conf
   *               or .properties it is treated as a java.util.Properties file that is loaded and
   *               provided to the validator. Otherwise, the URI is provided as a single property to
   *               the validator. Can be null if a URI is not known or the validator does not need
   *               additional configuration--this could cause an exception if a validator requires
   *               properties.
   * @return a new {@link DataProcessor} with a specified validator.
   * @throws ValidatorNotRegisteredException if the validator cannot be found
   * @throws ValidatorInitializationException if initializing the validator fails
   */
  DataProcessor withValidation(String kind, URI config) throws ValidatorNotRegisteredException, ValidatorInitializationException;

  /**
   * Obtain a new {@link DataProcessor} with external variables read from a Daffodil configuration file
   *
   * @param extVars file to read DFDL variables from.
   * @return a new {@link DataProcessor} with external variables read from a Daffodil configuration file
   * @throws org.apache.daffodil.api.exceptions.ExternalVariableException if an error occurs while setting an external variable
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/'>Daffodil Configuration File</a> - Daffodil configuration file format
   */
  DataProcessor withExternalVariables(File extVars) throws ExternalVariableException;

  /**
   * Obtain a new {@link DataProcessor} with multiple DFDL variables set.
   *
   * @param extVars a map of key/value pairs, where the key is the variable
   *                name, and the value is the value of the variable. The key
   *                may be preceded by a string of the form "{namespace}" to
   *                define a namespace for the variable. If preceded with "{}",
   *                then no namespace is used. If not preceded by anything,
   *                then Daffodil will figure out the namespace.
   * @return a new {@link DataProcessor} with multiple DFDL variables set.
   * @throws org.apache.daffodil.api.exceptions.ExternalVariableException if an error occurs while setting an external variable
   */
  DataProcessor withExternalVariables(Map<String, String> extVars) throws ExternalVariableException;

  /**
   * Save the DataProcessor
   * <p>
   * The resulting output can be reloaded by {@code Compiler.reload(savedParser:java\.nio\.channels\.ReadableByteChannel)* Compiler.reload}.
   * Note that any changes due to withValidator, withDebugger, and any compile diagnostics are not saved
   * 
   * @param output the byte channel to write the {@link DataProcessor} to. Note that external variable settings are not saved.
   */
  void save(WritableByteChannel output);


  /**
   * Walks the handler over the runtime metadata structures
   *
   * @param handler - the handler is called-back during the walk as each metadata structure is encountered.
   */
  void walkMetadata(MetadataHandler handler);

  /**
   * @return a new {@link DaffodilParseXMLReader} from the current {@link DataProcessor} for SAX Parsing.
   */
  DaffodilParseXMLReader newXMLReaderInstance();

  /**
   * @param output Writable Byte Channel for unparsing
   * @return a new {@link DaffodilUnparseContentHandler} from the current {@link DataProcessor} for SAX Unparsing.
   */
  DaffodilUnparseContentHandler newContentHandlerInstance(WritableByteChannel output);

  /**
   * Parse input data from an InputSourceDataInputStream and output the infoset to an InfosetOutputter
   *
   * @param input  data to be parsed
   * @param output the InfosetOutputter that will be used to output the infoset
   * @return an object which contains the result, and/or diagnostic information.
   */
  ParseResult parse(InputSourceDataInputStream input, InfosetOutputter output);

  /**
   * Unparse (i.e serializes) data from an InfosetInputter to the output
   *
   * @param input  the infoset inputter to use for unparsing
   * @param output the byte channel to write the data to
   * @return an object with contains diagnostic information
   */
  UnparseResult unparse(InfosetInputter input, WritableByteChannel output);
}
