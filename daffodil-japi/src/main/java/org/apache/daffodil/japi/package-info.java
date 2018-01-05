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

/**
 * Provides the classes necessary to compile DFDL schemas, parse and
 * unparse files using the compiled objects, and retrieve results and
 * parsing diagnostics
 *
 * <h3>Overview</h3>
 *
 * The {@link org.apache.daffodil.japi.Daffodil} object is a factory object to create a {@link org.apache.daffodil.japi.Compiler}. The
 * {@link org.apache.daffodil.japi.Compiler} provides a method to compils a provided DFDL schema into a
 * {@link org.apache.daffodil.japi.ProcessorFactory}, which creates a {@link org.apache.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * Compiler c = Daffodil.compiler();
 * ProcessorFactory pf = c.compileFile(file);
 * DataProcessor dp = pf.onPath("/");
 * }</pre>
 *
 * The {@link org.apache.daffodil.japi.DataProcessor} provides the necessary functions to parse and unparse
 * data, returning a {@link org.apache.daffodil.japi.ParseResult} or {@link org.apache.daffodil.japi.UnparseResult}, respectively. These
 * contain information about the parse/unparse, such as whether or not the
 * processing succeeded any diagnostic information.
 *
 * <h4>Parse</h4>
 *
 * The {@link org.apache.daffodil.japi.DataProcessor#parse(java.nio.channels.ReadableByteChannel, org.apache.daffodil.japi.infoset.InfosetOutputter, long)} method accepts input data to parse in the form
 * of a {@link java.nio.channels.ReadableByteChannel} and an {@link org.apache.daffodil.japi.infoset.InfosetOutputter}
 * to determine the output representation of the infoset (e.g. Scala XML Nodes,
 * JDOM2 Documents, etc.):
 *
 * <pre>
 * {@code
 * JDOMInfosetOutputter jdomOutputter= new JDOMInfosetOutputter();
 * ParseResult pr = dp.parse(data, jdomOutputter);
 * Document doc = jdomOutputter.getResult();
 * }</pre>
 *
 * The {@link org.apache.daffodil.japi.DataProcessor#parse(java.nio.channels.ReadableByteChannel, org.apache.daffodil.japi.infoset.InfosetOutputter, long)} method is thread-safe and may be called multiple
 * times without the need to create other data processors. However,
 * {@link org.apache.daffodil.japi.infoset.InfosetOutputter}'s are not thread safe, requiring a unique instance per
 * thread. An {@link org.apache.daffodil.japi.infoset.InfosetOutputter} should call {@link org.apache.daffodil.japi.infoset.InfosetOutputter#reset()} before
 * reuse (or a new one should be allocated). For example:
 *
 * <pre>
 * {@code
 * JDOMInfosetOutputter jdomOutputter = new JDOMInfosetOutputter();
 * for (File f : inputFiles) {
 *   jdomOutputter.reset();
 *   ParseResult pr = dp.parse(f, jdomOutputter);
 *   Document doc = jdomOutputter.getResult();
 * }
 * }</pre>
 *
 * <h4>Unparse</h4>
 *
 * The same {@link org.apache.daffodil.japi.DataProcessor} used for parse can be used to unparse an infoset
 * via the {@link org.apache.daffodil.japi.DataProcessor#unparse(org.apache.daffodil.japi.infoset.InfosetInputter, java.nio.channels.WritableByteChannel)} method. An {@link org.apache.daffodil.japi.infoset.InfosetInputter}
 * provides the infoset to unparse, with the unparsed data written to the
 * provided {@link java.nio.channels.WritableByteChannel}. For example:
 *
 * <pre>
 * {@code
 * JDOMInfosetInputter jdomInputter = new JDOMInfosetInputter(doc);
 * UnparseResult ur = dp.unparse(jdomInputter, wbc)
 * }</pre>
 *
 * <h3>Failures and Diagnostics</h3>
 *
 * It is possible that failures could occur during the creation of the
 * {@link org.apache.daffodil.japi.ProcessorFactory}, {@link org.apache.daffodil.japi.DataProcessor}, or {@link org.apache.daffodil.japi.ParseResult}. However, rather than
 * throwing an exception on error (e.g. invalid DFDL schema, parse
 * error, etc), these classes extend {@link org.apache.daffodil.japi.WithDiagnostics}, which is used to
 * determine if an error occurred, and any diagnostic information (see
 * {@link org.apache.daffodil.japi.Diagnostic}) related to the step. Thus, before continuing, one must check
 * {@link org.apache.daffodil.japi.WithDiagnostics#isError}. For example:
 *
 * <pre>
 * {@code
 * ProcessorFactor pf = c.compile(files);
 * if (pf.isError()) {
 *   java.util.List<Diagnostic> diags = pf.getDiagnostics();
 *   foreach (Diagnostic d : diags) {
 *     System.out.println(d.toString());
 *   }
 *   return -1;
 * }
 * }</pre>
 *
 * <h3>Saving and Reloading Parsers</h3>
 *
 * In some cases, it may be beneficial to save a parser and reload it.
 * For example, when starting up, it may be quicker to reload an
 * already compiled parser than to compile it from scratch. To save a
 * {@link org.apache.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * DataProcessor dp = pf.onPath("/");
 * dp.save(saveFile);
 * }</pre>
 *
 * And to restore a saved {@link org.apache.daffodil.japi.DataProcessor}:
 *
 * <pre>
 * {@code
 * DataProcessor dp = Daffodil.reload(saveFile);
 * ParseResult pr = dp.parse(data);
 * }</pre>
 *
 */

package org.apache.daffodil.japi;

