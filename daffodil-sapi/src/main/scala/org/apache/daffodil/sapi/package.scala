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

package org.apache.daffodil

/**
 * Provides the classes necessary to compile DFDL schemas, parse and
 * unparse files using the compiled objects, and retrieve results and
 * parsing diagnostics
 *
 * <h3>Overview</h3>
 *
 * The [[Daffodil]] object is a factory object to create a [[Compiler]]. The
 * [[Compiler]] provides a method to compils a provided DFDL schema into a
 * [[ProcessorFactory]], which creates a [[DataProcessor]]:
 *
 * {{{
 * val c = Daffodil.compiler()
 * val pf = c.compileFile(file)
 * val dp = pf.onPath("/")
 * }}}
 *
 * The [[DataProcessor]] provides the necessary functions to parse and unparse
 * data, returning a [[ParseResult]] or [[UnparseResult]], respectively. These
 * contain information about the parse/unparse, such as whether or not the
 * processing succeeded any diagnostic information.
 *
 * <h4>Parse</h4>
 *
 * The [[DataProcessor#parse(input:org\.apache\.daffodil\.sapi\.io\.InputSourceDataInputStream,output:org\.apache\.daffodil\.sapi\.infoset\.InfosetOutputter)*]] method accepts input data to parse in the form
 * of a [[io.InputSourceDataInputStream]] and an [[infoset.InfosetOutputter]]
 * to determine the output representation of the infoset (e.g. Scala XML Nodes,
 * JDOM2 Documents, etc.):
 *
 * {{{
 * val scalaOutputter = new ScalaXMLInfosetOutputter()
 * val is = new InputSourceDataInputStream(data)
 * val pr = dp.parse(is, scalaOutputter)
 * val node = scalaOutputter.getResult
 * }}}
 *
 * The [[DataProcessor#parse(input:org\.apache\.daffodil\.sapi\.io\.InputSourceDataInputStream,output:org\.apache\.daffodil\.sapi\.infoset\.InfosetOutputter)*]] method is thread-safe and may be called multiple
 * times without the need to create other data processors. However,
 * [[infoset.InfosetOutputter]]'s are not thread safe, requiring a unique instance per
 * thread. An [[infoset.InfosetOutputter]] should call [[infoset.InfosetOutputter#reset]] before
 * reuse (or a new one should be allocated). For example:
 *
 * {{{
 * val scalaOutputter = new ScalaXMLInfosetOutputter()
 * files.foreach { f => {
 *   outputter.reset
 *   val is = new InputSourceDataInputStream(new FileInputStream(f))
 *   val pr = dp.parse(is, scalaOutputter)
 *   val node = scalaOutputter.getResult
 * }}
 * }}}
 *
 * One can repeat calls to parse() using the same InputSourceDataInputStream to continue parsing where the previous parse ended. For example:
 *
 * {{{
 * val is = new InputSourceDataInputStream(dataStream)
 * val scalaOutputter = new ScalaXMLInfosetOutputter()
 * val keepParsing = true
 * while (keepParsing) {
 *   scalaOutputter.reset()
 *   val pr = dp.parse(is, jdomOutputter)
 *   ...
 *   keepParsing = !pr.location().isAtEnd() && !pr.isError()
 * }}}
 *
 * <h4>Unparse</h4>
 *
 * The same [[DataProcessor]] used for parse can be used to unparse an infoset
 * via the [[DataProcessor#unparse(input*]] method. An [[infoset.InfosetInputter]]
 * provides the infoset to unparse, with the unparsed data written to the
 * provided java.nio.channels.WritableByteChannel. For example:
 *
 * {{{
 * val inputter = new ScalaXMLInfosetInputter(node)
 * val ur = dp.unparse(inputter, wbc)
 * }}}
 *
 * <h3>Failures and Diagnostics</h3>
 *
 * It is possible that failures could occur during the creation of the
 * [[ProcessorFactory]], [[DataProcessor]], or [[ParseResult]]. However, rather than
 * throwing an exception on error (e.g. invalid DFDL schema, parse
 * error, etc), these classes extend [[WithDiagnostics]], which is used to
 * determine if an error occurred, and any diagnostic information (see
 * [[Diagnostic]]) related to the step. Thus, before continuing, one must check
 * [[WithDiagnostics#isError]]. For example:
 *
 * {{{
 * val pf = c.compile(file)
 * if (pf.isError()) {
 *   val diags = pf.getDiagnostics()
 *   diags.foreach { d =>
 *     System.out.println(d.toString())
 *   }
 *   return -1;
 * }
 * }}}
 *
 * <h3>Saving and Reloading Parsers</h3>
 *
 * In some cases, it may be beneficial to save a parser and reload it.
 * For example, when starting up, it may be quicker to reload an
 * already compiled parser than to compile it from scratch. To save a
 * [[DataProcessor]]:
 *
 * {{{
 * val dp = pf.onPath("/")
 * dp.save(saveFile);
 * }}}
 *
 * And to restore a saved [[DataProcessor]]:
 *
 * {{{
 * val dp = Daffodil.reload(saveFile);
 * val pr = dp.parse(data, inputter);
 * }}}
 *
 */
package object sapi


