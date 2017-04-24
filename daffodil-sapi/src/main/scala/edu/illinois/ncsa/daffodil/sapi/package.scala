/* Copyright (c) 2012-2017 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil

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
 * The [[DataProcessor#parse(input:java\.nio\.channels\.ReadableByteChannel,output:edu\.illinois\.ncsa\.daffodil\.sapi\.infoset\.InfosetOutputter)*]] method accepts input data to parse in the form
 * of a java.nio.channels.ReadableByteChannel and an [[infoset.InfosetOutputter]]
 * to determine the output representation of the infoset (e.g. Scala XML Nodes,
 * JDOM2 Documents, etc.):
 *
 * {{{
 * val scalaOutputter = new ScalaXMLInfosetOutputter()
 * val pr = dp.parse(data, scalaOutputter)
 * val node = scalaOutputter.getResult
 * }}}
 *
 * The [[DataProcessor#parse(input:java\.nio\.channels\.ReadableByteChannel,output:edu\.illinois\.ncsa\.daffodil\.sapi\.infoset\.InfosetOutputter)*]] method is thread-safe and may be called multiple
 * times without the need to create other data processors. However,
 * [[infoset.InfosetOutputter]]'s are not thread safe, requiring a unique instance per
 * thread. An [[infoset.InfosetOutputter]] should call [[infoset.InfosetOutputter#reset]] before
 * reuse (or a new one should be allocated). For example:
 *
 * {{{
 * val scalaOutputter = new ScalaXMLInfosetOutputter()
 * files.foreach { f => {
 *   outputter.reset
 *   val pr = dp.parse(f, scalaOutputter)
 *   val node = scalaOutputter.getResult
 * }}
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


