/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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
 * The main class to use is [[Daffodil]] to
 * create a [[Compiler]]:
 *
 * {{{
 * val c = Daffodil.compiler()
 * }}}
 *
 * This can then be used to compiled a DFDL schema, and generate a
 * [[ProcessorFactory]]:
 *
 * {{{
 * val pf = c.compileFile(file)
 * }}}
 *
 * This can then be used to create a [[DataProcessor]]:
 *
 * {{{
 * val dp = pf.onPath("/")
 * }}}
 *
 * This can then be used to parse data, returning a [[ParseResult]], which contains the
 * DFDL infoset in either a jdom2 Document or a scala XML Node:
 *
 * {{{
 * val pr = dp.parse(data)
 * val infoset = pr.result()
 * }}}
 *
 * The [[DataProcessor.parse(java.nio.channels.ReadableByteChannel)]]
 * method may be called multiple times without the need to create
 * another data processors. For example:
 *
 * {{{
 * files.foreach { f => {
 *   val pr = dp.parse(f)
 *   val infoset = pr.result()
 * }}
 * }}}
 *
 * <h3>Failures &amp; Diagnostics</h3>
 *
 * It is possible that failures could occur during the creation of the
 * [[ProcessorFactory]], [[DataProcessor]], or [[ParseResult]]. However, rather than
 * throwing an exception on error (e.g. invalid DFDL schema, parse
 * error, etc), these classes extend [[WithDiagnostics]], which is used to
 * determine if an error occured, and any diagnostic information (see
 * [[Diagnostic]]) related to the
 * step. thus, before contining, one must check [[WithDiagnostics#isError]]. For
 * example:
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
 * <h3>Saving &amp; Reloading Parsers</h3>
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
 * val pr = dp.parse(data);
 * }}}
 *
 */
package object sapi

