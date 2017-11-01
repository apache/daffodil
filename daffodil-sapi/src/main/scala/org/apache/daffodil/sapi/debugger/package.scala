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

package org.apache.daffodil.sapi

/**
 * Provides the classes necessary to perform parse tracing or create a custom debugger
 *
 * <h3>Overview</h3>
 *
 * Daffodil comes with one prebuilt debugger, the [[TraceDebuggerRunner]], which outputs
 * verbose information during the parsing processes, which can be used to aid
 * in debugging a DFDL schema. For example, the [[TraceDebuggerRunner]] can be use like so:
 *
 * {{{
 * val tdr = new TraceDebuggerRunner()
 * Daffodil.setDebugger(tdr)
 * }}}
 *
 * Additionally, one may create their own debugger runner by implementing the
 * methods in the [[DebuggerRunner]].
 * <p>
 * Once the debugger is set, it must then be turned on, like so:
 *
 * {{{
 * Daffodil.setDebugging(true);
 * }}}
 */

package object debugger

