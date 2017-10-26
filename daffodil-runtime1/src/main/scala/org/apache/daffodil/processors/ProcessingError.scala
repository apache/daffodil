/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.processors.parsers.ParseError

abstract class ProcessingError protected (
  override val modeName: String,
  schemaContext: Maybe[SchemaFileLocation],
  dataContext: Maybe[DataLocation],
  val maybeCause: Maybe[Throwable], // use this OR the format string, Not both.
  val maybeFormatString: Maybe[String],
  val args: Any*)
  extends Diagnostic(schemaContext, dataContext, maybeCause, maybeFormatString, args: _*) {

  override def isError = true

  def this(
    modeName: String,
    rd: Maybe[SchemaFileLocation],
    loc: Maybe[DataLocation],
    fmtString: String,
    args: Any*) = this(modeName, rd, loc, Maybe.Nope, Maybe(fmtString), args: _*)

  /**
   * Used to convert a processing error into a parse error so that it
   * looks like the same as other parse errors to tests that search for the
   * "Parse Error" string.
   */
  def toParseError = new ParseError(schemaContext, dataContext, Maybe(this), maybeFormatString, args: _*)

  /**
   * Used to convert a processing error into a unparse error so that it
   * looks like the same as other unparse errors to tests that search for the
   * "Unparse Error" string.
   */
  def toUnparseError = new UnparseError(schemaContext, dataContext, maybeCause, maybeFormatString, args: _*)

}
