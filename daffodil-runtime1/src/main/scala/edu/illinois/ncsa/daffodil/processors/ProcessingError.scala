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
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation

abstract class ProcessingError(
  pOrU: String,
  rd: Maybe[SchemaFileLocation],
  loc: Maybe[DataLocation],
  kind: String,
  args: Any*)
  extends Exception with DiagnosticImplMixin {

  /**
   * Used to convert a processing error into a parse error so that it
   * looks like the same as other parse errors to tests that search for the
   * "Parse Error" string.
   */
  def toParseError = new ParseError(rd, loc, kind, args: _*)

  /**
   * Used to convert a processing error into a unparse error so that it
   * looks like the same as other unparse errors to tests that search for the
   * "Unparse Error" string.
   */
  def toUnparseError = new UnparseError(rd, loc, kind, args: _*)

  override def getLocationsInSchemaFiles: Seq[LocationInSchemaFile] = rd.toSeq

  override def getDataLocations: Seq[DataLocation] = loc.toSeq

  private lazy val schemaLocationsString = {
    val strings = getLocationsInSchemaFiles.map { _.locationDescription }
    val res = if (strings.length > 0)
      " " + strings.mkString(", ")
    else
      " (no schema file location)"
    res
  }

  def componentText: String = ""

  override def toString = msg
  //
  // Right here is where we would lookup the symbolic error kind id, and
  // choose a locale-based message string.
  //
  // For now, we'll just do an automatic English message.
  //
  lazy val msg = {
    val m =
      if (args.size > 0) {
        try {
          kind.format(args: _*)
        } catch {
          case e: IllegalArgumentException =>
            throw new IllegalArgumentException("""format string "%s" did not accept these arguments: %s""".format(kind, args.mkString(", ")))
        }
      } else kind
    val res = pOrU + ": " + m +
      componentText +
      "\nSchema context: %s%s".format((if (rd.isDefined) rd.value.toString else "(no schema component identifier)"), schemaLocationsString) +
      (if (loc.isDefined)
        "\nData location was preceding %s".format(loc.value)
      else
        "(no data location)")
    res
  }

  override def getMessage = msg
}
