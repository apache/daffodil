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

package edu.illinois.ncsa.daffodil.processors.unparsers
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation

class UnparseAlternativeFailed(rd: RuntimeData, loc: DataLocation, val errors: Seq[Diagnostic])
  extends UnparseError(One(rd.schemaFileLocation), One(loc), "Alternative failed. Reason(s): %s", errors)

object UnparseError {
  def apply(rd: Maybe[SchemaFileLocation], loc: Maybe[DataLocation], formatString: String, args: Any*) = {
    val ue = new UnparseError(rd, loc, formatString, args: _*)
    throw ue
  }
}

class UnparseError(rd: Maybe[SchemaFileLocation], loc: Maybe[DataLocation], kind: String, args: Any*)
  extends ProcessingError("Unparse Error", rd, loc, kind, args: _*)

