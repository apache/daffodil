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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionDiagnosticBase
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.processors.ProcessingError
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One

class ParseError(rd: Maybe[SchemaFileLocation], val loc: Maybe[DataLocation], causedBy: Maybe[Throwable], kind: Maybe[String], args: Any*)
  extends ProcessingError("Parse", rd, loc, causedBy, kind, args: _*) {
  def this(rd: Maybe[SchemaFileLocation], loc: Maybe[DataLocation], kind: String, args: Any*) =
    this(rd, loc, Maybe.Nope, Maybe(kind), args: _*)

  override def toParseError = this
}

class AssertionFailed(rd: SchemaFileLocation, state: PState, msg: String, details: Maybe[String] = Nope)
  extends ParseError(One(rd), One(state.currentLocation), "Assertion failed. %s", msg) {
  override def componentText: String = {

    if (details.isDefined) "\nDetails: " + details.get
    else ""

  }
}

class ParseAlternativeFailed(rd: SchemaFileLocation, state: PState, val errors: Seq[Diagnostic])
  extends ParseError(One(rd), One(state.currentLocation), "Alternative failed. Reason(s): %s", errors)

class AltParseFailed(rd: SchemaFileLocation, state: PState,
                     diags: Seq[Diagnostic])
  extends ParseError(One(rd), One(state.currentLocation), "All alternatives failed. Reason(s): %s", diags) {

  override def getLocationsInSchemaFiles: Seq[LocationInSchemaFile] = diags.flatMap { _.getLocationsInSchemaFiles }

  override def getDataLocations: Seq[DataLocation] = {
    // all should have the same starting location if they are alternatives.
    val dataLocs = diags.flatMap { _.getDataLocations }
    // TBD: what is the idiom for "insert a equals sign between all the elements of the list...??"
    // Well, this works, but isn't there a one-liner for this idiom.
    val allAreSame = dataLocs match {
      case f :: r => !r.exists { _ != f }
      case _ => true
    }
    Assert.invariant(allAreSame)
    diags.head.getDataLocations
  }
}

class ChoiceDispatchNoMatch(rd: SchemaFileLocation, state: PState, val key: String)
  extends ParseError(One(rd), One(state.currentLocation), "Choice dispatch key (%s) failed to match any of the branch keys.", key)

class ChoiceDispatchFailed(rd: SchemaFileLocation, state: PState, val errors: Seq[Diagnostic])
  extends ParseError(One(rd), One(state.currentLocation), "Choice dispatch branch failed: %s", errors)

class GeneralParseFailure(msg: String) extends Diagnostic(Nope, Nope, Nope, Maybe(msg)) {
  Assert.usage(msg != null && msg != "")
  override def isError = true
  override def modeName = "Parse"
}

/**
 * Mix this into parsers that have deep algorithms that are spread over multiple classes.
 *
 * These allow one to bend the rule about parsers not throwing ParseError so that
 * if you are inside a parser, but you are way down a bunch of calls away from the parser itself
 * you can throw, and it will be intercepted and proper behavior (not throwing, but returning
 * a failed status) will result.
 *
 * Use like this:
 * @example {{{
 * withParseErrorThrowing(pstate) { // something enclosing like the parser
 * ...
 *   // calls something which calls something which eventually calls
 *       PE(context, bitOffset % 8 == 0, "must be byte boundary, not bit %s", bitOffset)
 * ...
 * }
 * }}}
 */

trait DoSDEMixin {

  protected final def doSDE(e: Throwable, state: ParseOrUnparseState) = {
    e match {
      case sde: SchemaDefinitionDiagnosticBase => {
        state.setFailed(sde)
        throw sde
      }
      case other => {
        val sde = new RuntimeSchemaDefinitionError(state.getContext().schemaFileLocation, state, Maybe(e), Nope)
        state.setFailed(sde)
        throw sde
      }
    }
  }
}
