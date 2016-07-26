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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionDiagnosticBase
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError

class ParseError(rd: Maybe[SchemaFileLocation], val loc: Maybe[DataLocation], kind: String, args: Any*)
  extends ProcessingError("Parse Error", rd, loc, kind, args: _*)

class AssertionFailed(rd: SchemaFileLocation, state: PState, msg: String, details: Maybe[String] = Nope)
  extends ParseError(One(rd), One(state.currentLocation), "Assertion failed. %s", msg) {
  override def componentText: String = {
    val currentElem = state.infoset

    val parsedValue =
      "\nParsed value was: " + currentElem.toXML().toString

    val finalString =
      if (details.isDefined) "\nDetails: " + details.get + parsedValue
      else parsedValue
    finalString
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

class GeneralParseFailure(msg: String) extends Throwable with DiagnosticImplMixin {
  Assert.usage(msg != null && msg != "")
  override def getMessage() = msg
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
        val sde = new RuntimeSchemaDefinitionError(state.getContext().schemaFileLocation, state, e.getMessage())
        state.setFailed(sde)
        throw sde
      }
    }
  }
}

trait WithParseErrorThrowing extends DoSDEMixin {

  /**
   * Use to check for parse errors.
   *
   * Must be used only in the context of the withParseErrorThrowing wrapper.
   *
   * The schema component providing the context is implicit (via def context virtual member)
   */
  def PE(context: SchemaFileLocation, kind: String, args: Any*): Nothing = {
    Assert.usage(WithParseErrorThrowing.flag, "Must use inside of withParseErrorThrowing construct.")
    throw new ParseError(One(context), Nope, kind, args: _*)
  }


  /**
   * Wrap around parser code that wants to throw parse errors (e.g., parsers which call things which
   * call things which detect a parse error want to throw back to this)
   *
   * This wrapper then implements the required behavior for parsers
   * that being returning a failed parser state.
   */
  @inline final def withParseErrorThrowing(pstate: PState)(body: => Unit): Unit = {
    val saveCanThrowParseErrors = WithParseErrorThrowing.flag
    WithParseErrorThrowing.flag = true
    try body
    catch {
      case e: InfosetException =>
        doSDE(e, pstate)
      case v: VariableException =>
        doSDE(v, pstate)
      case e: IndexOutOfBoundsException => {
        // This should come through as an InfosetException, not a raw java exception like this.
        Assert.invariantFailed("Should not be allowing propagation of " + e)
        // pstate.setFailed(new ParseError(Nope, One(pstate.currentLocation), "%s", e))
      }
      case e: ParseError => {
        pstate.setFailed(e)
      }
      // TODO: Runtime SDEs should be distinguished somehow usefully.
      //        case e : SchemaDefinitionError => {
      //          val res = pstate.failed(e)
      //          res
      //        }
      //
      case e: NumberFormatException =>
        handleNumberFormatException(pstate, e)
      // Note: We specifically do not catch other exceptions here
      // On purpose. If those exist, then there's someplace that should have already caught them
      // and turned them into a thrown parse error, or a schema definition error.
      //
      // Other kinds of spontaneous throws are bugs, and we don't want to mask them by
      // putting blanket catches in.
      //
    } finally {
      WithParseErrorThrowing.flag = saveCanThrowParseErrors
    }
  }

  final protected def handleNumberFormatException(pstate: PState, e: NumberFormatException) = {
    val ie = pstate.infoset.asInstanceOf[InfosetElement]
    val msg =
      if (e.getMessage() != null && e.getMessage() != "")
        e.getMessage()
      else Misc.getNameFromClass(e)
    val pe = new ParseError(One(ie.runtimeData.schemaFileLocation), One(pstate.currentLocation), msg)
    pstate.setFailed(pe)
  }

  /**
   * Use to check things that really are schema-definition issues, but we can't check until run-time.
   * E.g., since byteOrder might be an expression, if the expression returns neither bigEndian nor littleEndian,
   * then it's an SDE, but we didn't know until runtime.
   *
   * No catching for this SDE throw, since SDEs are fatal.
   */
  def SDECheck(testTrueMeansOK: Boolean, context: SchemaFileLocation, pstate: PState, kind: String, args: Any*) = {
    if (!testTrueMeansOK) {
      throw new SchemaDefinitionError(Some(context), None, kind, args: _*)
    }
  }
}

/**
 * Global flag to insure we aren't throwing ParseErrors in a context that won't catch them
 * properly.
 */
object WithParseErrorThrowing {
  // TODO: FIXME Bad bad global state. This flag needs to live in the DataProcessor object,
  // or find a way to have it go away entirely.
  var flag: Boolean = false

  /**
   * for unit tests and other context where you want to exercise runtime code that might throw PEs
   * but you are not inside a parser
   */
  def pretendThisIsAParser[T](body: => T) = {
    val savedFlag: Boolean = WithParseErrorThrowing.flag
    try {
      WithParseErrorThrowing.flag = true
      body
    } finally {
      WithParseErrorThrowing.flag = savedFlag
    }
  }
}
