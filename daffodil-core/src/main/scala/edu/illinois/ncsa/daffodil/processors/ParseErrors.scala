package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.xml.Node
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.dsom.AnnotatedSchemaComponent
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionWarning
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.dsom.SchemaComponentRegistry
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.dsom.ValidationError
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

abstract class ProcessingError extends Exception with DiagnosticImplMixin

class ParseError(sc: SchemaComponent, val pstate: Maybe[PState], kind: String, args: Any*)
  extends ProcessingError {
  override def getLocationsInSchemaFiles: Seq[LocationInSchemaFile] = List(sc)
  override def getDataLocations: Seq[DataLocation] = pstate.map { _.currentLocation }.toList

  def componentText: String = ""

  override def toString = {
    lazy val argsAsString = args.map { _.toString }.mkString(", ")
    //
    // Right here is where we would lookup the symbolic error kind id, and
    // choose a locale-based message string.
    //
    // For now, we'll just do an automatic English message.
    //
    val msg = {
      if (args.size > 0) kind.format(args: _*)
      else kind
    }
    val res = "Parse Error: " + msg +
      componentText +
      "\nSchema context: %s %s".format(sc, sc.locationDescription) +
      pstate.map { ps => "\nData location was preceding %s".format(ps.currentLocation) }.getOrElse("(no data location)")
    res
  }

  override def getMessage = toString
}

class AssertionFailed(sc: SchemaComponent, state: PState, msg: String, details: Maybe[String] = Nope)
  extends ParseError(sc, One(state), "Assertion failed. %s", msg) {
  override def componentText: String = {
    val currentElem = state.infoset

    val parsedValue =
      if (currentElem.jdomElt.isDefined) {
        val jdomElem = currentElem.jdomElt.get
        "\nParsed value was: " + {
          if (jdomElem.getChildren().size() > 0) {
            // Complex
            val name = jdomElem.getName()
            "<" + name + ">...</" + name + ">"
          } else {
            // Simple
            currentElem.toBriefXML.toString
          }
        }
      } else ""
    
    val finalString =
      if (details.isDefined) "\nDetails: " + details.get + parsedValue
      else parsedValue
    finalString
  }
}

class ParseAlternativeFailed(sc: SchemaComponent, state: PState, val errors: Seq[Diagnostic])
  extends ParseError(sc, One(state), "Alternative failed. Reason(s): %s", errors)

class AltParseFailed(sc: SchemaComponent, state: PState,
  diags: Seq[Diagnostic])
  extends ParseError(sc, One(state), "All alternatives failed. Reason(s): %s", diags) {

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
 *       PECheck(bitOffset % 8 == 0, "must be byte boundary, not bit %s", bitOffset)
 * ...
 * }
 * }}}
 */
trait WithParseErrorThrowing {

  def context: SchemaComponent

  /**
   * Use to check for parse errors.
   *
   * Must be used only in the context of the withParseErrorThrowing wrapper.
   *
   * The schema component providing the context is implicit (via def context virtual member)
   */
  def PECheck(
    testTrueMeansOK: => Boolean,
    kind: String, args: Any*) {
    Assert.usage(WithParseErrorThrowing.flag, "Must use inside of withParseErrorThrowing construct.")
    if (!testTrueMeansOK) {
      throw new ParseError(context, Nope, kind, args: _*)
    }
  }

  /**
   * Passing the context explicitly
   */
  def PECheck(contextArg: SchemaComponent,
    testTrueMeansOK: => Boolean,
    kind: String, args: Any*) {
    Assert.usage(WithParseErrorThrowing.flag, "Must use inside of withParseErrorThrowing construct.")
    if (!testTrueMeansOK) {
      throw new ParseError(contextArg, Nope, kind, args: _*)
    }
  }

  def PE(kind: String, args: Any*): Nothing = {
    PE(context, kind, args: _*)
  }

  def PE(context: SchemaComponent, kind: String, args: Any*): Nothing = {
    Assert.usage(WithParseErrorThrowing.flag, "Must use inside of withParseErrorThrowing construct.")
    throw new ParseError(context, Nope, kind, args: _*)
  }

  /**
   * Wrap around parser code that wants to throw parse errors (e.g., parsers which call things which
   * call things which detect a parse error want to throw back to this)
   *
   * This wrapper then implements the required behavior for parsers
   * that being returning a failed parser state.
   */
  def withParseErrorThrowing(pstate: PState)(body: => PState): PState = {
    val saveCanThrowParseErrors = WithParseErrorThrowing.flag
    WithParseErrorThrowing.flag = true
    val result =
      try body
      catch {
        case e: ParseError => {
          val maybePS = e.pstate
          // if there is a maybePS, then use it to create the failed state (because it 
          // is probably more specific about the failure location), otherwise
          // use the one passed as an argument. 
          val res = maybePS.map { _.failed(e) }.getOrElse(pstate.failed(e))
          res
        }
        // TODO: Runtime SDEs should be distinguished somehow usefully.
        //        case e : SchemaDefinitionError => {
        //          val res = pstate.failed(e)
        //          res
        //        }
        //
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
    result
  }

  /**
   * Use to check things that really are schema-definition issues, but we can't check until run-time.
   * E.g., since byteOrder might be an expression, if the expression returns neither bigEndian nor littleEndian,
   * then it's an SDE, but we didn't know until runtime.
   *
   * No catching for this SDE throw, since SDEs are fatal.
   */
  def SDECheck(testTrueMeansOK: => Boolean, context: SchemaComponent, pstate: PState, kind: String, args: Any*) = {
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
