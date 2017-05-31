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

import java.io.StringWriter
import java.io.PrintWriter

import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionDiagnosticBase
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.Processor
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.ModelGroupRuntimeData

/**
 * Encapsulates lower-level parsing with a uniform interface
 *
 * A parser can have sub-parsers. See also PrimParser which is a parser with no sub-parsers.
 *
 * This trait is preferred (and PrimParser) over the ParserObject because this requires one to
 * explicitly manage runtimeDependencies, whereas ParserObject hides that detail, which isn't helpful.
 */
trait Parser
  extends Processor {

  protected def parserName = Misc.getNameFromClass(this)

  def PE(pstate: PState, s: String, args: Any*) = {
    pstate.setFailed(new ParseError(One(context.schemaFileLocation), One(pstate.currentLocation), s, args: _*))
  }

  def processingError(state: PState, str: String, args: Any*) =
    PE(state, str, args) // long form synonym

  protected def parse(pstate: PState): Unit

  final def parse1(pstate: PState) = {
    Assert.invariant(isInitialized)
    val savedParser = pstate.maybeProcessor
    pstate.setProcessor(this)
    if (pstate.dataProc.isDefined) pstate.dataProc.get.before(pstate, this)
    try {
      pstate.processor.context match {
        case mgrd: ModelGroupRuntimeData =>
          ParserBitOrderChecks.checkParseBitOrder(pstate)
        case _ => // ok. Elements are checked elsewhere.
      }
      parse(pstate)
    } catch {
      /*
       * We only catch ParseError here as we expect the majority of
       * exceptions to be handled internally. Parsers should call
       * state.setFailed(exception).
       *
       * When exceptions are necessary we expect them to be handled internally
       * in DPath.scala and DPathRuntime where they are either converted to
       * ParseError, SDE or rethrown (when necessary).
       *
       * In DPath.scala there's a method called handleThrow which catches:
       * InfosetException, VariableException, ExpressionEvaluationException,
       * IllegalSTateException, NumberFormatException, ArithmeticException
       * and FNErrorException.  Upon which we call handleCompileState or
       * doPE based upon the exception type. HandleCompileState further
       * decides whether to doSDE, doPE or just call state.setFailed(e). Any
       * other exception is simply rethrown.
       *
       * In DPathRuntime.runExpressionForConstant there's a catch for
       * InfosetException, VariableException, IllegalStateException,
       * IndexOutOfBoundsException, IllegalArgumentException and
       * FNErrorException that just returns false.  For ArithmeticException,
       * NumberFormatException, SchemaDefinitionDiagnosticsBase and
       * ProcessingError it throws a new SDE.
       * */
      case pe: ParseError => pstate.setFailed(pe)
    }
    if (pstate.dataProc.isDefined) pstate.dataProc.get.after(pstate, this)
    pstate.setMaybeProcessor(savedParser)
  }
}

// Deprecated and to be phased out. Use the trait Parser instead.
abstract class ParserObject(override val context: RuntimeData)
  extends Parser {

  override lazy val runtimeDependencies = Nil
}

class SeqCompParser(context: RuntimeData, val childParsers: Array[Parser])
  extends ParserObject(context) {

  override def childProcessors = childParsers

  override def nom = "seq"

  val numChildParsers = childParsers.size

  def parse(pstate: PState): Unit = {
    var i: Int = 0
    while (i < numChildParsers) {
      val parser = childParsers(i)
      parser.parse1(pstate)
      if (pstate.processorStatus ne Success)
        return
      i += 1
    }
  }

}

class AltCompParser(ctxt: RuntimeData, val childParsers: Seq[Parser])
  extends ParserObject(ctxt) {

  override lazy val childProcessors = childParsers

  override def nom = "alt"

  def parse(pstate: PState): Unit = {
    var pBefore: PState.Mark = null
    var markLeakCausedByException = false;

    try {
      pstate.pushDiscriminator
      var diagnostics: Seq[Diagnostic] = Nil
      var i = 0
      var parser: Parser = null
      val limit = childParsers.length
      var returnFlag = false
      while (!returnFlag && i < limit) {
        parser = childParsers(i)
        i += 1
        log(LogLevel.Debug, "Trying choice alternative: %s", parser)
        pBefore = pstate.mark("AltCompParser1")
        try {
          parser.parse1(pstate)
        } catch {
          case d: SchemaDefinitionDiagnosticBase => {
            pstate.discard(pBefore)
            pBefore = null
            throw d
          }
        }
        if (pstate.processorStatus eq Success) {
          log(LogLevel.Debug, "Choice alternative success: %s", parser)
          pstate.discard(pBefore)
          pBefore = null
          returnFlag = true
        } else {
          // If we get here, then we had a failure
          log(LogLevel.Debug, "Choice alternative failed: %s", parser)
          //
          // capture diagnostics
          //
          val diag = new ParseAlternativeFailed(context.schemaFileLocation, pstate, pstate.diagnostics)
          diagnostics = diag +: diagnostics
          //
          // check for discriminator evaluated to true.
          //
          if (pstate.discriminator == true) {
            log(LogLevel.Debug, "Failure, but discriminator true. Additional alternatives discarded.")
            // If so, then we don't run the next alternative, we
            // consume this discriminator status result (so it doesn't ripple upward)
            // and return the failed state with all the diagnostics.
            //
            val allDiags = new AltParseFailed(context.schemaFileLocation, pstate, diagnostics.reverse)
            pstate.discard(pBefore) // because disc set, we don't unwind side effects on input stream & infoset
            pBefore = null
            pstate.setFailed(allDiags)
            returnFlag = true
          } else {
            //
            // Here we have a failure, but no discriminator was set, so we try the next alternative.
            // Which means we just go around the loop
            //
            // But we have to unwind side-effects on input-stream, infoset, variables, etc.
            pstate.reset(pBefore)
            pBefore = null
            returnFlag = false
          }
        }
      }
      Assert.invariant(i <= limit)
      if (returnFlag == false) {
        Assert.invariant(i == limit)
        // Out of alternatives. All of them failed.
        val allDiags = new AltParseFailed(context.schemaFileLocation, pstate, diagnostics.reverse)
        pstate.setFailed(allDiags)
        log(LogLevel.Debug, "All AltParser alternatives failed.")
      }

      pstate.popDiscriminator
    } catch {
      // Similar try/catch/finally logic for returning marks is also used in
      // the RepAtMostTotalNParser and RepUnboundedParser. The logic isn't
      // easily factored out so it is duplicated. Changes made here should also
      // be made there. Only these parsers deal with taking marks, so this logic
      // should not be needed elsewhere.
      case t: Throwable => {
        if (pBefore != null) {
          markLeakCausedByException = true
          if (!t.isInstanceOf[SchemaDefinitionDiagnosticBase] && !t.isInstanceOf[UnsuppressableException]) {
            val stackTrace = new StringWriter()
            t.printStackTrace(new PrintWriter(stackTrace))
            Assert.invariantFailed("Exception thrown with mark not returned: " + t + "\nStackTrace:\n" + stackTrace)
          }
        }
        throw t
      }
    } finally {
      var markLeak = false;
      if (pBefore != null) {
        pstate.discard(pBefore)
        markLeak = true;
      }

      if (markLeak && !markLeakCausedByException) {
        // likely a logic bug, throw assertion
        Assert.invariantFailed("mark not returned, likely a logic bug")
      }
    }
  }

}

case class DummyParser(rd: TermRuntimeData) extends ParserObject(null) {
  def parse(pstate: PState): Unit = pstate.SDE("Parser for " + rd + " is not yet implemented.")

  override def childProcessors = Nil
  override def toBriefXML(depthLimit: Int = -1) = "<dummy/>"
  override def toString = if (rd == null) "Dummy[null]" else "Dummy[" + rd + "]"
}

case class NotParsableParser(context: ElementRuntimeData) extends Parser {

  def parse(state: PState): Unit = {
    // We can't use state.SDE because that needs the infoset to determine the
    // context. No infoset will exist when this is called, so we'll manually
    // create an SDE and toss it
    val rsde = new RuntimeSchemaDefinitionError(context.schemaFileLocation, state, "This schema was compiled without parse support. Check the parseUnparsePolicy tunable or daf:parseUnparsePolicy property.")
    context.toss(rsde)
  }

  override lazy val childProcessors = Nil
  override lazy val runtimeDependencies = Nil
}
