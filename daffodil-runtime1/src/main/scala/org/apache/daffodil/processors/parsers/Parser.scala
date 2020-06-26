/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.processors.parsers

import java.io.StringWriter
import java.io.PrintWriter

import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.UnsuppressableException
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Processor
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Misc
import org.apache.daffodil.io.BacktrackingException
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.CombinatorProcessor
import org.apache.daffodil.processors.PrimProcessor
import org.apache.daffodil.processors.TextProcessor
import org.apache.daffodil.processors.PrimProcessorNoData

/**
 * Encapsulates lower-level parsing with a uniform interface
 *
 * A parser can have sub-parsers. See also PrimParser which is a parser with no sub-parsers,
 * and CombinatorParser, which is a parser with sub parsers.
 */
sealed trait Parser
  extends Processor {

  def isEmpty = false // override in NadaParser

  protected lazy val parserName = Misc.getNameFromClass(this)

  def PE(pstate: PState, s: String, args: Any*) = {
    pstate.setFailed(new ParseError(One(context.schemaFileLocation), One(pstate.currentLocation), s, args: _*))
  }

  def PENotEnoughBits(pstate: PState, neededBits: Long, remainingBits: MaybeULong) = {
    val remainingStr = if (remainingBits.isDefined) s" but found only ${remainingBits.get} available" else ""
    PE(pstate, "Insufficient bits in data. Needed %d bit(s)%s.", neededBits, remainingStr)
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
      case be: BacktrackingException => pstate.SDE("Attempted to backtrack too far: " + be.getMessage())
    } finally {
      pstate.resetFormatInfoCaches()
      if (pstate.dataProc.isDefined) pstate.dataProc.get.after(pstate, this)
      pstate.setMaybeProcessor(savedParser)
    }
  }
}

/**
 * A PrimParser is a parser that contains no child parsers.
 * Combinators are NOT PrimParser
 */
trait PrimParser
  extends PrimProcessor
  with Parser

/**
 * A parser which is "primitive" no sub-parsers, but which doesn't
 * parses any data. Evaluates expressions, binds variables, evaluates
 * asserts, etc.
 */
trait PrimParserNoData
  extends PrimProcessorNoData
  with Parser

/**
 * Mixed in as a marker for parsers that only operate on text.
 */
trait TextPrimParser
  extends PrimParser
  with TextProcessor

/**
 * Parser which does "Nada" (Nothing in Spanish)
 *
 * Used for optionality in some cases, but is usually recognized and
 * optimized out.
 */
final class NadaParser(override val context: RuntimeData)
  extends PrimParserNoData {
  override def runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()

  override def isEmpty = true

  override def toString = "Nada"

  override def parse(start: PState): Unit = {
    Assert.abort("NadaParsers are all supposed to optimize out!")
  }
}

abstract class CombinatorParser(override val context: RuntimeData)
  extends Parser with CombinatorProcessor

final class SeqCompParser(context: RuntimeData, val childParsers: Vector[Parser])
  extends CombinatorParser(context) {
  override lazy val runtimeDependencies = Vector()
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

class ChoiceParser(
  ctxt: RuntimeData,
  val childParsers: Vector[Parser],
  val unordered: Boolean = false)
  extends CombinatorParser(ctxt) {
  override lazy val runtimeDependencies = Vector()
  override lazy val childProcessors = childParsers

  override def nom = "choice"

  def parse(pstate: PState): Unit = {
    var pBefore: PState.Mark = null
    var markLeakCausedByException = false;

    try {
      pstate.pushPointOfUncertainty
      var diagnostics: Seq[Diagnostic] = Nil
      var i = 0
      var parser: Parser = null
      val limit = childParsers.length
      var returnFlag = false
      var discriminatedFailure = false
      while (!returnFlag && i < limit) {
        parser = childParsers(i)
        i += 1
        log(LogLevel.Debug, "Trying choice alternative: %s", parser)
        pBefore = pstate.mark("ChoiceParser1")
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
          val diag = new ChoiceBranchFailed(context.schemaFileLocation, pstate, pstate.diagnostics)
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
            val allDiags = new EntireChoiceFailed(context.schemaFileLocation, pstate, diagnostics.reverse)
            pstate.discard(pBefore) // because disc set, we don't unwind side effects on input stream & infoset
            pBefore = null
            pstate.setFailed(allDiags)

            // We are actually dealing with an unordered sequence, which uses ChoiceParser behind
            // the scenes. Because a discriminator was set, but the discriminated content failed,
            // we need to make sure that this discriminator information gets passed back up to the
            // unordered sequence parser and will know to fail the entire unordered sequence.
            if (unordered)
              discriminatedFailure = true

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
        val allDiags = new EntireChoiceFailed(context.schemaFileLocation, pstate, diagnostics.reverse)
        pstate.setFailed(allDiags)
        log(LogLevel.Debug, "All Choice alternatives failed.")
      }

      pstate.popPointOfUncertainty

      // This is only used for unordered sequences. If we hit a failure after parsing a discriminator,
      // we need to pass this discriminator out of the make shift ChoiceParser and back to the
      // unordered sequence parser. So, now that the inner choice discriminator is off the stack, we
      // are setting the discriminator of the outer unordered sequence loop, ensuring that while we
      // have two physical discriminators (outer for the loop, inner for the choice), this is making
      // it behave as if there is only a single discriminator
      if (discriminatedFailure)
        pstate.setDiscriminator(true)

    } catch {
      // Similar try/catch/finally logic for returning marks is also used in
      // the Sequence parser base. The logic isn't
      // easily factored out so it is duplicated. Changes made here should also
      // be made there. Only these parsers deal with taking marks, so this logic
      // should not be needed elsewhere.
      //
      // TODO: Refactor by hoisting this logic into CombinatorParser using same
      // technique as in SequenceParserBase's tryParseDetectMarkLeaks and parseOne
      // methods. That way this code really can live in exactly one place.
      //
      case t: Throwable => {
        if (pBefore != null) {
          markLeakCausedByException = true
          if (!t.isInstanceOf[SchemaDefinitionDiagnosticBase] && !t.isInstanceOf[UnsuppressableException] && !t.isInstanceOf[java.lang.Error]) {
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

case class DummyParser(override val context: TermRuntimeData)
  extends PrimParserNoData {
  override def runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()

  def parse(pstate: PState): Unit = pstate.SDE("Parser for " + context + " is not yet implemented.")

  override def childProcessors = Vector()
  override def toBriefXML(depthLimit: Int = -1) = "<dummy/>"
  override def toString = if (context == null) "Dummy[null]" else "Dummy[" + context + "]"
}

case class NotParsableParser(context: ElementRuntimeData) extends PrimParserNoData {

  def parse(state: PState): Unit = {
    // We can't use state.SDE because that needs the infoset to determine the
    // context. No infoset will exist when this is called, so we'll manually
    // create an SDE and toss it
    val rsde = new RuntimeSchemaDefinitionError(context.schemaFileLocation, state, "This schema was compiled without parse support. Check the parseUnparsePolicy tunable or dfdlx:parseUnparsePolicy property.")
    context.toss(rsde)
  }

  override lazy val childProcessors = Vector()
  override lazy val runtimeDependencies = Vector()
}
