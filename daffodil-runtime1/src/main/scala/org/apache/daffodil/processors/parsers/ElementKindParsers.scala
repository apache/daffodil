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

import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.processors.ChoiceDispatchKeyEv
import org.apache.daffodil.processors.DelimiterParseEv
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.EscapeSchemeParseEv
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.Failure
import java.io.StringWriter
import org.apache.daffodil.exceptions.UnsuppressableException
import java.io.PrintWriter
import org.apache.daffodil.dsom.SchemaDefinitionDiagnosticBase

class ComplexTypeParser(rd: RuntimeData, bodyParser: Parser)
  extends CombinatorParser(rd) {
  override def nom = "ComplexType"

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {
    start.mpstate.childIndexStack.push(1L) // one-based indexing
    bodyParser.parse1(start)
    start.mpstate.childIndexStack.pop()
    ()
  }
}

/**
 * The purpose of this parser is to create/evaluate delimiter DFAs
 * and push them to the delimiter stack (bring them in scope) for
 * subsequent (internal/body) parse steps.  Then on the way out pop
 * the delimiter DFAs (bring them out of scope) after
 * the internal/body parser has completed.
 */
class DelimiterStackParser(delimiters: Array[DelimiterParseEv],
  ctxt: RuntimeData, bodyParser: Parser)
  extends CombinatorParser(ctxt) {

  override lazy val childProcessors = List(bodyParser)

  override lazy val runtimeDependencies = delimiters.toSeq

  def parse(start: PState): Unit = {

    val newLocalIndex = start.mpstate.delimiters.length
    start.mpstate.delimitersLocalIndexStack.push(newLocalIndex)

    // evaluate and add delimiters to the stack
    var i: Int = 0
    while (i < delimiters.length) {
      start.mpstate.delimiters ++= delimiters(i).evaluate(start)
      i += 1
    }

    // set the index of the newly added delimiters
    val newDelimLen = start.mpstate.delimiters.length
    i = newLocalIndex
    while (i < newDelimLen) {
      start.mpstate.delimiters(i).indexInDelimiterStack = i
      i += 1
    }

    // parse
    bodyParser.parse1(start)

    // pop delimiters
    start.mpstate.delimiters.reduceToSize(start.mpstate.delimitersLocalIndexStack.pop)
  }
}

/**
 * *
 * This parser should only ever be called when a dynamic escape scheme exists
 * so the escape scheme is evaluated in the right scope. If a constant
 * escape scheme exists, the Evaluatable should store the constant and this
 * should never be called.
 *
 * Note that the escape scheme evaluatable (and its dependencies) are manually
 * cached, so upon exiting scope the cache must be invalidated.
 */
class DynamicEscapeSchemeParser(escapeScheme: EscapeSchemeParseEv,
  ctxt: TermRuntimeData, bodyParser: Parser)
  extends CombinatorParser(ctxt) {

  override lazy val childProcessors = Seq(bodyParser)

  override lazy val runtimeDependencies = List(escapeScheme)

  def parse(start: PState): Unit = {
    // evaluate the dynamic escape scheme in the correct scope. the resulting
    // value is cached in the Evaluatable (since it is manually cached) and
    // future parsers that use this escape scheme will use that cached value.
    escapeScheme.newCache(start)
    escapeScheme.evaluate(start)

    // Parse
    bodyParser.parse1(start)

    // invalidate the escape scheme cache
    escapeScheme.invalidateCache(start)
  }
}

sealed abstract class OrderedSequenceParserBase(rd: TermRuntimeData, childParsers: Vector[Parser])
  extends CombinatorParser(rd) {
  override def nom = "Sequence"

  override lazy val runtimeDependencies: Seq[Evaluatable[AnyRef]] = Nil

  override lazy val childProcessors = childParsers.toSeq

  def createDiagnostic(start: PState) = {}
}

class OrderedUnseparatedSequenceParser(rd: TermRuntimeData, childParsers: Vector[Parser])
  extends OrderedSequenceParserBase(rd, childParsers) {

  def parse(start: PState): Unit = {
    var i = 0
    start.mpstate.groupIndexStack.push(1L) // one-based indexing

    while ((i < childParsers.size) && (start.processorStatus eq Success)) {
      val parser = childParsers(i)

      parser.parse1(start)

      i += 1
    }

    start.mpstate.groupIndexStack.pop()
    start.mpstate.moveOverOneGroupIndexOnly()
    ()
  }
}

class OrderedSeparatedSequenceParser(rd: TermRuntimeData,
  ssp: SeparatorSuppressionPolicy,
  spos: SeparatorPosition,
  sep: Parser,
  childPairs: Vector[(TermRuntimeData, Parser)])
  extends OrderedSequenceParserBase(rd, sep +: childPairs.map { _._2 }) {

  def parse(initialState: PState): Unit = {
    var index = 0
    initialState.mpstate.groupIndexStack.push(1L) // one-based indexing

    val pstate = initialState

    var prefixDone = false
    var atLeastOneParsed = false
    var priorState: PState.Mark = null
    var markLeakCausedByException = false

    val limit = childPairs.size

    var wasLastChildZeroLength = false

    while ((index < limit) && (pstate.processorStatus eq Success)) {
      val pair = childPairs(index)
      val trd = pair._1
      val parser = pair._2
      val prevBitPos = pstate.bitPos0b

      try {
        priorState = pstate.mark("OrderedSeparatedSequence_beforeSeparator")

        // parse prefix sep if any
        if ((spos eq SeparatorPosition.Prefix) && !prefixDone && trd.isRepresented) {
          sep.parse1(pstate)
          prefixDone = true
        }

        // except for the first position of the group, parse an infix separator
        // but only for required scalars. Arrays and optionals take care of their
        // own infix separator.

        if ((spos eq SeparatorPosition.Infix) && index != 0 && trd.isRequiredScalar && trd.isRepresented) {
          sep.parse1(pstate)
        }

        val sepSuccessful = pstate.processorStatus eq Success // true on succcess, and true if we didn't parse a separator

        // if that was successful, then parse child term.
        // If the child is an array, it takes care of its own infix separators

        if (sepSuccessful) {
          val childIndexBefore = pstate.childPos
          parser.parse1(pstate)
          val childIndexAfter = pstate.childPos

          val childSuccessful = pstate.processorStatus eq Success

          // This tells us if it was array or optional and 1 or more occurrences
          // were found. If so then it's a true successful parse and we'll keep
          // the parse of the separator we just did above.
          //
          // If no occurrences at all, then we'll backtrack the separator we parsed
          // above.
          val isAtLeast1Occurence = trd.isRequiredScalar || (childIndexAfter > childIndexBefore)

          if (childSuccessful && isAtLeast1Occurence) {
            pstate.discard(priorState) // we're keeping the separator. Don't need priorState any more.
            priorState = null
            if (trd.isRepresented) {
              atLeastOneParsed = true // flag to tell us we'll need a postix separator if postfix mode

              //
              // trailingEmptyStrict - we keep track of the last child, if it was zero length
              //
              if ((ssp eq SeparatorSuppressionPolicy.TrailingEmptyStrict)) {
                wasLastChildZeroLength = (prevBitPos == pstate.bitPos0b)
              }
            }

          } else {
            //
            // Failed to parse the child, due to real failure, or
            // Success, but where an array/optional found zero occurrences.
            //
            // However, for a scalar required element, we parsed a separator above.
            // (except for first in group when we only parsed one if a prefix sep is needed.
            // but that exception won't affect the logic here.)
            // Subsequently the parse of the term after the separator failed
            //
            // If the term was an array or optional element, then it will
            // have backed out its own separator parse.
            // But for a scalar required element, we have to back out the
            // separator here.
            //
            // It doesn't hurt to just always back out to the state before the
            // separator above.
            //
            pstate.processorStatus match {
              case Success => {
                Assert.invariant(!trd.isRequiredScalar)
                pstate.reset(priorState)
                priorState = null
              }
              case f: Failure => {
                val cause = f.cause
                pstate.reset(priorState)
                priorState = null
                PE(pstate, "Failed to parse sequence child. Cause: %s.", cause)
              }
            }
          } // end success/fail of child parse

          // so parse postfix separator if in postfix mode

          if ((pstate.processorStatus eq Success) &&
            (spos eq SeparatorPosition.Postfix) &&
            atLeastOneParsed) {
            sep.parse1(pstate)
          }
        } else {
          // failed to parse infix separator for required scalar
          val cause = pstate.processorStatus.asInstanceOf[Failure].cause
          pstate.reset(priorState)
          priorState = null
          PE(pstate, "Failed to parse separator. Cause: %s.", cause)
        } // end success/fail of separator-before-child parse
        index += 1
      } catch {
        // Similar try/catch/finally logic for returning marks is also used in
        // the AltCompParser and RepUnboundedParser. The logic isn't
        // easily factored out so it is duplicated. Changes made here should also
        // be made there. Only these parsers deal with taking marks, so this logic
        // should not be needed elsewhere.
        case t: Throwable => {
          if (priorState != null) {
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
        if (priorState != null) {
          initialState.discard(priorState)
          markLeak = true;
        }
        if (markLeak && !markLeakCausedByException) {
          // likely a logic bug, throw assertion
          Assert.invariantFailed("mark not returned, likely a logic bug")
        }
      } // end try/catch/finally
    } // end while

    Assert.invariant(priorState eq null)

    if ((pstate.processorStatus eq Success) && wasLastChildZeroLength) {
      Assert.invariant(ssp eq SeparatorSuppressionPolicy.TrailingEmptyStrict)
      PE(pstate, "Empty trailing elements are not allowed when dfdl:separatorSuppressionPolicy='trailingEmptyStrict'")
    }
    pstate.mpstate.groupIndexStack.pop()
    pstate.mpstate.moveOverOneGroupIndexOnly()
    ()
  }
}

/**
 * This is essentially just a wrapper around the bodyParser, which is an
 * AltCompParser. This is only here to maintain symmetry with the unparse side,
 * which has a more complicated unparser that differs from an AltCompUnparser.
 */
class ChoiceCombinatorParser(rd: TermRuntimeData, bodyParser: Parser)
  extends CombinatorParser(rd) {
  override def nom = "Choice"

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(bodyParser)

  def parse(start: PState): Unit = {
    bodyParser.parse1(start)
  }
}

class ChoiceDispatchCombinatorParser(rd: TermRuntimeData, dispatchKeyEv: ChoiceDispatchKeyEv, dispatchBranchKeyMap: Map[String, Parser])
  extends CombinatorParser(rd) {
  override def nom = "ChoiceDispatch"

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = dispatchBranchKeyMap.values.toSeq

  def parse(pstate: PState): Unit = {
    val key = dispatchKeyEv.evaluate(pstate)

    val parserOpt = dispatchBranchKeyMap.get(key)
    if (parserOpt.isEmpty) {
      val diag = new ChoiceDispatchNoMatch(context.schemaFileLocation, pstate, key)
      pstate.setFailed(diag)
    } else {
      val parser = parserOpt.get

      // Note that we are intentionally not pushing/popping a new
      // discriminator here, as is done in the ChoiceCombinatorParser and
      // AltCompParser. This has the effect that if a branch of this direct
      // dispatch choice specifies a discriminator, then it will discriminate a
      // point of uncertainty outside of the choice. If we pushed a new
      // discriminator here if would essentially ignore discriminators on a
      // choice branch.

      log(LogLevel.Debug, "Dispatching to choice alternative: %s", parser)
      parser.parse1(pstate)

      if (pstate.processorStatus eq Success) {
        log(LogLevel.Debug, "Choice dispatch success: %s", parser)
      } else {
        log(LogLevel.Debug, "Choice dispatch failed: %s", parser)
        val diag = new ChoiceDispatchFailed(context.schemaFileLocation, pstate, pstate.diagnostics)
        pstate.setFailed(diag)
      }
    }
  }
}

class ArrayCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser)
  extends CombinatorParser(erd) {
  override def nom = "Array"
  override lazy val childProcessors = Seq(bodyParser)

  override lazy val runtimeDependencies = Nil

  def parse(start: PState): Unit = {

    start.mpstate.arrayIndexStack.push(1L) // one-based indexing
    start.mpstate.occursBoundsStack.push(start.tunable.maxOccursBounds)

    bodyParser.parse1(start)

    val actualOccurs = start.mpstate.arrayIndexStack.pop()
    start.mpstate.occursBoundsStack.pop()

    if (start.processorStatus ne Success) return

    val shouldValidate =
      start.dataProc.isDefined && start.dataProc.value.getValidationMode != ValidationMode.Off

    if (shouldValidate && erd.minOccurs.isDefined && erd.maxOccurs.isDefined) {
      val minO = erd.minOccurs.get
      val maxO = erd.maxOccurs.get
      val isUnbounded = maxO == -1
      val occurrence = actualOccurs - 1

      if (isUnbounded && occurrence < minO)
        start.validationError("%s occurred '%s' times when it was expected to be a " +
          "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.diagnosticDebugName,
          occurrence, minO)
      else if (!isUnbounded && (occurrence < minO || occurrence > maxO))
        start.validationError("%s occurred '%s' times when it was expected to be a " +
          "minimum of '%s' and a maximum of '%s' times.", erd.diagnosticDebugName,
          occurrence, minO, maxO)
      else {
        //ok
      }
    }
  }
}

// This follows the same behavior as Arrays for parsing
class OptionalCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser) extends ArrayCombinatorParser(erd, bodyParser) {
  override def nom = "Optional"
}
