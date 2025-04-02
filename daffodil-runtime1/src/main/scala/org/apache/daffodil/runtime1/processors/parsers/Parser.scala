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

package org.apache.daffodil.runtime1.processors.parsers

import org.apache.daffodil.io.BacktrackingException
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.{ MaybeULong, Misc }
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.runtime1.processors.CombinatorProcessor
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.PrimProcessor
import org.apache.daffodil.runtime1.processors.PrimProcessorNoData
import org.apache.daffodil.runtime1.processors.Processor
import org.apache.daffodil.runtime1.processors.RuntimeData
import org.apache.daffodil.runtime1.processors.Success
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.TextProcessor

/**
 * Encapsulates lower-level parsing with a uniform interface
 *
 * A parser can have sub-parsers. See also PrimParser which is a parser with no sub-parsers,
 * and CombinatorParser, which is a parser with sub parsers.
 */
sealed trait Parser extends Processor {

  def isEmpty = false // override in NadaParser

  protected lazy val parserName = Misc.getNameFromClass(this)

  def PE(pstate: PState, s: String, args: Any*) = {
    pstate.setFailed(
      new ParseError(One(context.schemaFileLocation), One(pstate.currentLocation), s, args: _*)
    )
  }

  def PE(
    pstate: PState,
    sfl: SchemaFileLocation,
    dataLoc: DataLocation,
    s: String,
    args: Any*
  ) = {
    pstate.setFailed(new ParseError(One(sfl), One(dataLoc), s, args: _*))
  }

  def PENotEnoughBits(
    pstate: PState,
    sfl: SchemaFileLocation,
    dataLoc: DataLocation,
    neededBits: Long,
    source: InputSourceDataInputStream
  ): Unit = {
    val startPos = dataLoc.bitPos1b - 1
    val remainingLimitedBits = {
      if (source.bitLimit0b.isEmpty) MaybeULong.Nope
      else {
        val lim = source.bitLimit0b.get
        Assert.invariant(lim >= 0)
        val nBits = lim - startPos
        MaybeULong(nBits)
      }
    }
    val remainingBits = {
      if (source.hasReachedEndOfData) {
        val bitsAvailable = {
          val fragmentBitsReadFromSourcePos = source.bitPos0b % 8
          val bitsAvailableFromSourcePos =
            source.knownBytesAvailable * 8 - fragmentBitsReadFromSourcePos
          val bitsBetweenStartPosAndSourcePos = source.bitPos0b - startPos
          val bitsAvailableFromStartPos =
            bitsAvailableFromSourcePos + bitsBetweenStartPosAndSourcePos
          bitsAvailableFromStartPos
        }
        if (remainingLimitedBits.isEmpty || bitsAvailable < remainingLimitedBits.get)
          bitsAvailable
        else
          remainingLimitedBits.get
      } else remainingLimitedBits.get
    }

    PE(
      pstate,
      sfl,
      dataLoc,
      "Insufficient bits in data. Needed %d bit(s) but found only %d available",
      neededBits,
      remainingBits
    )
  }

  def PENotEnoughBits(
    pstate: PState,
    sfl: SchemaFileLocation,
    dataLoc: DataLocation,
    neededBits: Long
  ): Unit = {
    PE(
      pstate,
      sfl,
      dataLoc,
      "Insufficient bits in data. Needed %d bit(s)",
      neededBits
    )
  }

  def PENotEnoughBits(
    pstate: PState,
    neededBits: Long,
    source: InputSourceDataInputStream
  ): Unit = {
    PENotEnoughBits(
      pstate,
      pstate.schemaFileLocation,
      pstate.currentLocation,
      neededBits,
      source
    )
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
      case be: BacktrackingException =>
        pstate.SDE("Attempted to backtrack too far: " + be.getMessage())
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
trait PrimParser extends PrimProcessor with Parser

/**
 * A parser which is "primitive" no sub-parsers, but which doesn't
 * parses any data. Evaluates expressions, binds variables, evaluates
 * asserts, etc.
 */
trait PrimParserNoData extends PrimProcessorNoData with Parser

/**
 * Mixed in as a marker for parsers that only operate on text.
 */
trait TextPrimParser extends PrimParser with TextProcessor

/**
 * Parser which does "Nada" (Nothing in Spanish)
 *
 * Used for optionality in some cases, but is usually recognized and
 * optimized out.
 */
final class NadaParser(override val context: RuntimeData) extends PrimParserNoData {
  override def runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()

  override def isEmpty = true

  override def toString = "Nada"

  override def parse(start: PState): Unit = {
    Assert.abort("NadaParsers are all supposed to optimize out!")
  }
}

abstract class CombinatorParser(override val context: RuntimeData)
  extends Parser
  with CombinatorProcessor

final class SeqCompParser(context: RuntimeData, val childParsers: Array[Parser])
  extends CombinatorParser(context) {
  override def runtimeDependencies = Vector()
  override def childProcessors = childParsers.toVector

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

class ChoiceParser(ctxt: RuntimeData, val childParsers: Array[Parser])
  extends CombinatorParser(ctxt) {
  override def runtimeDependencies = Vector()
  override def childProcessors = childParsers.toVector

  override def nom = "choice"

  def parse(pstate: PState): Unit = {

    var diagnostics: Seq[Diagnostic] = Nil
    var i = 0
    val numAlternatives = childParsers.length

    var successfullyParsedChildBranch = false

    while (!successfullyParsedChildBranch && i < numAlternatives) {

      val parser = childParsers(i)
      i += 1

      pstate.withPointOfUncertainty("ChoiceParser", ctxt) { pou =>
        parser.parse1(pstate)

        if (pstate.processorStatus eq Success) {
          // Choice branch was successfull. Break out of the loop and let
          // withPointOfUncertainty discard the pou
          successfullyParsedChildBranch = true

          // We usually rely on the sequence parser to set elements as final.
          // But choices with scalar elements do not necessarily have a
          // sequence surrounding them and so they aren't set final. In order
          // to set these elements final, we do it here as well. We will
          // attempt to walk the infoset after the PoU is discarded.
          val newLastChildNode = pstate.infoset.maybeLastChild
          if (newLastChildNode.isDefined) {
            newLastChildNode.get.isFinal = true
          }

        } else {
          // Failed to parse this branch alternative. Create diagnostic and
          // check if anything resolved the associated point of uncertainty

          val diag =
            new ChoiceBranchFailed(context.schemaFileLocation, pstate, pstate.diagnostics)
          diagnostics = diag +: diagnostics

          if (pstate.isPointOfUncertaintyResolved(pou)) {
            // A discriminator resolved the point of uncertainty associated with
            // this choice. Since this branch failed, we will not attempt any
            // more alternatives. We set i to the numAlternatives to break out of
            // this loop. Once we break out, a PE will be created.
            i = numAlternatives
          } else {
            // We have a failure, but nothing resolved the point of uncertainty
            // associated with this choice. Reset back to the point of
            // uncertainty to unwind any side-effects from the branch and
            // attempt the next alternative.
            pstate.resetToPointOfUncertainty(pou)
          }
        }
      }

      pstate.walker.walk()
    }

    if (!successfullyParsedChildBranch) {
      val allDiags =
        new EntireChoiceFailed(context.schemaFileLocation, pstate, diagnostics.reverse)
      pstate.setFailed(allDiags)
    }
  }

}

case class DummyParser(override val context: TermRuntimeData) extends PrimParserNoData {
  override def runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()

  def parse(pstate: PState): Unit =
    pstate.SDE("Parser for " + context + " is not yet implemented.")

  override def childProcessors = Vector()
  override def toBriefXML(depthLimit: Int = -1) = "<dummy/>"
  override def toString = if (context == null) "Dummy[null]" else "Dummy[" + context + "]"
}

case class NotParsableParser(context: ElementRuntimeData) extends PrimParserNoData {

  def parse(state: PState): Unit = {
    // We can't use state.SDE because that needs the infoset to determine the
    // context. No infoset will exist when this is called, so we'll manually
    // create an SDE and toss it
    val rsde = new RuntimeSchemaDefinitionError(
      context.schemaFileLocation,
      "This schema was compiled without parse support. Check the parseUnparsePolicy tunable or dfdlx:parseUnparsePolicy property."
    )
    context.toss(rsde)
  }

  override def childProcessors = Vector()
  override def runtimeDependencies = Vector()
}
