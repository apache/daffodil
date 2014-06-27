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

/**
 * Encapsulates lower-level parsing with a uniform interface
 */
abstract class Parser(val context: SchemaComponent) extends Logging with ToBriefXMLImpl {

  def PE(pstate: PState, s: String, args: Any*) = {
    pstate.failed(new ParseError(context, Some(pstate), s, args: _*))
  }

  def processingError(state: PState, str: String, args: Any*) =
    PE(state, str, args) // long form synonym

  protected def parse(pstate: PState): PState

  final def parse1(pstate: PState, context: SchemaComponent): PState = {
    Debugger.before(pstate, this)
    // 
    // Since the pstate is being overwritten (in most case) now,
    // we must explicitly make a copy so we can compute a delta
    // after
    //
    val beforeState = if (Debugger.getDebugging()) pstate.duplicate() else pstate
    val afterState = parse(pstate)
    if (!(afterState eq pstate)) {
      // they're not the same state object
      // we make them so.
      pstate.assignFrom(afterState)
      pstate.inStream.assignFrom(afterState.inStream)
    }
    Debugger.after(beforeState, afterState, this)
    // afterState
    pstate
  }

  // TODO: other methods for things like asking for the ending position of something
  // which would enable fixed-length formats to skip over data and not parse it at all.

}

// No-op, in case an optimization lets one of these sneak thru. 
// TODO: make this fail, and test optimizer sufficiently to know these 
// do NOT get through.
class EmptyGramParser(context: Term = null) extends Parser(context) {
  def parse(pstate: PState) = Assert.invariantFailed("EmptyGramParsers are all supposed to optimize out!")
  override def toBriefXML(depthLimit: Int = -1) = "<empty/>"
  override def toString = toBriefXML()
}

class ErrorParser(context: Term = null) extends Parser(context) {
  def parse(pstate: PState): PState = Assert.abort("Error Parser")
  override def toBriefXML(depthLimit: Int = -1) = "<error/>"
  override def toString = "Error Parser"
}

/**
 * BriefXML is XML-style output, but intended for specific purposes. It is NOT
 * an XML serialization of the data structure. It's an XML-style string, suitable to
 * manipulate, by people, in XML tooling. E.g., can stick into an XML editor to
 * then get it all indented nicely, use a structure editor to expand/collapse subregions,
 * but it is NOT intended to capture all of the state of the object.
 */
trait ToBriefXMLImpl {

  val nom: String = Misc.getNameFromClass(this)

  val childParsers: Seq[Parser] = Nil

  // TODO: make this do indenting and newlines (maybe optionally?)
  def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else if (depthLimit == 1 && childParsers.length == 0) "<" + nom + "/>"
    else {
      val lessDepth = depthLimit - 1
      "<" + nom + ">" + childParsers.map { _.toBriefXML(lessDepth) }.mkString + "</" + nom + ">"
    }
  }

  override def toString = toBriefXML() // pParser.toString + " ~ " + qParser.toString
}

class SeqCompParser(context: AnnotatedSchemaComponent, children: Seq[Gram])
  extends Parser(context)
  with ToBriefXMLImpl {
  Assert.invariant(!children.exists { _.isEmpty })

  override val nom = "seq"

  override val childParsers = children.map { _.parser }

  def parse(pstate: PState): PState = {
    var pResult = pstate
    childParsers.foreach { parser =>
      {
        pResult = parser.parse1(pResult, context)
        if (pResult.status != Success) {
          // failed in a sequence
          return pResult
        }
        pResult = pResult
      }
    }
    pResult
  }

}

class AltCompParser(context: AnnotatedSchemaComponent, children: Seq[Gram])
  extends Parser(context)
  with ToBriefXMLImpl {
  Assert.invariant(!children.exists { _.isEmpty })

  override val nom = "alt"

  override val childParsers = children.map { _.parser }

  def parse(pInitial: PState): PState = {
    val pStart = pInitial.withNewPointOfUncertainty
    var pResult: PState = null
    var diagnostics: Seq[Diagnostic] = Nil
    val cloneNode = pStart.captureInfosetElementState // we must undo side-effects on the JDOM if we backtrack.
    childParsers.foreach { parser =>
      {
        log(LogLevel.Debug, "Trying choice alternative: %s", parser)
        try {
          val ps = pStart.duplicate()
          pResult = parser.parse1(ps, context)
        } catch {
          case u: UnsuppressableException => throw u
          case rsde: RuntimeSchemaDefinitionError => throw rsde
          case e: Exception => Assert.invariantFailed("Runtime parsers should not throw exceptions: " + e)
        }
        if (pResult.status == Success) {
          log(LogLevel.Debug, "Choice alternative success: %s", parser)
          val res = pResult.withRestoredPointOfUncertainty
          return res
        }
        // If we get here, then we had a failure
        log(LogLevel.Debug, "Choice alternative failed: %s", parser)
        // Unwind any side effects on the Infoset 
        // The infoset is the primary non-functional data structure. We have to un-side-effect it.
        pStart.restoreInfosetElementState(cloneNode)
        val diag = new ParseAlternativeFailed(context, pStart, pResult.diagnostics)
        diagnostics = diag +: diagnostics
        // check for discriminator evaluated to true.
        if (pResult.discriminator == true) {
          log(LogLevel.Debug, "Failure, but discriminator true. Additional alternatives discarded.")
          // If so, then we don't run the next alternative, we
          // consume this discriminator status result (so it doesn't ripple upward)
          // and return the failed state withall the diagnostics.
          //
          val allDiags = new AltParseFailed(context, pResult, diagnostics.reverse)
          val res = pResult.failed(allDiags).withRestoredPointOfUncertainty
          return res
        }
        //
        // Here we have a failure, but no discriminator was set, so we try the next alternative.
        // Which means we just go around the loop
      }
    }
    // Out of alternatives. All of them failed. 
    val allDiags = new AltParseFailed(context, pStart, diagnostics.reverse)
    val allFailedResult = pStart.failed(allDiags)
    log(LogLevel.Debug, "All AltParser alternatives failed.")
    val result = allFailedResult.withRestoredPointOfUncertainty
    result
  }

}

case class DummyParser(sc: SchemaComponent) extends Parser(null) {
  def parse(pstate: PState): PState = sc.SDE("Parser for " + sc + " is not yet implemented.")

  override def toBriefXML(depthLimit: Int = -1) = "<dummy/>"
  override def toString = if (sc == null) "Dummy[null]" else "Dummy[" + sc + "]"
}

