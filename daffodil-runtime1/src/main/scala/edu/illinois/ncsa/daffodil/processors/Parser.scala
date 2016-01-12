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

import scala.xml.Node
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.util._
import Maybe._
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import edu.illinois.ncsa.daffodil.dsom.TypeConversions
import edu.illinois.ncsa.daffodil.io.DataInputStream
import java.nio.charset.CharsetDecoder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression

trait Processor
  extends ToBriefXMLImpl
  with Logging
  with Serializable {
  // things common to both unparser and parser go here.
  def context: RuntimeData
}

/**
 * This mixin for setting up all the characteristics of charset encoding
 */
trait TextParserUnparserRuntimeBase {

  final protected def setupEncoding(state: ParseOrUnparseState, erd: TermRuntimeData) {
    val encInfo = erd.encodingInfo
    val dis = state.dataStream
    dis.setMaybeUTF16Width(encInfo.optionUTF16Width)
    dis.setEncodingErrorPolicy(encInfo.defaultEncodingErrorPolicy)
  }

}

/**
 * This mixin for setting up all the characteristics of charset encoding
 */
trait TextParserRuntimeMixin extends TextParserUnparserRuntimeBase {

  /**
   * Override this in selected derived classes such as the hexBinary ones in order
   * to force use of specific encodings.
   */
  protected def decoder(state: PState, trd: TermRuntimeData) = trd.encodingInfo.getDecoder(state)

  final protected def setupDecoder(state: PState, erd: TermRuntimeData) {
    setupEncoding(state, erd)
    val dis = state.dataInputStream
    dis.setDecoder(decoder(state, erd)) // must set after the above since this will compute other settings based on those.
  }

}

trait BinaryParserUnparserRuntimeMixin {

  final protected def setupByteOrder(state: ParseOrUnparseState, trd: TermRuntimeData, byteOrdExpr: CompiledExpression) {
    val dis = state.dataStream
    val byteOrdString = byteOrdExpr.evaluate(state).asInstanceOf[String] // expressions are type-checked, so we know this will be a string
    val byteOrd = ByteOrder(byteOrdString, trd)
    dis.setByteOrder(byteOrd)
    dis.setBitOrder(trd.defaultBitOrder)
  }
}

/**
 * Encapsulates lower-level parsing with a uniform interface
 */
abstract class Parser(override val context: RuntimeData)
  extends Processor {

  protected def parserName = Misc.getNameFromClass(this)

  def PE(pstate: PState, s: String, args: Any*) = {
    pstate.setFailed(new ParseError(One(context.schemaFileLocation), One(pstate.currentLocation), s, args: _*))
  }

  def processingError(state: PState, str: String, args: Any*) =
    PE(state, str, args) // long form synonym

  protected def parse(pstate: PState): Unit

  final def parse1(pstate: PState): Unit = {
    pstate.dataProc.before(pstate, this)
    parse(pstate)
    pstate.dataProc.after(pstate, this)
  }

  // TODO: other methods for things like asking for the ending position of something
  // which would enable fixed-length formats to skip over data and not parse it at all.

}

// No-op, in case an optimization lets one of these sneak thru.
// TODO: make this fail, and test optimizer sufficiently to know these
// do NOT get through.
//class EmptyGramParser(context: RuntimeData = null) extends Parser(context) {
//  def parse(pstate: PState) = Assert.invariantFailed("EmptyGramParsers are all supposed to optimize out!")
//  override def toBriefXML(depthLimit: Int = -1) = "<empty/>"
//  override def toString = toBriefXML()
//  override def childProcessors = Nil
//
//}

//class ErrorParser(context: RuntimeData = null) extends Parser(context) {
//  def parse(pstate: PState): Unit = Assert.abort("Error Parser")
//  override def toBriefXML(depthLimit: Int = -1) = "<error/>"
//  override def toString = "Error Parser"
//  override def childProcessors = Nil
//
//}

/**
 * BriefXML is XML-style output, but intended for specific purposes. It is NOT
 * an XML serialization of the data structure. It's an XML-style string, suitable to
 * manipulate, by people, in XML tooling. E.g., can stick into an XML editor to
 * then get it all indented nicely, use a structure editor to expand/collapse subregions,
 * but it is NOT intended to capture all of the state of the object.
 */
trait ToBriefXMLImpl {

  private lazy val nom_ : String = Misc.getNameFromClass(this)
  def nom = nom_

  protected def briefXMLAttributes: String = ""

  protected def childProcessors: Seq[Processor]

  // TODO: make this create a DOM tree, not a single string (because of size limits)
  def toBriefXML(depthLimit: Int = -1): String = {
    val eltStartText = nom + (if (briefXMLAttributes == "") "" else " " + briefXMLAttributes + " ")
    if (depthLimit == 0) "..."
    else if (childProcessors.length == 0) "<" + eltStartText + "/>"
    else {
      val lessDepth = depthLimit - 1
      val sb = new StringBuilder
      childProcessors.foreach {
        cp =>
          val s = cp.toBriefXML(lessDepth)
          if (sb.size < 3000) sb.append(s) // hack!
          else sb.append("...")
      }
      "<" + eltStartText + ">" + sb + "</" + nom + ">"
    }
  }

  override def toString = toBriefXML() // pParser.toString + " ~ " + qParser.toString
}

class SeqCompParser(context: RuntimeData, val childParsers: Array[Parser])
  extends Parser(context) {

  override def childProcessors = childParsers

  override def nom = "seq"

  val numChildParsers = childParsers.size

  def parse(pstate: PState): Unit = {
    var i: Int = 0
    while (i < numChildParsers) {
      val parser = childParsers(i)

      // val beforeParse = pstate.dataInputStream.mark
      parser.parse1(pstate)
      //Assert.invariant(pstate.dataProc.handlers == handlers)
      if (pstate.status ne Success) {
        // pstate.dataInputStream.reset(beforeParse)
        return
      } else {
        // pstate.dataInputStream.discard(beforeParse)
      }

      i += 1
    }
  }

}

class AltCompParser(context: RuntimeData, val childParsers: Seq[Parser])
  extends Parser(context) {

  override lazy val childProcessors = childParsers

  override def nom = "alt"

  def parse(pstate: PState): Unit = {
    pstate.pushDiscriminator
    var pBefore = pstate.mark
    var diagnostics: Seq[Diagnostic] = Nil
    val cloneNode = pstate.captureInfosetElementState // we must undo side-effects on the JDOM if we backtrack.
    childParsers.foreach { parser =>
      {
        log(LogLevel.Debug, "Trying choice alternative: %s", parser)
        try {
          pstate.reset(pBefore)
          pBefore = pstate.mark
          parser.parse1(pstate)
        } catch {
          case s: scala.util.control.ControlThrowable => { pstate.discard(pBefore); throw s }
          case u: UnsuppressableException => { pstate.discard(pBefore); throw u }
          case rsde: RuntimeSchemaDefinitionError => { pstate.discard(pBefore); throw rsde }
          // Don't catch very general exception classes. Only very specific
          // ones. Otherwise it makes it
          // hard to debug whatever caused the exception (e.g., class casts)
          // because they're getting caught and converted into rSDEs which makes them
          // look to be DFDL Schema or parser issues when they are code problems.
        }
        if (pstate.status eq Success) {
          log(LogLevel.Debug, "Choice alternative success: %s", parser)
          pstate.popDiscriminator
          pstate.discard(pBefore)
          return
        }
        // If we get here, then we had a failure
        log(LogLevel.Debug, "Choice alternative failed: %s", parser)
        //
        //
        // Unwind any side effects on the Infoset
        //
        pstate.restoreInfosetElementState(cloneNode)
        //
        // capture diagnostics
        //
        val diag = new ParseAlternativeFailed(context.schemaFileLocation, pstate, pstate.diagnostics)
        diagnostics = diag +: diagnostics

        // check for discriminator evaluated to true.
        if (pstate.discriminator == true) {
          log(LogLevel.Debug, "Failure, but discriminator true. Additional alternatives discarded.")
          // If so, then we don't run the next alternative, we
          // consume this discriminator status result (so it doesn't ripple upward)
          // and return the failed state withall the diagnostics.
          //
          val allDiags = new AltParseFailed(context.schemaFileLocation, pstate, diagnostics.reverse)
          pstate.discard(pBefore)
          pstate.setFailed(allDiags)
          pstate.popDiscriminator
          return
        }
        //
        // Here we have a failure, but no discriminator was set, so we try the next alternative.
        // Which means we just go around the loop
      }
    }
    // Out of alternatives. All of them failed.

    pstate.reset(pBefore)
    val allDiags = new AltParseFailed(context.schemaFileLocation, pstate, diagnostics.reverse)
    pstate.setFailed(allDiags)
    log(LogLevel.Debug, "All AltParser alternatives failed.")
    pstate.popDiscriminator
  }

}

case class DummyParser(rd: RuntimeData) extends Parser(null) {
  def parse(pstate: PState): Unit = pstate.SDE("Parser for " + rd + " is not yet implemented.")

  override def childProcessors = Nil
  override def toBriefXML(depthLimit: Int = -1) = "<dummy/>"
  override def toString = if (rd == null) "Dummy[null]" else "Dummy[" + rd + "]"
}
