/* Copyright (c) 2012-2017 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.processors.unparsers
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.Implicits._; object INoWarn { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.processors._
import org.apache.daffodil.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.processors.parsers.TextParserUnparserRuntimeBase
import org.apache.daffodil.processors.PrimProcessor
import org.apache.daffodil.processors.ToBriefXMLImpl
import org.apache.daffodil.processors.Processor

/**
 * This mixin for setting up all the characteristics of charset encoding
 */
trait TextUnparserRuntimeMixin extends TextParserUnparserRuntimeBase {

  /**
   * Override this in selected derived classes such as the hexBinary ones in order
   * to force use of specific encodings.
   */
  protected def encoder(state: UState, trd: TermRuntimeData) = trd.encodingInfo.getEncoder(state)

  // TODO: This is the wrong way to do these sorts of setting changes, as this incurs overhead
  // for every Term instance parsed/unparsed.
  //
  // The right way to fix this is to have  DataOutputStreams indirect reference
  // this from an Evaluatable so that it's cached on the node (if a runtime expression)
  // or taken from the TermRuntimeData if it's static.
  //
  // The cache on the node is really only needed for elements with dfdl:outputValueCalc
  // that forward reference to other things in the future. In those cases
  // we do need to "freeze" the encoding so that this encoding is used for
  // the element once the outputValueCalc has completed evaluation and can
  // actually be unparsed.
  //
  final protected def setupEncoder(state: UState, trd: TermRuntimeData) {
    val dis = state.dataOutputStream
    setupEncoding(state, trd)
    dis.setEncodingErrorPolicy(trd.encodingInfo.defaultEncodingErrorPolicy)
    dis.setEncoder(encoder(state, trd)) // must set after the above since this will compute other settings based on those.
  }

}

/**
 * Vast majority of unparsers are actually Term unparsers. (The ones that are not are expression related e.g., SetVar)
 */
abstract class TermUnparser(val termRuntimeData: TermRuntimeData) extends UnparserObject(termRuntimeData)

trait Unparser
  extends Processor {

  def isEmpty = false

  def context: RuntimeData

  def unparse(ustate: UState): Unit

  final def unparse1(ustate: UState, context: RuntimeData): Unit = {
    Assert.invariant(isInitialized)
    if (ustate.dataProc.isDefined) ustate.dataProc.get.before(ustate, this)
    //
    // Since the state is being overwritten (in most case) now,
    // we must explicitly make a copy so we can compute a delta
    // after
    //
    unparse(ustate)

    if (ustate.dataProc.isDefined) ustate.dataProc.get.after(ustate, this)
  }

  def UE(ustate: UState, s: String, args: Any*) = {
    UnparseError(One(context.schemaFileLocation), One(ustate.currentLocation), s, args: _*)
  }

}

/**
 * PrimUnparsers are for simple types. They support the ability to pre-evaluate any
 * computations that depend on runtime-valued properties. This is needed to support
 * the DFDL outputValueCalc property when the calculation forward references into the infoset.
 *
 * In that case, one must evaluate all the runtime-valued properties, save them for later use,
 * then suspend unparsing of the outputValueCalc'ed element. Later when the value has been
 * computed, one then unparses the item, and the right values are present for runtime-evaluated
 * things, as they've been cached (on the Infoset Element)
 */
trait PrimUnparser
  extends Unparser
  with PrimProcessor

// Deprecated and to be phased out. Use the trait Unparser instead.
abstract class UnparserObject(override val context: RuntimeData)
  extends Unparser {

  override lazy val runtimeDependencies: Seq[Evaluatable[AnyRef]] = Nil

}

// Deprecated and to be phased out. Use the trait PrimUnparser instead.
abstract class PrimUnparserObject(override val context: RuntimeData)
  extends PrimUnparser {
  override def runtimeDependencies: Seq[Evaluatable[AnyRef]] = Nil
}

// No-op, in case an optimization lets one of these sneak thru.
// TODO: make this fail, and test optimizer sufficiently to know these
// do NOT get through.
class EmptyGramUnparser(context: RuntimeData = null) extends UnparserObject(context) {

  def unparse(ustate: UState) {
    Assert.invariantFailed("EmptyGramUnparsers are all supposed to optimize out!")
  }

  override def isEmpty = true

  override lazy val childProcessors = Nil

  override def toBriefXML(depthLimit: Int = -1) = "<empty/>"
  override def toString = toBriefXML()
}

class ErrorUnparser(context: RuntimeData = null) extends UnparserObject(context) {
  def unparse(ustate: UState) {
    Assert.abort("Error Unparser")
  }
  override lazy val childProcessors = Nil

  override def toBriefXML(depthLimit: Int = -1) = "<error/>"
  override def toString = "Error Unparser"
}

class SeqCompUnparser(context: RuntimeData, val childUnparsers: Array[Unparser])
  extends UnparserObject(context)
  with ToBriefXMLImpl {

  override val childProcessors = childUnparsers.toSeq

  override def nom = "seq"

  def unparse(ustate: UState): Unit = {
    var i = 0
    while (i < childUnparsers.length) {
      val unparser = childUnparsers(i)
      i += 1
      unparser.unparse1(ustate, context)
    }
  }

  override def toString: String = {
    val strings = childUnparsers map { _.toString }
    strings.mkString(" ~ ")
  }
}

case class DummyUnparser(primitiveName: String) extends UnparserObject(null) {

  override def isEmpty = true

  def unparse(state: UState): Unit = state.SDE("Unparser (%s) is not yet implemented.", primitiveName)

  override lazy val childProcessors = Nil
  override def toBriefXML(depthLimit: Int = -1) = "<dummy primitive=\"%s\"/>".format(primitiveName)
  override def toString = "DummyUnparser (%s)".format(primitiveName)
}

case class NotUnparsableUnparser(context: ElementRuntimeData) extends Unparser {

  def unparse(state: UState): Unit = {
    // We can't use state.SDE because that needs the infoset to determine the
    // context. No infoset will exist when this is called, so we'll manually
    // create an SDE and toss it
    val rsde = new RuntimeSchemaDefinitionError(context.schemaFileLocation, state, "This schema was compiled without unparse support. Check the parseUnparsePolicy tunable or daf:parseUnparsePolicy property.")
    context.toss(rsde)
  }

  override lazy val childProcessors = Nil
  override lazy val runtimeDependencies = Nil
}
