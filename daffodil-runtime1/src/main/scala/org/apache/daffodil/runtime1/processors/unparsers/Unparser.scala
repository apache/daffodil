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

package org.apache.daffodil.runtime1.processors.unparsers

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions.Assert;
object INoWarn { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.runtime1.processors._

sealed trait Unparser extends Processor {

  def isEmpty = false

  def context: RuntimeData

  protected def unparse(ustate: UState): Unit

  final def unparse1(ustate: UState, ignore: AnyRef = null) = {
    Assert.invariant(isInitialized)
    val savedProc = ustate.maybeProcessor
    ustate.setProcessor(this)

    // ?? TODO: Should this be after the split below ??
    if (ustate.dataProc.isDefined) ustate.dataProc.get.before(ustate, this)

    // TODO: Remove this call to ustate.bitOrder below.
    // Figure out where this is needed elsewhere in unparser code.
    //
    // Clearly calling this here is overkilling the problem.
    //
    // In theory some places in the unparser code dealing with splitting/suspending
    // or outputValueCalc elements are missing proper checking of bitOrder, or
    // keeping track of prior bit order. Finding those has been problematic.
    //
    // So this is a temporary fix, until we can figure out where else to do this.
    //
    this match {
      // bit order only applies to primitives, not combinators, nor "noData" unparsers.
      case af: AlignmentPrimUnparser => // ok. Don't check bitOrder before Aligning.
      case u: PrimUnparser => {
        u.context match {
          case trd: TermRuntimeData => {
            ustate.bitOrder // asking for bitOrder checks bit order changes.
            // this splits DOS on bitOrder changes if absoluteBitPos not known
          }
          case rd: RuntimeData =>
            Assert.invariantFailed(
              "Primitive unparser " + u + " has non-Term runtime data: " + rd
            )
        }
      }
      case _ => // ok
    }
    try {
      unparse(ustate)
    } finally {
      ustate.resetFormatInfoCaches()
    }
    if (ustate.dataProc.isDefined) ustate.dataProc.get.after(ustate, this)
    ustate.setMaybeProcessor(savedProc)
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
trait PrimUnparser extends Unparser with PrimProcessor

/**
 * A marker trait for the unparsers that perform alignment.
 *
 * Needed to distinguish alignment operations from regular primitives so that
 * we can inspect for bitOrder changes on most primitives, but not
 * alignments - since their purpose may be to align so that the bitOrder change
 * is on the right boundary. Checking bit order before them defeats the purpose
 * of alignment.
 */
trait AlignmentPrimUnparser extends PrimUnparser

/**
 * An unparser that is primitive (no sub-unparsers), but doesn't write anything
 * to a data stream (buffered or real), so alignment, bitOrder, etc. cannot
 * apply to it.
 */
trait PrimUnparserNoData extends Unparser with PrimProcessorNoData

/**
 * Text primitive unparsers - primitive and textual only.
 */
trait TextPrimUnparser extends PrimUnparser with TextProcessor

abstract class CombinatorUnparser(override val context: RuntimeData)
  extends Unparser
  with CombinatorProcessor

trait SuspendableUnparser extends PrimUnparser {

  protected def suspendableOperation: SuspendableOperation

  override final def unparse(state: UState): Unit = {
    state.bitOrder // force checking of bitOrder changes on non-byte boundaries
    // also forces capture of bitOrder value in case of suspension.
    suspendableOperation.run(state)
  }
}

final class ErrorUnparser(override val context: TermRuntimeData = null)
  extends PrimUnparserNoData {

  override def runtimeDependencies = Vector()

  def unparse(ustate: UState): Unit = {
    Assert.abort("Error Unparser")
  }
  override def childProcessors = Vector()

  override def toBriefXML(depthLimit: Int = -1) = "<error/>"
  override def toString = "Error Unparser"
}

final class SeqCompUnparser(context: RuntimeData, val childUnparsers: Array[Unparser])
  extends CombinatorUnparser(context)
  with ToBriefXMLImpl {

  override def runtimeDependencies = Vector()

  override def childProcessors = childUnparsers.toVector

  override def nom = "seq"

  def unparse(ustate: UState): Unit = {
    var i = 0
    while (i < childUnparsers.length) {
      val unparser = childUnparsers(i)
      i += 1
      unparser.unparse1(ustate)
    }
  }

  override def toString: String = {
    val strings = childUnparsers.map { _.toString }
    strings.mkString(" ~ ")
  }
}

case class DummyUnparser(primitiveName: String) extends PrimUnparserNoData {

  override def context = null

  override def runtimeDependencies = Vector()

  override def isEmpty = true

  def unparse(state: UState): Unit =
    state.SDE("Unparser (%s) is not yet implemented.", primitiveName)

  override def childProcessors = Vector()
  override def toBriefXML(depthLimit: Int = -1) =
    "<dummy primitive=\"%s\"/>".format(primitiveName)
  override def toString = "DummyUnparser (%s)".format(primitiveName)
}

case class NotUnparsableUnparser(override val context: ElementRuntimeData)
  extends PrimUnparserNoData {

  def unparse(state: UState): Unit = {
    // We can't use state.SDE because that needs the infoset to determine the
    // context. No infoset will exist when this is called, so we'll manually
    // create an SDE and toss it
    val rsde = new RuntimeSchemaDefinitionError(
      context.schemaFileLocation,
      "This schema was compiled without unparse support. Check the parseUnparsePolicy tunable or dfdlx:parseUnparsePolicy property."
    )
    context.toss(rsde)
  }

  override def childProcessors = Vector()
  override def runtimeDependencies = Vector()
}
