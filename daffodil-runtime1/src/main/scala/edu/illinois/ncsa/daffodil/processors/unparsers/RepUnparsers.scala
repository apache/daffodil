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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.OccursCountKind

abstract class RepUnparser(n: Long, rUnparser: Unparser, context: ElementRuntimeData, baseName: String)
  extends UnparserObject(context) {

  override lazy val childProcessors = Seq(rUnparser)

  val intN = n.toInt

  def checkN(ustate: UState, n: Long) {
    if (n > DaffodilTunableParameters.maxOccursBounds) {
      // TODO: how can we go after bigger than max int bytes? We have 64-bit computers
      // after all....
      UE(ustate, "Occurs count %s exceeds implementation maximum of %s.", n, DaffodilTunableParameters.maxOccursBounds)
    }
  }

  final def unparse(ustate: UState): Unit = {
    checkN(ustate, n)
    unparseAllRepeats(ustate)
  }

  protected def unparseAllRepeats(ustate: UState): Unit

  override def toString = "Rep" + baseName + "(" + rUnparser.toString + ")"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Rep" + baseName + ">" + rUnparser.toBriefXML(depthLimit - 1) +
        "</Rep" + baseName + ">"
  }
}

/**
 * This object is so that we can share the iteration idioms between situations
 * where we know N statically, and where dynamic evaluation computes N.
 *
 * In these cases, there are no new points of uncertainty because computed or
 * otherwise, we know N.
 */
object Rep {
  def loopExactlyTotalN(intN: Int, rUnparser: Unparser, ustate: UState, context: RuntimeData, iParser: Unparser): Unit = {
    while (ustate.arrayPos <= intN && !ustate.isInspectArrayEnd) {
      // Debugger.beforeRepetition(ustate, iParser)
      rUnparser.unparse1(ustate, context)
      // Debugger.afterRepetition(ustate, iParser)
      ustate.moveOverOneArrayIndexOnly
    }
  }
}

class RepExactlyNUnparser(n: Long, rUnparser: Unparser, context: ElementRuntimeData)
  extends RepUnparser(n, rUnparser, context, "ExactlyN") {

  def unparseAllRepeats(ustate: UState) {
    1 to intN foreach { _ =>
      {
        // Debugger.beforeRepetition(pResult, this)
        rUnparser.unparse1(ustate, context)
        // Debugger.afterRepetition(pResult, this)
        ustate.moveOverOneArrayIndexOnly
      }
    }
  }
}

class RepAtMostTotalNUnparser(n: Long, rUnparser: Unparser, erd: ElementRuntimeData)
  extends RepUnparser(n, rUnparser, erd, "AtMostTotalN") {

  def unparseAllRepeats(ustate: UState): Unit = {
    while (ustate.arrayPos <= intN && !ustate.isInspectArrayEnd) {
      // Debugger.beforeRepetition(ustate, this)
      rUnparser.unparse1(ustate, context)
      if (ustate.isInspectArrayEnd) return
      // Debugger.afterRepetition(ustate, this)
      ustate.moveOverOneArrayIndexOnly
    }
  }
}

class RepExactlyTotalNUnparser(n: Long, rUnparser: Unparser, context: ElementRuntimeData)
  extends RepUnparser(n, rUnparser, context, "ExactlyTotalN") {

  def unparseAllRepeats(ustate: UState) {
    Rep.loopExactlyTotalN(intN, rUnparser, ustate, context, this)
  }
}

/**
 * This is a bit tricky: Since we are streaming the elements to the unparser,
 * it will receive a DIArray element, but the length of that DIArray element at the
 * time it is received, will probably be 0 because the child elements (not even
 * one of them) will have been pulled yet. So we cannot just measure how big the array
 * is and iterate that many times. Rather, we have to iterate until we come to
 * an End(DIArray) event.
 *
 * This requires the ability to look ahead into the input stream by 1, see if it
 * is EndArray, and if so consume it and end the iteration.
 */
class RepUnboundedUnparser(occursCountKind: OccursCountKind.Value, rUnparser: Unparser, erd: ElementRuntimeData)
  extends RepUnparser(-1, rUnparser, erd, "Unbounded") {

  def unparseAllRepeats(ustate: UState) {
    Assert.invariant(ustate.currentInfosetNodeMaybe.isDefined)
    while (!ustate.isInspectArrayEnd) {
      // Debugger.beforeRepetition(ustate, this)
      rUnparser.unparse1(ustate, erd)
      // Debugger.afterRepetition(ustate, this)
      ustate.moveOverOneArrayIndexOnly
    }
  }
}

//
// Unparsers don't evaluate or use expression values. The number of occurrences is the number
// in the infoset (augmented up to minOccurs with default values when appropriate)
//

class RepAtMostOccursCountUnparser(rUnparser: Unparser, intN: Long, erd: ElementRuntimeData)
  extends RepUnparser(intN, rUnparser, erd, "AtMostOccursCount") {
  def unparseAllRepeats(ustate: UState) {
    // repeat either n times, or occursCount times if that's less than n.
    // val n = math.min(ustate.occursBounds, erd.minOccurs.get)
    Rep.loopExactlyTotalN(intN.toInt, rUnparser, ustate, erd, this)
  }
}
