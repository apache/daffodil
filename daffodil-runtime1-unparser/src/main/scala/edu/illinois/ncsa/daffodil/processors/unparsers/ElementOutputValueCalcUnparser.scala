/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.dpath.SuspendableExpression
import edu.illinois.ncsa.daffodil.processors.LengthEv
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.util.MaybeULong

class ElementOutputValueCalcStaticLengthUnparser(erd: ElementRuntimeData, repUnparser: Unparser, maybeKnownLengthInBits: MaybeULong)
    extends UnparserObject(erd) with TextUnparserRuntimeMixin {

  Assert.invariant(erd.outputValueCalcExpr.isDefined)
  val expr = erd.outputValueCalcExpr.get

  override lazy val childProcessors = Seq(repUnparser)

  override def unparse(ustate: UState): Unit = {
    Assert.invariant(erd.outputValueCalcExpr.isDefined)

    val diSimple = ustate.currentInfosetNode.asSimple
    // note. This got attached to infoset in StatementElementOutputValueCalcUnparser

    //
    // Forces the evaluation of runtime-valued things, and this will cause those
    // that actually are runtime-expressions to be cached on the infoset element.
    //
    // Then later when the actual unparse occurs, these will be accessed off the
    // infoset element's cache.
    //

    repUnparser.runtimeDependencies.foreach {
      _.evaluate(ustate) // these evaluations will force dependencies of the dependencies. So we just do 1 tier, not a tree walk.
    }

    SuspendableExpression(diSimple, expr, ustate, repUnparser, maybeKnownLengthInBits)
  }
}

class ElementOutputValueCalcRuntimeLengthUnparser(erd: ElementRuntimeData, repUnparser: Unparser,
  lengthEv: LengthEv, lengthUnits: LengthUnits)
    extends UnparserObject(erd) with TextUnparserRuntimeMixin {

  Assert.invariant(erd.outputValueCalcExpr.isDefined)
  val expr = erd.outputValueCalcExpr.get

  override lazy val childProcessors = Seq(repUnparser)
  override lazy val runtimeDependencies = Seq(lengthEv)

  override def unparse(ustate: UState): Unit = {
    Assert.invariant(erd.outputValueCalcExpr.isDefined)

    val diSimple = ustate.currentInfosetNode.asSimple
    // note. This got attached to infoset in StatementElementOutputValueCalcUnparser

    //
    // Forces the evaluation of runtime-valued things, and this will cause those
    // that actually are runtime-expressions to be cached on the infoset element.
    //
    // Then later when the actual unparse occurs, these will be accessed off the
    // infoset element's cache.
    //

    repUnparser.runtimeDependencies.foreach {
      _.evaluate(ustate) // these evaluations will force dependencies of the dependencies. So we just do 1 tier, not a tree walk.
    }

    val length: Long = lengthEv.evaluate(ustate)

    val knownLengthInBits: Long = lengthUnits match {
      case LengthUnits.Bits => length
      case LengthUnits.Bytes => length * 8
      case LengthUnits.Characters => length * erd.encInfo.knownEncodingWidthInBits
    }

    SuspendableExpression(diSimple, expr, ustate, repUnparser, MaybeULong(knownLengthInBits))

  }

}
