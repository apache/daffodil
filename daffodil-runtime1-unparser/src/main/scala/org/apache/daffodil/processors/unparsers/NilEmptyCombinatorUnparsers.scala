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

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe

case class SimpleNilOrEmptyOrValueUnparser(ctxt: ElementRuntimeData,
  nilUnparser: Unparser, emptyUnparser: Unparser, valueUnparser: Unparser) extends CombinatorUnparser(ctxt) {

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(nilUnparser, emptyUnparser, valueUnparser)

  def unparse(state: UState): Unit = {
    Assert.invariant(Maybe.WithNulls.isDefined(state.currentInfosetNode))
    val inode = state.currentInfosetNode.asSimple
    // If this element has dfdl:outputValueCalc defined then the nilled value
    // is always ignored and only the OVC value is used. Furthermore, if the
    // element does not have xsi:nilled="true" in the infoset, that means
    // _isNilledSet will be false until the OVC is evaluated. Thus, if the OVC
    // suspends, this call to isNilled will throw a InfosetNoDataException
    // because _isNilled has not been set yet. Rather than having to deal with
    // suspending, only check isNilled for non-OVC elements.
    if (ctxt.outputValueCalcExpr.isEmpty && inode.isNilled) nilUnparser.unparse1(state)
    else if (inode.isEmpty) emptyUnparser.unparse1(state)
    else valueUnparser.unparse1(state)
  }
}

case class SimpleNilOrValueUnparser(ctxt: ElementRuntimeData,
  nilUnparser: Unparser, valueUnparser: Unparser) extends CombinatorUnparser(ctxt) {

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(nilUnparser, valueUnparser)

  def unparse(state: UState): Unit = {
    Assert.invariant(Maybe.WithNulls.isDefined(state.currentInfosetNode))
    val inode = state.currentInfosetNode.asSimple
    // see comment above for why this OVC check is necessary
    if (ctxt.outputValueCalcExpr.isEmpty && inode.isNilled) nilUnparser.unparse1(state)
    else valueUnparser.unparse1(state)
  }
}

case class SimpleEmptyOrValueUnparser(ctxt: ElementRuntimeData,
  emptyUnparser: Unparser, valueUnparser: Unparser) extends CombinatorUnparser(ctxt) {

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(emptyUnparser, valueUnparser)

  def unparse(state: UState): Unit = {
    Assert.invariant(Maybe.WithNulls.isDefined(state.currentInfosetNode))
    val inode = state.currentInfosetNode.asSimple
    if (inode.isEmpty) emptyUnparser.unparse1(state)
    else valueUnparser.unparse1(state)
  }
}

case class ComplexNilOrContentUnparser(ctxt: ElementRuntimeData,
  nilUnparser: Unparser, contentUnparser: Unparser) extends CombinatorUnparser(ctxt) {

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Seq(nilUnparser, contentUnparser)

  def unparse(state: UState): Unit = {
    Assert.invariant(Maybe.WithNulls.isDefined(state.currentInfosetNode))
    val inode = state.currentInfosetNode.asComplex
    if (inode.isNilled) nilUnparser.unparse1(state)
    else contentUnparser.unparse1(state)
  }
}
