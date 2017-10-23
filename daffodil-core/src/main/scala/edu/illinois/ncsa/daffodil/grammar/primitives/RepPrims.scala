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

package edu.illinois.ncsa.daffodil.grammar.primitives

import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.Implicits._;
import edu.illinois.ncsa.daffodil.dsom._
import RepPrims._
import edu.illinois.ncsa.daffodil.processors.parsers.RepAtMostTotalNParser
import edu.illinois.ncsa.daffodil.processors.parsers.RepExactlyTotalNParser
import edu.illinois.ncsa.daffodil.processors.parsers.RepUnboundedParser
import edu.illinois.ncsa.daffodil.processors.parsers.OccursCountExpressionParser
import edu.illinois.ncsa.daffodil.processors.parsers.RepExactlyNParser
import edu.illinois.ncsa.daffodil.processors.parsers.RepAtMostOccursCountParser
import edu.illinois.ncsa.daffodil.processors.parsers.RepExactlyTotalOccursCountParser
import edu.illinois.ncsa.daffodil.processors.unparsers.RepExactlyNUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.RepAtMostOccursCountUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.RepUnboundedUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.RepExactlyTotalNUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.RepAtMostTotalNUnparser
import edu.illinois.ncsa.daffodil.equality._;
import edu.illinois.ncsa.daffodil.processors.unparsers.NadaUnparser

object INoWarn6 { ImplicitsSuppressUnusedImportWarning() }

object ENoWarn6 { EqualitySuppressUnusedImportWarning() }

object RepPrims {
  abstract class RepPrim(context: ElementBase, n: Long, r: => Gram)
    extends UnaryGram(context, r) {
    Assert.invariant(n > 0)
    lazy val rd = context.elementRuntimeData
    lazy val intN = n.toInt
    lazy val rParser = r.parser
    lazy val rUnparser = r.unparser

  }

  abstract class Rep3Arg(f: (ElementBase, Long, => Gram) => Gram) {
    def apply(context: ElementBase, n: Long, rr: => Gram) = {
      lazy val r = rr
      if (n == 0 || r.isEmpty) EmptyGram
      else f(context, n, r)
    }
  }

  abstract class Rep2Arg(f: (ElementBase, => Gram) => Gram) {
    def apply(context: ElementBase, r: => Gram) = {
      lazy val rr = r
      if (rr.isEmpty) EmptyGram
      else f(context, r)
    }
  }

  class RepExactlyNPrim(context: ElementBase, n: Long, r: => Gram) extends RepPrim(context, n, r) {

    // Since this is Exactly N, there is no new point of uncertainty considerations here.
    override lazy val parser = new RepExactlyNParser(n, r.parser, context.elementRuntimeData)
    override lazy val unparser = new RepExactlyNUnparser(n, r.unparser, context.elementRuntimeData)

  }

  class RepAtMostTotalNPrim(context: ElementBase, n: Long, r: => Gram) extends RepPrim(context, n, r) {

    override lazy val parser = new RepAtMostTotalNParser(n, r.parser, context.elementRuntimeData)
    override lazy val unparser = new RepAtMostTotalNUnparser(n, r.unparser, context.elementRuntimeData)

  }

  class RepExactlyTotalNPrim(context: ElementBase, n: Long, r: => Gram) extends RepPrim(context, n, r) {

    override lazy val parser = new RepExactlyTotalNParser(n, r.parser, context.elementRuntimeData)
    override lazy val unparser = new RepExactlyTotalNUnparser(n, r.unparser, context.elementRuntimeData)

  }

  class RepUnboundedPrim(e: ElementBase, r: => Gram) extends RepPrim(e, 1, r) {

    override lazy val parser = new RepUnboundedParser(e.occursCountKind, r.parser, e.elementRuntimeData)
    override lazy val unparser = new RepUnboundedUnparser(e.occursCountKind, r.unparser, e.elementRuntimeData)
  }

  class RepAtMostOccursCountPrim(e: ElementBase, n: Long, r: => Gram)
    extends RepPrim(e, n, r) {

    override lazy val parser = new RepAtMostOccursCountParser(r.parser, n, e.elementRuntimeData)
    override lazy val unparser = new RepAtMostOccursCountUnparser(r.unparser, n, e.elementRuntimeData)

  }

  class RepExactlyTotalOccursCountPrim(e: ElementBase, r: => Gram)
    extends RepPrim(e, 1, r) {

    override lazy val parser = new RepExactlyTotalOccursCountParser(r.parser, e.elementRuntimeData)

    /**
     * Unparser for this is exactly the same as unbounded case - we output all the occurrences in the infoset.
     */
    override lazy val unparser = new RepUnboundedUnparser(e.occursCountKind, r.unparser, e.elementRuntimeData)

  }
}

object RepExactlyN extends Rep3Arg(new RepExactlyNPrim(_, _, _))

object RepAtMostTotalN extends Rep3Arg(new RepAtMostTotalNPrim(_, _, _))

object RepUnbounded extends Rep2Arg(new RepUnboundedPrim(_, _))

object RepExactlyTotalN extends Rep3Arg(new RepExactlyTotalNPrim(_, _, _))

object RepAtMostOccursCount extends Rep3Arg(new RepAtMostOccursCountPrim(_, _, _))

object RepExactlyTotalOccursCount extends Rep2Arg(new RepExactlyTotalOccursCountPrim(_, _))

case class OccursCountExpression(e: ElementBase)
  extends Terminal(e, true) {

  override lazy val parser = new OccursCountExpressionParser(e.occursCountEv, e.elementRuntimeData)
  override lazy val unparser = new NadaUnparser(e.runtimeData) // DFDL v1.0 Section 16.1.4 - The occursCount expression is not evaluated.

}
