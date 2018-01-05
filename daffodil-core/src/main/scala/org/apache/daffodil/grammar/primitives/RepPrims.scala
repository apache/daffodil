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

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.grammar._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.Implicits._;
import org.apache.daffodil.dsom._
import RepPrims._
import org.apache.daffodil.processors.parsers.RepAtMostTotalNParser
import org.apache.daffodil.processors.parsers.RepExactlyTotalNParser
import org.apache.daffodil.processors.parsers.RepUnboundedParser
import org.apache.daffodil.processors.parsers.OccursCountExpressionParser
import org.apache.daffodil.processors.parsers.RepExactlyNParser
import org.apache.daffodil.processors.parsers.RepAtMostOccursCountParser
import org.apache.daffodil.processors.parsers.RepExactlyTotalOccursCountParser
import org.apache.daffodil.processors.unparsers.RepExactlyNUnparser
import org.apache.daffodil.processors.unparsers.RepAtMostOccursCountUnparser
import org.apache.daffodil.processors.unparsers.RepUnboundedUnparser
import org.apache.daffodil.processors.unparsers.RepExactlyTotalNUnparser
import org.apache.daffodil.processors.unparsers.RepAtMostTotalNUnparser
import org.apache.daffodil.equality._;
import org.apache.daffodil.processors.unparsers.NadaUnparser

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
  override lazy val unparser = new NadaUnparser(e.termRuntimeData) // DFDL v1.0 Section 16.1.4 - The occursCount expression is not evaluated.

}
