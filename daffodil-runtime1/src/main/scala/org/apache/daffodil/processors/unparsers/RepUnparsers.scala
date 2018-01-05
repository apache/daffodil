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

package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.processors.TermRuntimeData

abstract class RepUnparser(n: Long, rUnparser: Unparser, context: ElementRuntimeData, baseName: String)
  extends CombinatorUnparser(context) {

  override lazy val childProcessors = Seq(rUnparser)

  val intN = n.toInt

  def checkN(ustate: UState, n: Long) {
    if (n > ustate.tunable.maxOccursBounds) {
      // TODO: how can we go after bigger than max int bytes? We have 64-bit computers
      // after all....
      UE(ustate, "Occurs count %s exceeds implementation maximum of %s.", n, ustate.tunable.maxOccursBounds)
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
  def loopExactlyTotalN(intN: Int, rUnparser: Unparser, ustate: UState, context: TermRuntimeData, iParser: Unparser): Unit = {
    while (ustate.arrayPos <= intN && !ustate.isInspectArrayEnd) {
      // Debugger.beforeRepetition(ustate, iParser)
      rUnparser.unparse1(ustate)
      // Debugger.afterRepetition(ustate, iParser)
      ustate.moveOverOneArrayIndexOnly
    }
  }
}

class RepExactlyNUnparser(n: Long, rUnparser: Unparser, context: ElementRuntimeData)
  extends RepUnparser(n, rUnparser, context, "ExactlyN") {

  override lazy val runtimeDependencies = Nil

  def unparseAllRepeats(ustate: UState) {
    1 to intN foreach { _ =>
      {
        // Debugger.beforeRepetition(pResult, this)
        rUnparser.unparse1(ustate)
        // Debugger.afterRepetition(pResult, this)
        ustate.moveOverOneArrayIndexOnly
      }
    }
  }
}

class RepAtMostTotalNUnparser(n: Long, rUnparser: Unparser, erd: ElementRuntimeData)
  extends RepUnparser(n, rUnparser, erd, "AtMostTotalN") {

  override lazy val runtimeDependencies = Nil

  def unparseAllRepeats(ustate: UState): Unit = {
    while (ustate.arrayPos <= intN && !ustate.isInspectArrayEnd) {
      // Debugger.beforeRepetition(ustate, this)
      rUnparser.unparse1(ustate)
      if (ustate.isInspectArrayEnd) return
      // Debugger.afterRepetition(ustate, this)
      ustate.moveOverOneArrayIndexOnly
    }
  }
}

class RepExactlyTotalNUnparser(n: Long, rUnparser: Unparser, context: ElementRuntimeData)
  extends RepUnparser(n, rUnparser, context, "ExactlyTotalN") {

  override lazy val runtimeDependencies = Nil

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

  override lazy val runtimeDependencies = Nil

  def unparseAllRepeats(ustate: UState) {
    Assert.invariant(ustate.currentInfosetNodeMaybe.isDefined)
    while (!ustate.isInspectArrayEnd) {
      // Debugger.beforeRepetition(ustate, this)
      rUnparser.unparse1(ustate)
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

  override lazy val runtimeDependencies = Nil

  def unparseAllRepeats(ustate: UState) {
    // repeat either n times, or occursCount times if that's less than n.
    // val n = math.min(ustate.occursBounds, erd.minOccurs.get)
    Rep.loopExactlyTotalN(intN.toInt, rUnparser, ustate, erd, this)
  }
}
