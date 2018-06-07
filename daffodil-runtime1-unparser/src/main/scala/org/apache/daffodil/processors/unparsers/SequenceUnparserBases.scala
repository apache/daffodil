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

import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.equality._

abstract class OrderedSequenceUnparserBase(srd: SequenceRuntimeData, childUnparsers: Seq[SequenceChildUnparser])
  extends CombinatorUnparser(srd) {

  override def nom = "Sequence"

  override lazy val runtimeDependencies: Seq[Evaluatable[AnyRef]] = Nil

  override lazy val childProcessors = childUnparsers

  /**
   * Unparses one iteration of an array/optional element
   */
  protected def unparseOne(
    unparser: SequenceChildUnparser,
    trd: TermRuntimeData,
    state: UState): Unit

  // Sequences of nothing (no initiator, no terminator, nothing at all) should
  // have been optimized away
  Assert.invariant(childUnparsers.length > 0)

  // Since some of the grammar terms might have folded away to EmptyGram,
  // the number of unparsers here may be different from the number of
  // children of the sequence group.
  Assert.invariant(srd.groupMembers.length >= childUnparsers.length - 1) // minus 1 for the separator unparser

  protected def unparse(start: UState): Unit = {

    start.groupIndexStack.push(1L) // one-based indexing
    unparseChildren(start)
    start.groupIndexStack.pop()
    //
    // this is establishing the invariant that unparsers (in this case the sequence unparser)
    // moves over within its containing group. The caller of an unparser does not do this move.
    //
    start.moveOverOneGroupIndexOnly()
  }

  private def unparseChildren(start: UState): Unit = {

    var index = 0
    var doUnparser = false
    val limit = childUnparsers.length
    while (index < limit) {
      val childUnparser = childUnparsers(index)
      val trd = childUnparser.trd
      val childRD = childUnparser.trd

      doUnparser = shouldDoUnparser(childRD, start)

      if (doUnparser) {
        if (start.dataProc.isDefined) start.dataProc.get.beforeRepetition(start, this)

        unparseOne(childUnparser, trd, start)

        if (start.dataProc.isDefined) start.dataProc.get.afterRepetition(start, this)
      }
      index += 1
      //
      // Note: the invariant is that unparsers move over 1 within their group themselves
      // we do not do the moving over here as we are the caller of the unparser.
      //
    }
  }

  private def shouldDoUnparser(childRD: TermRuntimeData, start: UState): Boolean = {
    childRD match {
      case erd: ElementRuntimeData if !erd.isRequired => {
        // it's not a required element, so we check to see if we have a matching
        // incoming infoset event
        if (start.inspect) {
          val ev = start.inspectAccessor
          if (ev.isStart) {
            val eventNQN = ev.node.namedQName
            if (eventNQN =:= erd.namedQName) {
              true
            } else {
              false // event not a start for this element
            }
          } else if (ev.isEnd && ev.isComplex) {
            val c = ev.asComplex
            //ok. We've peeked ahead and found the end of the complex element
            //that this sequence is the model group of.
            val optParentRD = srd.immediateEnclosingElementRuntimeData
            optParentRD match {
              case Some(e: ElementRuntimeData) => {
                Assert.invariant(c.runtimeData.namedQName =:= e.namedQName)
                false
              }
              case _ =>
                Assert.invariantFailed("Not end element for this sequence's containing element. Event %s, optParentRD %s.".format(
                  ev, optParentRD))
            }
          } else {
            Assert.invariantFailed("Not a start event: " + ev)
          }
        } else {
          // was no element, so no unparse
          false
        }
      }
      case _ => {
        // since only elements can be optional, anything else is non-optional
        true
      }
    }
  }

  //  protected def unparse(state: UState): Unit = {
  //
  //    var scpIndex = 0
  //    state.groupIndexStack.push(1L) // one-based indexing
  //
  //    val limit = childUnparsers.length
  //
  //    while ((scpIndex < limit) && (state.processorStatus eq Success)) {
  //      val child = childUnparsers(scpIndex)
  //      val trd = child.trd
  //      child match {
  //        case unparser: RepSeparatedChildUnparser => {
  //
  //          val loopState = unparser.loopState(state)
  //
  //          // push new array context for array/optional
  //          unparser.startArray(state)
  //
  //          var ais: ArrayIndexStatus = null
  //
  //          while ({
  //            ais = loopState.arrayIndexStatus(unparser, state)
  //            ais.isInstanceOf[GoArrayIndexStatus]
  //          }) {
  //
  //            unparseOne(unparser, trd, state, ais.asInstanceOf[GoArrayIndexStatus])
  //
  //            loopState.nextArrayIndex(state)
  //          } // end while for each repeat
  //
  //          unparser.endArray(state)
  //        } // end match case RepUnparser
  //
  //        case scalarUnparser => {
  //          unparseOne(scalarUnparser, trd, state, ArrayIndexStatus.Required)
  //        } // end match case scalar unparser
  //      } // end match
  //      scpIndex += 1
  //    } // end while for each sequence child unparser
  //
  //    state.groupIndexStack.pop()
  //    state.moveOverOneGroupIndexOnly()
  //    ()
  //  }
}
object SequenceChildUnparser {
  type SeparatedChildUnparser = SequenceChildUnparser with Separated
  type RepSeparatedChildUnparser = SeparatedChildUnparser with RepUnparser

  type UnseparatedChildUnparser = SequenceChildUnparser with Unseparated
  type RepUnseparatedChildUnparser = UnseparatedChildUnparser with RepUnparser
}

abstract class SequenceChildUnparser(
  val childUnparser: Unparser,
  val srd: SequenceRuntimeData,
  val trd: TermRuntimeData)
  extends CombinatorUnparser(srd) {

  override def runtimeDependencies = Nil
}

trait RepUnparser { self: SequenceChildUnparser =>

  def childUnparser: Unparser
  def srd: SequenceRuntimeData
  def erd: ElementRuntimeData
  def baseName: String
  def minRepeats: Long
  def maxRepeats: Long

  override protected def unparse(state: UState): Unit = {
    childUnparser.unparse1(state)
  }

  override lazy val runtimeDependencies = Nil

  override def toString = "Rep" + baseName + "(" + childUnparser.toString + ")"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Rep" + baseName + " name='" + erd.name + "'>" + childUnparser.toBriefXML(depthLimit - 1) +
        "</Rep" + baseName + ">"
  }

  def startArray(state: UState): Unit = {
    state.arrayIndexStack.push(1L) // one-based indexing
    state.occursBoundsStack.push(state.tunable.maxOccursBounds)
  }

  def endArray(state: UState): Unit = {
    val actualOccurs = state.arrayIndexStack.pop()
    state.occursBoundsStack.pop()

    if (state.processorStatus ne Success) return

    val shouldValidate =
      state.dataProc.isDefined && state.dataProc.value.getValidationMode != ValidationMode.Off

    if (shouldValidate) {
      val minO = erd.minOccurs
      val maxO = erd.maxOccurs
      val isUnbounded = maxO == -1
      val occurrence = actualOccurs - 1

      if (isUnbounded && occurrence < minO)
        state.validationError("%s occurred '%s' times when it was expected to be a " +
          "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.diagnosticDebugName,
          occurrence, minO)
      else if (!isUnbounded && (occurrence < minO || occurrence > maxO))
        state.validationError("%s occurred '%s' times when it was expected to be a " +
          "minimum of '%s' and a maximum of '%s' times.", erd.diagnosticDebugName,
          occurrence, minO, maxO)
      else {
        //ok
      }
    }
  }
}

