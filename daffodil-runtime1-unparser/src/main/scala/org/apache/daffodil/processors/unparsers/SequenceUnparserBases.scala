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
import org.apache.daffodil.infoset.DIArray
import org.apache.daffodil.util.Maybe._

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

  /**
   * Unparses an entire sequence, including both scalar and array/optional children.
   *
   * Of note: This is not symmetric with parsing. For parsing there are separate parse() methods
   * for the separated and unseparated cases. For unparsing we were able to factor this
   * difference out into this common base method.
   */
  protected def unparse(state: UState): Unit = {

    state.groupIndexStack.push(1L) // one-based indexing

    var index = 0
    var doUnparser = false
    val limit = childUnparsers.length
    while (index < limit) {
      val childUnparser = childUnparsers(index)
      val trd = childUnparser.trd

      //
      // Unparsing an ordered sequence depends on the incoming
      // stream of infoset events matching up with the order that
      // they are expected as the unparser recurses through the
      // child term unparsers.
      //
      childUnparser match {
        case rep: RepUnparser => {
          val erd = rep.erd
          //
          // The number of occurrances we unparse is always exactly driven
          // by the number of infoset events for the repeating/optional element.
          //
          // For RepUnparser - array/optional case - in all cases we should get a
          // startArray event. If we don't then
          // the element must be entirely optional, so we get no events for it
          // at all.
          //
          if (state.inspect) {
            val ev = state.inspectAccessor
            if (ev.isStart && ev.isArray) {
              val eventNQN = ev.node.namedQName
              if (eventNQN =:= erd.namedQName) {
                //
                // StartArray for this unparser's array element
                //
                rep.startArray(state)
                while ({
                  doUnparser = shouldDoUnparser(trd, state)
                  doUnparser
                }) {
                  if (state.dataProc.isDefined) state.dataProc.get.beforeRepetition(state, this)

                  unparseOne(rep, erd, state)

                  if (state.dataProc.isDefined) state.dataProc.get.afterRepetition(state, this)
                }
                rep.endArray(state)
              } else {
                //
                // start array for some other array. Not this one. So we
                // don't unparse anything here, and we'll go on to the next
                // sequence child, which hopefully will be a matching array.
                //
                Assert.invariant(erd.minOccurs == 0L)
              }
            } else if (ev.isStart) {
              Assert.invariant(!ev.erd.isArray)
              //
              // start of scalar.
              // That has to be for a different element later in the sequence
              // since this one has a RepUnparser (i.e., is NOT scalar)
              val eventNQN = ev.node.namedQName
              Assert.invariant(eventNQN !=:= erd.namedQName)
            } else {
              Assert.invariant(ev.isEnd && ev.isComplex)
            }
          } else {
            // no event (state.inspect returned false)
            ??? // TODO: does this happen for the root element ??

          }
        }
        //
        case scalarUnparser => {
          unparseOne(scalarUnparser, trd, state)
        }
      }
      index += 1
      //
      // Note: the invariant is that unparsers move over 1 within their group themselves
      // we do not do the moving over here as we are the caller of the unparser.
      //
    }

    state.groupIndexStack.pop()
    //
    // this is establishing the invariant that unparsers (in this case the sequence unparser)
    // moves over within its containing group. The caller of an unparser does not do this move.
    //
    state.moveOverOneGroupIndexOnly() // move past this sequence itself, next group child it ITS parent.
  }

  private def shouldDoUnparser(childRD: TermRuntimeData, state: UState): Boolean = {
    childRD match {
      case erd: ElementRuntimeData => {
        // it's not a required element, so we check to see if we have a matching
        // incoming infoset event
        if (state.inspect) {
          val ev = state.inspectAccessor
          if (ev.isStart) {
            val eventNQN = ev.node.namedQName
            if (eventNQN =:= erd.namedQName) {
              true
            } else {
              false // event not a state for this element
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
            false
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
}
object SequenceChildUnparser {
  type SeparatedChildUnparser = SequenceChildUnparser with Separated
  type RepSeparatedChildUnparser = SeparatedChildUnparser with RepUnparser

  type UnseparatedChildUnparser = SequenceChildUnparser with Unseparated
  type RepUnseparatedChildUnparser = UnseparatedChildUnparser with RepUnparser
}

/**
 * base for unparsers for the children of sequences.
 *
 * There is one sequence child unparser for each child (declared) of the sequence.
 *
 * These do not iterate over multiple recurring instances. That iteration happens
 * in the caller. These unparse only a single occurrence when the child unparser
 * is for an array/optional element.
 */
abstract class SequenceChildUnparser(
  val childUnparser: Unparser,
  val srd: SequenceRuntimeData,
  val trd: TermRuntimeData)
  extends CombinatorUnparser(srd) {

  override def runtimeDependencies = Nil
}

/**
 * Mixin trait for unparsers of array/optional elements.
 *
 * The unparse() method unparses exactly one occurrance, does NOT iterate over
 * all the occurrences.
 */
trait RepUnparser { self: SequenceChildUnparser =>

  def childUnparser: Unparser
  def srd: SequenceRuntimeData
  def erd: ElementRuntimeData
  def baseName: String
  def minRepeats: Long
  def maxRepeats: Long

  /**
   * Unparse exactly one occurrence of an array/optional element.
   *
   * Iterating for arrays/optionals is done in the caller.
   */
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

  /**
   * Sets up for the start of an array. Pulls an event, which must be a start-array
   * event.
   */
  def startArray(state: UState): Unit = {
    state.arrayIndexStack.push(1L) // one-based indexing
    state.occursBoundsStack.push(state.tunable.maxOccursBounds)

    val event = state.advanceOrError
    Assert.invariant(event.isStart && event.node.isInstanceOf[DIArray])
  }

  /**
   * Ends an array. Pulls an event which must be an end-array event.
   * Validates array dimensions if validation has been requested.
   */
  def endArray(state: UState): Unit = {

    val event = state.advanceOrError
    if (!(event.isEnd && event.node.isInstanceOf[DIArray])) {
      UnparseError(One(erd.schemaFileLocation), One(state.currentLocation), "Expected array end event for %s, but received %s.", erd.namedQName, event)
    }

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

