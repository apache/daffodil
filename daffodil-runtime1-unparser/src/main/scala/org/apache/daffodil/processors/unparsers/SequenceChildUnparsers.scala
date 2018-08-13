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

import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DIArray
import org.apache.daffodil.equality._
import org.apache.daffodil.processors.parsers.MinMaxRepeatsMixin
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind

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

  override def runtimeDependencies = Vector()
}

/**
 * Base for unparsers of array/optional elements.
 *
 * The unparse() method unparses exactly one occurrance, does NOT iterate over
 * all the occurrences.
 */
abstract class RepeatingChildUnparser(
  override val childUnparser: Unparser,
  override val srd: SequenceRuntimeData,
  val erd: ElementRuntimeData)
  extends SequenceChildUnparser(childUnparser, srd, erd)
  with MinMaxRepeatsMixin {

  /**
   * Unparse exactly one occurrence of an array/optional element.
   *
   * Iterating for arrays/optionals is done in the caller.
   */
  override protected def unparse(state: UState): Unit = {
    childUnparser.unparse1(state)
  }

  override lazy val runtimeDependencies = Vector()

  override def toString = "RepUnparser(" + childUnparser.toString + ")"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<RepUnparser name='" + erd.name + "'>" + childUnparser.toBriefXML(depthLimit - 1) +
        "</RepUnparser>"
  }

  /**
   * Sets up for the start of an array/optional. For true array, pulls an event, which must be a start-array
   * event.
   */
  final def startArrayOrOptional(state: UState): Unit = {
    //
    // The Infoset events don't create an array start/end event for a
    // truly optional (0..1) element.
    //
    // But an expression might still refer to dfdl:occursIndex() and we want that
    // to be 1, not wherever an enclosing parent array is.
    //
    state.arrayIndexStack.push(1L) // one-based indexing

    val ev = state.inspectAccessor
    if (ev.erd.isArray) {
      // only pull start array event for a true array, not an optional.
      val event = state.advanceOrError
      Assert.invariant(event.isStart && event.node.isInstanceOf[DIArray])
      // pull the next event, so that nothing else sees or deals with array start events
      state.inspect
    }
  }

  /**
   * Ends an array/optional. For a true array, pulls an event which must be an end-array event.
   * Validates array dimensions if validation has been requested.
   */
  final def endArrayOrOptional(currentArrayERD: ElementRuntimeData, state: UState): Unit = {
    if (currentArrayERD.isArray) {
      // only pull end array event for a true array, not an optional
      val event = state.advanceOrError
      if (!(event.isEnd && event.node.isInstanceOf[DIArray])) {
        UE(state, "Expected array end event for %s, but received %s.", erd.namedQName, event)
      }
    }

    // However, we pop the arrayIndexStack for either true arrays or optionals.
    val actualOccurs = state.arrayIndexStack.pop()

    Assert.invariant(state.processorStatus eq Success)

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

  /**
   * Determines if the incoming event matches the current term, so we
   * should run its unparser.
   *
   * If the term is an element and the event is a start element,
   * then true if the incoming element event namedQName matches the expected element namedQName.
   * Always true if the term is a model-group and the event is a start element.
   * If the event is not a start, it must be an endArray for the enclosing complex element, and
   * the answer is false.
   */
  final def shouldDoUnparser(unparser: RepeatingChildUnparser, state: UState): Boolean = {
    val childRD = unparser.trd
    val res =
      childRD match {
        case erd: ElementRuntimeData => {
          // check to see if we have a matching
          // incoming infoset event
          if (state.inspect) {
            val ev = state.inspectAccessor
            if (ev.isStart) {
              val eventNQN = ev.node.namedQName
              if (eventNQN =:= erd.namedQName) {
                true
              } else {
                // System.err.println("Stopping occurrences(1) of %s due to event %s".format(erd.namedQName, ev))
                false // event not a start for this element
              }
            } else if (ev.isEnd && ev.isComplex) {
              val c = ev.asComplex
              //ok. We've peeked ahead and found the end of the complex element
              //that this sequence is the model group of.
              val optParentRD = srd.immediateEnclosingElementRuntimeData
              // FIXME: there's no reason to walk backpointer to immediateEnclosingElementRuntimeData
              // as we just want that element's name, and we could precompute that and
              // include it on the SequenceChildUnparser for this
              optParentRD match {
                case Some(e: ElementRuntimeData) => {
                  Assert.invariant(c.runtimeData.namedQName =:= e.namedQName)
                  // System.err.println("Stopping occurrences(2) of %s due to event %s".format(erd.namedQName, ev))
                  false
                }
                case _ =>
                  Assert.invariantFailed("Not end element for this sequence's containing element. Event %s, optParentRD %s.".format(
                    ev, optParentRD))
              }
            } else {
              // end of simple element. We shouldUnparse on the start event.
              // System.err.println("Stopping occurrences(3) of %s due to event %s".format(erd.namedQName, ev))
              false
            }
          } else {
            // was no event, so no unparse
            // System.err.println("Stopping occurrences of(4) %s due to No infoset event".format(erd.namedQName))
            false
          }
        }
        case _ => {
          // since only elements can be optional, anything else is non-optional
          true
        }
      }
    val chk: Boolean =
      if (res)
        unparser.checkArrayPosAgainstMaxOccurs(state)
      else
        false
    chk
  }

  /**
   * True if arrayPos is less than maxOccurs, but only if we care about
   * maxOccurs. Always true if occursCountKind is not one where we
   * bound occurrences with maxOccurs.
   *
   */
  def checkArrayPosAgainstMaxOccurs(state: UState): Boolean

  /**
   * For OccursCountKind 'implicit', we need to check for arrayPos in range
   * and enforce the min/maxOccurs bounds.
   *
   * maxReps is passed in, since it will already have been computed before hand.
   *
   * This depends on maxReps and minReps being properly setup to represent the
   * bounds checking that should be done. E.g., if 'implicit' but maxReps is
   * 'unbounded', then maxReps will be Long.MaxValue.
   */
  def checkFinalOccursCountBetweenMinAndMaxOccurs(
    state: UState,
    unparser: RepeatingChildUnparser,
    numOccurrences: Int, maxReps: Long): Unit = {
    import OccursCountKind._

    val minReps = unparser.minRepeats(state)
    val ev = state.inspectAccessor
    val erd = unparser.erd

    if (numOccurrences < minReps) {
      //
      // Defaultable feature not yet implemented.
      //
      // Assert.invariant(!erd.isDefaultable) // should have handled this elsewhere
      //
      UE(state, "Expected %s additional %s elements, but received %s.", minReps - numOccurrences, erd.namedQName, ev)
    }
    //
    // Now if there is an unspecified number of occurrences
    // and we're supposed to respect min and maxOccurs
    // (Per DFDL Spec 16.1)
    //
    val ock = erd.maybeOccursCountKind.get
    // System.err.println("Checking for events consistent with Array index:\n ==> Array Index Stack is:" + state.arrayIndexStack)
    val arrPos = state.arrayPos - 1 // because we advance after each unparse so this is the position of the next (non-existing) occurrence

    Assert.invariant(arrPos <= maxReps || !(ock eq Implicit))

    //
    // check that if we could consume more (didn't end due to maxReps)
    // Then we ended with whatever's next NOT being another occurrence of this
    // same element.
    //
    if (arrPos < maxReps &&
      minReps < maxReps &&
      (ock ne Implicit)) {
      if (state.inspect) {
        val ev = state.inspectAccessor
        if (ev.isStart && ev.node.namedQName == erd)
          // This is the error we get if there are more events in the infoset event stream
          // for an array, than are allowed by maxOccurs.
          UE(state, "More than maxOccurs %s occurrences of %s in Infoset input: Expected no more events for %s, but received %s.", maxReps, erd.namedQName, ev)
      }
    }
  }
}

