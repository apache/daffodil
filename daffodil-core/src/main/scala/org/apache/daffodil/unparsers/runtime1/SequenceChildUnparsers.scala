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
package org.apache.daffodil.unparsers.runtime1

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.parsers.EndArrayChecksMixin
import org.apache.daffodil.runtime1.processors.parsers.MinMaxRepeatsMixin
import org.apache.daffodil.runtime1.processors.unparsers._

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
  val trd: TermRuntimeData
) extends CombinatorUnparser(srd) {

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
  val erd: ElementRuntimeData
) extends SequenceChildUnparser(childUnparser, srd, erd)
  with MinMaxRepeatsMixin
  with EndArrayChecksMixin {

  /**
   * Unparse exactly one occurrence of an array/optional element.
   *
   * Iterating for arrays/optionals is done in the caller.
   */
  override protected def unparse(state: UState): Unit = {
    childUnparser.unparse1(state)
  }

  override def runtimeDependencies = Vector()

  override def toString = "RepUnparser(" + childUnparser.toString + ")"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else
      "<RepUnparser name='" + erd.name + "'>" + childUnparser.toBriefXML(depthLimit - 1) +
        "</RepUnparser>"
  }

  /**
   * Sets up for the start of an array/optional. For true array, pulls an event, which must be a start-array
   * event.
   */
  final def startArrayOrOptional(state: UState): Unit = {
    val ev = state.inspectAccessor
    if (ev.erd.isArray) {
      // only pull start array event for a true array, not an optional.
      val event = state.advanceOrError
      Assert.invariant(event.isStart && event.isArray)
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
      if (!(event.isEnd && event.isArray)) {
        UE(
          state,
          "Expected array end event for %s, but received %s.",
          erd.namedQName.toExtendedSyntax,
          event
        )
      }
    }

    // State could be Success or Failure here.
    endArray(state, state.occursPos - 1)
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
              if (ev.erd eq erd) {
                true
              } else {
                // System.err.println("Stopping occurrences(1) of %s due to event %s".format(erd.namedQName, ev))
                false // event not a start for this element
              }
            } else {
              Assert.invariant(ev.isEnd)
              // could be end of simple element - we handle on the start event. Nothing to do.
              // or could be end of complex event, i.e., we've peeked ahead and found the end of a complex element.
              // It has to be the complex element that ultimately encloses this sequence.
              // Though that's not a unique element given that this sequence could be inside
              // a global group definition that is reused in multiple places.
              // Nothing to do for complex type either.
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
    numOccurrences: Int,
    maxReps: Long,
    arrPos: Long
  ): Unit = {
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
      UE(
        state,
        "Expected %s additional %s elements, but received %s.",
        minReps - numOccurrences,
        erd.namedQName,
        ev
      )
    }
    //
    // Now if there is an unspecified number of occurrences
    // and we're supposed to respect min and maxOccurs
    // (Per DFDL Spec 16.1)
    //
    val ock = erd.maybeOccursCountKind.get

    // check against maxReps + 1 since we incremented to the next array position
    // before calling this check.
    if ((ock eq Implicit) && unparser.isBoundedMax && (arrPos > maxReps + 1))
      UE(state, "Expected maximum of %s elements, but received %s.", maxReps, arrPos - 1)

    //
    // check that if we could consume more (didn't end due to maxReps)
    // Then we ended with whatever's next NOT being another occurrence of this
    // same element.
    //
    if (
      arrPos < maxReps &&
      minReps < maxReps &&
      (ock ne Implicit)
    ) {
      if (state.inspect) {
        val ev = state.inspectAccessor
        if (ev.isStart && ev.erd.namedQName == erd)
          // This is the error we get if there are more events in the infoset event stream
          // for an array, than are allowed by maxOccurs.
          UE(
            state,
            "More than maxOccurs %s occurrences of %s in Infoset input: Expected no more events for %s, but received %s.",
            maxReps,
            erd.namedQName,
            ev
          )
      }
    }
  }
}
