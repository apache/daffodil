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

import org.apache.daffodil.processors.{ SequenceRuntimeData, TermRuntimeData }
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind

trait Unseparated { self: SequenceChildUnparser =>

  val childProcessors = Vector(childUnparser)
}

class ScalarOrderedUnseparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData)
  extends SequenceChildUnparser(childUnparser, srd, trd)
  with Unseparated {

  override protected def unparse(state: UState) = childUnparser.unparse1(state)
}

class RepOrderedUnseparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData)
  extends RepeatingChildUnparser(childUnparser, srd, erd)
  with Unseparated {

  private val ock = erd.maybeOccursCountKind.get

  override def checkArrayPosAgainstMaxOccurs(state: UState): Boolean = {
    if (ock eq OccursCountKind.Implicit)
      state.arrayPos <= maxRepeats(state)
    else
      true
  }
}

class OrderedUnseparatedSequenceUnparser(rd: SequenceRuntimeData, childUnparsers: Seq[SequenceChildUnparser])
  extends OrderedSequenceUnparserBase(rd, childUnparsers.toVector) {

  /**
   * Unparses one iteration of an array/optional element
   */
  protected def unparseOne(
    unparser: SequenceChildUnparser,
    trd: TermRuntimeData,
    state: UState): Unit = {

    unparser.unparse1(state)
  }

  /**
   * Unparses an entire sequence, including both scalar and array/optional children.
   */
  protected def unparse(state: UState): Unit = {

    state.groupIndexStack.push(1L) // one-based indexing

    var index = 0
    var doUnparser = false
    val limit = childUnparsers.length
    while (index < limit) {
      val childUnparser = childUnparsers(index)
      val trd = childUnparser.trd
      state.pushTRD(trd) // because we inspect before we invoke the unparser

      //
      // Unparsing an ordered sequence depends on the incoming
      // stream of infoset events matching up with the order that
      // they are expected as the unparser recurses through the
      // child term unparsers.
      //
      childUnparser match {
        case unparser: RepeatingChildUnparser => {
          state.arrayIndexStack.push(1L)
          val erd = unparser.erd
          var numOccurrences = 0
          val maxReps = unparser.maxRepeats(state)
          val minReps = unparser.minRepeats(state)

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
            val isArr = ev.isArray
            if (ev.isStart && (isArr || ev.erd.isOptional)) {
              val eventNQN = ev.namedQName
              if (ev.erd eq erd) {
                //
                // StartArray for this unparser's array element
                //
                unparser.startArrayOrOptional(state)
                while ({
                  doUnparser = unparser.shouldDoUnparser(unparser, state)
                  doUnparser
                }) {
                  if (isArr) if (state.dataProc.isDefined) state.dataProc.get.beforeRepetition(state, this)

                  unparseOne(unparser, erd, state)
                  numOccurrences += 1
                  state.moveOverOneArrayIndexOnly()
                  state.moveOverOneGroupIndexOnly() // array elements are always represented.

                  if (isArr) if (state.dataProc.isDefined) state.dataProc.get.afterRepetition(state, this)
                }

                unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(state, unparser, numOccurrences, maxReps, state.arrayPos - 1)
                unparser.endArrayOrOptional(erd, state)
              } else {
                //
                // start array but not for the expected array,
                // rather for some other array. Not this one. So we
                // don't unparse anything here, and we'll go on to the next
                // sequence child, which hopefully will be a matching array.
                //
                // minReps has to be 0, meaning it is allowed to have zero
                // occurrences (not necessarily valid, but allowed),
                // because we got zero instances of this array
                //
                Assert.invariant(minReps == 0L)
              }
            } else if (ev.isStart) {
              Assert.invariant(!isArr && !ev.erd.isOptional)
              //
              // start of scalar.
              // That has to be for a different element later in the sequence
              // since this one has a RepUnparser (i.e., is NOT scalar)
              val eventNQN = ev.namedQName
              Assert.invariant(eventNQN != erd.namedQName)
            } else {
              Assert.invariant(ev.isEnd && ev.erd.isComplexType)
              unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(state, unparser, numOccurrences, maxReps, 0)
            }
          } else {
            // no event (state.inspect returned false)
            Assert.invariantFailed("No event for unparing.")
          }

          state.arrayIndexStack.pop()
        }
        //
        case scalarUnparser => {
          unparseOne(scalarUnparser, trd, state)
          // only move over in group if the scalar "thing" is an element
          // that is represented.
          scalarUnparser.trd match {
            case erd: ElementRuntimeData if (!erd.isRepresented) => // ok, skip group advance
            case _ => state.moveOverOneGroupIndexOnly()
          }
        }
      }

      state.popTRD(trd)
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
    // state.moveOverOneGroupIndexOnly() // move past this sequence itself, next group child it ITS parent.
  }

}
