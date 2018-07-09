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
//
//package org.apache.daffodil.processors.unparsers
//
//import org.apache.daffodil.processors.Evaluatable
//import org.apache.daffodil.processors.ElementRuntimeData
//import org.apache.daffodil.processors.RuntimeData
//import org.apache.daffodil.processors.Processor
//import org.apache.daffodil.schema.annotation.props.gen.SeparatorPosition
//import org.apache.daffodil.exceptions.Assert
//import org.apache.daffodil.equality._
//import org.apache.daffodil.processors.SequenceRuntimeData
//import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
//import org.apache.daffodil.processors.TermRuntimeData
//
//sealed abstract class OrderedSequenceUnparserBase(ctxt: SequenceRuntimeData, childUnparsers: Seq[Unparser])
//  extends CombinatorUnparser(ctxt) {
//  override def nom = "Sequence"
//
//  // Sequences of nothing (no initiator, no terminator, nothing at all) should
//  // have been optimized away
//  Assert.invariant(childUnparsers.length > 0)
//
//  // Since some of the grammar terms might have folded away to EmptyGram,
//  // the number of unparsers here may be different from the number of
//  // children of the sequence group.
//  Assert.invariant(ctxt.groupMembers.length >= childUnparsers.length - 1) // minus 1 for the separator unparser
//
//  override lazy val childProcessors: Seq[Processor] = childUnparsers
//
//  def unparse(start: UState): Unit = {
//
//    start.groupIndexStack.push(1L) // one-based indexing
//    unparseChildren(start)
//    start.groupIndexStack.pop()
//    //
//    // this is establishing the invariant that unparsers (in this case the sequence unparser)
//    // moves over within its containing group. The caller of an unparser does not do this move.
//    //
//    start.moveOverOneGroupIndexOnly()
//  }
//
//  protected def unparseChildren(start: UState): Unit
//
//  protected final def shouldDoUnparser(childRD: RuntimeData, start: UState): Boolean = {
//    childRD match {
//      case erd: ElementRuntimeData if !erd.isRequired => {
//        // it's not a required element, so we check to see if we have a matching
//        // incoming infoset event
//        if (start.inspect) {
//          val ev = start.inspectAccessor
//          if (ev.isStart) {
//            val eventNQN = ev.node.namedQName
//            if (eventNQN =:= erd.namedQName) {
//              true
//            } else {
//              false // event not a start for this element
//            }
//          } else if (ev.isEnd && ev.isComplex) {
//            val c = ev.asComplex
//            //ok. We've peeked ahead and found the end of the complex element
//            //that this sequence is the model group of.
//            val optParentRD = ctxt.immediateEnclosingElementRuntimeData
//            optParentRD match {
//              case Some(e: ElementRuntimeData) => {
//                Assert.invariant(c.runtimeData.namedQName =:= e.namedQName)
//                false
//              }
//              case _ =>
//                Assert.invariantFailed("Not end element for this sequence's containing element. Event %s, optParentRD %s.".format(
//                  ev, optParentRD))
//            }
//          } else {
//            Assert.invariantFailed("Not a start event: " + ev)
//          }
//        } else {
//          // was no element, so no unparse
//          false
//        }
//      }
//      case _ => {
//        // since only elements can be optional, anything else is non-optional
//        true
//      }
//    }
//  }
//}
//
///*
// * Temporarily these next two classes are identical, but as we break out the separator
// * functionality from being built into the child unparsers, these will diverge.
// * The unseparated variant will stay like it is, but the separated one should be
// * distinct.
// */
//class OrderedUnseparatedSequenceUnparser(ctxt: SequenceRuntimeData, childUnparsers: Seq[SequenceChildUnparser])
//  extends OrderedSequenceUnparserBase(ctxt, childUnparsers) {
//
//  override lazy val runtimeDependencies: Seq[Evaluatable[AnyRef]] = Nil
//
//  protected def unparseChildren(start: UState): Unit = {
//
//    var index = 0
//    var doUnparser = false
//    val limit = childUnparsers.length
//    while (index < limit) {
//      val childUnparser = childUnparsers(index)
//      val childRD = childUnparser.context
//
//      doUnparser = shouldDoUnparser(childRD, start)
//
//      if (doUnparser) {
//        childUnparser.unparse1(start)
//      }
//      index += 1
//      //
//      // Note: the invariant is that unparsers move over 1 within their group themselves
//      // we do not do the moving over here as we are the caller of the unparser.
//      //
//    }
//  }
//}
//
//trait Repeating { self: SequenceChildUnparser => }
//trait Scalar { self: SequenceChildUnparser => }
//
//abstract class SequenceChildUnparser(
//  val srd: SequenceRuntimeData,
//  val trd: TermRuntimeData,
//  override val childProcessors: Seq[Unparser])
//  extends CombinatorUnparser(trd) {
//  override def unparse(state: UState): Unit = ???
//  override def runtimeDependencies = Nil
//}
//
//abstract class UnseparatedSequenceChildUnparser(
//  srd: SequenceRuntimeData,
//  trd: TermRuntimeData,
//  childUnparsers: Seq[Unparser])
//  extends SequenceChildUnparser(srd, trd, childUnparsers)
//
//class ScalarOrderedRequiredUnseparatedSequenceChildUnparser(
//  childUnparser: Unparser,
//  srd: SequenceRuntimeData,
//  trd: TermRuntimeData)
//  extends UnseparatedSequenceChildUnparser(srd, trd, Seq(childUnparser)) with Scalar
//
//class RepOrderedExactlyNUnseparatedSequenceChildUnparser(
//  childUnparser: Unparser,
//  srd: SequenceRuntimeData,
//  erd: ElementRuntimeData)
//  extends UnseparatedSequenceChildUnparser(srd, erd, Seq(childUnparser)) with Repeating
//
//class RepOrderedExactlyTotalOccursCountUnseparatedSequenceChildUnparser(
//  childUnparser: Unparser,
//  ocUnparser: Unparser,
//  srd: SequenceRuntimeData,
//  erd: ElementRuntimeData)
//  extends UnseparatedSequenceChildUnparser(srd, erd, Seq(childUnparser)) with Repeating
//
//class RepOrderedWithMinMaxUnseparatedSequenceChildUnparser(
//  childUnparser: Unparser,
//  srd: SequenceRuntimeData,
//  erd: ElementRuntimeData)
//  extends UnseparatedSequenceChildUnparser(srd, erd, Seq(childUnparser)) with Repeating
//
//class OrderedSeparatedSequenceUnparser(ctxt: SequenceRuntimeData,
//  spos: SeparatorPosition,
//  sep: Unparser,
//  childUnparsers: Seq[SequenceChildUnparser])
//  extends OrderedSequenceUnparserBase(ctxt, childUnparsers :+ sep) {
//
//  override lazy val runtimeDependencies = Nil
//
//  protected def unparseChildren(start: UState): Unit = {
//
//    var prefixDone = false
//    var atLeastOneUnparsed = false
//
//    var index = 0
//    var doUnparser = false
//
//    val limit = childUnparsers.length
//    while (index < limit) {
//      val child = childUnparsers(index)
//      val trd = child.trd
//      val childUnparser = child
//      val childRD = childUnparser.context
//      doUnparser = shouldDoUnparser(childRD, start)
//
//      if (doUnparser) {
//        // unparse prefix sep if any
//        if ((spos eq SeparatorPosition.Prefix) && !prefixDone && trd.isRepresented) {
//          prefixDone = true
//          sep.unparse1(start)
//        }
//        // except for the first position of the group, unparse an infix separator
//        if (index != 0 && trd.isRequiredScalar && trd.isRepresented) {
//          sep.unparse1(start)
//        }
//        childUnparser.unparse1(start)
//        if (trd.isRepresented)
//          atLeastOneUnparsed = true
//      }
//      index += 1
//      if (index == limit && (spos eq SeparatorPosition.Postfix) && atLeastOneUnparsed) {
//        sep.unparse1(start)
//      }
//      //
//      // Note: the invariant is that unparsers move over 1 within their group themselves
//      // we do not do the moving over here as we are the caller of the unparser.
//      //
//    }
//  }
//}
//
//abstract class SeparatedSequenceChildUnparser(
//  srd: SequenceRuntimeData,
//  trd: TermRuntimeData,
//  childUnparsers: Seq[Unparser],
//  val sep: Unparser,
//  val spos: SeparatorPosition,
//  val ssp: SeparatorSuppressionPolicy)
//  extends SequenceChildUnparser(srd, trd, childUnparsers :+ sep)
//
//class ScalarOrderedRequiredSeparatedSequenceChildUnparser(
//  childUnparser: Unparser,
//  srd: SequenceRuntimeData,
//  trd: TermRuntimeData,
//  sep: Unparser,
//  spos: SeparatorPosition,
//  ssp: SeparatorSuppressionPolicy)
//  extends SeparatedSequenceChildUnparser(srd, trd, Seq(childUnparser), sep, spos, ssp) with Scalar
//
//class RepOrderedExactlyNSequenceSeparatedChildUnparser(
//  childUnparser: Unparser,
//  srd: SequenceRuntimeData,
//  erd: ElementRuntimeData,
//  sep: Unparser,
//  spos: SeparatorPosition,
//  ssp: SeparatorSuppressionPolicy)
//  extends SeparatedSequenceChildUnparser(srd, erd, Seq(childUnparser), sep, spos, ssp) with Repeating
//
//class RepOrderedExactlyTotalOccursCountSeparatedSequenceChildUnparser(
//  childUnparser: Unparser,
//  ocUnparser: Unparser,
//  srd: SequenceRuntimeData,
//  erd: ElementRuntimeData,
//  sep: Unparser,
//  spos: SeparatorPosition,
//  ssp: SeparatorSuppressionPolicy)
//  extends SeparatedSequenceChildUnparser(srd, erd, Seq(childUnparser), sep, spos, ssp) with Repeating
//
//class RepOrderedWithMinMaxSeparatedSequenceChildUnparser(
//  childUnparser: Unparser,
//  srd: SequenceRuntimeData,
//  erd: ElementRuntimeData,
//  sep: Unparser,
//  spos: SeparatorPosition,
//  ssp: SeparatorSuppressionPolicy)
//  extends SeparatedSequenceChildUnparser(srd, erd, Seq(childUnparser), sep, spos, ssp) with Repeating
//
