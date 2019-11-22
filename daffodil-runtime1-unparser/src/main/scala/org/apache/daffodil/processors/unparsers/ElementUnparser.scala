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
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.DIComplex
import org.apache.daffodil.dpath.SuspendableExpression
import org.apache.daffodil.processors.UnparseTargetLengthInBitsEv
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.infoset.RetryableException
import org.apache.daffodil.processors.TypeCalculator
import org.apache.daffodil.util.MaybeBoolean

/**
 * Elements that, when unparsing, have no length specified.
 *
 * That is, lengtKind delimited, pattern, and implicit(for complexTypes)
 */
class ElementUnspecifiedLengthUnparser(
  erd: ElementRuntimeData,
  setVarUnparsers: Array[Unparser],
  eBeforeUnparser: Maybe[Unparser],
  eUnparser: Maybe[Unparser],
  eAfterUnparser: Maybe[Unparser],
  eReptypeUnparser: Maybe[Unparser])
  extends ElementUnparserBase(
    erd,
    setVarUnparsers,
    eBeforeUnparser,
    eUnparser,
    eAfterUnparser,
    eReptypeUnparser)
  with RegularElementUnparserStartEndStrategy
  with RepMoveMixin {

  override lazy val runtimeDependencies = Vector()

}

sealed trait RepMoveMixin {
  def move(start: UState) {
    val childIndex = start.childIndexStack.pop()
    start.childIndexStack.push(childIndex + 1)
  }
}

/**
 * The unparser used for an element that has inputValueCalc.
 *
 * The only thing we do is move over one child element, because the
 * inputValueCalc element does take up one child element position.
 * However, not in the group - because that is what is used to decide
 * whether to place separators, and there should not be any separator
 * corresponding to an IVC element.
 */
class ElementUnparserNoRep(
  erd: ElementRuntimeData,
  setVarUnparsers: Array[Unparser])
  extends ElementUnparserBase(
    erd,
    setVarUnparsers,
    Nope,
    Nope,
    Nope,
    Nope)
  with RegularElementUnparserStartEndStrategy {

  override lazy val runtimeDependencies = Vector()

  /**
   * Move over in the element children, but not in the group.
   * This avoids separators for this IVC element.
   */
  override def move(state: UState) {
    val childIndex = state.childIndexStack.pop()
    state.childIndexStack.push(childIndex + 1)
  }
}

class ElementOVCUnspecifiedLengthUnparser(
  erd: ElementRuntimeData,
  setVarUnparsers: Array[Unparser],
  eBeforeUnparser: Maybe[Unparser],
  eUnparser: Maybe[Unparser],
  eAfterUnparser: Maybe[Unparser])
  extends ElementUnparserBase(
    erd,
    setVarUnparsers,
    eBeforeUnparser,
    eUnparser,
    eAfterUnparser,
    Nope)
  with OVCStartEndStrategy
  with RepMoveMixin {

  override lazy val runtimeDependencies = Vector()

}

/**
 * Base class for unparsing elements
 *
 * Depends on use of separate unparsers for the padding/fill regions which
 * calculate their own sizes, generally after the length of the value region
 * has been determined.
 */
sealed abstract class ElementUnparserBase(
  val erd: ElementRuntimeData,
  val setVarUnparsers: Array[Unparser],
  val eBeforeUnparser: Maybe[Unparser],
  val eUnparser: Maybe[Unparser],
  val eAfterUnparser: Maybe[Unparser],
  val eReptypeUnparser: Maybe[Unparser])
  extends CombinatorUnparser(erd)
  with RepMoveMixin
  with ElementUnparserStartEndStrategy {

  final override lazy val childProcessors =
    (eBeforeUnparser.toList ++ eUnparser.toList ++ eAfterUnparser.toList ++ eReptypeUnparser.toList ++ setVarUnparsers.toList).toVector

  private val name = erd.name

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Element name='" + name + "'>" +
        (if (eBeforeUnparser.isDefined) eBeforeUnparser.value.toBriefXML(depthLimit - 1) else "") +
        (if (eReptypeUnparser.isDefined) eReptypeUnparser.value.toBriefXML(depthLimit - 1) else "") +
        (if (eUnparser.isDefined) eUnparser.value.toBriefXML(depthLimit - 1) else "") +
        (if (eAfterUnparser.isDefined) eAfterUnparser.value.toBriefXML(depthLimit - 1) else "") +
        setVarUnparsers.map { _.toBriefXML(depthLimit - 1) }.mkString +
        "</Element>"
  }

  final def computeSetVariables(state: UState) {
    // variable assignment. Always after the element value, but
    // also still with this element itself as the context, so before
    // we end element.
    {
      var i = 0;
      while (i < setVarUnparsers.length) {
        setVarUnparsers(i).unparse1(state)
        i += 1
      }
    }
  }

  protected def doBeforeContentUnparser(state: UState) {
    if (eBeforeUnparser.isDefined)
      eBeforeUnparser.get.unparse1(state)
  }

  protected def doAfterContentUnparser(state: UState) {
    if (eAfterUnparser.isDefined)
      eAfterUnparser.get.unparse1(state)
  }

  protected def runContentUnparser(state: UState) {
    if (eReptypeUnparser.isDefined) {
      eReptypeUnparser.get.unparse1(state)
    } else if (eUnparser.isDefined)
      eUnparser.get.unparse1(state)
  }

  override def unparse(state: UState): Unit = {

    if (state.dataProc.isDefined) state.dataProc.value.startElement(state, this)

    unparseBegin(state)

    captureRuntimeValuedExpressionValues(state)

    doBeforeContentUnparser(state)

    //
    // We must push the TermRuntimeData for all model-groups.
    // The starting point for this is the model-group of a complex type.
    // Simple types don't have model groups, so no pushing those.
    //
    if (erd.isComplexType)
      state.pushTRD(erd.optComplexTypeModelGroupRuntimeData.get)

    runContentUnparser(state)

    if (erd.isComplexType)
      state.popTRD(erd.optComplexTypeModelGroupRuntimeData.get)

    doAfterContentUnparser(state)

    computeSetVariables(state)

    unparseEnd(state)

    if (state.dataProc.isDefined) state.dataProc.value.endElement(state, this)

  }

  def validate(state: UState): Unit = {
    ??? // TODO: JIRA DFDL-1582 - Is the ticket for implementing the Unparser - validation feature

    //    val currentElement = state.thisElement
    //
    //    if (currentElement.valid.isDefined) { return }
    //
    //    val resultState = DFDLCheckConstraintsFunction.validate(state) match {
    //      case Right(boolVal) => {
    //        log(LogLevel.Debug, "Validation succeeded for %s", currentElement.toXML())
    //        currentElement.setValid(true)
    //      }
    //      case Left(failureMessage) => {
    //        log(LogLevel.Debug,
    //          "Validation failed for %s due to %s. The element value was %s.",
    //          context.toString, failureMessage, currentElement.toXML())
    //        state.reportValidationError("%s failed facet checks due to: %s",
    //          context.toString, failureMessage)
    //        currentElement.setValid(false)
    //      }
    //    }
  }
}

trait ElementSpecifiedLengthMixin {

  protected def maybeTargetLengthEv: Maybe[UnparseTargetLengthInBitsEv]
  protected def erd: ElementRuntimeData

  /**
   * This is a maybeTLOp so that this base class can be used to handle
   * data types that do not have specified length as well.
   *
   * An example is lengthKind 'pattern' which while not "specified" length,
   * uses this same code path, just there is no possibility of pad/fill regions.
   *
   * It's a degenerate case of specified length.
   *
   * Note: thread safety: This must be def, not val/lazyval because TargetLengthOperation is
   * a stateful class instance, so cannot be a static member of an unparser
   * object (unparsers are shared by multiple threads. Suspensions cannot be.)
   */
  //  private def maybeTLOp = {
  //    val mtlop = if (maybeTargetLengthEv.isDefined)
  //      One(new TargetLengthOperation(erd, maybeTargetLengthEv.get))
  //    else
  //      Nope
  //    mtlop
  //  }

  protected def computeTargetLength(state: UState) {
    if (maybeTargetLengthEv.isDefined) {
      val tlEv = maybeTargetLengthEv.get
      if (tlEv.isConstant) {
        // bypass creating a suspension when we know the target length is constant
        // do nothing
      } else {
        // it is an expression. It might suspend.
        val op = new TargetLengthOperation(erd, tlEv)
        op.run(state)
      }
    }
  }
}

/**
 * For regular (not dfdl:outputValueCalc) elements.
 */
class ElementSpecifiedLengthUnparser(
  context: ElementRuntimeData,
  override val maybeTargetLengthEv: Maybe[UnparseTargetLengthInBitsEv],
  setVarUnparsers: Array[Unparser],
  eBeforeUnparser: Maybe[Unparser],
  eUnparser: Maybe[Unparser],
  eAfterUnparser: Maybe[Unparser],
  eReptypeUnparser: Maybe[Unparser])
  extends ElementUnparserBase(
    context,
    setVarUnparsers,
    eBeforeUnparser,
    eUnparser,
    eAfterUnparser,
    eReptypeUnparser)
  with RegularElementUnparserStartEndStrategy
  with ElementSpecifiedLengthMixin {

  override lazy val runtimeDependencies = maybeTargetLengthEv.toList.toVector

  override def runContentUnparser(state: UState) {
    computeTargetLength(state) // must happen before run() so that we can take advantage of knowing the length
    super.runContentUnparser(state) // setup unparsing, which will block for no valu
  }

}

/**
 * For dfdl:outputValueCalc elements.
 */
class ElementOVCSpecifiedLengthUnparserSuspendableExpresion(
  callingUnparser: ElementOVCSpecifiedLengthUnparser)
  extends SuspendableExpression {

  override def rd = callingUnparser.erd

  override lazy val expr = rd.outputValueCalcExpr.get

  override final protected def processExpressionResult(state: UState, v: AnyRef) {
    val diSimple = state.currentInfosetNode.asSimple

    diSimple.setDataValue(v)

    //
    // These are now done in the main unparse, but they will
    // suspend if they cannot be evaluated because there is not data value yet.
    //
    // callingUnparser.computeSetVariables(state)
  }

  override protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = MaybeULong(0L)

}

class ElementOVCSpecifiedLengthUnparser(
  context: ElementRuntimeData,
  override val maybeTargetLengthEv: Maybe[UnparseTargetLengthInBitsEv],
  setVarUnparsers: Array[Unparser],
  eBeforeUnparser: Maybe[Unparser],
  eUnparser: Maybe[Unparser],
  eAfterUnparser: Maybe[Unparser])
  extends ElementUnparserBase(
    context,
    setVarUnparsers,
    eBeforeUnparser,
    eUnparser,
    eAfterUnparser,
    Nope)
  with OVCStartEndStrategy
  with ElementSpecifiedLengthMixin {

  override lazy val runtimeDependencies = maybeTargetLengthEv.toList.toVector

  private def suspendableExpression =
    new ElementOVCSpecifiedLengthUnparserSuspendableExpresion(this)

  Assert.invariant(context.outputValueCalcExpr.isDefined)

  override def runContentUnparser(state: UState) {
    computeTargetLength(state) // must happen before run() so that we can take advantage of knowing the length
    suspendableExpression.run(state) // run the expression. It might or might not have a value.
    super.runContentUnparser(state) // setup unparsing, which will block for no valu
  }

}

/**
 * specifies the way the element will consume infoset events,
 */
sealed trait ElementUnparserStartEndStrategy {
  /**
   * Consumes the required infoset events and changes context so that the
   * element's DIElement node is the context element.
   */
  protected def unparseBegin(state: UState): Unit

  /**
   * Restores prior context. Consumes end-element event.
   */
  protected def unparseEnd(state: UState): Unit

  protected def captureRuntimeValuedExpressionValues(ustate: UState): Unit

  protected def move(start: UState)

  protected def erd: ElementRuntimeData

  def runtimeDependencies: Vector[Evaluatable[AnyRef]]
}

sealed trait RegularElementUnparserStartEndStrategy
  extends ElementUnparserStartEndStrategy {
  /**
   * Consumes the required infoset events and changes context so that the
   * element's DIElement node is the context element.
   */
  final override protected def unparseBegin(state: UState): Unit = {
    if (erd.isQuasiElement) {
      //Quasi elements are used for TypeValueCalc, and have no corresponding events in the infoset inputter
      //The parent parser will push a DIElement for us to consume containing the logical value, so we do
      //not need to do so here
      Assert.invariant(state.currentInfosetNode.isSimple)
      Assert.invariant(state.currentInfosetNode.asSimple.erd eq erd)
      ()
    } else {
      val elem =
        if (!erd.isHidden) {
          // Hidden elements are not in the infoset, so we will never get an event
          // for them. Only try to consume start events for non-hidden elements
          val event = state.advanceOrError
          if (!event.isStart || event.erd != erd) {
            // it's not a start element event, or it's a start element event, but for a different element.
            // this indicates that the incoming infoset (as events) doesn't match the schema
            UnparseError(Nope, One(state.currentLocation), "Expected element start event for %s, but received %s.",
              erd.namedQName.toExtendedSyntax, event)
          }
          val res = event.info.element
          val mCurNode = state.currentInfosetNodeMaybe
          if (mCurNode.isDefined) {
            val c = mCurNode.get.asComplex
            Assert.invariant(!c.isFinal)
            if (c.maybeIsNilled == MaybeBoolean.True) {
              // cannot add content to a nilled complex element
              UnparseError(One(erd.schemaFileLocation), Nope, "Nilled complex element %s has content from %s",
                c.erd.namedQName.toExtendedSyntax,
                res.erd.namedQName.toExtendedSyntax)
            }
            c.addChild(res)
          } else {
            val doc = state.documentElement
            doc.addChild(res) // DIDocument, which is never a current node, must have the child added
            doc.setFinal() // that's the only child.
          }
          res
        } else {
          Assert.invariant(erd.isHidden)
          // Since we never get events for hidden elements, their infoset elements
          // will have never been created. This means we need to manually create them
          val e = if (erd.isComplexType) new DIComplex(erd) else new DISimple(erd)
          state.currentInfosetNode.asComplex.addChild(e)
          e
        }

      // When the infoset events are being advanced, the currentInfosetNodeStack
      // is pushing and popping to match the events. This provides the proper
      // context for evaluation of expressions.
      val e = One(elem)
      state.currentInfosetNodeStack.push(e)
    }
  }

  /**
   * Restores prior context. Consumes end-element event.
   */
  final override protected def unparseEnd(state: UState): Unit = {
    if (erd.isQuasiElement) {
      //Quasi elements are used for TypeValueCalc, and have no corresponding events in the infoset inputter
      //The parent parser will handle pushing and poping the Infoset, so we do not need to do anything here.
      Assert.invariant(state.currentInfosetNode.isSimple)
      Assert.invariant(state.currentInfosetNode.asSimple.erd eq erd)
      ()
    } else {
      if (!erd.isHidden) {
        // Hidden elements are not in the infoset, so we will never get an event
        // for them. Only try to consume end events for non-hidden elements
        val event = state.advanceOrError
        if (!event.isEnd || event.erd != erd) {
          // it's not an end-element event, or it's an end element event, but for a different element.
          // this indicates that the incoming infoset (as events) doesn't match the schema
          UnparseError(Nope, One(state.currentLocation), "Expected element end event for %s, but received %s.",
            erd.namedQName.toExtendedSyntax, event)
        }
      }
      val cur = state.currentInfosetNode
      if (cur.isComplex)
        cur.asComplex.setFinal()
      state.currentInfosetNodeStack.pop

      move(state)
    }
  }

  final override protected def captureRuntimeValuedExpressionValues(ustate: UState): Unit = {}

}

trait OVCStartEndStrategy
  extends ElementUnparserStartEndStrategy {

  /**
   * For OVC, the behavior w.r.t. consuming infoset events is different.
   */
  protected final override def unparseBegin(state: UState) {
    val elem =
      if (!erd.isHidden) {
        // outputValueCalc elements are optional in the infoset. If the next event
        // is for this OVC element, then consume the start/end events.
        // Otherwise, the next event is for a following element, and we do not want
        // to consume it. Don't even bother checking all this if it's hidden. It
        // definitely won't be in the infoset in that case.
        val eventMaybe = state.inspectMaybe
        if (eventMaybe.isDefined && eventMaybe.get.erd == erd) {
          // Event existed for this OVC element, should be a start and end events
          val startEv = state.advanceOrError // Consume the start event
          Assert.invariant(startEv.isStart && startEv.erd == erd)
          val endEv = state.advanceOrError // Consume the end event
          Assert.invariant(endEv.isEnd && endEv.erd == erd)

          val e = new DISimple(erd)
          state.currentInfosetNode.asComplex.addChild(e)
          // Remove any state that was set by what created this event. Later
          // code asserts that OVC elements do not have a value
          e.resetValue
          e
        } else {
          // Event was optional and didn't exist, create a new InfosetElement and add it
          val e = new DISimple(erd)
          state.currentInfosetNode.asComplex.addChild(e)
          e
        }
      } else {
        // Event was hidden and will never exist, create a new InfosetElement and add it
        val e = new DISimple(erd)
        state.currentInfosetNode.asComplex.addChild(e)
        e
      }

    val e = One(elem)
    state.currentInfosetNodeStack.push(e)
  }

  protected final override def unparseEnd(state: UState) {
    state.currentInfosetNodeStack.pop

    // if an OVC element existed, the start AND end events were consumed in
    // unparseBegin. No need to advance the cursor here.

    move(state)
  }

  // For OVC, or for a target length expression,
  //
  // If we delayed evaluating the expressions, some variables might not be read
  // until we come back to retrying the suspension. Those variables might
  // be set downstream. That set would be illegal (set after read), but we
  // would not detect it, because we would not have read it at the point where
  // the element was first encountered.
  //
  // This problem would presumably be caught if the same schema was being used
  // to parse data.
  //
  // Note that we can't try to be clever and just evaluate all the variables
  // in the expressions... because expressions can have conditional branches
  // so we can't tell which variables will be read if they're in the arms of
  // conditionals.
  //
  // For runtime-valued properties, the expressions cannot block, so we can evaluate
  // them at the time we first encounter the element. We could, at that point
  // remember them, or we could just re-evaluate them again later when their
  // values are actually needed. Variables that are read will have the right
  // state.
  //
  // So caching the result values of the runtime-valued properties is allowed,
  // but not required. Really we just need the side-effects of the expressions
  // on reading of variables.
  //
  // Additionally, even if the target length is computed without suspending, some other
  // aspect of the content might be suspended (something as simple as
  // a mandatory text alignment, for example)
  //
  // This can cause value lengths/content lengths for many things to be
  // non computable. That can cause arbitrary unparsing to block. That unparsing
  // might need say, properties like those for controlling text number format,
  // many of which are runtime-valued.
  //
  // Those expressions might reference variables.
  //
  // Only runtime-valued properties relevant to this element are needed.

  // TODO: Performance: Ideas: maybe each unparse should sport a captureRuntimeValueProperties, and
  // similarly each Ev. Then suspend could capture exactly and only what is
  // needed. (Similarly, for parts of the UState that need to be saved, each
  // unparser could have specific methods for capturing that information and
  // caching it on the Infoset node. (E.g., delimiter stack - but only when
  // delimiters are relevant instead of every time as we do now.)
  //
  // Right now we pessimistically clone the entire DOS, the entire UState
  // (though we share the variable map) including various stacks. (We need the whole
  // delimiter stack, but probably could get a way with only top of stack for
  // many other things.

  final override protected def captureRuntimeValuedExpressionValues(state: UState) {
    //
    // Forces the evaluation of runtime-valued things, and this will cause those
    // that actually are runtime-expressions to be cached on the infoset element.
    //
    // Then later when the actual unparse occurs, these will be accessed off the
    // infoset element's cache.
    //
    // So we have to do this here in order to Freeze the state of these
    // evaluations on the Infoset at the time this unparse call happens.

    runtimeDependencies.foreach { dep =>
      try {
        dep.evaluate(state) // these evaluations will force dependencies of the dependencies. So we just do 1 tier, not a tree walk.
      } catch {
        case _: RetryableException => ()
      }
    }
  }
}
