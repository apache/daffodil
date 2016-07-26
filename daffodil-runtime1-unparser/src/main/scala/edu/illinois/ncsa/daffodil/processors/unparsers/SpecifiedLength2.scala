package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.MaybeChar
import edu.illinois.ncsa.daffodil.processors.UnparseTargetLengthInBitsEv
import edu.illinois.ncsa.daffodil.processors.SuspendableOperation
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.processors.FillByteEv
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.dpath.SuspendableExpression
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.processors.RetryableException
import edu.illinois.ncsa.daffodil.util.Misc

abstract class SpecifiedLengthUnparserBase2(
  override val context: ElementRuntimeData,
  maybeTargetLengthEv: Maybe[UnparseTargetLengthInBitsEv])
    extends PrimUnparser {

  def erd = context

  final override lazy val childProcessors =
    valueUnparsers.toList ++
      setVarUnparsers.toList ++
      beforeContentUnparsers.toList
  //    beforeValueUnparsers.toList ++
  //    valueUnparsers.toList ++
  //    afterValueUnparsers.toList ++
  //    afterContentUnparsers.toList

  protected def beforeContentUnparsers: Array[Unparser]
  //  protected def beforeValueUnparsers: Array[Unparser]
  protected def valueUnparsers: Array[Unparser]
  //  protected def afterValueUnparsers: Array[Unparser]
  //  protected def afterContentUnparsers: Array[Unparser]
  protected def setVarUnparsers: Array[Unparser]

  private val maybeTLOp =
    if (maybeTargetLengthEv.isDefined)
      One(new TLOp(context, maybeTargetLengthEv.get))
    else
      Nope

  protected def computeTargetLength(state: UState) {
    if (maybeTLOp.isDefined)
      maybeTLOp.get.run(state);
  }

  protected def computeSetVariables(state: UState) {
    // variable assignment. Always after the element value, but
    // also still with this element itself as the context, so before
    // we end element.
    {
      var i = 0;
      while (i < setVarUnparsers.length) {
        setVarUnparsers(i).unparse1(state, context)
        i += 1
      }
    }
  }

  protected def doBeforeContentUnparsers(state: UState) {
    {
      var i = 0;
      while (i < beforeContentUnparsers.length) {
        beforeContentUnparsers(i).unparse1(state, context)
        i += 1
      }
    }
  }

  protected def runValueUnparsers(state: UState) {
    //
    // value
    //
    {
      var i = 0
      while (i < valueUnparsers.length) {
        valueUnparsers(i).unparse1(state, context)
        i += 1
      }
    }
  }

  protected def captureRuntimeValuedExpressionValues(state: UState) {
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

  override def unparse(state: UState): Unit = {

    startElement(state)

    captureRuntimeValuedExpressionValues(state)

    doBeforeContentUnparsers(state)

    computeTargetLength(state)

    runValueUnparsers(state)

    computeSetVariables(state)

    endElement(state)
  }

  protected def move(start: UState) {
    val grIndex = start.groupIndexStack.pop()
    start.groupIndexStack.push(grIndex + 1)
    val childIndex = start.childIndexStack.pop()
    start.childIndexStack.push(childIndex + 1)
  }

  protected def startElement(state: UState): Unit = {
    val elem =
      if (!erd.isHidden) {
        // Hidden elements are not in the infoset, so we will never get an event
        // for them. Only try to consume start events for non-hidden elements
        val event = state.advanceOrError
        if (!event.isStart) { //  || event.erd != erd) {
          UnparseError(Nope, One(state.currentLocation), "Expected element start event for %s, but received %s.", erd.namedQName, event)
        }
        event.asElement
      } else {
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
    state.aaa_currentNode = e
  }

  protected def endElement(state: UState): Unit = {
    if (!erd.isHidden) {
      // Hidden elements are not in the infoset, so we will never get an event
      // for them. Only try to consume end events for non-hidden elements
      val event = state.advanceOrError
      if (!event.isEnd) { //  || event.erd != erd) {
        UnparseError(Nope, One(state.currentLocation), "Expected element end event for %s, but received %s.", erd.namedQName, event)
      }
    }

    state.currentInfosetNodeStack.pop
    state.aaa_currentNode =
      if (state.currentInfosetNodeStack.isEmpty)
        Nope
      else
        state.currentInfosetNodeStack.top

    move(state)
  }
}

class CaptureStartOfContentLengthUnparser(override val context: ElementRuntimeData)
    extends PrimUnparserObject(context) {

  override def unparse(state: UState) {
    val dos = state.dataOutputStream
    val elem = state.currentInfosetNode.asInstanceOf[DIElement]
    if (dos.maybeAbsBitPos0b.isDefined) {
      elem.contentLength.setAbsStartPos0bInBits(dos.maybeAbsBitPos0b.getULong)
    } else {
      elem.contentLength.setRelStartPos0bInBits(dos.relBitPos0b, dos)
    }
  }
}

class CaptureEndOfContentLengthUnparser(override val context: ElementRuntimeData)
    extends PrimUnparserObject(context) {

  override def unparse(state: UState) {
    val dos = state.dataOutputStream
    val elem = state.currentInfosetNode.asInstanceOf[DIElement]
    if (dos.maybeAbsBitPos0b.isDefined) {
      elem.contentLength.setAbsEndPos0bInBits(dos.maybeAbsBitPos0b.getULong)
    } else {
      elem.contentLength.setRelEndPos0bInBits(dos.relBitPos0b, dos)
    }
  }
}

class CaptureStartOfValueLengthUnparser(override val context: ElementRuntimeData)
    extends PrimUnparserObject(context) {

  override def unparse(state: UState) {
    val dos = state.dataOutputStream
    val elem = state.currentInfosetNode.asInstanceOf[DIElement]
    if (dos.maybeAbsBitPos0b.isDefined) {
      elem.valueLength.setAbsStartPos0bInBits(dos.maybeAbsBitPos0b.getULong)
    } else {
      elem.valueLength.setRelStartPos0bInBits(dos.relBitPos0b, dos)
    }
  }
}

class CaptureEndOfValueLengthUnparser(override val context: ElementRuntimeData)
    extends PrimUnparserObject(context) {

  override def unparse(state: UState) {
    val dos = state.dataOutputStream
    val elem = state.currentInfosetNode.asInstanceOf[DIElement]
    if (dos.maybeAbsBitPos0b.isDefined) {
      elem.valueLength.setAbsEndPos0bInBits(dos.maybeAbsBitPos0b.getULong)
    } else {
      elem.valueLength.setRelEndPos0bInBits(dos.relBitPos0b, dos)
    }
  }
}

class ElementSpecifiedLengthUnparser(
  override val context: ElementRuntimeData,
  maybeTargetLengthEv: Maybe[UnparseTargetLengthInBitsEv],
  override val setVarUnparsers: Array[Unparser],
  override val beforeContentUnparsers: Array[Unparser],
  //  override val beforeValueUnparsers: Array[Unparser],
  override val valueUnparsers: Array[Unparser] //  override val afterValueUnparsers: Array[Unparser],
  //  override val afterContentUnparsers: Array[Unparser],
  //  eElementUnusedUnparser: Maybe[Unparser]
  )
    extends SpecifiedLengthUnparserBase2(context, maybeTargetLengthEv) {

  override val runtimeDependencies = maybeTargetLengthEv.toList

}

class ElementOVCSpecifiedLengthUnparser(
  override val context: ElementRuntimeData,
  maybeTargetLengthEv: Maybe[UnparseTargetLengthInBitsEv],
  override val setVarUnparsers: Array[Unparser],
  override val beforeContentUnparsers: Array[Unparser],
  //  override val beforeValueUnparsers: Array[Unparser],
  override val valueUnparsers: Array[Unparser] //  override val afterValueUnparsers: Array[Unparser],
  //  override val afterContentUnparsers: Array[Unparser],
  //  eElementUnusedUnparser: Maybe[Unparser]
  ) extends SpecifiedLengthUnparserBase2(context, maybeTargetLengthEv)
    with SuspendableExpression {

  override val runtimeDependencies = maybeTargetLengthEv.toList

  override def rd = context

  override final protected def processExpressionResult(state: UState, v: AnyRef) {
    val diSimple = state.currentInfosetNode.asSimple
    // note. This got attached to infoset in StatementElementOutputValueCalcUnparser

    diSimple.setDataValue(v)
    //
    // now we have to unparse the value.
    //
    runValueUnparsers(state)

    computeSetVariables(state)
  }

  Assert.invariant(context.outputValueCalcExpr.isDefined)
  override lazy val expr = context.outputValueCalcExpr.get

  override def unparse(state: UState): Unit = {

    startElement(state)

    captureRuntimeValuedExpressionValues(state)

    doBeforeContentUnparsers(state)

    computeTargetLength(state) // must happen before run() so that we can take advantage of knowing the length

    run(state)

    endElement(state)
  }

  override protected def maybeKnownLengthInBits(state: UState): MaybeULong = {
    if (maybeTargetLengthEv.isDefined) {
      val tlEv = maybeTargetLengthEv.get
      val maybeTL = tlEv.evaluate(state)
      if (maybeTL.isDefined) {
        val tl = maybeTL.get
        MaybeULong(tl)
      } else {
        MaybeULong.Nope
      }
    } else {
      MaybeULong.Nope
    }
  }

  override def startElement(state: UState) {
    val elem =
      if (!erd.isHidden) {
        // outputValueCalc elements are optional in the infoset. If the next event
        // is for this is for this OVC element, then consume the start/end events.
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

          val e = startEv.asSimple
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
    state.aaa_currentNode = e
  }

  override def endElement(state: UState) {
    state.currentInfosetNodeStack.pop

    // if an OVC element existed, the start AND end events were consumed in
    // startElement. No need to advance the cursor here.
    state.aaa_currentNode =
      if (state.currentInfosetNodeStack.isEmpty)
        Nope
      else
        state.currentInfosetNodeStack.top

    move(state)
  }
}

//class ComplexElementSpecifiedLengthUnparser(
//  override val context: ElementRuntimeData,
//  targetLengthEv: UnparseTargetLengthInBitsEv,
//  eUnparser: Maybe[Unparser],
//  eElementUnusedUnparser: Maybe[Unparser])
//    extends {
//      override val beforeValueUnparsers: Array[Unparser] = Nil.toArray
//
//      override val valueUnparsers = (eUnparser.toList).toArray
//
//      override val afterValueUnparsers = (eElementUnusedUnparser.toList).toArray
//
//      override val runtimeDependencies = List(targetLengthEv)
//
//    } with SpecifiedLengthUnparserBase2(context, targetLengthEv) {
//
//}

//class HexBinarySpecifiedLengthUnparser(
//  override val context: ElementRuntimeData,
//  targetLengthEv: UnparseTargetLengthInBitsEv,
//  eUnparser: Maybe[Unparser],
//  eRightFillUnparser: Maybe[Unparser])
//    extends {
//      override val beforeValueUnparsers: Array[Unparser] = Nil.toArray
//
//      override val valueUnparsers = (eUnparser.toList).toArray
//
//      override val afterValueUnparsers = (eRightFillUnparser.toList).toArray
//
//      override val runtimeDependencies = List(targetLengthEv)
//
//    } with SpecifiedLengthUnparserBase2(context, targetLengthEv) {
//
//}
//
//class SimpleBinarySpecifiedLengthUnparser(
//  override val context: ElementRuntimeData,
//  targetLengthEv: UnparseTargetLengthInBitsEv,
//  eUnparser: Maybe[Unparser])
//    extends {
//      override val beforeValueUnparsers: Array[Unparser] = Nil.toArray
//
//      override val valueUnparsers = (eUnparser.toList).toArray
//
//      override val afterValueUnparsers: Array[Unparser] = Nil.toArray
//
//      override val runtimeDependencies = List(targetLengthEv)
//
//    } with SpecifiedLengthUnparserBase2(context, targetLengthEv) {
//
//}
//
//class SimpleTextSpecifiedLengthUnparser(
//  context: ElementRuntimeData,
//  targetLengthEv: UnparseTargetLengthInBitsEv,
//  eLeftPaddingUnparser: Maybe[Unparser],
//  eUnparser: Maybe[Unparser],
//  eRightPaddingUnparser: Maybe[Unparser],
//  eRightFillUnparser: Maybe[Unparser])
//    extends {
//
//      override val beforeValueUnparsers = (eLeftPaddingUnparser.toList).toArray
//
//      override val valueUnparsers = (eUnparser.toList).toArray
//
//      override val afterValueUnparsers = (eRightPaddingUnparser.toList ++
//        eRightFillUnparser.toList).toArray
//
//      override val runtimeDependencies = List(targetLengthEv)
//
//    } with SpecifiedLengthUnparserBase2(context, targetLengthEv)

class TLOp(override val rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv)
    extends SuspendableOperation {

  override def toString = "target length for " + rd.prettyName + " expr " + targetLengthEv.lengthInBitsEv.lengthEv.toBriefXML()

  override protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = MaybeULong(0L)

  override def test(ustate: UState): Boolean = {
    targetLengthEv.evaluate(ustate)
    true
  }

  override def continuation(state: UState) {
    // once we have evaluated the targetLengthEv, nothing else to do
    // here
  }
}

trait SuspendableUnparser extends SuspendableOperation {

  final def unparse(state: UState): Unit = {
    run(state)
  }
}

trait NeedValueAndTargetLengthMixin {

  def targetLengthEv: UnparseTargetLengthInBitsEv

  protected def test(ustate: UState): Boolean = {
    val mtl = targetLengthEv.evaluate(ustate)
    if (mtl.isEmpty) {
      val lib = targetLengthEv.lengthInBitsEv
      val lu = lib.lengthUnits
      val lk = lib.lengthKind
      val mcs = targetLengthEv.lengthInBitsEv.maybeCharsetEv
      val cs = mcs.get.evaluate(ustate)
      val csName = cs.charsetName
      ustate.SDE("For dfdl:lengthKind '%s', variable width character encoding '%s', and dfdl:lengthUnits '%s' are incompatible.", lk, csName, lu)
    } else {
      val e = ustate.currentInfosetNode.asInstanceOf[DIElement]
      val hasValueLength = e.valueLength.maybeLengthInBits().isDefined
      hasValueLength
    }
  }

  protected def getSkipBits(ustate: UState): Long = {
    val mtl = targetLengthEv.evaluate(ustate)
    val tl = mtl.get
    val e = ustate.currentInfosetNode.asInstanceOf[DIElement]
    val vl = e.valueLength.lengthInBits.longValue
    Assert.invariant(tl >= vl)
    val skipInBits = tl - vl
    skipInBits
  }

}

class ElementUnusedUnparser(
  override val rd: ElementRuntimeData,
  override val targetLengthEv: UnparseTargetLengthInBitsEv,
  fillByteEv: FillByteEv)
    extends PrimUnparserObject(rd)
    with SuspendableUnparser
    with NeedValueAndTargetLengthMixin {

  override lazy val runtimeDependencies = List(targetLengthEv, fillByteEv)

  /**
   * determine delta between value length and target length
   *
   * and skip that many bits.
   */
  override def continuation(ustate: UState) {
    val skipInBits = getSkipBits(ustate)
    skipTheBits(ustate, skipInBits)
  }

  protected final def skipTheBits(ustate: UState, skipInBits: Long) {
    if (!targetLengthEv.isConstant)
      System.err.println(Misc.getNameFromClass(this) + " filling " + skipInBits + " for " + rd.prettyName + " DOS " + ustate.dataOutputStream)
    if (skipInBits > 0) {
      val dos = ustate.dataOutputStream
      val fb = fillByteEv.evaluate(ustate)
      dos.setFillByte(fb)
      if (!dos.skip(skipInBits))
        UE(ustate, "Unable to skip %s(bits).", skipInBits)
    }
  }

}

trait PaddingUnparserMixin
    extends SuspendableUnparser
    with NeedValueAndTargetLengthMixin { self: Unparser =>

  override lazy val runtimeDependencies = List(targetLengthEv)

  protected def maybePadChar: MaybeChar

  override def test(ustate: UState): Boolean = {
    super.test(ustate) && {
      // we know there is a charset. We can't have a padChar without one
      val charsetEv = targetLengthEv.lengthInBitsEv.maybeCharsetEv.get
      charsetEv.evaluate(ustate)
      true
    }
  }

  protected def numPadChars(skipInBits: Long, charWidthInBits: Long) =
    skipInBits / charWidthInBits // discarding any fragment of a character

  protected final def charset(state: UState) =
    targetLengthEv.lengthInBitsEv.maybeCharsetEv.get.evaluate(state)

  protected final def charWidthInBits(charset: DFDLCharset) = {
    val res = charset.maybeFixedWidth.get
    res
  }

  override def continuation(state: UState) {
    val skipInBits = getSkipBits(state)
    val cs = charset(state)

    val nChars = numPadChars(skipInBits, cs.padCharWidthInBits)
    if (nChars > 0) {
      val dos = state.dataOutputStream
      // TODO: Performance - do we need to do this?
      dos.setEncoder(state.getEncoder(cs.charset))
      // TODO: Performance - do this better. No reason to be doing this one character at a time
      var i = 0
      val padChar = maybePadChar.get
      val padString = padChar.toString
      while (i < nChars) {
        if (dos.putString(padString) != 1)
          UE(state, "Unable to output %s pad characters.", nChars)
        i += 1
      }
    }
  }
}

/**
 * Doesn't matter if we're left or right padding if we're the only padding
 */
class OnlyPaddingUnparser(override val rd: ElementRuntimeData,
  override val targetLengthEv: UnparseTargetLengthInBitsEv,
  override val maybePadChar: MaybeChar)
    extends PrimUnparserObject(rd)
    with PaddingUnparserMixin

class RightCenteredPaddingUnparser(rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  maybePadChar: MaybeChar)
    extends OnlyPaddingUnparser(rd, targetLengthEv, maybePadChar) {

  override def numPadChars(skipInBits: Long, charWidthInBits: Long) = {
    val numChars = super.numPadChars(skipInBits, charWidthInBits)
    numChars / 2
  }
}

class LeftCenteredPaddingUnparser(override val rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  maybePadChar: MaybeChar)
    extends OnlyPaddingUnparser(rd, targetLengthEv, maybePadChar) {

  override def numPadChars(skipInBits: Long, charWidthInBits: Long) = {
    val numChars = super.numPadChars(skipInBits, charWidthInBits)
    if ((numChars & 1) == 0)
      numChars / 2
    else
      (numChars / 2) + 1
  }
}

class RightFillUnparser(
  override val rd: ElementRuntimeData,
  override val targetLengthEv: UnparseTargetLengthInBitsEv,
  fillByteEv: FillByteEv,
  override val maybePadChar: MaybeChar)
    extends ElementUnusedUnparser(rd, targetLengthEv, fillByteEv)
    with PaddingUnparserMixin {

  override def continuation(state: UState) {
    val skipInBits = getSkipBits(state)
    val cs = charset(state)
    val skipInBitsMinusPadding =
      if (maybePadChar.isDefined) {
        skipInBits % cs.padCharWidthInBits
      } else {
        skipInBits
      }

    skipTheBits(state, skipInBitsMinusPadding)
  }

}

