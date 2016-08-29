package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.dpath.SuspendableExpression
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.FillByteEv
import edu.illinois.ncsa.daffodil.processors.RetryableException
import edu.illinois.ncsa.daffodil.processors.SuspendableOperation
import edu.illinois.ncsa.daffodil.processors.UnparseTargetLengthInBitsEv
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.MaybeChar
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors.SuspendableUnparser

/*
 * Notes on variable-width characters with lengthUnits 'characters'
 *
 * (Not worth doing for complex types, at least initially)
 *
 * (Similarly, don't implement dfdl:contentLength nor dfdl:valueLength
 * functions in units 'characters' for complex types if the encoding is
 * variable width).
 *
 * Simple types:
 *
 * For parsing, we must decode characters one by one until we accumulate
 * the number of characters.
 *
 * The dfdl:contentLength in characters is this number of characters. This requires
 * specific unparsers for all the textual data types for this case.
 *
 * If we are not trimming off pad characters, then the dfdl:valueLength is the
 * same as the dfdl:contentLength.
 *
 * A single-pass algorithm that does trimming on left/right, and filling of the
 * rightFill region would be:
 *
 * If we are trimming on the left (justification right or center) we must
 * (1) record bit position as start of dfdl:contentLength in bits.
 * (2) decode characters, counting up to N, keeping count also of the number of
 * pad characters encountered (and discarding those pad characters). When a
 * non-pad-character is encountered, the start position (in bits) is the start
 * of the dfdl:valueLength in bits.
 * (3) decode characters, counting up to N, and this part is tricky.
 * If we are trimming on the right (justification center and left) then keeping
 * track of the position of the last non-pad character, and the length of a
 * current run of adjacent pad characters since the last non-pad character. When
 * we reach N characters, the end of value position (in bits) is the position after the last
 * non-pad character, the final position after N characters is the dfdl:contentLength
 * in bits.  The dfdl:valueLength in characters is the dfdl:valueLength in characters
 * minus the length of the run of pad characters on the right, and the number of
 * pad characters counted on the left.
 *
 * For unparsing, we must encode characters one by one until we have
 * encoded the number of characters, or we run out of characters.
 * If we run out of characters, then if we are padding, the length of the left
 * and right padding regions is computed, and that many are encoded into
 * those. If we are not padding and we run out of characters it is an error.
 * If we have too many characters, then if simpleType string and
 * truncateVariableLengthString, then we discard any excess characters, otherwise
 * it is an error.
 *
 * The contentLength and valueLength in characters need to be stored on the infoset
 * node explicitly by these processors.
 *
 * The infoset value is NOT modified by truncation nor padding.
 * The fn:stringLength of the value is constant throughout this.
 *
 * Complex Types:
 *
 * (Not worth doing. Should SDE - not implemented by Daffodil - complex types with
 * specified length with length units characters, with variable length encoding.)
 *
 * For parsing, we record the start of content bit position (and start of value bit
 * position is the same), then we decode N characters, and the new bit position
 * is the end of content bit position. Behavior on a decode error is controlled by
 * dfdl:encodingErrorPolicy. So dfdl:contentLength in 'characters' is N,
 * and we have the positions to enable us to compute dfdl:contentLength in bits.
 * Then we backup to the start of content bit position and recursively parse
 * the complex type body, in an environment where the data limit is set to prevent
 * parsing beyond the content length in bits. When this recursive parse
 * returns, the bit position is the end of value bit position, and we then
 * skip to the content end position.
 *
 * For unparsing, we record the start of content bit position and start of value
 * bit position is the same. Then we recursively unparse the complex type body into
 * a buffer. Then we scan and decode this counting the characters. If a decode
 * error occurs, dfdl:encodingErrorPolicy is used to decide whether to error, or
 * count 1 for the unicodeReplacementCharacter that is the replacement.
 *
 * This makes length of a complex type in characters fundamentally unreliable if
 * decode errors are possible. User beware. Use length in bytes or bits instead.
 *
 * When the recursive unparse completes, we block on the end bit pos
 * and the ElementUnused region is filled with the number of characters to reach total N.
 * If the number of characters is greater than N it is an error.
 */

/*
 * Notes on variable-width characters when length units are bits
 *
 * (Really does need to be implemented, examples like 80-byte records, but where
 * the characters can be utf-8 not just ascii = would be a typical Unicode
 * upgrade to a legacy 80-byte oriented application.)
 *
 * In this case we know the number of bits, we don't know how many characters
 * will be parsed or unparsed.
 *
 * Content and value length regions in bits/bytes are computed by the framework,
 * but content and/or value length in characters must be determined by keeping
 * count of the number of characters parsed by the pad/trim processors, and the
 * value processor. These counts need to be stored on the infoset node.
 *
 */

/*
 * non-text or fixed-width characters, units are bits
 *
 * For parsing or unparsing, we know the exact length in bits.
 *
 * However, if textual, the number of bits does not necessarily divide by
 * the width of a character in bits. There may be a fragment of a character
 * at the end.
 *
 * An example would be if there are 56 bits (7 bytes), but utf-16 characters
 * which are 16 bits each, will hold 3 characters with 8 bits left over.
 *
 * For unparsing, in the case where all the characters do not fit, we may
 * discard the extra characters, or we may fail.
 *
 * We don't have 'bytes' here because that is always converted to bits.
 *
 * Note that the dfdl:contentLength and dfdl:valueLength can be requested in 'characters'
 * and in that case, we can just divide by the character set width to convert
 * the number of bits to characters.
 */

/**
 * Base class for unparsing elements with specified length.
 *
 * Depends on use of separate unparsers for the padding/fill regions which
 * calculate their own sizes, generally after the length of the value region
 * has been determined.
 */
sealed abstract class SpecifiedLengthUnparserBase2(
  override val context: ElementRuntimeData,
  maybeTargetLengthEv: Maybe[UnparseTargetLengthInBitsEv])
  extends PrimUnparser {

  def erd = context

  final override lazy val childProcessors =
    contentUnparsers.toList ++
      setVarUnparsers.toList ++
      beforeContentUnparsers.toList

  protected def beforeContentUnparsers: Array[Unparser]
  protected def contentUnparsers: Array[Unparser]
  protected def setVarUnparsers: Array[Unparser]

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
  private def maybeTLOp = {
    val mtlop = if (maybeTargetLengthEv.isDefined)
      One(new TargetLengthOperation(context, maybeTargetLengthEv.get))
    else
      Nope
    mtlop
  }

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

  protected def runContentUnparsers(state: UState) {
    {
      var i = 0
      while (i < contentUnparsers.length) {
        contentUnparsers(i).unparse1(state, context)
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

    runContentUnparsers(state)

    computeSetVariables(state)

    endElement(state)
  }

  protected def move(start: UState) {
    val grIndex = start.groupIndexStack.pop()
    start.groupIndexStack.push(grIndex + 1)
    val childIndex = start.childIndexStack.pop()
    start.childIndexStack.push(childIndex + 1)
  }

  /**
   * Consumes the required infoset events and changes context so that the
   * element's DIElement node is the context element.
   */
  protected def startElement(state: UState): Unit = {
    val elem =
      if (!erd.isHidden) {
        // Hidden elements are not in the infoset, so we will never get an event
        // for them. Only try to consume start events for non-hidden elements
        val event = state.advanceOrError
        if (!event.isStart || event.erd != erd) {
          // it's not a start element event, or it's a start element event, but for a different element.
          // this indicates that the incoming infoset (as events) doesn't match the schema
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

  /**
   * Restores prior context. Consumes end-element event.
   */
  protected def endElement(state: UState): Unit = {
    if (!erd.isHidden) {
      // Hidden elements are not in the infoset, so we will never get an event
      // for them. Only try to consume end events for non-hidden elements
      val event = state.advanceOrError
      if (!event.isEnd || event.erd != erd) {
        // it's not an end-element event, or it's an end element event, but for a different element.
        // this indicates that the incoming infoset (as events) doesn't match the schema
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

class OVCRetryUnparserSuspendableOperation(override val rd: ElementRuntimeData,
  maybeUnparserTargetLengthInBitsEv: Maybe[UnparseTargetLengthInBitsEv], vUnparser: Unparser)
  extends SuspendableOperation {

  override protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = {
    if (this.maybeUnparserTargetLengthInBitsEv.isDefined) {
      val ev = this.maybeUnparserTargetLengthInBitsEv.get
      val mjul = ev.evaluate(ustate)
      if (mjul.isEmpty)
        MaybeULong.Nope
      else
        MaybeULong(mjul.get)
    } else
      MaybeULong.Nope
  }

  protected def test(state: UState) = {
    state.currentInfosetNode.asSimple.hasValue
  }

  protected def continuation(state: UState) {
    vUnparser.unparse1(state, rd)
  }
}

class OVCRetryUnparser(override val context: ElementRuntimeData,
  maybeUnparserTargetLengthInBitsEv: Maybe[UnparseTargetLengthInBitsEv], vUnparser: Unparser)
  extends PrimUnparser
  with SuspendableUnparser {

  override final def runtimeDependencies = Nil

  final override lazy val childProcessors = List(vUnparser)

  def suspendableOperation = new OVCRetryUnparserSuspendableOperation(
    context, maybeUnparserTargetLengthInBitsEv, vUnparser)

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

/**
 * For regular (not dfdl:outputValueCalc) elements.
 */
class ElementSpecifiedLengthUnparser(
  override val context: ElementRuntimeData,
  maybeTargetLengthEv: Maybe[UnparseTargetLengthInBitsEv],
  override val setVarUnparsers: Array[Unparser],
  override val beforeContentUnparsers: Array[Unparser],
  override val contentUnparsers: Array[Unparser])
  extends SpecifiedLengthUnparserBase2(context, maybeTargetLengthEv) {

  override val runtimeDependencies = maybeTargetLengthEv.toList

}

/**
 * For dfdl:outputValueCalc elements.
 */
class ElementOVCSpecifiedLengthUnparserSuspendableExpresion(
  override val rd: ElementRuntimeData,
  val setVarUnparsers: Array[Unparser])
  extends SuspendableExpression {

  override lazy val expr = rd.outputValueCalcExpr.get

  override final protected def processExpressionResult(state: UState, v: AnyRef) {
    val diSimple = state.currentInfosetNode.asSimple

    diSimple.setDataValue(v)

    computeSetVariables(state)
  }

  private def computeSetVariables(state: UState) {
    // variable assignment. Always after the element value, but
    // also still with this element itself as the context, so before
    // we end element.
    {
      var i = 0;
      while (i < setVarUnparsers.length) {
        setVarUnparsers(i).unparse1(state, rd)
        i += 1
      }
    }
  }

  override protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = MaybeULong(0L)

  //  override protected def maybeKnownLengthInBits(state: UState): MaybeULong = {
  //    if (maybeTargetLengthEv.isDefined) {
  //      val tlEv = maybeTargetLengthEv.get
  //      val maybeTL = tlEv.evaluate(state)
  //      if (maybeTL.isDefined) {
  //        val tl = maybeTL.get
  //        MaybeULong(tl)
  //      } else {
  //        MaybeULong.Nope
  //      }
  //    } else {
  //      MaybeULong.Nope
  //    }
  //  }

}

class ElementOVCSpecifiedLengthUnparser(
  override val context: ElementRuntimeData,
  maybeTargetLengthEv: Maybe[UnparseTargetLengthInBitsEv],
  override val setVarUnparsers: Array[Unparser],
  override val beforeContentUnparsers: Array[Unparser],
  override val contentUnparsers: Array[Unparser])
  extends SpecifiedLengthUnparserBase2(context, maybeTargetLengthEv) {

  override val runtimeDependencies = maybeTargetLengthEv.toList

  private def suspendableExpression =
    new ElementOVCSpecifiedLengthUnparserSuspendableExpresion(context,
      setVarUnparsers)

  Assert.invariant(context.outputValueCalcExpr.isDefined)

  override def unparse(state: UState): Unit = {

    startElement(state)

    captureRuntimeValuedExpressionValues(state)

    doBeforeContentUnparsers(state)

    computeTargetLength(state) // must happen before run() so that we can take advantage of knowing the length

    suspendableExpression.run(state) // run the expression. It might or might not have a value.

    runContentUnparsers(state) // setup unparsing, which will block for no value

    endElement(state)
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

/**
 * Carries out computation of the target length for a specified-length element.
 */
class TargetLengthOperation(override val rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv)
  extends SuspendableOperation {

  override def toString = "target length for " + rd.prettyName + " expr " + targetLengthEv.lengthInBitsEv.lengthEv.toBriefXML()

  /**
   * This override indicates that this operation itself doesn't correspond
   * to any bits in the unparsed data stream. It's just a computation.
   */
  override protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = MaybeULong(0L)

  override def test(ustate: UState): Boolean = {
    targetLengthEv.evaluate(ustate) // can we successfully evaluate without blocking (blocking would throw)
    true
  }

  override def continuation(state: UState) {
    // once we have evaluated the targetLengthEv, nothing else to do
    // here
  }
}

/**
 * Several sub-unparsers need to have the value length, and the target length
 * in order to compute their own length.
 */
trait NeedValueAndTargetLengthMixin {

  def targetLengthEv: UnparseTargetLengthInBitsEv

  protected final def hasTargetLength(ustate: UState): Boolean = {
    val mtl = targetLengthEv.evaluate(ustate)
    if (mtl.isEmpty) {
      val lib = targetLengthEv.lengthInBitsEv
      val lu = lib.lengthUnits
      val lk = lib.lengthKind
      val mcs = targetLengthEv.lengthInBitsEv.maybeCharsetEv
      val cs = mcs.get.evaluate(ustate)
      val csName = cs.charsetName
      ustate.SDE("For dfdl:lengthKind '%s', variable width character encoding '%s', and dfdl:lengthUnits '%s' are incompatible.",
        lk, csName, lu)
    }
    true
  }

  protected def test(ustate: UState): Boolean = {
    hasTargetLength(ustate) && {
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

class ElementUnusedUnparserSuspendableOperation(
  override val rd: ElementRuntimeData,
  override val targetLengthEv: UnparseTargetLengthInBitsEv,
  fillByteEv: FillByteEv)
  extends SuspendableOperation
  with NeedValueAndTargetLengthMixin {

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
    if (skipInBits > 0) {
      val dos = ustate.dataOutputStream
      val fb = fillByteEv.evaluate(ustate)
      dos.setFillByte(fb)
      if (!dos.skip(skipInBits))
        UE(ustate, "Unable to skip %s(bits).", skipInBits)
    }
    if (skipInBits == 0) {
      log(LogLevel.Debug, "%s no fill for %s DOS %s.",
        Misc.getNameFromClass(this), rd.prettyName, ustate.dataOutputStream)
    } else {
      log(LogLevel.Debug, "%s filled %s bits for %s DOS %s.",
        Misc.getNameFromClass(this), skipInBits, rd.prettyName, ustate.dataOutputStream)
    }
  }

}

class ElementUnusedUnparser(
  val rd: ElementRuntimeData,
  val targetLengthEv: UnparseTargetLengthInBitsEv,
  fillByteEv: FillByteEv)
  extends PrimUnparserObject(rd)
  with SuspendableUnparser {

  override lazy val runtimeDependencies = List(targetLengthEv, fillByteEv)

  override def suspendableOperation =
    new ElementUnusedUnparserSuspendableOperation(
      rd, targetLengthEv, fillByteEv)

}

trait PaddingUnparserMixin
  extends NeedValueAndTargetLengthMixin { self: SuspendableOperation =>

  protected def charsKind = "pad"

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
          UE(state, "Unable to output %s %s characters.", nChars, charsKind)
        i += 1
      }
    }
  }
}

class OnlyPaddingUnparserSuspendableOperation(override val rd: ElementRuntimeData,
  override val targetLengthEv: UnparseTargetLengthInBitsEv,
  override val maybePadChar: MaybeChar)
  extends SuspendableOperation
  with PaddingUnparserMixin

/**
 * Doesn't matter if we're left or right padding if we're the only padding
 */
class OnlyPaddingUnparser(
  val rd: ElementRuntimeData,
  val targetLengthEv: UnparseTargetLengthInBitsEv,
  val maybePadChar: MaybeChar)
  extends PrimUnparserObject(rd)
  with SuspendableUnparser {

  override lazy val runtimeDependencies = List(targetLengthEv)

  override def suspendableOperation =
    new OnlyPaddingUnparserSuspendableOperation(
      rd, targetLengthEv, maybePadChar)
}

class NilLiteralCharacterUnparserSuspendableOperation(override val rd: ElementRuntimeData,
  override val targetLengthEv: UnparseTargetLengthInBitsEv,
  literalNilChar: Char)
  extends SuspendableOperation
  with PaddingUnparserMixin {

  override def charsKind = "dfdl:nilKind 'literalCharacter'"

  override val maybePadChar: MaybeChar = MaybeChar(literalNilChar)

  //
  // We don't wait for the valueLength, because the unparsed
  // nil is part of the valueLength
  //
  override def test(state: UState) =
    hasTargetLength(state) && {
      val e = state.currentInfosetNode.asInstanceOf[DISimple]
      val isNilled = e.isNilled
      isNilled
    }

  override protected def getSkipBits(ustate: UState): Long = {
    val mtl = targetLengthEv.evaluate(ustate)
    val tl = mtl.get
    val skipInBits = tl
    skipInBits
  }

}

class NilLiteralCharacterUnparser(
  val rd: ElementRuntimeData,
  val targetLengthEv: UnparseTargetLengthInBitsEv,
  literalNilChar: Char)
  extends PrimUnparserObject(rd)
  with SuspendableUnparser {

  override lazy val runtimeDependencies = List(targetLengthEv)

  override def suspendableOperation = new NilLiteralCharacterUnparserSuspendableOperation(
    rd, targetLengthEv, literalNilChar)

}

class RightCenteredPaddingUnparserSuspendaableOperation(rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  maybePadChar: MaybeChar)
  extends OnlyPaddingUnparserSuspendableOperation(rd, targetLengthEv, maybePadChar) {

  override def numPadChars(skipInBits: Long, charWidthInBits: Long) = {
    val numChars = super.numPadChars(skipInBits, charWidthInBits)
    numChars / 2
  }
}

class RightCenteredPaddingUnparser(rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  maybePadChar: MaybeChar)
  extends OnlyPaddingUnparser(rd, targetLengthEv, maybePadChar) {

  override def suspendableOperation =
    new RightCenteredPaddingUnparserSuspendaableOperation(
      rd, targetLengthEv, maybePadChar)
}

class LeftCenteredPaddingUnparserSuspendableOperation(override val rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  maybePadChar: MaybeChar)
  extends OnlyPaddingUnparserSuspendableOperation(rd, targetLengthEv, maybePadChar) {

  override def numPadChars(skipInBits: Long, charWidthInBits: Long) = {
    val numChars = super.numPadChars(skipInBits, charWidthInBits)
    if ((numChars & 1) == 0)
      numChars / 2
    else
      (numChars / 2) + 1
  }
}

class LeftCenteredPaddingUnparser(override val rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  maybePadChar: MaybeChar)
  extends OnlyPaddingUnparser(rd, targetLengthEv, maybePadChar) {

  override def suspendableOperation =
    new LeftCenteredPaddingUnparserSuspendableOperation(
      rd, targetLengthEv, maybePadChar)
}

class RightFillUnparserSuspendableOperation(
  rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  fillByteEv: FillByteEv,
  override val maybePadChar: MaybeChar)
  extends ElementUnusedUnparserSuspendableOperation(rd, targetLengthEv, fillByteEv)
  with PaddingUnparserMixin {

  override def continuation(state: UState) {
    val skipInBits = getSkipBits(state)
    Assert.invariant(skipInBits >= 0)
    if (skipInBits > 0) {
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

}

class RightFillUnparser(
  rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  fillByteEv: FillByteEv,
  val maybePadChar: MaybeChar)
  extends ElementUnusedUnparser(rd, targetLengthEv, fillByteEv) {

  override def suspendableOperation =
    new RightFillUnparserSuspendableOperation(
      rd, targetLengthEv, fillByteEv, maybePadChar)

}

