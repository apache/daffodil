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


import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DIElement
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.Infoset
import org.apache.daffodil.infoset.RetryableException
import org.apache.daffodil.processors.CharsetEv
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.UnparseTargetLengthInBitsEv
import org.apache.daffodil.processors.UnparseTargetLengthInCharactersEv
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.schema.annotation.props.gen.Representation
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeJULong

/**
 * Restricts the bits available for unparsing to just those within
 * the specified length computed.
 *
 * If a unparser (supplied as arg) runs past the available space,
 * that's an unparse error.
 *
 * Truncation of strings - the only case where we truncate, and only when
 * dfdl:truncateSpecifiedLengthString is 'yes', is handled elsewhere.
 */
sealed abstract class SpecifiedLengthUnparserBase(
  eUnparser: Unparser,
  erd: ElementRuntimeData)

final class SpecifiedLengthExplicitImplicitUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  targetLengthInBitsEv: UnparseTargetLengthInBitsEv,
  maybeTargetLengthInCharactersEv: Maybe[UnparseTargetLengthInCharactersEv])
  extends CombinatorUnparser(erd) {

  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector(eUnparser)

  private val libEv = targetLengthInBitsEv.lengthInBitsEv
  private val mcsEv = libEv.maybeCharsetEv
  private val lengthUnits = libEv.lengthUnits
  private val lengthKind = libEv.lengthKind

  private def getCharset(state: UState) = {
    val csEv: CharsetEv = mcsEv.get
    val dcs = csEv.evaluate(state)
    dcs
  }

  override final def unparse(state: UState): Unit = {

    erd.impliedRepresentation match {
      case Representation.Binary =>
        unparseBits(state)
      case Representation.Text => {
        val dcs = getCharset(state)
        if (dcs.maybeFixedWidth.isDefined)
          unparseBits(state)
        else {
          // we know the encoding is variable width characters
          // but we don't know if the length units characters or bits/bytes.
          lengthUnits match {
            case LengthUnits.Bits | LengthUnits.Bytes =>
              unparseVarWidthCharactersInBits(state)
            case LengthUnits.Characters =>
              unparseVarWidthCharactersInCharacters(state)
          }
        }
      }
    }
  }

  /**
   * Encoding is variable width (e.g., utf-8, but the
   * target length is expressed in bits.
   *
   * Truncation, in this case, requires determining how many
   * of the string's characters fit within the available target length
   * bits.
   */
  def unparseVarWidthCharactersInBits(state: UState): Unit = {
    val maybeTLBits = getMaybeTL(state, targetLengthInBitsEv)

    if (maybeTLBits.isDefined) {
      //
      // We know the target length. We can use it.
      //
      if (areTruncating) {
        val diSimple = state.currentInfosetNode.asSimple
        val v = diSimple.dataValue.getString
        val tl = maybeTLBits.get
        val cs = getCharset(state)
        val newV = state.truncateToBits(v, cs, tl)
        //
        // JIRA DFDL-1592
        //
        // BUG: should not modify the actual dataset value.
        // as fn:string-length of the value should always return the same
        // value which is the un-truncated length.
        //
        diSimple.overwriteDataValue(newV)
      }
      eUnparser.unparse1(state)
    } else {
      // target length unknown
      // ignore constraining the output length. Just unparse it.
      //
      // This happens when we're unparsing, and this element depends on a prior element for
      // determining its length, but that prior element has dfdl:outputValueCalc that depends
      // on this element.
      // This breaks the chicken-egg cycle.
      //
      eUnparser.unparse1(state)
    }
  }

  private def areTruncating = {
    if (erd.isSimpleType && (erd.optPrimType.get eq PrimType.String)) {
      Assert.invariant(erd.optTruncateSpecifiedLengthString.isDefined)
      erd.optTruncateSpecifiedLengthString.get
    } else
      false
  }

  /**
   * Encoding is variable width (e.g., utf-8). The target
   * length is expressed in characters.
   */
  def unparseVarWidthCharactersInCharacters(state: UState): Unit = {

    //
    // variable-width encodings and lengthUnits characters, and lengthKind explicit
    // is not supported (currently) for complex types
    //
    state.schemaDefinitionUnless(
      erd.isSimpleType,
      "Variable width character encoding '%s', dfdl:lengthKind '%s' and dfdl:lengthUnits '%s' are not supported for complex types.",
      getCharset(state).name, lengthKind.toString, lengthUnits.toString)

    Assert.invariant(erd.isSimpleType)
    Assert.invariant(this.maybeTargetLengthInCharactersEv.isDefined)
    val tlEv = this.maybeTargetLengthInCharactersEv.get
    val tlChars = this.getMaybeTL(state, tlEv)

    if (tlChars.isDefined) {
      //
      // possibly truncate
      //
      if (areTruncating) {
        val diSimple = state.currentInfosetNode.asSimple
        val v = diSimple.dataValue.getString
        val tl = tlChars.get
        if (v.length > tl) {
          // string is too long, truncate to target length
          val newV = v.substring(0, tl.toInt)
          //
          // BUG: JIRA DFDL-1592 - should not be overwriting the value with
          // truncated value.
          //
          diSimple.overwriteDataValue(newV)
        }
      }
      eUnparser.unparse1(state, erd)
    } else {
      // target length unknown
      // ignore constraining the output length. Just unparse it.
      //
      // This happens when we're unparsing, and this element depends on a prior element for
      // determining its length, but that prior element has dfdl:outputValueCalc that depends
      // on this element.
      // This breaks the chicken-egg cycle.
      //
      eUnparser.unparse1(state, erd)
    }
  }

  private def getMaybeTL(state: UState, TLEv: Evaluatable[MaybeJULong]): MaybeJULong = {
    val maybeTLBits = try {
      val tlRes = TLEv.evaluate(state)
      Assert.invariant(tlRes.isDefined) // otherwise we shouldn't be in this method at all
      tlRes
    } catch {
      case e: RetryableException => {
        //
        // TargetLength expression couldn't be evaluated.
        //
        MaybeJULong.Nope
      }
    }
    maybeTLBits
  }

  /**
   * Regardless of the type (text or binary), the target length
   * will be provided in bits.
   */
  def unparseBits(state: UState): Unit = {

    val maybeTLBits = getMaybeTL(state, targetLengthInBitsEv)

    if (maybeTLBits.isDefined) {
      //
      // We know the target length. We can use it.
      //
      //      val nBits = maybeTLBits.get
      //      val dos = state.dataOutputStream

      //
      // withBitLengthLimit is incorrect. It doesn't take into account
      // that after the unparse, we could be looking at a current state
      // with a distinct DOS
      //
      //      val isLimitOk = dos.withBitLengthLimit(nBits) {
      //        eUnparser.unparse1(state, erd)
      //      }
      //
      //      if (!isLimitOk) {
      //        val availBits = if (dos.remainingBits.isDefined) dos.remainingBits.get.toString else "(unknown)"
      //        UE(state, "Insufficient bits available. Required %s bits, but only %s were available.", nBits, availBits)
      //      }

      eUnparser.unparse1(state, erd)

      // at this point the recursive parse of the children is finished

      if (state.processorStatus ne Success) return

      // We might not have used up all the bits. So some bits may need to
      // be skipped and filled in by fillbyte.
      //
      // In the DFDL data grammar the region being skipped is either the
      // RightFill region, or the ElementUnused region. Skipping this is handled
      // elsewhere, along with insertion of padding before/after a string.
      //

    } else {
      //
      // we couldn't get the target length
      //
      // This happens when we're unparsing, and this element depends on a prior element for
      // determining its length via a length expression, but that prior element
      // has dfdl:outputValueCalc that depends on this element, typically by a
      // call to dfdl:valueLength of this, or of some structure that includes
      // this.
      //
      // This breaks the chicken-egg cycle, by just unparsing it without
      // constraint. That produces the value which (ignoring truncation)
      // can be unparsed to produce the dfdl:valueLength of this element.
      //
      // This does assume that the value will get truncated properly for the
      // case where we do truncation (type string, with
      // dfdl:truncateSpecifiedLengthString 'yes') by some other mechanism.
      //
      eUnparser.unparse1(state, erd)
    }
  }
}

// TODO: implement the capture length unparsers as just using this trait?
trait CaptureUnparsingValueLength {

  def captureValueLengthStart(state: UState, elem: DIElement): Unit = {
    val dos = state.dataOutputStream
    if (dos.maybeAbsBitPos0b.isDefined) {
      elem.valueLength.setAbsStartPos0bInBits(dos.maybeAbsBitPos0b.getULong)
    } else {
      elem.valueLength.setRelStartPos0bInBits(dos.relBitPos0b, dos)
    }
  }

  def captureValueLengthEnd(state: UState, elem: DIElement): Unit = {
    val dos = state.dataOutputStream
    if (dos.maybeAbsBitPos0b.isDefined) {
      elem.valueLength.setAbsEndPos0bInBits(dos.maybeAbsBitPos0b.getULong)
    } else {
      elem.valueLength.setRelEndPos0bInBits(dos.relBitPos0b, dos)
    }
  }
}

/**
 * This trait is to be used with prefixed length unparsers where the length is
 * known without needing to unparse the data. This means there is either a
 * fixed length (like in the case of some binary numbers), or the length can be
 * determined completly be inspecting the infoset data (like in the case of
 * packed decimals). The length calculation performed in the getBitLength
 * function, which returns the length of the data in bits.
 */
trait KnownPrefixedLengthUnparserMixin {
  def prefixedLengthERD: ElementRuntimeData
  def prefixedLengthUnparser: Unparser
  def lengthUnits: LengthUnits
  def getBitLength(s: ParseOrUnparseState): Int
  def prefixedLengthAdjustmentInUnits: Long

  def unparsePrefixedLength(state: UState): Unit = {
    val bits = getBitLength(state)
    val lenInUnits =
      if (lengthUnits == LengthUnits.Bytes) {
        bits >> 3
      } else {
        bits
      }
    val adjustedLenInUnits = lenInUnits + prefixedLengthAdjustmentInUnits

    // create a "detached" element that the prefix length will be parsed to.
    val plElement = Infoset.newElement(prefixedLengthERD).asInstanceOf[DISimple]
    plElement.setDataValue(java.lang.Integer.valueOf(adjustedLenInUnits.toInt))

    // unparse the prefixed length element
    state.currentInfosetNodeStack.push(One(plElement))
    prefixedLengthUnparser.unparse1(state)
    state.currentInfosetNodeStack.pop
  }
}

/**
 * This trait is to be used with prefixed length unparsers where the length
 * must be calculated based on the value length of the data. This means the
 * data must be unparsed, the value length calculated, and that value will be
 * assigned to the prefix length element.
 */
trait CalculatedPrefixedLengthUnparserMixin {
  def lengthUnits: LengthUnits
  def prefixedLengthAdjustmentInUnits: Long

  /**
   * Gets the length of a provided element and stores the unit adjusted length
   * as the value in the element that represent its prefix length
   *
   * @param elem The element who's length to get
   * @param plElem The element to store the length
   * @param lengthUnits The length units (bytes or bits)
   */
  def assignPrefixLength(elem: DIElement, plElem: DISimple): Unit = {
    val lenInUnits = lengthUnits match {
      case LengthUnits.Bits => elem.valueLength.lengthInBits
      case LengthUnits.Bytes => elem.valueLength.lengthInBytes
      case LengthUnits.Characters => ???
    }
    val adjustedLenInUnits = lenInUnits + prefixedLengthAdjustmentInUnits
    plElem.setDataValue(java.lang.Integer.valueOf(adjustedLenInUnits.toInt))
  }
}

class SpecifiedLengthPrefixedUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  prefixedLengthUnparser: Unparser,
  prefixedLengthERD: ElementRuntimeData,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long)
  extends CombinatorUnparser(erd)
  with CaptureUnparsingValueLength
  with CalculatedPrefixedLengthUnparserMixin {

  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector(prefixedLengthUnparser, eUnparser)

  override def unparse(state: UState): Unit = {
    // create a "detached" element that the prefix length will be used to unparse
    val plElem = Infoset.newElement(prefixedLengthERD).asInstanceOf[DISimple]

    // The prefixedLengthUnparser is going to end up creating a suspension
    // because plElem does not have a value yet. We will temporary push the
    // detached element to the stack, the suspension will clone the ustate and
    // keep track of that detached element, then we can pop it off the stack as
    // it's no longer needed.
    state.currentInfosetNodeStack.push(One(plElem))
    prefixedLengthUnparser.unparse1(state)
    state.currentInfosetNodeStack.pop

    // We now need to capture the length of the actual element
    val elem = state.currentInfosetNode.asInstanceOf[DIElement]
    captureValueLengthStart(state, elem)
    eUnparser.unparse1(state)
    captureValueLengthEnd(state, elem)

    if (elem.valueLength.maybeLengthInBits.isDefined) {
      // If we were able to immediately calculate the length of the element,
      // then just set it as the value of the detached element created above so
      // that when the prefixedLengthUnparser suspension resumes it can unparse
      // the value
      assignPrefixLength(elem, plElem)
    } else {
      // The length was not able to be calculated, likely because there was a
      // suspension when unparsing the eUnparser. So let's create a new
      // suspension with the only goal to retry until the valueLength of this
      // element is determined. Once determined, it will set the value of the
      // prefix length element, ultimately allowing the prefix length element
      // suspension to resume and unparse the value
      val suspension = new PrefixLengthSuspendableOperation(erd, elem, plElem, lengthUnits, prefixedLengthAdjustmentInUnits)

      // Run the suspension--we know he suspension will not be able to succeed
      // since maybeLengthInBits is not defined, but this performs various
      // actions to suspend the operation and allow it to be run later
      suspension.run(state)
    }
  }

}
