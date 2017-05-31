/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.processors.unparsers

//import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.UnparseTargetLengthInBitsEv
import edu.illinois.ncsa.daffodil.processors.CharsetEv
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.util.MaybeJULong
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.UnparseTargetLengthInCharactersEv
import edu.illinois.ncsa.daffodil.processors.Evaluatable
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.infoset.RetryableException

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
sealed abstract class SpecifiedLengthUnparserBase(eUnparser: Unparser,
  erd: ElementRuntimeData)

final class SpecifiedLengthExplicitImplicitUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  targetLengthInBitsEv: UnparseTargetLengthInBitsEv,
  maybeTargetLengthInCharactersEv: Maybe[UnparseTargetLengthInCharactersEv])
  extends UnparserObject(erd) {

  override lazy val childProcessors = Seq(eUnparser)

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
  def unparseVarWidthCharactersInBits(state: UState) {
    val maybeTLBits = getMaybeTL(state, targetLengthInBitsEv)

    if (maybeTLBits.isDefined) {
      //
      // We know the target length. We can use it.
      //
      if (areTruncating) {
        val diSimple = state.currentInfosetNode.asSimple
        val v = diSimple.dataValue.asInstanceOf[String]
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
  def unparseVarWidthCharactersInCharacters(state: UState) {

    //
    // variable-width encodings and lengthUnits characters, and lengthKind explicit
    // is not supported (currently) for complex types
    //
    state.schemaDefinitionUnless(erd.isSimpleType,
      "Variable width character encoding '%s', dfdl:lengthKind '%s' and dfdl:lengthUnits '%s' are not supported for complex types.",
      getCharset(state).charsetName, lengthKind.toString, lengthUnits.toString)

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
        val v = diSimple.dataValue.asInstanceOf[String]
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
  def unparseBits(state: UState) {

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
