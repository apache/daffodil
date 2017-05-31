/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.util.TransientParam
import java.nio.charset.StandardCharsets
import edu.illinois.ncsa.daffodil.util.Maybe

/**
 * To eliminate circularities between RuntimeData objects and the
 * encoding compiled expression, all information derived from encodings
 * must come from one of these objects.
 *
 * That way we can construct this separately, the compilation of the
 * compiled expression for encoding can happily insist on a runtimeData object
 * existing to provide the information it generally needs.
 */

/**
 * Definitions that are the same whether we're in the schema compiler
 * or runtime are on this trait.
 */

trait KnownEncodingMixin { self: ThrowsSDE =>

  def isKnownEncoding: Boolean
  def charsetEv: CharsetEv
  def knownEncodingAlignmentInBits: Int

  def optionUTF16Width: Option[UTF16Width]

  /**
   * Note that the canonical form for encoding names is all upper case.
   */
  final lazy val knownEncodingName = {
    Assert.invariant(isKnownEncoding)
    val res = charsetEv.optConstant.get.charsetName
    res
  }

  final lazy val knownEncodingCharset = {
    new DFDLCharset(knownEncodingName)
  }

  /**
   * enables optimizations and random-access
   *
   * variable-width character sets require scanning to determine
   * their end.
   */
  final lazy val knownEncodingIsFixedWidth = {
    if (!isKnownEncoding)
      false
    else {
      val maybeFixedWidth = knownEncodingCharset.maybeFixedWidth
      maybeFixedWidth.isDefined
    }
  }

  final lazy val knownEncodingWidthInBits = encodingMinimumCodePointWidthInBits(knownEncodingCharset)

  final def encodingMinimumCodePointWidthInBits(cs: DFDLCharset) = {
    val res = cs.charset match {
      case StandardCharsets.UTF_8 => 8
      case _ => cs.maybeFixedWidth.get
    }
    res
  }

  final lazy val knownEncodingIsUnicode = {
    if (!isKnownEncoding) { false }
    else { knownEncodingName.toUpperCase.startsWith("UTF") }
  }

  final lazy val mustBeAnEncodingWith8BitAlignment = {
    !isKnownEncoding || knownEncodingAlignmentInBits == 8
  }

  final lazy val couldBeVariableWidthEncoding = !knownEncodingIsFixedWidth

  final def knownFixedWidthEncodingInCharsToBits(nChars: Long): Long = {
    Assert.usage(isKnownEncoding)
    Assert.usage(knownEncodingIsFixedWidth)
    val nBits = knownEncodingWidthInBits * nChars
    nBits
  }

}

/**
 * This is the object we serialize.
 *
 * At compile time we will create an encodingInfo
 * for ourselves supplying as context a schema component.
 *
 * At runtime we will create an encodingInfo supplying as context
 * a TermRuntimeData object.
 */

final class EncodingRuntimeData(
  @TransientParam termRuntimeDataArg: => TermRuntimeData,
  @TransientParam charsetEvArg: => CharsetEv,
  @TransientParam checkEncodingEvArg: => CheckEncodingEv,
  override val schemaFileLocation: SchemaFileLocation,
  optionUTF16WidthArg: Option[UTF16Width],
  val defaultEncodingErrorPolicy: EncodingErrorPolicy,
  val summaryEncoding: EncodingLattice,
  val isKnownEncoding: Boolean,
  val isScannable: Boolean,
  override val knownEncodingAlignmentInBits: Int)
  extends KnownEncodingMixin with ImplementsThrowsSDE with PreSerialization {

  private val optionUTF16Width_ = optionUTF16WidthArg
  private val maybeUTF16Width_ = Maybe.toMaybe[UTF16Width](optionUTF16WidthArg)

  @deprecated("20170530", "Use maybeUTF16Width instead.")
  def optionUTF16Width = optionUTF16Width_
  def maybeUTF16Width = maybeUTF16Width_

  lazy val termRuntimeData = termRuntimeDataArg
  lazy val charsetEv = charsetEvArg
  lazy val checkEncodingEv = checkEncodingEvArg

  lazy val runtimeDependencies = List(charsetEv, checkEncodingEv)

  def getDecoderInfo(state: ParseOrUnparseState) = {
    checkEncodingEv.evaluate(state)
    val cs = charsetEv.evaluate(state)
    val dec = state.getDecoderInfo(cs.charset)
    dec
  }

  def getEncoderInfo(state: ParseOrUnparseState) = {
    checkEncodingEv.evaluate(state)
    val cs = charsetEv.evaluate(state)
    val enc = state.getEncoderInfo(cs.charset)
    enc
  }

  def getEncoder(state: ParseOrUnparseState, dcs: DFDLCharset) = {
    checkEncodingEv.evaluate(state)
    val enc = state.getEncoder(dcs.charset)
    enc
  }

  def getDFDLCharset(state: ParseOrUnparseState): DFDLCharset = {
    checkEncodingEv.evaluate(state)
    val cs = charsetEv.evaluate(state)
    cs
  }

  override def preSerialization: Any = {
    super.preSerialization
    termRuntimeData
    charsetEv
    checkEncodingEv
  }

  @throws(classOf[java.io.IOException])
  private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  /**
   * no alignment properties that would explicitly create
   * a need to align in a way that is not on a suitable boundary
   * for a character.
   */
  lazy val hasTextAlignment = {
    this.knownEncodingAlignmentInBits == termRuntimeData.alignmentValueInBits
  }
}
