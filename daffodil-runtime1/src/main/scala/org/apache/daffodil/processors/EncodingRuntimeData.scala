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

package org.apache.daffodil.processors

import org.apache.daffodil.dsom.EncodingLattice
import org.apache.daffodil.dsom.ImplementsThrowsSDE
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.processors.charset.BitsCharset
import org.apache.daffodil.processors.charset.StandardBitsCharsets
import org.apache.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.schema.annotation.props.gen.UTF16Width
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.PreSerialization
import org.apache.daffodil.util.TransientParam
import org.apache.daffodil.processors.charset.CharsetUtils

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

  /**
   * Note that the canonical form for encoding names is all upper case.
   */
  final lazy val knownEncodingName = {
    Assert.invariant(isKnownEncoding)
    val res = charsetEv.optConstant.get.name
    res
  }

  final lazy val knownEncodingCharset = {
    CharsetUtils.getCharset(knownEncodingName)
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

  final def encodingMinimumCodePointWidthInBits(cs: BitsCharset) = {
    val res = cs match {
      case StandardBitsCharsets.UTF_8 => 8
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
  override val schemaFileLocation: SchemaFileLocation,
  optionUTF16WidthArg: Option[UTF16Width],
  val defaultEncodingErrorPolicy: EncodingErrorPolicy,
  val summaryEncoding: EncodingLattice,
  val isKnownEncoding: Boolean,
  val isScannable: Boolean,
  override val knownEncodingAlignmentInBits: Int)
  extends KnownEncodingMixin with ImplementsThrowsSDE with PreSerialization {

  private val maybeUTF16Width_ = Maybe.toMaybe[UTF16Width](optionUTF16WidthArg)

  def maybeUTF16Width = maybeUTF16Width_

  lazy val termRuntimeData = termRuntimeDataArg
  lazy val charsetEv = charsetEvArg

  lazy val runtimeDependencies = List(charsetEv)

  def getDecoderInfo(state: ParseOrUnparseState) = {
    val cs = charsetEv.evaluate(state)
    val dec = state.getDecoderInfo(cs)
    dec
  }

  def getEncoderInfo(state: ParseOrUnparseState) = {
    val cs = charsetEv.evaluate(state)
    val enc = state.getEncoderInfo(cs)
    enc
  }

  def getEncoder(state: ParseOrUnparseState, cs: BitsCharset) = {
    val enc = state.getEncoder(cs)
    enc
  }

  def getDFDLCharset(state: ParseOrUnparseState): BitsCharset = {
    val cs = charsetEv.evaluate(state)
    cs
  }

  override def preSerialization: Any = {
    super.preSerialization
    termRuntimeData
    charsetEv
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
