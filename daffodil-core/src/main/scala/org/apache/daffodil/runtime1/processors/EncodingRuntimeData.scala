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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.io.processors.charset.BitsCharset
import org.apache.daffodil.io.processors.charset.CharsetUtils
import org.apache.daffodil.io.processors.charset.StandardBitsCharsets
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.UTF16Width
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.dsom.ImplementsThrowsSDE

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

  final lazy val knownEncodingWidthInBits = encodingMinimumCodePointWidthInBits(
    knownEncodingCharset
  )

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

final class EncodingRuntimeData(
  val charsetEv: CharsetEv,
  override val schemaFileLocation: SchemaFileLocation,
  val maybeUTF16Width: Maybe[UTF16Width],
  val defaultEncodingErrorPolicy: EncodingErrorPolicy,
  val isKnownEncoding: Boolean,
  val isScannable: Boolean,
  override val knownEncodingAlignmentInBits: Int,
  val hasTextAlignment: Boolean
) extends KnownEncodingMixin
  with ImplementsThrowsSDE
  with Serializable {

  def runtimeDependencies = Vector(charsetEv)

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

}
