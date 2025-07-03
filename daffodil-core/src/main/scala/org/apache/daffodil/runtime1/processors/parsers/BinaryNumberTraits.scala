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

package org.apache.daffodil.runtime1.processors.parsers

import java.lang.{ Long => JLong }

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Numbers
import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.Infoset
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.Success

trait HasKnownLengthInBits {
  def lengthInBits: Int

  def getBitLength(s: ParseOrUnparseState) = {
    lengthInBits
  }
}

trait HasRuntimeExplicitLength {
  def e: ElementRuntimeData
  def lengthUnits: LengthUnits // get at compile time, not runtime.
  def lengthEv: Evaluatable[JLong]

  // binary numbers will use this conversion. Others won't.
  lazy val toBits = lengthUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ =>
      Assert.invariantFailed("Binary Numbers should never have length units of Bits or Bytes.")
  }

  def getBitLength(s: ParseOrUnparseState): Int = {
    val nBytesAsJLong = lengthEv.evaluate(s)
    val nBytes = Numbers.asInt(nBytesAsJLong)
    nBytes * toBits
  }
}

trait PrefixedLengthParserMixin {

  def prefixedLengthParser: Parser
  def prefixedLengthERD: ElementRuntimeData
  def lengthUnits: LengthUnits
  def prefixedLengthAdjustmentInUnits: Long

  /**
   * Runs the prefixedLengthParser to determine the prefix length value, making
   * adjustments as necessary. If the parse fails, this returns 0. The caller
   * is expected to check to see if state.processorStatus is error upon
   * returning, indicating that the returned value is meaningless.
   */
  def getPrefixedLengthInUnits(state: PState): Long = {
    val savedInfoset = state.infoset

    // Create a "detached" document with a single child element that the prefix
    // length will be parsed to. This creates a completely new infoset and
    // parses to that, so care is taken to ensure we reset it back after the parse
    val plElement = Infoset.newDetachedElement(state, prefixedLengthERD).asInstanceOf[DISimple]

    state.infoset = plElement

    try {
      prefixedLengthParser.parse1(state)
    } finally {
      // reset back to the original infoset and throw away the detatched
      // element
      state.infoset = savedInfoset
    }

    if (state.processorStatus eq Success) {
      val parsedLen = Numbers.asLong(plElement.dataValue.getAnyRef)
      state.schemaDefinitionWhen(
        parsedLen < 0,
        "Prefixed length result must be non-negative, but was: %d",
        parsedLen
      )
      val adjustedLen = parsedLen - prefixedLengthAdjustmentInUnits
      state.schemaDefinitionWhen(
        adjustedLen < 0,
        "Prefixed length result must be non-negative after dfdl:prefixIncludesPrefixLength adjustment , but was: %d",
        adjustedLen
      )
      // do checks on facets expressed on prefixLengthType
      val optSTRD = plElement.erd.optSimpleTypeRuntimeData
      if (optSTRD.isDefined) {
        val strd = optSTRD.get
        val check = strd.executeCheck(plElement)
        if (check.isError) {
          val pe = state.toProcessingError(
            s"The prefix length value of ${savedInfoset.namedQName} ($parsedLen) failed check due to ${check.errMsg}"
          )
          state.setFailed(pe)
        }
      }
      adjustedLen
    } else {
      // Return zero if there was an error parsing the prefix length, the caller of this
      // function should check the processorStatus to see if anything failed and ignore
      // this zero and lead to back tracking
      0
    }
  }

  /**
   * Get the prefixed length in bits. If the parse fails, this returns 0. The
   * caller is expected to check to see if state.processorStatus is error upon
   * returning, indicating that the returned value is meaningless.
   */
  def getPrefixedLengthInBits(state: PState): Long = {
    val lenInUnits = getPrefixedLengthInUnits(state)
    if (state.processorStatus eq Success) {
      val lenInBits = lengthUnits match {
        case LengthUnits.Bits => lenInUnits
        case LengthUnits.Bytes => lenInUnits * 8
        case LengthUnits.Characters => {
          val mfw = state.encoder.bitsCharset.maybeFixedWidth
          Assert.invariant(
            mfw.isDefined,
            "Prefixed length for text data in non-fixed width encoding."
          )
          lenInUnits * mfw.get
        }
      }
      // Now we save the contentLength that we obtained from the prefix
      // so that the dfdl:contentLength function can be called on the prefixed length element
      // at parse time.
      val mLenInBits = MaybeULong(lenInBits)
      state.infoset match {
        case ci: DIComplex => {
          ci.contentLength.maybeComputedLengthInBits = mLenInBits
          ci.valueLength.maybeComputedLengthInBits = mLenInBits
        }
        case si: DISimple => {
          si.contentLength.maybeComputedLengthInBits = mLenInBits
          // Note value length for simple types is handled elsewhere as we don't
          // have any information about padding/trimming here.
        }
      }
      lenInBits
    } else {
      0
    }
  }
}

/**
 * Some parsers do not calculate their own length, but instead expect another parser
 * to set the bit limit, and then they use that bit limit as the length.
 * An example of this is prefix length parsers. This trait can be used by those
 * parsers to do determine the length based on the bitLimit and position.
 */
trait BitLengthFromBitLimitMixin {

  def getBitLength(s: ParseOrUnparseState): Int = {
    val pState = s.asInstanceOf[PState]
    val len = getLengthInBits(pState)
    len.toInt
  }

  def getLengthInBits(pstate: PState): Long = {
    val len = pstate.bitLimit0b.get - pstate.bitPos0b
    len
  }
}
