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

package org.apache.daffodil.processors.parsers

import java.lang.{ Long => JLong }

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.Infoset
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.Success
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.util.Numbers

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
    case _ => Assert.invariantFailed("Binary Numbers should never have length units of Bits or Bytes.")
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

    // create a "detached" element that the prefix length will be parsed to.
    // This temporarily removes to infoset.
    val plElement = Infoset.newElement(prefixedLengthERD).asInstanceOf[DISimple]
    state.infoset = plElement

    val parsedLen: JLong = try {
      prefixedLengthParser.parse1(state)
      // Return zero if there was an error parsing, the caller of this
      // evaluatable should check the processorStatus to see if anything
      // failed and ignore this zero. If there was no error, return the value
      // as a long.
      if (state.processorStatus ne Success) 0 else Numbers.asLong(plElement.dataValue.getAnyRef)
    } finally {
      // reset back to the original infoset and throw away the detatched
      // element
      state.infoset = savedInfoset
    }
    if (parsedLen < 0) {
      state.SDE("Prefixed length result must be non-negative, but was: %d", parsedLen)
    }
    val adjustedLen = parsedLen - prefixedLengthAdjustmentInUnits
    if (adjustedLen < 0) {
      state.SDE("Prefixed length result must be non-negative after dfdl:prefixIncludesPrefixLength adjustment , but was: %d", adjustedLen)
    }
    adjustedLen
  }

  /**
   * Get the prefixed length in bits. If the parse fails, this returns 0. The
   * caller is expected to check to see if state.processorStatus is error upon
   * returning, indicating that the returned value is meaningless.
   */
  def getPrefixedLengthInBits(state: PState): Long = {
    val lenInUnits = getPrefixedLengthInUnits(state)
    lengthUnits match {
      case LengthUnits.Bits => lenInUnits
      case LengthUnits.Bytes => lenInUnits * 8
      case LengthUnits.Characters => {
        val mfw = state.encoder.bitsCharset.maybeFixedWidth
        if (mfw.isDefined) mfw.get
        else
          Assert.invariantFailed("Prefixed length for text data in non-fixed width encoding.")
      }
    }
  }
}
