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
import org.apache.daffodil.io.processors.charset.BitsCharsetDefinitionRegistry
import org.apache.daffodil.io.processors.charset.BitsCharsetJava
import org.apache.daffodil.io.processors.charset.BitsCharsetNonByteSize
import org.apache.daffodil.io.processors.charset.CharsetUtils
import org.apache.daffodil.lib.cookers.EncodingCooker
import org.apache.daffodil.lib.cookers.FillByteCooker
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.runtime1.dsom._

/*
 * The way encoding works, is if a EncodingChangeParser or Unparser is
 * added to the processor by the DFDL compiler, then when that processor is evaluated
 * it will invoke these EVs to obtain the right charset decoder and encoder.
 *
 * The encoder or decoder actually being used when characters are processed
 * is stored in the data stream by the change operation so that it is there when needed.
 *
 * Note that this implies for situations where backtracking can occur (parsing) or
 * the outputValueCalc with forward reference (unparsing), that the ChangeEncoding processor
 * must be re-evaluated (in general) if any change has occurred since, so that the
 * encoder/decoder we start using when we back up to the earlier position is the right
 * one.
 *
 * Often whole schemas will use only one encoding however, so the DFDL compiler may
 * optimize out the ChangeEncoding processors except the very first one.
 */

/**
 * Encoding is a string, so there is no converter.
 */
abstract class EncodingEvBase(
  override val expr: CompiledExpression[String],
  tci: DPathCompileInfo
) extends EvaluatableConvertedExpression[String, String](
    expr,
    EncodingCooker, // cooker insures upper-case and trimmed of whitespace.
    tci
  )
  with InfosetCachedEvaluatable[String] {
  override def runtimeDependencies = Vector()

  override protected def compute(state: ParseOrUnparseState): String = {
    // compute via the cooker first
    val encoding = super.compute(state)

    if (encoding == "UTF-16" || encoding == "UTF-32") {
      // TODO: Use byte order mark (from P/UState or another EV?) to determine
      // if encoding should be BE or LE. Note that this means that if encoding
      // is UTF-16 or UTF-32, then we cannot calculate EncodingEv at compile
      // time since it maybe depend on the a parse value

      // If BOM doesn't exist, then default to BE.
      encoding + "BE"
    } else {
      encoding
    }
  }
}

final class EncodingEv(expr: CompiledExpression[String], tci: DPathCompileInfo)
  extends EncodingEvBase(expr, tci)

abstract class CharsetEvBase(encodingEv: EncodingEvBase, tci: DPathCompileInfo)
  extends Evaluatable[BitsCharset](tci)
  with InfosetCachedEvaluatable[BitsCharset] {

  override def runtimeDependencies = Seq(encodingEv)

  private def checkCharset(state: ParseOrUnparseState, bitsCharset: BitsCharset): Unit = {
    if (bitsCharset.bitWidthOfACodeUnit != 8) {
      tci.schemaDefinitionError(
        "Only encodings with byte-sized code units are allowed to be specified using a runtime-valued expression. " +
          "Encodings with 7 or fewer bits in their code units must be specified as a literal encoding name in the DFDL schema. " +
          "The encoding found was '%s'.",
        bitsCharset.name
      )
    }
  }

  override def compute(state: ParseOrUnparseState) = {
    val encString = encodingEv.evaluate(state)
    val bc = CharsetUtils.getCharset(encString)
    if (bc == null) {
      tci.schemaDefinitionError(
        "Unsupported encoding: %s. Supported encodings: %s",
        encString,
        BitsCharsetDefinitionRegistry.supportedEncodingsString
      )
    }
    if (!encodingEv.isConstant) checkCharset(state, bc)
    bc
  }
}

final class CharsetEv(encodingEv: EncodingEv, tci: DPathCompileInfo)
  extends CharsetEvBase(encodingEv, tci)

class FillByteEv(fillByteRaw: String, charsetEv: CharsetEv, tci: DPathCompileInfo)
  extends Evaluatable[Integer](tci)
  with InfosetCachedEvaluatable[Integer] {

  override def runtimeDependencies = Seq(charsetEv)

  private val maybeSingleRawByteValue: MaybeInt = {
    val RawByte = """\%\#r([0-9a-fA-F]{2})\;""".r
    val mfb = fillByteRaw match {
      case RawByte(hex) =>
        MaybeInt(Integer.parseInt(hex, 16))
      case _ => MaybeInt.Nope
    }
    mfb
  }

  override protected def compute(state: ParseOrUnparseState): Integer = {
    val res =
      if (maybeSingleRawByteValue.isDefined) {
        // fillByte was a single raw byte, don't have to worry about encoding
        maybeSingleRawByteValue.get
      } else {
        // not a single raw byte, need to cook and encode it
        val cookedFillByte = FillByteCooker.cook(fillByteRaw, tci, true)
        Assert.invariant(cookedFillByte.length == 1)

        val bitsCharset = charsetEv.evaluate(state)
        bitsCharset match {
          case _: BitsCharsetNonByteSize => {
            state.SDE(
              "The fillByte property cannot be specified as a" +
                " character ('%s') when the dfdl:encoding property is '%s' because that" +
                " encoding is not a single-byte character set.",
              fillByteRaw,
              bitsCharset.name
            )
          }
          case cs: BitsCharsetJava => {
            val bytes = cookedFillByte.getBytes(cs.javaCharset)
            Assert.invariant(bytes.length > 0)
            if (bytes.length > 1) {
              state.SDE(
                "The fillByte property must be a single-byte" +
                  " character, but for encoding '%s' the specified character '%s'" +
                  " occupies %d bytes",
                bitsCharset.name,
                cookedFillByte,
                bytes.length
              )
            }
            bytes(0).toInt
          }
        }
      }
    res
  }

}

object FillByteUseNotAllowedEv
  extends FillByteEv(fillByteRaw = null, charsetEv = null, tci = null) {
  override protected def compute(state: ParseOrUnparseState): Integer =
    Assert.invariantFailed("Not supposed to use fill byte.")
}
