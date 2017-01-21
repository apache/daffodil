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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.io.NonByteSizeCharset
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.cookers.FillByteCooker
import edu.illinois.ncsa.daffodil.cookers.EncodingCooker

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
class EncodingEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    EncodingCooker, // cooker insures upper-case and trimmed of whitespace.
    trd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Nil

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

class CharsetEv(encodingEv: EncodingEv, val trd: TermRuntimeData)
  extends Evaluatable[DFDLCharset](trd)
  with InfosetCachedEvaluatable[DFDLCharset] {

  override lazy val runtimeDependencies = Seq(encodingEv)

  override def compute(state: ParseOrUnparseState) = {
    val encString = encodingEv.evaluate(state)
    val cs = CharsetUtils.getCharset(encString)
    Assert.invariant(cs ne null)
    new DFDLCharset(encString)
  }
}

class FillByteEv(fillByteRaw: String, charsetEv: CharsetEv, val trd: TermRuntimeData)
  extends Evaluatable[Integer](trd)
  with InfosetCachedEvaluatable[Integer] {

  override lazy val runtimeDependencies = Seq(charsetEv)

  private val maybeSingleRawByteValue: MaybeInt = {
    val RawByte = """\%\#r([0-9a-fA-F]{2})\;""".r
    fillByteRaw match {
      case RawByte(hex) => MaybeInt(Integer.parseInt(hex, 16))
      case _ => MaybeInt.Nope
    }
  }

  override protected def compute(state: ParseOrUnparseState): Integer = {
    val res =
      if (maybeSingleRawByteValue.isDefined) {
        // fillByte was a single raw byte, don't have to worry about encoding
        maybeSingleRawByteValue.get
      } else {
        // not a single raw byte, need to cook and encode it
        val cookedFillByte = FillByteCooker.cook(fillByteRaw, trd, true)
        Assert.invariant(cookedFillByte.length == 1)

        val dfdlCharset = charsetEv.evaluate(state)
        dfdlCharset.charset match {
          case _: NonByteSizeCharset => {
            state.SDE("The fillByte property cannot be specified as a" +
              " character ('%s') when the dfdl:encoding property is '%s' because that" +
              " encoding is not a single-byte character set.", fillByteRaw, dfdlCharset.charsetName)
          }
          case cs => {
            val bytes = cookedFillByte.getBytes(cs)
            Assert.invariant(bytes.length > 0)
            if (bytes.length > 1) {
              state.SDE("The fillByte property must be a single-byte" +
                " character, but for encoding '%s' the specified character '%s'" +
                " occupies %d bytes", dfdlCharset.charsetName, cookedFillByte, bytes.length)
            }
            bytes(0).toInt
          }
        }
      }
    res
  }

}
