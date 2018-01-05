/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.TextJustificationType
import org.apache.daffodil.util.MaybeChar
import org.apache.daffodil.util.Misc
import passera.unsigned.ULong
import org.apache.daffodil.processors.CharsetEv

/**
 * Specifically designed to be used inside one of the SpecifiedLength parsers.
 * override
 * This grabs a string as long as it coverride an get, depending on the SpecifiedLength context
 * to constrain how much it can get.
 */
final class StringOfSpecifiedLengthParser(
  override val parsingPadChar: MaybeChar,
  override val justificationTrim: TextJustificationType.Type,
  erd: ElementRuntimeData)
  extends TextPrimParser
  with StringOfSpecifiedLengthMixin {

  override lazy val runtimeDependencies = List(erd.encInfo.charsetEv)

  override lazy val charsetEv = erd.encInfo.charsetEv

  override def context = erd

  private val eName = erd.name

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + eName + " parser='" + Misc.getNameFromClass(this) + "' />"
  }

  override def parse(start: PState) {
    val field = parseString(start)
    start.simpleElement.setDataValue(field)
  }

}

trait CaptureParsingValueLength {
  def charsetEv: CharsetEv

  final def captureValueLength(state: PState, startBitPos0b: ULong, endBitPos0b: ULong) {
    val elem = state.infoset
    elem.valueLength.setAbsStartPos0bInBits(startBitPos0b)
    elem.valueLength.setAbsEndPos0bInBits(endBitPos0b)
  }

  final def captureValueLengthOfString(state: PState, str: String) {
    val lengthInBits = state.lengthInBits(str, charsetEv.evaluate(state))
    // TODO: this is a hack, it doesn't actually set the correct start/end
    // pos due to padding. But it does set the correct information so that
    // the valueLength can be calculated, which is all this is really
    // needed for. For debug purposes, we might want to eventually set the
    // actual start/end bit positions.
    captureValueLength(state, ULong(0), ULong(lengthInBits))
  }
}

trait StringOfSpecifiedLengthMixin
  extends PaddingRuntimeMixin
  with CaptureParsingValueLength {

  protected final def parseString(start: PState): String = {
    val dis = start.dataInputStream
    val maxLen = dis.limits.maximumSimpleElementSizeInCharacters
    val startBitPos0b = dis.bitPos0b

    val strOpt = dis.getSomeString(maxLen, start)
    val str = if (strOpt.isDefined) strOpt.get else ""
    // TODO: Performance - trimByJustification wants to operate on a StringBuilder
    // That means that dis.getSomeString wants to return a StringBuilder instead of
    // a string itself.
    val field = trimByJustification(str)

    justificationTrim match {
      case TextJustificationType.None => captureValueLength(start, ULong(startBitPos0b), ULong(dis.bitPos0b))
      case _ => captureValueLengthOfString(start, field)
    }

    field
  }
}
