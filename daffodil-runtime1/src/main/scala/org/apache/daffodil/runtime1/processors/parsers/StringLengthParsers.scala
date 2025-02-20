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

import org.apache.daffodil.io.processors.charset.BitsCharsetDecoderUnalignedCharDecodeException
import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.processors.CharsetEv
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.TextJustificationType

import passera.unsigned.ULong

/**
 * Specifically designed to be used inside one of the SpecifiedLength parsers.
 * override
 * This grabs a string as long as it can get, depending on the SpecifiedLength context
 * to constrain how much it can get.
 */
final class StringOfSpecifiedLengthParser(
  override val parsingPadChar: MaybeChar,
  override val justificationTrim: TextJustificationType.Type,
  erd: ElementRuntimeData
) extends TextPrimParser
  with StringOfSpecifiedLengthMixin {

  override def runtimeDependencies = Vector(erd.encInfo.charsetEv)

  override lazy val charsetEv = erd.encInfo.charsetEv

  override def context = erd

  private val eName = erd.name

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + eName + " parser='" + Misc.getNameFromClass(this) + "' />"
  }

  override def parse(start: PState): Unit = {
    val field = parseString(start)
    start.simpleElement.setDataValue(field)
  }

}

trait CaptureParsingValueLength {
  def charsetEv: CharsetEv

  final def captureValueLength(
    state: PState,
    startBitPos0b: ULong,
    endBitPos0b: ULong
  ): Unit = {
    val elem = state.infoset
    elem.valueLength.setAbsStartPos0bInBits(startBitPos0b)
    elem.valueLength.setAbsEndPos0bInBits(endBitPos0b)
  }

  final def captureValueLengthOfString(state: PState, str: String): Unit = {
    val lengthInBits = state.lengthInBits(str, charsetEv.evaluate(state))
    // TODO: this is a hack, it doesn't actually set the correct start/end
    // pos due to padding. But it does set the correct information so that
    // the valueLength can be calculated, which is all this is really
    // needed for. For debug purposes, we might want to eventually set the
    // actual start/end bit positions.
    captureValueLength(state, ULong(0), ULong(lengthInBits))
  }
}

trait StringOfSpecifiedLengthMixin extends PaddingRuntimeMixin with CaptureParsingValueLength {

  protected final def parseString(start: PState): String = {
    val dis = start.dataInputStream
    val maxLen = start.tunable.maximumSimpleElementSizeInCharacters
    val startBitPos0b = dis.bitPos0b

    val strOpt =
      try {
        dis.getSomeString(maxLen, start)
      } catch {
        case e: BitsCharsetDecoderUnalignedCharDecodeException =>
          throw new CharsetNotByteAlignedError(start, e)
      }
    val str = if (strOpt.isDefined) strOpt.get else ""
    // TODO: Performance - trimByJustification wants to operate on a StringBuilder
    // That means that dis.getSomeString wants to return a StringBuilder instead of
    // a string itself.
    val field = trimByJustification(str)

    justificationTrim match {
      case TextJustificationType.None =>
        captureValueLength(start, ULong(startBitPos0b), ULong(dis.bitPos0b))
      case _ => captureValueLengthOfString(start, field)
    }

    field
  }
}
