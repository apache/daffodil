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

package org.apache.daffodil.io.processors.charset

import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream

object BitsCharsetUTF16BE extends BitsCharsetJava {
  override lazy val name = "UTF-16BE"

  override def newDecoder() = new BitsCharsetDecoderUTF16BE()
}

class BitsCharsetDecoderUTF16BE extends BitsCharsetDecoderCreatesSurrogates {

  protected override def decodeOneUnicodeChar(
    dis: InputSourceDataInputStream,
    finfo: FormatInfo
  ): Char = {
    val byte1 = getByte(dis, 0)
    val byte2 = getByte(dis, 8)

    val high = (byte1 << 8) | byte2

    if (high >= 0xd800 && high <= 0xdfff) {
      // surrogate pair, this needs to be a high or its an error
      if (high >= 0xdc00) throw new BitsCharsetDecoderMalformedException(16)

      // this is a valid high surrogate pair, need to get the low and save it
      // for the next decode
      val byte3 = getByte(dis, 16)
      val byte4 = getByte(dis, 24)

      val low = (byte3 << 8) | byte4
      if (low < 0xdc00 || low > 0xdfff) throw new BitsCharsetDecoderMalformedException(32)

      setLowSurrogate(low.toChar)
    }
    high.toChar
  }
}

final class BitsCharsetUTF16BEDefinition extends BitsCharsetDefinition(BitsCharsetUTF16BE)

final class BitsCharsetUTF16Definition
  extends BitsCharsetDefinition(BitsCharsetUTF16BE, Some("UTF-16"))
