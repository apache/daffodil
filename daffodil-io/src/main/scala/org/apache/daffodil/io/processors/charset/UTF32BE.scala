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

object BitsCharsetUTF32BE
  extends {
    override val name = "UTF-32BE"
  }
  with BitsCharsetJava {

  override def newDecoder() = new BitsCharsetDecoderUTF32BE()
}

class BitsCharsetDecoderUTF32BE extends BitsCharsetDecoderCreatesSurrogates {

  protected override def decodeOneUnicodeChar(
    dis: InputSourceDataInputStream,
    finfo: FormatInfo
  ): Char = {
    val byte1 = getByte(dis, 0)
    val byte2 = getByte(dis, 8)
    val byte3 = getByte(dis, 16)
    val byte4 = getByte(dis, 24)

    val cp = (byte1 << 24) | (byte2 << 16) | (byte3 << 8) | byte4
    if (cp >= 0 && cp <= 0xffff) {
      cp.toChar
    } else if (cp <= 0x10ffff) {
      val high = Character.highSurrogate(cp)
      setLowSurrogate(Character.lowSurrogate(cp))
      high
    } else {
      throw new BitsCharsetDecoderMalformedException(32)
    }
  }
}

final class BitsCharsetUTF32BEDefinition extends BitsCharsetDefinition(BitsCharsetUTF32BE)

final class BitsCharsetUTF32Definition
  extends BitsCharsetDefinition(BitsCharsetUTF32BE, Some("UTF-32"))
