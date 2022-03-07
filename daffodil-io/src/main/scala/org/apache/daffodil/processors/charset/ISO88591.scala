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

package org.apache.daffodil.processors.charset

import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.io.FormatInfo

object BitsCharsetISO88591 extends {
  override val name = "ISO-8859-1"
} with BitsCharsetJava {

  override def newDecoder() = new BitsCharsetDecoderISO88591()
}


class BitsCharsetDecoderISO88591
  extends BitsCharsetDecoderByteSize {

  protected override def decodeOneChar(dis: InputSourceDataInputStream, finfo: FormatInfo): Char = {
    val byte = getByte(dis, 0)
    byte.toChar
  }
}

final class BitsCharsetISO88591Definition
  extends BitsCharsetDefinition(BitsCharsetISO88591)
