
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

package org.apache.daffodil.charsets

import org.apache.daffodil.processors.charset.BitsCharsetJava
import org.apache.daffodil.processors.charset.BitsCharsetDecoderByteSize
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.processors.charset.BitsCharsetDefinition
import java.nio.ByteBuffer
import java.nio.charset.Charset

object BitsCharsetTest_ISO_8859_13 extends {
  override val name = "ISO-8859-13"
} with BitsCharsetJava {

  override def newDecoder() = new BitsCharsetTest_Decoder_ISO_8859_13()

}

class BitsCharsetTest_Decoder_ISO_8859_13
  extends BitsCharsetDecoderByteSize {

  val decodeString = {
    val bytes = ByteBuffer.wrap((0 to 255).map{ _.toByte }.toArray)
    Charset.forName("ISO-8859-13").newDecoder().decode(bytes).toString
  }

  protected override def decodeOneChar(dis: InputSourceDataInputStream, finfo: FormatInfo): Char = {
    val byte = getByte(dis, 0)
    decodeString(byte)
  }
}

final class BitsCharsetTest_ISO_8859_13_Definition
  extends BitsCharsetDefinition(BitsCharsetTest_ISO_8859_13)
