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

import org.apache.daffodil.exceptions.Assert

object DaffodilCharsetProvider {

  lazy val charsets = Seq(
    BitsCharset3BitDFI336DUI001,
    BitsCharset3BitDFI746DUI002,
    BitsCharset3BitDFI747DUI001,
    BitsCharset4BitDFI746DUI002,
    BitsCharset5BitDFI769DUI002,
    BitsCharset5BitDFI1661DUI001,
    BitsCharset5BitPackedLSBF,
    BitsCharset6BitDFI264DUI001,
    BitsCharset6BitDFI311DUI002,
    BitsCharsetBase4LSBF,
    BitsCharsetBase4MSBF,
    BitsCharsetBinaryLSBF,
    BitsCharsetBinaryMSBF,
    BitsCharsetHexLSBF,
    BitsCharsetHexMSBF,
    BitsCharsetIBM037,
    BitsCharsetISO88591,
    BitsCharsetOctalLSBF,
    BitsCharsetOctalMSBF,
    BitsCharsetUSASCII,
    BitsCharsetUSASCII6BitPackedLSBF,
    BitsCharsetUSASCII6BitPackedMSBF,
    BitsCharsetUSASCII7BitPacked,
    BitsCharsetISO885918BitPackedLSBF,
    BitsCharsetISO885918BitPackedMSBF,
    BitsCharsetUTF16BE,
    BitsCharsetUTF16LE,
    BitsCharsetUTF32BE,
    BitsCharsetUTF32LE,
    BitsCharsetUTF8,
    BitsCharsetIBM1047
  )

  private lazy val charsetMap = {
    val nameCharsetPairs = charsets.flatMap { cs =>
      val names = cs.name +: cs.aliases
      names.map { name => (name -> cs) }
    }
    nameCharsetPairs.toMap
  }

  /**
   * Retrieves a charset for the given charset name.
   *
   * @param  charsetName
   *         The name of the requested charset; may be either
   *         a canonical name or an alias
   *
   * @return  A charset object for the named charset,
   *          or <tt>null</tt> if the named charset
   *          is not supported by this provider
   */
  def charsetForName(charsetName: String): BitsCharset = {
    Assert.usage(charsetName != null);
    val lookupResult = charsetMap.get(charsetName)
    val cs = lookupResult.getOrElse(null)
    cs
  }

}
