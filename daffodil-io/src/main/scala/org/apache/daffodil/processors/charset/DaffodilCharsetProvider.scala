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

/**
 * This implements a CharsetProvider that implements Daffodil's support
 * for less-than-byte-sized encodings. However, it seems hard to get
 * additional encodings to actually be recognized by the JVM, as if this
 * extension mechanism was really intended for JVM maintainers to use, but
 * not for end-user jar libraries to use.
 *
 * Also, we have to have a proxy class BitsCharset, and we have to produce
 * those for all charsets, rather than being able to extend
 * java.nio.charset.Charset - because that's full of final methods.
 *
 * Rather than contort to try to get this working, We simply call this
 * directly from a CharsetUtils method that we use instead of Charset.forName.
 */
object DaffodilCharsetProvider {
  import collection.JavaConverters._

  /**
   * Creates an iterator that iterates over the charsets supported by this
   * provider.  This method is used in the implementation of the
   * `java.nio.charset.Charset.availableCharsets()` method.
   *
   * @return  The new iterator
   */
  def charsets(): java.util.Iterator[BitsCharset] = {
    addedCharsets.toIterator.asJava
  }

  // keep in case we need to put this check back in temporarily
  // private val noWSNoLowerCasePatern = """[^\sa-z]+""".r.pattern // one or more characters, not whitespace, not lower case.

  private lazy val addedCharsets = addedCharsetsMap.map { _._2 }

  private val addedCharsetsMap: Map[String, BitsCharset] =
    Map(
      "X-DFDL-US-ASCII-7-BIT-PACKED" -> USASCII7BitPackedCharset, // DFDL v1.0
      "X-DFDL-US-ASCII-6-BIT-PACKED" -> USASCII6BitPackedCharset, // DFDL v1.0
      //
      // below are Daffodil extensions
      //
      "X-DFDL-5-BIT-PACKED-LSBF" -> DFDL5BitPackedLSBFCharset,
      "X-DFDL-HEX-LSBF" -> HexLSBF4BitCharset,
      "X-DFDL-HEX-MSBF" -> HexMSBF4BitCharset,
      "X-DFDL-OCTAL-LSBF" -> OctalLSBF3BitCharset,
      "X-DFDL-OCTAL-MSBF" -> OctalMSBF3BitCharset,
      "X-DFDL-BITS-LSBF" -> X_DFDL_BITS_LSBF,
      "X-DFDL-BITS-MSBF" -> X_DFDL_BITS_MSBF,
      "X-DFDL-6-BIT-DFI-264-DUI-001" -> X_DFDL_6_BIT_DFI_264_DUI_001 // needed for STANAG 5516/Link16
      )

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
    //    {
    //      // TODO: expensive check, remove if unnecessary
    //      //
    //      // However, encodingEv should guarantee that we already get an upper case token.
    //      // So we shouldn't need to test this.
    //      val m = noWSNoLowerCasePatern.matcher(charsetName)
    //      Assert.usage(m.matches)
    //    }
    val lookupResult = addedCharsetsMap.get(charsetName)
    val cs = lookupResult.getOrElse(null)
    cs
  }

}

