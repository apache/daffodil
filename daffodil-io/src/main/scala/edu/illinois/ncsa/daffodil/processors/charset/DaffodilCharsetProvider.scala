/* Copyright (c) 2012-2017 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.charset

import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.charset.spi.CharsetProvider

/**
 * This implements a CharsetProvider that implements Daffodil's support
 * for less-than-byte-sized encodings. However, it seems hard to get
 * additional encodings to actually be recognized by the JVM, as if this
 * extension mechanism was really intended for JVM maintainers to use, but
 * not for end-user jar libraries to use.
 *
 * Rather than contort to try to get this working, We simply call this
 * directly from a CharsetUtils method that we use instead of Charset.forName.
 */
object DaffodilCharsetProvider extends CharsetProvider {
  import collection.JavaConverters._

  /**
   * Creates an iterator that iterates over the charsets supported by this
   * provider.  This method is used in the implementation of the
   * `java.nio.charset.Charset.availableCharsets()` method.
   *
   * @return  The new iterator
   */
  override def charsets(): java.util.Iterator[Charset] = {
    addedCharsets.toIterator.asJava
  }

  // keep in case we need to put this check back in temporarily
  // private val noWSNoLowerCasePatern = """[^\sa-z]+""".r.pattern // one or more characters, not whitespace, not lower case.

  private lazy val addedCharsets = addedCharsetsMap.map { _._2 }

  private val addedCharsetsMap: Map[String, Charset] =
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
  override def charsetForName(charsetName: String): Charset = {
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
    val cs: Charset = lookupResult.getOrElse(null)
    cs
  }

}

