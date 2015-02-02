/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.charset.Charset
import com.ibm.icu.charset.CharsetICU
import java.nio.charset.IllegalCharsetNameException
import java.io.UnsupportedEncodingException

class DFDLCharset(val charsetName: String) extends Serializable {
  charset // Force charset to be evaluted to ensure it's valid at compile time.  It's a lazy val so it will be evaluated when de-serialized
  @transient lazy val charset = CharsetUtils.getCharset(charsetName)
}

object CharsetUtils {

  def getCharset(charsetName: String): Charset = {
    // We should throw if csn is null. Tolerating this would just lead to bugs.
    Assert.usage(charsetName != null)
    Assert.usage(charsetName != "")

    // There is no notion of a default charset in DFDL.
    // So this can be val.
    val csn: String = charsetName

    val cs = try {
      val cs =
        if (csn.toUpperCase() == "US-ASCII-7-BIT-PACKED") USASCII7BitPackedCharset
        else CharsetICU.forNameICU(csn)
      Some(cs)
    } catch {
      case e: IllegalCharsetNameException => None
    }
    cs.getOrElse(throw new UnsupportedEncodingException(csn))
  }
}

class CharacterSetAlignmentError(csName: String, requiredAlignmentInBits: Int, alignmentInBitsWas: Int)
  extends Exception("Character set %s requires %s alignment (bits), but alignment was %s (bits)".
    format(csName, requiredAlignmentInBits, alignmentInBitsWas))