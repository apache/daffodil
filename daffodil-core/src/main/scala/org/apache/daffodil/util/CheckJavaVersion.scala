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

package org.apache.daffodil.util

import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.processors.charset.CharsetUtils

class InvalidJavaVersionException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

object CheckJavaVersion {

  def checkJavaVersion(context: ThrowsSDE) = {
    val jVersion = scala.util.Properties.javaVersion
    if (!scala.util.Properties.isJavaAtLeast("1.8")) {
      throw new InvalidJavaVersionException("Daffodil requires Java 8 (1.8) or higher. You are currently running %s".format(jVersion))
    }
    //
    // Test specifically for this particular decoder bug
    // 
    if (CharsetUtils.hasJava7DecoderBug) {
      throw new InvalidJavaVersionException("This Java JVM has the Java 7 Decoder Bug. Daffodil requires Java 8 or higher.")
    }
  }

}
