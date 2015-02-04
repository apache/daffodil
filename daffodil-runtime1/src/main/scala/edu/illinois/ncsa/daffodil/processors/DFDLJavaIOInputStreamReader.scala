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

package edu.illinois.ncsa.daffodil.processors

import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.io.InputStream

object DFDLJavaIOInputStreamReader {

  def apply(in: InputStream, charset: Charset, bitOffset0to7: Int, bitLimit: Long) = {
    new DFDLJavaIOInputStreamReader(in, DFDLJavaIOStreamDecoder.forInputStreamReader(in, charset, bitOffset0to7, bitLimit))
  }

}

/**
 * This class was ported from java in order to use our customized
 * version of the StreamDecoder.
 *
 * This was necessary because in DFDL we want malformed input to be
 * treated as the end of a data stream.
 */
class DFDLJavaIOInputStreamReader private (val in: InputStream, val sd: DFDLJavaIOStreamDecoder)
  extends java.io.Reader(in: InputStream) {

  def getEncoding: String = sd.getEncoding
  override def read: Int = sd.read()
  def read(cbuf: Array[Char], offset: Int, length: Int): Int = sd.read(cbuf, offset, length)
  override def ready: Boolean = sd.ready()
  def close: Unit = sd.close()
}
