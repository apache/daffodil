package edu.illinois.ncsa.daffodil.processors.charset

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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


import org.scalatest.junit.JUnitSuite
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.tdml.Document

class TestUSASCII7BitPacked extends JUnitSuite {

  @Test def testOne7Bit() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(<document><documentPart type="bits">0110100</documentPart></document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    // println(outstr)
    assertEquals("4", outstr)
  }

  @Test def test7Bit42() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(<document><documentPart type="bits">0110100 0 110010 00</documentPart></document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    // println(outstr)
    assertEquals("42", outstr)
  }

  @Test def test7Bit1234567890() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(<document><documentPart type="bits"><![CDATA[
       0110001 0110010 0110011 0110100 0110101 0110110 0110111 0111000 0111001 0110000
    ]]></documentPart></document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    // println(outstr)
    assertEquals("1234567890", outstr)
  }

}