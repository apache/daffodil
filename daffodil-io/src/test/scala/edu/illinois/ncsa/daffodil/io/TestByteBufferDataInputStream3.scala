/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.io

import org.junit.Test
import org.junit.Assert._

class TestByteBufferDataInputStream3 {

  val Dump = new DataDumper

  @Test def dumpVisible1 {
    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-8")
    val lengthInBits = bytes.length * 8
    val dis = ByteBufferDataInputStream(bytes)
    dis.setDebugging(true)
    val fb = dis.futureData(48)

    val dumpString =
      Dump.dump(Dump.MixedHexLTR(Some("utf-8")), 0, lengthInBits, fb,
        includeHeadingLine = true).mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000000: 4461 7465 20e5 b9b4 e69c 88e6 97a5 3d32  D~a~t~e~␣~年~~~~月~~~~日~~~~=~2~
00000010: 3030 33e5 b9b4 3038 e69c 8832 37e6 97a5  0~0~3~年~~~~0~8~月~~~~2~7~日~~~~
"""
    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def dumpVisible2 {
    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-8")
    val lengthInBits = bytes.length * 8
    val dis = ByteBufferDataInputStream(bytes)
    dis.setDebugging(true)
    val fb = dis.futureData(48)

    val dumpString =
      Dump.dump(Dump.MixedHexLTR(Some("utf-8")), 16 * 8, lengthInBits, fb,
        includeHeadingLine = true).mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000010: 4461 7465 20e5 b9b4 e69c 88e6 97a5 3d32  D~a~t~e~␣~年~~~~月~~~~日~~~~=~2~
00000020: 3030 33e5 b9b4 3038 e69c 8832 37e6 97a5  0~0~3~年~~~~0~8~月~~~~2~7~日~~~~
"""

    assertEquals(expected, "\n" + dumpString + "\n")
  }

  @Test def dumpVisible3 {
    val bytes = "Date 年月日=2003年08月27日".getBytes("utf-8")
    val lengthInBits = bytes.length * 8
    val dis = ByteBufferDataInputStream(bytes)
    dis.setDebugging(true)
    val fb = dis.futureData(bytes.length)

    val dumpString =
      Dump.dump(Dump.MixedHexLTR(Some("utf-8")), 20 * 8, lengthInBits, fb,
        includeHeadingLine = true).mkString("\n")
    val expected = """
87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
00000010:           4461 7465 20e5 b9b4 e69c 88e6          D~a~t~e~␣~年~~~~月~~~~日
00000020: 97a5 3d32 3030 33e5 b9b4 3038 e69c 8832  ~~~~=~2~0~0~3~年~~~~0~8~月~~~~2~
00000030: 37e6 97a5                                7~日~~~~                        
"""
    assertEquals(expected, "\n" + dumpString + "\n")
  }
}
