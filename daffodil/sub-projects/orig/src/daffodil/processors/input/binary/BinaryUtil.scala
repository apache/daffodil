package daffodil.processors.input.binary

import java.nio.ByteBuffer

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies 
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its 
 *     contributors may be used to endorse or promote products derived from this 
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */
 
/* 
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu > 
 * Date: 2010
 */

object BinaryUtil {

  def bcdToNumber(buffer:ByteBuffer):String = {
    val result = new StringBuilder

    for(i <- 1 to buffer.limit){
      val byte = buffer.get
      val lowestPart = byte & 0xF0
      val highestPart = byte & 0x0F

      lowestPart match {
        case 0x00 => result.append("0")
        case 0x10 => result.append("1")
        case 0x20 => result.append("2")
        case 0x30 => result.append("3")
        case 0x40 => result.append("4")
        case 0x50 => result.append("5")
        case 0x60 => result.append("6")
        case 0x70 => result.append("7")
        case 0x80 => result.append("8")
        case 0x90 => result.append("9")
        case _ =>   throw new IllegalArgumentException(byte.toString)
      }

      highestPart match {
        case 0x00 => result.append("0")
        case 0x01 => result.append("1")
        case 0x02 => result.append("2")
        case 0x03 => result.append("3")
        case 0x04 => result.append("4")
        case 0x05 => result.append("5")
        case 0x06 => result.append("6")
        case 0x07 => result.append("7")
        case 0x08 => result.append("8")
        case 0x09 => result.append("9")
        case _ =>   throw new IllegalArgumentException(byte.toString)
      }
    }

    result.toString
  }
}