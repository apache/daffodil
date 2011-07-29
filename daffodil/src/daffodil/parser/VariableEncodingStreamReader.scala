package daffodil.parser

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

import java.io.InputStream
import java.io.Reader
import java.io.IOException
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.nio.charset.CoderResult

/**
 * A Reader that allows changing the character encoding on the fly.
 *
 * @author Alejandro Rodriguez
 * @version 1
 */
class VariableEncodingStreamReader(input:InputStream,charset:Charset) extends Reader{

  private var decoder:CharsetDecoder = charset newDecoder()
  private var _charset:Charset = charset
  private val inputBuffer = ByteBuffer allocate(16)
  private val charBuffer = CharBuffer allocate(1)
  private var bytesRead = 0
  
  
  override def read():Int = {
    charBuffer position(0)
    bytesRead = 0    
    inputBuffer limit(0)
    
    while(charBuffer.position()<1){
      decoder reset()
      inputBuffer position(inputBuffer limit())
      inputBuffer limit(inputBuffer.limit()+1)
      val c = input read()
      if (c == -1)
        return -1
			
      bytesRead += 1
      inputBuffer put(c toByte);
      inputBuffer flip();

      val cr = decoder decode(inputBuffer, charBuffer, false)
			
      assert( !cr.isOverflow())
      if (cr isError())
        throw new IOException()
    }

    //println("I read '"+charBuffer.get(0)+"'")
    
		
    charBuffer get(0)
  }
  
  override def close() = {}

  override def read(cbuf:Array[Char],offset:Int,len:Int):Int = {
    val charBuffer = CharBuffer wrap(cbuf,offset,len)
    val inputBuffer = ByteBuffer allocate(charBuffer.length * (1/decoder.maxCharsPerByte).toInt+1)

    val bytesRead = input.read(inputBuffer.array,offset,len)
    inputBuffer.position(0)
    inputBuffer.limit(bytesRead)

    val cr = decoder decode(inputBuffer, charBuffer, false)
    if (cr isError())
        throw new IOException()

    //println("I read '"+bufferToString(cbuf,0,charBuffer.position)+"'")
    
    charBuffer.position
  }

  private def bufferToString(cbuf:Array[Char],i:Int,l:Int):String =
    if (i==l) ""
      else
    cbuf(i) + bufferToString(cbuf,i+1,l)

//  override def read(cbuf:Array[Char],offset:Int,len:Int):Int = {
//    var t = 0
//    var totalRead = 0
//    for(i <- 0 until len){
//      val c = read()
//      totalRead += bytesRead
//      if (c == -1)
//        return t match { case 0 => -1; case _ => t }
//      cbuf(i+offset) = c.toChar
//      t += 1
//    }
//    bytesRead = totalRead
//    t
//  }
  
  /** Returns the number of bytes read from the stream in the last read operation*/
  def getBytesRead:Int = bytesRead
  
  def setCharset(charset:Charset) = {
    _charset = charset
    decoder = charset newDecoder()
  }

  /** Returns the size of a string in bytes in the current encoding */
  def size(string:String) = 
    charset.encode(string).limit 
  
}
