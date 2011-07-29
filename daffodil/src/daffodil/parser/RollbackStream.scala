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
import java.nio.ByteBuffer

import scala.collection.mutable.Stack
import java.io.{BufferedInputStream, InputStream, InputStreamReader}


/**
 * An InputStream that provides checkpoint and rollback operations to
 * return to arbitrary points back in the stream.
 * 
 * Unlike a regular input stream, this class provides stackable marks
 * that allow to keep an arbitrary number of checkpoints in the stream.
 *
 * @param input the input stream wrapped by this RollbackStream
 *
 * @author Alejandro Rodriguez
 * @version 1 
 */
class RollbackStream(input:BufferedInputStream) extends InputStream{

  //TODO Current implementation keeps a fix buffer and so it can jump
  //only so far back in the input.
  //Keeping an arbitrary length buffer is prohibitive in memory cost,
  //but if the input stream comes from a file, it should be possible
  //to reopen the file and skip forward to the desired position, providing
  //arbitrary long step backs

  /** The current position in the input stream */
  private var position:Long = 0
  /** A stack with the marks kept */
  private var commits:Stack[Long] = new Stack()
  /** A circular buffer of bytes read from the input */
  private val buffer = new Array[Byte](1024*1024)

  /** Current position in the buffer pointing to next byte to return to user */
  private var c = 0
  /** Tail of the buffer (where next byte read from input should be put) */
  private var e = 0
  /** Valid bytes in the buffer */
  private var size:Long = 0
  /** Max position read from the input stream so far*/
  private var total:Long = 0

  /** Creates a new checkpoint. */
  def checkpoint():Unit = 
    commits push(position)
  
  /** Rolls back to the last checkpoint available. Deletes the checkpoint. */
  def rollback():Unit =  {
    val desiredPosition = commits pop()
    val stepBack = position-desiredPosition
    pushBack(stepBack)
  }
//    position = commits pop()


  /** Deletes the newest checkpoint available. */
  def uncheck():Unit = 
    commits pop()
  
  /** Go back in the stream by the given number of bytes*/
  def pushBack(bytes:Long):Unit = {
    if (bytes>size)
      throw new IllegalArgumentException("Pushing too far back ("+bytes+" and buffer size is "+size+")")

    if (bytes<0)
      throw new IllegalArgumentException("Pushing back beyond beginning of input")
    c = (c-bytes.toInt+buffer.length)%buffer.length
    position -= bytes
    size -= bytes
  }

  override def read():Int =
    if (position==total){
      val char:Int = input.read()

      if(char != -1){        
        buffer(e) = char.toByte
        e = (e+1)%buffer.length
        c = (c+1)%buffer.length
        position += 1
        total += 1
        if (size < buffer.length)
          size += 1
      }
      char
    }else{
      val char = byteToUnsignedByte(buffer(c))
      c = (c+1)%buffer.length
      position+=1
      if (size < buffer.length)
        size += 1
      char
    }

  override def read(cbuf:Array[Byte],offset:Int,length:Int):Int = {
    var counted = 0
    for(i <-0 until length){
      val c = read()
      if (c<0)
        return counted
      else
          cbuf(offset+i) = c.toByte
      counted += 1
    }
    counted
  } 
  
  override def close() = {
    input close
  }
  
  /** Returns the current position in the stream (in bytes)*/
  def getPosition = position

  private def byteToUnsignedByte(b:Byte):Int =
    if (b>=0)
      b.toInt
    else
      256+b.toInt
  
}
