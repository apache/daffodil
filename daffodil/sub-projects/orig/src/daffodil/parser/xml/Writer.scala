package daffodil.parser.xml

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
import scala.collection.mutable.Map

import org.jdom.Element

/**
 * A writer for outputting XML that keeps track of where (character number) in the output an XML element is.
 * This class is useful for printing error messages and debugging information.
 *
 * @param writer the writer to wrap
 *
 * @author Alejandro Rodriguez
 * @version 1
 */
class Writer(writer:java.io.Writer) extends java.io.Writer {
  
  private var chars:Int = 0
  
  private val positions:Map[Element,(Int,Int)] = Map()

  def setElement(element:Element,level:Int) =
    positions += ((element,(chars,level)))
  
  def getPosition(element:Element) = 
	  positions(element)._1
  
  def getLevel(element:Element) = 
    positions(element)._2
  
  override def close = writer close
  
  override def flush = writer flush
  
  override def write(cbuf:Array[Char]) = {
    writer write(cbuf)
    chars += cbuf.length
  }   
    
  override def write(cbuf:Array[Char],off:Int,len:Int) = {
    writer write(cbuf,off,len)
    chars += len
  } 

  override def write(c:Int) {
    writer write(c)
    chars += 1
  }
  
  override def write(s:String) = {
    writer write(s)
    chars += s.length
  }
  
  override def write(s:String,off:Int,len:Int) = {
    writer write(s,off,len)
    chars += len
  } 
  
  override def append(c:Char) = {
    writer append(c)
    chars += 1
    this
  } 
  
  override def append(csq:CharSequence) = {
    writer append(csq)
    chars += csq.length
    this
  } 
        
  override def append(csq:CharSequence,start:Int,end:Int) = {
    writer append(csq,start,end)
    chars += end-start
    this
  } 
  
 }
