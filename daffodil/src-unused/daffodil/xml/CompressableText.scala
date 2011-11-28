package daffodil.xml

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
 * User: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu > 
 * Date: 2010
 */

import org.jdom.Parent
import org.jdom.Text


class CompressableText(s:String)  extends Text(s) with CompressableNode {

  private var compressed = false

  override def setImmediateAncestor(parent:Parent) = setParent(parent)

  override def decompress:Unit =
    if (!compressed){
      DBUtil restore(this)
      compressed = false
    }


  override def compress = {
    if (!compressed){
      id = DBUtil store(this)
      compressed = true
    }
    if (getID<0)
      throw new AssertionError("Compressed but got no ID")
//      if (!compressed){
//        getParent match {
//          case e:CompressableNode => id = DBUtil store(this,e getID,nodePosition)
//          case _ => id = DBUtil store(this,-1,0)
//        }
//
//        compressed = true
//        setParent(null)
//    }
  }

  override def isCompressed = compressed

//  override def setCompressed(compressed:Boolean) = this.compressed = compressed
}
