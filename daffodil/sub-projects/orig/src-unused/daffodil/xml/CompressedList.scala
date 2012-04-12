package daffodil.xml

import java.util.Collection

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

import daffodil.exceptions.UnimplementedException

class CompressedList(parent:CompressableElement) extends java.util.List[CompressableNode] {

  override def get(index: Int) = throw new UnimplementedException(null)//DBUtil getChild(parent,index)

  override def isEmpty = size == 0

  override def size = DBUtil getNumberChildren(parent)

  override def iterator = throw new UnimplementedException(null)//DBUtil getChildrenIterator(parent)


  override def clear = throw new UnimplementedException(null)

  override def subList(p1: Int, p2: Int) = throw new UnimplementedException(null,null,null,None)

  override def listIterator(p1: Int) = throw new UnimplementedException(null,null,null,None)

  override def listIterator = throw new UnimplementedException(null,null,null,None)

  override def lastIndexOf(p1: Any) = throw new UnimplementedException(null,null,null,None)

  override def indexOf(p1: Any) = throw new UnimplementedException(null,null,null,None)

  override def remove(p1: Int) = throw new UnimplementedException(null,null,null,None)

  override def add(p1: Int, p2: CompressableNode) = throw new UnimplementedException(null,null,null,None)

  override def set(p1: Int, p2: CompressableNode) = throw new UnimplementedException(null,null,null,None)

  override def addAll(p1: Int, p2: Collection[_ <: CompressableNode]) = throw new UnimplementedException(null,null,null,None)

  override def retainAll(p1: Collection[_]) = throw new UnimplementedException(null,null,null,None)

  override def removeAll(p1: Collection[_]) = throw new UnimplementedException(null,null,null,None)

  override def addAll(p1: Collection[_ <: CompressableNode]) = throw new UnimplementedException(null,null,null,None)

  override def containsAll(p1: Collection[_]) = throw new UnimplementedException(null,null,null,None)

  override def remove(p1: Any) = throw new UnimplementedException(null,null,null,None)

  override def add(p1: CompressableNode) = throw new UnimplementedException(null,null,null,None)

  override def toArray[T](x: Array[T with java.lang.Object]):Array[T with java.lang.Object] =
      throw new UnimplementedException(null,null,null,None)

  override def toArray = throw new UnimplementedException(null,null,null,None)

  override def contains(p1: Any) = throw new UnimplementedException(null,null,null,None)

}