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

//TODO Scala 2.8 LinkedList makes this whole class redundant. Compare performance replacing this class.  

/**
 * A mutable linked list optimize for appending
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
class LinkedList[A](val element:A){
  private val head = this
  private var tail = this
  private var next:LinkedList[A] = null

  /**
   * Appends a list
   */
  def append(l:LinkedList[A]):LinkedList[A] = {
    if (l!=null){
      if (tail==this)
        tail = l
      else
        tail = tail append l
      
      if (next==null)
      next = l
    }
    tail
  }

  def size = {
    var count = 1
    var node = next
    while (node!=null){
      count += 1
      node = node.next
    }
    count
  }

  def foreach(f:(A) => Unit):Unit = {
    f(element)
    if (next != null)
      next foreach(f)
  }

  /**
   * Returns the first element of the list
   */
  def first = element

  /**
   * Returns the last element of the list
   */
  def last = tail.element
}
