/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

/**
 * This code commented out because sbt was crashing.
 *
 * It might be useful in the future so we should keep it around like this.
 */
//package org.apache.daffodil.api
//
//import junit.framework.Assert._
//import org.junit.Test
//
//class Delay(v: => Any) {
//  lazy val value = { v }
//}
//
//class L {
//  var list: Seq[Delay] = Nil
//}
//
//class H(l: L) {
//  def R(a0: => Any): Unit = {
//    l.list :+= new Delay(a0)
//  }
//}
//
//class Q(body: => String) {
//  val wasteSomeMemory = new Array[Byte](500000) // makes the memory leak obvious
//  def value = body
//}
//
//class TestForMemLeak {
//
//  @Test def testLeak() {
//    1 to 100000 foreach { _ =>
//      System.gc()
//      Thread.sleep(10) // lets jvisualvm get a timeslice so it can open the vm quickly
//      new H(new L) {
//        (attrib.value)
//        val attrib = new Q("foobar")
//      } // Make them, and discard them. 
//    }
//  }
//}

