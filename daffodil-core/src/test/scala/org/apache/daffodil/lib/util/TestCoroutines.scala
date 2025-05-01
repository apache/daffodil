/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.daffodil.lib.util

import org.apache.daffodil.lib.Implicits._

import org.junit.Assert._
import org.junit.Test

private class TestException(e: String) extends Exception(e)

private class TestInvertControl {

  /**
   * We need JInt because Int is not subtype of AnyRef
   * but Coroutines requires T <: AnyRef
   */
  type JInt = java.lang.Integer

  def sprintln(s: String): Unit = {
    /*
     * Uncomment this for verbose messsaging so you can see what is going on.
     */
    // println(s)
  }

  /**
   * Simulates some library that takes a callback function
   *
   * Note that this library is allowed to be NON THREAD SAFE.
   */
  class NotThreadSafeLibrary[T]() {
    def doIt(l: Seq[T]): Unit = {
      sprintln("NotThreadSafeLibrary running")
      l.foreach { x =>
        sprintln("NotThreadSafeLibrary calling back with " + x)
        handleEvent(x)
      }
      sprintln("NotThreadSafeLibrary done (normal)")
    }

    private var handleEvent: T => Any = _

    def setHandler(f: T => Any): Unit = {
      handleEvent = f
    }
  }

  /**
   * Illustrates how to use the InvertControl class
   */
  @Test def test1(): Unit = {

    val cb =
      new NotThreadSafeLibrary[JInt]() // some non-safe library that gets a callback handler
    //
    // Wrap the initial call that starts the library in the
    // InvertControl class.
    //
    val iter = new InvertControl[JInt](
      // this argument is the code to call to run the library
      cb.doIt(List(1, 2, 3))
      // not executed until we start iterating
    )
    assertTrue(iter.isMain)
    //
    // define a handler as the library requires
    //
    def handler(i: JInt): Unit = {
      // sprintln("handler called on: " + i)
      //
      // handler must call the callback function of the InvertControl
      // instance
      //
      iter.setNext(i)
    }
    //
    // Don't forget to tell the library that this is your handler
    //
    cb.setHandler(handler) // setting callback handler.

    sprintln("asking for first element")
    var i = iter.next() // library runs until first callback.
    sprintln("got first element")
    assertEquals(1, i)
    sprintln("asking for second element")
    i = iter.next()
    sprintln("got second element")
    assertEquals(2, i)
    sprintln("asking for third element")
    i = iter.next()
    sprintln("got third element")
    assertEquals(3, i)
    assertFalse(iter.hasNext)
    sprintln("done")
  }

  /**
   * Illustrates exception in generator ends up back on the consumer.
   */
  @Test def test2(): Unit = {

    val cb =
      new NotThreadSafeLibrary[JInt]() // some non-safe library that gets a callback handler
    //
    // Wrap the initial call that starts the library in the
    // InvertControl class.
    //
    lazy val iter: InvertControl[JInt] = new InvertControl[JInt]({
      // this argument is the code to call to run the library
      var wasThrown = false
      try {
        cb.doIt(List(1, 2, 3)) // not executed until we start iterating
      } catch {
        case e: TestException =>
          // iter.setFinal(e)
          wasThrown = true
      }
      assertTrue(wasThrown)
      sprintln("NotThreadSafeLibrary exiting")
    })

    //
    // define a handler as the library requires
    //
    def handler(i: JInt): Unit = {
      sprintln("handler called on: " + i)
      //
      // handler must call the callback function of the InvertControl
      // instance
      //
      if (i == 3) {
        val e = new TestException("you had to give me a three?")
        sprintln("NotThreadSafeLibrary throwing :" + e)
        throw e
      }
      iter.setNext(i)
    }
    //
    // Don't forget to tell the library that this is your handler
    //
    cb.setHandler(handler) // setting callback handler.

    sprintln("asking for first element")
    var i = iter.next() // library runs until first callback.
    sprintln("got first element")
    assertEquals(1, i)
    sprintln("asking for second element")
    i = iter.next()
    sprintln("got second element")
    assertEquals(2, i)
    sprintln("asking for third element")
    val e = intercept[NoSuchElementException] {
      i = iter.next()
      fail()
    }
    sprintln("consumer caught exception: " + e)
    assertFalse(iter.hasNext)
    sprintln("consumer done")
  }

}
