package edu.illinois.ncsa.daffodil.util

import java.util.concurrent.ArrayBlockingQueue
import org.junit.Test
import org.junit.Assert._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import edu.illinois.ncsa.daffodil.exceptions.Assert

private class TestInvertControl {

  /**
   * Simulates some library that takes a callback function
   *
   * Note that this library is allowed to be NON THREAD SAFE.
   */
  class NotThreadSafeLibrary[T]() {
    def doIt(l: Seq[T]) {
      println("NotThreadSafeLibrary running")
      l.foreach { x =>
        println("NotThreadSafeLibrary calling back with " + x)
        handleEvent(x)
      }
      println("NotThreadSafeLibrary done (normal)")
    }

    private var handleEvent: T => Any = _

    def setHandler(f: T => Any) {
      handleEvent = f
    }
  }

  /**
   * Illustrates how to use the InvertControl class
   */
  @Test def test1() {

    val cb = new NotThreadSafeLibrary[Int]() // some non-safe library that gets a callback handler
    //
    // Wrap the initial call that starts the library in the
    // InvertControl class.
    //
    val iter = new InvertControl[Int]({
      // this argument is the code to call to run the library
      cb.doIt(List(1, 2, 3)) // not executed until we start iterating 
    })

    //
    // define a handler as the library requires
    //
    def handler(i: Int) {
      println("handler called on: " + i)
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

    println("asking for first element")
    var i = iter.next() // library runs until first callback.
    println("got first element")
    assertEquals(1, i)
    println("asking for second element")
    i = iter.next()
    println("got second element")
    assertEquals(2, i)
    println("asking for third element")
    i = iter.next()
    println("got third element")
    assertEquals(3, i)
    assertFalse(iter.hasNext)
    println("done")
  }

  /**
   * Illustrates exception in generator ends up back on the consumer.
   */
  @Test def test2() {

    val cb = new NotThreadSafeLibrary[Int]() // some non-safe library that gets a callback handler
    //
    // Wrap the initial call that starts the library in the
    // InvertControl class.
    //
    val iter = new InvertControl[Int]({
      // this argument is the code to call to run the library
      cb.doIt(List(1, 2, 3)) // not executed until we start iterating 
    })

    //
    // define a handler as the library requires
    //
    def handler(i: Int) {
      println("handler called on: " + i)
      //
      // handler must call the callback function of the InvertControl
      // instance
      //
      if (i == 3) {
        val e = new Exception("you had to give me a three?")
        println("NotThreadSafeLibrary throwing :" + e)
        throw e
      }
      iter.setNext(i)
    }
    //
    // Don't forget to tell the library that this is your handler
    //
    cb.setHandler(handler) // setting callback handler.

    println("asking for first element")
    var i = iter.next() // library runs until first callback.
    println("got first element")
    assertEquals(1, i)
    println("asking for second element")
    i = iter.next()
    println("got second element")
    assertEquals(2, i)
    println("asking for third element")
    try {
      i = iter.next()
      fail()
    } catch {
      case e: Exception =>
        println("consumer caught exception: " + e)
    }
    assertFalse(iter.hasNext)
    println("consumer done")
  }

}