package edu.illinois.ncsa.daffodil.util

import java.util.concurrent.ArrayBlockingQueue
import org.junit.Test
import org.junit.Assert._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.Implicits._

private class TestException(e: String) extends Exception(e)

private class TestInvertControl {

  /**
   * We need JInt because Int is not subtype of AnyRef
   * but Coroutines requires T <: AnyRef
   */
  type JInt = java.lang.Integer

  def sprintln(s: String) = {
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
    def doIt(l: Seq[T]) {
      sprintln("NotThreadSafeLibrary running")
      l.foreach { x =>
        sprintln("NotThreadSafeLibrary calling back with " + x)
        handleEvent(x)
      }
      sprintln("NotThreadSafeLibrary done (normal)")
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

    val cb = new NotThreadSafeLibrary[JInt]() // some non-safe library that gets a callback handler
    //
    // Wrap the initial call that starts the library in the
    // InvertControl class.
    //
    val iter = new InvertControl[JInt]({
      // this argument is the code to call to run the library
      cb.doIt(List(1, 2, 3)) // not executed until we start iterating
    })

    //
    // define a handler as the library requires
    //
    def handler(i: JInt) {
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
  @Test def test2() {

    val cb = new NotThreadSafeLibrary[JInt]() // some non-safe library that gets a callback handler
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
          iter.setFinal(e)
          wasThrown = true
      }
      assertTrue(wasThrown)
      sprintln("NotThreadSafeLibrary exiting")
    })

    //
    // define a handler as the library requires
    //
    def handler(i: JInt) {
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
    val e = intercept[TestException] {
      i = iter.next()
      fail()
    }
    sprintln("consumer caught exception: " + e)
    val msg = e.getMessage()
    assertTrue(msg.contains("you had to give me a three?"))
    assertFalse(iter.hasNext)
    sprintln("consumer done")
  }

}
