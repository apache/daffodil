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

package org.apache.daffodil.oolag

import org.junit.Assert._
import org.apache.daffodil.oolag.OOLAG._
import org.junit.Test
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.Abort
import org.apache.daffodil.Implicits._
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._

class MyException(msg: String)
  extends Diagnostic(Nope, Nope, Nope, Maybe(msg)) {
  override def isError = true
  override def modeName = "Parse"
}

abstract class MyBase(parentArg: MyBase)
  extends OOLAGHostImpl(parentArg) {

  def a1 = a1_.value
  def a1_ = LV('a1) {

    // println("evaluating a1")
    "a1 value"
  }

  def a2 = a2_.value
  def a2_ = LV('a2) {

    // println("evaluating a2")
    val msg = "a2 failed with an exception"
    // println(msg)
    val e = new MyException(msg)
    // e.printStackTrace()
    throw e
    // "a2 value"
  }

}

class MySubHost(name: String, parent: MyBase)
  extends MyBase(parent) {
  requiredEvaluationsAlways(a1)
}

class MyHost extends MyBase(null) {

  requiredEvaluationsIfActivated(a1)
  // LoggingDefaults.setLoggingLevel(LogLevel.Debug)

  lazy val subHostCreator = {

    val subHost1 = new MySubHost("subHost1", this)
    val subHost2 = new MySubHost("subHost2", this)
    (subHost1, subHost2)
  }

  def a3 = a3_.value
  def a3_ = LV('a3) {
    // println("My LV name is " + LV.name)
    "a3 value"
  }

  def a4 = a4_.value
  def a4_ = LV('a4) {
    // println("My LV name is " + LV.name)
    a3
  }

  def circ1: Int = circ1_.value
  private def circ1_ = LV('circ1) {
    circ2
  }

  def circ2: Int = circ2_.value
  private def circ2_ = LV('circ2) {
    circ1
  }

  def abortInside = abortInside_.value
  private def abortInside_ = LV('abortInside) {
    abend
  }

  def abend = abend_.value
  private def abend_ = LV('err) {
    Assert.abort("supposed to abort here")
  }

  var x = 0

  def divZero = divZero_.value
  def divZero_ = LV('divZero) {
    5 / x
  }

  def warnTest = warnTest_.value
  def warnTest_ = LV('warnTest) {
    if (x < 1) {
      val diag = new MyException("warnTest")
      warn(diag)
    }
    x
  }

}

class TestOOLAG {

  @Test def testPrettyName() {
    val h = new MyHost
    h.setRequiredEvaluationsActive()
    assertEquals("MyHost", h.diagnosticDebugName)
  }

  @Test def testSuccessLazyVal() {
    val h = new MyHost
    h.setRequiredEvaluationsActive()
    // println("get the LV")

    val a1lv = h.a1_
    assertFalse(a1lv.hasError)
    OOLAG.keepGoing({}) {
      val a1v = h.a1
      // println("value of a1_ is: " + a1lv)
      assertEquals("a1 value", a1v)
    }
    // println(h.errorLVs)
    assertFalse(a1lv.hasError)
    assertFalse(h.isError)
  }

  @Test def testFailLazyVal() {
    val h = new MyHost
    h.setRequiredEvaluationsActive()

    OOLAG.keepGoing({}) {
      // println("ask host for the LV")
      h.a2
      //    catch {
      //      case e : Throwable => println("got exception " + e)
      //    }
      // println("now test if it is an error")
    }
    val isErrorA2 = h.isError

    assertTrue(isErrorA2)
  }

  // Note: Below commented out, as this reflection/stack-trace stuff
  // was deemed too fragile, and troublesome in a lazy-evaluation world
  // So the name must now, annoyingly, be passed to LVs.
  //  /**
  //   * Test mechanism by which LVs figure out their own method names
  //   *
  //   * That is, when you write lazy val foo = LV{...} the code can
  //   * get the name foo.
  //   *
  //   * This depends on some reflection stuff underneath which might change if
  //   * scala compilation model evolves. So if these tests break, then search
  //   * OOLAG.scala for "magic number" and see what is going on there.
  //   */
  //  @Test def testLVName() {
  //    val h = new MyHost
  //    h.setRequiredEvaluationsActive()
  //    // println("ask for the value")
  //    val a3: String = h.a3.value
  //    val a3Name = h.a3.name
  //    assertEquals("a3 value", a3)
  //    assertEquals("a3", a3Name)
  //  }

  /**
   * Make sure one LV calling another doesn't confuse things.
   */
  @Test def testLVName2() {
    val h = new MyHost
    h.setRequiredEvaluationsActive()
    var res: String = null
    OOLAG.keepGoing({}) {
      // println("ask for the value")
      res = h.a4 + h.a3
    }
    assertEquals("a3 valuea3 value", res)
  }

  @Test def testCircularDefinitionDetected() {
    val h = new MyHost
    h.setRequiredEvaluationsActive()
    var e: Exception = null
    OOLAG.keepGoing({}) {
      e = intercept[CircularDefinition] {
        h.circ1
        fail()
      }
    }
    val msg = e.getMessage()
    println(e)
    assertTrue(msg.toLowerCase().contains("circ1"))
  }

  @Test def testAlreadyTried() {
    val h = new MyHost
    h.setRequiredEvaluationsActive()
    assertTrue(h.a2_.isError)
    val e = intercept[AlreadyTried] {
      h.a2_.value
    }
    e match {
      case at: AlreadyTried => {
        val m = e.getMessage()
        assertTrue(m.contains("a2"))
      }
      case _ => fail()
    }
  }

  @Test def testThrowToTopLevel() {
    val h = new MyHost
    h.setRequiredEvaluationsActive()
    intercept[Abort] {
      OOLAG.keepGoing({}) {
        h.abortInside // should print useful lazy val nest messages to log
      }
    }
  }

  @Test def testDivZeroInside() {
    // LoggingDefaults.setLoggingLevel(LogLevel.OOLAGDebug)
    val h = new MyHost
    h.setRequiredEvaluationsActive()
    // println("done constructing MyHost")
    h.a1
    h.subHostCreator
    intercept[Exception] {
      OOLAG.keepGoing({}) {
        h.divZero
      }
    }
    // println("Message: " + e.getMessage())
    // assertTrue(h.isError)
    assertTrue(h.divZero_.isError)

    val d = h.diagnostics
    d.foreach { System.err.println(_) }
    // Division by zero is not caught by oolag anymore, so there will be
    // no diagnostic message.
    // assertTrue(d.length == 1)
  }

  @Test def testAutoTreeCreate() {
    val h = new MyHost
    h.setRequiredEvaluationsActive()
    OOLAG.keepGoing({}) {
      h.subHostCreator
      h.a2
    }
    assertTrue(h.isError)
    val errs = h.diagnostics
    println(errs)

  }
}
