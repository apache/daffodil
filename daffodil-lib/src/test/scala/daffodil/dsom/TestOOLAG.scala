package daffodil.dsom

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import daffodil.dsom.OOLAG._
import daffodil.util.LoggingDefaults
import daffodil.util.LogLevel
import daffodil.util.LogLevel
import org.junit.Test

class MyHost extends OOLAGHost {
  def LV = LVFactory(this)

  // LoggingDefaults.setLoggingLevel(LogLevel.Debug)

  def prettyName = "myName"
  def path = prettyName

  def handleThrownError(th: Throwable, ov: OOLAGValue) {
    // println("handling thrown error")
    // e.printStackTrace
  }

  def handleWarning(ov: OOLAGValue, th: Throwable) {
    // println("handling warning")
    // th.printStackTrace()
  }

  lazy val a1 = LV {
    // println("evaluating a1")
    "a1 value"
  }

  lazy val a2 = LV {
    // println("evaluating a2")
    val msg = "a2 failed with an exception"
    // println(msg)
    val e = new Exception(msg)
    // e.printStackTrace()
    throw e
    "a2 value"
  }

  lazy val a3 = LV {
    // println("My LV name is " + LV.name)
    "a3 value"
  }

  lazy val a4 = LV {
    // println("My LV name is " + LV.name)
    a3.value
  }

}

class TestOOLAG extends JUnitSuite {

  @Test def testSuccessLazyVal() {
    val h = new MyHost
    // println("get the LV")
    val a1LV = h.a1
    // println("now evaluate the LV")
    val a1: String = a1LV.value
    // println("value of LV is: " + a1)
    assertEquals("a1 value", a1)
    assertFalse(h.a1.isError)
  }

  @Test def testFailLazyVal() {
    val h = new MyHost
    // println("ask host for the LV")
    val a2LV = h.a2
    //    catch {
    //      case e : Throwable => println("got exception " + e)
    //    }
    // println("now test if it is an error")
    val isErrorA2 = a2LV.isError
    assertTrue(isErrorA2)
  }

  /**
   * Test mechanism by which LVs figure out their own method names
   *
   * That is, when you write lazy val foo = LV{...} the code can
   * get the name foo.
   *
   * This depends on some reflection stuff underneath which might change if
   * scala compilation model evolves. So if these tests break, then search
   * OOLAG.scala for "magic number" and see what is going on there.
   */
  @Test def testLVName() {
    val h = new MyHost
    // println("ask for the value")
    val a3: String = h.a3.value
    val a3Name = h.a3.name
    // println("a3's name is " + a3Name)
    assertEquals("a3 value", a3)
    assertEquals("a3", a3Name)
  }

  /**
   * Make sure one LV calling another doesn't confuse things.
   */
  @Test def testLVName2() {
    val h = new MyHost
    // println("ask for the value")
    val a4: String = h.a4.value
    val a3: String = h.a3.value
    val a4Name = h.a4.name
    // println("a4's name is " + a4Name)
    assertEquals("a3 value", a4)
    assertEquals("a4", a4Name)
  }

}