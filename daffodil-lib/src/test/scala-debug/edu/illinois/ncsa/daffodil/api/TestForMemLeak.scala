/**
 * This code commented out because sbt was crashing.
 *
 * It might be useful in the future so we should keep it around like this.
 */
//package edu.illinois.ncsa.daffodil.api
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

