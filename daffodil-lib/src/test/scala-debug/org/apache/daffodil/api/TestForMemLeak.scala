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

