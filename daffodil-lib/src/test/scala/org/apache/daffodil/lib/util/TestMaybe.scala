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

class TestMaybe {

  type JINT = java.lang.Long
  var e: JINT = 5

  var t: Long = 0

  def passOne(mi: Maybe[JINT]) = {
    if (mi.isDefined) t += mi.get
  }

  def returnOne(e: JINT) = Maybe(e)

  /**
   * This test can be used to verify that passing Maybe[T] objects
   * and returning Maybe[T] objects doesn't cause allocation to occur.
   *
   * Uncomment and run, then examine with jvisualvm or jprofiler or some
   * tool where you can watch allocation on the main thread.
   */
  //  @Test def testIfMaybeAllocatesOnPassOrReturn {
  //    var i: Int = 0
  //    while (i < 10000000000000L) {
  //      //
  //      // While this is running, watch on jvisualvm or jprofiler
  //      // to see if allocation is occurring on the main thread.
  //      //
  //      // slow allocation on other threads is to be expected.
  //      //
  //      i += 1
  //      passOne(Maybe(e))
  //      val mi = returnOne(e)
  //      if (mi.isDefined) t += mi.get
  //    }
  //    println(t)
  //  }

}
