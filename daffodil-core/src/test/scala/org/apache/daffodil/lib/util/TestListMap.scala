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

import scala.collection.immutable.ListMap
import scala.util.Random

import org.junit.Assert.assertEquals
import org.junit.Test

class TestListMap {

  /*
   * The purpose of this test is to ensure that ListMap maintains insertion
   * order. A ListMap is used in the IIMap structure, but it is important that
   * it maintains insertion order. From the limited documentation, there does
   * not appear to be a contract that ListMap maintain order. However, in
   * practice, it is implemented as a linked list that does maintain order.
   * This test ensures that this behavior does not change.
   */
  @Test def test_listMap() = {
    val orig = Random.shuffle((0 until 1000).toList)
    val mt: ListMap[Int, String] = ListMap.empty
    val listMap: ListMap[Int, String] = orig.foldLeft(mt) { (lm, n) =>
      lm + (n -> n.toString)
    }

    // test that removals still maintain order
    val removeKey = Random.nextInt(1000)
    val smallerOrig = orig.filter(_ != removeKey)
    val smallerMap = listMap - removeKey

    val zipped = smallerOrig.zip(smallerMap)
    zipped.foreach { case (o, (k, v)) => assertEquals(o, k) }
  }
}
