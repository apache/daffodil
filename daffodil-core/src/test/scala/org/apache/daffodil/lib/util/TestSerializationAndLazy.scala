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

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import org.junit.Assert._
import org.junit.Test

class ToSerialize extends Serializable {

  val v = 5
  var lazyValWasEvaluated = false
  lazy val x = {
    // println("v is " + v)
    lazyValWasEvaluated = true
    2 * v
  }

}

/**
 * This test shows that use of lazy val does not interfere with object serialization.
 * An unevaluated lazy val remains that way across serialization/deserialization.
 *
 * We're going to be very dependent on this not being broken by some Scala release patch, so leave
 * this test in just in case of that so we can detect it.
 */
class TestSerializationAndLazy {

  @Test
  def testSerializeBeforeLazyEval(): Unit = {
    val instance = new ToSerialize
    val baos = new ByteArrayOutputStream
    val stream = new ObjectOutputStream(baos)
    stream.writeObject(instance)
    stream.flush()
    stream.close()
    assertFalse(instance.lazyValWasEvaluated)
    val ba = baos.toByteArray()
    val bais = new ByteArrayInputStream(ba)
    val istream = new ObjectInputStream(bais)
    val restoredInstance = istream.readObject()
    istream.close()
    assertTrue(restoredInstance.isInstanceOf[ToSerialize])
    val ts = restoredInstance.asInstanceOf[ToSerialize]
    assertFalse(ts.lazyValWasEvaluated)
    ts.x
    assertTrue(ts.lazyValWasEvaluated)
  }

}
