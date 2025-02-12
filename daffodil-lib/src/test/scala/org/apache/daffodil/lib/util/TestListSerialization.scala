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

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class Thing() extends Serializable {
  var things: Seq[Thing] = _
}

class TestListSerialization {

  /**
   * Scala 2.12 handles List serialization in a special way using a
   * SerializationProxy. In special cases such as recursion and self
   * referential lists, this can lead to a ClassCastException during
   * deserialization with an error like:
   *
   *   java.lang.ClassCastException: cannot assign instance of
   *   scala.collection.immutable.List$SerializationProxy to field Thing.things
   *   of type scala.collection.Seq in instance of Thing
   *
   * We have implemented workarounds for this in CompiledExpression1.scala with
   * uses of:
   * 
   *   .map(identity)
   *
   * If this test fails, it likely means Scala has fixed this issue and we can
   * saftely remove the unnecessary .map(identity) workarounds.
   */
  @Test def testListSerializationProxyError(): Unit = {
    val a = new Thing()
    val b = new Thing()
    val l = Seq(b)
    a.things = l
    b.things = l

    // Serialize and Deserialize Thing A
    val baos = new java.io.ByteArrayOutputStream()
    val oos = new java.io.ObjectOutputStream(baos)
    oos.writeObject(a)
    oos.close()
    baos.close()
    val bytes = baos.toByteArray()
    val bais = new java.io.ByteArrayInputStream(bytes)
    val ois = new java.io.ObjectInputStream(bais)

    val newA =
      try {
        ois.readObject()
        fail("Expected ClassCastException to be thrown")
      } catch {
        case e: ClassCastException => {
          assertTrue(e.getMessage.contains("SerializationProxy"))
          assertTrue(e.getMessage.contains("Seq"))
          assertTrue(e.getMessage.contains("Thing"))
        }
        case _: Throwable => fail("Expected ClassCastException to be thrown")
      }
  }

  /**
   * Shows that the above issue can be worked around by creating a copy of the
   * list via .map(identity) of the serialized list
   */
  @Test def testListSerializationProxyErrorWorkaround(): Unit = {
    val a = new Thing()
    val b = new Thing()
    val l = Seq(b)
    a.things = l.map(identity)
    b.things = l.map(identity)

    // Serialize and Deserialize Thing A
    val baos = new java.io.ByteArrayOutputStream()
    val oos = new java.io.ObjectOutputStream(baos)
    oos.writeObject(a)
    oos.close()
    baos.close()
    val bytes = baos.toByteArray()
    val bais = new java.io.ByteArrayInputStream(bytes)
    val ois = new java.io.ObjectInputStream(bais)

    ois.readObject().asInstanceOf[Thing]
  }

}
