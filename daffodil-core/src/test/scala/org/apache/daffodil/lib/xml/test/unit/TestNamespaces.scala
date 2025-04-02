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

package org.apache.daffodil.lib.xml.test.unit

import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * Characterize namespace behavior for scala's xml support
 *
 * This is here to illustrate how it works, and our assumptions
 * about how it works.
 */
class TestNamespaces {

  /**
   * basic namespace access from prefix.
   */
  @Test def testScalaNamespace1(): Unit = {
    val xml = <bar xmlns:foo="fooNamespaceID">
                <quux attr1="x"/>
              </bar>
    val foons = (xml \ "quux")(0).getNamespace("foo")
    assertEquals("fooNamespaceID", foons)
  }

  /**
   * Pass null (not "") to retrieve the default NS
   */
  @Test def testScalaNamespace2(): Unit = {
    val xml = <bar xmlns="defaultNamespaceID">
                <quux attr1="x"/>
              </bar>
    val defNS = (xml \ "quux")(0).getNamespace(null)
    assertEquals("defaultNamespaceID", defNS)
  }

  /**
   * When the prefix doesn't correspond to any ns definition
   * you get null back.
   */
  @Test def testScalaNamespace3(): Unit = {
    val xml = <bar>
                <quux attr1="x"/>
              </bar>
    val ns = (xml \ "quux")(0).getNamespace("foo")
    assertEquals(null, ns)
  }

  /**
   * Pass null in order to retrieve the default namespace.
   * If there isn't one, you get null back.
   */
  @Test def testScalaNamespace4(): Unit = {
    val xml = <bar>
                <quux attr1="x"/>
              </bar>
    val ns = (xml \ "quux")(0).getNamespace(null)
    assertEquals(null, ns)
  }

  /**
   * Passing "" to getNamespace() is problematic.
   */
  @Test def testScalaNamespace5(): Unit = {
    val xml = <bar>
                <quux attr1="x"/>
              </bar>
    val ns = (xml \ "quux")(0).getNamespace("")
    assertEquals(null, ns)
  }

  /**
   * Illustrates that you cannot use "" to access the default namespace.
   */
  @Test def testScalaNamespace6(): Unit = {
    val xml = <bar xmlns="defaultNS">
                <quux attr1="x"/>
              </bar>
    val ns = (xml \ "quux")(0).getNamespace("")
    assertEquals(null, ns) // ANNOYING - null, not the default namespace.
  }

  /**
   * This test illustrates the technique for constructing a new
   * xml element that has the same namespaces as some other XML context.
   */
  @Test def testNewElementWithNamespaces(): Unit = {
    val xml = <bar xmlns="defaultNS" xmlns:foo="fooNS" xmlns:bar="barNS">
                <quux xmlns:baz="bazNS" attr1="x"/>
              </bar>

    // val scope = (xml \ "quux")(0).scope // will have multiple namespace definitions.
    // println(scope)
    // val newElem = scala.xml.Elem("somePrefix", "someElement", Null, scope, true)
    // println(newElem)
    val quux = (xml \ "quux")(0)
    val barNS = quux.getNamespace("bar")
    assertEquals("barNS", barNS)
    val fooNS = quux.getNamespace("foo")
    assertEquals("fooNS", fooNS)
    val bazNS = quux.getNamespace("baz")
    assertEquals("bazNS", bazNS)
    val defaultNS = quux.getNamespace(null)
    assertEquals("defaultNS", defaultNS)
  }

}
