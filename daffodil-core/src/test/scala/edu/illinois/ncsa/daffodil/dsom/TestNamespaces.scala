package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */


import scala.xml._
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
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
  @Test def testScalaNamespace1() {
    val xml = <bar xmlns:foo="fooNamespaceID">
                <quux attr1="x"/>
              </bar>
    val foons = (xml \ "quux")(0).getNamespace("foo")
    assertEquals("fooNamespaceID", foons)
  }

  /**
   * Pass null (not "") to retrieve the default NS
   */
  @Test def testScalaNamespace2() {
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
  @Test def testScalaNamespace3() {
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
  @Test def testScalaNamespace4() {
    val xml = <bar>
                <quux attr1="x"/>
              </bar>
    val ns = (xml \ "quux")(0).getNamespace(null)
    assertEquals(null, ns)
  }

  /**
   * Passing "" to getNamespace() is problematic.
   */
  @Test def testScalaNamespace5() {
    val xml = <bar>
                <quux attr1="x"/>
              </bar>
    val ns = (xml \ "quux")(0).getNamespace("")
    assertEquals(null, ns)
  }

  /**
   * Illustrates that you cannot use "" to access the default namespace.
   */
  @Test def testScalaNamespace6() {
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
  @Test def testNewElementWithNamespaces() {
    val xml = <bar xmlns="defaultNS" xmlns:foo="fooNS" xmlns:bar="barNS">
                <quux xmlns:baz="bazNS" attr1="x"/>
              </bar>

    val scope = (xml \ "quux")(0).scope // will have multiple namespace definitions.
    // println(scope)
    val newElem = scala.xml.Elem("somePrefix", "someElement", Null, scope, true)
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
