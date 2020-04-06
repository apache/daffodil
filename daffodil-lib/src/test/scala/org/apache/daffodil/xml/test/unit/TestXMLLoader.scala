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

package org.apache.daffodil.xml.test.unit

import org.junit.Assert._
import org.junit.Test

class TestXMLLoader {

  /**
   * Characterize behavior of scala's xml loader w.r.t. CDATA preservation.
   *
   * At the time of this testing. The basic scala xml loader uses Xerces (java)
   * under the hood. It seems hopelessly broken w.r.t CDATA region preservation.
   */
  @Test def test_scala_loader_cdata_bug() {

    val data = """<x><![CDATA[a
b&"<>]]></x>"""
    val node = scala.xml.XML.loadString(data)
    val <x>{ xbody @ _* }</x> = node
    assertEquals(1, xbody.length)
    val body = xbody(0)
    val txt = body.text
    assertTrue(txt.contains("a"))
    assertTrue(txt.contains("b"))
    //
    // Note to developer - whomewever sees this test failing....
    //
    // IF this test fails, it means that the scala xml loader have been FIXED (hooray!)
    // and our need for the ConstructingParser may have gone away.
    //
    assertFalse(txt.contains("<![CDATA[")) // wrong
    assertFalse(txt.contains("]]>")) // wrong
    assertTrue(txt.contains("a\nb")) // they preserve the contents
    assertFalse(body.isInstanceOf[scala.xml.PCData]) // wrong - they don't preserve the object properly.
    assertTrue(body.isInstanceOf[scala.xml.Text]) // wrong
    assertTrue(txt.contains("""&"<>""")) // They get the right characters in there.
    assertTrue(body.toString.contains("""&amp;&quot;&lt;&gt;""")) // wrong
    assertFalse(body.toString.contains("""<![CDATA[a
b&"<>]]>"""))

  }

}
