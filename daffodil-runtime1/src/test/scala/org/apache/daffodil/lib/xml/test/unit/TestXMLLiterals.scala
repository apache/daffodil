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

import org.junit.Assert._
import org.junit.Test

class TestXMLLiterals {

  /**
   * Characterize behavior of scala's xml literals w.r.t. CDATA preservation
   */
  @Test def test_scala_literals_cdata_bug(): Unit = {
    //
    // since Scala 3 is dropping support for XML Literals, we explicitly create an Elem
    // with PCData as the child here.
    //
    val actual =
      scala.xml.Elem(
        null,
        "x",
        scala.xml.Null,
        scala.xml.TopScope,
        true,
        scala.xml.PCData("a\nb")
      )
    val xbody = actual.child
    assertEquals(1, xbody.length)
    val body = xbody(0)
    val txt = body.text
    assertTrue(txt.contains("a"))
    assertTrue(txt.contains("b"))
    assertFalse(txt.contains("<![CDATA[")) // wrong
    assertFalse(txt.contains("]]>")) // wrong
    assertTrue(txt.contains("a\nb")) // they preserve the contents
    assertTrue(body.isInstanceOf[scala.xml.PCData])
    assertFalse(body.isInstanceOf[scala.xml.Text])
    //
    // Note to developer - whomewever sees this test failing....
    //
    // IF this test fails, it means that the scala xml literals have been FIXED (hooray!)
    // and a bunch of hacks where we have XML literals with manual splicing of PCData objects
    // can go away. Look for tests with "PCData(" in them.
    //
    // Note this test now passes, scala-xml does the correct thing with Xxml:-coalescing compile option set
  }

}
