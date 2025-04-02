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

import scala.xml._

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert._
import org.junit.Test

class TestUnicodeXMLI18N {

  /**
   * Let's characterize the behavior of XML Literals in Scala.
   */
  @Test def testXMLEntities(): Unit = {
    val fragment = <x>abc&amp;def</x> // XML Entities disappear for us. We never see them.
    val txt = fragment.text
    assertEquals("abc&def", txt)
  }

  /**
   * Obscure corner about XML. XML isn't allowed to contain character code 0 no way no how, not even as an entity.
   */
  @Test def testNUL1(): Unit = {
    val fragment =
      <x>abc&#x0000;def</x> // This isn't supposed to work in XML. character code 0 is never allowed in XML documents.
    // TODO: implement our own enforcement of disallowing NUL in XML infoset strings.
    // TODO: consider XML v1.0 versus v1.1 - infosets allow different character codes. 1.1 only disallows 0.
    val txt = fragment.text
    assertEquals("abc\u0000def", txt)
    assertEquals(7, txt.length)
  }

  /**
   * if this test doesn't pass, your environment is not adequately set up for Unicode and internationalized
   * characters
   */
  @Test def testUnicodeElementAndAttributeNames(): Unit = {
    //    val fragment = <Date 年月日="2003年08月27日">2003年08月27日<SUB2003年08月27日>hi</SUB2003年08月27日></Date> // <!-- element contains unicode in its name -->
    //    val txt = fragment.text
    //    assertEquals("2003年08月27日hi", txt)
    //    assertEquals(13, txt.length)
    //    val subfrag = (fragment \ "SUB2003年08月27日").text
    //    assertEquals("hi", subfrag)
    //    val attr = (fragment \ "@年月日").text
    //    assertEquals("2003年08月27日", attr)
  }

  /**
   * These next tests characterize Scala's namespace handling in XML literals
   * If these tests break, then that means they have changed the behavior of XML literals in Scala
   * in a way that will break daffodil (or is likely to break it.)
   */

  def isXS(elem: Node) = NS(elem.namespace) == XMLUtils.XSD_NAMESPACE
  def isSequence(elem: Node) = elem.label == "sequence" && isXS(elem)
  def isSchema(elem: Node) = elem.label == "schema" && isXS(elem)
  def isElement(elem: Node) = elem.label == "element" && isXS(elem)

  val xmlnsURI = XMLUtils.XSD_NAMESPACE
  @Test def testRightElementRightPrefixRightNS(): Unit = {
    val xsSequence = <xs:sequence xmlns:xs={xmlnsURI}/>
    assertTrue(isSequence(xsSequence))
  }

  @Test def testRightElementWrongPrefixRightNS(): Unit = {
    val xsSequence = <wrong:sequence xmlns:wrong={xmlnsURI}/>
    assertTrue(isSequence(xsSequence))
  }

  @Test def testRightElementRightPrefixWrongNS(): Unit = {
    val xsSequence = <xs:sequence xmlns:xs="http://Not.the.right.namespace"/>
    assertFalse(isSequence(xsSequence))
  }

  @Test def testRightElementWrongPrefixWrongNS(): Unit = {
    val xsSequence = <wrong:sequence xmlns:wrong="http://Not.the.right.namespace"/>
    assertFalse(isSequence(xsSequence))
  }

  @Test def testWrongElementRightPrefixRightNS(): Unit = {
    val xsSequence = <xs:idaho xmlns:xs={xmlnsURI}/>
    assertFalse(isSequence(xsSequence))
  }

  @Test def testIsSchema(): Unit = {
    val xsSchema = <xs:schema xmlns:xs={xmlnsURI}/>
    assertTrue(isSchema(xsSchema))
  }

  @Test def testIsElement(): Unit = {
    val xsElement = <xs:element xmlns:xs={xmlnsURI}/>
    assertTrue(isElement(xsElement))
  }

}
