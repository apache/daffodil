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

class TestXMLPrettyPrinter {

  /**
   * Characterize behavior of scala's xml pretty printer with respect to
   * CDATA region preservation.
   */
  @Test def test_scala_xml_pretty_printer_normalizes_whitespace_inside_cdata_bug(): Unit = {
    //
    // because we know scala's XML literals don't preserve CDATA as PCData nodes
    // we force it to have a PCData node by constructing one explicitly here.
    //
    val pcdata = scala.xml.PCData("a\nb")
    val fragment = <x>{pcdata}</x>
    val pp = new scala.xml.PrettyPrinter(Int.MaxValue, 2)
    val xmlString = pp.format(fragment)
    //
    // If this test fails, then scala's pretty printer has been fixed.
    //
    // There's a possibility then that we can get rid of our pretty printer by just
    // defining it to be the scala pretty printer with width specified as maxInt, so
    // that it will always put simple types on one line.
    //
    assertTrue(xmlString.contains("<x><![CDATA[a b]]></x>")) // wrong!
    assertFalse(
      xmlString.contains("<x><![CDATA[a\nb]]></x>")
    ) // right. Should preserve the newline.
  }

  /**
   * Characterize behavior of daffodil's fixed pretty printer with respect to
   * CDATA region preservation.
   */
  @Test def test_daffodil_pretty_printer_preserves_whitespace_inside_cdata_properly(): Unit = {
    //
    // because we know scala's XML literals don't preserve CDATA as PCData nodes
    // we force it to have a PCData node by constructing one explicitly here.
    //
    val pcdata = scala.xml.PCData("a\nb")
    val fragment = <x>{pcdata}</x>
    val pp = new org.apache.daffodil.lib.xml.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    //
    // This verifies that our modified pretty printer does the right thing
    // with PCData nodes.
    //
    assertFalse(xmlString.contains("<x><![CDATA[a b]]></x>")) // wrong!
    assertTrue(
      xmlString.contains("<x><![CDATA[a\nb]]></x>")
    ) // right. Should preserve the newline.
  }

  @Test def test_daffodil_pretty_printer_preserves_whitespace_inside_text(): Unit = {

    val str = """aaaaa
bbbbb""".replace("\r\n", "\n")
    val fragment = <xxxxx><yyyyy><zzzzz>{str}</zzzzz></yyyyy></xxxxx>
    val pp = new org.apache.daffodil.lib.xml.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    assertEquals(
      """<xxxxx>
  <yyyyy>
    <zzzzz>aaaaa
bbbbb</zzzzz>
  </yyyyy>
</xxxxx>""".replace("\r\n", "\n"),
      xmlString
    )
  }

  @Test def test_daffodil_pretty_printer_preserves_whitespace_inside_cdata_properly2(): Unit = {
    //
    // because we know scala's XML literals don't preserve CDATA as PCData nodes
    // we force it to have a PCData node by constructing one explicitly here.
    //
    val pcdata = scala.xml.PCData(" aaaaa\nbbbbb ")
    val fragment = <xxxxx><yyyyy><zzzzz>{pcdata}</zzzzz></yyyyy></xxxxx>
    val pp = new org.apache.daffodil.lib.xml.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    //
    // This verifies that our modified pretty printer does the right thing
    // with PCData nodes.
    //
    assertTrue(xmlString.contains("""<xxxxx>
  <yyyyy>
    <zzzzz><![CDATA[ aaaaa
bbbbb ]]></zzzzz>
  </yyyyy>
</xxxxx>""".replace("\r\n", "\n")))
  }

  @Test def test_daffodil_pretty_printer_preserves_whitespace_inside_cdata_properly3(): Unit = {
    //
    // because we know scala's XML literals don't preserve CDATA as PCData nodes
    // we force it to have a PCData node by constructing one explicitly here.

    val pcdata = scala.xml.PCData(" aaaaa\nbbbbb ")
    val fragment = <xxxxx><yyyyy><zzzzz>{pcdata} xyzzy {pcdata}</zzzzz></yyyyy></xxxxx>
    val pp = new org.apache.daffodil.lib.xml.PrettyPrinter(2)
    var xmlString = pp.format(fragment)
    //
    // This verifies that our modified pretty printer does the right thing
    // with PCData nodes.
    //
    // The first chunk must be exactly this.
    //
    val chunk = """<xxxxx>
  <yyyyy>
    <zzzzz><![CDATA[ aaaaa
bbbbb ]]>""".replace("\r\n", "\n")
    assertTrue(xmlString.startsWith(chunk))
    xmlString = xmlString.substring(chunk.length, xmlString.length)
    //
    // After that, there's whitespace (which could be spaces and/or line
    // endings inserted by the pretty printing, followed by the next token xyzzy
    // then more whitespace or line endings, then the rest of the thing.
    //
    val Pat = """(\s+)xyzzy(\s+)([\s|\S]*)""".r
    xmlString match {
      case Pat(ws1, ws2, rest) => {
        //
        // the rest must match this chunk
        //
        assertEquals(
          """<![CDATA[ aaaaa
bbbbb ]]></zzzzz>
  </yyyyy>
</xxxxx>""".replace("\r\n", "\n"),
          rest
        )
      }
    }
  }

  @Test def test_daffodil_pretty_printer_preserves_simple_values1(): Unit = {

    val fragment =
      <xxxxx><yyyyy><zzzzz>aaaaa bbbbb ccccc ddddd eeeee fffff ggggg</zzzzz></yyyyy></xxxxx>
    val pp = new org.apache.daffodil.lib.xml.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    val expected = """<xxxxx>
  <yyyyy>
    <zzzzz>aaaaa bbbbb ccccc ddddd eeeee fffff ggggg</zzzzz>
  </yyyyy>
</xxxxx>""".replace("\r\n", "\n")
    // The following 'replace' corrects for Windows CR/LF sequence in the compared string.
    val actual = xmlString.replace("\r\n", "\n")
    assertEquals(expected, actual)
  }

  @Test def test_daffodil_pretty_printer_removes_redundant_xmlns_bindings(): Unit = {

    val fragment =
      <xxxxx xmlns="foobar"><yyyyy><zzzzz xmlns="foobar">aaaaa bbbbb ccccc ddddd eeeee fffff ggggg</zzzzz></yyyyy></xxxxx>
    val pp = new org.apache.daffodil.lib.xml.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    //
    val expected = """<xxxxx xmlns="foobar">
  <yyyyy>
    <zzzzz>aaaaa bbbbb ccccc ddddd eeeee fffff ggggg</zzzzz>
  </yyyyy>
</xxxxx>""".replace("\r\n", "\n")
    assertEquals(expected, xmlString)
  }

  @Test def test_daffodil_pretty_printer_newlines_and_indents1(): Unit = {

    val fragment =
      <tns:row2 xmlns:tns="http://example.com"><cell>-9</cell><cell>-2</cell><cell>-8</cell></tns:row2>
    val pp = new org.apache.daffodil.lib.xml.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    //
    val expected = """<tns:row2 xmlns:tns="http://example.com">
  <cell>-9</cell>
  <cell>-2</cell>
  <cell>-8</cell>
</tns:row2>""".replace("\r\n", "\n")
    assertEquals(expected, xmlString)
  }

}
