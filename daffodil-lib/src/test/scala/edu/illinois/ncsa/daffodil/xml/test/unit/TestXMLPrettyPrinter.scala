/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.xml.test.unit

import org.junit.Test
import org.junit.Assert._

class TestXMLPrettyPrinter {

  /**
   * Characterize behavior of scala's xml pretty printer with respect to
   * CDATA region preservation.
   */
  @Test def test_scala_xml_pretty_printer_normalizes_whitespace_inside_cdata_bug() {
    //
    // because we know scala's XML literals don't preserve CDATA as PCData nodes
    // we force it to have a PCData node by constructing one explicitly here.
    //
    val pcdata = scala.xml.PCData("a\nb")
    val fragment = <x>{ pcdata }</x>
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
    assertFalse(xmlString.contains("<x><![CDATA[a\nb]]></x>")) // right. Should preserve the newline.
  }

  /**
   * Characterize behavior of daffodil's fixed pretty printer with respect to
   * CDATA region preservation.
   */
  @Test def test_daffodil_pretty_printer_preserves_whitespace_inside_cdata_properly() {
    //
    // because we know scala's XML literals don't preserve CDATA as PCData nodes
    // we force it to have a PCData node by constructing one explicitly here.
    //
    import edu.illinois.ncsa._
    val pcdata = scala.xml.PCData("a\nb")
    val fragment = <x>{ pcdata }</x>
    val pp = new daffodil.xml.scalaLib.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    //
    // This verifies that our modified pretty printer does the right thing
    // with PCData nodes.
    //
    assertFalse(xmlString.contains("<x><![CDATA[a b]]></x>")) // wrong!
    assertTrue(xmlString.contains("<x><![CDATA[a\nb]]></x>")) // right. Should preserve the newline.
  }

  @Test def test_daffodil_pretty_printer_preserves_whitespace_inside_text() {

    import edu.illinois.ncsa._
    val str = """aaaaa
bbbbb"""
    val fragment = <xxxxx><yyyyy><zzzzz>{ str }</zzzzz></yyyyy></xxxxx>
    val pp = new daffodil.xml.scalaLib.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    assertEquals("""<xxxxx>
  <yyyyy>
    <zzzzz>aaaaa
bbbbb</zzzzz>
  </yyyyy>
</xxxxx>""", xmlString)
  }

  @Test def test_daffodil_pretty_printer_preserves_whitespace_inside_cdata_properly2() {
    //
    // because we know scala's XML literals don't preserve CDATA as PCData nodes
    // we force it to have a PCData node by constructing one explicitly here.
    //
    import edu.illinois.ncsa._
    val pcdata = scala.xml.PCData(" aaaaa\nbbbbb ")
    val fragment = <xxxxx><yyyyy><zzzzz>{ pcdata }</zzzzz></yyyyy></xxxxx>
    val pp = new daffodil.xml.scalaLib.PrettyPrinter(2)
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
</xxxxx>"""))
  }

  @Test def test_daffodil_pretty_printer_preserves_whitespace_inside_cdata_properly3() {
    //
    // because we know scala's XML literals don't preserve CDATA as PCData nodes
    // we force it to have a PCData node by constructing one explicitly here.

    import edu.illinois.ncsa._
    val pcdata = scala.xml.PCData(" aaaaa\nbbbbb ")
    val fragment = <xxxxx><yyyyy><zzzzz>{ pcdata } xyzzy { pcdata }</zzzzz></yyyyy></xxxxx>
    val pp = new daffodil.xml.scalaLib.PrettyPrinter(2)
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
bbbbb ]]>"""
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
        assertEquals("""<![CDATA[ aaaaa
bbbbb ]]></zzzzz>
  </yyyyy>
</xxxxx>""", rest)
      }
    }
  }

  @Test def test_daffodil_pretty_printer_preserves_simple_values1() {

    import edu.illinois.ncsa._
    val fragment = <xxxxx><yyyyy><zzzzz>aaaaa bbbbb ccccc ddddd eeeee fffff ggggg</zzzzz></yyyyy></xxxxx>
    val pp = new daffodil.xml.scalaLib.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    val expected = """<xxxxx>
  <yyyyy>
    <zzzzz>aaaaa bbbbb ccccc ddddd eeeee fffff ggggg</zzzzz>
  </yyyyy>
</xxxxx>"""
    assertEquals(expected, xmlString)
  }

  @Test def test_daffodil_pretty_printer_removes_redundant_xmlns_bindings() {

    import edu.illinois.ncsa._
    val fragment = <xxxxx xmlns="foobar"><yyyyy><zzzzz xmlns="foobar">aaaaa bbbbb ccccc ddddd eeeee fffff ggggg</zzzzz></yyyyy></xxxxx>
    val pp = new daffodil.xml.scalaLib.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    //
    val expected = """<xxxxx xmlns="foobar">
  <yyyyy>
    <zzzzz>aaaaa bbbbb ccccc ddddd eeeee fffff ggggg</zzzzz>
  </yyyyy>
</xxxxx>"""
    assertEquals(expected, xmlString)
  }

  @Test def test_daffodil_pretty_printer_newlines_and_indents1() {

    import edu.illinois.ncsa._
    val fragment = <tns:row2 xmlns:tns="http://example.com"><cell>-9</cell><cell>-2</cell><cell>-8</cell></tns:row2>
    val pp = new daffodil.xml.scalaLib.PrettyPrinter(2)
    val xmlString = pp.format(fragment)
    //
    val expected = """<tns:row2 xmlns:tns="http://example.com">
  <cell>-9</cell>
  <cell>-2</cell>
  <cell>-8</cell>
</tns:row2>"""
    assertEquals(expected, xmlString)
  }

}
