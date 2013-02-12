package edu.illinois.ncsa.daffodil.parser

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


import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import scala.collection.mutable.Queue
import java.util.regex.Pattern
import java.io.FileInputStream
import scala.util.parsing.input.CharSequenceReader
import edu.illinois.ncsa.daffodil.util.Misc
import javax.xml.transform.stream.StreamSource
import java.io.File
import java.nio.charset.Charset
import java.net.URI
import org.junit.Test
import edu.illinois.ncsa.daffodil.dsom.Fakes
import edu.illinois.ncsa.daffodil.processors.DFDLByteReader
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.DelimParser

class TestParsingBehaviors extends JUnitSuite {

  val testFileDir = "/test/"

  val rsrcAB007 = Misc.getRequiredResource(testFileDir + "AB007.in")

  @Test def testEscapeCharacterRemoval_Same = {
    // escape and escapeEscape are the same
    val input0 = "texttexttext"
    val input1 = "text1//text2"
    val input2 = "text1//text2//text3"
    val input3 = "text1////text2"
    val input4 = "//text1"
    val input5 = "text1//"
    val input6 = "//text1//text2"
    val input7 = "text1//text2//"
    val input8 = "text1/,text2"
    val input9 = "text1///,text2"
    val input10 = "/,text1"

    val d = new DelimParser(Fakes.fakeElem)

    assertEquals("texttexttext", d.removeEscapeCharacters(input0, "/", "/", ","))
    assertEquals("text1/text2", d.removeEscapeCharacters(input1, "/", "/", ","))
    assertEquals("text1/text2/text3", d.removeEscapeCharacters(input2, "/", "/", ","))
    assertEquals("text1//text2", d.removeEscapeCharacters(input3, "/", "/", ","))
    assertEquals("/text1", d.removeEscapeCharacters(input4, "/", "/", ","))
    assertEquals("text1/", d.removeEscapeCharacters(input5, "/", "/", ","))
    assertEquals("/text1/text2", d.removeEscapeCharacters(input6, "/", "/", ","))
    assertEquals("text1/text2/", d.removeEscapeCharacters(input7, "/", "/", ","))
    assertEquals("text1,text2", d.removeEscapeCharacters(input8, "/", "/", ","))
    assertEquals("text1/,text2", d.removeEscapeCharacters(input9, "/", "/", ","))
    assertEquals(",text1", d.removeEscapeCharacters(input10, "/", "/", ","))
  }

  @Test def testEscapeCharacterRemoval_Diff = {
    // different escape and escapeEscape characters
    val input0 = "texttexttext"
    val input1 = "text1%/text2"
    val input2 = "text1%/text2%/text3"
    val input3 = "text1%/%/text2"
    val input4 = "%/text1"
    val input5 = "text1%/"
    val input6 = "%/text1%/text2"
    val input7 = "text1%/text2%/"
    val input8 = "text1/,text2"
    val input9 = "text1%//,text2"
    val input10 = "/,text1"
    val input11 = "text1/?text2"
    val input12 = "text1%text2"
    val input13 = "text1%%/text2"
    val input14 = "text1%/%text2"

    val d = new DelimParser(Fakes.fakeElem)

    assertEquals("texttexttext", d.removeEscapeCharacters(input0, "%", "/", ","))
    assertEquals("text1/text2", d.removeEscapeCharacters(input1, "%", "/", ","))
    assertEquals("text1/text2/text3", d.removeEscapeCharacters(input2, "%", "/", ","))
    assertEquals("text1//text2", d.removeEscapeCharacters(input3, "%", "/", ","))
    assertEquals("/text1", d.removeEscapeCharacters(input4, "%", "/", ","))
    assertEquals("text1/", d.removeEscapeCharacters(input5, "%", "/", ","))
    assertEquals("/text1/text2", d.removeEscapeCharacters(input6, "%", "/", ","))
    assertEquals("text1/text2/", d.removeEscapeCharacters(input7, "%", "/", ","))
    assertEquals("text1,text2", d.removeEscapeCharacters(input8, "%", "/", ","))
    assertEquals("text1/,text2", d.removeEscapeCharacters(input9, "%", "/", ","))
    assertEquals(",text1", d.removeEscapeCharacters(input10, "%", "/", ","))
    assertEquals("text1?text2", d.removeEscapeCharacters(input11, "%", "/", ","))
    assertEquals("text1%text2", d.removeEscapeCharacters(input12, "%", "/", ","))
    assertEquals("text1%/text2", d.removeEscapeCharacters(input13, "%", "/", ","))
    assertEquals("text1/%text2", d.removeEscapeCharacters(input14, "%", "/", ","))
  }

  @Test def testEscapeCharacterRemoval_Diff_MultiCharDelim = {
    val d = new DelimParser(Fakes.fakeElem)
    val input0 = "text1/septext2"
    val input1 = "text1%//septext2"
    val input2 = "/septext1text2"

    assertEquals("text1septext2", d.removeEscapeCharacters(input0, "%", "/", "sep"))
    assertEquals("text1/septext2", d.removeEscapeCharacters(input1, "%", "/", "sep"))
    assertEquals("septext1text2", d.removeEscapeCharacters(input2, "%", "/", "sep"))
  }

  // Invalid, escapeBlockRemoval code expects valid start/end blocks already picked off
  //  @Test def testEscapeBlockRemoval_Diff = {
  //    // Different Start/End characters
  //    val qInputOutput = Queue.empty[(String, String)]
  //    val qOutput = Queue.empty[String]
  //
  //    qInputOutput.enqueue("texttext" -> "texttext")
  //    qInputOutput.enqueue("[[texttext]" -> "[texttext")
  //    qInputOutput.enqueue("]texttext" -> "]texttext")
  //    qInputOutput.enqueue("text[text" -> "text[text")
  //    qInputOutput.enqueue("text]text" -> "text]text")
  //    qInputOutput.enqueue("texttext]" -> "texttext]")
  //    qInputOutput.enqueue("[[[texttext]" -> "[[texttext")
  //    qInputOutput.enqueue("texttext]]" -> "texttext]]")
  //    qInputOutput.enqueue("text[[text" -> "text[[text")
  //    qInputOutput.enqueue("text]]text" -> "text]]text")
  //    qInputOutput.enqueue("[[texttext%]]" -> "[texttext]")
  //    qInputOutput.enqueue("[[text%]text]" -> "[text]text")
  //    qInputOutput.enqueue("text[text]" -> "text[text]")
  //    qInputOutput.enqueue("[[text[text%]]" -> "[text[text]")
  //    qInputOutput.enqueue("[[text%]text%]]" -> "[text]text]")
  //    qInputOutput.enqueue("[[[texttext%]]" -> "[[texttext]")
  //    qInputOutput.enqueue("[[texttext%]%]]" -> "[texttext]]")
  //    qInputOutput.enqueue("[[[texttext%]%]]" -> "[[texttext]]")
  //    qInputOutput.enqueue("text%text" -> "text%text")
  //    qInputOutput.enqueue("text%%text" -> "text%%text")
  //    qInputOutput.enqueue("text%[text" -> "text%[text")
  //    qInputOutput.enqueue("text%]text" -> "text%]text")
  //    qInputOutput.enqueue("%[texttext" -> "%[texttext")
  //    qInputOutput.enqueue("texttext%]" -> "texttext%]")
  //    qInputOutput.enqueue("%[texttext%]" -> "%[texttext%]")
  //    qInputOutput.enqueue("[[text%text%]]" -> "[text%text]")
  //    qInputOutput.enqueue("[[text%%]text%]]" -> "[text%]text]")
  //    qInputOutput.enqueue("[text;text]" -> "text;text")
  //    qInputOutput.enqueue("[text%;text]" -> "text%;text")
  //    qInputOutput.enqueue("[[text;text%]]" -> "[text;text]")
  //    qInputOutput.enqueue("[text?text]" -> "text?text")
  //
  //    val d = new DelimParser(Fakes.fakeElem)
  //    var idx = 1
  //    qInputOutput.foreach(x => {
  //      println("trying... expect: " + x._2 + " for input: " + x._1)
  //      val result = d.removeEscapesBlocks(x._1, "%", """]""")
  //      //println("...got: " + result)
  //      assertEquals(x._2, result)
  //      //println("test " + idx + " succeeded")
  //      idx += 1
  //    })
  //  }
  //
  //  @Test def testEscapeBlockRemoval_Same = {
  //    // Same Start/End characters
  //    val qInputOutput = Queue.empty[(String, String)]
  //    val qOutput = Queue.empty[String]
  //
  //    qInputOutput.enqueue("texttext" -> "texttext")
  //    qInputOutput.enqueue("'%'texttext'" -> "'texttext")
  //    qInputOutput.enqueue("text'text" -> "text'text")
  //    qInputOutput.enqueue("texttext'" -> "texttext'")
  //    qInputOutput.enqueue("'%'%'texttext'" -> "''texttext")
  //    qInputOutput.enqueue("texttext''" -> "texttext''")
  //    qInputOutput.enqueue("text''text" -> "text''text")
  //    qInputOutput.enqueue("'%'texttext%''" -> "'texttext'")
  //
  //    qInputOutput.enqueue("'%'text%'text'" -> "'text'text")
  //    qInputOutput.enqueue("text'text'" -> "text'text'")
  //    qInputOutput.enqueue("'%'text%'text%''" -> "'text'text'")
  //    qInputOutput.enqueue("'%'%'texttext%''" -> "''texttext'")
  //    qInputOutput.enqueue("'%'texttext%'%''" -> "'texttext''")
  //    qInputOutput.enqueue("'%'%'texttext%'%''" -> "''texttext''")
  //
  //    qInputOutput.enqueue("text%text" -> "text%text")
  //    qInputOutput.enqueue("text%%text" -> "text%%text")
  //    qInputOutput.enqueue("text%'text" -> "text%'text")
  //    qInputOutput.enqueue("%'texttext" -> "%'texttext")
  //
  //    qInputOutput.enqueue("texttext%'" -> "texttext%'")
  //    qInputOutput.enqueue("'%'texttext%%''" -> "'texttext%'")
  //    qInputOutput.enqueue("%'texttext%'" -> "%'texttext%'")
  //    qInputOutput.enqueue("'%'text%text%''" -> "'text%text'")
  //    qInputOutput.enqueue("'%'text%%'text%''" -> "'text%'text'")
  //
  //    qInputOutput.enqueue("'text;text'" -> "text;text")
  //    qInputOutput.enqueue("'text%;text'" -> "text%;text")
  //    qInputOutput.enqueue("'%'text;text%''" -> "'text;text'")
  //    qInputOutput.enqueue("'text?text'" -> "text?text")
  //
  //    val d = new DelimParser(Fakes.fakeElem)
  //    var idx = 1
  //    qInputOutput.foreach(x => {
  //      //println("trying... expect: " + x._2 + " for input: " + x._1)
  //      val result = d.removeEscapesBlocks(x._1, "%", """'""")
  //      //println("...got: " + result)
  //      assertEquals(x._2, result)
  //      //println("test " + idx + " succeeded")
  //      idx += 1
  //    })
  //  }
  //
  //  @Test def testParseSingleFieldFromAB007 = {
  //    ////println(System.getProperty("user.dir"))
  //    //val channel = new FileInputStream(testFileDir + "AB007.in").getChannel()
  //    val channel = new FileInputStream(new File(new URI(rsrcAB007.toString()))).getChannel()
  //
  //    val byteR = new DFDLByteReader(channel)
  //
  //    //val r = byteR.charReader("UTF-8")
  //    val r = byteR.newCharReader(Charset.forName("UTF-8"), 0)
  //
  //    val d = new DelimParser(Fakes.fakeElem)
  //
  //    val separators = Set[String](",")
  //
  //    val terminators = Set[String]("%NL;")
  //
  //    val res = d.parseInput(separators, terminators, r, TextJustificationType.None, "")
  //
  //    assertEquals("1", res.field)
  //    assertEquals(",", res.delimiter)
  //  }

  @Test def testParsingEscapeSchemeBlockAtStart = {
    // Valid escapeBlockStart and escapeBlockEnd
    // escBS starts at beginning of field AND
    // escBE ends immediately followed by a delimiter.
    //
    val r = new CharSequenceReader("/*hidden/*:text*/:def:ghi") // Input 1
    val d = new DelimParser(Fakes.fakeElem)
    val separators = Set[String](":")
    val terminators = Set[String]()
    val escapeBlockStart = "/*"
    val escapeBlockEnd = "*/"
    val escapeEscapeCharacter = ""

    val res = d.parseInputEscapeBlock(separators, terminators, r,
      escapeBlockStart, escapeBlockEnd, escapeEscapeCharacter, TextJustificationType.None, "")

    assertTrue(res.isSuccess)
    assertEquals("hidden/*:text", res.field)
    assertEquals(":", res.delimiter)
    assertEquals(17 * 8, res.numBits)
  }

  @Test def testParsingEscapedEscapeSchemeBlockAtStart = {
    // Not a valid escapeBlockStart as it does not start
    // at the beginning of the field
    //
    val r = new CharSequenceReader("//*hidden/*:text*/:def:ghi") // Input 1
    val d = new DelimParser(Fakes.fakeElem)
    val separators = Set[String](":")
    val terminators = Set[String]()
    val escapeBlockStart = "/*"
    val escapeBlockEnd = "*/"
    val escapeEscapeCharacter = "/"

    val res = d.parseInputEscapeBlock(separators, terminators, r,
      escapeBlockStart, escapeBlockEnd, escapeEscapeCharacter, TextJustificationType.None, "")

    assertTrue(res.isSuccess)
    assertEquals("//*hidden/*", res.field)
    assertEquals(":", res.delimiter)
    assertEquals(11 * 8, res.numBits)
  }

  @Test def testParsingEscapeSchemeBlockInMiddle = {
    // Not a valid escapeBlockStart as it does not start
    // at the beginning of the field
    //
    val r = new CharSequenceReader("abc/*hidden/*:text*/:def:ghi") // Input 1
    val d = new DelimParser(Fakes.fakeElem)
    val separators = Set[String](":")
    val terminators = Set[String]()
    val escapeBlockStart = "/*"
    val escapeBlockEnd = "*/"
    val escapeEscapeCharacter = ""

    val res = d.parseInputEscapeBlock(separators, terminators, r,
      escapeBlockStart, escapeBlockEnd, escapeEscapeCharacter, TextJustificationType.None, "")

    assertTrue(res.isSuccess)
    assertEquals("abc/*hidden/*", res.field)
    assertEquals(":", res.delimiter)
    assertEquals(13 * 8, res.numBits)
  }

  @Test def testParsingEscapeSchemeBlock_PartialBlock = {
    // Because there are no full escape blocks, we expect this to parse
    // normally
    //
    val r = new CharSequenceReader("/*abchidden:text:def:ghi") // Input 1
    val d = new DelimParser(Fakes.fakeElem)
    val separators = Set[String](":")
    val terminators = Set[String]()
    val escapeBlockStart = "/*"
    val escapeBlockEnd = "*/"
    val escapeEscapeCharacter = ""

    val res = d.parseInputEscapeBlock(separators, terminators, r,
      escapeBlockStart, escapeBlockEnd, escapeEscapeCharacter, TextJustificationType.None, "")

    assertTrue(res.isSuccess)
    assertEquals("/*abchidden", res.field)
    assertEquals(":", res.delimiter)
    assertEquals(11 * 8, res.numBits)
  }

  @Test def testParsingEscapeSchemeBlock_NoBlocks = {
    // Because there are no escape blocks, we expect this to parse
    // normally
    //
    val r = new CharSequenceReader("abchidden*:text:def:ghi") // Input 1
    val d = new DelimParser(Fakes.fakeElem)
    val separators = Set[String](":")
    val terminators = Set[String]()
    val escapeBlockStart = "/*"
    val escapeBlockEnd = "*/"
    val escapeEscapeCharacter = ""

    val res = d.parseInputEscapeBlock(separators, terminators, r,
      escapeBlockStart, escapeBlockEnd, escapeEscapeCharacter, TextJustificationType.None, "")

    assertTrue(res.isSuccess)
    assertEquals("abchidden*", res.field)
    assertEquals(":", res.delimiter)
    assertEquals(10 * 8, res.numBits)
  }

  @Test def testParsingEscapeSchemeCharacter_NoEscapes = {
    // Because there are no escapes present we expect
    // this to parse normally.
    //
    val r = new CharSequenceReader("abc:def:ghi") // Input 1
    val d = new DelimParser(Fakes.fakeElem)
    val separators = Set[String](":")
    val terminators = Set[String]()
    val escapeCharacter = "/"
    val escapeEscapeCharacter = "/"

    val res = d.parseInputEscapeCharacter(separators, terminators, r, escapeCharacter,
      escapeEscapeCharacter, TextJustificationType.None, "")

    assertTrue(res.isSuccess)
    assertEquals("abc", res.field)
    assertEquals(":", res.delimiter)
    assertEquals(3 * 8, res.numBits)
  }

  @Test def testParsingEscapeSchemeCharacter_UnescapedEscape = {

    val r = new CharSequenceReader("abc/:def:ghi") // Input 1
    val d = new DelimParser(Fakes.fakeElem)
    val separators = Set[String](":")
    val terminators = Set[String]()
    val escapeCharacter = "/"
    val escapeEscapeCharacter = "/"

    val res = d.parseInputEscapeCharacter(separators, terminators, r, escapeCharacter,
      escapeEscapeCharacter, TextJustificationType.None, "")

    assertTrue(res.isSuccess)
    assertEquals("abc:def", res.field)
    assertEquals(":", res.delimiter)
    assertEquals(8 * 8, res.numBits)
  }

  @Test def testParsingEscapeSchemeCharacter_EscapedEscape = {

    val r = new CharSequenceReader("abc//:def:ghi") // Input 1
    val d = new DelimParser(Fakes.fakeElem)
    val separators = Set[String](":")
    val terminators = Set[String]()
    val escapeCharacter = "/"
    val escapeEscapeCharacter = "/"

    val res = d.parseInputEscapeCharacter(separators, terminators, r, escapeCharacter,
      escapeEscapeCharacter, TextJustificationType.None, "")

    assertTrue(res.isSuccess)
    assertEquals("abc/", res.field)
    assertEquals(":", res.delimiter)
    assertEquals(5 * 8, res.numBits)
  }

}