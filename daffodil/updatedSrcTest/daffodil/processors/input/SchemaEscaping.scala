package daffodil.processors.input

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import java.nio.charset.Charset
import java.io.{BufferedInputStream, ByteArrayInputStream}
import org.jdom.Element
import daffodil.schema.annotation.ListLiteralValue
import daffodil.parser.RollbackStream
import daffodil.xml.Namespaces
import daffodil.processors.{Success, Last, VariableMap}
import daffodil.schema.annotation.enumerations._
import daffodil.processors.input.text.TextProcessor

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies 
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its 
 *     contributors may be used to endorse or promote products derived from this 
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */
 
/* 
 * Developer: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu > 
 * Date: 2010
 */

class SchemaEscaping extends FunSuite with ShouldMatchers {
//  test ("no escape") {
//
//    val tp = new TextProcessor(Charset.forName("UTF-8"),true)
//
//
//    tp setLengthKind(Delimited)
//    tp setSeparator(ListLiteralValue(List(",")))
//    tp setSeparatorPolicy(Require)
//    tp setSeparatorPosition(Postfix)
//    tp setTextStringJustification(NoneJustification)
//    tp setTypeName(null)
//
//    val input = new ByteArrayInputStream("a list#,# is just a list, is just a list, is just a list" getBytes("UTF-8"))
//
//    val stream = new RollbackStream(new BufferedInputStream(input))
//
//    val element0 = new Element("MyElement")
//    val element1 = new Element("MyElement")
//    val element2 = new Element("MyElement")
//    val element3 = new Element("MyElement")
//
//    val variables = new VariableMap
//    val namespaces = new Namespaces
//
//
//    tp.apply(stream,element0,variables,namespaces,List()) should equal (Success)
//    tp.findPostfixSeparator(stream,element0,variables,namespaces) should be (SeparatorFound("",","))
//    element0.getText should equal ("a list#")
//
//    tp.apply(stream,element1,variables,namespaces,List()) should equal (Success)
//    tp.findPostfixSeparator(stream,element1,variables,namespaces) should be (SeparatorFound("",","))
//    element1.getText should equal ("# is just a list")
//
//    tp.apply(stream,element2,variables,namespaces,List()) should equal (Success)
//    tp.findPostfixSeparator(stream,element2,variables,namespaces) should be (SeparatorFound("",","))
//    element2.getText should equal (" is just a list")
//
//    tp.apply(stream,element3,variables,namespaces,List()) should equal (Last)
//    tp.findPostfixSeparator(stream,element3,variables,namespaces) should be (NothingFound)
//    element3.getText should equal (" is just a list")
//
//  }
//
//
//  test("simple escape character fixed length") {
//    val tp = new TextProcessor(Charset.forName("UTF-8"),true)
//
//    tp setLengthKind(Explicit)
//    tp setLength("39")
//    tp setTextStringJustification(NoneJustification)
//    tp setTypeName(null)
//    tp setEscapeKind(EscapeCharacter)
//    tp setEscapeCharacter("#")
//
//    val input = new ByteArrayInputStream("this #Asentence is 35 #Bcharacters longAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB" getBytes("UTF-8"))
//
//    val stream = new RollbackStream(new BufferedInputStream(input))
//
//    val element0 = new Element("MyElement")
//    val element1 = new Element("MyElement")
//
//    val variables = new VariableMap
//    val namespaces = new Namespaces
//
//    tp.apply(stream,element0,variables,namespaces,List()) should equal (Success)
//    element0.getText should equal ("this sentence is 35 characters long")
//
//    tp.apply(stream,element1,variables,namespaces,List()) should equal (Success)
//    element1.getText should equal ("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
//  }
//
//
//  test("escape character fixed length at end") {
//    val tp = new TextProcessor(Charset.forName("UTF-8"),true)
//
//    tp setLengthKind(Explicit)
//    tp setLength("9")
//    tp setTextStringJustification(NoneJustification)
//    tp setTypeName(null)
//    tp setEscapeKind(EscapeCharacter)
//    tp setEscapeCharacter("#")
//
//    val input = new ByteArrayInputStream("abcdefg#ABBBBBBBBBC" getBytes("UTF-8"))
//
//    val stream = new RollbackStream(new BufferedInputStream(input))
//
//    val element0 = new Element("MyElement")
//    val element1 = new Element("MyElement")
//
//    val variables = new VariableMap
//    val namespaces = new Namespaces
//
//    tp.apply(stream,element0,variables,namespaces,List()) should equal (Success)
//    element0.getText should equal ("abcdefg")
//
//    tp.apply(stream,element1,variables,namespaces,List()) should equal (Success)
//    element1.getText should equal ("BBBBBBBBB")
//  }
//
//  test("escape character fixed length over the end") {
//    val tp = new TextProcessor(Charset.forName("UTF-8"),true)
//
//    tp setLengthKind(Explicit)
//    tp setLength("8")
//    tp setTextStringJustification(NoneJustification)
//    tp setTypeName(null)
//    tp setEscapeKind(EscapeCharacter)
//    tp setEscapeCharacter("#")
//
//    val input = new ByteArrayInputStream("abcdefg#ABCDEFGHI" getBytes("UTF-8"))
//
//    val stream = new RollbackStream(new BufferedInputStream(input))
//
//    val element0 = new Element("MyElement")
//    val element1 = new Element("MyElement")
//
//    val variables = new VariableMap
//    val namespaces = new Namespaces
//
//    tp.apply(stream,element0,variables,namespaces,List()) should equal (Success)
//    element0.getText should equal ("abcdefg")
//
//    tp.apply(stream,element1,variables,namespaces,List()) should equal (Success)
//    element1.getText should equal ("ABCDEFGH")
//  }
//
//  test("simple escape character, variable length") {
//    val tp = new TextProcessor(Charset.forName("UTF-8"),true)
//
//    tp setLengthKind(Delimited)
//    tp setSeparator(ListLiteralValue(List(",")))
//    tp setSeparatorPolicy(Require)
//    tp setSeparatorPosition(Postfix)
//    tp setTextStringJustification(NoneJustification)
//    tp setTypeName(null)
//    tp setEscapeKind(EscapeCharacter)
//    tp setEscapeCharacter("#")
//
//    val input = new ByteArrayInputStream("a word #incorrect, a second word" getBytes("UTF-8"))
//
//    val stream = new RollbackStream(new BufferedInputStream(input))
//
//    val element0 = new Element("MyElement")
//    val element1 = new Element("MyElement")
//
//    val variables = new VariableMap
//    val namespaces = new Namespaces
//
//
//    tp.apply(stream,element0,variables,namespaces,List()) should equal (Success)
//    tp.findPostfixSeparator(stream,element0,variables,namespaces) should be (SeparatorFound("",","))
//    element0.getText should equal ("a word ncorrect")
//
//    tp.apply(stream,element1,variables,namespaces,List()) should equal (Last)
//    tp.findPostfixSeparator(stream,element1,variables,namespaces) should be (NothingFound)
//    element1.getText should equal (" a second word")
//
//  }
//
//  test ("escape character escapes separator") {
//
//    val tp = new TextProcessor(Charset.forName("UTF-8"),true)
//
//    tp setLengthKind(Delimited)
//    tp setSeparator(ListLiteralValue(List(",")))
//    tp setSeparatorPolicy(Require)
//    tp setSeparatorPosition(Postfix)
//    tp setTextStringJustification(NoneJustification)
//    tp setTypeName(null)
//    tp setEscapeKind(EscapeCharacter)
//    tp setEscapeCharacter("#")
//
//
//    val input = new ByteArrayInputStream("a list#, is just a list, is just a list, is just a list" getBytes("UTF-8"))
//
//    val stream = new RollbackStream(new BufferedInputStream(input))
//
//    val element0 = new Element("MyElement")
//    val element1 = new Element("MyElement")
//    val element2 = new Element("MyElement")
//
//    val variables = new VariableMap
//    val namespaces = new Namespaces
//
//
//    tp.apply(stream,element0,variables,namespaces,List()) should equal (Success)
//    tp.findPostfixSeparator(stream,element0,variables,namespaces) should be (SeparatorFound("",","))
//    element0.getText should equal ("a list is just a list")
//
//    tp.apply(stream,element1,variables,namespaces,List()) should equal (Success)
//    tp.findPostfixSeparator(stream,element1,variables,namespaces) should be (SeparatorFound("",","))
//    element1.getText should equal (" is just a list")
//
//    tp.apply(stream,element2,variables,namespaces,List()) should equal (Last)
//    tp.findPostfixSeparator(stream,element2,variables,namespaces) should be (NothingFound)
//    element2.getText should equal (" is just a list")
//
//  }
//
//  test ("escape character escapes terminator") { pending }
//
//  test ("escape escape character") {
//
//    val tp = new TextProcessor(Charset.forName("UTF-8"),true)
//
//    tp setLengthKind(Delimited)
//    tp setSeparator(ListLiteralValue(List(",")))
//    tp setSeparatorPolicy(Require)
//    tp setSeparatorPosition(Infix)
//    tp setTextStringJustification(NoneJustification)
//    tp setTypeName(null)
//    tp setEscapeKind(EscapeCharacter)
//    tp setEscapeCharacter("#")
//    tp setEscapeEscapeCharacter("#")
//
//    val input = new ByteArrayInputStream("item#X2,".getBytes("UTF-8"))
//
//    val stream = new RollbackStream(new BufferedInputStream(input))
//    val element0 = new Element("MyElement")
//    val variables = new VariableMap
//    val namespaces = new Namespaces
//
//    tp.apply(stream,element0,variables,namespaces,List()) should equal (Success)
//    element0.getText should equal ("item2")
//
//  }


}