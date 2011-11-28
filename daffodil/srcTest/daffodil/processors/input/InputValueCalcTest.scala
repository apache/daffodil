package daffodil.processors.input

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

import org.jdom.Document
import org.jdom.Element
import org.jdom.Text

import java.nio.charset.Charset
import java.io.{BufferedInputStream, ByteArrayInputStream}
import org.jdom.Element
import daffodil.schema.annotation.ListLiteralValue
import daffodil.parser.RollbackStream
import daffodil.xml.Namespaces
import daffodil.schema.annotation.enumerations._
import daffodil.processors.input.text.TextProcessor
import daffodil.processors.Success
import daffodil.processors.ProcessorResult
import daffodil.processors.VariableMap
import daffodil.Implicits._
import daffodil.xml.XMLUtil.compare
import daffodil.exceptions._
import daffodil.processors.xpath.XPathUtil
import daffodil.processors.xpath.StringResult

import org.scalatest.junit.JUnit3Suite
import scala.collection.mutable.ListBuffer
import junit.framework.Assert._

class InputExpressionTest extends JUnit3Suite {

  // @Test
  def testCreatingInvalidProcessor() {
    intercept[ExpressionDefinitionException] {
      new InputExpressionProcessor("not an expression")
    }
    intercept[ExpressionDefinitionException] {
      new InputExpressionProcessor("not an expression } ")
    }
    intercept[ExpressionDefinitionException] {
      new InputExpressionProcessor(" { not an expression")
    }
    intercept[ExpressionDefinitionException] {
      new InputExpressionProcessor(" { not an expression } ")
    }
    new InputExpressionProcessor("{ 'is an expression'  }")
  }

  // @Test
  def testConstantExpression() {
    val processor = new InputExpressionProcessor("{ 16 + 26 }")

    val doc = new Document()
    val root = new Element("root")
    root addContent(new Text("42"))
    doc addContent(root)

    val doc2 = new Document()
    val root2 = new Element("root")
    doc2 addContent(root2)
    val vmap = new VariableMap()
    val ns = new Namespaces
    val l = List()

    val res : ProcessorResult = processor(null, root2, vmap, ns, l)
    
    assertEquals(Success, res)
    assertTrue(compare(root,root2))
  }

  // @Test
  def testPathExpression00() { // Man, this stuff is intricate. Baby baby steps here.
    /*
     * root
     *     hello
    */

    val doc = new Document()
    val root = new Element("root")
    val hello = new Text("hello")
  

    doc addContent (root)
    root addContent (hello )

    val variables = new VariableMap()
    val namespaces = new Namespaces
    val res = XPathUtil.evalExpression("/root/text()[1]",variables,root,namespaces)
    res match {
      case StringResult(s) => assertEquals("hello", s)
      case _ => fail("result was not a string: " + res)
    }
  }
  
  
   
  // @Test
  def testPathExpressionAbsolute() {
    /*
     * root
     *   a1
     *     a2
     *       "x"
    */

    val processor = new InputExpressionProcessor("{ /root/a1/a2/text()[1] }")

    val expectedDoc = new Document()
    val expectedRoot = new Element("root")
    val expectedA1 = new Element("a1")
    val expectedA2 = new Element("a2")

    expectedDoc addContent (expectedRoot)
    expectedRoot addContent (expectedA1)
    expectedA1 addContent (expectedA2)
    expectedA2 addContent ( new Text("x"))
    expectedRoot addContent ( new Text("x"))

    val actualDoc = new Document()
    val actualRoot = new Element("root")
    val actualA1 = new Element("a1")
    val actualA2 = new Element("a2")
    actualDoc addContent(actualRoot)
    actualRoot addContent (actualA1)
    actualA1 addContent (actualA2)
    actualA2 addContent ( new Text("x"))
    
    val map = new VariableMap()
    val ns = new Namespaces
    val ls = List()
    val res = processor(null,actualRoot, map, ns, ls) 
    assertEquals(Success, res)
    assertTrue(compare(expectedRoot,actualRoot))
  }

  // @Test
  def testPathExpressionRelative() {
    /*
     * root
     *   a1
     *     a2
     *       "x"
    */

    val processor = new InputExpressionProcessor("{ ../../a1/a2/text()[1] }")

    val expectedDoc = new Document()
    val expectedRoot = new Element("root")
    val expectedA1 = new Element("a1")
    val expectedA2 = new Element("a2")

    expectedDoc addContent (expectedRoot)
    expectedRoot addContent (expectedA1)
    expectedA1 addContent (expectedA2)
    expectedA2 addContent ( new Text("x"))
    expectedA2 addContent ( new Text("x"))

    val actualDoc = new Document()
    val actualRoot = new Element("root")
    val actualA1 = new Element("a1")
    val actualA2 = new Element("a2")
    actualDoc addContent(actualRoot)
    actualRoot addContent (actualA1)
    actualA1 addContent (actualA2)
    actualA2 addContent ( new Text("x"))
    
    val map = new VariableMap()
    val ns = new Namespaces
    val ls = List()
    val res = processor(null,actualA2, map, ns, ls) 
    assertEquals(Success, res)
    assertTrue(compare(expectedRoot,actualRoot))
  }

  
  // @Test
  def testPathExpression10() {
    /*
     * root
     *   a1
     *     a2
     *       "prefix"
     *   b1
     *     b2
     *       "postfix"
     *   c1
     *     c2
     *       "prefixpostfix"
    */

    val processor = new InputExpressionProcessor("{ concat(../../a1/a2,../../b1/b2) }")

    val expectedDoc = new Document()
    val expectedRoot = new Element("root")
    val expectedA1 = new Element("a1")
    val expectedA2 = new Element("a2")
    val expectedB1 = new Element("b1")
    val expectedB2 = new Element("b2")
    val expectedC1 = new Element("c1")
    val expectedC2 = new Element("c2")

    expectedDoc addContent (expectedRoot)
    expectedRoot addContent (expectedA1)
    expectedRoot addContent (expectedB1)
    expectedRoot addContent (expectedC1)
    expectedA1 addContent (expectedA2)
    expectedB1 addContent (expectedB2)
    expectedC1 addContent (expectedC2)
    expectedA2 addContent ( new Text("prefix"))
    expectedB2 addContent ( new Text("postfix"))
    expectedC2 addContent ( new Text("prefixpostfix"))

    val actualDoc = new Document()
    val actualRoot = new Element("root")
    val actualA1 = new Element("a1")
    val actualA2 = new Element("a2")
    val actualB1 = new Element("b1")
    val actualB2 = new Element("b2")
    val actualC1 = new Element("c1")
    val actualC2 = new Element("c2")

    actualDoc addContent(actualRoot)
    actualRoot addContent (actualA1)
    actualRoot addContent (actualB1)
    actualRoot addContent (actualC1)
    actualA1 addContent (actualA2)
    actualB1 addContent (actualB2)
    actualC1 addContent (actualC2)
    actualA2 addContent ( new Text("prefix"))
    actualB2 addContent ( new Text("postfix"))

    // Code style should support debugging!
    // That is, you put one function call per line. Makes
    // step-into/over/out-of debugging very tedious if you can't separate
    // calls of interest onto separate lines.
    val map = new VariableMap()
    val ns = new Namespaces
    val ls = List()
    val res = processor(null,actualC2, map, ns, ls) 
    assertEquals(Success, res)
    assertTrue(compare(expectedRoot,actualRoot))
  }
  
  // @Test
  def testPathExpression2() {
    /*
     * root
     *   a1
     *     a2
     *       c
     *       c
     *       c
     *     b1
     *       3
    */

    val processor = new InputExpressionProcessor("{ count(../a2/c) }")

    val expectedDoc = new Document()
    val expectedRoot = new Element("root")
    val expectedA1 = new Element("a1")
    val expectedA2 = new Element("a2")
    val expectedA3 = new Element("c")
    val expectedA4 = new Element("c")
    val expectedA5 = new Element("c")
    val expectedB1 = new Element("b1")

    expectedDoc addContent(expectedRoot)
    expectedRoot addContent (expectedA1)
    expectedA1 addContent (expectedA2)
    expectedA1 addContent (expectedB1)
    expectedA2 addContent (expectedA3)
    expectedA2 addContent (expectedA4)
    expectedA2 addContent (expectedA5)
    expectedB1 addContent ( new Text("3"))

    val actualDoc = new Document()
    val actualRoot = new Element("root")
    val actualA1 = new Element("a1")
    val actualA2 = new Element("a2")
    val actualA3 = new Element("c")
    val actualA4 = new Element("c")
    val actualA5 = new Element("c")
    val actualB1 = new Element("b1")

    actualDoc addContent(actualRoot)
    actualRoot addContent (actualA1)
    actualA1 addContent (actualA2)
    actualA1 addContent (actualB1)
    actualA2 addContent (actualA3)
    actualA2 addContent (actualA4)
    actualA2 addContent (actualA5)

    val map = new VariableMap()
    val ns = new Namespaces
    val ls = List()
    val res = processor(null, actualB1, map, ns, ls) 
    assertEquals(Success, res)
    assertTrue(compare(expectedRoot,actualRoot))
  }

  // @Test
  def testEmptyExpression() {
    val processor = new InputExpressionProcessor("{}")

    val expectedDoc = new Document()
    val expectedRoot = new Element("root")
    val expectedA1 = new Element("a1")

    expectedDoc addContent (expectedRoot)
    expectedRoot addContent(expectedA1)

    val actualDoc = new Document()
    val actualRoot = new Element("root")
    val actualA1 = new Element("a1")


    actualDoc addContent(actualRoot)
    actualRoot addContent(actualA1)

    val map = new VariableMap()
    val ns = new Namespaces
    val ls = List()
    val res = processor(null, actualRoot, map, ns, ls) 
    assertEquals(Success, res)

    assertTrue(compare(expectedRoot,actualRoot))
  }

//  test("return value should be actual node"){
//    val processor1 = new InputExpressionProcessor(" { 42 }")
//    val processor2 = new InputExpressionProcessor("")
//
//
//    val theNode = new Element("TheNode")
//
//    theNode addContent(new Text("42"))
//
//    processor1(null,new Document(),new VariableMap(),new Namespaces,List()) match {
//      case Success(n:Element) => compare(theNode,n) should be (true)
//      case _ => fail
//    }
//
//    processor2(null,new Document(),new VariableMap(),new Namespaces,List()) match {
//      case Success(n:Element) => compare(new Element("EmptyNode"),n) should be (true)
//      case _ => fail
//    }


//  }
    
}
