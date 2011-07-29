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
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import daffodil.exceptions._
import daffodil.processors.input._
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil.compare
import daffodil.processors.VariableMap
import daffodil.processors.Success

class InputExpressionTest extends FunSuite with ShouldMatchers {

//  test("creating invalid processor"){
//    evaluating { new InputExpressionProcessor("not an expression") } should produce [ExpressionDefinitionException]
//    evaluating { new InputExpressionProcessor("not an expression } ") } should produce [ExpressionDefinitionException]
//    evaluating { new InputExpressionProcessor(" { not an expression ") } should produce [ExpressionDefinitionException]
//  }
//
//  test("basic expression 1"){
//    val processor = new InputExpressionProcessor(" { 16 + 26 } ")
//
//    val doc = new Document()
//    val root = new Element("root")
//    val n1 = new Element("child")
//    n1 addContent (new Text("42"))
//    root addContent(n1)
//    doc addContent(root)
//
//    val doc2 = new Document()
//    val root2 = new Element("root")
//    doc2 addContent(root2)
//
//    processor(null,root2,new VariableMap(),new Namespaces,List()).isInstanceOf[_root_.daffodil.processors.Success] should be (true)
//
//
//
//    compare(root,root2) should be (true)
//  }
//
//  test("path expression 1"){
//    /*
//     * root
//     *   a1
//     *     a2
//     *       "prefix"
//     *   b1
//     *     b2
//     *       "postfix"
//     *   c1
//     *     c2
//     *       "prefixpostfix"
//    */
//
//    val processor = new InputExpressionProcessor(" { concat(../../a1/a2,../../b1/b2) } ")
//
//    val expectedDoc = new Document()
//    val expectedRoot = new Element("root")
//    val expectedA1 = new Element("a1")
//    val expectedA2 = new Element("a2")
//    val expectedB1 = new Element("b1")
//    val expectedB2 = new Element("b2")
//    val expectedC1 = new Element("c1")
//    val expectedC2 = new Element("c2")
//
//    expectedDoc addContent (expectedRoot)
//    expectedRoot addContent (expectedA1)
//    expectedRoot addContent (expectedB1)
//    expectedRoot addContent (expectedC1)
//    expectedA1 addContent (expectedA2)
//    expectedB1 addContent (expectedB2)
//    expectedC1 addContent (expectedC2)
//    expectedA2 addContent ( new Text("prefix"))
//    expectedB2 addContent ( new Text("postfix"))
//    expectedC2 addContent ( new Text("prefixpostfix"))
//
//    val actualDoc = new Document()
//    val actualRoot = new Element("root")
//    val actualA1 = new Element("a1")
//    val actualA2 = new Element("a2")
//    val actualB1 = new Element("b1")
//    val actualB2 = new Element("b2")
//    val actualC1 = new Element("c1")
//    val actualC2 = new Element("c2")
//
//    actualDoc addContent(actualRoot)
//    actualRoot addContent (actualA1)
//    actualRoot addContent (actualB1)
//    actualRoot addContent (actualC1)
//    actualA1 addContent (actualA2)
//    actualB1 addContent (actualB2)
//    actualA2 addContent ( new Text("prefix"))
//    actualB2 addContent ( new Text("postfix"))
//
//
//    processor(null,actualC1,new VariableMap(),new Namespaces,List()).isInstanceOf[Success] should be (true)
//
//    compare(expectedRoot,actualRoot) should be (true)
//  }
//
//  test("path expression 2"){
//    /*
//     * root
//     *   a1
//     *     a2
//     *       c
//     *       c
//     *       c
//     *     b1
//     *       3
//    */
//
//    val processor = new InputExpressionProcessor(" { count(../a2/c) } ")
//
//    val expectedDoc = new Document()
//    val expectedRoot = new Element("root")
//    val expectedA1 = new Element("a1")
//    val expectedA2 = new Element("a2")
//    val expectedA3 = new Element("c")
//    val expectedA4 = new Element("c")
//    val expectedA5 = new Element("c")
//    val expectedB1 = new Element("b1")
//
//    expectedDoc addContent(expectedRoot)
//    expectedRoot addContent (expectedA1)
//    expectedA1 addContent (expectedA2)
//    expectedA1 addContent (expectedB1)
//    expectedA2 addContent (expectedA3)
//    expectedA2 addContent (expectedA4)
//    expectedA2 addContent (expectedA5)
//    expectedB1 addContent ( new Text("3"))
//
//    val actualDoc = new Document()
//    val actualRoot = new Element("root")
//    val actualA1 = new Element("a1")
//    val actualA2 = new Element("a2")
//    val actualA3 = new Element("c")
//    val actualA4 = new Element("c")
//    val actualA5 = new Element("c")
//
//    actualDoc addContent(actualRoot)
//    actualRoot addContent (actualA1)
//    actualA1 addContent (actualA2)
//    actualA2 addContent (actualA3)
//    actualA2 addContent (actualA4)
//    actualA2 addContent (actualA5)
//
//    processor(null,actualA1,new VariableMap(),new Namespaces,List()).isInstanceOf[Success] should be (true)
//
//    compare(expectedRoot,actualRoot) should be (true)
//  }
//
//  test("empty expression"){
//    val processor = new InputExpressionProcessor("")
//
//    val expectedDoc = new Document()
//    val expectedRoot = new Element("root")
//    val a1 = new Element("a1")
//
//    expectedDoc addContent (expectedRoot)
//    expectedRoot addContent(a1)
//
//    val actualDoc = new Document()
//    val actualRoot = new Element("root")
//
//
//    actualDoc addContent(actualRoot)
//
//    processor(null,actualRoot,new VariableMap(),new Namespaces,List()).isInstanceOf[Success] should be (true)
//
//    compare(expectedRoot,actualRoot) should be (true)
//  }
//
////  test("return value should be actual node"){
////    val processor1 = new InputExpressionProcessor(" { 42 }")
////    val processor2 = new InputExpressionProcessor("")
////
////
////    val theNode = new Element("TheNode")
////
////    theNode addContent(new Text("42"))
////
////    processor1(null,new Document(),new VariableMap(),new Namespaces,List()) match {
////      case Success(n:Element) => compare(theNode,n) should be (true)
////      case _ => fail
////    }
////
////    processor2(null,new Document(),new VariableMap(),new Namespaces,List()) match {
////      case Success(n:Element) => compare(new Element("EmptyNode"),n) should be (true)
////      case _ => fail
////    }
//
//
////  }
    
}
