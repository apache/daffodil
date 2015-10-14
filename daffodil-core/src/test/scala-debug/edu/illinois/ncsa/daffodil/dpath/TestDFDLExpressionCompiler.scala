/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dpath

class TestDFDLExpressionCompiler {

  //  @Test def test_single_path_expression() = {
  //    val expr = "{ /bookstore/book/title }"
  //    val dfdl = new DFDLPathExpressionParser(NodeInfo.AnyType, null, null)
  //    val result = dfdl.getPathsFromExpression(expr, EmptyVariableMap)
  //
  //    assertTrue(result.isRight) // Success
  //
  //    result match {
  //      case Right((paths, vmap)) => {
  //        assertEquals(2, paths.length)
  //        assertEquals("/bookstore/book/title", paths(0).toString)
  //        assertEquals("bookstore/book/title", paths(1).toString)
  //      }
  //      case Left(_) => fail // FAILED
  //    }
  //  }
  //
  //  @Test def test_single_path_and_fnc_expression() = {
  //    val expr = "{ fn:nlled(/bookstore/book/title) }"
  //    val dfdl = new DFDLPathExpressionParser(NodeInfo.AnyType, null, null)
  //    val result = dfdl.getPathsFromExpression(expr, EmptyVariableMap)
  //    assertTrue(result.isRight) // Success
  //
  //    result match {
  //      case Right((paths, vmap)) => {
  //        assertEquals(2, paths.length)
  //        assertEquals("/bookstore/book/title", paths(0).toString)
  //        assertEquals("bookstore/book/title", paths(1).toString)
  //      }
  //      case Left(_) => fail // FAILED
  //    }
  //  }
  //
  //  @Test def test_multiple_path_and_predicate_expression() = {
  //    val expr = "{ fn:concat(/bookstore/book[1]/title,/bookstore/book[2]/title) }"
  //    val dfdl = new DFDLPathExpressionParser(NodeInfo.AnyType, null, null)
  //
  //    val result = dfdl.getPathsFromExpression(expr, EmptyVariableMap)
  //    assertTrue(result.isRight) // Success
  //
  //    result match {
  //      case Right((paths, vmap)) => {
  //        assertEquals(6, paths.length)
  //        assertEquals("/bookstore/book[1]/title", paths(0).toString)
  //        assertEquals("bookstore/book[1]/title", paths(1).toString)
  //        assertEquals("1", paths(2).toString)
  //        assertEquals("/bookstore/book[2]/title", paths(3).toString)
  //        assertEquals("bookstore/book[2]/title", paths(4).toString)
  //        assertEquals("2", paths(5).toString)
  //      }
  //      case Left(_) => fail // FAILED
  //    }
  //  }
  //
  //  @Test def test_multiple_path_and_predicate_expressions_nested() = {
  //    val expr = "{ fn:concat(/bookstore/book[number(../something/blah)]/title/text,/bookstore/book[2]/title) }"
  //    val dfdl = new DFDLPathExpressionParser(NodeInfo.AnyType, null, null)
  //
  //    val result = dfdl.getPathsFromExpression(expr, EmptyVariableMap)
  //    assertTrue(result.isRight) // Success
  //
  //    result match {
  //      case Right((paths, vmap)) => {
  //        assertEquals(6, paths.length)
  //        assertEquals("/bookstore/book[number(../something/blah)]/title/text", paths(0).toString)
  //        assertEquals("bookstore/book[number(../something/blah)]/title/text", paths(1).toString)
  //        assertEquals("../something/blah", paths(2).toString)
  //        assertEquals("/bookstore/book[2]/title", paths(3).toString)
  //        assertEquals("bookstore/book[2]/title", paths(4).toString)
  //        assertEquals("2", paths(5).toString)
  //      }
  //      case Left(_) => fail // FAILED
  //    }
  //  }

}
