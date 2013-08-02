package edu.illinois.ncsa.daffodil.dsom

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import scala.xml.Utility
import edu.illinois.ncsa.daffodil.processors.EmptyVariableMap

class TestDFDLExpressionCompiler {
  
  @Test def test_single_path_expression() = {
    val expr = "{ /bookstore/book/title }"
    val dfdl = new DFDLPathExpressionCompiler(null)
    val result = dfdl.getPathsFromExpression(expr, EmptyVariableMap)

    assertTrue(result.isRight) // Success
    
    result match {
      case Right((paths, vmap)) => {
        assertEquals(2, paths.length)
        assertEquals("/bookstore/book/title", paths(0).toString)
        assertEquals("bookstore/book/title", paths(1).toString)
      }
      case Left(_) => fail// FAILED
    }
  }

  @Test def test_single_path_and_fnc_expression() = {
    val expr = "{ fn:nlled(/bookstore/book/title) }"
    val dfdl = new DFDLPathExpressionCompiler(null)
    val result = dfdl.getPathsFromExpression(expr, EmptyVariableMap)
    assertTrue(result.isRight) // Success
    
    result match {
      case Right((paths, vmap)) => {
        assertEquals(2, paths.length)
        assertEquals("/bookstore/book/title", paths(0).toString)
        assertEquals("bookstore/book/title", paths(1).toString)
      }
      case Left(_) => fail// FAILED
    }
  }

  @Test def test_multiple_path_and_predicate_expression() = {
    val expr = "{ fn:concat(/bookstore/book[1]/title,/bookstore/book[2]/title) }"
    val dfdl = new DFDLPathExpressionCompiler(null)

    val result = dfdl.getPathsFromExpression(expr, EmptyVariableMap)
    assertTrue(result.isRight) // Success
    
    result match {
      case Right((paths, vmap)) => {
        assertEquals(6, paths.length)
        assertEquals("/bookstore/book[1]/title", paths(0).toString)
        assertEquals("bookstore/book[1]/title", paths(1).toString)
        assertEquals("1", paths(2).toString)
        assertEquals("/bookstore/book[2]/title", paths(3).toString)
        assertEquals("bookstore/book[2]/title", paths(4).toString)
        assertEquals("2", paths(5).toString)
      }
      case Left(_) => fail// FAILED
    }
  }

  @Test def test_multiple_path_and_predicate_expressions_nested() = {
    val expr = "{ fn:concat(/bookstore/book[number(../something/blah)]/title/text,/bookstore/book[2]/title) }"
    val dfdl = new DFDLPathExpressionCompiler(null)

    val result = dfdl.getPathsFromExpression(expr, EmptyVariableMap)
    assertTrue(result.isRight) // Success
    
    result match {
      case Right((paths, vmap)) => {
        assertEquals(6, paths.length)
        assertEquals("/bookstore/book[number(../something/blah)]/title/text", paths(0).toString)
        assertEquals("bookstore/book[number(../something/blah)]/title/text", paths(1).toString)
        assertEquals("../something/blah", paths(2).toString)
        assertEquals("/bookstore/book[2]/title", paths(3).toString)
        assertEquals("bookstore/book[2]/title", paths(4).toString)
        assertEquals("2", paths(5).toString)
      }
      case Left(_) => fail// FAILED
    }
  }

}
