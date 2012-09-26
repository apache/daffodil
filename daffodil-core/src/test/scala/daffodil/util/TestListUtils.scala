package daffodil.util

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import daffodil.util._
import org.junit.Test


class TestListUtils extends JUnitSuite {

  @Test def testTailAfter1 = {
    val actual = ListUtils.tailAfter(List(1,2,3,4,5), 3)
    val expected = List(4,5)
    assertEquals(expected,actual)
  }
  
  @Test def testTailAfter2 = {
    val actual = ListUtils.tailAfter(Nil, 3)
    val expected = Nil
    assertEquals(expected,actual)
  }
  
  @Test def testTailAfter3 = {
    val actual = ListUtils.tailAfter(List(1,2,3,4,5), 5)
    val expected = Nil
    assertEquals(expected,actual)
  }
  
  @Test def testTailAfter4 = {
    val actual = ListUtils.tailAfter(List(1,2,3,4,5), 0)
    val expected = Nil // The answer if not found at all is Nil
    assertEquals(expected,actual)
  }

   @Test def testPreceding1 = {
    val actual = ListUtils.preceding(List(1,2,3,4,5), 3)
    val expected = List(1, 2)
    assertEquals(expected,actual)
  }
  
  @Test def testPreceding2 = {
    val actual = ListUtils.preceding(Nil, 3)
    val expected = Nil
    assertEquals(expected,actual)
  }
  
  @Test def testPreceding3 = {
    val actual = ListUtils.preceding(List(1,2,3,4,5), 1)
    val expected = Nil
    assertEquals(expected,actual)
  }
  
  @Test def testPreceding4 = {
    val actual = ListUtils.preceding(List(1,2,3,4,5), 0)
    val expected = Nil // The answer if not found at all is Nil
    assertEquals(expected,actual)
  }
  
  @Test def testPreceding5 = {
    val actual = ListUtils.preceding(List(1,2,3,4,5), 5)
    val expected = List(1,2,3,4)
    assertEquals(expected,actual)
  }

}