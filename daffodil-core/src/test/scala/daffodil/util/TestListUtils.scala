package daffodil.util

import org.scalatest.junit.JUnit3Suite

import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import daffodil.util._


class TestListUtils extends JUnit3Suite {

  def testTailAfter1 = {
    val actual = ListUtils.tailAfter(List(1,2,3,4,5), 3)
    val expected = List(4,5)
    assertEquals(expected,actual)
  }
  
  def testTailAfter2 = {
    val actual = ListUtils.tailAfter(Nil, 3)
    val expected = Nil
    assertEquals(expected,actual)
  }
  
  def testTailAfter3 = {
    val actual = ListUtils.tailAfter(List(1,2,3,4,5), 5)
    val expected = Nil
    assertEquals(expected,actual)
  }
  
  def testTailAfter4 = {
    val actual = ListUtils.tailAfter(List(1,2,3,4,5), 0)
    val expected = Nil // The answer if not found at all is Nil
    assertEquals(expected,actual)
  }

   def testPreceding1 = {
    val actual = ListUtils.preceding(List(1,2,3,4,5), 3)
    val expected = List(1, 2)
    assertEquals(expected,actual)
  }
  
  def testPreceding2 = {
    val actual = ListUtils.preceding(Nil, 3)
    val expected = Nil
    assertEquals(expected,actual)
  }
  
  def testPreceding3 = {
    val actual = ListUtils.preceding(List(1,2,3,4,5), 1)
    val expected = Nil
    assertEquals(expected,actual)
  }
  
  def testPreceding4 = {
    val actual = ListUtils.preceding(List(1,2,3,4,5), 0)
    val expected = Nil // The answer if not found at all is Nil
    assertEquals(expected,actual)
  }
  
  def testPreceding5 = {
    val actual = ListUtils.preceding(List(1,2,3,4,5), 5)
    val expected = List(1,2,3,4)
    assertEquals(expected,actual)
  }

}