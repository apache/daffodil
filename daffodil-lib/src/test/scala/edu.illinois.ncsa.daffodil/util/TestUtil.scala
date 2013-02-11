package edu.illinois.ncsa.daffodil.util

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.exceptions._
import org.scalatest.junit.JUnitSuite
import org.junit.Test

class TestUtil extends JUnitSuite {

  @Test def testGetRequiredResourceSucceeds() {
    val res = Misc.getRequiredResource("/xsd/XMLSchema.xsd")
    assertNotNull(res)
  }

  @Test def testGetRequiredResourceFails() {
    val e = intercept[Exception] {
      val res = Misc.getRequiredResource("/xsd/NotAResourceName.foo")
    }
    assertTrue(e.getMessage().contains("NotAResourceName"))
  }

  @Test
  def testStripQuotes() {
    assertEquals("foo", Misc.stripQuotes("\"foo\""))
  }

  @Test
  def testAssert() {
    intercept[Abort] {
      Assert.abort("yadda")
    }
  }

  @Test
  def testCopyrightNotices() {
    val fake = """
  Test fake Copyright (C) 3023 by Space Invaders of the Earth. All rights reserved.
  Of course these can be long verbose license statements and such.
  """
    CopyrightNotices.add(fake)
    val notices = CopyrightNotices.getNotices()
    val hasFake = notices.contains("Test fake Copyright")
    assertTrue(hasFake)
  }

  @Test
  def testBitsConverters1() {
    val bytes = Misc.bits2Bytes("11")
    val theByte = bytes(0)
    assertEquals(3, theByte.toInt)
  }

  @Test
  def testBitsConverters2() {
    val bytes = Misc.bits2Bytes("110110110110")
    val byte0 = bytes(0)
    val byte1 = bytes(1)
    assertEquals(-37, byte0.toInt)
    assertEquals(6, byte1.toInt)
  }

}
