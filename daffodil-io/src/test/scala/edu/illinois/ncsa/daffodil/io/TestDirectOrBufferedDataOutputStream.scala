package edu.illinois.ncsa.daffodil.io

import org.junit.Test
import junit.framework.Assert._

import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import org.apache.commons.io.IOUtils

class TestDirectOrBufferedDataOutputStream {

  private def getString(baos: ByteArrayOutputStreamWithGetBuf) = {
    val is = new ByteArrayInputStream(baos.getBuf)
    val ir = new InputStreamReader(is, "ascii")
    val line = IOUtils.toString(ir)
    val res = line.replace("""\u0000""", "")
    res
  }

  @Test def testCollapsingBufferIntoDirect1 {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val layered = DirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered

    buf1.putBytes("buf1".getBytes("ascii"))

    layered.setFinished() // collapses layered into buf1.

    assertTrue(layered.isDead)

    assertEquals(hw + "buf1", getString(baos))

    assertTrue(buf1.isDirect)
    assertFalse(buf1.isFinished)

  }

  @Test def testCollapsingFinishedBufferIntoLayered {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val layered = DirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered

    buf1.putBytes("buf1".getBytes("ascii"))

    buf1.setFinished()

    assertTrue(buf1.isFinished)

    layered.setFinished() // collapses layered into buf1.

    assertTrue(buf1.isDead) // because it was finished when layered was subsequently finished
    assertTrue(layered.isDead)

    assertEquals(hw + "buf1", getString(baos))

  }

  @Test def testCollapsingTwoBuffersIntoDirect {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val layered = DirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered
    val buf2 = buf1.addBuffered

    buf1.putBytes("buf1".getBytes("ascii"))
    buf2.putBytes("buf2".getBytes("ascii"))

    assertTrue(buf2.isBuffering)

    buf1.setFinished() // buf1 finished while layered (before it) is still unfinished.

    assertTrue(buf1.isFinished)

    layered.setFinished() // collapses layered into buf1. Since buf1 is finished already, this melds them, outputs everything
    // and leaves the whole thing finished.
    // leaves layered dead/unusable.

    assertTrue(buf1.isDead) // because it was finished when layered was subsequently finished
    assertTrue(layered.isDead)

    assertEquals(hw + "buf1" + "buf2", getString(baos))

    assertTrue(buf2.isDirect)

  }
}
