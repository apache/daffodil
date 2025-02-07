/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.io

import java.io.ByteArrayInputStream
import java.io.File
import java.io.InputStreamReader

import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.util.Maybe

import org.apache.commons.io.IOUtils
import org.junit.Assert._
import org.junit.Test

class TestDirectOrBufferedDataOutputStream {

  private def getString(baos: ByteArrayOrFileOutputStream) = {
    val is = new ByteArrayInputStream(baos.getBuf)
    val ir = new InputStreamReader(is, "ascii")
    val line = IOUtils.toString(ir)
    val res = line.replace("\u0000", "")
    res
  }

  def newDirectOrBufferedDataOutputStream(
    jos: java.io.OutputStream,
    creator: DirectOrBufferedDataOutputStream,
    bo: BitOrder = BitOrder.MostSignificantBitFirst
  ) = {
    val os = DirectOrBufferedDataOutputStream(
      jos,
      creator,
      false,
      4096,
      2000 * (1 << 20),
      new File("."),
      Maybe.Nope
    )
    os.setPriorBitOrder(bo)
    os
  }

  /**
   * Tests that the toString method doesn't throw. Can't even use a debugger
   * if that happens.
   */
  @Test def testToStringDoesNotThrow(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)
    assertFalse(layered.toString().isEmpty())
  }

  val finfo = FormatInfoForUnitTest()

  @Test def testCollapsingBufferIntoDirect1(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes, finfo)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered()

    buf1.putBytes("buf1".getBytes("ascii"), finfo)

    layered.setFinished(finfo) // collapses layered into buf1.

    assertTrue(layered.isDead)

    assertEquals(hw + "buf1", getString(baos))

    assertTrue(buf1.isDirect)
    assertFalse(buf1.isFinished)

  }

  @Test def testCollapsingFinishedBufferIntoLayered(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes, finfo)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered()

    buf1.putBytes("buf1".getBytes("ascii"), finfo)

    buf1.setFinished(finfo)

    assertTrue(buf1.isFinished)

    layered.setFinished(finfo) // collapses layered into buf1.

    assertTrue(buf1.isDead) // because it was finished when layered was subsequently finished
    assertTrue(layered.isDead)

    assertEquals(hw + "buf1", getString(baos))

  }

  @Test def testCollapsingTwoBuffersIntoDirect(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes, finfo)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered()
    val buf2 = buf1.addBuffered()

    buf1.putBytes("buf1".getBytes("ascii"), finfo)
    buf2.putBytes("buf2".getBytes("ascii"), finfo)

    assertTrue(buf2.isBuffering)

    buf1.setFinished(finfo) // buf1 finished while layered (before it) is still unfinished.

    assertTrue(buf1.isFinished)

    layered.setFinished(
      finfo
    ) // collapses layered into buf1. Since buf1 is finished already, this melds them, outputs everything
    // and leaves the whole thing finished.
    // leaves layered dead/unusable.

    assertTrue(buf1.isDead) // because it was finished when layered was subsequently finished
    assertTrue(layered.isDead)

    assertEquals(hw + "buf1" + "buf2", getString(baos))

    assertTrue(buf2.isDirect)

  }
}
