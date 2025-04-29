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

import java.io.File

import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.util.Maybe

import org.junit.Assert._
import org.junit.Test
import passera.unsigned.ULong

class TestDataOutputStream4 {

  val finfo = FormatInfoForUnitTest()
  finfo.fillByte = 0.toByte

  def setup(setAbs: Boolean = true, bitOrd: BitOrder = BitOrder.MostSignificantBitFirst) = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = DirectOrBufferedDataOutputStream(
      baos,
      null,
      false,
      4096,
      2000 * (1 << 20),
      new File("."),
      Maybe.Nope
    )
    direct.setPriorBitOrder(bitOrd)

    direct.putLong(0x5a5a5, 19, finfo)

    // X101 1010 0101 1010 0101
    // 1011 0100 1011 0100 101X  big endian, MSBF

    val out = direct.addBuffered()
    if (setAbs)
      out.setAbsStartingBitPos0b(ULong(19))
    val out2 = out.addBuffered()
    if (setAbs)
      out2.setAbsStartingBitPos0b(ULong(38))

    out.putLong(0x5a5a5, 19, finfo)
    // 101 1010 0101 1010 0101
    // 1 0110 1001 0110 1001 01XX

    out2.putLong(0x5a5a5, 19, finfo)
    // 101 1010 0101 1010 0101
    // 10 1101 0010 1101 0010 1XXX

    // So the whole data is:
    // 1011 0100 1011 0100 101X
    //                        1 0110 1001 0110 1001 01XX
    //                                                10 1101 0010 1101 0010 1XXX  XXXX
    // B     4    B    4     B   6    9    6    9     6   D    2     D    2    8   0

    (baos, direct, out, out2)
  }

  private def checkResults(baos: ByteArrayOrFileOutputStream): Unit = {
    val buf = baos.getBuf

    assertEquals(0xb4.toByte, buf(0))
    assertEquals(0xb4.toByte, buf(1))
    assertEquals(0xb6.toByte, buf(2))
    assertEquals(0x96.toByte, buf(3))
    assertEquals(0x96.toByte, buf(4))
    assertEquals(0xd2.toByte, buf(5))
    assertEquals(0xd2.toByte, buf(6))
    assertEquals(0x80.toByte, buf(7))
  }

  @Test def testPutLong19FinishInOrderAbs(): Unit = {
    val (baos, direct, out, out2) = setup()

    direct.setFinished(finfo)
    out.setFinished(finfo)
    out2.setFinished(finfo)

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder1Abs(): Unit = {
    val (baos, direct, out, out2) = setup()

    out.setFinished(finfo)
    direct.setFinished(finfo)
    out2.setFinished(finfo)

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder2Abs(): Unit = {
    val (baos, direct, out, out2) = setup()

    out2.setFinished(finfo)
    out.setFinished(finfo)
    direct.setFinished(finfo)

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder3Abs(): Unit = {
    val (baos, direct, out, out2) = setup()

    out2.setFinished(finfo)
    direct.setFinished(finfo)
    out.setFinished(finfo)

    checkResults(baos)

  }

  @Test def testPutLong19FinishInOrder(): Unit = {
    val (baos, direct, out, out2) = setup(false)

    direct.setFinished(finfo)
    out.setFinished(finfo)
    out2.setFinished(finfo)

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder1(): Unit = {
    val (baos, direct, out, out2) = setup(false)

    out.setFinished(finfo)
    direct.setFinished(finfo)
    out2.setFinished(finfo)

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder2(): Unit = {
    val (baos, direct, out, out2) = setup(false)

    out2.setFinished(finfo)
    out.setFinished(finfo)
    direct.setFinished(finfo)

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder3(): Unit = {
    val (baos, direct, out, out2) = setup(false)

    out2.setFinished(finfo)
    direct.setFinished(finfo)
    out.setFinished(finfo)

    checkResults(baos)

  }
}
