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

package org.apache.daffodil

import junit.framework.Assert.assertEquals
import org.apache.daffodil.util._
import org.junit.Test
import org.apache.daffodil.util._

class TestByteOrder {

  @Test def testLittleEndianBitValue = {
    var bsl = 13
    assertEquals(0x80, BitsUtils.littleEndianBitValue(1, bsl))
    assertEquals(0x40, BitsUtils.littleEndianBitValue(2, bsl))
    assertEquals(0x20, BitsUtils.littleEndianBitValue(3, bsl))
    assertEquals(0x10, BitsUtils.littleEndianBitValue(4, bsl))
    assertEquals(0x08, BitsUtils.littleEndianBitValue(5, bsl))
    assertEquals(0x04, BitsUtils.littleEndianBitValue(6, bsl))
    assertEquals(0x02, BitsUtils.littleEndianBitValue(7, bsl))
    assertEquals(0x01, BitsUtils.littleEndianBitValue(8, bsl))
    assertEquals(0x1000, BitsUtils.littleEndianBitValue(9, bsl))
    assertEquals(0x800, BitsUtils.littleEndianBitValue(10, bsl))
    assertEquals(0x400, BitsUtils.littleEndianBitValue(11, bsl))
    assertEquals(0x200, BitsUtils.littleEndianBitValue(12, bsl))
    assertEquals(0x100, BitsUtils.littleEndianBitValue(13, bsl))

    bsl = 3
    assertEquals(0x4, BitsUtils.littleEndianBitValue(1, bsl))
    assertEquals(0x2, BitsUtils.littleEndianBitValue(2, bsl))
    assertEquals(0x1, BitsUtils.littleEndianBitValue(3, bsl))
  }
}

class TestBitOrder {

  @Test def testAsLSBitFirst = {
    assertEquals(0x20, Bits.asLSBitFirst(0x04))
    assertEquals(0x80, Bits.asLSBitFirst(1))
    assertEquals(0xA5, Bits.asLSBitFirst(0xA5))
    assertEquals(0xCC, Bits.asLSBitFirst(0x33))
  }

}