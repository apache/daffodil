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

package org.apache.daffodil.util

import org.apache.daffodil.util.DecimalUtils._
import org.apache.daffodil.schema.annotation.props.gen.{ BinaryNumberCheckPolicy, TextZonedSignStyle }

import java.math.{ BigInteger => JBigInteger, BigDecimal => JBigDecimal }
import org.junit.Test
import org.junit.Assert._

class TestDecimalUtils {

  @Test def packedInt1StrictPos(): Unit = {
    val num = new Array[Byte](1)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x1C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("1"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt2StrictPos(): Unit = {
    val num = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x2C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("12"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt3StrictPos(): Unit = {
    val num = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x12.toByte
    num(1) = 0x3C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("123"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt4StrictPos(): Unit = {
    val num = new Array[Byte](6)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    num(2) = 0x45.toByte
    num(3) = 0x67.toByte
    num(4) = 0x89.toByte
    num(5) = 0x0C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("1234567890"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt5StrictPos(): Unit = {
    val num = new Array[Byte](11)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x00.toByte
    num(1) = 0x00.toByte
    num(2) = 0x00.toByte
    num(3) = 0x00.toByte
    num(4) = 0x00.toByte
    num(5) = 0x01.toByte
    num(6) = 0x23.toByte
    num(7) = 0x45.toByte
    num(8) = 0x67.toByte
    num(9) = 0x89.toByte
    num(10) = 0x0C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("1234567890"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt1StrictNeg(): Unit = {
    val num = new Array[Byte](1)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x1D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-1"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt2StrictNeg(): Unit = {
    val num = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x2D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-12"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt3StrictNeg(): Unit = {
    val num = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x12.toByte
    num(1) = 0x3D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-123"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt4StrictNeg(): Unit = {
    val num = new Array[Byte](6)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    num(2) = 0x45.toByte
    num(3) = 0x67.toByte
    num(4) = 0x89.toByte
    num(5) = 0x0D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-1234567890"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt5StrictNeg(): Unit = {
    val num = new Array[Byte](11)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x00.toByte
    num(1) = 0x00.toByte
    num(2) = 0x00.toByte
    num(3) = 0x00.toByte
    num(4) = 0x00.toByte
    num(5) = 0x01.toByte
    num(6) = 0x23.toByte
    num(7) = 0x45.toByte
    num(8) = 0x67.toByte
    num(9) = 0x89.toByte
    num(10) = 0x0D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-1234567890"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt1LaxPos(): Unit = {
    val num = new Array[Byte](1)
    val expected = new Array[Byte](1)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x1A.toByte
    expected(0) = 0x1C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("1"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt2LaxPos(): Unit = {
    val num = new Array[Byte](2)
    val expected = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x01.toByte
    num(1) = 0x2C.toByte
    expected(0) = 0x01.toByte
    expected(1) = 0x2C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("12"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt3LaxPos(): Unit = {
    val num = new Array[Byte](2)
    val expected = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x12.toByte
    num(1) = 0x3E.toByte
    expected(0) = 0x12.toByte
    expected(1) = 0x3C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("123"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt4LaxPos(): Unit = {
    val num = new Array[Byte](6)
    val expected = new Array[Byte](6)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    num(2) = 0x45.toByte
    num(3) = 0x67.toByte
    num(4) = 0x89.toByte
    num(5) = 0x0F.toByte
    expected(0) = 0x01.toByte
    expected(1) = 0x23.toByte
    expected(2) = 0x45.toByte
    expected(3) = 0x67.toByte
    expected(4) = 0x89.toByte
    expected(5) = 0x0C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("1234567890"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt5LaxPos(): Unit = {
    val num = new Array[Byte](11)
    val expected = new Array[Byte](11)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x00.toByte
    num(1) = 0x00.toByte
    num(2) = 0x00.toByte
    num(3) = 0x00.toByte
    num(4) = 0x00.toByte
    num(5) = 0x01.toByte
    num(6) = 0x23.toByte
    num(7) = 0x45.toByte
    num(8) = 0x67.toByte
    num(9) = 0x89.toByte
    num(10) = 0x0A.toByte
    expected(0) = 0x00.toByte
    expected(1) = 0x00.toByte
    expected(2) = 0x00.toByte
    expected(3) = 0x00.toByte
    expected(4) = 0x00.toByte
    expected(5) = 0x01.toByte
    expected(6) = 0x23.toByte
    expected(7) = 0x45.toByte
    expected(8) = 0x67.toByte
    expected(9) = 0x89.toByte
    expected(10) = 0x0C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("1234567890"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt1LaxNeg(): Unit = {
    val num = new Array[Byte](1)
    val expected = new Array[Byte](1)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x1B.toByte
    expected(0) = 0x1D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-1"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt2LaxNeg(): Unit = {
    val num = new Array[Byte](2)
    val expected = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x01.toByte
    num(1) = 0x2D.toByte
    expected(0) = 0x01.toByte
    expected(1) = 0x2D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-12"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt3LaxNeg(): Unit = {
    val num = new Array[Byte](2)
    val expected = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x12.toByte
    num(1) = 0x3B.toByte
    expected(0) = 0x12.toByte
    expected(1) = 0x3D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-123"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt4LaxNeg(): Unit = {
    val num = new Array[Byte](6)
    val expected = new Array[Byte](6)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    num(2) = 0x45.toByte
    num(3) = 0x67.toByte
    num(4) = 0x89.toByte
    num(5) = 0x0D.toByte
    expected(0) = 0x01.toByte
    expected(1) = 0x23.toByte
    expected(2) = 0x45.toByte
    expected(3) = 0x67.toByte
    expected(4) = 0x89.toByte
    expected(5) = 0x0D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-1234567890"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt5LaxNeg(): Unit = {
    val num = new Array[Byte](11)
    val expected = new Array[Byte](11)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x00.toByte
    num(1) = 0x00.toByte
    num(2) = 0x00.toByte
    num(3) = 0x00.toByte
    num(4) = 0x00.toByte
    num(5) = 0x01.toByte
    num(6) = 0x23.toByte
    num(7) = 0x45.toByte
    num(8) = 0x67.toByte
    num(9) = 0x89.toByte
    num(10) = 0x0B.toByte
    expected(0) = 0x00.toByte
    expected(1) = 0x00.toByte
    expected(2) = 0x00.toByte
    expected(3) = 0x00.toByte
    expected(4) = 0x00.toByte
    expected(5) = 0x01.toByte
    expected(6) = 0x23.toByte
    expected(7) = 0x45.toByte
    expected(8) = 0x67.toByte
    expected(9) = 0x89.toByte
    expected(10) = 0x0D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-1234567890"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedDec1StrictPos(): Unit = {
    val num = new Array[Byte](1)
    val scale = 0
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x1C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("1"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec2StrictPos(): Unit = {
    val num = new Array[Byte](2)
    val scale = 1
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x2C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("1.2"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec3StrictPos(): Unit = {
    val num = new Array[Byte](2)
    val scale = 3
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x12.toByte
    num(1) = 0x3C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal(".123"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec4StrictPos(): Unit = {
    val num = new Array[Byte](6)
    val scale = 5
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    num(2) = 0x45.toByte
    num(3) = 0x67.toByte
    num(4) = 0x89.toByte
    num(5) = 0x0C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("12345.67890"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec5StrictPos(): Unit = {
    val num = new Array[Byte](11)
    val scale = 19
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x00.toByte
    num(1) = 0x00.toByte
    num(2) = 0x00.toByte
    num(3) = 0x00.toByte
    num(4) = 0x00.toByte
    num(5) = 0x01.toByte
    num(6) = 0x23.toByte
    num(7) = 0x45.toByte
    num(8) = 0x67.toByte
    num(9) = 0x89.toByte
    num(10) = 0x0C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("0.0000000001234567890"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec1StrictNeg(): Unit = {
    val num = new Array[Byte](1)
    val scale = 0
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x1D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-1"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec2StrictNeg(): Unit = {
    val num = new Array[Byte](2)
    val scale = 1
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x2D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-1.2"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec3StrictNeg(): Unit = {
    val num = new Array[Byte](2)
    val scale = 3
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x12.toByte
    num(1) = 0x3D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-.123"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec4StrictNeg(): Unit = {
    val num = new Array[Byte](6)
    val scale = 5
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    num(2) = 0x45.toByte
    num(3) = 0x67.toByte
    num(4) = 0x89.toByte
    num(5) = 0x0D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-12345.67890"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec5StrictNeg(): Unit = {
    val num = new Array[Byte](11)
    val scale = 19
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x00.toByte
    num(1) = 0x00.toByte
    num(2) = 0x00.toByte
    num(3) = 0x00.toByte
    num(4) = 0x00.toByte
    num(5) = 0x01.toByte
    num(6) = 0x23.toByte
    num(7) = 0x45.toByte
    num(8) = 0x67.toByte
    num(9) = 0x89.toByte
    num(10) = 0x0D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-0.0000000001234567890"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec1LaxPos(): Unit = {
    val num = new Array[Byte](1)
    val expected = new Array[Byte](1)
    val scale = 0
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x1A.toByte
    expected(0) = 0x1C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("1"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), expected)
  }

  @Test def packedDec2LaxPos(): Unit = {
    val num = new Array[Byte](2)
    val expected = new Array[Byte](2)
    val scale = 1
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x01.toByte
    num(1) = 0x2C.toByte
    expected(0) = 0x01.toByte
    expected(1) = 0x2C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("1.2"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), expected)
  }

  @Test def packedDec3LaxPos(): Unit = {
    val num = new Array[Byte](2)
    val expected = new Array[Byte](2)
    val scale = 3
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x12.toByte
    num(1) = 0x3E.toByte
    expected(0) = 0x12.toByte
    expected(1) = 0x3C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal(".123"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), expected)
  }

  @Test def packedDec4LaxPos(): Unit = {
    val num = new Array[Byte](6)
    val expected = new Array[Byte](6)
    val scale = 5
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    num(2) = 0x45.toByte
    num(3) = 0x67.toByte
    num(4) = 0x89.toByte
    num(5) = 0x0F.toByte
    expected(0) = 0x01.toByte
    expected(1) = 0x23.toByte
    expected(2) = 0x45.toByte
    expected(3) = 0x67.toByte
    expected(4) = 0x89.toByte
    expected(5) = 0x0C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("12345.67890"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), expected)
  }

  @Test def packedDec5LaxPos(): Unit = {
    val num = new Array[Byte](11)
    val expected = new Array[Byte](11)
    val scale = 19
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x00.toByte
    num(1) = 0x00.toByte
    num(2) = 0x00.toByte
    num(3) = 0x00.toByte
    num(4) = 0x00.toByte
    num(5) = 0x01.toByte
    num(6) = 0x23.toByte
    num(7) = 0x45.toByte
    num(8) = 0x67.toByte
    num(9) = 0x89.toByte
    num(10) = 0x0A.toByte
    expected(0) = 0x00.toByte
    expected(1) = 0x00.toByte
    expected(2) = 0x00.toByte
    expected(3) = 0x00.toByte
    expected(4) = 0x00.toByte
    expected(5) = 0x01.toByte
    expected(6) = 0x23.toByte
    expected(7) = 0x45.toByte
    expected(8) = 0x67.toByte
    expected(9) = 0x89.toByte
    expected(10) = 0x0C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("0.0000000001234567890"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), expected)
  }

  @Test def packedDec1LaxNeg(): Unit = {
    val num = new Array[Byte](1)
    val expected = new Array[Byte](1)
    val scale = 0
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x1B.toByte
    expected(0) = 0x1D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-1"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), expected)
  }

  @Test def packedDec2LaxNeg(): Unit = {
    val num = new Array[Byte](2)
    val expected = new Array[Byte](2)
    val scale = 1
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x01.toByte
    num(1) = 0x2D.toByte
    expected(0) = 0x01.toByte
    expected(1) = 0x2D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-1.2"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), expected)
  }

  @Test def packedDec3LaxNeg(): Unit = {
    val num = new Array[Byte](2)
    val expected = new Array[Byte](2)
    val scale = 3
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x12.toByte
    num(1) = 0x3B.toByte
    expected(0) = 0x12.toByte
    expected(1) = 0x3D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-.123"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), expected)
  }

  @Test def packedDec4LaxNeg(): Unit = {
    val num = new Array[Byte](6)
    val expected = new Array[Byte](6)
    val scale = 5
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    num(2) = 0x45.toByte
    num(3) = 0x67.toByte
    num(4) = 0x89.toByte
    num(5) = 0x0D.toByte
    expected(0) = 0x01.toByte
    expected(1) = 0x23.toByte
    expected(2) = 0x45.toByte
    expected(3) = 0x67.toByte
    expected(4) = 0x89.toByte
    expected(5) = 0x0D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-12345.67890"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), expected)
  }

  @Test def packedDec5LaxNeg(): Unit = {
    val num = new Array[Byte](11)
    val expected = new Array[Byte](11)
    val scale = 19
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x00.toByte
    num(1) = 0x00.toByte
    num(2) = 0x00.toByte
    num(3) = 0x00.toByte
    num(4) = 0x00.toByte
    num(5) = 0x01.toByte
    num(6) = 0x23.toByte
    num(7) = 0x45.toByte
    num(8) = 0x67.toByte
    num(9) = 0x89.toByte
    num(10) = 0x0B.toByte
    expected(0) = 0x00.toByte
    expected(1) = 0x00.toByte
    expected(2) = 0x00.toByte
    expected(3) = 0x00.toByte
    expected(4) = 0x00.toByte
    expected(5) = 0x01.toByte
    expected(6) = 0x23.toByte
    expected(7) = 0x45.toByte
    expected(8) = 0x67.toByte
    expected(9) = 0x89.toByte
    expected(10) = 0x0D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-0.0000000001234567890"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), expected)
  }

  @Test def ibm4690Int1Pos(): Unit = {
    val num = new Array[Byte](1)

    num(0) = 0xF1.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("1"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int2Pos(): Unit = {
    val num = new Array[Byte](1)

    num(0) = 0x12.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("12"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int3Pos(): Unit = {
    val num = new Array[Byte](2)

    num(0) = 0xF1.toByte
    num(1) = 0x23.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("123"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int4Pos(): Unit = {
    val num = new Array[Byte](5)

    num(0) = 0x12.toByte
    num(1) = 0x34.toByte
    num(2) = 0x56.toByte
    num(3) = 0x78.toByte
    num(4) = 0x90.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("1234567890"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int5Pos(): Unit = {
    val num = new Array[Byte](10)

    num(0) = 0xFF.toByte
    num(1) = 0xFF.toByte
    num(2) = 0xFF.toByte
    num(3) = 0xFF.toByte
    num(4) = 0xFF.toByte
    num(5) = 0x12.toByte
    num(6) = 0x34.toByte
    num(7) = 0x56.toByte
    num(8) = 0x78.toByte
    num(9) = 0x90.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("1234567890"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int1Neg(): Unit = {
    val num = new Array[Byte](1)

    num(0) = 0xD1.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("-1"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int2Neg(): Unit = {
    val num = new Array[Byte](2)

    num(0) = 0xFD.toByte
    num(1) = 0x12.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("-12"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int3Neg(): Unit = {
    val num = new Array[Byte](2)

    num(0) = 0xD1.toByte
    num(1) = 0x23.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("-123"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int4Neg(): Unit = {
    val num = new Array[Byte](6)

    num(0) = 0xFD.toByte
    num(1) = 0x12.toByte
    num(2) = 0x34.toByte
    num(3) = 0x56.toByte
    num(4) = 0x78.toByte
    num(5) = 0x90.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("-1234567890"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int5Neg(): Unit = {
    val num = new Array[Byte](11)

    num(0) = 0xFF.toByte
    num(1) = 0xFF.toByte
    num(2) = 0xFF.toByte
    num(3) = 0xFF.toByte
    num(4) = 0xFF.toByte
    num(5) = 0xFD.toByte
    num(6) = 0x12.toByte
    num(7) = 0x34.toByte
    num(8) = 0x56.toByte
    num(9) = 0x78.toByte
    num(10) = 0x90.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("-1234567890"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Dec1Pos(): Unit = {
    val num = new Array[Byte](1)
    val scale = 0

    num(0) = 0xF1.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("1"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec2Pos(): Unit = {
    val num = new Array[Byte](1)
    val scale = 1

    num(0) = 0x12.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("1.2"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec3Pos(): Unit = {
    val num = new Array[Byte](2)
    val scale = 3

    num(0) = 0xF1.toByte
    num(1) = 0x23.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal(".123"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec4Pos(): Unit = {
    val num = new Array[Byte](5)
    val scale = 5

    num(0) = 0x12.toByte
    num(1) = 0x34.toByte
    num(2) = 0x56.toByte
    num(3) = 0x78.toByte
    num(4) = 0x90.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("12345.67890"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec5Pos(): Unit = {
    val num = new Array[Byte](10)
    val scale = 19

    num(0) = 0xFF.toByte
    num(1) = 0xFF.toByte
    num(2) = 0xFF.toByte
    num(3) = 0xFF.toByte
    num(4) = 0xFF.toByte
    num(5) = 0x12.toByte
    num(6) = 0x34.toByte
    num(7) = 0x56.toByte
    num(8) = 0x78.toByte
    num(9) = 0x90.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("0.0000000001234567890"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec1Neg(): Unit = {
    val num = new Array[Byte](1)
    val scale = 0

    num(0) = 0xD1.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("-1"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec2Neg(): Unit = {
    val num = new Array[Byte](2)
    val scale = 1

    num(0) = 0xFD.toByte
    num(1) = 0x12.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("-1.2"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec3Neg(): Unit = {
    val num = new Array[Byte](2)
    val scale = 3

    num(0) = 0xD1.toByte
    num(1) = 0x23.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("-.123"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec4Neg(): Unit = {
    val num = new Array[Byte](6)
    val scale = 5

    num(0) = 0xFD.toByte
    num(1) = 0x12.toByte
    num(2) = 0x34.toByte
    num(3) = 0x56.toByte
    num(4) = 0x78.toByte
    num(5) = 0x90.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("-12345.67890"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec5Neg(): Unit = {
    val num = new Array[Byte](11)
    val scale = 19

    num(0) = 0xFF.toByte
    num(1) = 0xFF.toByte
    num(2) = 0xFF.toByte
    num(3) = 0xFF.toByte
    num(4) = 0xFF.toByte
    num(5) = 0xFD.toByte
    num(6) = 0x12.toByte
    num(7) = 0x34.toByte
    num(8) = 0x56.toByte
    num(9) = 0x78.toByte
    num(10) = 0x90.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("-0.0000000001234567890"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def bcdInt1Pos(): Unit = {
    val num = new Array[Byte](1)

    num(0) = 0x01.toByte
    val bignum = bcdToBigInteger(num)
    assertEquals(bignum, new JBigInteger("1"))
    assertArrayEquals(bcdFromBigInteger(bignum, num.length*8), num)
  }

  @Test def bcdInt2Pos(): Unit = {
    val num = new Array[Byte](1)

    num(0) = 0x12.toByte
    val bignum = bcdToBigInteger(num)
    assertEquals(bignum, new JBigInteger("12"))
    assertArrayEquals(bcdFromBigInteger(bignum, num.length*8), num)
  }

  @Test def bcdInt3Pos(): Unit = {
    val num = new Array[Byte](2)

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    val bignum = bcdToBigInteger(num)
    assertEquals(bignum, new JBigInteger("123"))
    assertArrayEquals(bcdFromBigInteger(bignum, num.length*8), num)
  }

  @Test def bcdInt4Pos(): Unit = {
    val num = new Array[Byte](5)

    num(0) = 0x12.toByte
    num(1) = 0x34.toByte
    num(2) = 0x56.toByte
    num(3) = 0x78.toByte
    num(4) = 0x90.toByte
    val bignum = bcdToBigInteger(num)
    assertEquals(bignum, new JBigInteger("1234567890"))
    assertArrayEquals(bcdFromBigInteger(bignum, num.length*8), num)
  }

  @Test def bcdInt5Pos(): Unit = {
    val num = new Array[Byte](10)

    num(0) = 0x00.toByte
    num(1) = 0x00.toByte
    num(2) = 0x00.toByte
    num(3) = 0x00.toByte
    num(4) = 0x00.toByte
    num(5) = 0x12.toByte
    num(6) = 0x34.toByte
    num(7) = 0x56.toByte
    num(8) = 0x78.toByte
    num(9) = 0x90.toByte
    val bignum = bcdToBigInteger(num)
    assertEquals(bignum, new JBigInteger("1234567890"))
    assertArrayEquals(bcdFromBigInteger(bignum, num.length*8), num)
  }

  @Test def bcdDec1Pos(): Unit = {
    val num = new Array[Byte](1)
    val scale = 0

    num(0) = 0x01.toByte
    val bignum = bcdToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("1"))
    assertArrayEquals(bcdFromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def bcdDec2Pos(): Unit = {
    val num = new Array[Byte](1)
    val scale = 1

    num(0) = 0x12.toByte
    val bignum = bcdToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("1.2"))
    assertArrayEquals(bcdFromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def bcdDec3Pos(): Unit = {
    val num = new Array[Byte](2)
    val scale = 3

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    val bignum = bcdToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal(".123"))
    assertArrayEquals(bcdFromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def bcdDec4Pos(): Unit = {
    val num = new Array[Byte](5)
    val scale = 5

    num(0) = 0x12.toByte
    num(1) = 0x34.toByte
    num(2) = 0x56.toByte
    num(3) = 0x78.toByte
    num(4) = 0x90.toByte
    val bignum = bcdToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("12345.67890"))
    assertArrayEquals(bcdFromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def bcdDec5Pos(): Unit = {
    val num = new Array[Byte](10)
    val scale = 19

    num(0) = 0x00.toByte
    num(1) = 0x00.toByte
    num(2) = 0x00.toByte
    num(3) = 0x00.toByte
    num(4) = 0x00.toByte
    num(5) = 0x12.toByte
    num(6) = 0x34.toByte
    num(7) = 0x56.toByte
    num(8) = 0x78.toByte
    num(9) = 0x90.toByte
    val bignum = bcdToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("0.0000000001234567890"))
    assertArrayEquals(bcdFromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def packedInvalidHighNibble1(): Unit = {
    val num = new Array[Byte](1)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)
    val scale = 0

    num(0) = 0xDC.toByte
    try {
      val bignum = packedToBigDecimal(num, scale, signCodes)
      assertEquals(bignum, new JBigDecimal("1"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid high nibble"))
    }
  }

  @Test def packedInvalidHighNibble2(): Unit = {
    val num = new Array[Byte](6)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)
    val scale = 5

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    num(2) = 0xD5.toByte
    num(3) = 0x67.toByte
    num(4) = 0x89.toByte
    num(5) = 0x0C.toByte
    try {
      val bignum = packedToBigDecimal(num, scale, signCodes)
      assertEquals(bignum, new JBigDecimal("12345.67890"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid high nibble"))
    }
  }

  @Test def packedInvalidLowNibble1(): Unit = {
    val num = new Array[Byte](1)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x10.toByte
    try {
      val bignum = packedToBigInteger(num, signCodes)
      assertEquals(bignum, new JBigInteger("1"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid sign nibble"))
    }
  }

  @Test def packedInvalidLowNibble2(): Unit = {
    val num = new Array[Byte](6)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)
    val scale = 5

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    num(2) = 0x45.toByte
    num(3) = 0x6D.toByte
    num(4) = 0x89.toByte
    num(5) = 0x0C.toByte
    try {
      val bignum = packedToBigDecimal(num, scale, signCodes)
      assertEquals(bignum, new JBigDecimal("12345.67890"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid low nibble"))
    }
  }

  @Test def bcdInvalidHighNibble1(): Unit = {
    val num = new Array[Byte](1)
    val scale = 0

    num(0) = 0xD1.toByte
    try {
      val bignum = bcdToBigDecimal(num, scale)
      assertEquals(bignum, new JBigDecimal("1"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid high nibble"))
    }
  }

  @Test def bcdInvalidHighNibble2(): Unit = {
    val num = new Array[Byte](5)
    val scale = 5

    num(0) = 0x12.toByte
    num(1) = 0x34.toByte
    num(2) = 0xD6.toByte
    num(3) = 0x78.toByte
    num(4) = 0x90.toByte
    try {
      val bignum = bcdToBigDecimal(num, scale)
      assertEquals(bignum, new JBigDecimal("12345.67890"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid high nibble"))
    }
  }

  @Test def bcdInvalidLowNibble1(): Unit = {
    val num = new Array[Byte](1)
    val scale = 0

    num(0) = 0x1C.toByte
    try {
      val bignum = bcdToBigDecimal(num, scale)
      assertEquals(bignum, new JBigDecimal("1"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid low nibble"))
    }
  }

  @Test def bcdInvalidLowNibble2(): Unit = {
    val num = new Array[Byte](5)
    val scale = 5

    num(0) = 0x12.toByte
    num(1) = 0x34.toByte
    num(2) = 0x5D.toByte
    num(3) = 0x78.toByte
    num(4) = 0x90.toByte
    try {
      val bignum = bcdToBigDecimal(num, scale)
      assertEquals(bignum, new JBigDecimal("12345.67890"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid low nibble"))
    }
  }

  @Test def ibm4690InvalidHighNibble1(): Unit = {
    val num = new Array[Byte](1)
    val scale = 0

    num(0) = 0xA1.toByte
    try {
      val bignum = ibm4690ToBigDecimal(num, scale)
      assertEquals(bignum, new JBigDecimal("1"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid high nibble"))
    }
  }

  @Test def ibm4690InvalidHighNibble2(): Unit = {
    val num = new Array[Byte](5)
    val scale = 5

    num(0) = 0x12.toByte
    num(1) = 0x34.toByte
    num(2) = 0xD6.toByte
    num(3) = 0x78.toByte
    num(4) = 0x90.toByte
    try {
      val bignum = ibm4690ToBigDecimal(num, scale)
      assertEquals(bignum, new JBigDecimal("12345.67890"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid high nibble"))
    }
  }

  @Test def ibm4690InvalidLowNibble1(): Unit = {
    val num = new Array[Byte](1)
    val scale = 0

    num(0) = 0x1C.toByte
    try {
      val bignum = ibm4690ToBigDecimal(num, scale)
      assertEquals(bignum, new JBigDecimal("1"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid low nibble"))
    }
  }

  @Test def ibm4690InvalidLowNibble2(): Unit = {
    val num = new Array[Byte](5)
    val scale = 5

    num(0) = 0x12.toByte
    num(1) = 0x34.toByte
    num(2) = 0x5D.toByte
    num(3) = 0x78.toByte
    num(4) = 0x90.toByte
    try {
      val bignum = ibm4690ToBigDecimal(num, scale)
      assertEquals(bignum, new JBigDecimal("12345.67890"))
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid low nibble"))
    }
  }

  @Test def zonedIntAsciiStandardPos1(): Unit = {
    val num = "1"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start)
    assertEquals(result, "1")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiStandardPos2(): Unit = {
    val num = "12"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.End)
    System.out.println("Result: " + result)
    assertEquals(result, "12")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiStandard, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiStandardPos3(): Unit = {
    val num = "123"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.None)
    assertEquals(result, "123")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiStandard, OverpunchLocation.None), num)
  }

  @Test def zonedIntAsciiStandardPos4(): Unit = {
    val num = "1234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start)
    assertEquals(result, "1234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiStandardPos5(): Unit = {
    val num = "000000000001234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start)
    assertEquals(result, "000000000001234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "000000000001234567890")
  }

  @Test def zonedIntAsciiStandardNeg1(): Unit = {
    val num = "q"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start)
    assertEquals(result, "-1")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiStandardNeg2(): Unit = {
    val num = "1r"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.End)
    assertEquals(result, "-12")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiStandard, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiStandardNeg3(): Unit = {
    val num = "q23"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start)
    assertEquals(result, "-123")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiStandardNeg4(): Unit = {
    val num = "123456789p"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.End)
    assertEquals(result, "-1234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiStandard, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiStandardNeg5(): Unit = {
    val num = "p00000000001234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start)
    assertEquals(result, "-000000000001234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiStandardInvalidDigit(): Unit = {
    val num = "z123"
    try {
      val result = zonedToNumber(num, TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start)
      assertEquals(result, "-0123")
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid zoned digit"))
    }
  }

  @Test def zonedIntAsciiTranslatedEBCDICPos1(): Unit = {
    val num = "A"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start)
    assertEquals(result, "1")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiTranslatedEBCDICPos2(): Unit = {
    val num = "1B"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.End)
    assertEquals(result, "12")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiTranslatedEBCDICPos3(): Unit = {
    val num = "123"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None)
    assertEquals(result, "123")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), num)
  }

  @Test def zonedIntAsciiTranslatedEBCDICPos4(): Unit = {
    val num = "A234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start)
    assertEquals(result, "1234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiTranslatedEBCDICPos5(): Unit = {
    val num = "00000000000123456789{"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.End)
    assertEquals(result, "000000000001234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiTranslatedEBCDICNeg1(): Unit = {
    val num = "J"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start)
    assertEquals(result, "-1")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiTranslatedEBCDICNeg2(): Unit = {
    val num = "1K"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.End)
    assertEquals(result, "-12")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiTranslatedEBCDICNeg3(): Unit = {
    val num = "J23"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start)
    assertEquals(result, "-123")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiTranslatedEBCDICNeg4(): Unit = {
    val num = "123456789}"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.End)
    assertEquals(result, "-1234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiTranslatedEBCDICNeg5(): Unit = {
    val num = "}00000000001234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start)
    assertEquals(result, "-000000000001234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiTranslatedEBCDICInvalidDigit(): Unit = {
    val num = "z123"
    try {
      val result = zonedToNumber(num, TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start)
      assertEquals(result, "-0123")
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid zoned digit"))
    }
  }

  @Test def zonedIntAsciiCARealiaModifiedPos1(): Unit = {
    val num = "1"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start)
    assertEquals(result, "1")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiCARealiaModifiedPos2(): Unit = {
    val num = "12"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.End)
    assertEquals(result, "12")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiCARealiaModifiedPos3(): Unit = {
    val num = "123"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.None)
    assertEquals(result, "123")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.None), num)
  }

  @Test def zonedIntAsciiCARealiaModifiedPos4(): Unit = {
    val num = "1234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start)
    assertEquals(result, "1234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiCARealiaModifiedPos5(): Unit = {
    val num = "000000000001234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.End)
    assertEquals(result, "000000000001234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiCARealiaModifiedNeg1(): Unit = {
    val num = "!"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start)
    assertEquals(result, "-1")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiCARealiaModifiedNeg2(): Unit = {
    val num = "1\""
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.End)
    assertEquals(result, "-12")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiCARealiaModifiedNeg3(): Unit = {
    val num = "!23"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start)
    assertEquals(result, "-123")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiCARealiaModifiedNeg4(): Unit = {
    val num = "123456789 "
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.End)
    assertEquals(result, "-1234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiCARealiaModifiedNeg5(): Unit = {
    val num = " 00000000001234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start)
    assertEquals(result, "-000000000001234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiCARealiaModifiedInvalidDigit(): Unit = {
    val num = "z123"
    try {
      val result = zonedToNumber(num, TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start)
      assertEquals(result, "-0123")
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid zoned digit"))
    }
  }

  @Test def zonedIntAsciiTandemModifiedPos1(): Unit = {
    val num = "1"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start)
    assertEquals(result, "1")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiTandemModifiedPos2(): Unit = {
    val num = "12"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.End)
    assertEquals(result, "12")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiTandemModifiedPos3(): Unit = {
    val num = "123"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.None)
    assertEquals(result, "123")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.None), num)
  }

  @Test def zonedIntAsciiTandemModifiedPos4(): Unit = {
    val num = "1234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start)
    assertEquals(result, "1234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiTandemModifiedPos5(): Unit = {
    val num = "000000000001234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.End)
    assertEquals(result, "000000000001234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiTandemModifiedNeg1(): Unit = {
    val num = ""
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start)
    assertEquals(result, "-1")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiTandemModifiedNeg2(): Unit = {
    val num = "1"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.End)
    assertEquals(result, "-12")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiTandemModifiedNeg3(): Unit = {
    val num = "23"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start)
    assertEquals(result, "-123")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiTandemModifiedNeg4(): Unit = {
    val num = "123456789"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.End)
    assertEquals(result, "-1234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.End), num)
  }

  @Test def zonedIntAsciiTandemModifiedNeg5(): Unit = {
    val num = "00000000001234567890"
    val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start)
    assertEquals(result, "-000000000001234567890")
    assertEquals(zonedFromNumber(result, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), num)
  }

  @Test def zonedIntAsciiTandemModifiedInvalidDigit(): Unit = {
    val num = "z123"
    try {
      val result = zonedToNumber(num, TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start)
      assertEquals(result, "-0123")
    } catch {
      case nfe: NumberFormatException => assertTrue(nfe.getMessage().contains("Invalid zoned digit"))
    }
  }

  @Test def zonedIntAsciiStandardAllDigits(): Unit = {
    assertEquals(zonedToNumber("0", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "0")
    assertEquals(zonedFromNumber("0", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "0")
    assertEquals(zonedToNumber("1", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "1")
    assertEquals(zonedFromNumber("1", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "1")
    assertEquals(zonedToNumber("2", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "2")
    assertEquals(zonedFromNumber("2", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "2")
    assertEquals(zonedToNumber("3", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "3")
    assertEquals(zonedFromNumber("3", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "3")
    assertEquals(zonedToNumber("4", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "4")
    assertEquals(zonedFromNumber("4", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "4")
    assertEquals(zonedToNumber("5", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "5")
    assertEquals(zonedFromNumber("5", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "5")
    assertEquals(zonedToNumber("6", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "6")
    assertEquals(zonedFromNumber("6", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "6")
    assertEquals(zonedToNumber("7", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "7")
    assertEquals(zonedFromNumber("7", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "7")
    assertEquals(zonedToNumber("8", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "8")
    assertEquals(zonedFromNumber("8", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "8")
    assertEquals(zonedToNumber("9", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "9")
    assertEquals(zonedFromNumber("9", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "9")
    assertEquals(zonedToNumber("1p", TextZonedSignStyle.AsciiStandard, OverpunchLocation.End), "-10")
    assertEquals(zonedFromNumber("-10", TextZonedSignStyle.AsciiStandard, OverpunchLocation.End), "1p")
    assertEquals(zonedToNumber("q", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "-1")
    assertEquals(zonedFromNumber("-1", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "q")
    assertEquals(zonedToNumber("r", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "-2")
    assertEquals(zonedFromNumber("-2", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "r")
    assertEquals(zonedToNumber("s", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "-3")
    assertEquals(zonedFromNumber("-3", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "s")
    assertEquals(zonedToNumber("t", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "-4")
    assertEquals(zonedFromNumber("-4", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "t")
    assertEquals(zonedToNumber("u", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "-5")
    assertEquals(zonedFromNumber("-5", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "u")
    assertEquals(zonedToNumber("v", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "-6")
    assertEquals(zonedFromNumber("-6", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "v")
    assertEquals(zonedToNumber("w", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "-7")
    assertEquals(zonedFromNumber("-7", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "w")
    assertEquals(zonedToNumber("x", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "-8")
    assertEquals(zonedFromNumber("-8", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "x")
    assertEquals(zonedToNumber("y", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "-9")
    assertEquals(zonedFromNumber("-9", TextZonedSignStyle.AsciiStandard, OverpunchLocation.Start), "y")
  }

  @Test def zonedIntAsciiTranslatedEBCDICAllDigits(): Unit = {
    assertEquals(zonedToNumber("0", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "0")
    assertEquals(zonedFromNumber("0", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "0")
    assertEquals(zonedToNumber("1", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "1")
    assertEquals(zonedFromNumber("1", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "1")
    assertEquals(zonedToNumber("2", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "2")
    assertEquals(zonedFromNumber("2", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "2")
    assertEquals(zonedToNumber("3", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "3")
    assertEquals(zonedFromNumber("3", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "3")
    assertEquals(zonedToNumber("4", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "4")
    assertEquals(zonedFromNumber("4", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "4")
    assertEquals(zonedToNumber("5", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "5")
    assertEquals(zonedFromNumber("5", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "5")
    assertEquals(zonedToNumber("6", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "6")
    assertEquals(zonedFromNumber("6", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "6")
    assertEquals(zonedToNumber("7", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "7")
    assertEquals(zonedFromNumber("7", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "7")
    assertEquals(zonedToNumber("8", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "8")
    assertEquals(zonedFromNumber("8", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "8")
    assertEquals(zonedToNumber("9", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "9")
    assertEquals(zonedFromNumber("9", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.None), "9")
    assertEquals(zonedToNumber("{", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "0")
    assertEquals(zonedFromNumber("0", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "{")
    assertEquals(zonedToNumber("A", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "1")
    assertEquals(zonedFromNumber("1", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "A")
    assertEquals(zonedToNumber("B", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "2")
    assertEquals(zonedFromNumber("2", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "B")
    assertEquals(zonedToNumber("C", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "3")
    assertEquals(zonedFromNumber("3", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "C")
    assertEquals(zonedToNumber("D", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "4")
    assertEquals(zonedFromNumber("4", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "D")
    assertEquals(zonedToNumber("E", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "5")
    assertEquals(zonedFromNumber("5", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "E")
    assertEquals(zonedToNumber("F", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "6")
    assertEquals(zonedFromNumber("6", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "F")
    assertEquals(zonedToNumber("G", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "7")
    assertEquals(zonedFromNumber("7", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "G")
    assertEquals(zonedToNumber("H", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "8")
    assertEquals(zonedFromNumber("8", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "H")
    assertEquals(zonedToNumber("I", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "9")
    assertEquals(zonedFromNumber("9", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "I")
    assertEquals(zonedToNumber("1}", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.End), "-10")
    assertEquals(zonedFromNumber("-10", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.End), "1}")
    assertEquals(zonedToNumber("J", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "-1")
    assertEquals(zonedFromNumber("-1", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "J")
    assertEquals(zonedToNumber("K", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "-2")
    assertEquals(zonedFromNumber("-2", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "K")
    assertEquals(zonedToNumber("L", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "-3")
    assertEquals(zonedFromNumber("-3", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "L")
    assertEquals(zonedToNumber("M", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "-4")
    assertEquals(zonedFromNumber("-4", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "M")
    assertEquals(zonedToNumber("N", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "-5")
    assertEquals(zonedFromNumber("-5", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "N")
    assertEquals(zonedToNumber("O", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "-6")
    assertEquals(zonedFromNumber("-6", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "O")
    assertEquals(zonedToNumber("P", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "-7")
    assertEquals(zonedFromNumber("-7", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "P")
    assertEquals(zonedToNumber("Q", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "-8")
    assertEquals(zonedFromNumber("-8", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "Q")
    assertEquals(zonedToNumber("R", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "-9")
    assertEquals(zonedFromNumber("-9", TextZonedSignStyle.AsciiTranslatedEBCDIC, OverpunchLocation.Start), "R")
  }

  @Test def zonedIntAsciiCARealiaModifiedAllDigits(): Unit = {
    assertEquals(zonedToNumber("0", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "0")
    assertEquals(zonedFromNumber("0", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "0")
    assertEquals(zonedToNumber("1", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "1")
    assertEquals(zonedFromNumber("1", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "1")
    assertEquals(zonedToNumber("2", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "2")
    assertEquals(zonedFromNumber("2", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "2")
    assertEquals(zonedToNumber("3", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "3")
    assertEquals(zonedFromNumber("3", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "3")
    assertEquals(zonedToNumber("4", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "4")
    assertEquals(zonedFromNumber("4", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "4")
    assertEquals(zonedToNumber("5", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "5")
    assertEquals(zonedFromNumber("5", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "5")
    assertEquals(zonedToNumber("6", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "6")
    assertEquals(zonedFromNumber("6", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "6")
    assertEquals(zonedToNumber("7", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "7")
    assertEquals(zonedFromNumber("7", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "7")
    assertEquals(zonedToNumber("8", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "8")
    assertEquals(zonedFromNumber("8", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "8")
    assertEquals(zonedToNumber("9", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "9")
    assertEquals(zonedFromNumber("9", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "9")
    assertEquals(zonedToNumber("1 ", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.End), "-10")
    assertEquals(zonedFromNumber("-10", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.End), "1 ")
    assertEquals(zonedToNumber("!", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "-1")
    assertEquals(zonedFromNumber("-1", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "!")
    assertEquals(zonedToNumber("\"", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "-2")
    assertEquals(zonedFromNumber("-2", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "\"")
    assertEquals(zonedToNumber("#", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "-3")
    assertEquals(zonedFromNumber("-3", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "#")
    assertEquals(zonedToNumber("$", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "-4")
    assertEquals(zonedFromNumber("-4", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "$")
    assertEquals(zonedToNumber("%", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "-5")
    assertEquals(zonedFromNumber("-5", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "%")
    assertEquals(zonedToNumber("&", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "-6")
    assertEquals(zonedFromNumber("-6", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "&")
    assertEquals(zonedToNumber("'", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "-7")
    assertEquals(zonedFromNumber("-7", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "'")
    assertEquals(zonedToNumber("(", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "-8")
    assertEquals(zonedFromNumber("-8", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "(")
    assertEquals(zonedToNumber(")", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), "-9")
    assertEquals(zonedFromNumber("-9", TextZonedSignStyle.AsciiCARealiaModified, OverpunchLocation.Start), ")")
  }

  @Test def zonedIntAsciiTandemModifiedAllDigits(): Unit = {
    assertEquals(zonedToNumber("0", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "0")
    assertEquals(zonedFromNumber("0", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "0")
    assertEquals(zonedToNumber("1", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "1")
    assertEquals(zonedFromNumber("1", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "1")
    assertEquals(zonedToNumber("2", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "2")
    assertEquals(zonedFromNumber("2", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "2")
    assertEquals(zonedToNumber("3", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "3")
    assertEquals(zonedFromNumber("3", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "3")
    assertEquals(zonedToNumber("4", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "4")
    assertEquals(zonedFromNumber("4", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "4")
    assertEquals(zonedToNumber("5", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "5")
    assertEquals(zonedFromNumber("5", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "5")
    assertEquals(zonedToNumber("6", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "6")
    assertEquals(zonedFromNumber("6", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "6")
    assertEquals(zonedToNumber("7", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "7")
    assertEquals(zonedFromNumber("7", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "7")
    assertEquals(zonedToNumber("8", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "8")
    assertEquals(zonedFromNumber("8", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "8")
    assertEquals(zonedToNumber("9", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "9")
    assertEquals(zonedFromNumber("9", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "9")
    assertEquals(zonedToNumber("1", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.End), "-10")
    assertEquals(zonedFromNumber("-10", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.End), "1")
    assertEquals(zonedToNumber("", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "-1")
    assertEquals(zonedFromNumber("-1", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "")
    assertEquals(zonedToNumber("", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "-2")
    assertEquals(zonedFromNumber("-2", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "")
    assertEquals(zonedToNumber("", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "-3")
    assertEquals(zonedFromNumber("-3", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "")
    assertEquals(zonedToNumber("", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "-4")
    assertEquals(zonedFromNumber("-4", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "")
    assertEquals(zonedToNumber("", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "-5")
    assertEquals(zonedFromNumber("-5", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "")
    assertEquals(zonedToNumber("", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "-6")
    assertEquals(zonedFromNumber("-6", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "")
    assertEquals(zonedToNumber("", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "-7")
    assertEquals(zonedFromNumber("-7", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "")
    assertEquals(zonedToNumber("", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "-8")
    assertEquals(zonedFromNumber("-8", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "")
    assertEquals(zonedToNumber("", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "-9")
    assertEquals(zonedFromNumber("-9", TextZonedSignStyle.AsciiTandemModified, OverpunchLocation.Start), "")
  }
}
