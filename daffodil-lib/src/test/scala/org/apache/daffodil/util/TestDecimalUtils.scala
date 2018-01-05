/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
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
import org.apache.daffodil.schema.annotation.props.gen.BinaryNumberCheckPolicy

import java.math.{ BigInteger => JBigInteger, BigDecimal => JBigDecimal }
import org.junit.Test
import org.junit.Assert._

class TestDecimalUtils {

  @Test def packedInt1StrictPos() {
    val num = new Array[Byte](1)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x1C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("1"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt2StrictPos() {
    val num = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x2C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("12"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt3StrictPos() {
    val num = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x12.toByte
    num(1) = 0x3C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("123"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt4StrictPos() {
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

  @Test def packedInt5StrictPos() {
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

  @Test def packedInt1StrictNeg() {
    val num = new Array[Byte](1)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x1D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-1"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt2StrictNeg() {
    val num = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x2D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-12"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt3StrictNeg() {
    val num = new Array[Byte](2)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x12.toByte
    num(1) = 0x3D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-123"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), num)
  }

  @Test def packedInt4StrictNeg() {
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

  @Test def packedInt5StrictNeg() {
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

  @Test def packedInt1LaxPos() {
    val num = new Array[Byte](1)
    val expected = new Array[Byte](1)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x1A.toByte
    expected(0) = 0x1C.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("1"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt2LaxPos() {
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

  @Test def packedInt3LaxPos() {
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

  @Test def packedInt4LaxPos() {
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

  @Test def packedInt5LaxPos() {
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

  @Test def packedInt1LaxNeg() {
    val num = new Array[Byte](1)
    val expected = new Array[Byte](1)
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Lax)

    num(0) = 0x1B.toByte
    expected(0) = 0x1D.toByte
    val bignum = packedToBigInteger(num, signCodes)
    assertEquals(bignum, new JBigInteger("-1"))
    assertArrayEquals(packedFromBigInteger(bignum, num.length*8, signCodes), expected)
  }

  @Test def packedInt2LaxNeg() {
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

  @Test def packedInt3LaxNeg() {
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

  @Test def packedInt4LaxNeg() {
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

  @Test def packedInt5LaxNeg() {
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

  @Test def packedDec1StrictPos() {
    val num = new Array[Byte](1)
    val scale = 0
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x1C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("1"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec2StrictPos() {
    val num = new Array[Byte](2)
    val scale = 1
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x2C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("1.2"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec3StrictPos() {
    val num = new Array[Byte](2)
    val scale = 3
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x12.toByte
    num(1) = 0x3C.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal(".123"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec4StrictPos() {
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

  @Test def packedDec5StrictPos() {
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

  @Test def packedDec1StrictNeg() {
    val num = new Array[Byte](1)
    val scale = 0
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x1D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-1"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec2StrictNeg() {
    val num = new Array[Byte](2)
    val scale = 1
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x01.toByte
    num(1) = 0x2D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-1.2"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec3StrictNeg() {
    val num = new Array[Byte](2)
    val scale = 3
    val signCodes = PackedSignCodes("C D F C", BinaryNumberCheckPolicy.Strict)

    num(0) = 0x12.toByte
    num(1) = 0x3D.toByte
    val bignum = packedToBigDecimal(num, scale, signCodes)
    assertEquals(bignum, new JBigDecimal("-.123"))
    assertArrayEquals(packedFromBigInteger(bignum.unscaledValue, num.length*8, signCodes), num)
  }

  @Test def packedDec4StrictNeg() {
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

  @Test def packedDec5StrictNeg() {
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

  @Test def packedDec1LaxPos() {
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

  @Test def packedDec2LaxPos() {
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

  @Test def packedDec3LaxPos() {
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

  @Test def packedDec4LaxPos() {
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

  @Test def packedDec5LaxPos() {
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

  @Test def packedDec1LaxNeg() {
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

  @Test def packedDec2LaxNeg() {
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

  @Test def packedDec3LaxNeg() {
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

  @Test def packedDec4LaxNeg() {
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

  @Test def packedDec5LaxNeg() {
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

  @Test def ibm4690Int1Pos() {
    val num = new Array[Byte](1)

    num(0) = 0xF1.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("1"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int2Pos() {
    val num = new Array[Byte](1)

    num(0) = 0x12.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("12"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int3Pos() {
    val num = new Array[Byte](2)

    num(0) = 0xF1.toByte
    num(1) = 0x23.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("123"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int4Pos() {
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

  @Test def ibm4690Int5Pos() {
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

  @Test def ibm4690Int1Neg() {
    val num = new Array[Byte](1)

    num(0) = 0xD1.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("-1"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int2Neg() {
    val num = new Array[Byte](2)

    num(0) = 0xFD.toByte
    num(1) = 0x12.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("-12"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int3Neg() {
    val num = new Array[Byte](2)

    num(0) = 0xD1.toByte
    num(1) = 0x23.toByte
    val bignum = ibm4690ToBigInteger(num)
    assertEquals(bignum, new JBigInteger("-123"))
    assertArrayEquals(ibm4690FromBigInteger(bignum, num.length*8), num)
  }

  @Test def ibm4690Int4Neg() {
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

  @Test def ibm4690Int5Neg() {
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

  @Test def ibm4690Dec1Pos() {
    val num = new Array[Byte](1)
    val scale = 0

    num(0) = 0xF1.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("1"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec2Pos() {
    val num = new Array[Byte](1)
    val scale = 1

    num(0) = 0x12.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("1.2"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec3Pos() {
    val num = new Array[Byte](2)
    val scale = 3

    num(0) = 0xF1.toByte
    num(1) = 0x23.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal(".123"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec4Pos() {
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

  @Test def ibm4690Dec5Pos() {
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

  @Test def ibm4690Dec1Neg() {
    val num = new Array[Byte](1)
    val scale = 0

    num(0) = 0xD1.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("-1"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec2Neg() {
    val num = new Array[Byte](2)
    val scale = 1

    num(0) = 0xFD.toByte
    num(1) = 0x12.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("-1.2"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec3Neg() {
    val num = new Array[Byte](2)
    val scale = 3

    num(0) = 0xD1.toByte
    num(1) = 0x23.toByte
    val bignum = ibm4690ToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("-.123"))
    assertArrayEquals(ibm4690FromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def ibm4690Dec4Neg() {
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

  @Test def ibm4690Dec5Neg() {
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

  @Test def bcdInt1Pos() {
    val num = new Array[Byte](1)

    num(0) = 0x01.toByte
    val bignum = bcdToBigInteger(num)
    assertEquals(bignum, new JBigInteger("1"))
    assertArrayEquals(bcdFromBigInteger(bignum, num.length*8), num)
  }

  @Test def bcdInt2Pos() {
    val num = new Array[Byte](1)

    num(0) = 0x12.toByte
    val bignum = bcdToBigInteger(num)
    assertEquals(bignum, new JBigInteger("12"))
    assertArrayEquals(bcdFromBigInteger(bignum, num.length*8), num)
  }

  @Test def bcdInt3Pos() {
    val num = new Array[Byte](2)

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    val bignum = bcdToBigInteger(num)
    assertEquals(bignum, new JBigInteger("123"))
    assertArrayEquals(bcdFromBigInteger(bignum, num.length*8), num)
  }

  @Test def bcdInt4Pos() {
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

  @Test def bcdInt5Pos() {
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

  @Test def bcdDec1Pos() {
    val num = new Array[Byte](1)
    val scale = 0

    num(0) = 0x01.toByte
    val bignum = bcdToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("1"))
    assertArrayEquals(bcdFromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def bcdDec2Pos() {
    val num = new Array[Byte](1)
    val scale = 1

    num(0) = 0x12.toByte
    val bignum = bcdToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal("1.2"))
    assertArrayEquals(bcdFromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def bcdDec3Pos() {
    val num = new Array[Byte](2)
    val scale = 3

    num(0) = 0x01.toByte
    num(1) = 0x23.toByte
    val bignum = bcdToBigDecimal(num, scale)
    assertEquals(bignum, new JBigDecimal(".123"))
    assertArrayEquals(bcdFromBigInteger(bignum.unscaledValue, num.length*8), num)
  }

  @Test def bcdDec4Pos() {
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

  @Test def bcdDec5Pos() {
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

  @Test def packedInvalidHighNibble1() {
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

  @Test def packedInvalidHighNibble2() {
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

  @Test def packedInvalidLowNibble1() {
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

  @Test def packedInvalidLowNibble2() {
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

  @Test def bcdInvalidHighNibble1() {
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

  @Test def bcdInvalidHighNibble2() {
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

  @Test def bcdInvalidLowNibble1() {
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

  @Test def bcdInvalidLowNibble2() {
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

  @Test def ibm4690InvalidHighNibble1() {
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

  @Test def ibm4690InvalidHighNibble2() {
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

  @Test def ibm4690InvalidLowNibble1() {
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

  @Test def ibm4690InvalidLowNibble2() {
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
}
