/*
 * Copyright (c) 2011-2013, Nate Nystrom
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or
 * other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package passera.test

import scala.language.implicitConversions

import org.junit.Assert._
import org.junit.Test
// import org.scalacheck.ConsoleReporter
import org.scalacheck.Prop._
import org.scalacheck._
import passera.unsigned._

class UnsignedCheck {
  import Arbitrary.arbitrary

  val zero = 0.toUInt
  val one = 1.toUInt

  implicit def doCheck(p: Prop): Boolean = {
    val params = org.scalacheck.Test.Parameters.default
    val passed = org.scalacheck.Test.check(params, p).passed
    passed
  }

  def genUInt: Gen[UInt] = for (n <- arbitrary[Int]) yield UInt(n)
  implicit def arbUInt: Arbitrary[UInt] = Arbitrary(genUInt)

  @Test def testIntToString() = {
    assertTrue(
      forAll { (n: Int) => n >= 0 ==> (n.toUInt.toString == n.toString) }
    )
  }

  val nonNegLong = Gen.choose(0L, 0x00000000ffffffffL)

  @Test def testLongToString() = {
    assertTrue(
      forAll(nonNegLong) { n => n.toUInt.toString == n.toString }
    )
  }

  @Test def testToUIntTotoIntInverses() = {
    assertTrue(
      forAll { (a: Int) => a.toUInt.toInt == a }
    )
  }

  @Test def testToIntTotoUIntInverses() = {
    assertTrue(
      forAll { (a: UInt) => a.toInt.toUInt == a }
    )
  }

  @Test def testToUIntToDouble() = {
    assertTrue(
      forAll { (a: Int) => (a >= 0) ==> (a.toUInt.toDouble == a.toDouble) }
    )
  }

  @Test def testGe0() = {
    assertTrue(
      forAll { (a: UInt) => a >= zero }
    )
  }

  @Test def testAddCommutes() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a + b == b + a }
    )
  }

  @Test def testMulCommutes() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a * b == b * a }
    )
  }

  @Test def testZeroIdentityForAdd() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a + zero == a }
    )
  }

  @Test def testOneIdentityForMul() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a * one == a }
    )
  }

  @Test def testZeroIsZeroMul() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a * zero == zero }
    )
  }

  @Test def testAddAssociates() = {
    assertTrue(
      forAll { (a: UInt, b: UInt, c: UInt) => a + (b + c) == (a + b) + c }
    )
  }

  @Test def testMulDistributesLeft() = {
    assertTrue(
      forAll { (a: UInt, b: UInt, c: UInt) => a * (b + c) == (a * b) + (a * c) }
    )
  }

  @Test def testMulDistributesRight() = {
    assertTrue(
      forAll { (a: UInt, b: UInt, c: UInt) => (a + b) * c == (a * c) + (b * c) }
    )
  }

  @Test def testAddAndSub() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a + (b - a) == b }
    )
  }

  @Test def testAddAndSub2() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => (b - a) + a == b }
    )
  }

  @Test def testDivAndShift() = {
    assertTrue(
      forAll { (a: UInt) => a / 2.toUInt == a >>> 1 }
    )
  }

  @Test def testShift() = {
    assertTrue(
      forAll { (a: UInt) => a >> 1 == a >>> 1 }
    )
  }

  @Test def testZeroFrac() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => (a < b && b != 0) ==> ((a / b) == 0) }
    )
  }

  @Test def testNonzeroFrac() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => (a > b && b != 0) ==> ((a / b) > zero) }
    )
  }

  @Test def testQr() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) =>
        (b != 0) ==> {
          val q = a / b
          val r = a % b
          q * b + r == a
        }
      }
    )
  }

  @Test def testLtAndGt() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a < b == b > a }
    )
  }

  @Test def testLeAndGe() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a <= b == b >= a }
    )
  }

  @Test def testLeAndLtAndEq() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a <= b == (a < b || a == b) }
    )
  }

  @Test def testGeAndGtAndEq() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a >= b == (a > b || a == b) }
    )
  }

  @Test def testLtAndGeAndNe() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a < b == (a <= b && a != b) }
    )
  }

  @Test def testGtAndGeAndNe() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a > b == (a >= b && a != b) }
    )
  }

  @Test def testLeAndNgt() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a <= b == !(a > b) }
    )
  }

  @Test def testGeAndNlt() = {
    assertTrue(
      forAll { (a: UInt, b: UInt) => a >= b == !(a < b) }
    )
  }

  @Test def testLshiftInt() = {
    assertTrue(
      forAll { (a: Int, b: Int) => a.toUInt << (b & 0x1f) == (a << (b & 0x1f)).toUInt }
    )
  }

  @Test def testLshiftLong() = {
    assertTrue(
      forAll { (a: Int, b: Long) => a.toUInt << (b & 0x1f) == (a << (b & 0x1f).toInt).toUInt }
    )
  }

  @Test def testLshiftUInt() = {
    assertTrue(
      forAll { (a: Int, b: Int) => a.toUInt << (b & 0x1f).toUInt == (a << (b & 0x1f)).toUInt }
    )
  }

  @Test def testLshiftULong() = {
    assertTrue(
      forAll { (a: Int, b: Long) =>
        a.toUInt << (b & 0x1f).toULong == (a << (b & 0x1f).toInt).toUInt
      }
    )
  }

  @Test def testRshiftInt() = {
    assertTrue(
      forAll { (a: Int, b: Int) => a.toUInt >> (b & 0x1f) == (a >>> (b & 0x1f)).toUInt }
    )
  }

  @Test def testRshiftLong() = {
    assertTrue(
      forAll { (a: Int, b: Long) => a.toUInt >> (b & 0x1f) == (a >>> (b & 0x1f).toInt).toUInt }
    )
  }

  @Test def testRshiftUInt() = {
    assertTrue(
      forAll { (a: Int, b: Int) => a.toUInt >> (b & 0x1f).toUInt == (a >>> (b & 0x1f)).toUInt }
    )
  }

  @Test def testRshiftULong() = {
    assertTrue(
      forAll { (a: Int, b: Long) =>
        a.toUInt >> (b & 0x1f).toULong == (a >>> (b & 0x1f).toInt).toUInt
      }
    )
  }

  @Test def testZrshiftInt() = {
    assertTrue(
      forAll { (a: Int, b: Int) => a.toUInt >>> (b & 0x1f) == (a >>> (b & 0x1f)).toUInt }
    )
  }

  @Test def testZrshiftLong() = {
    assertTrue(
      forAll { (a: Int, b: Long) =>
        a.toUInt >>> (b & 0x1f).toInt == (a >>> (b & 0x1f).toInt).toUInt
      }
    )
  }

  @Test def testZrshiftUInt() = {
    assertTrue(
      forAll { (a: Int, b: Int) => a.toUInt >>> (b & 0x1f).toUInt == (a >>> (b & 0x1f)).toUInt }
    )
  }

  @Test def testZrshiftULong() = {
    assertTrue(
      forAll { (a: Int, b: Long) =>
        a.toUInt >>> (b & 0x1f).toULong == (a >>> (b & 0x1f).toInt).toUInt
      }
    )
  }

  @Test def testRshiftAndZrshiftEquivalent() = {
    assertTrue(
      forAll { (a: Int, b: Int) => a.toUInt >> (b & 0x1f) == a.toUInt >>> (b & 0x1f) }
    )
  }

}
