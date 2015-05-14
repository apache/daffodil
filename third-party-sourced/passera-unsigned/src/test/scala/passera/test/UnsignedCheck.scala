package passera.test

import org.scalacheck._
import org.scalacheck.Prop._
import passera.unsigned._

object UnsignedCheck extends Properties("Unsigned") {
  import Gen._
  import Arbitrary.arbitrary

  val zero = 0.toUInt
  val one = 1.toUInt

  def genUInt: Gen[UInt] = for (n <- arbitrary[Int]) yield UInt(n)
  implicit def arbUInt: Arbitrary[UInt] = Arbitrary(genUInt)

  property("int-toString") =
    forAll { n: Int => n >= 0 ==> (n.toUInt.toString == n.toString) }

  val nonNegLong = Gen.choose(0L, 0x00000000ffffffffL)
  property("long-toString") =
    forAll(nonNegLong){ n => n.toUInt.toString == n.toString }

  property("toUInt->toInt inverses") =
    forAll { (a: Int) => a.toUInt.toInt == a }
  property("toInt->toUInt inverses") =
    forAll { (a: UInt) => a.toInt.toUInt == a }
  property("toUInt-toDouble") =
    forAll { (a: Int) => (a >= 0) ==> (a.toUInt.toDouble == a.toDouble) }
  property(">= 0") =
    forAll { (a: UInt) => a >= zero }
  property("+ commutes") =
    forAll { (a: UInt, b: UInt) => a + b == b + a }
  property("* commutes") =
    forAll { (a: UInt, b: UInt) => a * b == b * a }
  property("zero identity for +") =
    forAll { (a: UInt, b: UInt) => a + zero == a }
  property("one identity for *") =
    forAll { (a: UInt, b: UInt) => a * one == a }
  property("zero is zero *") =
    forAll { (a: UInt, b: UInt) => a * zero == zero }
  property("+ associates") =
    forAll { (a: UInt, b: UInt, c: UInt) => a + (b + c) == (a + b) + c }
  property("* distributes left") =
    forAll { (a: UInt, b: UInt, c: UInt) => a * (b + c) == (a*b) + (a*c) }
  property("* distributes right") =
    forAll { (a: UInt, b: UInt, c: UInt) => (a + b) * c == (a*c) + (b*c) }

  property("+ and -") =
    forAll { (a: UInt, b: UInt) => a + (b - a) == b }
  property("+ and - (2)") =
    forAll { (a: UInt, b: UInt) => (b - a) + a == b }

  property("/ and shift") =
    forAll { (a: UInt) => a / 2.toUInt == a >>> 1 }
  property("shift") =
    forAll { (a: UInt) => a >> 1 == a >>> 1 }
  property("zero frac") =
    forAll { (a: UInt, b: UInt) => (a < b && b != 0) ==> ((a / b) == 0) }
  property("nonzero frac") =
    forAll { (a: UInt, b: UInt) => (a > b && b != 0) ==> ((a / b) > zero) }
  property("qr") =
    forAll { (a: UInt, b: UInt) => (b != 0) ==> {
      val q = a / b
      val r = a % b
      q * b + r == a
    } }

  property("< and >") =
    forAll { (a: UInt, b: UInt) => a < b == b > a }
  property("<= and >=") =
    forAll { (a: UInt, b: UInt) => a <= b == b >= a }
  property("<= and < and ==") =
    forAll { (a: UInt, b: UInt) => a <= b == (a < b || a == b) }
  property(">= and > and ==") =
    forAll { (a: UInt, b: UInt) => a >= b == (a > b || a == b) }
  property("< and >= and !=") =
    forAll { (a: UInt, b: UInt) => a < b == (a <= b && a != b) }
  property("> and >= and !=") =
    forAll { (a: UInt, b: UInt) => a > b == (a >= b && a != b) }
  property("<= and ! >") =
    forAll { (a: UInt, b: UInt) => a <= b == ! (a > b) }
  property(">= and ! <") =
    forAll { (a: UInt, b: UInt) => a >= b == ! (a < b) }

  property("<< by Int") =
    forAll { (a: Int, b: Int) => a.toUInt << (b & 0x1f) == (a << (b & 0x1f)).toUInt }
  property("<< by Long") =
    forAll { (a: Int, b: Long) => a.toUInt << (b & 0x1f) == (a << (b & 0x1f)).toUInt }
  property("<< by UInt") =
    forAll { (a: Int, b: Int) => a.toUInt << (b & 0x1f).toUInt == (a << (b & 0x1f)).toUInt }
  property("<< by ULong") =
    forAll { (a: Int, b: Long) => a.toUInt << (b & 0x1f).toULong == (a << (b & 0x1f)).toUInt }

  property(">> by Int") =
    forAll { (a: Int, b: Int) => a.toUInt >> (b & 0x1f) == (a >>> (b & 0x1f)).toUInt }
  property(">> by Long") =
    forAll { (a: Int, b: Long) => a.toUInt >> (b & 0x1f) == (a >>> (b & 0x1f)).toUInt }
  property(">> by UInt") =
    forAll { (a: Int, b: Int) => a.toUInt >> (b & 0x1f).toUInt == (a >>> (b & 0x1f)).toUInt }
  property(">> by ULong") =
    forAll { (a: Int, b: Long) => a.toUInt >> (b & 0x1f).toULong == (a >>> (b & 0x1f)).toUInt }

  property(">>> by Int") =
    forAll { (a: Int, b: Int) => a.toUInt >>> (b & 0x1f) == (a >>> (b & 0x1f)).toUInt }
  property(">>> by Long") =
    forAll { (a: Int, b: Long) => a.toUInt >>> (b & 0x1f) == (a >>> (b & 0x1f)).toUInt }
  property(">>> by UInt") =
    forAll { (a: Int, b: Int) => a.toUInt >>> (b & 0x1f).toUInt == (a >>> (b & 0x1f)).toUInt }
  property(">>> by ULong") =
    forAll { (a: Int, b: Long) => a.toUInt >>> (b & 0x1f).toULong == (a >>> (b & 0x1f)).toUInt }

  property(">> and >>> equivalent") =
    forAll { (a: Int, b: Int) => a.toUInt >> (b & 0x1f) == a.toUInt >>> (b & 0x1f) }

}
