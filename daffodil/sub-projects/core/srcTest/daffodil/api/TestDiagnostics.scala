package daffodil.api

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import daffodil.dsom.DiagnosticsImpl

/**
 * Illustrates how to pair a value with diagnostic information.
 */
private[api] class MyInt(i : Option[Int], diagnostics : Seq[Diagnostic]) 
  extends ValueWithDiagnostics[Int, MyInt](i, diagnostics) 
  with DiagnosticsImpl {
  // must write a newInstance routine callable from the base class
  def newInstance(i : Option[Int], d : Seq[Diagnostic]) = new MyInt(i, d)
}


/**
 * Companion class for implicit conversion (when desirable)
 */
object MyInt {
  def apply(i : Int) = new MyInt(Some(i), Seq.empty)
  implicit def convertMyIntToInt(myInt : MyInt) : Int = myInt.v.get
}


class TestDiagnostics extends JUnit3Suite {

	def testValueWithDiagnostics() {
	  val dint = MyInt(5)
	  val res = 10 + dint
	  assertEquals(15, res)
	}
}