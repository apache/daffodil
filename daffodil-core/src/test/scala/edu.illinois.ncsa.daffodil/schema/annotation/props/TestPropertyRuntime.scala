package daffodil.schema.annotation.props

import junit.framework.Assert._
import daffodil.exceptions.ThrowsSDE
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import daffodil.dsom.ImplementsThrowsSDE
import daffodil.dsom.FindPropertyMixin
import daffodil.dsom.Found
import daffodil.dsom.LookupLocation
import java.net.URL

sealed trait MyPropType extends MyProp.Value
object MyProp extends Enum[MyPropType]
  with ImplementsThrowsSDE {
  lazy val context = daffodil.dsom.Fakes.fakeElem
  lazy val schemaComponent = context
  lazy val diagnosticChildren: DiagnosticsList = Nil
  lazy val path = prettyName
  lazy val prettyName = "MyPropType"

  case object PropVal1 extends MyPropType
  forceConstruction(PropVal1)
  case object PropVal2 extends MyPropType
  forceConstruction(PropVal2)
  def apply(name: String): MyPropType = stringToEnum("myProp", name, this)

}

class MyPropMixin {
  lazy val myProp = MyProp("propVal1")
}

class RealObject extends MyPropMixin

class TestPropertyRuntime extends JUnitSuite {

  @Test
  def testConstructed() {
    val myPropUser = new RealObject
    val av = MyProp.allValues
    val pv1 = MyProp.PropVal1
    val pv2 = MyProp.PropVal2
    assertTrue(av.contains(pv1))
    assertTrue(av.contains(pv2))
  }

  @Test
  def testCanCreateProp() {
    val propVal1 = MyProp("propVal1")
    assertEquals(MyProp.PropVal1, propVal1)
  }

  class HasMixin extends TheExamplePropMixin with LookupLocation {
    def findPropertyOption(pname: String) =
      Found("left", this)
    lazy val context = this
    lazy val xml = <foo/>
    lazy val fileName = new URL("file:dummy")
    lazy val properties: PropMap = Map.empty
  }

  @Test
  def testMixin() {
    val m = new HasMixin
    assertTrue(m.initWasCalled)
    val tep = m.theExampleProp
    val s = m.verboseToString
    assertTrue(m.propToStringWasCalled)
    assertTrue(s.contains("theExampleProp='left'"))
  }

}

sealed trait TheExampleProp extends TheExampleProp.Value
object TheExampleProp extends Enum[TheExampleProp] {
  case object Left extends TheExampleProp; forceConstruction(Left)
  case object Right extends TheExampleProp; forceConstruction(Right)
  case object Center extends TheExampleProp; forceConstruction(Center)

  def apply(name: String, self: ThrowsSDE): TheExampleProp = stringToEnum("theExampleProp", name, self)
}

trait TheExamplePropMixin extends PropertyMixin with ThrowsSDE {

  def SDE(id: String, args: Any*): Nothing = {
    throw new Exception(id.toString + args)
  }
  def SDEButContinue(id: String, args: Any*) = SDW(id, args: _*)
  def SDW(id: String, args: Any*): Unit = {
    System.err.println(new Exception(id.toString + args))
  }

  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val theExampleProp = TheExampleProp(getProperty("theExampleProp"), this)

  /**
   * get Some(property value) or None if not defined in scope.
   *
   * Mostly do not use this. Most code shouldn't need to test for
   * property existence. Just insist on the property you need by
   * calling getProperty("theExampleProp")
   */
  lazy val optionTheExampleProp = getPropertyOption("theExampleProp")

  /**
   * This will print the property value if the property has any value
   * in scope. This is mostly for debugging purposes.
   */
  var propToStringWasCalled: Boolean = false
  def theExamplePropToString() = {
    propToStringWasCalled = true
    optionTheExampleProp match {
      case None => "" // empty string if not present
      case Some(theExampleProp) => "dfdl:theExampleProp='" + theExampleProp + "'"
    }
  }

  var initWasCalled: Boolean = false
  def init() = {
    initWasCalled = true
    registerToStringFunction(theExamplePropToString)
  }

  init() // call at object creation to initialize
}
