package daffodil.schema.annotation.props

import junit.framework.Assert._ 
import org.scalatest.junit.JUnit3Suite

sealed trait MyPropType extends MyProp.Value
object MyProp extends Enum[MyPropType] {
  case object PropVal1 extends MyPropType
  forceConstruction(PropVal1)
  case object PropVal2 extends MyPropType
  forceConstruction(PropVal2)
  def apply(name: String): MyPropType = stringToEnum("myProp", name)
}

class MyPropMixin {
  lazy val myProp = MyProp("propVal1")
}

class RealObject extends MyPropMixin

class TestPropertyRuntime extends JUnit3Suite {
  
  def testConstructed() {
    val myPropUser = new RealObject
    val av = MyProp.allValues
    val pv1 = MyProp.PropVal1
    val pv2 = MyProp.PropVal2
    assertTrue(av.contains(pv1))
    assertTrue(av.contains(pv2))
  }
  
  def testCanCreateProp() {
	  val propVal1 = MyProp("propVal1")
	  assertEquals(MyProp.PropVal1, propVal1)
  }
  
  class HasMixin extends TheExamplePropMixin {
    def getPropertyOption(pname : String) =
      Some("left")
    val detailName ="HasMixin"
  }
  
  def testMixin() {
    val m = new HasMixin
    assertTrue(m.initWasCalled)
    val tep = m.theExampleProp
    val s = m.toString
    assertTrue(m.propToStringWasCalled)
    assertTrue(s.contains("theExampleProp='left'"))
  }
  
}

sealed trait TheExampleProp extends TheExampleProp.Value
object TheExampleProp extends Enum[TheExampleProp] {
  case object Left extends TheExampleProp ; forceConstruction(Left)
  case object Right extends TheExampleProp ; forceConstruction(Right)
  case object Center extends TheExampleProp ; forceConstruction(Center)

  def apply(name: String) : TheExampleProp = stringToEnum("theExampleProp", name)
}

trait TheExamplePropMixin extends PropertyMixin {
    
  /**
   * get property value, or fail trying. Use this if you need
   * the property value.
   */
  lazy val theExampleProp = TheExampleProp(getProperty("theExampleProp"))
    
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
  var propToStringWasCalled : Boolean = false
  def theExamplePropToString() = {
    propToStringWasCalled = true
    optionTheExampleProp match {
      case None => "" // empty string if not present
      case Some(theExampleProp) =>  "dfdl:theExampleProp='" + theExampleProp + "'"
    }
  }
    
  var initWasCalled : Boolean = false
  def init() = {
    initWasCalled = true
    registerToStringFunction(theExamplePropToString)
  }
    
  init() // call at object creation to initialize
}