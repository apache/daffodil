/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.schema.annotation.props

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import org.junit.Test
import edu.illinois.ncsa.daffodil.dsom.Found
import edu.illinois.ncsa.daffodil.dsom.LookupLocation
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGHost
import edu.illinois.ncsa.daffodil.dsom.SchemaComponentBase
import edu.illinois.ncsa.daffodil.util.Fakes
import edu.illinois.ncsa.daffodil.api.DaffodilTunableParameters

sealed trait MyPropType extends MyProp.Value
object MyProp extends Enum[MyPropType] // with ThrowsSDE
{
  lazy val context = Fakes.fakeElem
  //  lazy val schemaComponent = context
  case object PropVal1 extends MyPropType
  forceConstruction(PropVal1)
  case object PropVal2 extends MyPropType
  forceConstruction(PropVal2)
  def apply(name: String): MyPropType = apply(name, context)
  def apply(name: String, context: ThrowsSDE) = stringToEnum("myProp", name, context)
}

class MyPropMixin {
  lazy val myProp = MyProp("propVal1")
}

class RealObject extends MyPropMixin

class TestPropertyRuntime {

  @Test
  def testConstructed() {
    // val myPropUser = new RealObject
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

  class HasMixin extends {
    override val xml = <foo/>
  } with SchemaComponentBase(<dummy/>, null)
    with TheExamplePropMixin
    with LookupLocation {

    final override def enclosingComponent: Option[SchemaComponentBase] = None

    // Members declared in edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
    def columnDescription: String = ???
    def fileDescription: String = ???
    def lineDescription: String = ???
    def locationDescription: String = ???
    // Members declared in edu.illinois.ncsa.daffodil.dsom.ResolvesQNames
    def namespaces: scala.xml.NamespaceBinding = scala.xml.TopScope
    // Members declared in edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
    def schemaFileLocation: edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation = ???

    def findPropertyOption(pname: String) =
      Found("left", this, pname, false)
    lazy val fileName = "file:dummy"
    lazy val properties: PropMap = Map.empty

    def columnAttribute: Option[String] = ???
    def fileAttribute: Option[String] = ???
    def lineAttribute: Option[String] = ???
  }

  @Test
  def testMixin() {
    val m = new HasMixin
    assertTrue(m.initWasCalled)
    m.theExampleProp
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

trait TheExamplePropMixin
  extends PropertyMixin with ThrowsSDE { self: OOLAGHost =>

  def SDE(id: String, args: Any*): Nothing = {
    throw new Exception(id.toString + args)
  }
  def SDEButContinue(id: String, args: Any*) = SDW(id, args: _*)
  def SDW(id: String, args: Any*): Unit = {
    System.err.println(new Exception(id.toString + args))
  }

  def SDW(warnID: DaffodilTunableParameters.WarnID, id: String, args: Any*): Unit = {
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
