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

package org.apache.daffodil.core.schema.annotation.props

import org.apache.daffodil.core.dsom.NestingLexicalMixin
import org.apache.daffodil.core.dsom.SchemaComponentImpl
import org.apache.daffodil.core.util.Fakes
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.schema.annotation.props._

import org.junit.Assert._
import org.junit.Test

sealed trait MyPropType extends EnumValue
object MyProp
  extends Enum[MyPropType] // with ThrowsSDE
  {
  lazy val context = Fakes.fakeElem
  //  lazy val schemaComponent = context
  case object PropVal1 extends MyPropType
  case object PropVal2 extends MyPropType
  override lazy val values = Array(PropVal1, PropVal2)

  def apply(name: String): MyPropType = apply(name, context)
  def apply(name: String, context: ThrowsSDE) = stringToEnum("myProp", name, context)
}

class MyPropMixin {
  lazy val myProp = MyProp("propVal1")
}

class RealObject extends MyPropMixin

class TestPropertyRuntime {

  @Test
  def testConstructed(): Unit = {
    // val myPropUser = new RealObject
    val av = MyProp.values
    val pv1 = MyProp.PropVal1
    val pv2 = MyProp.PropVal2
    assertTrue(av.contains(pv1))
    assertTrue(av.contains(pv2))
  }

  @Test
  def testCanCreateProp(): Unit = {
    val propVal1 = MyProp("propVal1")
    assertEquals(MyProp.PropVal1, propVal1)
  }

  @Test
  def testMixin(): Unit = {
    val m = new HasMixin
    assertTrue(m.initWasCalled)
    m.theExampleProp
    val s = m.verboseToString
    assertTrue(m.propToStringWasCalled)
    assertTrue(s.contains("theExampleProp='left'"))
  }

}

class HasMixin
  extends SchemaComponentImpl(<foo/>, None)
  with TheExamplePropMixin
  with NestingLexicalMixin {

  override def lookupProperty(pname: String) =
    Found("left", this, pname, false)
  lazy val fileName = "file:dummy"
  lazy val properties: PropMap = Map.empty
}

sealed trait TheExampleProp extends EnumValue
object TheExampleProp extends Enum[TheExampleProp] {
  case object Left extends TheExampleProp
  case object Right extends TheExampleProp
  case object Center extends TheExampleProp
  override lazy val values = Array(Left, Right, Center)

  def apply(name: String, self: ThrowsSDE): TheExampleProp =
    stringToEnum("theExampleProp", name, self)
}

trait TheExamplePropMixin extends PropertyMixin { self: HasMixin =>

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
    registerToStringFunction(() => theExamplePropToString())
  }

  init() // call at object creation to initialize
}
