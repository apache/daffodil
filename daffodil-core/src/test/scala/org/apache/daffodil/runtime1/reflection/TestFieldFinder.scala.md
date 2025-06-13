<!--
  Licensed to the Apache Software Foundation (ASF) under one or more
  contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
  The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-->

```scala
package org.apache.daffodil.reflection

import org.apache.daffodil.util.Maybe
import org.apache.daffodil.exceptions.Assert
import org.junit.Test
import org.junit.Assert._

/**
 * These have to be top-level classes or for some reason
 * the reflection doesn't work.
 */
abstract class C(
  notInterested: String, // wrong type
  warnAbout: List[String], // only used here in constructor
  s: List[String], // yes. Right type, and in use in the printMore method
  var t: List[String]) { // yes. var
  def printMore = println(s)
}

class Bar1(
  val ignoreMe: String, // nope, wrong type.
  dontWarnAboutMe: String, // wrong type
  val x: List[String], // yes, val
  var y: List[String], // yes, var
  warnAboutMe: List[String], // need warning. Right type, but used only as a constructor arg
  w: List[String], // yes, it is used in the printIt method
  warnAboutMeToo: List[String]) // warning. Used nowhere.
  extends C(ignoreMe, warnAboutMe, List("s"), List("t")) { // warnAboutMe is used only as a constructor arg.

  lazy val p: List[String] = List("p")

  val q: List[String] = List("q")

  def printIt = {
    println(w) // w gets used here, so even though it's not var/val it's not supposedly a constructor arg.
    println(dontWarnAboutMe)
  }
}

class TestFieldFinder {

  @Test def testFindAllMembersOfType1() {

    val a = new Bar1("ignoreMe", "dontWarnAboutMe", List("x"), List("y"), List("warnAboutMe"), List("w"), List("warnAboutMeToo"))

    val (values, warns) = FieldFinder.findAllMembersOfType[List[String]](a)
    println(values)
    println(warns)

    val valueStrings = values.map { case List(s: String) => s }
    assertEquals("x y p q t w s", valueStrings.mkString(" "))
    assertEquals(List("value warnAboutMe", "value warnAboutMeToo", "value warnAbout"), warns)
  }

  /**
   * Same thing, but looks for List[AnyRef] instead of the exact type List[String]
   */
  @Test def testFindAllMembersOfTypeWithAnyRef() {

    val a = new Bar1("ignoreMe", "dontWarnAboutMe", List("x"), List("y"), List("warnAboutMe"), List("w"), List("warnAboutMeToo"))

    val (values, warns) = FieldFinder.findAllMembersOfType[List[AnyRef]](a)
    println(values)
    println(warns)

    val valueStrings = values.map { case List(s: String) => s }
    assertEquals("x y p q t w s", valueStrings.mkString(" "))
    assertEquals(List("value warnAboutMe", "value warnAboutMeToo", "value warnAbout"), warns)
  }

  //
  // This test fails. It's near identical to the above,
  // but fails because the classes being examined are nested within a method.
  //
  // This shows that they have to be top-level classes for this particular
  // set of reflective techniques to work.
  //
  // @Test
  def testFindAllMembersOfType2() {

    abstract class C2(val notInterested: List[String]) // not interested because it's not immediately in Bar1.

    class Bar2(val ignoreMe: String, dontWarnAboutMe: String, val x: List[String], var y: List[String], warnAboutMe: List[String], w: List[String], warnAboutMeToo: List[String])
      extends C2(warnAboutMe) { // warnAboutMe is used only as a constructor arg.

      lazy val p: List[String] = List("p")

      val q: List[String] = List("q")

      def printIt = {
        println(w) // w gets used here, so even though it's not var/val it's not supposedly a constructor arg.
        println(dontWarnAboutMe)
      }
    }

    val a = new Bar2("ignoreMe", "dontWarnAboutMe", List("x"), List("y"), List("warnAboutMe"), List("w"), List("warnAboutMeToo"))

    val (values, warns) = FieldFinder.findAllMembersOfType[List[String]](a)
    println(values)
    println(warns)

    val valueStrings = values.map { case List(s: String) => s }
    assertEquals("x y p q w", valueStrings.mkString(" "))
  }
}
```
