/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

//
// Please leave this code here for now.
// It's unused, but documents how to use reflection, many of its foibles,
// and in other sitautions we may want to use these techniques.
//

//package org.apache.daffodil.reflection
//
//import org.apache.daffodil.util.Maybe
//import org.apache.daffodil.exceptions.Assert
//import org.junit.Test
//import org.junit.Assert._
//
///**
// * These have to be top-level classes or for some reason
// * the reflection doesn't work.
// */
//abstract class C(
//  notInterested: String, // wrong type
//  warnAbout: List[String], // only used here in constructor
//  s: List[String], // yes. Right type, and in use in the printMore method
//  var t: List[String]) { // yes. var
//  def printMore = println(s)
//}
//
//class Bar1(
//  val ignoreMe: String, // nope, wrong type.
//  dontWarnAboutMe: String, // wrong type
//  val x: List[String], // yes, val
//  var y: List[String], // yes, var
//  warnAboutMe: List[String], // need warning. Right type, but used only as a constructor arg
//  w: List[String], // yes, it is used in the printIt method
//  warnAboutMeToo: List[String]) // warning. Used nowhere.
//  extends C(ignoreMe, warnAboutMe, List("s"), List("t")) { // warnAboutMe is used only as a constructor arg.
//
//  lazy val p: List[String] = List("p")
//
//  val q: List[String] = List("q")
//
//  def printIt = {
//    println(w) // w gets used here, so even though it's not var/val it's not supposedly a constructor arg.
//    println(dontWarnAboutMe)
//  }
//}
//
//class TestFieldFinder {
//
//  @Test def testFindAllMembersOfType1() {
//
//    val a = new Bar1("ignoreMe", "dontWarnAboutMe", List("x"), List("y"), List("warnAboutMe"), List("w"), List("warnAboutMeToo"))
//
//    val (values, warns) = FieldFinder.findAllMembersOfType[List[String]](a)
//    println(values)
//    println(warns)
//
//    val valueStrings = values.map { case List(s: String) => s }
//    assertEquals("x y p q t w s", valueStrings.mkString(" "))
//    assertEquals(List("value warnAboutMe", "value warnAboutMeToo", "value warnAbout"), warns)
//  }
//
//  /**
//   * Same thing, but looks for List[AnyRef] instead of the exact type List[String]
//   */
//  @Test def testFindAllMembersOfTypeWithAnyRef() {
//
//    val a = new Bar1("ignoreMe", "dontWarnAboutMe", List("x"), List("y"), List("warnAboutMe"), List("w"), List("warnAboutMeToo"))
//
//    val (values, warns) = FieldFinder.findAllMembersOfType[List[AnyRef]](a)
//    println(values)
//    println(warns)
//
//    val valueStrings = values.map { case List(s: String) => s }
//    assertEquals("x y p q t w s", valueStrings.mkString(" "))
//    assertEquals(List("value warnAboutMe", "value warnAboutMeToo", "value warnAbout"), warns)
//  }
//
//  //
//  // This test fails. It's near identical to the above,
//  // but fails because the classes being examined are nested within a method.
//  //
//  // This shows that they have to be top-level classes for this particular
//  // set of reflective techniques to work.
//  //
//  // @Test
//  def testFindAllMembersOfType2() {
//
//    abstract class C2(val notInterested: List[String]) // not interested because it's not immediately in Bar1.
//
//    class Bar2(val ignoreMe: String, dontWarnAboutMe: String, val x: List[String], var y: List[String], warnAboutMe: List[String], w: List[String], warnAboutMeToo: List[String])
//      extends C2(warnAboutMe) { // warnAboutMe is used only as a constructor arg.
//
//      lazy val p: List[String] = List("p")
//
//      val q: List[String] = List("q")
//
//      def printIt = {
//        println(w) // w gets used here, so even though it's not var/val it's not supposedly a constructor arg.
//        println(dontWarnAboutMe)
//      }
//    }
//
//    val a = new Bar2("ignoreMe", "dontWarnAboutMe", List("x"), List("y"), List("warnAboutMe"), List("w"), List("warnAboutMeToo"))
//
//    val (values, warns) = FieldFinder.findAllMembersOfType[List[String]](a)
//    println(values)
//    println(warns)
//
//    val valueStrings = values.map { case List(s: String) => s }
//    assertEquals("x y p q w", valueStrings.mkString(" "))
//  }
//}
