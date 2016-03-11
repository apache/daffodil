//
// Please leave this code here for now.
// It's unused, but documents how to use reflection, many of its foibles,
// and in other sitautions we may want to use these techniques.
//

//package edu.illinois.ncsa.daffodil.reflection
//
//import edu.illinois.ncsa.daffodil.util.Maybe
//import edu.illinois.ncsa.daffodil.exceptions.Assert
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
