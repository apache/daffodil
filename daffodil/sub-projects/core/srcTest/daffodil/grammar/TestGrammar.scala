
package daffodil.grammar

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import daffodil.dsom._
import daffodil.exceptions.Assert

class TestGrammar extends JUnit3Suite {

  val fakeSchemaComponent = null
  case class Primitive1(e: SchemaComponent, guard: Boolean = true) extends Terminal(e, guard) {
    def parser: Parser = Assert.notYetImplemented()
    def unparser: Unparser = Assert.notYetImplemented()
  }
  //  case class Primitive2(e: SchemaComponent, guard: Boolean = true) extends Terminal(e, guard)
  //  case class Primitive3(e: SchemaComponent, guard: Boolean = true) extends Terminal(e, guard)

  def testBasicTripleSequential() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent)
    object last extends Primitive1(fakeSchemaComponent)

    lazy val triple = Prod("triple", fakeSchemaComponent, first ~ mid ~ last)

    assertFalse(triple.isEmpty)
    val str = triple.toString
    assertFalse(str.contains("triple"))

    val exp = triple.gram
    assertTrue(exp.name.contains("SeqComp"))
    assertTrue(exp.toString.contains("first"))
    assertTrue(exp.toString.contains("mid"))
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" ~ "))
  }

  def testMiddleSplicedOut() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent, false)
    object last extends Primitive1(fakeSchemaComponent)

    lazy val triple = Prod("triple", fakeSchemaComponent, first ~ mid ~ last)

    assertFalse(triple.isEmpty)

    val exp = triple.gram
    println(exp)
    assertTrue(exp.name.contains("SeqComp"))
    assertTrue(exp.toString.contains("first"))
    assertFalse(exp.toString.contains("mid")) // spliced out.
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" ~ "))

  }

  def testTopProdSplicedOut() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent, false)
    object last extends Primitive1(fakeSchemaComponent)

    lazy val triple = Prod("triple", fakeSchemaComponent, false, first ~ mid ~ last)

    assertTrue(triple.isEmpty)

    val exp = triple.gram
    println(exp)
    assertFalse(exp.name.contains("SeqComp"))
    assertFalse(exp.toString.contains("first"))
    assertFalse(exp.toString.contains("mid")) // spliced out.
    assertFalse(exp.toString.contains("last"))
    assertFalse(exp.toString.contains(" ~ "))

  }

  def testMultipleSpliceOuts() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent, false)
    object last extends Primitive1(fakeSchemaComponent, false)

    lazy val triple = Prod("triple", fakeSchemaComponent, first | (last ~ mid ~ first) | last)

    assertFalse(triple.isEmpty)

    val exp = triple.gram
    println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("first"))
    assertFalse(exp.toString.contains("mid")) // spliced out.
    assertFalse(exp.toString.contains("last")) // spliced out.
    assertTrue(exp.toString.contains(" | "))
    assertFalse(exp.toString.contains(" (")) // no interior parens. There will be around the outside though.

  }

  def testPrecedence1() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent)
    object last extends Primitive1(fakeSchemaComponent)

    lazy val triple = Prod("triple", fakeSchemaComponent, first | mid ~ last)

    assertFalse(triple.isEmpty)

    val exp = triple.gram
    println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("first"))
    assertTrue(exp.toString.contains("mid"))
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" | "))
    assertTrue(exp.toString.contains(" ~ "))
 //   assertTrue(exp.toString.contains(" | (")) // ~ binds tighter

  }

  def testPrecedence2() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent)
    object last extends Primitive1(fakeSchemaComponent)

    lazy val triple = Prod("triple", fakeSchemaComponent, first ~ mid | last)

    assertFalse(triple.isEmpty)

    val exp = triple.gram
    println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("first"))
    assertTrue(exp.toString.contains("mid"))
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" | "))
    assertTrue(exp.toString.contains(" ~ "))
 //   assertTrue(exp.toString.contains(") | ")) // ~ binds tighter

  }

  def testProdsSpliceOut() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent, false)
    object last extends Primitive1(fakeSchemaComponent)

    lazy val prod1 = Prod("prod1", fakeSchemaComponent, first ~ mid | last)
    lazy val prod2 = Prod("prod2", fakeSchemaComponent, false, first ~ mid | last)
    lazy val prod3 = Prod("prod3", fakeSchemaComponent, first ~ mid | last)
    lazy val prod4 = Prod("prod4", fakeSchemaComponent, prod1 | (prod2 ~ prod3))

    assertFalse(prod4.isEmpty)

    val exp = prod4.gram
    println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertFalse(exp.toString.contains("prod1"))
    assertFalse(exp.toString.contains("prod2"))
    assertFalse(exp.toString.contains("prod3"))
    assertTrue(exp.toString.contains(" | "))
    assertFalse(exp.toString.contains(" ~ "))
    assertTrue(exp.toString.contains(" | "))

  }

  def testUnary() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent)
    object last extends Primitive1(fakeSchemaComponent)

    lazy val prod1 = Prod("prod1", fakeSchemaComponent, first ~ RepExactlyN(1, mid) | last)

    assertFalse(prod1.isEmpty)

    val exp = prod1.gram
    println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("RepExactlyN"))

  }

}
