package daffodil.grammar

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import daffodil.dsom._

class TestGrammar extends JUnit3Suite {

  val fakeSchemaComponent = null
  case class Primitive1(e: SchemaComponent, guard: Boolean = true) extends Terminal(e, guard)
  case class Primitive2(e: SchemaComponent, guard: Boolean = true) extends Terminal(e, guard)
  case class Primitive3(e: SchemaComponent, guard: Boolean = true) extends Terminal(e, guard)

  def testBasicTripleSequential() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent)
    object last extends Primitive1(fakeSchemaComponent)

    object triple extends Production(fakeSchemaComponent, first ~ mid ~ last)

    assertFalse(triple.isEmpty)
    val str = triple.toString
    assertTrue(str.contains("triple"))

    val exp = triple.expr
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

    object triple extends Production(fakeSchemaComponent, first ~ mid ~ last)

    assertFalse(triple.isEmpty)

    val exp = triple.expr
    println(exp)
    assertTrue(exp.name.contains("SeqComp"))
    assertTrue(exp.toString.contains("first"))
    assertFalse(exp.toString.contains("mid")) // spliced out.
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" ~ "))

  }

  def testTopProductionSplicedOut() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent, false)
    object last extends Primitive1(fakeSchemaComponent)

    object triple extends Production(fakeSchemaComponent, false, first ~ mid ~ last)

    assertTrue(triple.isEmpty)

    val exp = triple.expr
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

    object triple extends Production(fakeSchemaComponent, first | (last ~ mid ~ first) | last)

    assertFalse(triple.isEmpty)

    val exp = triple.expr
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

    object triple extends Production(fakeSchemaComponent, first | mid ~ last)

    assertFalse(triple.isEmpty)

    val exp = triple.expr
    println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("first"))
    assertTrue(exp.toString.contains("mid"))
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" | "))
    assertTrue(exp.toString.contains(" ~ "))
    assertTrue(exp.toString.contains(" | (")) // ~ binds tighter

  }

  def testPrecedence2() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent)
    object last extends Primitive1(fakeSchemaComponent)

    object triple extends Production(fakeSchemaComponent, first ~ mid | last)

    assertFalse(triple.isEmpty)

    val exp = triple.expr
    println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("first"))
    assertTrue(exp.toString.contains("mid"))
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" | "))
    assertTrue(exp.toString.contains(" ~ "))
    assertTrue(exp.toString.contains(") | ")) // ~ binds tighter

  }

  def testProductionsSpliceOut() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent)
    object last extends Primitive1(fakeSchemaComponent)

    object prod1 extends Production(fakeSchemaComponent, first ~ mid | last)
    object prod2 extends Production(fakeSchemaComponent, false, first ~ mid | last)
    object prod3 extends Production(fakeSchemaComponent, first ~ mid | last)
    object prod4 extends Production(fakeSchemaComponent, prod1 | prod2 ~ prod3)

    assertFalse(prod4.isEmpty)

    val exp = prod4.expr
    println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("prod1"))
    assertFalse(exp.toString.contains("prod2"))
    assertTrue(exp.toString.contains("prod3"))
    assertTrue(exp.toString.contains(" | "))
    assertFalse(exp.toString.contains(" ~ "))
    assertTrue(exp.toString.contains(" | "))

  }
  
   def testUnary() {

    object first extends Primitive1(fakeSchemaComponent)
    object mid extends Primitive1(fakeSchemaComponent)
    object last extends Primitive1(fakeSchemaComponent)

    object prod1 extends Production(fakeSchemaComponent, first ~ RepExactlyN(mid) | last)
    
    assertFalse(prod1.isEmpty)

    val exp = prod1.expr
    println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("RepExactlyN"))

  }

}