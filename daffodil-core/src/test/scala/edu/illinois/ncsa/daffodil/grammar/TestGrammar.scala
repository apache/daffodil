

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

package edu.illinois.ncsa.daffodil.grammar

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors._
import org.junit.Test
import edu.illinois.ncsa.daffodil.util.Fakes

class TestGrammar extends GrammarMixin {

  val fakeTerm = new GlobalElementDeclFactory(<element name="foo" type="xs:int"/>, Fakes.fakeSD).forRoot()

  final override protected def grammarContext = fakeTerm

  case class Primitive1(e: Term, guard: Boolean = true) extends Terminal(e, guard) with HasNoUnparser {
    def parser: Parser = Assert.notYetImplemented()
  }
  //  case class Primitive2(e: SchemaComponent, guard: Boolean = true) extends Terminal(e, guard)
  //  case class Primitive3(e: SchemaComponent, guard: Boolean = true) extends Terminal(e, guard)

  @Test def testBasicTripleSequential() {

    object first extends Primitive1(fakeTerm)
    object mid extends Primitive1(fakeTerm)
    object last extends Primitive1(fakeTerm)

    lazy val triple = prod("triple") { first ~ mid ~ last }

    assertFalse(triple.isEmpty)
    val str = triple.toString
    // assertFalse(str.contains("triple"))

    val exp = triple.gram
    val n = exp.name
    val s = exp.toString
    assertTrue(n.contains("SeqComp"))
    assertTrue(s.contains("first"))
    assertTrue(s.contains("mid"))
    assertTrue(s.contains("last"))
    assertTrue(s.contains(" ~ "))
  }

  @Test def testMiddleSplicedOut() {

    object first extends Primitive1(fakeTerm)
    object mid extends Primitive1(fakeTerm, false)
    object last extends Primitive1(fakeTerm)

    lazy val triple = prod("triple") { first ~ mid ~ last }

    assertFalse(triple.isEmpty)

    val exp = triple.gram
    // println(exp)
    assertTrue(exp.name.contains("SeqComp"))
    assertTrue(exp.toString.contains("first"))
    assertFalse(exp.toString.contains("mid")) // spliced out.
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" ~ "))

  }

  @Test def testTopProdSplicedOut() {

    object first extends Primitive1(fakeTerm)
    object mid extends Primitive1(fakeTerm, false)
    object last extends Primitive1(fakeTerm)

    lazy val triple = prod("triple", false) { first ~ mid ~ last }

    assertTrue(triple.isEmpty)

    val exp = triple.gram
    // println(exp)
    assertFalse(exp.name.contains("SeqComp"))
    assertFalse(exp.toString.contains("first"))
    assertFalse(exp.toString.contains("mid")) // spliced out.
    assertFalse(exp.toString.contains("last"))
    assertFalse(exp.toString.contains(" ~ "))

  }

  @Test def testMultipleSpliceOuts() {

    object first extends Primitive1(fakeTerm)
    object mid extends Primitive1(fakeTerm, false)
    object last extends Primitive1(fakeTerm, false)

    lazy val triple = prod("triple") { first | (last ~ mid ~ first) | last }

    assertFalse(triple.isEmpty)

    val exp = triple.gram
    // println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("first"))
    assertFalse(exp.toString.contains("mid")) // spliced out.
    assertFalse(exp.toString.contains("last")) // spliced out.
    assertTrue(exp.toString.contains(" | "))
    assertFalse(exp.toString.contains(" (")) // no interior parens. There will be around the outside though.

  }

  @Test def testPrecedence1() {

    object first extends Primitive1(fakeTerm)
    object mid extends Primitive1(fakeTerm)
    object last extends Primitive1(fakeTerm)

    lazy val triple = prod("triple") { first | mid ~ last }

    assertFalse(triple.isEmpty)

    val exp = triple.gram
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("first"))
    assertTrue(exp.toString.contains("mid"))
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" | "))
    assertTrue(exp.toString.contains(" ~ "))
    //   assertTrue(exp.toString.contains(" | (")) // ~ binds tighter

  }

  @Test def testPrecedence2() {

    object first extends Primitive1(fakeTerm)
    object mid extends Primitive1(fakeTerm)
    object last extends Primitive1(fakeTerm)

    lazy val triple = prod("triple") { first ~ mid | last }

    assertFalse(triple.isEmpty)

    val exp = triple.gram
    // println(exp)
    assertTrue(exp.name.contains("AltComp"))
    assertTrue(exp.toString.contains("first"))
    assertTrue(exp.toString.contains("mid"))
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" | "))
    assertTrue(exp.toString.contains(" ~ "))
    //   assertTrue(exp.toString.contains(") | ")) // ~ binds tighter

  }

  @Test def testProdsSpliceOut() {

    object first extends Primitive1(fakeTerm)
    object mid extends Primitive1(fakeTerm, false)
    object last extends Primitive1(fakeTerm)

    lazy val prod1 = prod("prod1") { first ~ mid | last }
    lazy val prod2 = prod("prod2", false) { first ~ mid | last }
    lazy val prod3 = prod("prod3") { first ~ mid | last }
    lazy val prod4 = prod("prod4") { prod1 | (prod2 ~ prod3) }

    assertFalse(prod4.isEmpty)

    val exp = prod4.gram
    assertTrue(exp.name.contains("AltComp"))
    // assertTrue(exp.toString.contains("prod1"))
    assertFalse(exp.toString.contains("prod2"))
    assertFalse(exp.toString.contains("prod3"))
    assertTrue(exp.toString.contains(" | "))
    assertFalse(exp.toString.contains(" ~ "))
    assertTrue(exp.toString.contains(" | "))

  }

}
