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

package org.apache.daffodil.grammar

import junit.framework.Assert._
import org.apache.daffodil.Implicits._; object INoWarnG1 { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.dsom._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.parsers.Parser
import org.junit.Test
import org.apache.daffodil.util.Fakes

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
    // val str = triple.toString
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
