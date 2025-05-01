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

package org.apache.daffodil.core.grammar

import org.apache.daffodil.lib.Implicits._

import org.junit.Assert._; object INoWarnG1 { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.core.dsom._
import org.apache.daffodil.core.util.Fakes
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.processors.parsers.Parser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser

import org.junit.Test

class TestGrammar extends GrammarMixin {

  val fakeDecl =
    GlobalElementDecl(
      <xs:element name="foo" type="xs:int" xmlns:xs="http://www.w3.org/2001/XMLSchema"/>,
      Fakes.fakeSD
    )
  val fakeTerm = fakeDecl.asRoot

  final override protected def grammarContext = fakeTerm

  case class Primitive1(e: Term, guard: Boolean = true) extends Terminal(e, guard) {
    def parser: Parser = Assert.notYetImplemented()
    def unparser: Unparser = Assert.notYetImplemented()
  }

  @Test def testBasicTripleSequential(): Unit = {

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

  @Test def testMiddleSplicedOut(): Unit = {

    object first extends Primitive1(fakeTerm)
    object mid extends Primitive1(fakeTerm, false)
    object last extends Primitive1(fakeTerm)

    lazy val triple = prod("triple") { first ~ mid ~ last }

    assertFalse(triple.isEmpty)

    val exp = triple.gram
    assertTrue(exp.name.contains("SeqComp"))
    assertTrue(exp.toString.contains("first"))
    assertFalse(exp.toString.contains("mid")) // spliced out.
    assertTrue(exp.toString.contains("last"))
    assertTrue(exp.toString.contains(" ~ "))

  }

  @Test def testTopProdSplicedOut(): Unit = {

    object first extends Primitive1(fakeTerm)
    object mid extends Primitive1(fakeTerm, false)
    object last extends Primitive1(fakeTerm)

    lazy val triple = prod("triple", false) { first ~ mid ~ last }

    assertTrue(triple.isEmpty)

    val exp = triple.gram
    assertFalse(exp.name.contains("SeqComp"))
    assertFalse(exp.toString.contains("first"))
    assertFalse(exp.toString.contains("mid")) // spliced out.
    assertFalse(exp.toString.contains("last"))
    assertFalse(exp.toString.contains(" ~ "))

  }

}
