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

import org.apache.daffodil.core.compiler.BothParserAndUnparser
import org.apache.daffodil.core.compiler.ParserOrUnparser
import org.apache.daffodil.core.dsom.SchemaComponent
import org.apache.daffodil.core.dsom.Term
import org.apache.daffodil.core.runtime1.GramRuntime1Mixin
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.oolag.OOLAG.OOLAGHostImpl
import org.apache.daffodil.runtime1.BasicComponent

/**
 * Gram - short for "Grammar Term"
 *
 * These are the objects in the grammar.
 *
 * This grammar really differs a great deal from what we find in the DFDL specification
 * because it actually has to be operationalized.
 *
 * Many of the grammar productions really aren't terribly grammar-like
 * in appearance, because the conditional logic overwhelms the aspects that look
 * like grammar productions.
 *
 * Another way to think of this as it's just the "second tree".
 * Daffodil starts by creating the DSOM "first tree"
 * which is just the AST (Abstract Syntax Tree) of the DFDL language. Then by way of compiling creates
 * this Gram tree from that which enables a variety of optimizations based on a simple rules-with-guards idiom.
 *
 * This Gram tree is then a generator of a Parser and an Unparser which incorporate both
 * the parsing/unparsing logic and all RuntimeData structures in their members. If something completely optimizes out
 * then it becomes the EmptyGram which other Gram combinators recognize and optimize out.
 */
abstract class Gram(contextArg: SchemaComponent)
  extends OOLAGHostImpl(contextArg)
  with BasicComponent
  with GramRuntime1Mixin {

  final override lazy val tunable = context.tunable

  final override def schemaFileLocation = context.schemaFileLocation
  final override def localSuppressSchemaDefinitionWarnings =
    context.localSuppressSchemaDefinitionWarnings

  final override def SDE(str: String, args: Any*): Nothing = context.SDE(str, args: _*)

  final override def SDW(warnID: WarnID, str: String, args: Any*): Unit =
    context.SDW(warnID, str, args: _*)

  val forWhat: ParserOrUnparser = BothParserAndUnparser

  val context: SchemaComponent = contextArg

  def term = context.asInstanceOf[Term]

  /**
   * Some grammar terms (productions specifically) have an indirection
   * to the grammar term that is their definition.
   *
   * This would be protected, except it is used in unit testing.
   */
  private[grammar] def gram: Gram = this

  private[grammar] def deref = this // override in Prod

  def name: String = diagnosticDebugName

  override lazy val path = context.path + "%" + diagnosticDebugName

  def isEmpty =
    false // they are by default not empty. Overridden in the cases where they could be.

  /**
   * Sequential composition operator in the grammar.
   *
   * Note: This should not evaluate the argument unless it has to.
   */
  def ~(qq: => Gram) = {
    lazy val q = qq.deref
    val self = this.deref

    val res =
      if (self.isEmpty) {
        if (q.isEmpty)
          EmptyGram
        else q
      } else if (q.isEmpty) {
        self
      } else {
        Assert.invariant(!self.isEmpty)
        Assert.invariant(!q.isEmpty)
        SeqComp(context, self, q)
      }
    res
  }

  /**
   * This || operator means one of the operands ONLY. I.e., statically
   * they are supposed to be mutually exclusive, so only one (or none) of them should
   * ever survive.
   */
  def ||(qq: => Gram) = {
    lazy val q = qq.deref
    val self = this.deref
    if (self.isEmpty)
      if (q.isEmpty) EmptyGram
      else q
    else if (q.isEmpty) self
    else
      Assert.invariantFailed("More than one alternative for || survived in the grammar")
  }

}
