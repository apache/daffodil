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

import org.apache.daffodil.core.compiler._
import org.apache.daffodil.core.dsom.SchemaComponent

trait GrammarMixin {

  /**
   * For unit testing, we want to create GrammarMixin objects that are not
   * schema components. So we can't use a self-type here. Instead we
   * define this abstract grammarContext.
   */
  protected def grammarContext: SchemaComponent

  protected val NYI = false // our flag for Not Yet Implemented

  protected final def mt: Gram = EmptyGram

  /**
   * Use when production has a guard predicate
   *
   */
  def prod(
    prodName: String,
    guard: Boolean = true,
    forWhat: ParserOrUnparser = BothParserAndUnparser
  )(gram: => Gram): Gram = {
    new Prod(prodName, grammarContext, guard, gram, forWhat)
  }

  /**
   * Use when production has no guard, but you want to name the production
   * anyway (for debug visibility perhaps).
   */
  def prod(prodName: String)(gram: => Gram): Gram = {
    new Prod(prodName, grammarContext, true, gram, BothParserAndUnparser)
  }

}
