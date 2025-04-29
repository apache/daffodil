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

import org.apache.daffodil.core.dsom.Term

trait HasStatementsGrammarMixin extends GrammarMixin { self: Term =>

  // Includes setVariable as well as assert/discriminator statements that
  // are not testKind="pattern"
  private lazy val lowPriorityStatementGrams = lowPriorityStatements.map { _.gram(self) }

  final lazy val dfdlLowPriorityStatementEvaluations =
    prod("dfdlStatementEvaluations", lowPriorityStatementGrams.length > 0) {
      lowPriorityStatementGrams.fold(mt) { _ ~ _ }
    }

  // assert/discriminator statements with testKind="pattern"
  private lazy val patternStatementGrams = patternStatements.map { _.gram(self) }

  final lazy val dfdlPatternStatementEvaluations =
    prod("dfdlPatternStatementEvaluations", patternStatementGrams.length > 0) {
      patternStatementGrams.fold(mt) { _ ~ _ }
    }
}
