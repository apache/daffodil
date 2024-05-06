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

package org.apache.daffodil.codegen.c.generators

// Generate C code for two types of assertion expressions:
//
//   { ../../foo/x eq 2 } as used in empty sequences
//   { . eq 2 } as used in simple elements
//
// Anything else likely won't produce valid C code at this time.

trait AssertStatementGenerateCode {

  def assertStatementGenerateCode(
    name: String,
    exprText: String,
    recoverable: Boolean,
    cgState: CodeGeneratorState
  ): Unit = {
    val indent = if (cgState.hasChoice) INDENT else NO_INDENT
    val exprWOBraces = exprText.stripPrefix("{").stripSuffix("}").trim()
    // Handle assertion expressions with "../" as used in empty sequences
    val modifiedExpr =
      if (exprWOBraces.startsWith("../"))
        s"../$exprWOBraces"
      else
        exprWOBraces
    // Handle assertion expressions with "." as used in simple elements
    val cExpr = cgState.cExpression(modifiedExpr).replaceFirst("^\\. ", s"instance->$name ")

    val initERDStatement = ""
    val parseStatement =
      s"""$indent    validate_dfdl_assert($cExpr, "$exprText", $recoverable, &pstate->pu);
         |$indent    if (pstate->pu.error) return;""".stripMargin
    val unparseStatement =
      s"""$indent    validate_dfdl_assert($cExpr, "$exprText", $recoverable, &ustate->pu);
         |$indent    if (ustate->pu.error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }

}
