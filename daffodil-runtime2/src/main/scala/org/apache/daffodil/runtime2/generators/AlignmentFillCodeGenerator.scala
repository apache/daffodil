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

package org.apache.daffodil.runtime2.generators

import org.apache.daffodil.grammar.primitives.AlignmentFill

trait AlignmentFillCodeGenerator {

  def alignmentFillGenerateCode(g: AlignmentFill, cgState: CodeGeneratorState): Unit = {
    val indent1 = if (cgState.hasChoice) INDENT else NO_INDENT
    val indent2 = if (cgState.hasArray) INDENT else NO_INDENT
    val alignmentInBits = g.alignment
    val octalFillByte = g.e.fillByteEv.constValue.toByte.toOctalString
    val initERDStatement = ""
    val parseStatement =
      s"""$indent1$indent2    // Fill to closest alignment
         |$indent1$indent2    parse_align($alignmentInBits, pstate);
         |$indent1$indent2    if (pstate->error) return;""".stripMargin
    val unparseStatement =
      s"""$indent1$indent2    // Fill to closest alignment
         |$indent1$indent2    unparse_align($alignmentInBits, '\\$octalFillByte', ustate);
         |$indent1$indent2    if (ustate->error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }
}
