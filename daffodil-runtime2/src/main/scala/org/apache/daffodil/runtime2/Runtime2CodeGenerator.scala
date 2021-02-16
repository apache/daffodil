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

package org.apache.daffodil.runtime2

import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.grammar.Prod
import org.apache.daffodil.grammar.RootGrammarMixin
import org.apache.daffodil.grammar.SeqComp
import org.apache.daffodil.grammar.primitives.BinaryBoolean
import org.apache.daffodil.grammar.primitives.BinaryDouble
import org.apache.daffodil.grammar.primitives.BinaryFloat
import org.apache.daffodil.grammar.primitives.BinaryIntegerKnownLength
import org.apache.daffodil.grammar.primitives.CaptureContentLengthEnd
import org.apache.daffodil.grammar.primitives.CaptureContentLengthStart
import org.apache.daffodil.grammar.primitives.CaptureValueLengthEnd
import org.apache.daffodil.grammar.primitives.CaptureValueLengthStart
import org.apache.daffodil.grammar.primitives.ElementCombinator
import org.apache.daffodil.grammar.primitives.ElementParseAndUnspecifiedLength
import org.apache.daffodil.grammar.primitives.OrderedSequence
import org.apache.daffodil.grammar.primitives.ScalarOrderedSequenceChild
import org.apache.daffodil.grammar.primitives.SpecifiedLengthImplicit
import org.apache.daffodil.runtime2.generators.BinaryBooleanCodeGenerator
import org.apache.daffodil.runtime2.generators.BinaryFloatCodeGenerator
import org.apache.daffodil.runtime2.generators.BinaryIntegerKnownLengthCodeGenerator
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.runtime2.generators.ElementParseAndUnspecifiedLengthCodeGenerator
import org.apache.daffodil.runtime2.generators.OrderedSequenceCodeGenerator
import org.apache.daffodil.runtime2.generators.SeqCompCodeGenerator
import org.apache.daffodil.util.Misc

import scala.annotation.tailrec

object Runtime2CodeGenerator
  extends BinaryBooleanCodeGenerator
    with BinaryIntegerKnownLengthCodeGenerator
    with BinaryFloatCodeGenerator
    with ElementParseAndUnspecifiedLengthCodeGenerator
    with OrderedSequenceCodeGenerator
    with SeqCompCodeGenerator {

  @tailrec
  def generateCode(gram: Gram, state: CodeGeneratorState): Unit = {
    gram match {
      case g: RootGrammarMixin => Runtime2CodeGenerator.generateCode(g.documentElement, state)
      case g: Prod if (g.guard) => Runtime2CodeGenerator.generateCode(g.gram, state)
      case g: ElementCombinator => Runtime2CodeGenerator.generateCode(g.subComb, state)
      case g: SpecifiedLengthImplicit => Runtime2CodeGenerator.generateCode(g.eGram, state)
      case g: ScalarOrderedSequenceChild => Runtime2CodeGenerator.generateCode(g.term.termContentBody, state)
      case g: BinaryBoolean => binaryBooleanGenerateCode(g.e, state)
      case g: BinaryDouble => binaryFloatGenerateCode(g.e, 64, state)
      case g: BinaryFloat => binaryFloatGenerateCode(g.e, 32, state)
      case g: BinaryIntegerKnownLength => binaryIntegerKnownLengthGenerateCode(g, state)
      case g: ElementParseAndUnspecifiedLength => elementParseAndUnspecifiedLengthGenerateCode(g, state)
      case g: OrderedSequence => orderedSequenceGenerateCode(g, state)
      case g: SeqComp => seqCompGenerateCode(g, state)
      case _: CaptureContentLengthStart => noop
      case _: CaptureContentLengthEnd => noop
      case _: CaptureValueLengthStart => noop
      case _: CaptureValueLengthEnd => noop
      case _ => gram.SDE("Code generation not supported for: %s", Misc.getNameFromClass(gram))
    }
  }

  private def noop: Unit = {
    // Not generating code here, but can use as a breakpoint
  }
}
