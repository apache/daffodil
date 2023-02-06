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

import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.core.grammar.Prod
import org.apache.daffodil.core.grammar.SeqComp
import org.apache.daffodil.core.grammar.primitives.AlignmentFill
import org.apache.daffodil.core.grammar.primitives.BinaryBoolean
import org.apache.daffodil.core.grammar.primitives.BinaryDouble
import org.apache.daffodil.core.grammar.primitives.BinaryFloat
import org.apache.daffodil.core.grammar.primitives.BinaryIntegerKnownLength
import org.apache.daffodil.core.grammar.primitives.CaptureContentLengthEnd
import org.apache.daffodil.core.grammar.primitives.CaptureContentLengthStart
import org.apache.daffodil.core.grammar.primitives.CaptureValueLengthEnd
import org.apache.daffodil.core.grammar.primitives.CaptureValueLengthStart
import org.apache.daffodil.core.grammar.primitives.ChoiceCombinator
import org.apache.daffodil.core.grammar.primitives.ElementCombinator
import org.apache.daffodil.core.grammar.primitives.ElementParseAndUnspecifiedLength
import org.apache.daffodil.core.grammar.primitives.ElementUnused
import org.apache.daffodil.core.grammar.primitives.HexBinaryLengthPrefixed
import org.apache.daffodil.core.grammar.primitives.HexBinarySpecifiedLength
import org.apache.daffodil.core.grammar.primitives.OrderedSequence
import org.apache.daffodil.core.grammar.primitives.RepOrderedExactlyNSequenceChild
import org.apache.daffodil.core.grammar.primitives.RepOrderedExpressionOccursCountSequenceChild
import org.apache.daffodil.core.grammar.primitives.RightFill
import org.apache.daffodil.core.grammar.primitives.ScalarOrderedSequenceChild
import org.apache.daffodil.core.grammar.primitives.SpecifiedLengthExplicit
import org.apache.daffodil.core.grammar.primitives.SpecifiedLengthImplicit
import org.apache.daffodil.runtime2.generators.AlignmentFillCodeGenerator
import org.apache.daffodil.runtime2.generators.BinaryBooleanCodeGenerator
import org.apache.daffodil.runtime2.generators.BinaryFloatCodeGenerator
import org.apache.daffodil.runtime2.generators.BinaryIntegerKnownLengthCodeGenerator
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.runtime2.generators.HexBinaryCodeGenerator
import org.apache.daffodil.lib.util.Misc

object Runtime2CodeGenerator
  extends AlignmentFillCodeGenerator
  with BinaryBooleanCodeGenerator
  with BinaryIntegerKnownLengthCodeGenerator
  with BinaryFloatCodeGenerator
  with HexBinaryCodeGenerator {

  def generateCode(gram: Gram, cgState: CodeGeneratorState): Unit = {
    gram match {
      case g: AlignmentFill => alignmentFillGenerateCode(g, cgState)
      case g: BinaryBoolean => binaryBooleanGenerateCode(g.e, cgState)
      case g: BinaryDouble => binaryFloatGenerateCode(g.e, lengthInBits = 64, cgState)
      case g: BinaryFloat => binaryFloatGenerateCode(g.e, lengthInBits = 32, cgState)
      case g: BinaryIntegerKnownLength => binaryIntegerKnownLengthGenerateCode(g.e, g.lengthInBits, g.signed, cgState)
      case g: CaptureContentLengthEnd => noop(g)
      case g: CaptureContentLengthStart => noop(g)
      case g: CaptureValueLengthEnd => noop(g)
      case g: CaptureValueLengthStart => noop(g)
      case g: ChoiceCombinator => choiceCombinator(g, cgState)
      case g: ElementCombinator => elementCombinator(g, cgState)
      case g: ElementParseAndUnspecifiedLength => elementParseAndUnspecifiedLengthGenerateCode(g, cgState)
      case g: ElementUnused => noop(g)
      case g: HexBinaryLengthPrefixed => hexBinaryLengthPrefixedGenerateCode(g.e, cgState)
      case g: HexBinarySpecifiedLength => hexBinarySpecifiedLengthGenerateCode(g.e, cgState)
      case g: OrderedSequence => orderedSequenceGenerateCode(g, cgState)
      case g: Prod => prod(g, cgState)
      case g: RepOrderedExactlyNSequenceChild => repOrderedExactlyNSequenceChild(g, cgState)
      case g: RepOrderedExpressionOccursCountSequenceChild => repOrderedExpressionOccursCountSequenceChild(g, cgState)
      case g: RightFill => noop(g)
      case g: ScalarOrderedSequenceChild => scalarOrderedSequenceChild(g, cgState)
      case g: SeqComp => seqCompGenerateCode(g, cgState)
      case g: SpecifiedLengthExplicit => specifiedLengthExplicit(g, cgState)
      case g: SpecifiedLengthImplicit => specifiedLengthImplicit(g, cgState)
      case _ => gram.SDE("Code generation not supported for: %s", Misc.getNameFromClass(gram))
    }
  }

  private def choiceCombinator(g: ChoiceCombinator, cgState: CodeGeneratorState): Unit = {
    cgState.addBeforeSwitchStatements() // switch statements for choices
    for (gram <- g.alternatives) {
      Runtime2CodeGenerator.generateCode(gram, cgState)
    }
    cgState.addAfterSwitchStatements() // switch statements for choices
  }

  private def elementCombinator(g: ElementCombinator, cgState: CodeGeneratorState): Unit = {
    cgState.pushElement(g.context)
    Runtime2CodeGenerator.generateCode(g.subComb, cgState)
    cgState.popElement(g.context)
  }

  private def elementParseAndUnspecifiedLengthGenerateCode(g: ElementParseAndUnspecifiedLength, cgState: CodeGeneratorState): Unit = {
    Runtime2CodeGenerator.generateCode(g.eGram, cgState)
  }

  private def noop(g: Gram): Unit = {
    g.name // Not generating code, but can use as a breakpoint
  }

  private def orderedSequenceGenerateCode(g: OrderedSequence, cgState: CodeGeneratorState): Unit = {
    for (gram <- g.sequenceChildren) {
      Runtime2CodeGenerator.generateCode(gram, cgState)
    }
  }

  private def prod(g: Prod, cgState: CodeGeneratorState): Unit = {
    if (g.guard) Runtime2CodeGenerator.generateCode(g.gram, cgState)
  }

  private def repOrderedExactlyNSequenceChild(g: RepOrderedExactlyNSequenceChild, cgState: CodeGeneratorState): Unit = {
    cgState.pushArray(g.context)
    Runtime2CodeGenerator.generateCode(g.term.termContentBody, cgState)
    cgState.popArray(g.context)
  }

  private def repOrderedExpressionOccursCountSequenceChild(g: RepOrderedExpressionOccursCountSequenceChild, cgState: CodeGeneratorState): Unit = {
    cgState.pushArray(g.context)
    Runtime2CodeGenerator.generateCode(g.term.termContentBody, cgState)
    cgState.popArray(g.context)
  }

  private def scalarOrderedSequenceChild(g: ScalarOrderedSequenceChild, cgState: CodeGeneratorState): Unit = {
    Runtime2CodeGenerator.generateCode(g.term.termContentBody, cgState)
  }

  private def seqCompGenerateCode(g: SeqComp, cgState: CodeGeneratorState): Unit = {
    for (gram <- g.children) {
      Runtime2CodeGenerator.generateCode(gram, cgState)
    }
  }

  private def specifiedLengthExplicit(g: SpecifiedLengthExplicit, cgState: CodeGeneratorState): Unit = {
    Runtime2CodeGenerator.generateCode(g.eGram, cgState)
  }

  private def specifiedLengthImplicit(g: SpecifiedLengthImplicit, cgState: CodeGeneratorState): Unit = {
    Runtime2CodeGenerator.generateCode(g.eGram, cgState)
  }
}
