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
import org.apache.daffodil.grammar.primitives.ElementParseAndUnspecifiedLength
import org.apache.daffodil.runtime2.Runtime2CodeGenerator

trait ElementParseAndUnspecifiedLengthCodeGenerator {

  def elementParseAndUnspecifiedLengthGenerateCode(g: ElementParseAndUnspecifiedLength,
                                                   cgState: CodeGeneratorState): Unit = {
    val context = g.context
    val elementContentGram = g.eGram // a Gram isA ParserGenerator

    context.schemaDefinitionWhen(context.inputValueCalcOption.isDefined, "Elements with inputValueCalc are not supported.")
    context.schemaDefinitionWhen(context.outputValueCalcOption.isDefined, "Elements with outputValueCalc are not supported.")
    context.schemaDefinitionUnless(g.eBeforeGram.isEmpty
      || g.eBeforeGram == AlignmentFill(context), "Statements associated with elements are not supported.")
    context.schemaDefinitionUnless(g.eAfterGram.isEmpty, "Statements associated with elements are not supported.")
    context.schemaDefinitionUnless(g.repTypeElementGram.isEmpty, "dfdlx:repType is not supported.")

    if (context.isSimpleType) {
      cgState.addSimpleTypeERD(context) // ERD static initializer
      Runtime2CodeGenerator.generateCode(elementContentGram, cgState) // initSelf, parseSelf, unparseSelf
    } else {
      cgState.pushComplexElement(context)
      cgState.addBeforeSwitchStatements(context) // switch statements for choices
      context.elementChildren.foreach { child =>
        if (!child.isSimpleType) {
          cgState.addComplexTypeStatements(child) // recursive calls to parse, unparse, init
          cgState.addComputations(child) // offset, ERD computations
        }
        cgState.addFieldDeclaration(context, child) // struct member for child
        Runtime2CodeGenerator.generateCode(child.enclosedElement, cgState) // generate children too
      }
      cgState.addAfterSwitchStatements(context) // switch statements for choices
      cgState.addStruct(context) // struct definition
      cgState.addImplementation(context) // initSelf, parseSelf, unparseSelf
      cgState.addComplexTypeERD(context) // ERD static initializer
      cgState.popComplexElement()
    }
  }
}
