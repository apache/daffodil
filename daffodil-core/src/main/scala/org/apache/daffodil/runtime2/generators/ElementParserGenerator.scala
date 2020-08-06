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

import org.apache.daffodil.dsom.ElementBase

class ElementParserGenerator(context: ElementBase, contentParserGenerator: ParserGenerator)
  extends ParserGenerator {

  override def generateCode(cgState: CodeGeneratorState): Unit = {

    if (context.isSimpleType) {
      cgState.addSimpleTypeERD(context) // ERD static initializer
      contentParserGenerator.generateCode(cgState)
    } else {
      cgState.pushComplexElement(context)
      context.elementChildren.foreach { child =>
        if (child.isSimpleType) {
          cgState.addFieldDeclaration(cgState.toPrimitive(child.optPrimType.get, context), child.name)
          child.enclosedElement.generateCode(cgState)
        } else {
          ??? // add a child struct, union, maybe array element inside parent element
          //cgState.addFieldDeclaration(toComplexType(child), child.name)
        }
      }
      cgState.addStruct(context) // struct definition
      cgState.addParser(context) // parse_self, unparse_self, newInstance
      cgState.addComplexTypeERD(context) // ERD static initializer
      cgState.popComplexElement(context)
    }
  }
}
