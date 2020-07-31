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

import scala.collection.JavaConverters._

import org.apache.daffodil.codegen.ast._
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dsom.ElementBase

class ElementParserGenerator(context: ElementBase, contentParserGenerator: ParserGenerator)
  extends ParserGenerator {

  def toPrimitive(primType: NodeInfo.PrimType): TypeDefinition = {
    import NodeInfo.PrimType
    primType match {
      case PrimType.Long => Primitive.LONG
      case PrimType.Int => Primitive.INTEGER
      case _ => context.SDE("Unsupported primitive type: " + primType)
    }
  }

  def toComplexType(e: ElementBase): TypeDefinition = {
    // Stubbed code
    Expressions.typeOf("e2")
  }

  def toParseMethod(e: ElementBase): Method = {
    Expressions.method(Expressions.typeOf("E2_Type"), "parseSelf", Primitive.VOID,
      Nil.asJava, Nil.asJava)
  }

  override def generateCode(cgState: CodeGeneratorState): Unit = {

    if (context.isSimpleType) {
      cgState.newSimpleTypeERD(context) // e1ERD static initializer
      contentParserGenerator.generateCode(cgState)
    } else {
      cgState.pushComplexElement(context)
      context.elementChildren.foreach { child =>
        if (child.isSimpleType) {
          // int e1;
          cgState.addFieldDeclaration(cgState.toPrimitive(child.optPrimType.get, context), child.name)
          child.enclosedElement.generateCode(cgState)
        } else {
          ???
          //cgState.newFieldDeclaration(toComplexType(child), child.name) // struct, union, maybe array
        }
      }
      cgState.addStruct(context)
      cgState.addParser(context)
      cgState.addNewInstance(context)
      cgState.addComplexTypeERD(context)
      cgState.popComplexElement(context)
    }
  }
}
