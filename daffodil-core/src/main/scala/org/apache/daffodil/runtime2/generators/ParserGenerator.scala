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

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.exceptions.ThrowsSDE

import scala.collection.mutable

trait ParserGenerator {
  // TBD: if the code-generator state builds up content by side-effect, there's no
  // reason to be returning it. It should return Unit.
  def generateCode(state: CodeGeneratorState): Unit
}

/**
 * Builds up the state of generated code.
 */
class CodeGeneratorState(private var code: String) extends DFDL.CodeGeneratorState {
  private val structs = mutable.Stack[ComplexCGState]()
  private val finalStructs = mutable.ArrayBuffer[ComplexCGState]()
  private val erds = mutable.ArrayBuffer[String]()

  def this() = this(null)

  def pushComplexElement(context: ElementBase): Unit = {
    structs.push(new ComplexCGState())
  }

  def popComplexElement(context: ElementBase): Unit = {
    val complexCgState = structs.pop()
    finalStructs += complexCgState
  }

  def newSimpleTypeERD(context: ElementBase): Unit = {
    val e = context.namedQName.local
    val typeCode = context.optPrimType.get match {
      case PrimType.Int => "PRIMITIVE_INT"
      case PrimType.String => "PRIMITIVE_STRING"
      case p: PrimType => context.SDE("PrimType %s not supported yet.", p.toString)
    }
    val erd =
      s"""
        |ERD ${e}ERD =
        |{
        |	{ "${e}" }, 			// namedQName
        |	${typeCode},		// typeCode
        |	0,					// count_children
        |	NULL,				// offsets
        |	NULL,				// childrenERD
        |	NULL,				// parseSelf
        |	NULL,				// unparseSelf
        |	NULL				// newInstance
        |};""".stripMargin
    erds += erd
  }

  def toPrimitive(primType: NodeInfo.PrimType, context: ThrowsSDE): String = {
    import NodeInfo.PrimType
    primType match {
      case PrimType.Long => "long"
      case PrimType.Int => "int"
      case _ => context.SDE("Unsupported primitive type: " + primType)
    }
  }

  def newFieldDeclaration(definition: String, name: String): Unit = {
    val complexCGState = structs.top
    complexCGState.declarations += s"  $definition $name;"
  }

  //  def toComplexType(child: ElementBase): String = {
//    child.complexType.diagnosticDebugName // for now?
//  }
//
//  def newAssignment(name: String, str: String): Unit = {
//    statements.append(s"$name = $str;\n")
//  }
//
//  def newAllocation(name: String, typeDeclaration: String): Unit = {
//    declarations.append(s"$name = new $typeDeclaration();\n")
//  }
//
//  def newRecursiveCall(name: String, method: String): Unit = {
//    statements.append("$name->$method;\n")
//  }
//
//  def closeDefinition(): Unit = {
//    declarations.append("\n")
//  }
//
//  def finalGenerate(): Unit = {
//    code = declarations.toString() + statements.toString()
//  }

  def viewCode: String = code
}

class ComplexCGState() {
  val declarations = mutable.ArrayBuffer[String]()
  val parserStatements = mutable.ArrayBuffer[String]()
  val unparserStatements = mutable.ArrayBuffer[String]()
  val newInstanceStatements = mutable.ArrayBuffer[String]()
}
