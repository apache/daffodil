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

import scala.collection.mutable.ArrayBuffer
import org.apache.daffodil.codegen.ast.{Node, ClassDeclaration, WriterFactory, Expressions, Block}
import org.apache.daffodil.codegen.cpp.CppGenerator

trait ParserGenerator {
  // TBD: if the code-generator state builds up content by side-effect, there's no
  // reason to be returning it. It should return Unit.
  def generateCode(state: CodeGeneratorState): CodeGeneratorState
}

/**
 * Builds up the state of generated code.
 *
 * Contains declarations of object classes corresponding to the
 * schema-described object - that is, the POJO object definitions.
 */
class CodeGeneratorState(nodeArg: Node) {

  val node: Node = nodeArg

  def block: Block = nodeArg.asInstanceOf[Block]

  /**
   * The expression that gives access to the current state.
   */
  val pStateType = Expressions.typeOf("org.apache.daffodil.runtime2.parser.PState")
  val stateExp = Expressions.parameter("state", pStateType)

  /**
   * Allocates an initial code generator state
   */
  def this() = this(null)

  private val classDecls_ = new ArrayBuffer[ClassDeclaration]

  def addClassDecl(classDecl: ClassDeclaration): Unit = {
    classDecls_ += classDecl
  }

  private var codeString: String = _

  def finalGenerate(): Unit = {
    val factory = new WriterFactory(4)
    val generator = new CppGenerator(factory)
    val strings =
      classDecls_.map { cd =>
        generator.generateOutput(cd).get(0)
      }
    codeString = strings.mkString("\n")
  }

  def viewCode: String = codeString
}
