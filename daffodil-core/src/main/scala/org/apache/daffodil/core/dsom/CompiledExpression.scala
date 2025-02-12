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

package org.apache.daffodil.core.dsom

import java.lang.{ Boolean => JBoolean }
import java.lang.{ Long => JLong }
import scala.xml.NamespaceBinding

import org.apache.daffodil.core.dpath._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.Found
import org.apache.daffodil.lib.util.DPathUtil
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.runtime1.BasicComponent
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dpath.NodeInfo.AnyAtomic
import org.apache.daffodil.runtime1.dsom._
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive

object ExpressionCompilers extends ExpressionCompilerClass {
  override val String = new ExpressionCompiler[String]
  override val JLong = new ExpressionCompiler[JLong]
  override val AnyRef = new ExpressionCompiler[AnyRef]
  override val JBoolean = new ExpressionCompiler[JBoolean]
}

class ExpressionCompiler[T <: AnyRef] extends ExpressionCompilerBase[T] {

  /**
   * Compiles an expression or a literl value. If the passed in string does not
   * start with an unescaped curly brace it is treated as a literal value.
   *
   * An expression could be evaluated as constant as in { 5 } or { "California" },
   * and in that case this should return a ConstantExpression object.
   * Non-constant expressions will return a RuntimeExpressionDPath.
   *
   * Literal values (i.e. those that do not start with a unescape curly brace),
   * will be converted to a privite value as defined by nodeInfoKind and will
   * return a ConstantExpression with that value.
   *
   * This method available at compilation and also at runtime for use by the debuggger.
   */
  def compileExpression(
    qn: NamedQName,
    nodeInfoKind: NodeInfo.Kind,
    exprOrLiteral: String,
    namespaces: NamespaceBinding,
    noPrefixNamespace: NS,
    compileInfoWhereExpressionWasLocated: DPathCompileInfo,
    isEvaluatedAbove: Boolean,
    host: BasicComponent,
    compileInfo: DPathCompileInfo
  ): CompiledExpression[T] = {

    compileInfo.initialize()
    compileInfoWhereExpressionWasLocated.initialize()

    val res =
      if (DPathUtil.isExpression(exprOrLiteral)) {
        compileRealExpression(
          qn,
          nodeInfoKind,
          exprOrLiteral,
          namespaces,
          noPrefixNamespace,
          compileInfoWhereExpressionWasLocated,
          isEvaluatedAbove,
          host,
          compileInfo
        )
      } else {
        convertLiteralToConstant(
          qn,
          nodeInfoKind,
          exprOrLiteral,
          namespaces,
          noPrefixNamespace,
          compileInfoWhereExpressionWasLocated,
          isEvaluatedAbove
        )
      }
    res
  }

  /**
   * Compile a potentially runtime-valued property.
   *
   * The property value can be an expression or a literal constant.
   *
   * This form for expressions that are the values of most DFDL properties
   *
   * The isEvaluatedAbove argument is used for properties like occursCount
   * which are evaluated before the element whose declaration carries it, exists.
   * That is, it is evaluated one Infoset node above where the expression is
   * written. This doesn't matter unless the expression contains relative
   * paths. In that case those relative paths will all have to be adjusted
   * so they work in the context of one node above.
   */
  def compileProperty(
    qn: NamedQName,
    nodeInfoKind: NodeInfo.Kind,
    property: Found,
    host: BasicComponent,
    compileInfo: DPathCompileInfo,
    isEvaluatedAbove: Boolean = false
  ): CompiledExpression[T] = {

    compileExpression(
      qn,
      nodeInfoKind,
      property.value,
      property.location.namespaces,
      property.location.noPrefixNamespace,
      compileInfo,
      isEvaluatedAbove,
      host,
      compileInfo
    )
  }

  /**
   * Compile a potentially runtime-valued delimiter property
   *
   * The property value can be an expression or a literal constant.
   *
   * This form for delimiters and escapeEscapeCharacter since they
   * can have empty string if statically known, but if an evaluated expression,
   * it must be non-empty string.
   *
   * You can have an empty string, but only statically.
   * That turns off separators entirely.
   * If you have an expression (that is not trivially an empty string),
   * then it must be a non-empty string as the compiled parser will be
   * generated assuming there will be a concrete separator that is part of
   * the data syntax and serves as any delimiter to anchor the parse algorithm.
   *
   * We don't want to allow turning on/off whether a format is delimited or
   * not based on runtime expressions, only what the delimiters are.
   */
  def compileDelimiter(
    qn: NamedQName,
    staticNodeInfoKind: NodeInfo.Kind,
    runtimeNodeInfoKind: NodeInfo.Kind,
    property: Found,
    host: BasicComponent,
    compileInfo: DPathCompileInfo
  ): CompiledExpression[T] = {

    val isEvaluatedAbove = false
    val exprOrLiteral = property.value
    val namespacesForNamespaceResolution = property.location.namespaces
    val compileInfoWhereExpressionWasLocated = propertyCompileInfo(property)
    val compiled1 = compileExpression(
      qn,
      staticNodeInfoKind,
      exprOrLiteral,
      namespacesForNamespaceResolution,
      property.location.noPrefixNamespace,
      compileInfoWhereExpressionWasLocated,
      isEvaluatedAbove,
      host,
      compileInfo
    )

    if (compiled1.isConstant || (staticNodeInfoKind == runtimeNodeInfoKind)) {
      compiled1
    } else {
      val compiled2 = compileExpression(
        qn,
        runtimeNodeInfoKind,
        exprOrLiteral,
        namespacesForNamespaceResolution,
        property.location.noPrefixNamespace,
        compileInfoWhereExpressionWasLocated,
        isEvaluatedAbove,
        host,
        compileInfo
      )
      compiled2
    }
  }

  /**
   * Returns compile info of property regardless of origin.
   *
   * Needed because before serialization of the runtime data
   * objects, the location of a property may be the associated schema
   * component. Once serialized, at runtime when we're compiling expressions
   * we have only the DPathCompileInfo.
   */
  private def propertyCompileInfo(property: Found) = {
    val compileInfoWherePropertyWasLocated = {
      property.location match {
        case sc: SchemaComponent => sc.dpathCompileInfo
        case di: DPathCompileInfo => di
      }
    }
    compileInfoWherePropertyWasLocated
  }

  private def compileRealExpression(
    qn: NamedQName,
    nodeInfoKind: NodeInfo.Kind,
    exprOrLiteral: String,
    namespaces: NamespaceBinding,
    noPrefixNamespace: NS,
    compileInfoWhereExpressionWasLocated: DPathCompileInfo,
    isEvaluatedAbove: Boolean,
    host: BasicComponent,
    compileInfo: DPathCompileInfo
  ): CompiledExpression[T] = {

    // Treat this as an expression--validate and compile it

    val expr = exprOrLiteral.trim
    if (!expr.endsWith("}")) {
      val msg =
        "'%s' is an unterminated expression. Add missing closing brace, or escape opening brace with another opening brace."
      compileInfoWhereExpressionWasLocated.SDE(msg, exprOrLiteral)
    }

    val compiler = new DFDLPathExpressionParser[T](
      qn,
      nodeInfoKind,
      namespaces,
      noPrefixNamespace,
      compileInfo,
      isEvaluatedAbove,
      host
    )
    val compiledDPath = compiler.compile(expr)
    compiledDPath
  }

  private def convertLiteralToConstant(
    qn: NamedQName,
    nodeInfoKind: NodeInfo.Kind,
    exprOrLiteral: String,
    namespaces: NamespaceBinding,
    noPrefixNamespace: NS,
    compileInfoWhereExpressionWasLocated: DPathCompileInfo,
    isEvaluatedAbove: Boolean
  ): CompiledExpression[T] = {

    // This string is not a real expression, we need to convert it to it's
    // logical type and set it as a constant expression. Not compilation
    // required. If something is passed in that does not convert to the
    // expected type it will result in error.

    nodeInfoKind match {
      case atomic: AnyAtomic.Kind => {

        // remove the leading escape curly brace if it exists
        val literal =
          if (exprOrLiteral.startsWith("{{")) exprOrLiteral.tail
          else exprOrLiteral

        val logical: DataValuePrimitive =
          try {
            atomic.primType.fromXMLString(literal)
          } catch {
            case e: Exception => {
              val msg = "Unable to convert logical value \"%s\" to %s: %s"
              compileInfoWhereExpressionWasLocated.SDE(
                msg,
                exprOrLiteral,
                nodeInfoKind,
                e.getMessage
              )
            }
          }

        new ConstantExpression[T](qn, nodeInfoKind, logical.getAnyRef.asInstanceOf[T])
      }
      // $COVERAGE-OFF$
      case _ => {
        val msg = "No known primitive type to convert logical value to: %s"
        Assert.invariantFailed(
          msg + compileInfoWhereExpressionWasLocated.schemaFileLocation.toString
        )
      }
      // $COVERAGE-ON$
    }
  }
}
