/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.dpath._
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.xml.NamedQName
import java.lang.{ Long => JLong, Boolean => JBoolean }
import edu.illinois.ncsa.daffodil.schema.annotation.props.Found
import edu.illinois.ncsa.daffodil.oolag.OOLAG._

object ExpressionCompilers extends ExpressionCompilerClass {
  override val String = new ExpressionCompiler[String]
  override val JLong = new ExpressionCompiler[JLong]
  override val AnyRef = new ExpressionCompiler[AnyRef]
  override val JBoolean = new ExpressionCompiler[JBoolean]
}

class ExpressionCompiler[T <: AnyRef] extends ExpressionCompilerBase[T] {

  /**
   * For expressions that are the values of DFDL properties
   *
   * The isEvaluatedAbove argument is used for properties like occursCount
   * which are evaluated before the element whose declaration carries it, exists.
   * That is, it is evaluated one Infoset node above where the expression is
   * written. This doesn't matter unless the expression contains relative
   * paths. In that case those relative paths will all have to be adjusted
   * so they work in the context of one node above.
   *
   * There are two nodeInfokind args as a hack for dealing with delimiters
   * where they can be empty string if a literal constant, but cannot be empty
   * string if returned.
   */
  /*
   * This form for most properties
   */
  def compile(qn: NamedQName, nodeInfoKind: NodeInfo.Kind, property: Found, host: OOLAGHost, isEvaluatedAbove: Boolean = false): CompiledExpression[T] =
    compile(qn, nodeInfoKind, nodeInfoKind, property, isEvaluatedAbove, host)

  /*
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
  def compile(qn: NamedQName, staticNodeInfoKind: NodeInfo.Kind, runtimeNodeInfoKind: NodeInfo.Kind, property: Found,
    host: OOLAGHost): CompiledExpression[T] =
    compile(qn, staticNodeInfoKind, runtimeNodeInfoKind, property, false, host)

  private def compile(qn: NamedQName, staticNodeInfoKind: NodeInfo.Kind, runtimeNodeInfoKind: NodeInfo.Kind,
    property: Found, isEvaluatedAbove: Boolean, host: OOLAGHost): CompiledExpression[T] = {
    val expr: String = property.value
    val namespacesForNamespaceResolution = property.location.namespaces
    val compileInfoWherePropertyWasLocated = {
      property.location match {
        case sc: SchemaComponent => sc.dpathCompileInfo
        case di: DPathCompileInfo => di
      }
    }

    compile(qn, staticNodeInfoKind, runtimeNodeInfoKind, expr, namespacesForNamespaceResolution, compileInfoWherePropertyWasLocated, isEvaluatedAbove, host)
  }

  /**
   * This is the fully general case.
   */
  def compile(qn: NamedQName,
    staticNodeInfoKind: NodeInfo.Kind,
    runtimeNodeInfoKind: NodeInfo.Kind,
    expr: String,
    namespaces: NamespaceBinding,
    compileInfoWherePropertyWasLocated: DPathCompileInfo,
    isEvaluatedAbove: Boolean,
    host: OOLAGHost): CompiledExpression[T] = {
    val compiled1 = compile(qn, staticNodeInfoKind, expr, namespaces, compileInfoWherePropertyWasLocated,
      isEvaluatedAbove, host)
    if (compiled1.isConstant) return compiled1
    if (staticNodeInfoKind == runtimeNodeInfoKind) return compiled1
    //
    // TODO: consider passing in a flag or some other way of avoiding this
    // duplicate compile run.

    // This is, this nodeInfo.Kind is used as the target type in the DPath expression compiler, and
    //
    val compiled2 = compile(qn, runtimeNodeInfoKind, expr, namespaces, compileInfoWherePropertyWasLocated,
      isEvaluatedAbove, host)
    compiled2
  }

  /**
   * compiles the expression.
   *
   * If it happens to be a literal constant (i.e.,
   * no braces, then this handles that case directly. Otherwise it calls
   * the method to actually compile the expression.
   *
   */
  def compile(qn: NamedQName, nodeInfoKind: NodeInfo.Kind, exprWithBracesMaybe: String, namespaces: NamespaceBinding,
    compileInfoWherePropertyWasLocated: DPathCompileInfo,
    isEvaluatedAbove: Boolean, host: OOLAGHost) = {
    var compile: Boolean = true
    val expr = exprWithBracesMaybe
    //
    // we want to standardize that the expression has braces
    //
    val exprForCompiling =
      if (DPathUtil.isExpression(expr)) expr.trim
      else {
        // not an expression. For some properties like delimiters, you can use a literal string
        // whitespace separated list of literal strings, or an expression in { .... }
        if (expr.startsWith("{") && !expr.startsWith("{{")) {
          val msg = "'%s' is an unterminated expression.  Add missing closing brace, or escape opening brace with another opening brace."
          compileInfoWherePropertyWasLocated.SDE(msg, expr)
        }
        val expr1 = if (expr.startsWith("{{"))
          expr.tail // everything except the self-escaped leading brace
        else expr
        //
        // Literal String: if the target type is String, do not compile.
        //
        nodeInfoKind match {
          case _: NodeInfo.String.Kind => compile = false // Constant String
          case _ => compile = true
        }
        expr1
      }
    // If we get here then now it's something we can compile. It might be trivial
    // to compile (e.g, '5' compiles to Literal(5)) but we no longer uniformly
    // compile everything.  Due to the performance optimization (DFDL-1775),
    // we will NOT compile constant strings (constant values whose target type
    // is String).

    /* Question: If something starts with {{, e.g.
     * separator="{{ not an expression", then we strip off the first brace,
     * wrap in quotes, and compile it? Why try compiling it? Shouldn't we just
     * return a constant expression or something at this point?
     * <p>
     * Answer: Conversions. E.g., if you have "{{ 6.847 }" as the expression
     * for an inputValueCalc on an element of float type, then the compiler
     * can tell you this isn't going to convert - you get a type check error or
     * maybe a number format exception at constant-folding time, which tells us
     * that the expression - even though it's a constant, isn't right.
     *
     * If we try to do this outside the expression compiler we'd be replicating
     * some of this type-infer/check logic.
     */
    val res = if (compile) {
      compileExpression(qn, nodeInfoKind, exprForCompiling, namespaces,
        compileInfoWherePropertyWasLocated, isEvaluatedAbove, host)
    } else {
      // Don't compile, meaning this is a constant string
      val res = new ConstantExpression[T](qn, nodeInfoKind, exprForCompiling.asInstanceOf[T])
      res
    }
    res
  }

  /**
   * Compile the expression.
   *
   * Expression may or may not have braces around it. It could still be a constant as in
   * { 5 } or { "California" }, and in that case this should return a ConstantExpression
   * object.
   */
  private def compileExpression(
    qn: NamedQName,
    nodeInfoKind: NodeInfo.Kind,
    expr: String,
    // Why this additional namespaceBinding argument?
    // how is this different from the namespace resolution that the
    // next argument provides? Ans: Point of use versus point of definition.
    namespaces: NamespaceBinding,
    compileInfoWherePropertyWasLocated: DPathCompileInfo,
    isEvaluatedAbove: Boolean,
    host: OOLAGHost): CompiledExpression[T] = {
    // This is important. The namespace bindings we use must be
    // those from the object where the property carrying the expression
    // was written, not those of the edecl object where the property
    // value is being used/compiled. JIRA DFDL-407
    //
    val compiler = new DFDLPathExpressionParser[T](qn,
      nodeInfoKind, namespaces, compileInfoWherePropertyWasLocated, isEvaluatedAbove, host)
    val compiledDPath = compiler.compile(expr)
    compiledDPath
  }
}
