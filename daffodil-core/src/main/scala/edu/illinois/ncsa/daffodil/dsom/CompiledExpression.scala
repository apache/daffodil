package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.processors.EmptyVariableMap
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import scala.xml.Node
import scala.collection.JavaConversions._
import scala.collection.immutable.Queue
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.dpath._
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.xml.QName
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.processors.SchemaSetRuntimeData
import edu.illinois.ncsa.daffodil.xml.NamedQName
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.processors.HasSlotIndexInParent
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

object ExpressionCompiler extends ExpressionCompilerBase {

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
  def compile(nodeInfoKind: NodeInfo.Kind, property: Found, isEvaluatedAbove: Boolean = false): CompiledExpression =
    compile(nodeInfoKind, nodeInfoKind, property, isEvaluatedAbove)

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
  def compile(staticNodeInfoKind: NodeInfo.Kind, runtimeNodeInfoKind: NodeInfo.Kind, property: Found): CompiledExpression =
    compile(staticNodeInfoKind, runtimeNodeInfoKind, property, false)

  private def compile(staticNodeInfoKind: NodeInfo.Kind, runtimeNodeInfoKind: NodeInfo.Kind, property: Found, isEvaluatedAbove: Boolean): CompiledExpression = {
    val expr: String = property.value
    val namespacesForNamespaceResolution = property.location.namespaces
    val compileInfoWherePropertyWasLocated = {
      property.location match {
        case sc: SchemaComponent => sc.dpathCompileInfo
        case di: DPathCompileInfo => di
      }
    }

    val compiled1 = compile(staticNodeInfoKind, expr, namespacesForNamespaceResolution, compileInfoWherePropertyWasLocated,
      isEvaluatedAbove)
    if (compiled1.isConstant) return compiled1
    if (staticNodeInfoKind == runtimeNodeInfoKind) return compiled1
    //
    // TODO: consider passing in a flag or some other way of avoiding this
    // duplicate compile run.
    //
    val compiled2 = compile(runtimeNodeInfoKind, expr, namespacesForNamespaceResolution, compileInfoWherePropertyWasLocated,
      isEvaluatedAbove)
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
  def compile(nodeInfoKind: NodeInfo.Kind, exprWithBracesMaybe: String, namespaces: NamespaceBinding,
    compileInfoWherePropertyWasLocated: DPathCompileInfo,
    isEvaluatedAbove: Boolean) = {
    val expr = exprWithBracesMaybe
    //
    // we want to standardize that the expression has braces, and if it was
    // a literal string, that it has quotes around it. 
    //
    val exprForCompiling =
      if (DPathUtil.isExpression(expr)) expr
      else {
        // not an expression. For some properties like delimiters, you can use a literal string 
        // whitespace separated list of literal strings, or an expression in { .... }
        if (expr.startsWith("{") && !expr.startsWith("{{")) {
          val msg = "'%s' is an unterminated expression.  Add missing closing brace, or escape opening brace with another opening brace."
          compileInfoWherePropertyWasLocated.SDE(msg, expr)
        }
        val expr1 = if (expr.startsWith("{{"))
          expr.tail // everthing except the self-escaped leading brace
        else expr
        //
        // Literal strings get surrounded with quotation marks
        //
        val withQuotes = {
          nodeInfoKind match {
            case _: NodeInfo.String.Kind if (expr1.contains("'")) => "\"" + expr1 + "\""
            case _: NodeInfo.String.Kind => "'" + expr1 + "'"
            case _ => expr1
          }
        }
        withQuotes
      }
    // If we get here then now it's something we can compile. It might be trivial
    // to compile (e.g, '5' compiles to Literal(5)) but we uniformly compile 
    // everything.

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
    compileExpression(nodeInfoKind, exprForCompiling, namespaces,
      compileInfoWherePropertyWasLocated, isEvaluatedAbove)
  }

  /**
   * Compile the expression.
   *
   * Expression may or may not have braces around it. It could still be a constant as in
   * { 5 } or { "California" }, and in that case this should return a ConstantExpression
   * object.
   */
  private def compileExpression(
    nodeInfoKind: NodeInfo.Kind,
    expr: String,
    // Why this additional namespaceBinding argument?
    // how is this different from the namespace resolution that the
    // next argument provides? Ans: Point of use versus point of definition.
    namespaces: NamespaceBinding,
    compileInfoWherePropertyWasLocated: DPathCompileInfo,
    isEvaluatedAbove: Boolean = false): CompiledExpression = {
    // This is important. The namespace bindings we use must be
    // those from the object where the property carrying the expression 
    // was written, not those of the edecl object where the property 
    // value is being used/compiled. JIRA DFDL-407
    //
    val compiler = new DFDLPathExpressionParser(
      nodeInfoKind, namespaces, compileInfoWherePropertyWasLocated, isEvaluatedAbove)
    val compiledDPath = compiler.compile(expr)
    compiledDPath
  }
}

