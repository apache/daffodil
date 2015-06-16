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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.dpath._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.xml.Node
import edu.illinois.ncsa.daffodil.util.{ Debug, LogLevel, Logging, Info }
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthKind
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.parsers.NewVariableInstanceStartParser
import edu.illinois.ncsa.daffodil.processors.parsers.AssertExpressionEvaluationParser
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.parsers.AssertPatternParser
import edu.illinois.ncsa.daffodil.processors.parsers.DiscriminatorPatternParser
import edu.illinois.ncsa.daffodil.processors.parsers.NewVariableInstanceEndParser
import edu.illinois.ncsa.daffodil.processors.parsers.SetVariableParser
import edu.illinois.ncsa.daffodil.processors.parsers.IVCParser
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.processors.unparsers.ElementOutputValueCalcUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.SetVariableUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.ElementOutputValueCalcUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.SetVariableUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.NewVariableInstanceEndUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.NewVariableInstanceStartUnparser
import edu.illinois.ncsa.daffodil.compiler.ForParser

abstract class AssertBase(decl: AnnotatedSchemaComponent,
  exprWithBraces: String,
  namespacesForNamespaceResolution: scala.xml.NamespaceBinding,
  scWherePropertyWasLocated: AnnotatedSchemaComponent,
  msg: String,
  discrim: Boolean, // are we a discriminator or not.
  assertKindName: String)
  extends ExpressionEvaluatorBase(scWherePropertyWasLocated) {

  def this(
    decl: AnnotatedSchemaComponent,
    foundProp: Found,
    msg: String,
    discrim: Boolean, // are we a discriminator or not.
    assertKindName: String) =
    this(decl, foundProp.value, foundProp.location.namespaces, decl, msg, discrim, assertKindName)

  override val baseName = assertKindName
  override lazy val exprText = exprWithBraces
  override lazy val exprNamespaces = namespacesForNamespaceResolution
  override lazy val exprComponent = scWherePropertyWasLocated
  override def nodeKind = NodeInfo.Boolean

  override val forWhat = ForParser

  def parser: DaffodilParser = new AssertExpressionEvaluationParser(msg, discrim, decl.runtimeData, expr)

  override def unparser: DaffodilUnparser = Assert.invariantFailed("should not request unparser for asserts/discriminators")

}

abstract class AssertBooleanPrimBase(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase,
  discrim: Boolean, // are we a discriminator or not.
  assertKindName: String) extends AssertBase(decl, Found(stmt.testTxt, stmt), stmt.message, discrim, assertKindName)

case class AssertBooleanPrim(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase)
  extends AssertBooleanPrimBase(decl, stmt, false, "assert") {
}

case class DiscriminatorBooleanPrim(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase)
  extends AssertBooleanPrimBase(decl, stmt, true, "discriminator")

// TODO: performance wise, initiated content is supposed to be faster
// than evaluating an expression. There should be a better way to say
// "resolve this point of uncertainty" without having to introduce
// an XPath evaluator that runs fn:true() expression.
case class InitiatedContent(
  decl: AnnotatedSchemaComponent)
  extends AssertBase(decl,
    "{ fn:true() }", <xml xmlns:fn={ XMLUtils.XPATH_FUNCTION_NAMESPACE }/>.scope, decl,
    // always true. We're just an assertion that says an initiator was found.
    "initiatedContent. This message should not be used.",
    true,
    "initiatedContent") {
}

case class SetVariable(decl: AnnotatedSchemaComponent, stmt: DFDLSetVariable)
  extends ExpressionEvaluatorBase(decl) {

  val baseName = "SetVariable[" + stmt.varQName.local + "]"

  override lazy val exprText = stmt.value
  override lazy val exprNamespaces = stmt.xml.scope
  override lazy val exprComponent = stmt

  override lazy val nodeKind = stmt.defv.primType

  def parser: DaffodilParser = new SetVariableParser(expr, stmt.defv.runtimeData)
  override def unparser: DaffodilUnparser = new SetVariableUnparser(expr, stmt.defv.runtimeData)
}

abstract class NewVariableInstanceBase(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends Terminal(decl, true) {
}

case class NewVariableInstanceStart(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser: DaffodilParser = new NewVariableInstanceStartParser(decl.runtimeData)
  override def unparser: DaffodilUnparser = new NewVariableInstanceStartUnparser(decl.runtimeData)
}

case class NewVariableInstanceEnd(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser: DaffodilParser = new NewVariableInstanceEndParser(decl.runtimeData)
  override def unparser: DaffodilUnparser = new NewVariableInstanceEndUnparser(decl.runtimeData)
}

/**
 * Refactored primitives that use expressions to put expression evaluation in one place.
 * On this base (for the primitive), and a corresponding parser base class for the
 * actual evaluation.
 *
 * That fixed a bug where a SDE wasn't being reported until the parser was run that
 * could have been reported at compilation time.
 *
 * Anything being computed that involves the dsom or grammar objects or attributes of them,
 * should be done in the grammar primitives, and NOT in the parser.
 * This is important to insure errors are captured at compilation time and
 * reported on relevant objects.
 */
abstract class ExpressionEvaluatorBase(e: AnnotatedSchemaComponent) extends Terminal(e, true) {
  override def toString = baseName + "(" + exprText + ")"

  def baseName: String
  def exprNamespaces: scala.xml.NamespaceBinding
  def exprComponent: SchemaComponent
  def exprText: String

  def nodeKind: NodeInfo.Kind

  lazy val expr = LV('expr) {
    ExpressionCompiler.compile(
      nodeKind, exprText, exprNamespaces, exprComponent.dpathCompileInfo, false)
  }.value
}

abstract class ValueCalcBase(e: ElementBase)
  extends ExpressionEvaluatorBase(e) {

  def exprProp: Found
  override lazy val exprText = exprProp.value
  override lazy val exprNamespaces = exprProp.location.namespaces
  override lazy val exprComponent = exprProp.location.asInstanceOf[SchemaComponent]

  lazy val pt = e.primType //.typeRuntimeData
  override lazy val nodeKind = pt
  lazy val ptn = pt.name
}

case class ValueCalc(
  override val baseName: String,
  e: ElementBase,
  property: PropertyLookupResult)
  extends ValueCalcBase(e) {

  val exprProp = property.asInstanceOf[Found]

  def parser: DaffodilParser = new IVCParser(expr, e.elementRuntimeData)

  override def unparser: DaffodilUnparser = new ElementOutputValueCalcUnparser(e.elementRuntimeData, expr)

}

abstract class AssertPatternPrimBase(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase)
  extends Terminal(decl, true) {

  lazy val eName = decl.prettyName
  lazy val testPattern = {
    PatternChecker.checkPattern(stmt.testTxt, decl)
    stmt.testTxt
  }

  override val forWhat = ForParser

  def parser: DaffodilParser

  override def unparser: DaffodilUnparser = Assert.invariantFailed("should not request unparser for asserts/discriminators")
}

case class AssertPatternPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssert)
  extends AssertPatternPrimBase(decl, stmt) {

  val kindString = "AssertPatternPrim"

  def parser: DaffodilParser = {
    new AssertPatternParser(eName, kindString, decl.term.termRuntimeData, testPattern, stmt.message)
  }

}

case class DiscriminatorPatternPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase)
  extends AssertPatternPrimBase(decl, stmt) {

  val kindString = "DiscriminatorPatternPrim"

  def parser: DaffodilParser = new DiscriminatorPatternParser(testPattern, eName, kindString, decl.term.termRuntimeData, stmt.message)
}
