package edu.illinois.ncsa.daffodil.processors

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

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.dpath._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.xml.Node
import edu.illinois.ncsa.daffodil.util.{ Debug, LogLevel, Logging, Info }
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
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

abstract class AssertBase(decl: AnnotatedSchemaComponent,
  exprWithBraces: String,
  xmlForNamespaceResolution: Node,
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
    this(decl, foundProp.value, foundProp.location.xml, decl, msg, discrim, assertKindName)

  override val baseName = assertKindName
  override lazy val expandedTypeName = XMLUtils.XSD_BOOLEAN
  override lazy val exprText = exprWithBraces
  override lazy val exprXMLForNamespace = xmlForNamespaceResolution
  override lazy val exprComponent = scWherePropertyWasLocated
  override def nodeKind = NodeInfo.Boolean

  def parser: DaffodilParser = new AssertExpressionEvaluationParser(msg, discrim, decl.runtimeData, expr)

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
    "{ fn:true() }", <xml xmlns:fn={ XMLUtils.XPATH_FUNCTION_NAMESPACE }/>, decl,
    // always true. We're just an assertion that says an initiator was found.
    "initiatedContent. This message should not be used.",
    true,
    "initiatedContent") {
}

case class SetVariable(decl: AnnotatedSchemaComponent, stmt: DFDLSetVariable)
  extends ExpressionEvaluatorBase(decl) {

  val baseName = "SetVariable[" + stmt.localName + "]"

  override lazy val exprText = stmt.value
  override lazy val exprXMLForNamespace = stmt.xml
  override lazy val exprComponent = stmt

  lazy val expandedTypeName = stmt.defv.extType

  override lazy val nodeKind = DPathUtil.convertTypeString(expandedTypeName)

  def parser: DaffodilParser = new SetVariableParser(expr, decl.runtimeData, stmt.defv.extName)
}

abstract class NewVariableInstanceBase(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends Terminal(decl, true) {
  val (uri, localName) = XMLUtils.QName(decl.namespaces, stmt.ref, decl.schemaDocument)
  val expName = XMLUtils.expandedQName(uri, localName)
}

case class NewVariableInstanceStart(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser: DaffodilParser = new NewVariableInstanceStartParser(decl.runtimeData)
}

case class NewVariableInstanceEnd(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser: DaffodilParser = new NewVariableInstanceEndParser(decl.runtimeData)
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
  def exprXMLForNamespace: Node
  def exprComponent: SchemaComponent
  def expandedTypeName: String
  def exprText: String

  def nodeKind: NodeInfo.Kind

  lazy val expr = _expr.value
  private val _expr = LV('expr) {
    e.expressionCompiler.compile(
      nodeKind, exprText, exprXMLForNamespace.scope, exprComponent, false)
  }
}

case class InputValueCalc(e: ElementBase)
  extends ExpressionEvaluatorBase(e) {

  val baseName = "InputValueCalc"
  lazy val exprProp = e.inputValueCalcOption match {
    case f: Found => f
    case _: NotFound => Assert.invariantFailed("must be a Found object")
  }

  override lazy val exprText = exprProp.value
  override lazy val exprXMLForNamespace = exprProp.location.xml
  override lazy val exprComponent = exprProp.location.asInstanceOf[SchemaComponent]

  lazy val pt = e.primType.typeRuntimeData
  override lazy val nodeKind = NodeInfo.fromPrimType(pt)
  lazy val ptn = pt.name
  lazy val expandedTypeName = XMLUtils.expandedQName(XMLUtils.XSD_NAMESPACE, ptn)

  def parser: DaffodilParser = new IVCParser(expr, e.elementRuntimeData)
}

abstract class AssertPatternPrimBase(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase)
  extends Terminal(decl, true) {

  lazy val eName = decl.prettyName
  lazy val testPattern = stmt.testTxt
  lazy val csName = charset.charsetName
  lazy val charset = decl.knownEncodingCharset

  def parser: DaffodilParser
}

case class AssertPatternPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssert)
  extends AssertPatternPrimBase(decl, stmt) {

  val kindString = "AssertPatternPrim"

  lazy val d = new ThreadLocal[DFDLDelimParser] {
    override def initialValue() = {
      new DFDLDelimParser(decl.knownEncodingIsFixedWidth, decl.knownEncodingWidthInBits, decl.knownEncodingName)
    }
  }

  def parser: DaffodilParser = new AssertPatternParser(eName, kindString, charset, d, decl.runtimeData, stmt)

}

case class DiscriminatorPatternPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase)
  extends AssertPatternPrimBase(decl, stmt) {

  val kindString = "DiscriminatorPatternPrim"

  lazy val d = new ThreadLocal[DFDLDelimParser] {
    override def initialValue() = {
      new DFDLDelimParser(decl.knownEncodingIsFixedWidth, decl.knownEncodingWidthInBits, decl.knownEncodingName)
    }
  }

  def parser: DaffodilParser = new DiscriminatorPatternParser(testPattern, eName, kindString, charset, d, decl.runtimeData, stmt, this)
}

trait TextReader extends Logging {

  /**
   * Readers are stored in the PState within the InStream object.
   */
  def getReader(charset: Charset, bitPos: Long, state: PState): DFDLCharReader = {
    // withLoggingLevel(LogLevel.Info) 
    {
      val csName = charset.name()
      log(LogLevel.Debug, "Retrieving reader at bytePos %s", bitPos >> 3)
      // Do we already have a reader in the PState?
      val res = state.inStream.getCharReader(charset, bitPos)
      res
    }
  }

}

