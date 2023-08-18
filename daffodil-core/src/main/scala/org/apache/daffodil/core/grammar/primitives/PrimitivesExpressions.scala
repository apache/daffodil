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

package org.apache.daffodil.core.grammar.primitives

import org.apache.daffodil.core.compiler.ForParser
import org.apache.daffodil.core.dsom.DFDLNewVariableInstance
import org.apache.daffodil.core.dsom.DFDLSetVariable
import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.dsom.ExpressionCompilers
import org.apache.daffodil.core.dsom._
import org.apache.daffodil.core.grammar._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.Found
import org.apache.daffodil.lib.schema.annotation.props.PropertyLookupResult
import org.apache.daffodil.lib.schema.annotation.props.gen.FailureType
import org.apache.daffodil.lib.schema.annotation.props.gen.VariableDirection
import org.apache.daffodil.lib.xml.GlobalQName
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dsom._
import org.apache.daffodil.runtime1.processors.parsers.AssertExpressionEvaluationParser
import org.apache.daffodil.runtime1.processors.parsers.AssertPatternParser
import org.apache.daffodil.runtime1.processors.parsers.IVCParser
import org.apache.daffodil.runtime1.processors.parsers.InitiatedContentDiscrimChoiceAndIndexGreaterThanMinParser
import org.apache.daffodil.runtime1.processors.parsers.InitiatedContentDiscrimChoiceOnlyOnFirstIndexParser
import org.apache.daffodil.runtime1.processors.parsers.InitiatedContentDiscrimChoiceParser
import org.apache.daffodil.runtime1.processors.parsers.InitiatedContentDiscrimOnIndexGreaterThanMinParser
import org.apache.daffodil.runtime1.processors.parsers.NadaParser
import org.apache.daffodil.runtime1.processors.parsers.NewVariableInstanceEndParser
import org.apache.daffodil.runtime1.processors.parsers.NewVariableInstanceStartParser
import org.apache.daffodil.runtime1.processors.parsers.SetVariableParser
import org.apache.daffodil.runtime1.processors.parsers.TypeValueCalcParser
import org.apache.daffodil.runtime1.processors.parsers.{ Parser => DaffodilParser }
import org.apache.daffodil.runtime1.processors.unparsers.{ Unparser => DaffodilUnparser }
import org.apache.daffodil.unparsers.runtime1.NadaUnparser
import org.apache.daffodil.unparsers.runtime1.NewVariableInstanceEndUnparser
import org.apache.daffodil.unparsers.runtime1.NewVariableInstanceStartUnparser
import org.apache.daffodil.unparsers.runtime1.SetVariableUnparser
import org.apache.daffodil.unparsers.runtime1.TypeValueCalcUnparser

abstract class AssertBase(
  decl: AnnotatedSchemaComponent,
  exprWithBraces: String,
  namespacesForNamespaceResolution: scala.xml.NamespaceBinding,
  scWherePropertyWasLocated: AnnotatedSchemaComponent,
  msgOpt: Option[String],
  discrim: Boolean, // are we a discriminator or not.
  assertKindName: String,
  failureType: FailureType,
) extends ExpressionEvaluatorBase(scWherePropertyWasLocated) {

  def this(
    decl: AnnotatedSchemaComponent,
    foundProp: Found,
    msgOpt: Option[String],
    discrim: Boolean, // are we a discriminator or not.
    assertKindName: String,
    failureType: FailureType,
  ) =
    this(
      decl,
      foundProp.value,
      foundProp.location.namespaces,
      decl,
      msgOpt,
      discrim,
      assertKindName,
      failureType,
    )

  override val baseName = assertKindName
  override lazy val exprText = exprWithBraces
  override lazy val exprNamespaces = namespacesForNamespaceResolution
  override lazy val exprComponent = scWherePropertyWasLocated
  override def nodeKind = NodeInfo.Boolean

  override val forWhat = ForParser

  lazy val msgExpr = {
    if (msgOpt.isDefined) {
      ExpressionCompilers.String.compileExpression(
        qn,
        NodeInfo.String,
        msgOpt.get,
        exprNamespaces,
        exprComponent.dpathCompileInfo,
        false,
        this,
        exprComponent.dpathCompileInfo,
      )
    } else {
      val typeString = if (discrim) "Discriminator" else "Assertion"
      val defaultMessage = s"$typeString expression failed: $exprWithBraces"
      new ConstantExpression[String](qn, NodeInfo.String, defaultMessage)
    }
  }

  lazy val parser: DaffodilParser =
    new AssertExpressionEvaluationParser(msgExpr, discrim, decl.runtimeData, expr, failureType)

  override def unparser: DaffodilUnparser = hasNoUnparser

}

abstract class AssertBooleanPrimBase(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase,
  discrim: Boolean, // are we a discriminator or not.
  assertKindName: String,
) extends AssertBase(
    decl,
    Found(stmt.testTxt, stmt, "test", false),
    stmt.messageAttrib,
    discrim,
    assertKindName,
    stmt.failureType,
  )

case class AssertBooleanPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase)
  extends AssertBooleanPrimBase(decl, stmt, false, "assert") {}

case class DiscriminatorBooleanPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase)
  extends AssertBooleanPrimBase(decl, stmt, true, "discriminator")

// TODO: performance wise, initiated content is supposed to be faster
// than evaluating an expression. There should be a better way to say
// "resolve this point of uncertainty" without having to introduce
// an XPath evaluator that runs fn:true() expression.
case class InitiatedContent(mg: ModelGroup, t: Term) extends Terminal(t, true) {

  override val forWhat = ForParser

  override def parser = {
    // Each of the parsers that this could create always appear immediately
    // after an initiator parser. So they can all assume that the previous
    // initiator was successfully parsed and thus resolve points of uncertainty
    // appropriately

    t match {
      case eb: ElementBase if eb.isArray => {
        (mg, eb.optPoUMinOccurs) match {
          case (sq: SequenceTermBase, Some(min)) => {
            // This is an array of elements inside a sequence with initiated
            // content. There is no PoU if occursIndex <= min. All elements
            // after that have a PoU, which are resolved by this parser. Note
            // that min here is the value of optPoUMinOccurs, which is slightly
            // different than value of minOccurs, e.g. when occursCountKind="parsed"
            // this value is zero rather than the value of minOccurs.
            new InitiatedContentDiscrimOnIndexGreaterThanMinParser(min, eb.erd)
          }
          case (ch: ChoiceTermBase, None) => {
            // This branch of a choice is a fixed number of array elements.
            // Because they are fixed, there is no PoU associated with the
            // elements, only the choice. This must resolve the choice PoU only
            // the first time so as not to resolve any other PoUs.
            new InitiatedContentDiscrimChoiceOnlyOnFirstIndexParser(eb.erd)
          }
          case (ch: ChoiceTermBase, Some(min)) => {
            // This branch of a choice is for an array of elements with a
            // minimum number of occurrences. When occursIndex <= min, there
            // are no PoU's for the array elements, so this parser only
            // resolves PoU's once that index has been reached. However, there
            // is still a PoU for the choice, which must only be resolved once
            // when we parsed the first array element. Note that min here is
            // the value of optPoUMinOccurs, which is slightly different than
            // the value of minOccurs, e.g. when occursCountKind="parsed" this
            // value is zero rather than the value of minOccurs.
            new InitiatedContentDiscrimChoiceAndIndexGreaterThanMinParser(min, eb.erd)
          }
          case x => Assert.invariantFailed("Guard should exclude this case: " + x)
        }
      }
      case _ => new InitiatedContentDiscrimChoiceParser(t.termRuntimeData)
    }
  }

  override def unparser = hasNoUnparser
}

case class SetVariable(stmt: DFDLSetVariable, override val term: Term)
  extends ExpressionEvaluatorBase(stmt.annotatedSC) {

  val baseName = "SetVariable[" + stmt.varQName.local + "]"

  override lazy val exprText = stmt.value
  override lazy val exprNamespaces = stmt.xml.scope
  override lazy val exprComponent = stmt

  override lazy val nodeKind = stmt.defv.primType

  lazy val parser: DaffodilParser = {
    if (stmt.defv.runtimeData.direction == VariableDirection.UnparseOnly)
      new NadaParser(stmt.defv.runtimeData)
    else
      new SetVariableParser(expr, stmt.defv.runtimeData, term.termRuntimeData)
  }

  override lazy val unparser: DaffodilUnparser = {
    if (stmt.defv.runtimeData.direction == VariableDirection.ParseOnly)
      new NadaUnparser(stmt.defv.runtimeData)
    else
      new SetVariableUnparser(expr, stmt.defv.runtimeData, stmt.nonTermRuntimeData)
  }
}

abstract class NewVariableInstanceBase(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLNewVariableInstance,
) extends Terminal(decl, true) {}

case class NewVariableInstanceStart(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLNewVariableInstance,
  override val term: Term,
) extends NewVariableInstanceBase(decl, stmt) {

  lazy val parser: DaffodilParser = {
    if (stmt.defv.runtimeData.direction == VariableDirection.UnparseOnly)
      new NadaParser(stmt.variableRuntimeData)
    else
      new NewVariableInstanceStartParser(stmt.variableRuntimeData, term.termRuntimeData)
  }

  override lazy val unparser: DaffodilUnparser = {
    if (stmt.defv.runtimeData.direction == VariableDirection.ParseOnly)
      new NadaUnparser(stmt.variableRuntimeData)
    else
      new NewVariableInstanceStartUnparser(stmt.variableRuntimeData, term.termRuntimeData)
  }
}

case class NewVariableInstanceEnd(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLNewVariableInstance,
  override val term: Term,
) extends NewVariableInstanceBase(decl, stmt) {

  lazy val parser: DaffodilParser = {
    if (stmt.defv.runtimeData.direction == VariableDirection.UnparseOnly)
      new NadaParser(stmt.variableRuntimeData)
    else
      new NewVariableInstanceEndParser(stmt.variableRuntimeData, term.termRuntimeData)
  }

  override lazy val unparser: DaffodilUnparser = {
    if (stmt.defv.runtimeData.direction == VariableDirection.ParseOnly)
      new NadaUnparser(stmt.variableRuntimeData)
    else
      new NewVariableInstanceEndUnparser(stmt.variableRuntimeData, term.termRuntimeData)
  }
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

  protected def qn = GlobalQName(Some("daf"), baseName, XMLUtils.dafintURI)

  lazy val expr = LV('expr) {
    ExpressionCompilers.AnyRef.compileExpression(
      qn,
      nodeKind,
      exprText,
      exprNamespaces,
      exprComponent.dpathCompileInfo,
      false,
      this,
      exprComponent.dpathCompileInfo,
    )
  }.value
}

abstract class ValueCalcBase(e: ElementBase, property: PropertyLookupResult)
  extends ExpressionEvaluatorBase(e) {

  override lazy val exprText = exprProp.value
  override lazy val exprNamespaces = exprProp.location.namespaces
  override lazy val exprComponent = exprProp.location.asInstanceOf[SchemaComponent]

  lazy val pt = e.primType // .typeRuntimeData
  override lazy val nodeKind = pt
  lazy val ptn = pt.name

  lazy val exprProp = property.asInstanceOf[Found]

}

case class InputValueCalc(e: ElementBase, property: PropertyLookupResult)
  extends ValueCalcBase(e, property) {

  override def baseName = "inputValueCalc"

  override lazy val parser: DaffodilParser = {
    new IVCParser(expr, e.elementRuntimeData)
  }

  override lazy val unparser = Assert.usageError("Not to be called on InputValueCalc class.")
}

case class TypeValueCalc(e: ElementBase) extends Terminal(e, e.hasRepType) {

  private lazy val simpleTypeDefBase = e.simpleType.asInstanceOf[SimpleTypeDefBase]
  private lazy val typeCalculator = simpleTypeDefBase.optTypeCalculator.get
  private lazy val repTypeRuntimeData =
    simpleTypeDefBase.optRepTypeElement.get.elementRuntimeData
  private lazy val repTypeParser =
    simpleTypeDefBase.optRepTypeElement.get.enclosedElement.parser
  private lazy val repTypeUnparser =
    simpleTypeDefBase.optRepTypeElement.get.enclosedElement.unparser

  override lazy val parser: DaffodilParser = {
    new TypeValueCalcParser(
      typeCalculator,
      repTypeParser,
      e.elementRuntimeData,
      repTypeRuntimeData,
    )
  }
  override lazy val unparser: DaffodilUnparser = {
    new TypeValueCalcUnparser(
      typeCalculator,
      repTypeUnparser,
      e.elementRuntimeData,
      repTypeRuntimeData,
    )
  }

}

abstract class AssertPatternPrimBase(decl: Term, stmt: DFDLAssertionBase, discrim: Boolean)
  extends ExpressionEvaluatorBase(decl) {

  override val baseName = if (discrim) "Discriminator" else "Assert"
  override lazy val exprText = stmt.messageAttrib.get
  override lazy val exprNamespaces = decl.namespaces
  override lazy val exprComponent = decl

  override def nodeKind = NodeInfo.String

  lazy val testPattern = {
    PatternChecker.checkPattern(stmt.testTxt, decl)
    stmt.testTxt
  }

  lazy val msgExpr =
    if (stmt.messageAttrib.isDefined) {
      expr
    } else {
      val typeString = if (discrim) "Discriminator" else "Assertion"
      val defaultMessage = s"$typeString pattern failed: $testPattern"
      new ConstantExpression[String](qn, NodeInfo.String, defaultMessage)
    }

  override val forWhat = ForParser

  lazy val parser: DaffodilParser = new AssertPatternParser(
    decl.termRuntimeData,
    discrim,
    testPattern,
    msgExpr,
    stmt.failureType,
  )

  override def unparser: DaffodilUnparser =
    Assert.invariantFailed("should not request unparser for asserts/discriminators")
}

case class AssertPatternPrim(override val term: Term, stmt: DFDLAssert)
  extends AssertPatternPrimBase(term, stmt, false)

case class DiscriminatorPatternPrim(override val term: Term, stmt: DFDLDiscriminator)
  extends AssertPatternPrimBase(term, stmt, true)
