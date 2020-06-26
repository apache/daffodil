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

package org.apache.daffodil.dsom

import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import org.apache.daffodil.grammar.EmptyGram
import org.apache.daffodil.processors._
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.xml.GlobalQName
import org.apache.daffodil.xml.QName
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.grammar.primitives.{ SetVariable, NewVariableInstanceStart, NewVariableInstanceEnd }
import org.apache.daffodil.schema.annotation.props.Found

class DFDLDefineVariable(node: Node, doc: SchemaDocument)
  extends DFDLDefiningAnnotation(node, doc) {

  requiredEvaluationsAlways(variableRuntimeData.preSerialization)

  final lazy val gram = EmptyGram // has to have because statements have parsers layed in by the grammar.

  private lazy val typeQNameString = getAttributeOption("type").getOrElse("xs:string")

  final lazy val external = getAttributeOption("external").map { _.toBoolean }.getOrElse(false)

  private lazy val defaultValueAsAttribute = getAttributeOption("defaultValue")
  private lazy val defaultValueAsElement = node.child.text.trim

  final lazy val defaultValue = (defaultValueAsAttribute, defaultValueAsElement) match {
    case (None, "") => None
    case (None, str) => Some(str)
    case (Some(str), "") => Some(str)
    case (Some(str), v) => schemaDefinitionError("Default value of variable was supplied both as attribute and element value: %s", node.toString)
  }

  final lazy val typeQName = {
    val eQN = QName.resolveRef(typeQNameString, namespaces, tunable.unqualifiedPathStepPolicy)
    val res = eQN.recover {
      case _: Throwable =>
        SDE("Variables must have primitive types. Type is '%s'.", typeQNameString)
    }.get
    res
  }

  final lazy val primType = PrimType.fromNameString(typeQName.local).getOrElse(
    this.SDE("Variables must have primitive type. Type was '%s'.", typeQName.toPrettyString))

  final def createVariableInstance = variableRuntimeData.createVariableInstance

  final override lazy val runtimeData = variableRuntimeData

  lazy val maybeDefaultValueExpr = {
    val compilationTargetType = primType
    val qn = this.qNameForProperty("defaultValue", XMLUtils.dafintURI)
    val defaultValExpr = defaultValue.map { e =>
      ExpressionCompilers.AnyRef.compileProperty(qn, compilationTargetType, Found(e, this.dpathCompileInfo, "defaultValue", false), this, dpathCompileInfo)
    }

    Maybe.toMaybe(defaultValExpr)
  }

  final lazy val variableRuntimeData = {
    val vrd = new VariableRuntimeData(
      this.schemaFileLocation,
      this.diagnosticDebugName,
      this.path,
      this.namespaces,
      this.external,
      maybeDefaultValueExpr,
      this.typeQName,
      this.namedQName.asInstanceOf[GlobalQName],
      this.primType,
      this.tunable.unqualifiedPathStepPolicy)
    vrd
  }
}

abstract class VariableReference(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl) {

  final lazy val ref = getAttributeRequired("ref")
  final lazy val varQName = resolveQName(ref)

  def variableRuntimeData = defv.runtimeData

  final lazy val defv = decl.schemaSet.getDefineVariable(varQName).getOrElse(
    this.schemaDefinitionError("Variable definition not found: %s", ref))
}

final class DFDLNewVariableInstance(node: Node, decl: AnnotatedSchemaComponent)
  extends VariableReference(node, decl) // with NewVariableInstance_AnnotationMixin
  {
  // newVariableInstance only allowed within group ref, sequence, or choice
  decl match {
    case gr: GroupRef =>
    case grl: GroupDefLike =>
    case _ => decl.SDE("newVariableInstance may only be used on group reference, sequence or choice")
  }

  private lazy val attrValue = getAttributeOption("value")

  private lazy val <dfdl:setVariable>{ eltChildren @ _* }</dfdl:setVariable> = node

  private lazy val eltValue = eltChildren.text.trim

  private lazy val defaultValueAsAttribute = getAttributeOption("defaultValue")
  private lazy val defaultValueAsElement = node.child.text.trim

  final lazy val defaultValue = (defaultValueAsAttribute, defaultValueAsElement) match {
    case (None, "") => defv.defaultValue
    case (None, str) => Some(str)
    case (Some(str), "") => Some(str)
    case (Some(str), v) => schemaDefinitionError("Default value of variable was supplied both as attribute and element value: %s", node.toString)
  }

  final lazy val value = (attrValue, eltValue) match {
    case (None, v) if (v != "") => v
    case (Some(v), "") => v
    case (Some(v), ev) if (ev != "") => decl.SDE("Cannot have both a value attribute and an element value: %s", node)
    case (None, "") => decl.SDE("Must have either a value attribute or an element value: %s", node)
  }

  lazy val maybeDefaultValueExpr = {
    val compilationTargetType = defv.primType
    val qn = this.qNameForProperty("defaultValue", XMLUtils.dafintURI)
    val defaultValExpr = defaultValue.map { e =>
      ExpressionCompilers.AnyRef.compileProperty(qn, compilationTargetType, Found(e, this.dpathCompileInfo, "defaultValue", false), this, dpathCompileInfo)
    }

    Maybe.toMaybe(defaultValExpr)
  }

  /* Need to override variableRuntimeData so that defaultValues
   * are read from newVariableInstance instead of the original
   * variable definition. Also allows diagnostic messages to
   * point to this location instead of the original definition
   */
  final override lazy val variableRuntimeData = {
    val vrd = new VariableRuntimeData(
      this.schemaFileLocation,
      this.diagnosticDebugName,
      this.path,
      this.namespaces,
      defv.external,
      maybeDefaultValueExpr,
      defv.typeQName,
      defv.namedQName.asInstanceOf[GlobalQName],
      defv.primType,
      this.tunable.unqualifiedPathStepPolicy)
    vrd
  }

  final def gram(term: Term) = LV('gram) {
    NewVariableInstanceStart(decl, this)
  }.value

  final def endGram(term: Term) = LV('endGram) {
    NewVariableInstanceEnd(decl, this)
  }.value
}

final class DFDLSetVariable(node: Node, decl: AnnotatedSchemaComponent)
  extends VariableReference(node, decl) // with SetVariable_AnnotationMixin
  {
  private lazy val attrValue = getAttributeOption("value")
  private lazy val <dfdl:setVariable>{ eltChildren @ _* }</dfdl:setVariable> = node
  private lazy val eltValue = eltChildren.text.trim
  final lazy val value = (attrValue, eltValue) match {
    case (None, v) if (v != "") => v
    case (Some(v), "") => v
    case (Some(v), ev) if (ev != "") => decl.SDE("Cannot have both a value attribute and an element value: %s", node)
    case (None, "") => decl.SDE("Must have either a value attribute or an element value: %s", node)
  }

  final def gram(term: Term) = LV('gram) {
    SetVariable(this)
  }.value
}
