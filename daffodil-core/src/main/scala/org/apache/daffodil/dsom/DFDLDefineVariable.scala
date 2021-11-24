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
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.xml.QName
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.grammar.primitives.NewVariableInstanceEnd
import org.apache.daffodil.grammar.primitives.NewVariableInstanceStart
import org.apache.daffodil.grammar.primitives.SetVariable
import org.apache.daffodil.runtime1.DFDLDefineVariableRuntime1Mixin
import org.apache.daffodil.runtime1.DFDLNewVariableInstanceRuntime1Mixin
import org.apache.daffodil.runtime1.DFDLSetVariableRuntime1Mixin
import org.apache.daffodil.runtime1.VariableReferenceRuntime1Mixin
import org.apache.daffodil.schema.annotation.props.gen.VariableDirection
import org.apache.daffodil.xml.NS

object DFDLDefineVariable {
  def apply(node: Node, doc: SchemaDocument) = {
    val dv = new DFDLDefineVariable(node, doc)
    dv.initialize()
    dv
  }

  def apply(node: Node, doc: SchemaDocument, nsURI: String) = {
    val dv = new DFDLDefineVariable(node, doc) {
      override lazy val namespace = NS(nsURI)
      override lazy val targetNamespace = NS(nsURI)
    }
    dv.initialize()
    dv
  }
}

class DFDLDefineVariable private (node: Node, doc: SchemaDocument)
  extends DFDLDefiningAnnotation(node, doc)
  with DFDLDefineVariableRuntime1Mixin{

  final lazy val gram = EmptyGram // has to have because statements have parsers layed in by the grammar.

  private lazy val typeQNameString = getAttributeOption("type").getOrElse("xs:string")

  final lazy val external = getAttributeOption("external").map { _.toBoolean }.getOrElse(false)

  final lazy val direction = {
    val directionStr = getAttributeOption(XMLUtils.DFDLX_NAMESPACE, "direction").getOrElse("both")
    VariableDirection(directionStr, this)
  }

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
}

sealed abstract class VariableReference(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl)
  with VariableReferenceRuntime1Mixin {

  requiredEvaluationsIfActivated(varQName)

  final lazy val ref = getAttributeRequired("ref")
  final lazy val varQName = resolveQName(ref)

  final lazy val defv = decl.schemaSet.getDefineVariable(varQName).getOrElse(
    this.schemaDefinitionError("Variable definition not found: %s", ref))
}

final class DFDLNewVariableInstance(node: Node, decl: AnnotatedSchemaComponent)
  extends VariableReference(node, decl)
    with DFDLNewVariableInstanceRuntime1Mixin  {

  requiredEvaluationsIfActivated(checks)

  private lazy val checks = {
    // newVariableInstance only allowed within group ref, sequence, or choice
    decl match {
      case gr: GroupRef =>
      case grl: GroupDefLike =>
      case _ => decl.SDE("newVariableInstance may only be used on group reference, sequence or choice")
    }
  }

  private lazy val attrValue = getAttributeOption("value")

  private lazy val <dfdl:newVariableInstance>{ eltChildren @ _* }</dfdl:newVariableInstance> = node

  private lazy val eltValue = eltChildren.text.trim

  private lazy val defaultValueAsAttribute = getAttributeOption("defaultValue")
  private lazy val defaultValueAsElement = node.child.text.trim

  final lazy val defaultValue = (defaultValueAsAttribute, defaultValueAsElement) match {
    case (None, "") => None
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

  final def gram(term: Term) = LV('gram) {
    NewVariableInstanceStart(decl, this, term)
  }.value

  final def endGram(term: Term) = LV('endGram) {
    NewVariableInstanceEnd(decl, this, term)
  }.value
}

final class DFDLSetVariable(node: Node, decl: AnnotatedSchemaComponent)
  extends VariableReference(node, decl)
  with DFDLSetVariableRuntime1Mixin {
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
    SetVariable(this, term)
  }.value
}
