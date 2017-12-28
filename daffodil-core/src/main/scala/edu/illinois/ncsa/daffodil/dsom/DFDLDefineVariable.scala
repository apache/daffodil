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

import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import edu.illinois.ncsa.daffodil.grammar.EmptyGram
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.QName
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.grammar.primitives.SetVariable
import edu.illinois.ncsa.daffodil.grammar.primitives.NewVariableInstanceStart
import edu.illinois.ncsa.daffodil.grammar.primitives.NewVariableInstanceEnd
import edu.illinois.ncsa.daffodil.schema.annotation.props.Found

class DFDLDefineVariable(node: Node, doc: SchemaDocument)
  extends DFDLDefiningAnnotation(node, doc) {

  requiredEvaluations(variableRuntimeData.preSerialization)

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
    val eQN = QName.resolveRef(typeQNameString, namespaces, tunable)
    val res = eQN.recover {
      case _: Throwable =>
        SDE("Variables must have primitive types. Type is '%s'.", typeQNameString)
    }.get
    res
  }

  final lazy val primType = PrimType.fromNameString(typeQName.local).getOrElse(
    this.SDE("Variables must have primitive type. Type was '%s'.", typeQName.toPrettyString))

  final def newVariableInstance = variableRuntimeData.newVariableInstance

  final override lazy val runtimeData = variableRuntimeData

  lazy val maybeDefaultValueExpr = {
    val compilationTargetType = primType
    val qn = this.qNameForProperty("defaultValue", XMLUtils.dafintURI)
    val defaultValExpr = defaultValue.map { e =>
      ExpressionCompilers.AnyRef.compileProperty(qn, compilationTargetType, Found(e, this.dpathCompileInfo, "defaultValue", false), this)
    }

    Maybe.toMaybe(defaultValExpr)
  }

  final lazy val variableRuntimeData = new VariableRuntimeData(
    this.schemaFileLocation,
    this.diagnosticDebugName,
    this.path,
    this.namespaces,
    this.external,
    maybeDefaultValueExpr,
    this.typeQName,
    this.namedQName.asInstanceOf[GlobalQName],
    this.primType,
    this.tunable)
}

abstract class VariableReference(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl) {

  final lazy val ref = getAttributeRequired("ref")
  final lazy val varQName = resolveQName(ref)

  final def variableRuntimeData = defv.runtimeData

  final lazy val defv = decl.schemaSet.getDefineVariable(varQName).getOrElse(
    this.schemaDefinitionError("Variable definition not found: %s", ref))
}

final class DFDLNewVariableInstance(node: Node, decl: AnnotatedSchemaComponent)
  extends VariableReference(node, decl) // with NewVariableInstance_AnnotationMixin
  {
  requiredEvaluations(endGram)

  lazy val defaultValue = getAttributeOption("defaultValue")

  lazy val gram: Gram = NewVariableInstanceStart(decl, this)
  lazy val endGram: Gram = NewVariableInstanceEnd(decl, this)

  lazy val newVariableInstance = defv.newVariableInstance

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

  final def gram = LV('gram) {
    SetVariable(decl, this)
  }.value
}
