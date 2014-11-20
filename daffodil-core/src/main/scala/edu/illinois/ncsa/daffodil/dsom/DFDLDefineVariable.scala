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

import scala.collection.immutable.ListMap
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Utility
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.grammar.EmptyGram
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropertyMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeScheme_AnnotationMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dpath._
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.QName
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.processors.VariableUtils

class DFDLDefineVariable(node: Node, doc: SchemaDocument)
  extends DFDLDefiningAnnotation(node, doc) {
  lazy val gram = EmptyGram // has to have because statements have parsers layed in by the grammar.
  lazy val typeQNameString = getAttributeOption("type").getOrElse("xs:string")
  lazy val external = getAttributeOption("external").map { _.toBoolean }.getOrElse(false)
  lazy val defaultValueAsAttribute = getAttributeOption("defaultValue")
  lazy val defaultValueAsElement = node.child.text.trim
  lazy val defaultValue = (defaultValueAsAttribute, defaultValueAsElement) match {
    case (None, "") => None
    case (None, str) => Some(str)
    case (Some(str), "") => Some(str)
    case (Some(str), v) => schemaDefinitionError("Default value of variable was supplied both as attribute and element value: %s", node.toString)
  }

  @deprecated("Use real QNames system.", "2014-10-29")
  lazy val extName = expandedNCNameToQName

  @deprecated("Use real QNames system.", "2014-10-29")
  lazy val (typeURI, typeLocalName) = XMLUtils.QName(node.scope, typeQNameString, this)

  @deprecated("Use real QNames system.", "2014-10-29")
  lazy val extType = XMLUtils.expandedQName(typeURI, typeLocalName)

  lazy val typeQName = QName.resolveRef(typeQNameString, namespaces).getOrElse(
    SDE("Variables must have primitive types. Type is '%s'.", typeQNameString))

  lazy val primType = PrimType.fromNameString(typeQName.local).getOrElse(
    this.SDE("Variables must have primitive type. Type was '%s'.", typeQName.toPrettyString))
  lazy val newVariableInstance = VariableFactory.create(this, extName, extType, defaultValue, external, doc)

  // So that we can display the namespace information associated with
  // the variable when toString is called.
  override lazy val prettyName = Misc.getNameFromClass(this) + "(" + extName + ")"

  override lazy val runtimeData = variableRuntimeData

  lazy val variableRuntimeData = new VariableRuntimeData(
    this.schemaFileLocation,
    this.prettyName,
    this.path,
    this.namespaces,
    this.external,
    this.defaultValue,
    this.extName,
    this.extType,
    this.globalQName,
    this.primType)
}

class DFDLNewVariableInstance(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl) // with NewVariableInstance_AnnotationMixin 
  {
  requiredEvaluations(endGram)
  lazy val ref = getAttributeRequired("ref")
  lazy val defaultValue = getAttributeOption("defaultValue")

  lazy val gram: Gram = NewVariableInstanceStart(decl, this)
  lazy val endGram: Gram = NewVariableInstanceEnd(decl, this)

  lazy val (uri, localName) = XMLUtils.QName(decl.namespaces, ref, decl.schemaDocument)
  lazy val expName = XMLUtils.expandedQName(uri, localName)
  lazy val defv = decl.schemaSet.getDefineVariable(uri, localName).getOrElse(
    this.schemaDefinitionError("Variable not found: %s", ref))

  lazy val newVariableInstance = defv.newVariableInstance

}

class DFDLSetVariable(node: Node, decl: AnnotatedSchemaComponent)
  extends DFDLStatement(node, decl) // with SetVariable_AnnotationMixin 
  {
  lazy val ref = getAttributeRequired("ref")
  lazy val attrValue = getAttributeOption("value")
  lazy val <dfdl:setVariable>{ eltChildren @ _* }</dfdl:setVariable> = node
  lazy val eltValue = eltChildren.text.trim
  lazy val value = (attrValue, eltValue) match {
    case (None, v) if (v != "") => v
    case (Some(v), "") => v
    case (Some(v), ev) if (ev != "") => decl.SDE("Cannot have both a value attribute and an element value: %s", node)
    case (None, "") => decl.SDE("Must have either a value attribute or an element value: %s", node)
  }

  lazy val (uri, localName) = XMLUtils.QName(decl.namespaces, ref, decl.schemaDocument.runtimeData)
  lazy val defv = decl.schemaSet.getDefineVariable(uri, localName).getOrElse(
    schemaDefinitionError("Unknown variable: %s", ref))

  lazy val gram = gram_.value
  private val gram_ = LV('gram) {
    SetVariable(decl, this)
  }
}

