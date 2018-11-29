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
import scala.xml._
import org.apache.daffodil.xml.HasRefMixin
import org.apache.daffodil.schema.annotation.props.NotFound

trait GroupRef { self: ModelGroup =>

  final def asModelGroup: ModelGroup = self

  def groupDef: GlobalGroupDef

  final def referredToComponent = groupDef

  final override lazy val optReferredToComponent = Some(referredToComponent)

  final override lazy val groupMembers = LV('groupMembers) {
    groupDef.groupMembers
  }.value

  override protected def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:group>{ contents @ _* }</dfdl:group> => Some(new DFDLGroup(node, this))
      case _ => annotationFactoryForDFDLStatement(node, self)
    }
  }

  override protected final def emptyFormatFactory: DFDLFormatAnnotation = new DFDLGroup(newDFDLAnnotationXML("group"), this)

  override protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLAnnotation]

}

/**
 * A GroupRefFactory (group reference) is an indirection to
 * factories to create a SequenceGroupRef, or ChoiceGroupRef.
 *
 * The refXMLArg is the xml for the group reference.
 *
 * This factory exists in order to make error messages refer to the
 * right part of the schema.
 */
final class GroupRefFactory(refXMLArg: Node, val refLexicalParent: SchemaComponent, position: Int, isHidden: Boolean)
  extends SchemaComponentFactory(refXMLArg, refLexicalParent.schemaDocument)
  with HasRefMixin {

  final def qname = this.refQName

  lazy val groupRef = LV('groupRef) {
    val gdefFactory = parent.schemaSet.getGlobalGroupDef(qname).getOrElse {
      SDE("Referenced group definition not found: %s", this.ref)
    }
    val (gref, _) = gdefFactory.forGroupRef(refXMLArg, refLexicalParent, position, isHidden)
    gref
  }.value

}

final class SequenceGroupRef(
  globalGroupDef: => GlobalSequenceGroupDef,
  refXML: Node,
  refLexicalParent: SchemaComponent,
  positionArg: Int,
  isHiddenArg: Boolean)
  extends SequenceGroupTermBase(refXML, refLexicalParent, positionArg)
  with GroupRef {

  override def isHidden = isHiddenArg

  private lazy val sgd = groupDef.asInstanceOf[GlobalSequenceGroupDef]

  override def xmlChildren = sgd.xmlChildren

  override def apparentXMLChildren = sgd.apparentXMLChildren

  private val nf = NotFound(Nil, Nil, "hiddenGroupRef")

  override def hiddenGroupRefOption = nf

  override lazy val groupDef = LV('groupDef) { globalGroupDef }.value

}

final class ChoiceGroupRef(
  globalGroupDef: => GlobalChoiceGroupDef,
  refXML: Node,
  refLexicalParent: SchemaComponent,
  positionArg: Int,
  isHiddenArg: Boolean)
  extends ChoiceTermBase(refXML, refLexicalParent, positionArg)
  with GroupRef {

  requiredEvaluations(groupDef)

  override def isHidden = isHiddenArg

  override lazy val groupDef = globalGroupDef

  private lazy val cgd = groupDef.asInstanceOf[GlobalChoiceGroupDef]

  override lazy val xmlChildren = cgd.xmlChildren

}
