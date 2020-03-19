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

  final override protected lazy val groupMembersDef = LV('groupMembers) {
    groupDef.groupMembersNotShared
  }.value

  override protected def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:group>{ contents @ _* }</dfdl:group> => Some(new DFDLGroup(node, this))
      case _ => annotationFactoryForDFDLStatement(node, self)
    }
  }

  override protected final lazy val emptyFormatFactory: DFDLFormatAnnotation = new DFDLGroup(newDFDLAnnotationXML("group"), this)

  override protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLAnnotation]

}

/**
 * A GroupRefFactory (group reference) is an indirection to
 * create a SequenceGroupRef, or ChoiceGroupRef.
 *
 * The refXML is the xml for the group reference.
 *
 * This factory exists in order to make error messages refer to the
 * right part of the schema and to navigate the ref prior to creating
 * the object so that we can create a more specific kind of group ref that
 * knows if it is referring to a choice or a sequence.
 */
object GroupRefFactory {

  def apply(refXML: Node, refLexicalParent: SchemaComponent, position: Int, isHidden: Boolean) = {
    val f = new GroupRefFactory(refXML, refLexicalParent, position, isHidden)
    f.groupRef
  }
}

/**
 * This is a quasi schema component. Its purpose is simply to share a bunch of
 * mechanism that schema components inherit, in a hassle free manner.
 * (E.g., diagnostics mechanism that keeps track of file and line number). We
 * want to be able to issue those before we've created the actual SequenceGroupRef or
 * ChoiceGroupRef objects. Hence, a throwaway factory object.
 *
 * Private constructor insures it must be constructed by way of apply method of companion object.
 */
final class GroupRefFactory private (refXML: Node, val refLexicalParent: SchemaComponent, position: Int, isHidden: Boolean)
  extends SchemaComponentImpl(refXML, refLexicalParent)
  with NestingLexicalMixin
  with HasRefMixin {

  final def qname = this.refQName

  lazy val groupRef = {
    val gdef = refLexicalParent.schemaSet.getGlobalGroupDef(qname).getOrElse {
      SDE("Referenced group definition not found: %s", this.ref)
    }
    val gref = gdef match {
      case gd: GlobalSequenceGroupDef => new SequenceGroupRef(gd, refXML, refLexicalParent, position, isHidden)
      case gd: GlobalChoiceGroupDef => new ChoiceGroupRef(gd, refXML, refLexicalParent, position, isHidden)
    }
    gref
  }
}

final class SequenceGroupRef(
  globalGroupDefArg: => GlobalSequenceGroupDef,
  refXML: Node,
  refLexicalParent: SchemaComponent,
  positionArg: Int,
  isHiddenArg: Boolean)
  extends SequenceGroupTermBase(refXML, refLexicalParent, positionArg)
  with GroupRef {

  private lazy val globalGroupDef = globalGroupDefArg // once only

  override def isHidden = isHiddenArg
  override def isHiddenGroupRef = isHiddenArg

  private lazy val sgd = groupDef.asInstanceOf[GlobalSequenceGroupDef]

  override def xmlChildren = sgd.xmlChildren

  override def apparentXMLChildren = sgd.apparentXMLChildren

  private val nf = NotFound(Nil, Nil, "hiddenGroupRef")

  override def hiddenGroupRefOption = nf

  override lazy val groupDef = LV('groupDef) { globalGroupDef }.value

}

final class ChoiceGroupRef(
  globalGroupDefArg: GlobalChoiceGroupDef,
  refXML: Node,
  refLexicalParent: SchemaComponent,
  positionArg: Int,
  isHiddenArg: Boolean)
  extends ChoiceTermBase(refXML, Option(refLexicalParent), positionArg)
  with GroupRef {

  requiredEvaluationsIfActivated(groupDef)

  private lazy val globalGroupDef = globalGroupDefArg // once only

  override def isHidden = isHiddenArg
  override def isHiddenGroupRef = isHiddenArg

  override lazy val groupDef = globalGroupDef

  private lazy val cgd = groupDef.asInstanceOf[GlobalChoiceGroupDef]

  override lazy val xmlChildren = cgd.xmlChildren

}
