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
import org.apache.daffodil.exceptions.Assert
import scala.xml.Text
import scala.xml.Comment

final class GlobalGroupDefFactory(defXML: Node, schemaDocument: SchemaDocument)
  extends SchemaComponentFactory(defXML, schemaDocument)
  with GlobalNonElementComponentMixin {

  def forGroupRef(refXML: Node, refLexicalParent: SchemaComponent, position: Int,
    isHidden: Boolean): (GroupRef, GlobalGroupDef) = {
    val trimmedXml = scala.xml.Utility.trim(defXML)
    trimmedXml match {
      case <group>{ contents @ _* }</group> => {
        val list = contents.collect {
          case groupXML @ <sequence>{ _* }</sequence> => {
            lazy val gref: SequenceGroupRef = new SequenceGroupRef(gdef, refXML, refLexicalParent, position, isHidden)
            lazy val gdef: GlobalSequenceGroupDef = new GlobalSequenceGroupDef(defXML, groupXML, schemaDocument, gref, this)
            gref.groupDef
            gdef.groupRef
            (gref, gdef)
          }
          case groupXML @ <choice>{ _* }</choice> =>
            lazy val gref: ChoiceGroupRef = new ChoiceGroupRef(gdef, refXML, refLexicalParent, position, isHidden)
            lazy val gdef: GlobalChoiceGroupDef = new GlobalChoiceGroupDef(defXML, groupXML, schemaDocument, gref, this)
            gref.groupDef
            gdef.groupRef
            (gref, gdef)
        }
        val res = list(0)
        res
      }
      case _ => Assert.invariantFailed("not a group")
    }
  }
}

/**
 * Common concepts for components that define groups.
 *
 * This includes both global group definitions, and local sequence
 * and choice groups.
 */
trait GroupDefLike
  extends AnnotatedSchemaComponent
  with ProvidesDFDLStatementMixin {

  /**
   * Not named groupMembers to avoid confusion with ModelGroup.groupMembers.
   *
   * This is not necessarily a Term subclass, it is shared by global group definitions
   * which are not terms. Non-terms don't deal with sharing the members.
   * So this just provides them for sharing (as appropriate) by shared Terms.
   */
  final def groupMembersNotShared = groupMembersDef

  def xmlChildren: Seq[Node]

  private lazy val goodXmlChildren = LV('goodXMLChildren) { xmlChildren.flatMap { removeNonInteresting(_) } }.value

  /** Returns the group members that are elements or model groups. */
  protected lazy val groupMembersDef: Seq[Term] = LV('groupMembers) {
    val positions = List.range(1, goodXmlChildren.length + 1) // range is exclusive on 2nd arg. So +1.
    val pairs = goodXmlChildren zip positions
    val members = pairs.map {
      case (n, i) =>
        val t = TermFactory(n, this, i)
        t
    }
    members
  }.value

  /**
   * XML is full of uninteresting text nodes. We just want the element children, not all children.
   */
  private def removeNonInteresting(child: Node) = {
    val childElem: Option[Node] = child match {
      case _: Text => None
      case _: Comment => None
      case <annotation>{ _* }</annotation> => None
      case _ => Some(child)
    }
    childElem
  }
}

/**
 * Global Group Defs carry annotations that are combined wiht those
 * of the corresponding group reference that refers to them.
 *
 * These are not carried on the xs:group element itself, but the xs:sequence
 * or xs:choice XML child. When we refer to the annotations on a global group
 * definition, we are referring to the annotations on the xs:sequence or xs:choice.
 */
sealed abstract class GlobalGroupDef(
  defXML: Node,
  groupXML: Node,
  schemaDocumentArg: SchemaDocument,
  grefArg: => GroupRef,
  override val factory: GlobalGroupDefFactory)
  extends AnnotatedSchemaComponentImpl(groupXML, schemaDocumentArg)
  with GroupDefLike
  with GlobalNonElementComponentMixin
  with NestingTraversesToReferenceMixin
  with ResolvesLocalProperties // for dfdl:choiceBranchKey
  {

  requiredEvaluations(validateChoiceBranchKey)

  lazy val groupRef = grefArg // once only

  def validateChoiceBranchKey(): Unit = {
    // Ensure the model group of a global group def do not define choiceBranchKey.
    val found = findPropertyOption("choiceBranchKey")
    if (found.isDefined) {
      SDE("dfdl:choiceBranchKey cannot be specified on the choice/sequence child of a global group definition")
    }
  }

  final override lazy val name = defXML.attribute("name").map { _.text }.getOrElse(
    Assert.invariantFailed("Global group def without name attribute."))

  final override protected def optReferredToComponent = None

  final override lazy val referringComponent = Some(groupRef.asModelGroup)
}

final class GlobalSequenceGroupDef(
  defXMLArg: Node, seqXML: Node, schemaDocument: SchemaDocument, gref: => SequenceGroupRef,
  factory: GlobalGroupDefFactory)
  extends GlobalGroupDef(defXMLArg, seqXML, schemaDocument, gref, factory)
  with SequenceDefMixin

final class GlobalChoiceGroupDef(
  defXMLArg: Node, choiceXML: Node, schemaDocument: SchemaDocument, gref: => ChoiceGroupRef,
  factory: GlobalGroupDefFactory)
  extends GlobalGroupDef(defXMLArg, choiceXML, schemaDocument, gref, factory)
  with ChoiceDefMixin {

}
