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

package org.apache.daffodil.core.dsom

import scala.xml.Comment
import scala.xml.Elem
import scala.xml.Node
import scala.xml.Text

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.WarnID

object GlobalGroupDef {

  def apply(defXML: Node, schemaDocument: SchemaDocument) = {
    val trimmedXml = scala.xml.Utility.trim(defXML)
    trimmedXml match {
      case Elem(_, "group", _, _, contents @ _*) => {
        val list = contents.collect {
          case groupXML @ Elem(_, "sequence", _, _, _*) =>
            GlobalSequenceGroupDef(defXML, groupXML, schemaDocument)
          case groupXML @ Elem(_, "choice", _, _, _*) =>
            GlobalChoiceGroupDef(defXML, groupXML, schemaDocument)
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
trait GroupDefLike extends AnnotatedSchemaComponent with ProvidesDFDLStatementMixin {

  /**
   * Not named groupMembers to avoid confusion with ModelGroup.groupMembers.
   *
   * This is not necessarily a Term subclass, it is shared by global group definitions
   * which are not terms. Non-terms don't deal with sharing the members.
   * So this just provides them for sharing (as appropriate) by shared Terms.
   */
  final def groupMembersNotShared = groupMembersDef

  def xmlChildren: Seq[Node]

  private lazy val goodXmlChildren = LV(Symbol("goodXMLChildren")) {
    xmlChildren.flatMap { removeNonInteresting(_) }
  }.value

  /** Returns the group members that are elements or model groups. */
  protected lazy val groupMembersDef: Seq[Term] = LV(Symbol("groupMembers")) {
    computeGroupMembers()
  }.value

  // override in Choice
  protected def computeGroupMembers(): Seq[Term] = {
    val positions =
      List.range(1, goodXmlChildren.length + 1) // range is exclusive on 2nd arg. So +1.
    val pairs = goodXmlChildren.zip(positions)
    val members = pairs.map { case (n, i) =>
      val t = TermFactory(n, this, i)
      t
    }
    members
  }

  /**
   * XML is full of uninteresting text nodes. We just want the element children, not all children.
   */
  private def removeNonInteresting(child: Node) = {
    val childElem: Option[Node] = child match {
      case _: Text => None
      case _: Comment => None
      case Elem(_, "annotation", _, _, _*) => None
      case _ => Some(child)
    }
    childElem
  }
}

/**
 * Global Group Defs carry annotations that are combined with those
 * of the corresponding group reference that refers to them.
 *
 * These are not carried on the xs:group element itself, but the xs:sequence
 * or xs:choice XML child. When we refer to the annotations on a global group
 * definition, we are referring to the annotations on the xs:sequence or xs:choice.
 */
sealed abstract class GlobalGroupDef(
  defXML: Node,
  groupXML: Node,
  schemaDocumentArg: SchemaDocument
) extends AnnotatedSchemaComponentImpl(groupXML, schemaDocumentArg)
  with GroupDefLike
  with GlobalNonElementComponentMixin
  with NestingLexicalMixin
  with ResolvesLocalProperties // for dfdl:choiceBranchKey
  {

  def groupMembers = {
    validateChoiceBranchKey()
    checkForGroupDefAnnotations()
    this.groupMembersNotShared
  }

  private def validateChoiceBranchKey(): Unit = {
    // Ensure the model group of a global group def do not define choiceBranchKey.
    val found = findPropertyOption("choiceBranchKey")
    if (found.isDefined) {
      SDE(
        "dfdl:choiceBranchKey cannot be specified on the choice/sequence child of a global group definition"
      )
    }
  }

  private def checkForGroupDefAnnotations(): Unit = {
    // Ensure that the group definition itself does not have any annotations.
    // Annotations should only be on group references or children of the group
    // definition
    val dais = getDFDLAppinfos(defXML \ "annotation" \ "appinfo")
    if (dais.nonEmpty)
      SDW(
        WarnID.InvalidAnnotationPoint,
        "Annotations placed directly on a group definition will be ignored by DFDL. Any annotation expected to be processed by DFDL should instead be placed on the group reference, sequence or choice."
      )
  }

  final override lazy val name = defXML
    .attribute("name")
    .map { _.text }
    .getOrElse(Assert.invariantFailed("Global group def without name attribute."))

  override def optReferredToComponent = None
}

object GlobalSequenceGroupDef {
  def apply(defXMLArg: Node, seqXML: Node, schemaDocument: SchemaDocument) = {
    val gsgd = new GlobalSequenceGroupDef(defXMLArg, seqXML, schemaDocument)
    gsgd.initialize()
    gsgd
  }
}

final class GlobalSequenceGroupDef private (
  defXMLArg: Node,
  seqXML: Node,
  schemaDocument: SchemaDocument
) extends GlobalGroupDef(defXMLArg, seqXML, schemaDocument)
  with SequenceDefMixin {

  requiredEvaluationsIfActivated(checkGroupDefIsNotHiddenSequence)

  private lazy val checkGroupDefIsNotHiddenSequence: Unit = {
    if (hiddenGroupRefOption.isDefined) {
      SDE("the model group of a group definition cannot be a sequence with dfdl:hiddenGroupRef")
    }
  }
}

object GlobalChoiceGroupDef {
  def apply(defXMLArg: Node, xml: Node, schemaDocument: SchemaDocument) = {
    val gsgd = new GlobalChoiceGroupDef(defXMLArg, xml, schemaDocument)
    gsgd.initialize()
    gsgd
  }
}
final class GlobalChoiceGroupDef private (
  defXMLArg: Node,
  choiceXML: Node,
  schemaDocument: SchemaDocument
) extends GlobalGroupDef(defXMLArg, choiceXML, schemaDocument)
  with ChoiceDefMixin
