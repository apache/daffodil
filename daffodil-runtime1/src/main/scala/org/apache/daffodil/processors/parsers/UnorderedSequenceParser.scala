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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.processors.ModelGroupRuntimeData

class UnorderedSequenceParser(
  override val context: ModelGroupRuntimeData,
  sortOrder: Seq[(String, org.jdom2.Namespace)],
  scalarMembers: Seq[(String, String, org.jdom2.Namespace)],
  uoSeqParser: Parser)
  extends CombinatorParser(context) {

  override lazy val runtimeDependencies = Nil

  override def nom = "UnorderedSequence"
  override lazy val childProcessors = Seq(uoSeqParser)

  //  def sort(elt: org.jdom2.Element, pstate: PState): Unit = {
  //    val childrenDetached = elt.removeContent().toList.asInstanceOf[List[org.jdom2.Element]]
  //
  //        println("Children Detached: " + childrenDetached)
  //
  //    println("Sort Order: " + sortOrder)
  //    sortOrder.foreach {
  //      case (name, ns) => {
  //
  //        val newContent = childrenDetached.filter(child => {
  //          val infoSetElem = Infoset.newElement(child)
  //          val rd = infoSetElem.schemaComponent
  //          val isMatch = rd.name == name && rd.targetNamespace == ns
  //          isMatch
  //        })
  //
  //        println("New Content: " + newContent)
  //        if (newContent.size > 0)
  //          elt.addContent(newContent)
  //      }
  //    }
  //  }
  //
  //  def checkScalarsOccurExactlyOnce(elt: org.jdom2.Element, pstate: PState): Unit = {
  //    // Always occurs, does not depend on validation
  //    val children = elt.getChildren().toList.asInstanceOf[List[org.jdom2.Element]]
  //    scalarMembers.foreach {
  //      case (name, path, ns) => {
  //        val scalarChildren = children.filter(e => {
  //          e.getName() == name && e.getNamespace() == ns
  //        })
  //        val numScalarChildren = scalarChildren.length
  //        if (numScalarChildren > 1 || numScalarChildren == 0)
  //          pstate.SDE("%s is scalar but occurred %s times.\nPath: %s\nOffending children: %s",
  //            name, scalarChildren.length, path, scalarChildren.mkString(", "))
  //      }
  //    }
  //  }
  //
  //  /**
  //   * We want to check that we met he expected occurrence values
  //   * but we don't want to SDE, instead we issue a PE (ProcessingError).
  //   */
  //  def checkOccurrence(elt: org.jdom2.Element): Unit = {
  //    val childrenList = elt.getContent().toList.asInstanceOf[List[org.jdom2.Element]]
  //
  //    val erds =
  //      context.groupMembers.collect { case erd: ElementRuntimeData => erd }
  //    erds.foreach(erd => {
  //      val minOccurs = erd.minOccurs
  //      val maxOccurs = erd.maxOccurs
  //      val ns = erd.targetNamespace
  //      val name = erd.name
  //
  //      val children = childrenList.filter(c => {
  //        val childName = c.getName()
  //        val childNS = c.getNamespace()
  //        name == childName && ns == childNS
  //      })
  //
  //      val numChildren = children.length
  //
  //      minOccurs.foreach { minOccurs =>
  //        if (numChildren < minOccurs) {
  //          PE("UnorderedSequence.checkOccurrence - %s failed minOccurs check. Expected at least %s but found %s.",
  //            erd, minOccurs, numChildren)
  //        }
  //      }
  //      maxOccurs.foreach { maxOccurs =>
  //        if (numChildren > maxOccurs) {
  //          PE("UnorderedSequence.checkOccurrence - %s failed maxOccurs check. Expected at most %s but found %s.",
  //            erd, maxOccurs, numChildren)
  //        }
  //      }
  //    })
  //  }
  //
  //  def dropDefaulted(elt: org.jdom2.Element, pstate: PState): Unit = {
  //    // Always occurs, does not depend on validation
  //
  //    // RequiredElement, index in its array <= minOccurs
  //    // If empty rep, a required element that has a default value will be defaulted.
  //
  //    // Drop anything after the minOccurs that was defaulted.
  //
  //    // So we need the original minOccurs for each element.
  //    val childrenDetached = elt.removeContent().toList.asInstanceOf[List[org.jdom2.Element]]
  //    val erds = childrenDetached.map(c => {
  //      val infoSetElem = Infoset.newElement(c)
  //      val rd = infoSetElem.schemaComponent
  //      rd
  //    }).distinct
  //
  //    erds.foreach(erd => {
  //      val minOccurs = erd.minOccurs.get
  //      val ns = erd.targetNamespace
  //      val name = erd.name
  //
  //      val children = childrenDetached.filter(c => {
  //        val childName = c.getName()
  //        val childNS = c.getNamespace()
  //        name == childName && ns == childNS
  //      })
  //
  //      val numChildren = children.length
  //
  //      if (numChildren >= minOccurs) {
  //        val firstChild = children.head
  //        val restOfChildren = children.tail
  //        val restOfNonDefaultedChildren = restOfChildren.filterNot(c => c.getAttribute("defaulted", XMLUtils.INT_NS_OBJECT).getBooleanValue())
  //        val newContent = firstChild :: restOfNonDefaultedChildren
  //        if (newContent.length > 0)
  //          elt.addContent(newContent)
  //      } else {
  //        // Nothing to do here, reattach children.
  //        elt.addContent(childrenDetached)
  //      }
  //    })
  //  }

  override def parse(start: PState): Unit = {
    ???
    //    val end = uoSeqParser.parse1(start)
    //    val currentElemAfter = end.parentElement.jdomElt.get

    //    checkScalarsOccurExactlyOnce(currentElemAfter, end)
    //    dropDefaulted(currentElemAfter, end)
    //    checkOccurrence(currentElemAfter)
    //
    //    // Sort so that the contents are in the expected order.
    //    sort(currentElemAfter, end)

    //    end
  }
}
