/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.collection.JavaConversions._

class UnorderedSequenceParser(
  context: ModelGroupRuntimeData,
  sortOrder: Seq[(String, org.jdom2.Namespace)],
  scalarMembers: Seq[(String, String, org.jdom2.Namespace)],
  uoSeqParser: Parser)
  extends Parser(context) with WithParseErrorThrowing {

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

  def parse(start: PState): Unit = withParseErrorThrowing(start) {
    ???
    val end = uoSeqParser.parse1(start, context)
    //    val currentElemAfter = end.parentElement.jdomElt.get

    //    checkScalarsOccurExactlyOnce(currentElemAfter, end)
    //    dropDefaulted(currentElemAfter, end)
    //    checkOccurrence(currentElemAfter)
    //
    //    // Sort so that the contents are in the expected order.
    //    sort(currentElemAfter, end)

    end
  }
}
