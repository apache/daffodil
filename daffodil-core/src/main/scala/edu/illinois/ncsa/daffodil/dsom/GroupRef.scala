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
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.HasRefMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.NotFound

trait GroupRef { self: ModelGroup =>

  final def asModelGroup: ModelGroup = self

  def groupDef: GlobalGroupDef

  final def referredToComponent = groupDef

  final override lazy val optReferredToComponent = Some(referredToComponent)

  final override lazy val groupMembers = groupDef.groupMembers

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

  lazy val groupRef = {
    val gdefFactory = parent.schemaSet.getGlobalGroupDef(qname).getOrElse {
      SDE("Referenced group definition not found: %s", this.ref)
    }
    val (gref, _) = gdefFactory.forGroupRef(refXMLArg, refLexicalParent, position, isHidden)
    gref
  }

}

final class SequenceGroupRef(globalGroupDef: => GlobalSequenceGroupDef,
  refXML: Node,
  refLexicalParent: SchemaComponent,
  positionArg: Int,
  isHiddenArg: Boolean)
  extends SequenceTermBase(refXML, refLexicalParent, positionArg)
  with GroupRef {

  override def isHidden = isHiddenArg

  private lazy val sgd = groupDef.asInstanceOf[GlobalSequenceGroupDef]

  override def xmlChildren = sgd.xmlChildren

  override def apparentXMLChildren = sgd.apparentXMLChildren

  private val nf = NotFound(Nil, Nil, "hiddenGroupRef")

  override def hiddenGroupRefOption = nf

  override lazy val groupDef = globalGroupDef

}

final class ChoiceGroupRef(globalGroupDef: => GlobalChoiceGroupDef,
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
