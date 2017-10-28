/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://woverride val ww.tresys.com
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

trait GroupRef { self: ModelGroup =>

  final def asModelGroup: ModelGroup = self

  def groupDef: GlobalGroupDef

  final override def group = groupDef.modelGroup

  final def referredToComponent = group

  final override lazy val optReferredToComponent = Some(referredToComponent)

  protected final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:group>{ contents @ _* }</dfdl:group> => Some(new DFDLGroup(node, this))
      case _ => annotationFactoryForDFDLStatement(node, self)
    }
  }

  protected final def emptyFormatFactory = new DFDLGroup(newDFDLAnnotationXML("group"), this)
}

/**
 * A GroupRefFactory (group reference) is an indirection to
 * factories to create a SequenceGroupRef, or ChoiceGroupRef.
 *
 * The refXMLArg is the xml for the group reference.
 */
final class GroupRefFactory(refXMLArg: Node, val refLexicalParent: SchemaComponent, position: Int)
  extends SchemaComponentFactory(refXMLArg, refLexicalParent.schemaDocument)
  with HasRefMixin {

  final def qname = this.refQName

  lazy val groupRef = {
    val defFactory = parent.schemaSet.getGlobalGroupDef(qname).getOrElse {
      SDE("Referenced group definition not found: %s", this.ref)
    }
    lazy val gref = defFactory match {
      case s: GlobalSequenceGroupDefFactory => new SequenceGroupRef(gdef.asInstanceOf[GlobalSequenceGroupDef], refXMLArg, refLexicalParent, position)
      case c: GlobalChoiceGroupDefFactory => new ChoiceGroupRef(gdef.asInstanceOf[GlobalChoiceGroupDef], refXMLArg, refLexicalParent, position)
    }
    lazy val gdef: GlobalGroupDef = defFactory.forGroupRef(gref)
    gref
  }

}

final class SequenceGroupRef(globalGroupDef: => GlobalSequenceGroupDef, refXML: Node, refLexicalParent: SchemaComponent, position: Int)
  extends SequenceBase(refXML, refLexicalParent, position)
  with GroupRef {

  override lazy val groupDef = globalGroupDef

  override lazy val apparentXMLChildren = groupDef.modelGroup.asInstanceOf[Sequence].apparentXMLChildren

}

final class ChoiceGroupRef(globalGroupDef: => GlobalChoiceGroupDef, refXML: Node, refLexicalParent: SchemaComponent, position: Int)
  extends ChoiceBase(refXML, refLexicalParent, position)
  with GroupRef {

  override lazy val groupDef = globalGroupDef

  requiredEvaluations(validateChoiceBranchKey)

  private def validateChoiceBranchKey(): Unit = {
    // Ensure the model group of a global group def do not define choiceBranchKey.
    val found = findPropertyOptionThisComponentOnly("choiceBranchKey")
    if (found.isDefined) {
      SDE("dfdl:choiceBranchKey cannot be specified on the choice/sequence child of a global group definition")
    }
  }
}
