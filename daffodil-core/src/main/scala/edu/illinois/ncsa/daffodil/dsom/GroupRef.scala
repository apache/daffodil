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

import java.util.UUID
import scala.Option.option2Iterable
import scala.xml.Attribute
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Null
import scala.xml.Text
import scala.xml._
import edu.illinois.ncsa.daffodil.Implicits.ns2String
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.AlignmentType
import edu.illinois.ncsa.daffodil.schema.annotation.props.SeparatorSuppressionPolicyMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.AlignmentUnits
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Choice_AnnotationMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Group_AnnotationMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.OccursCountKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.SequenceKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Sequence_AnnotationMixin
import edu.illinois.ncsa.daffodil.util.ListUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.EncodingInfo
import edu.illinois.ncsa.daffodil.grammar.GroupRefGrammarMixin

/**
 * A GroupRef (group reference) is a term, but most everything is delgated to the
 * referred-to Global Group Definition object.
 */
class GroupRef(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends GroupBase(xmlArg, parent, position)
  with DFDLStatementMixin
  with GroupRefGrammarMixin
  with Group_AnnotationMixin
  with SeparatorSuppressionPolicyMixin
  with SequenceRuntimeValuedPropertiesMixin
  with HasRefMixin {

  requiredEvaluations(groupDef)

  override lazy val elementChildren = group.elementChildren

  // delegate to the model group object. It assembles properties from
  // the group ref and the group def
  override def findPropertyOption(pname: String): PropertyLookupResult = {
    val res = group.findPropertyOption(pname)
    res
  }
  lazy val nonDefaultPropertySources = group.nonDefaultPropertySources
  lazy val defaultPropertySources = group.defaultPropertySources

  lazy val prettyBaseName = "group.ref." + refQName.local

  lazy val myPeers = groupRefPeers

  override lazy val termChildren: Seq[Term] = {
    group.termChildren
  }

  lazy val qname = resolveQName(ref)

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:group>{ contents @ _* }</dfdl:group> => new DFDLGroup(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  def emptyFormatFactory = new DFDLGroup(newDFDLAnnotationXML("group"), this)
  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLGroup]

  def hasStaticallyRequiredInstances = group.hasStaticallyRequiredInstances

  lazy val group = groupDef.modelGroup

  override lazy val termRuntimeData = group.termRuntimeData

  override lazy val couldHaveText = group.couldHaveText

  override lazy val referredToComponent = group

  override lazy val encodingInfo = group.encodingInfo

  lazy val groupDef = groupDef_.value
  private val groupDef_ = LV('groupDef) {
    this.schemaSet.getGlobalGroupDef(qname) match {
      case None => SDE("Referenced group definition not found: %s", this.ref)
      case Some(x) => x.forGroupRef(this, position)
    }
  }

  lazy val statements = localStatements
  lazy val newVariableInstanceStatements = localNewVariableInstanceStatements
  lazy val assertStatements = localAssertStatements
  lazy val discriminatorStatements = localDiscriminatorStatements
  lazy val setVariableStatements = localSetVariableStatements

}

