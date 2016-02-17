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
import java.lang.{ Integer => JInt }

abstract class GroupBase(xmlArg: Node, parentArg: SchemaComponent, position: Int)
  extends Term(xmlArg, parentArg, position) {

  private def prettyIndex = {
    myPeers.map { peers =>
      {
        if (peers.length == 1) "" // no index expression if we are the only one
        else "[" + (peers.indexOf(this) + 1) + "]" // 1-based indexing in XML/XSD
      }
    }.getOrElse("")
  }

  override lazy val prettyName = prettyBaseName + prettyIndex
  protected def prettyBaseName: String

  final lazy val enclosingComponentModelGroup = enclosingComponent.collect { case mg: ModelGroup => mg }
  final lazy val sequencePeers = enclosingComponentModelGroup.map { _.sequenceChildren }
  final lazy val choicePeers = enclosingComponentModelGroup.map { _.choiceChildren }
  final lazy val groupRefPeers = enclosingComponentModelGroup.map { _.groupRefChildren }

  protected def myPeers: Option[Seq[GroupBase]]

  def group: ModelGroup

  final lazy val immediateGroup: Option[ModelGroup] = {
    val res: Option[ModelGroup] = this.group match {
      case (s: Sequence) => Some(s)
      case (c: Choice) => Some(c)
      case _ => None
    }
    res
  }

  private lazy val alignmentValueChildren: JInt = {
    immediateGroup match {
      case Some(m: ModelGroup) => {
        m.groupMembers.sortBy(m => -m.alignmentValueInBits).headOption match {
          case Some(child) => child.alignmentValueInBits
          case None => 0
        }
      }
      case None => 0
    }
  }

  final lazy val alignmentValueInBits: JInt = {
    this.alignment match {
      case AlignmentType.Implicit => alignmentValueChildren
      case align: JInt => this.alignmentUnits match {
        case AlignmentUnits.Bits => align
        case AlignmentUnits.Bytes => 8 * align
      }
    }
  }

}
