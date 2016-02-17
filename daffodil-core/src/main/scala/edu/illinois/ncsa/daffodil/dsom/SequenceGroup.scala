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

import scala.xml.Attribute
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Null
import scala.xml.Text
import scala.xml._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Sequence_AnnotationMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.SeparatorSuppressionPolicyMixin
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.OccursCountKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.SequenceKind
import edu.illinois.ncsa.daffodil.Implicits.ns2String
import edu.illinois.ncsa.daffodil.grammar.SequenceGrammarMixin
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.SequenceRuntimeData

class Sequence(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends ModelGroup(xmlArg, parent, position)
  with Sequence_AnnotationMixin
  with SequenceRuntimeValuedPropertiesMixin
  with SequenceGrammarMixin
  with SeparatorSuppressionPolicyMixin {

  requiredEvaluations(checkIfValidUnorderedSequence)

  final override lazy val myPeers = sequencePeers

  final override lazy val hasDelimiters = hasInitiator || hasTerminator || hasSeparator

  protected final def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:sequence>{ contents @ _* }</dfdl:sequence> => new DFDLSequence(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  protected final def emptyFormatFactory = new DFDLSequence(newDFDLAnnotationXML("sequence"), this)
  protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSequence]

  // The dfdl:hiddenGroupRef property cannot be scoped, nor defaulted. It's really a special
  // attribute, not a format property in the usual sense.
  // So we retrieve it by this lower-level mechanism which only combines short and long form.
  //
  // FIXME: must call findPropertyOption so that it gets a context back which
  // allows resolution of a QName using the right scope.
  final lazy val hiddenGroupRefOption = getPropertyOption("hiddenGroupRef")

  /**
   * We're hidden if we're inside something hidden, or we're explicitly a
   * hidden group reference (sequence with hiddenGroupRef property)
   */
  final override lazy val isHidden = {
    val res = hiddenGroupRefOption match {
      case Some(_) => true
      case None => someEnclosingComponent.isHidden
    }
    res
  }

  private lazy val <sequence>{ apparentXMLChildren @ _* }</sequence> = xml

  final def xmlChildren = LV('xmlChildren) {
    hiddenGroupRefOption match {
      case Some(qname) => {
        schemaDefinitionUnless(apparentXMLChildren.length == 0, "A sequence with hiddenGroupRef cannot have children.")
        // synthesize a group reference here.
        val contextScope = xml.asInstanceOf[Elem].scope
        val hgr = {
          (<xs:group xmlns:xs={ XMLUtils.xsdURI } ref={ qname }/>).copy(scope = contextScope)
        }
        List(hgr)
      }
      case None => apparentXMLChildren
    }
  }.value

  final lazy val hasStaticallyRequiredInstances = {
    // true if there are syntactic features
    hasInitiator || hasTerminator ||
      // or if any child of the sequence has statically required instances.
      groupMembers.exists { _.hasStaticallyRequiredInstances }
  }

  final override def hasKnownRequiredSyntax = LV('hasKnownRequiredSyntax) {
    lazy val memberHasRequiredSyntax = groupMembersNoRefs.exists(_.hasKnownRequiredSyntax)
    lazy val prefixOrPostfixAndStaticallyRequiredInstance =
      groupMembersNoRefs.filter { _.isRepresented }.exists { _.hasStaticallyRequiredInstances } &&
        (hasPrefixSep || hasPostfixSep)
    lazy val infixAnd2OrMoreStaticallyRequiredInstances =
      groupMembersNoRefs.filter { m => m.isRepresented && m.hasStaticallyRequiredInstances }.length > 1 && hasInfixSep
    lazy val sepAndArryaWith2OrMoreStaticallyRequiredInstances =
      groupMembersNoRefs.filter { m =>
        m.isRepresented && m.hasStaticallyRequiredInstances && (m match {
          case e: LocalElementBase => e.minOccurs > 1
          case _ => false
        })
      }.length > 0 && hasSeparator
    val res =
      hasInitiator ||
        hasTerminator ||
        memberHasRequiredSyntax ||
        prefixOrPostfixAndStaticallyRequiredInstance ||
        infixAnd2OrMoreStaticallyRequiredInstances ||
        sepAndArryaWith2OrMoreStaticallyRequiredInstances
    res
  }.value

  /**
   * Provides unordered sequence checks.  Will SDE if invalid.
   */
  protected final def checkIfValidUnorderedSequence(): Unit = {
    if (!isOrdered) {
      checkMembersAreAllElementOrElementRef
      checkMembersHaveValidOccursCountKind
      checkMembersHaveUniqueNamesInNamespaces
    }
  }

  private def checkMembersHaveValidOccursCountKind: Unit = {
    val validChildren: Seq[LocalElementBase] =
      groupMembers.filter { m => m.isInstanceOf[LocalElementDecl] || m.isInstanceOf[ElementRef]
      }.map(_.asInstanceOf[LocalElementBase])

    val invalidChildren = validChildren.filter(e => {
      if (e.minOccurs == 0 | !e.isScalar) {
        e.occursCountKind match {
          case OccursCountKind.Parsed => false
          case _ => true
        }
      } else false
    })
    val hasInvalidChildren = invalidChildren.length > 0
    if (hasInvalidChildren)
      this.SDE("Members of an unordered sequence (%s) that are optional or array elements must have dfdl:occursCountKind='parsed'." +
        "\nThe offending members: %s.", this.path, invalidChildren.mkString(","))
  }

  private def checkMembersAreAllElementOrElementRef: Unit = {
    val invalidChildren = groupMembers.filterNot(child =>
      child.isInstanceOf[LocalElementDecl] || child.isInstanceOf[ElementRef])
    val hasInvalidChildren = invalidChildren.length > 0
    if (hasInvalidChildren)
      this.SDE("Members of an unordered sequence (%s) must be Element or ElementRef." +
        "\nThe offending members: %s.", this.path, invalidChildren.mkString(","))
  }

  private def checkMembersHaveUniqueNamesInNamespaces: Unit = {
    val childrenGroupedByNamespace =
      groupMembers.filter(m => m.isInstanceOf[ElementBase]).map(_.asInstanceOf[ElementBase]).groupBy(_.targetNamespace)

    childrenGroupedByNamespace.foreach {
      case (ns, children) => {
        // At this point we're looking at the individual namespace buckets
        val childrenGroupedByName = children.groupBy(child => child.name)
        childrenGroupedByName.foreach {
          case (name, children) =>
            // Now we're looking at the individual name buckets within the
            // individual namespace bucket.
            if (children.length > 1)
              this.SDE("Two or more members of the unordered sequence (%s) have the same name and the same namespace." +
                "\nNamespace: %s\tName: %s.",
                this.path, ns, name)
        }
      }
    }
  }

  final lazy val isOrdered: Boolean = this.sequenceKind match {
    case SequenceKind.Ordered => true
    case SequenceKind.Unordered => false
  }

  final lazy val unorderedSeq: Option[UnorderedSequence] = if (!isOrdered) {

    val children = apparentXMLChildren.map(c => {
      c match {
        case elem: Elem => {
          val elemMin = elem % Attribute(None, "minOccurs", Text("1"), Null)
          val elemMax = elemMin % Attribute(None, "maxOccurs", Text("1"), Null)
          elemMax
        }
        case x => x
      }
    })

    // Create a list of the min/maxOccur pairs for each child
    val elementChildrenMinMaxOccurs = apparentXMLChildren.map(c =>
      c match {
        case elem: Elem => {
          val min = Integer.parseInt(elem.attributes.get("minOccurs").get.text)
          val max = Integer.parseInt(elem.attributes.get("maxOccurs").get.text)
          Some(min, max)
        }
        case x => None
      }).filter(_.isDefined).map(_.get)

    // Compute minimal number of elements required
    val newMinOccurs = {
      val minRequired = elementChildrenMinMaxOccurs.map { case (minVal, _) => minVal }.foldLeft(0)(_ + _)
      minRequired.toString
    }

    // Compute maximal number of elements required
    val newMaxOccurs = {
      val maxRequired = elementChildrenMinMaxOccurs.map { case (_, maxVal) => maxVal }.foldLeft(0)(_ + _)
      maxRequired.toString
    }

    val newContent: Node =
      <element name="choiceElement" minOccurs={ newMinOccurs } maxOccurs={ newMaxOccurs } dfdl:occursCountKind="parsed" dfdl:lengthKind="implicit">
        <complexType>
          <choice dfdl:choiceLengthKind="implicit">{ children }</choice>
        </complexType>
      </element>

    // Constructs a sequence of choice using newContent
    val newXML = {
      xmlArg match {
        case Elem(prefix, "sequence", attrs, scope, content @ _*) => Elem(prefix, "sequence", attrs, scope, true, newContent: _*)
        case other => other
      }
    }

    Some(new UnorderedSequence(newXML, children, parent, position))

  } else None

  final lazy val modelGroupRuntimeData = {
    new SequenceRuntimeData(
      schemaSet.variableMap,
      encodingInfo,
      // elementChildren.map { _.elementRuntimeData.dpathElementCompileInfo },
      schemaFileLocation,
      dpathCompileInfo,
      prettyName,
      path,
      namespaces,
      defaultBitOrder,
      groupMembersRuntimeData,
      enclosingElement.map { _.elementRuntimeData }.getOrElse(
        Assert.invariantFailed("model group with no surrounding element.")),
      enclosingTerm.map { _.termRuntimeData }.getOrElse {
        Assert.invariantFailed("model group with no surrounding term.")
      },
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      hasNoSkipRegions,
      fillByteValue,
      optIgnoreCase)
  }

}

final class UnorderedSequence(xmlArg: Node, xmlContents: Seq[Node], parent: SchemaComponent, position: Int)
  extends Sequence(xmlArg, parent, position) {
  // A shell, the actual XML representation is passed in
  // from Sequence
}
