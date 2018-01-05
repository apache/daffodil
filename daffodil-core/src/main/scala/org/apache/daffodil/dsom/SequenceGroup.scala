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

package org.apache.daffodil.dsom

import scala.xml.Attribute
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Null
import scala.xml.Text
import scala.xml._
import org.apache.daffodil.schema.annotation.props.gen.Sequence_AnnotationMixin
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicyMixin
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.schema.annotation.props.gen.SequenceKind
import org.apache.daffodil.Implicits.ns2String
import org.apache.daffodil.grammar.SequenceGrammarMixin
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.schema.annotation.props.Found
import org.apache.daffodil.schema.annotation.props.PropertyLookupResult

abstract class SequenceTermBase(
  final override val xml: Node,
  final override val parent: SchemaComponent,
  final override val position: Int)
  extends ModelGroup
  with Sequence_AnnotationMixin
  with SequenceRuntimeValuedPropertiesMixin
  with SequenceGrammarMixin
  with SeparatorSuppressionPolicyMixin {

  requiredEvaluations(checkIfValidUnorderedSequence)
  requiredEvaluations(modelGroupRuntimeData.preSerialization)

  protected def apparentXMLChildren: Seq[Node]

  final override lazy val myPeers = sequencePeers

  final override lazy val hasDelimiters = hasInitiator || hasTerminator || hasSeparator

  protected def hiddenGroupRefOption: PropertyLookupResult

  final lazy val hasStaticallyRequiredInstances = {
    // true if there are syntactic features
    hasInitiator || hasTerminator ||
      // or if any child of the sequence has statically required instances.
      groupMembers.exists { _.hasStaticallyRequiredInstances }
  }

  final override def hasKnownRequiredSyntax = LV('hasKnownRequiredSyntax) {
    lazy val memberHasRequiredSyntax = groupMembers.exists(_.hasKnownRequiredSyntax)
    lazy val prefixOrPostfixAndStaticallyRequiredInstance =
      groupMembers.filter { _.isRepresented }.exists { _.hasStaticallyRequiredInstances } &&
        (hasPrefixSep || hasPostfixSep)
    lazy val infixAnd2OrMoreStaticallyRequiredInstances =
      groupMembers.filter { m => m.isRepresented && m.hasStaticallyRequiredInstances }.length > 1 && hasInfixSep
    lazy val sepAndArryaWith2OrMoreStaticallyRequiredInstances =
      groupMembers.filter { m =>
        m.isRepresented && m.hasStaticallyRequiredInstances && (m match {
          case e: ElementBase => e.minOccurs > 1
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
    val validChildren: Seq[ElementBase] =
      groupMembers.filter { m => m.isInstanceOf[LocalElementDecl] || m.isInstanceOf[ElementRef]
      }.map(_.asInstanceOf[ElementBase])

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

  def checkHiddenSequenceIsDefaultableOrOVC: Unit = {
    if (this.isHidden) {
      val nonDefaultableOrOVC =
        this.childrenInHiddenGroupNotDefaultableOrOVC
      if (nonDefaultableOrOVC.length > 0) {
        this.SDE("Element(s) of hidden group must define dfdl:outputValueCalc:\n%s", nonDefaultableOrOVC.mkString("\n"))
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
      xml match {
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
      diagnosticDebugName,
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
      optIgnoreCase,
      maybeFillByteEv,
      maybeCheckByteAndBitOrderEv,
      maybeCheckBitOrderAndCharset)
  }

}

/**
 * Captures concepts associated with definitions of Sequence groups.
 *
 * Used by GlobalSequenceGroupDef and local Sequence, but not by SequenceGroupRef.
 * Used on objects that can carry DFDLSequence annotation objects.
 */
trait SequenceDefMixin
  extends AnnotatedSchemaComponent
  with GroupDefLike {

  protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSequence]

  protected final def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:sequence>{ contents @ _* }</dfdl:sequence> => Some(new DFDLSequence(node, this))
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  protected final def emptyFormatFactory = new DFDLSequence(newDFDLAnnotationXML("sequence"), this)

  final lazy val <sequence>{ apparentXMLChildren @ _* }</sequence> = (xml \\ "sequence")(0)

  final def xmlChildren = apparentXMLChildren

  // The dfdl:hiddenGroupRef property cannot be scoped, nor defaulted. It's really a special
  // attribute, not a format property in the usual sense.
  // So we retrieve it by this lower-level mechanism which only combines short and long form.
  //
  final lazy val hiddenGroupRefOption =
    findPropertyOptionThisComponentOnly("hiddenGroupRef")

}

/**
 * Represents a local sequence definition.
 */
class Sequence(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends SequenceTermBase(xmlArg, parent, position)
  with SequenceDefMixin {

  requiredEvaluations(checkHiddenGroupRefHasNoChildren)

  override lazy val optReferredToComponent = None

  private lazy val checkHiddenGroupRefHasNoChildren = {
    if (hiddenGroupRefOption.isDefined) {
      val axc = apparentXMLChildren
      val len = axc.length
      schemaDefinitionUnless(len == 0, "A sequence with hiddenGroupRef cannot have children.")
    }
  }

  final lazy val hiddenGroupRefXML = LV('hiddenGroupRefXML) {
    val Found(qname, _, _, _) = hiddenGroupRefOption
    // synthesize a group reference here.
    val contextScope = xml.asInstanceOf[Elem].scope
    val hgr = {
      (<xs:group xmlns:xs={ XMLUtils.xsdURI } ref={ qname }/>).copy(scope = contextScope)
    }
    hgr
  }.value
}

final class UnorderedSequence(xmlArg: Node, xmlContents: Seq[Node], parent: SchemaComponent, position: Int)
  extends Sequence(xmlArg, parent, position) {
  // A shell, the actual XML representation is passed in
  // from Sequence
}
