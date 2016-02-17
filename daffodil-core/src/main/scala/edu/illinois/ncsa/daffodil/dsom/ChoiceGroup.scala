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
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Choice_AnnotationMixin
import edu.illinois.ncsa.daffodil.grammar.ChoiceGrammarMixin
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.ChoiceRuntimeData
import edu.illinois.ncsa.daffodil.processors.unparsers.ChoiceBranchEvent
import edu.illinois.ncsa.daffodil.processors.unparsers.ChoiceBranchStartEvent
import edu.illinois.ncsa.daffodil.processors.unparsers.ChoiceBranchEndEvent
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe

/**
 * Choices are a bit complicated.
 *
 * They can have initiators and terminators. This is most easily thought of as wrapping each choice
 * inside a sequence having only the choice inside it, and moving the initiator and terminator specification to that
 * sequence.
 *
 * That sequence is then the term replacing the choice wherever the choice is.
 *
 * Choices can have children which are scalar elements. In this case, that scalar element behaves as if it were
 * a child of the enclosing sequence (which could be the one we just injected above the choice.
 *
 * Choices can have children which are recurring elements. In this case, the behavior is as if the recurring child was
 * placed inside a sequence which has no initiator nor terminator, but repeats the separator specification from
 * the sequence context that encloses the choice.
 *
 * All that, and the complexities of separator suppression too.
 *
 * There's also issues like this:
 *
 * <choice>
 *    <element .../>
 *    <sequence/>
 * </choice>
 *
 * in the above, one alternative is an empty sequence. So this choice may produce an element which takes up
 * a child position, and potentially requires separation, or it may produce nothing at all.
 *
 */

final class Choice(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends ModelGroup(xmlArg, parent, position)
  with Choice_AnnotationMixin
  with RawDelimitedRuntimeValuedPropertiesMixin // initiator and terminator (not separator)
  with ChoiceGrammarMixin {

  requiredEvaluations(branchesAreNonOptional)
  requiredEvaluations(branchesAreNotIVCElements)

  protected final override lazy val myPeers = choicePeers

  protected final override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:choice>{ contents @ _* }</dfdl:choice> => new DFDLChoice(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  protected final def emptyFormatFactory = new DFDLChoice(newDFDLAnnotationXML("choice"), this)
  protected final def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLChoice]

  protected final override lazy val <choice>{ xmlChildren @ _* }</choice> = xml

  final lazy val hasStaticallyRequiredInstances = {
    // true if the choice has syntactic features (initiator, terminator)
    hasInitiator || hasTerminator ||
      // or if all arms of the choice have statically required instances.
      groupMembers.forall { _.hasStaticallyRequiredInstances }
  }

  final override def hasKnownRequiredSyntax = LV('hasKnownRequiredSyntax) {
    if (hasInitiator || hasTerminator) true
    // else if (isKnownToBeAligned) true //TODO: Alignment may not occur; hence, cannot be part of determining whether there is known syntax.
    else groupMembers.forall(_.hasKnownRequiredSyntax)
  }.value

  /**
   * The DFDL Spec as of this writing 2015-06-02 says that branches cannot be optional, but then
   * specifies that what it means is that if a choice branch root is an element/element-ref, it
   * cannot have minOccurs="0". This is a different notion of optional.
   *
   * The DFDL Working group has Action 280 which is about whether this should be corrected/modified.
   * There are two notions of optionality in DFDL. First is optional meaning 0 or 1 occurrence of an
   * element. The second is "not required", and that is defined in terms of dfdl:occursCountKind.
   *
   * For the time being, the definition of non-optional used here is that is not minOccurs="0" if an
   * element, and that unless it is a calculated element, it must have some footprint syntactically
   * from the data value or framing.
   *
   * Open issues:
   * 1) Is alignment or leading/trailing skip to be considered syntax. Alignment might not be there.
   * 2) What about an empty sequence that only carries statement annotations such as dfdl:assert
   *
   * This latter need to be allowed, and are because while they do not have known required syntax,
   * they are still considered to be represented. (all sequence and choice groups are isRepresented == true).
   */
  final def branchesAreNonOptional = LV('branchesAreNonOptional) {
    val branchesOk = groupMembersNoRefs map { branch =>
      if (branch.isOptional) {
        schemaDefinitionErrorButContinue("Branch of choice %s must be non-optional.".format(branch.path))
        false
      } else true
    }
    assuming(branchesOk.forall { x => x })
  }.value

  final def branchesAreNotIVCElements = LV('branchesAreNotIVCElements) {
    val branchesOk = groupMembersNoRefs map { branch =>
      if (!branch.isRepresented) {
        schemaDefinitionErrorButContinue("Branch of choice %s cannot have the dfdl:inputValueCalc property.".format(branch.path))
        false
      } else true
    }
    assuming(branchesOk.forall { x => x })
  }.value

  final def choiceBranchMap: Map[ChoiceBranchEvent, RuntimeData] = LV('choiceBranchMap) {
    val eventERDTuples = groupMembersNoRefs.flatMap {
      case e: ElementBase => Seq((ChoiceBranchStartEvent(e.namedQName), e.runtimeData))
      case mg: ModelGroup => {
        val idEvents = mg.identifyingEventsForChoiceBranch
        Assert.invariant(!idEvents.isEmpty)
        idEvents.map { (_, mg.runtimeData) }
      }
    }

    // converts a sequence of tuples into a multi-map
    val eventERDMap = eventERDTuples.groupBy { _._1 }.mapValues { _.map(_._2) }

    val noDupes = eventERDMap.map {
      case (event, erds) =>
        if (erds.length > 1) {
          SDW("Event %s could identify multiple choice branches (%s). The first branch will be chosen during unparse when seeing this event.",
            event, erds.mkString(", "))
        }
        (event, erds(0))
    }

    noDupes
  }.value

  final lazy val modelGroupRuntimeData = {
    new ChoiceRuntimeData(
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
