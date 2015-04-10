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
import edu.illinois.ncsa.daffodil.grammar.TermGrammarMixin

/////////////////////////////////////////////////////////////////
// Groups System
/////////////////////////////////////////////////////////////////

// A term is content of a group
abstract class Term(xmlArg: Node, parentArg: SchemaComponent, val position: Int)
  extends AnnotatedSchemaComponent(xmlArg, parentArg)
  with LocalComponentMixin
  with TermGrammarMixin
  with DelimitedRuntimeValuedPropertiesMixin
  with InitiatedTerminatedMixin {

  def termRuntimeData: TermRuntimeData

  def elementChildren: Seq[ElementBase]

  override lazy val encodingInfo =
    new EncodingInfo(termRuntimeData, schemaFileLocation, encoding, optionUTF16Width, defaultEncodingErrorPolicy,
      termChildrenEncodingInfo)

  override lazy val dpathCompileInfo =
    new DPathCompileInfo(
      enclosingComponent.map { _.dpathCompileInfo },
      variableMap,
      namespaces,
      path,
      schemaFileLocation)

  final lazy val termChildrenEncodingInfo: Seq[EncodingInfo] = termChildren.map { _.encodingInfo }

  /**
   * An integer which is the alignment of this term. This takes into account the
   * representation, type, charset encoding and alignment-related properties.
   */
  def alignmentValueInBits: Int

  /**
   * True if this term is known to have some text aspect. This can be the value, or it can be
   * delimiters.
   * <p>
   * False only if this term cannot ever have text in it. Example: a sequence with no delimiters.
   * Example: a binary int with no delimiters.
   * <p>
   * Note: this is not recursive - it does not roll-up from children terms.
   * TODO: it does have to deal with the prefix length situation. The type of the prefix
   * may be textual.
   * <p>
   * Override in element base to take simple type or prefix length situations into account
   */
  lazy val couldHaveText = hasDelimiters

  //TODO: if we add recursive types capability to DFDL this will have to change
  // but so will many of these compiler passes up and down through the DSOM objects.

  /**
   * The termChildren are the children that are Terms, i.e., derived from the Term
   * base class. This is to make it clear
   * we're not talking about the XML structures inside the XML parent (which might
   * include annotations, etc.
   */
  def termChildren: Seq[Term]

  final val tID = UUID.randomUUID()

  // Scala coding style note: This style of passing a constructor arg that is named fooArg,
  // and then having an explicit val/lazy val which has the 'real' name is 
  // highly recommended. Lots of time wasted because a val constructor parameter can be 
  // accidently hidden if a derived class uses the same name as one of its own parameters.
  // These errors just seem easier to deal with if you use the fooArg style. 

  lazy val someEnclosingComponent = enclosingComponent.getOrElse(Assert.invariantFailed("All terms except a root element have an enclosing component."))

  lazy val referredToComponent = this // override in ElementRef and GroupRef

  lazy val isRepresented = true // overridden by elements, which might have inputValueCalc turning this off

  def isScalar = true // override in local elements

  lazy val allTerminatingMarkup: List[(CompiledExpression, String, String)] = {
    val (tElemName, tElemPath) = this.terminatorLoc
    val tm = List((this.terminator, tElemName, tElemPath)) ++ this.allParentTerminatingMarkup
    tm.filter { case (delimValue, elemName, elemPath) => delimValue.isKnownNonEmpty }
  }

  lazy val allParentTerminatingMarkup: List[(CompiledExpression, String, String)] = {
    // Retrieves the terminating markup for all parent
    // objects

    // TODO: This is not entirely correct as it assumes that separator and terminator
    // will always be defined.  It's entirely possible that one or neither is defined.
    // The call to this non-existant property will result in an SDE.
    // See created issue DFDL-571
    val pTM: List[(CompiledExpression, String, String)] = parent match {
      case s: Sequence => {
        val (sElemName, sElemPath) = s.separatorLoc
        val (tElemName, tElemPath) = s.terminatorLoc
        List((s.separator, sElemName, sElemPath), (s.terminator, tElemName, tElemPath)) ++ s.allParentTerminatingMarkup
      }
      case c: Choice => {
        val (tElemName, tElemPath) = c.terminatorLoc
        List((c.terminator, tElemName, tElemPath)) ++ c.allParentTerminatingMarkup
      }
      case d: SchemaDocument =>
        // we're a global object. Our parent is a schema document
        // so follow backpointers to whatever is referencing us.
        this match {
          case ge: GlobalElementDecl => ge.elementRef match {
            case None => {
              // we are root. So there is no enclosing sequence at all
              List.empty
            }
            case Some(er) => er.allTerminatingMarkup
          }
        }
      case ct: LocalComplexTypeDef => ct.parent match {
        case local: LocalElementDecl => local.allTerminatingMarkup
        case global: GlobalElementDecl => {
          global.elementRef match {
            case None => {
              val (tElemName, tElemPath) = global.terminatorLoc
              List((global.terminator, tElemName, tElemPath))
            }
            case Some(eRef) => eRef.allTerminatingMarkup
          }
        }
        case _ => Assert.impossibleCase()
      }
      // global type, we have to follow back to the element referencing this type
      case ct: GlobalComplexTypeDef => {
        // Since we are a term directly inside a global complex type def,
        // our nearest enclosing sequence is the one enclosing the element that
        // has this type. 
        //
        // However, that element might be local, or might be global and be referenced
        // from an element ref.
        //
        ct.element match {
          case local: LocalElementDecl => local.allTerminatingMarkup
          case global: GlobalElementDecl => {
            global.elementRef match {
              case None => {
                val (tElemName, tElemPath) = global.terminatorLoc
                List((global.terminator, tElemName, tElemPath))
              }
              case Some(eRef) => eRef.allTerminatingMarkup
            }
          }
          case _ => Assert.impossibleCase()
        }
      }
      case gd: GlobalGroupDef => gd.groupRef.allTerminatingMarkup
      // We should only be asking for the enclosingSequence when there is one.
      case _ => Assert.invariantFailed("No parent terminating markup for : " + this)
    }
    val res = pTM.filter { case (delimValue, elemName, elemPath) => delimValue.isKnownNonEmpty }
    res
  }

  /**
   * nearestEnclosingSequence
   *
   * An attribute that looks upward to the surrounding
   * context of the schema, and not just lexically surrounding context. It needs to see
   * what declarations will physically surround the place. This is the dynamic scope,
   * not just the lexical scope. So, a named global type still has to be able to
   * ask what sequence is surrounding the element that references the global type.
   *
   * This is why we have to have the GlobalXYZDefFactory stuff. Because this kind of back
   * pointer (contextual sensitivity) prevents sharing.
   */
  final lazy val nearestEnclosingSequence: Option[Sequence] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) => Some(s)
    case Some(_) => enclosingTerm.get.nearestEnclosingSequence
  }

  final lazy val nearestEnclosingChoiceBeforeSequence: Option[Choice] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) => None
    case Some(c: Choice) => Some(c)
    case Some(_) => enclosingTerm.get.nearestEnclosingChoiceBeforeSequence
  }

  final lazy val nearestEnclosingUnorderedSequence: Option[Sequence] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) if !s.isOrdered => Some(s)
    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  }

  final lazy val nearestEnclosingUnorderedSequenceBeforeSequence: Option[Sequence] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) if !s.isOrdered => Some(s)
    case Some(s: Sequence) => None
    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  }

  final lazy val inChoiceBeforeNearestEnclosingSequence: Boolean = enclosingTerm match {
    case None => false
    case Some(s: Sequence) => false
    case Some(c: Choice) => true
    case Some(_) => enclosingTerm.get.inChoiceBeforeNearestEnclosingSequence
  }

  final lazy val nearestEnclosingElement: Option[ElementBase] = enclosingTerm match {
    case None => None
    case Some(eb: ElementBase) => Some(eb)
    case Some(_) => enclosingTerm.get.nearestEnclosingElement
  }

  final lazy val nearestEnclosingElementNotRef: Option[ElementBase] = nearestEnclosingElement match {
    case None => None
    case Some(er: ElementRef) => er.nearestEnclosingElement // can't be an element ref again
    case x => x
  }

  protected final def thisTermNoRefs: Term = LV('thisTermNoRefs) {
    val es = nearestEnclosingSequence

    val thisTerm = this match {
      case eRef: ElementRef => eRef.referencedElement
      // case gd: GlobalGroupDef => gd.thisTermNoRefs // TODO: scala 2.10 compiler says this line is impossible. 
      case gb: GroupBase if gb.enclosingTerm.isDefined => {
        // We're a group.  We need to determine what we're enclosed by.
        gb.enclosingTerm.get match {
          case encGRef: GroupRef => {
            // We're enclosed by a GroupRef.  We need to retrieve
            // what encloses that GroupRef 

            val res = encGRef.enclosingTerm match {
              case None => encGRef.group
              case Some(encTerm) => encTerm.thisTermNoRefs
            }
            //encGRef.thisTerm
            res
          }
          case encGB: GroupBase if es.isDefined && encGB == es.get => {
            // We're an immediate child of the nearestEnclosingSequence
            // therefore we just return our self as the Term
            this
          }
          case e: LocalElementBase => e // Immediate enclosed by LocalElementBase, return it.
          case _ => gb.group
        }
      }
      case gb: GroupBase => gb.group
      case x => x
    }
    thisTerm
  }.value

  /**
   * We want to determine if we're in an unordered sequence
   * at any point along our parents.
   */
  final lazy val inUnorderedSequence: Boolean =
    nearestEnclosingSequence match {
      case None => {
        false
      }
      case Some(s) => {
        if (s.isOrdered) {
          val result = s.inUnorderedSequence
          result
        } else true
      }
    }

  final lazy val immediatelyEnclosingModelGroup: Option[ModelGroup] = {
    val res = parent match {
      case c: Choice => Some(c)
      case s: Sequence => Some(s)
      case d: SchemaDocument => {
        // we're a global object. Our parent is a schema document
        // so follow backpointers to whatever is referencing us.
        this match {
          case ge: GlobalElementDecl => ge.elementRef match {
            case None => {
              // we are root. So there is no enclosing model group at all
              None
            }
            case Some(er) => er.immediatelyEnclosingModelGroup
          }
        }
      }
      case gdd: GlobalGroupDef => gdd.groupRef.immediatelyEnclosingModelGroup
      case ct: ComplexTypeBase => {
        None
        // The above formerly was ct.element.immediatelyEnclosingModelGroup, 
        // but if we have a CT as our parent, the group around the element whose type 
        // that is, isn't "immediately enclosing".
      }
      case _ => Assert.invariantFailed("immediatelyEnclosingModelGroup called on " + this + "with parent " + parent)
    }
    res
  }

  final lazy val positionInNearestEnclosingSequence: Int = {
    val res =
      if (enclosingComponent == nearestEnclosingSequence) position
      else {
        enclosingComponent match {
          case Some(term: Term) => term.positionInNearestEnclosingSequence
          case Some(ct: ComplexTypeBase) => {
            val ctElem = ct.element
            val ctPos = ctElem.positionInNearestEnclosingSequence
            ctPos
          }
          case Some(ggd: GlobalGroupDef) => ggd.groupRef.positionInNearestEnclosingSequence
          case _ => Assert.invariantFailed("unable to compute position in nearest enclosing sequence")
        }
      }
    res
  }

  final lazy val terminatingMarkup: List[CompiledExpression] = {
    if (hasTerminator) List(terminator)
    else nearestEnclosingSequence match {
      case None => Nil
      case Some(sq) => {
        val sep = {
          if (sq.hasInfixSep || sq.hasPostfixSep) List(sq.separator)
          else Nil
        }
        if (!hasLaterRequiredSiblings) {
          val entm = sq.terminatingMarkup
          val res = sep ++ entm
          res
        } else {
          sep
        }
      }
    }
  }

  final lazy val prettyTerminatingMarkup =
    terminatingMarkup.map { _.prettyExpr }.map { "'" + _ + "'" }.mkString(" ")

  final lazy val isDirectChildOfSequence = parent.isInstanceOf[Sequence]

  import edu.illinois.ncsa.daffodil.util.ListUtils

  final lazy val allSiblings: Seq[Term] = {
    val res = nearestEnclosingSequence.map { enc =>
      val allSiblings = enc.groupMembers.map { _.referredToComponent }
      allSiblings
    }
    res.getOrElse(Nil)
  }

  final lazy val priorSiblings = ListUtils.preceding(allSiblings, this)
  final lazy val laterSiblings = ListUtils.tailAfter(allSiblings, this)
  final lazy val laterElementSiblings = laterSiblings.collect { case elt: ElementBase => elt }

  final lazy val priorSibling = priorSiblings.lastOption
  final lazy val nextSibling = laterSiblings.headOption

  final lazy val hasLaterRequiredSiblings = laterSiblings.exists(_.hasStaticallyRequiredInstances)
  final lazy val hasPriorRequiredSiblings = priorSiblings.exists(_.hasStaticallyRequiredInstances)

  def hasStaticallyRequiredInstances: Boolean
  def isKnownRequiredElement = false
  def isKnownToBePrecededByAllByteLengthItems: Boolean = false
  def hasKnownRequiredSyntax = false

}
