package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.xml._
import scala.xml.parsing._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._

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

  lazy val parent = parentArg
  // Scala coding style note: This style of passing a constructor arg that is named fooArg,
  // and then having an explicit val/lazy val which has the 'real' name is 
  // highly recommended. Lots of time wasted because a val constructor parameter can be 
  // accidently hidden if a derived class uses the same name as one of its own parameters.
  // These errors just seem easier to deal with if you use the fooArg style. 

  lazy val someEnclosingComponent = enclosingComponent.getOrElse(Assert.invariantFailed("All terms except a root element have an enclosing component."))

  //  lazy val enclosingComponent: Option[SchemaComponent] = {
  //    val res = Some(parent) // for global objects, the enclosing will be the thing referencing them.
  //    res
  //  }

  lazy val isRepresented = true // overridden by elements, which might have inputValueCalc turning this off

  def isScalar = true // override in local elements

  lazy val allTerminatingMarkup: List[(CompiledExpression, String, String)] = {
    val (tElemName, tElemPath) = this.terminatorLoc
    val tm = List((this.terminator, tElemName, tElemPath)) ++ this.allParentTerminatingMarkup
    tm.filter { case (delimValue, elemName, elemPath) => delimValue.isKnownNonEmpty }
  }

  // TODO Review Comment
  // This below should not reproduce the logic of enclosingComponent unless it needs
  // something different from that. 

  //  lazy val allParentTerminatingMarkup: List[CompiledExpression] = {
  //    // Retrieves the terminating markup for all parent
  //    // objects
  //    // println(this + " with parent " + parent)
  //
  //    // TODO: This is not entirely correct as it assumes that separator and terminator
  //    // will always be defined.  It's entirely possible that one or neither is defined.
  //    // The call to this non-existant property will result in an SDE.
  //    // See created issue DFDL-571
  //    val pTM = parent match {
  //      case s: Sequence => List(s.separator, s.terminator) ++ s.allParentTerminatingMarkup
  //      case c: Choice => List(c.terminator) ++ c.allParentTerminatingMarkup
  //      case d: SchemaDocument =>
  //        // we're a global object. Our parent is a schema document
  //        // so follow backpointers to whatever is referencing us.
  //        this match {
  //          case gct: GlobalComplexTypeDef => gct.element.allTerminatingMarkup
  //          case gd: GlobalGroupDef => gd.groupRef.allTerminatingMarkup
  //          case ge: GlobalElementDecl => ge.elementRef match {
  //            case None => {
  //              // we are root. So there is no enclosing sequence at all
  //              List.empty
  //            }
  //            case Some(er) => er.allTerminatingMarkup
  //          }
  //        }
  //      case ct: LocalComplexTypeDef => ct.parent match {
  //        case local: LocalElementDecl => local.allTerminatingMarkup
  //        case global: GlobalElementDecl => {
  //          global.elementRef match {
  //            case None => List(global.terminator)
  //            case Some(eRef) => eRef.allTerminatingMarkup
  //          }
  //        }
  //        case _ => Assert.impossibleCase()
  //      }
  //      // global type, we have to follow back to the element referencing this type
  //      case ct: GlobalComplexTypeDef => {
  //        // Since we are a term directly inside a global complex type def,
  //        // our nearest enclosing sequence is the one enclosing the element that
  //        // has this type. 
  //        //
  //        // However, that element might be local, or might be global and be referenced
  //        // from an element ref.
  //        //
  //        ct.element match {
  //          case local: LocalElementDecl => local.allTerminatingMarkup
  //          case global: GlobalElementDecl => {
  //            global.elementRef match {
  //              case None => List(global.terminator)
  //              case Some(eRef) => eRef.allTerminatingMarkup
  //            }
  //          }
  //          case _ => Assert.impossibleCase()
  //        }
  //      }
  //      case gd: GlobalGroupDef => gd.groupRef.allTerminatingMarkup
  //      // We should only be asking for the enclosingSequence when there is one.
  //      case _ => Assert.invariantFailed("No parent terminating markup for : " + this)
  //    }
  //    val res = pTM.filter(x => x.isKnownNonEmpty)
  //    // println(res)
  //    res
  //  }

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
          case gct: GlobalComplexTypeDef => gct.element.allTerminatingMarkup
          case gd: GlobalGroupDef => gd.groupRef.allTerminatingMarkup
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
  // TODO Review Comment
  // This below should not reproduce the logic of enclosingComponent unless it needs
  // something different from that. 
  lazy val nearestEnclosingSequence: Option[Sequence] = nearestEnclosingSequence_.value
  private val nearestEnclosingSequence_ = LV('nearestEnclosingSequence) {
    val res = parent match {
      case s: Sequence => Some(s)
      case t: Term => t.nearestEnclosingSequence
      case d: SchemaDocument => {
        // we're a global object. Our parent is a schema document
        // so follow backpointers to whatever is referencing us.
        this match {
          case gct: GlobalComplexTypeDef => gct.element.nearestEnclosingSequence
          case gd: GlobalGroupDef => gd.groupRef.nearestEnclosingSequence
          case ge: GlobalElementDecl => ge.elementRef match {
            case None => {
              // we are root. So there is no enclosing sequence at all
              None
            }
            case Some(er) => er.nearestEnclosingSequence
          }
        }
      }
      case ct: ComplexTypeBase => None // Stop when we get to an element // ct.element.nearestEnclosingSequence
      case gd: GlobalGroupDef => gd.groupRef.nearestEnclosingSequence
      case _ => Assert.invariantFailed("nearestEnclosingSequence called on " + this + "with parent " + parent)
    }
    res
  }

  lazy val inChoiceBeforeNearestEnclosingSequence: Boolean = inChoiceBeforeNearestEnclosingSequence_.value
  private val inChoiceBeforeNearestEnclosingSequence_ = LV('inChoiceBeforeNearestEnclosingSequence) {
    val res = parent match {
      case s: Sequence => false
      case c: Choice => true
      case t: Term => t.inChoiceBeforeNearestEnclosingSequence
      case d: SchemaDocument => {
        // we're a global object. Our parent is a schema document
        // so follow backpointers to whatever is referencing us.
        this match {
          case gct: GlobalComplexTypeDef => gct.element.inChoiceBeforeNearestEnclosingSequence
          case gd: GlobalGroupDef => gd.groupRef.inChoiceBeforeNearestEnclosingSequence
          case ge: GlobalElementDecl => ge.elementRef match {
            case None => {
              // we are root. So there is no coice at all
              false
            }
            case Some(er) => er.inChoiceBeforeNearestEnclosingSequence
          }
        }
      }
      case ct: ComplexTypeBase => false // Stop when we get to an element // ct.element.nearestEnclosingSequence
      case gd: GlobalGroupDef => gd.groupRef.inChoiceBeforeNearestEnclosingSequence
      case _ => Assert.invariantFailed("inChoiceBeforeNearestEnclosingSequence called on " + this + "with parent " + parent)
    }
    res
  }

  lazy val immediatelyEnclosingModelGroup: Option[ModelGroup] = {
    val res = parent match {
      case c: Choice => Some(c)
      case s: Sequence => Some(s)
      case d: SchemaDocument => {
        // we're a global object. Our parent is a schema document
        // so follow backpointers to whatever is referencing us.
        this match {
          case gct: GlobalComplexTypeDef => gct.element.immediatelyEnclosingModelGroup
          case gd: GlobalGroupDef => gd.groupRef.immediatelyEnclosingModelGroup
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

  lazy val positionInNearestEnclosingSequence: Int = {
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

  lazy val terminatingMarkup: List[CompiledExpression] = {
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

  lazy val prettyTerminatingMarkup =
    terminatingMarkup.map { _.prettyExpr }.map { "'" + _ + "'" }.mkString(" ")

  lazy val isDirectChildOfSequence = parent.isInstanceOf[Sequence]

  import edu.illinois.ncsa.daffodil.util.ListUtils

  lazy val hasLaterRequiredSiblings: Boolean = hasRequiredSiblings(ListUtils.tailAfter _)
  lazy val hasPriorRequiredSiblings: Boolean = hasRequiredSiblings(ListUtils.preceding _)

  def hasRequiredSiblings(splitter: ListUtils.SubListFinder[Term]) = {
    val res = nearestEnclosingSequence.map { es =>
      {
        val allSiblings = es.groupMembers
        val sibs = splitter(allSiblings, this)
        val hasAtLeastOne = sibs.find { term => term.hasStaticallyRequiredInstances }
        hasAtLeastOne != None
      }
    }.getOrElse(false)
    res
  }

  def hasStaticallyRequiredInstances: Boolean

}

abstract class GroupBase(xmlArg: Node, parentArg: SchemaComponent, position: Int)
  extends Term(xmlArg, parentArg, position) {

  lazy val prettyIndex = {
    myPeers.map { peers =>
      {
        if (peers.length == 1) "" // no index expression if we are the only one
        else "[" + (peers.indexOf(this) + 1) + "]" // 1-based indexing in XML/XSD
      }
    }.getOrElse("")
  }

  override def prettyName = prettyBaseName + prettyIndex
  def prettyBaseName: String

  lazy val enclosingComponentModelGroup = enclosingComponent.collect { case mg: ModelGroup => mg }
  lazy val sequencePeers = enclosingComponentModelGroup.map { _.sequenceChildren }
  lazy val choicePeers = enclosingComponentModelGroup.map { _.choiceChildren }
  lazy val groupRefPeers = enclosingComponentModelGroup.map { _.groupRefChildren }

  def myPeers: Option[Seq[GroupBase]]

  def group: ModelGroup

  lazy val immediateGroup: Option[GroupBase] = {

    val res: Option[GroupBase] = this.group match {
      case (s: Sequence) => Some(s)
      case (c: Choice) => Some(c)
      case (g: GroupRef) => Some(g)
      case _ => None
    }

    res
  }

}

/**
 * Base class for all model groups, which are term containers.
 */
abstract class ModelGroup(xmlArg: Node, parentArg: SchemaComponent, position: Int)
  extends GroupBase(xmlArg, parentArg, position)
  with DFDLStatementMixin
  with ModelGroupGrammarMixin
  with OverlapCheckMixin {

  requiredEvaluations(groupMembers)

  lazy val gRefNonDefault: Option[ChainPropProvider] = groupRef.map { _.nonDefaultFormatChain }
  lazy val gRefDefault: Option[ChainPropProvider] = groupRef.map { _.defaultFormatChain }

  lazy val nonDefaultPropertySources = nonDefaultPropertySources_.value
  private val nonDefaultPropertySources_ = LV('nonDefaultPropertySources) {
    val seq = (gRefNonDefault.toSeq ++ Seq(this.nonDefaultFormatChain)).distinct
    checkNonOverlap(seq)
    seq
  }

  lazy val defaultPropertySources = defaultPropertySources_.value
  private val defaultPropertySources_ = LV('defaultPropertySources) {
    val seq = (gRefDefault.toSeq ++ Seq(this.defaultFormatChain)).distinct
    seq
  }

  lazy val prettyBaseName = xmlArg.label

  def xmlChildren: Seq[Node]

  private lazy val goodXmlChildren = goodXmlChildren_.value
  private val goodXmlChildren_ = LV('goodXMLChildren) { xmlChildren.flatMap { removeNonInteresting(_) } }
  private lazy val positions = List.range(1, goodXmlChildren.length + 1) // range is exclusive on 2nd arg. So +1.
  private lazy val pairs = goodXmlChildren zip positions

  lazy val sequenceChildren = groupMembers.collect { case s: Sequence => s }
  lazy val choiceChildren = groupMembers.collect { case s: Choice => s }
  lazy val groupRefChildren = groupMembers.collect { case s: GroupRef => s }

  def group = this

  lazy val groupMembers = groupMembers_.value
  private val groupMembers_ = LV('groupMembers) {
    pairs.flatMap {
      case (n, i) =>
        termFactory(n, this, i)
    }
  }

  /**
   * Factory for Terms
   *
   * Because of the context where this is used, this returns a list. Nil for non-terms, non-Nil for
   * an actual term. There should be only one non-Nil.
   *
   * This could be static code in an object. It doesn't reference any of the state of the ModelGroup,
   * it's here so that type-specific overrides are possible in Sequence or Choice
   */
  def termFactory(child: Node, parent: ModelGroup, position: Int) = {
    val childList: List[Term] = child match {
      case <element>{ _* }</element> => {
        val refProp = child.attribute("ref").map { _.text }
        // must get an unprefixed attribute name, i.e. ref='foo:bar', and not
        // be tripped up by dfdl:ref="fmt:fooey" which is a format reference.
        refProp match {
          case None => List(new LocalElementDecl(child, parent, position))
          case Some(_) => List(new ElementRef(child, parent, position))
        }
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => GroupFactory(child, parent, position)
    }
    childList
  }

  /**
   * XML is full of uninteresting text nodes. We just want the element children, not all children.
   */
  def removeNonInteresting(child: Node) = {
    val childList: List[Node] = child match {
      case _: Text => Nil
      case <annotation>{ _* }</annotation> => Nil
      case _ => List(child)
    }
    childList
  }

  /**
   * Combine our statements with those of the group ref that is referencing us (if there is one)
   */
  lazy val statements: Seq[DFDLStatement] = localStatements ++ groupRef.map { _.statements }.getOrElse(Nil)
  lazy val newVariableInstanceStatements: Seq[DFDLNewVariableInstance] =
    localNewVariableInstanceStatements ++ groupRef.map { _.newVariableInstanceStatements }.getOrElse(Nil)
  lazy val (discriminatorStatements, assertStatements) = checkDiscriminatorsAssertsDisjoint(combinedDiscrims, combinedAsserts)
  private lazy val combinedAsserts: Seq[DFDLAssert] = localAssertStatements ++ groupRef.map { _.assertStatements }.getOrElse(Nil)
  private lazy val combinedDiscrims: Seq[DFDLDiscriminator] = localDiscriminatorStatements ++ groupRef.map { _.discriminatorStatements }.getOrElse(Nil)

  lazy val setVariableStatements: Seq[DFDLSetVariable] = {
    val combinedSvs = localSetVariableStatements ++ groupRef.map { _.setVariableStatements }.getOrElse(Nil)
    checkDistinctVariableNames(combinedSvs)
  }

  lazy val groupRef = parent match {
    case ggd: GlobalGroupDef => Some(ggd.groupRef)
    case _ => None
  }

  override lazy val enclosingComponent = {
    val res =
      groupRef match {
        case Some(ref) => groupRef
        case None => Some(parent)
      }
    res
  }

}

/**
 * A factory for model groups.
 */
object GroupFactory {

  /**
   * Because of the contexts where this is used, we return a list. That lets users
   * flatmap it to get a collection of model groups. Nil for non-model groups, non-Nil for the model group
   * object. There should be only one non-Nil.
   */
  def apply(child: Node, parent: SchemaComponent, position: Int) = {
    val childList: List[GroupBase] = child match {
      case <sequence>{ _* }</sequence> => List(new Sequence(child, parent, position))
      case <choice>{ _* }</choice> => List(new Choice(child, parent, position))
      case <group>{ _* }</group> => {
        parent match {
          case ct: ComplexTypeBase => List(new GroupRef(child, ct, 1))
          case mg: ModelGroup => List(new GroupRef(child, mg, position))
        }
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => {
        parent.SDE("Unrecognized construct: %s", child)
      }
    }
    childList
  }

}
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

class Choice(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends ModelGroup(xmlArg, parent, position)
  with Choice_AnnotationMixin
  with RawDelimitedRuntimeValuedPropertiesMixin // initiator and terminator (not separator)
  with ChoiceGrammarMixin {

  lazy val myPeers = choicePeers

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:choice>{ contents @ _* }</dfdl:choice> => new DFDLChoice(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  def emptyFormatFactory = new DFDLChoice(newDFDLAnnotationXML("choice"), this)
  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLChoice]

  lazy val <choice>{ xmlChildren @ _* }</choice> = xml

  lazy val hasStaticallyRequiredInstances = {
    // true if the choice has syntactic features (initiator, terminator)
    hasInitiator || hasTerminator ||
      // or if all arms of the choice have statically required instances.
      groupMembers.forall { _.hasStaticallyRequiredInstances }
  }
}

class Sequence(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends ModelGroup(xmlArg, parent, position)
  with Sequence_AnnotationMixin
  with SequenceRuntimeValuedPropertiesMixin
  with SequenceGrammarMixin
  with SeparatorSuppressionPolicyMixin {

  lazy val myPeers = sequencePeers

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:sequence>{ contents @ _* }</dfdl:sequence> => new DFDLSequence(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  def emptyFormatFactory = new DFDLSequence(newDFDLAnnotationXML("sequence"), this)
  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSequence]

  // The dfdl:hiddenGroupRef property cannot be scoped, nor defaulted. It's really a special
  // attribute, not a format property in the usual sense.
  // So we retrieve it by this lower-level mechanism which only combines short and long form.
  //
  // FIXME: must call findPropertyOption so that it gets a context back which 
  // allows resolution of a QName using the right scope. 
  lazy val hiddenGroupRefOption = getPropertyOption("hiddenGroupRef")

  /**
   * We're hidden if we're inside something hidden, or we're explicitly a
   * hidden group reference (sequence with hiddenGroupRef property)
   */
  override lazy val isHidden = {
    val res = hiddenGroupRefOption match {
      case Some(_) => true
      case None => someEnclosingComponent.isHidden
    }
    res
  }

  lazy val <sequence>{ apparentXMLChildren @ _* }</sequence> = xml

  lazy val xmlChildren = xmlChildren_.value
  private val xmlChildren_ = LV('xmlChildren) {
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
  }

  lazy val hasStaticallyRequiredInstances = {
    // true if there are syntactic features
    hasInitiator || hasTerminator ||
      // or if any child of the sequence has statically required instances.
      groupMembers.exists { _.hasStaticallyRequiredInstances }
  }

}

class GroupRef(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends GroupBase(xmlArg, parent, position)
  with DFDLStatementMixin
  with GroupRefGrammarMixin
  with Group_AnnotationMixin
  with SeparatorSuppressionPolicyMixin
  with SequenceRuntimeValuedPropertiesMixin
  with HasRefMixin {

  requiredEvaluations(groupDef)

  // delegate to the model group object. It assembles properties from
  // the group ref and the group def
  override def findPropertyOption(pname: String): PropertyLookupResult = {
    val res = group.findPropertyOption(pname)
    res
  }
  lazy val nonDefaultPropertySources = group.nonDefaultPropertySources
  lazy val defaultPropertySources = group.defaultPropertySources

  lazy val prettyBaseName = "group.ref." + localName

  lazy val myPeers = groupRefPeers

  lazy val qname = resolveQName(ref)
  lazy val (namespace, localName) = qname

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

  lazy val groupDef = groupDef_.value
  private val groupDef_ = LV('groupDef) {
    this.schemaSet.getGlobalGroupDef(namespace, localName) match {
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

class GlobalGroupDefFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SchemaComponent(xmlArg, schemaDocumentArg) with NamedMixin {

  def forGroupRef(gref: GroupRef, position: Int) = {
    scala.xml.Utility.trim(xml) match {
      case <group><sequence>{ _* }</sequence></group> =>
        new GlobalSequenceGroupDef(xml, schemaDocument, gref, position)
      case <group><choice>{ _* }</choice></group> =>
        new GlobalChoiceGroupDef(xml, schemaDocument, gref, position)
    }
  }

}

abstract class GlobalGroupDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, val groupRef: GroupRef, position: Int)
  extends SchemaComponent(xmlArg, schemaDocumentArg)
  with GlobalComponentMixin {

  requiredEvaluations(modelGroup)

  override lazy val enclosingComponent = {
    val res = Some(groupRef)
    res
  }
  //
  // Note: Dealing with XML can be fragile. It's easy to forget some of these children
  // might be annotations and Text nodes. Even if you trim the text nodes out, there are
  // places where annotations can be.
  //
  lazy val <group>{ xmlChildren @ _* }</group> = xml
  //
  // So we have to flatMap, so that we can tolerate annotation objects (like documentation objects).
  // and our ModelGroup factory has to return Nil for annotations and Text nodes.
  //
  lazy val Seq(modelGroup: ModelGroup) = xmlChildren.flatMap { GroupFactory(_, this, position) }

}

class GlobalSequenceGroupDef(xmlArg: Node, schemaDocument: SchemaDocument, groupRef: GroupRef, position: Int)
  extends GlobalGroupDef(xmlArg, schemaDocument, groupRef, position)

class GlobalChoiceGroupDef(xmlArg: Node, schemaDocument: SchemaDocument, groupRef: GroupRef, position: Int)
  extends GlobalGroupDef(xmlArg, schemaDocument, groupRef, position)

