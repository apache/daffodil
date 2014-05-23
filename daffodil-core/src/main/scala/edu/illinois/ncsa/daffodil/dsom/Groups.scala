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
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.LV
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.ListUtils
import java.util.UUID

 /**
   * Technique for analyzing and combining charset encoding 
   * information uses a lattice. Top of lattice means
   * "conflicting" information. Bottom of lattice is "no information" 
   * and points in between have specific amounts of information.
   * Basic operation is combine two values of the lattice to 
   * move up the lattice toward the Top. 
   */
  sealed abstract class EncodingLattice
  
  /**
   * This is the Top value for the lattice of knowledge 
   * about encodings. Mixed as in multiple different encodings
   * or a mixture of binary and text data, or some things
   * not known until runtime.
   */
  case object Mixed extends EncodingLattice
  /**
   * Contains binary data (only)
   */
  case object Binary extends EncodingLattice
  /**
   * Means the encoding is determined via a runtime expression.
   */
  case object Runtime extends EncodingLattice
  /**
   * NoText is the bottom of the lattice. We have no information
   * here. Means the item could have text, but just so happens to not 
   * have any, so regardless of what it's encoding is, it doesn't
   * interfere with contained children and parents having a
   * common encoding. Example: A sequence with no alignment region, 
   * and no delimiters. It's not binary, it has no text. It's 
   * nothing really. 
   */
  case object NoText extends EncodingLattice
  
  /**
   * Means we have a named encoding.
   */
  case class Encoding(name: String) extends EncodingLattice

  
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

  /**
   * An integer which is the alignment of this term. This takes into account the
   * representation, type, charset encoding and alignment-related properties.
   */
  def alignmentValueInBits: Int

  /**
   * True if it is sensible to scan this data e.g., with a regular expression.
   * Requires that all children have same encoding as enclosing groups and
   * elements, requires that there is no leading or trailing alignment regions,
   * skips. We have to be able to determine that we are for sure going to
   * always be properly aligned for text.
   * <p>
   * Caveat: we only care that the encoding is the same if the term
   * actually could have text (couldHaveText is an LV) as part of its
   * representation. For example, a sequence
   * with no initiator, terminator, nor separators can have any encoding at all,
   * without disqualifying an element containing it from being scannable. There
   * has to be text that would be part of the scan.
   * <p>
   * If the root element isScannable, and encodingErrorPolicy is 'replace',
   * then we can use a lower-overhead I/O layer - basically we can use a java.io.InputStreamReader
   * directly.
   * <p>
   * We are going to depend on the fact that if the encoding is going to be this
   * US-ASCII-7Bit-PACKED thingy (7-bits wide code units, so aligned at 1 bit) that
   * this encoding must be specified statically in the schema.
   * <p>
   * If an encoding is determined at runtime, then we will
   * insist on it being 8-bit aligned code units.
   */

  final lazy val isScannable: Boolean = {
    if (!this.isRepresented) true
    else {
      val res = summaryEncoding match {
        case Mixed => false
        case Binary => false
        case NoText => false
        case Runtime => false
        case _ => true
      }
      res
    }
  }
  
  /**
   * If s1 and s2 are the same encoding name
   * then s1, else "mixed". Also "notext" combines
   * with anything. 
   */
  def combinedEncoding(
      s1: EncodingLattice, 
      s2: EncodingLattice): EncodingLattice = {
    (s1, s2) match {
      case (x, y) if (x == y) => x
      case (Mixed, _) => Mixed
      case (_, Mixed) => Mixed
      case (Binary, Binary) => Binary
      case (Binary, _) => Mixed
      case (_, Binary) => Mixed
      case (NoText, x) => x
      case (x, NoText) => x
      case (x, y) => Mixed
    }
  }
  
  /**
   * Roll up from the bottom. This is abstract interpretation.
   * The top (aka conflicting encodings) is "mixed"
   * The bottom is "noText" (combines with anything)
   * The values are encoding names, or "runtime" for expressions.
   * <p>
   * By doing expression analysis we could do a better job
   * here and determine when things that use expressions
   * to get the encoding are all going to get the same
   * expression value. For now, if it is an expression
   * then we lose. 
   */
  lazy val summaryEncoding: EncodingLattice = {
    val myEnc = if (!isRepresented) NoText
    else if (!isLocallyTextOnly) Binary
    else if (!couldHaveText) NoText
    else if (!this.isKnownEncoding) Runtime
    else Encoding(this.knownEncodingName)
    val childEncs: Seq[EncodingLattice] = termChildren.map{ x => x.summaryEncoding }
    val res = childEncs.fold(myEnc) { (x, y) => combinedEncoding(x,y) }
    res
  }
  
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
  
  /**
   * Returns true if this term either cannot conflict because it has no textual 
   * aspects, or if it couldHaveText then the encoding must be same.
   */
  def hasCompatibleEncoding(t2: Term) : Boolean = {
    if (!this.couldHaveText) true
    else if (!t2.couldHaveText) true
    else this.knownEncodingCharset == t2.knownEncodingCharset
  }

  /**
   * True if this element itself consists only of text. No binary stuff like alignment
   * or skips.
   * <p>
   * Not recursive into contained children.
   */
  def isLocallyTextOnly: Boolean
  
  //TODO: if we add recursive types capability to DFDL this will have to change
  // but so will many of these compiler passes up and down through the DSOM objects.
  
  /**
   * The termChildren are the children that are Terms, i.e., derived from the Term 
   * base class. This is to make it clear
   * we're not talking about the XML structures inside the XML parent (which might
   * include annotations, etc.
   */
  def termChildren : Seq[Term]

  val tID = UUID.randomUUID()
  
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
  lazy val nearestEnclosingSequence: Option[Sequence] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) => Some(s)
    case Some(_) => enclosingTerm.get.nearestEnclosingSequence
  }

  lazy val nearestEnclosingChoiceBeforeSequence: Option[Choice] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) => None
    case Some(c: Choice) => Some(c)
    case Some(_) => enclosingTerm.get.nearestEnclosingChoiceBeforeSequence
  }

  lazy val nearestEnclosingUnorderedSequence: Option[Sequence] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) if !s.isOrdered => Some(s)
    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  }

  lazy val nearestEnclosingUnorderedSequenceBeforeSequence: Option[Sequence] = enclosingTerm match {
    case None => None
    case Some(s: Sequence) if !s.isOrdered => Some(s)
    case Some(s: Sequence) => None
    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  }

  lazy val inChoiceBeforeNearestEnclosingSequence: Boolean = enclosingTerm match {
    case None => false
    case Some(s: Sequence) => false
    case Some(c: Choice) => true
    case Some(_) => enclosingTerm.get.inChoiceBeforeNearestEnclosingSequence
  }

  lazy val nearestEnclosingElement: Option[ElementBase] = enclosingTerm match {
    case None => None
    case Some(eb: ElementBase) => Some(eb)
    case Some(_) => enclosingTerm.get.nearestEnclosingElement
  }

  lazy val thisTermNoRefs: Term = thisTermNoRefs_.value
  private val thisTermNoRefs_ = LV('thisTermNoRefs) {
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
  }

  /**
   * We want to determine if we're in an unordered sequence
   * at any point along our parents.
   */
  lazy val inUnorderedSequence: Boolean =
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

  lazy val immediatelyEnclosingModelGroup: Option[ModelGroup] = {
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

  //  lazy val hasLaterRequiredSiblings: Boolean = hasRequiredSiblings(ListUtils.tailAfter _)
  //  lazy val hasPriorRequiredSiblings: Boolean = hasRequiredSiblings(ListUtils.preceding _)
  //
  //  def hasRequiredSiblings(splitter: ListUtils.SubListFinder[Term]) = {
  //    val res = nearestEnclosingSequence.map { es =>
  //      {
  //        val allSiblings = es.groupMembers.map { _.referredToComponent }
  //        val sibs = splitter(allSiblings, this)
  //        val hasAtLeastOne = sibs.find { term => term.hasStaticallyRequiredInstances }
  //        hasAtLeastOne != None
  //      }
  //    }.getOrElse(false)
  //    res
  //  }

  lazy val allSiblings: Seq[Term] = {
    val res = nearestEnclosingSequence.map { enc =>
      val allSiblings = enc.groupMembers.map { _.referredToComponent }
      allSiblings
    }
    res.getOrElse(Nil)
  }

  lazy val priorSiblings = ListUtils.preceding(allSiblings, this)
  lazy val laterSiblings = ListUtils.tailAfter(allSiblings, this)

  lazy val priorSibling = priorSiblings.lastOption
  lazy val nextSibling = laterSiblings.headOption

  lazy val hasLaterRequiredSiblings = laterSiblings.exists(_.hasStaticallyRequiredInstances)
  lazy val hasPriorRequiredSiblings = priorSiblings.exists(_.hasStaticallyRequiredInstances)

  def hasStaticallyRequiredInstances: Boolean
  def isKnownRequiredElement = false
  def isKnownToBePrecededByAllByteLengthItems: Boolean = false
  def hasKnownRequiredSyntax = false

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

  lazy val immediateGroup: Option[ModelGroup] = {
    val res: Option[ModelGroup] = this.group match {
      case (s: Sequence) => Some(s)
      case (c: Choice) => Some(c)
      case _ => None
    }
    res
  }

  lazy val alignmentValueChildren: Int = {
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
  lazy val alignmentValueInBits: Int = {
    this.alignment match {
      case AlignmentType.Implicit => alignmentValueChildren
      case align: Int => this.alignmentUnits match {
        case AlignmentUnits.Bits => align
        case AlignmentUnits.Bytes => 8 * align
      }
    }
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

  val mgID = UUID.randomUUID()

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
  
  override
  lazy val termChildren = groupMembers
  
  override
  lazy val isLocallyTextOnly = {
    this.hasNoSkipRegions &&
    this.hasTextAlignment
  }

  lazy val groupMembersNoRefs = groupMembers.map {
    case eRef: ElementRef => eRef.referencedElement
    case gb: GroupBase => gb.group
    case x => x
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

  override lazy val isKnownToBePrecededByAllByteLengthItems: Boolean = {
    val es = nearestEnclosingSequence
    es match {
      case None => true
      case Some(s) => {
        if (s.groupMembers.head eq this) s.isKnownToBePrecededByAllByteLengthItems
        else {
          //pass for now
          val index = s.groupMembers.indexOf(this)
          s.groupMembers.slice(0, index).forall { _.isKnownToBePrecededByAllByteLengthItems }
        }
      }
    }
  }

  lazy val isKnownToBeAligned = isKnownToBeAligned_.value
  private val isKnownToBeAligned_ = LV('isKnownToBeAligned) {
    if (alignmentValueInBits == 1) {
      alignmentUnits match {
        case AlignmentUnits.Bits => true
        case AlignmentUnits.Bytes => isKnownToBePrecededByAllByteLengthItems
      }
    } else if (alignmentValueInBits > 1) {
      isKnownToBePrecededByAllByteLengthItems
    } else false
  }

  lazy val isDeclaredLastInSequence = isDeclaredLastInSequence_.value
  private val isDeclaredLastInSequence_ = LV('isDeclaredLastInSequence) {
    val es = nearestEnclosingSequence
    // how do we determine what child node we are? We search. 
    // TODO: better structure for O(1) answer to this.
    es match {
      case None => Assert.invariantFailed("We are not in a sequence therefore isDeclaredLastInSequence is an invalid question.")
      case Some(s) =>
        {
          val members = s.groupMembersNoRefs

          if (members.last eq thisTermNoRefs) true // we want object identity comparison here, not equality. 
          else false
        }
    }
  }

  lazy val allSelfContainedTermsTerminatedByRequiredElement: Seq[Term] =
    allSelfContainedTermsTerminatedByRequiredElement_.value
  private val allSelfContainedTermsTerminatedByRequiredElement_ =
    LV('allSelfContainedTermsTerminatedByRequiredElement) {
      val listOfTerms = groupMembersNoRefs.map(m => {
        m match {
          case e: LocalElementBase if e.isOptional => (Seq(e) ++ e.couldBeNext) // A LocalElement or ElementRef
          case e: LocalElementBase => Seq(e)
          case mg: ModelGroup => Seq(mg)
        }
      }).flatten
      listOfTerms
    }

  lazy val couldBeNext: Seq[Term] = couldBeNext_.value
  private val couldBeNext_ = LV('couldBeNext) {
    // We're a ModelGroup, we want a list of all
    // Terms that follow this ModelGroup.
    //
    val es = this.nearestEnclosingSequence
    val eus = this.nearestEnclosingUnorderedSequenceBeforeSequence
    val ec = this.nearestEnclosingChoiceBeforeSequence

    val enclosingUnorderedGroup = {
      (ec, eus) match {
        case (None, None) => None
        case (Some(choice), _) => Some(choice)
        case (None, Some(uoSeq)) => Some(uoSeq)
      }
    }

    val listOfNextTerm =
      (enclosingUnorderedGroup, es) match {
        case (None, None) => Seq.empty
        case (Some(unorderedGroup), _) => {
          // We're in a choice or unordered sequence

          // List must be all of our peers since
          // we could be followed by any of them plus
          // whatever follows the unordered group.
          val peersCouldBeNext = unorderedGroup.groupMembersNoRefs
          peersCouldBeNext
        }
        case (None, Some(oSeq)) => {
          // We're in an ordered sequence

          val termsUntilFirstRequiredTerm =
            isDeclaredLastInSequence match {
              case true => oSeq.couldBeNext
              case false => {

                val members = oSeq.group.groupMembersNoRefs

                val nextMember = members.dropWhile(m => m != thisTermNoRefs).filterNot(m => m == thisTermNoRefs).headOption

                val nextMembers =
                  nextMember match {
                    case Some(e: LocalElementBase) if e.isOptional => Seq(e) ++ e.couldBeNext
                    case Some(e: LocalElementBase) => Seq(e)
                    case Some(gb: GroupBase) => Seq(gb.group)
                    case None => Assert.impossibleCase
                  }
                nextMembers
              }
            }
          termsUntilFirstRequiredTerm
        }
      }
    listOfNextTerm
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

  override lazy val hasKnownRequiredSyntax = hasKnownRequiredSyntax_.value
  private val hasKnownRequiredSyntax_ = LV('hasKnownRequiredSyntax) {
    if (hasInitiator || hasTerminator) true
    else if (isKnownToBeAligned) true
    else groupMembers.forall(_.hasKnownRequiredSyntax)
  }
}

class Sequence(xmlArg: Node, parent: SchemaComponent, position: Int)
  extends ModelGroup(xmlArg, parent, position)
  with Sequence_AnnotationMixin
  with SequenceRuntimeValuedPropertiesMixin
  with SequenceGrammarMixin
  with SeparatorSuppressionPolicyMixin {

  requiredEvaluations(checkIfValidUnorderedSequence)
  
  lazy val myPeers = sequencePeers
  
  override
  lazy val hasDelimiters = hasInitiator || hasTerminator || hasSeparator
  
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

  override lazy val hasKnownRequiredSyntax = hasKnownRequiredSyntax_.value
  private val hasKnownRequiredSyntax_ = LV('hasKnownRequiredSyntax) {
    if (hasInitiator || hasTerminator) true
    else if (isKnownToBeAligned) true
    else groupMembers.exists(_.hasKnownRequiredSyntax)
  }

  /**
   * Provides unordered sequence checks.  Will SDE if invalid.
   */
  def checkIfValidUnorderedSequence(): Unit = {
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
        "\nThe offending members: %s.", this.nameAndPath._2, invalidChildren.mkString(","))
  }
  private def checkMembersAreAllElementOrElementRef: Unit = {
    val invalidChildren = groupMembers.filterNot(child =>
      child.isInstanceOf[LocalElementDecl] || child.isInstanceOf[ElementRef])
    val hasInvalidChildren = invalidChildren.length > 0
    if (hasInvalidChildren)
      this.SDE("Members of an unordered sequence (%s) must be Element or ElementRef." +
        "\nThe offending members: %s.", this.nameAndPath._2, invalidChildren.mkString(","))
  }
  private def checkMembersHaveUniqueNamesInNamespaces: Unit = {
    val childrenGroupedByNamespace =
      groupMembers.filter(m => m.isInstanceOf[ElementBase]).map(_.asInstanceOf[ElementBase]).groupBy(_.targetNamespace.toJDOM)

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
                this.nameAndPath._2, ns, name)
        }
      }
    }
  }

  lazy val isOrdered: Boolean = this.sequenceKind match {
    case SequenceKind.Ordered => true
    case SequenceKind.Unordered => false
  }

  lazy val unorderedSeq: Option[UnorderedSequence] = if (!isOrdered) {

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

}

class UnorderedSequence(xmlArg: Node, xmlContents: Seq[Node], parent: SchemaComponent, position: Int)
  extends Sequence(xmlArg, parent, position) {
  // A shell, the actual XML representation is passed in
  // from Sequence
}

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
  
  override
  lazy val termChildren: Seq[Term] = {
		  group.termChildren
  }

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
  
  override 
  lazy val couldHaveText = group.couldHaveText
  
  override
  lazy val isLocallyTextOnly = group.isLocallyTextOnly

  override lazy val referredToComponent = group

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

  lazy val referringComponent = {
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

