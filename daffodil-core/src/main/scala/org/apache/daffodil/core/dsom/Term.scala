/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.dsom

import java.lang.{ Integer => JInt }
import java.util.UUID

import org.apache.daffodil.core.dsom.walker.TermView
import org.apache.daffodil.core.grammar.TermGrammarMixin
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.schema.annotation.props.Found
import org.apache.daffodil.lib.schema.annotation.props.NotFound
import org.apache.daffodil.lib.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.lib.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo

/**
 * Mixin for objects that are shared, but have consistency checks to be run
 * that are based on the concrete Term objects they are associated with.
 *
 * E.g., DFDL statements may have checks that need to know the encoding
 * (if it is known at compile time). We call this on each statement to enable
 * the checking code to be expressed on that statement where it is relevant,
 * but have it be callable from the concrete Term once it is created.
 *
 * This is a way to avoid use of backpointers from shared objects to every
 * thing referencing them.
 */
trait HasTermCheck {

  /**
   * Perform checking of an object against the supplied Term arg.
   */
  final def checkTerm(term: Term): Unit = {
    //
    // This public method calling a protected method lets us play tricks
    // in the future to avoid repeated check calls by memoizing the
    // results.
    //
    check(term)
  }

  /**
   * Override to perform necessary checks that require information about the
   * concrete Term.
   *
   * This avoids the need for the checking code to have a backpointer to the
   * Term.
   */
  protected def check(term: Term): Unit = {
    // by default this does nothing.
  }
}

/**
 * Term, and what is and isn't a Term, is a key concept in DSOM.
 *
 * From the kinds of elements, ElementRef and LocalElementDecl are Term. A GlobalElementDecl is *not* a Term.
 * From the kinds of sequences, LocalSequence and SequenceGroupRef are Term. GlobalSequenceGroupDef is *not* a Term.
 * From the kinds of choices, Choice and ChoiceGroupRef are Term. GlobalChoiceGroupDef is *not* a Term.
 *
 * Terms are the things we actually generate parsers/unparsers for. Non-Terms just
 * contribute information used by Terms.
 */
trait Term
  extends AnnotatedSchemaComponent
  with ResolvesScopedProperties
  with ResolvesDFDLStatementMixin
  with TermRuntimeValuedPropertiesMixin
  with TermGrammarMixin
  with DelimitedRuntimeValuedPropertiesMixin
  with InitiatedTerminatedMixin
  with TermEncodingMixin
  with EscapeSchemeRefMixin
  with TermView {

  requiredEvaluationsIfActivated(annotationObjs)
  requiredEvaluationsIfActivated(nonDefaultPropertySources)
  requiredEvaluationsIfActivated(defaultPropertySources)
  requiredEvaluationsIfActivated(termChecks)

  private lazy val termChecks = {
    statements.foreach { _.checkTerm(this) }
  }

  /**
   * Abbreviation analogous to trd, tci is the compile-time counterpart.
   */
  final def tci = dpathCompileInfo

  /**
   * Used to recursively go through Terms and look for DFDL properties that
   * have not been accessed and record it as a warning. This function uses the
   * property cache state to determine which properties have been access, so
   * this function must only be called after all property accesses are complete
   * (e.g. schema compilation has finished) to ensure there are no false
   * positives.
   */
  final lazy val checkUnusedProperties: Unit = {
    // Get the properties defined on this term and what it refers to
    val localProps = formatAnnotation.justThisOneProperties
    val refProps = optReferredToComponent
      .map { _.formatAnnotation.justThisOneProperties }
      .getOrElse(Map.empty)

    val usedProperties = propCache

    localProps.foreach { case (prop, (value, _)) =>
      if (!usedProperties.contains(prop)) {
        SDW(WarnID.IgnoreDFDLProperty, "DFDL property was ignored: %s=\"%s\"", prop, value)
      }
    }

    refProps.foreach { case (prop, (value, _)) =>
      if (!usedProperties.contains(prop)) {
        optReferredToComponent.get.SDW(
          WarnID.IgnoreDFDLProperty,
          "DFDL property was ignored: %s=\"%s\"",
          prop,
          value
        )
      }
    }

    termChildren.foreach { _.checkUnusedProperties }
  }

  def position: Int

  def optIgnoreCase: Option[YesNo] = {
    val ic = findPropertyOption("ignoreCase")
    ic match {
      case Found(value, location, _, _) => Some(YesNo(value, location))
      case _ => None
    }
  }

  /**
   * A scalar means has no dimension. Exactly one occurrence.
   *
   * Since terms include both model groups and elements, in DFDL v1.0,
   * model groups are always scalar, as DFDL v1.0 doesn't allow min/max
   * occurs on model groups.
   */
  def isScalar: Boolean

  /**
   * Determines if the element is optional, as in has zero or one instance only.
   *
   * There are two senses of optional
   *
   * 1) Optional as in "might not be present" but for any reason.
   * Consistent with this is Required meaning must occur but for any
   * reason. So all the occurrences of an array that has fixed number of
   * occurrences are required, and some of the occurrences of an array
   * that has a variable number of occurrences are optional.
   *
   * 2) Optional is in minOccurs="0" maxOccurs="1".
   *
   * Consistent with (2) is defining array as maxOccurs >= 2, and Required
   * as minOccurs=maxOccurs=1, but there are also special cases for occursCountKind parsed and stopValue
   * since they don't examine min/max occurs - they are only used for validation
   * in those occursCountKinds.
   *
   * The DFDL spec is not entirely consistent here either I don't believe.
   */
  override def isOptional: Boolean

  /**
   * An array can have more than 1 occurrence.
   *
   * An optional element (minOccurs=0, maxOccurs=1) is an array only
   * if occursCountKind is parsed, because then the max/min are ignored.
   */
  override def isArray: Boolean

  def elementChildren: Seq[ElementBase]

  /**
   * An integer which is the alignment of this term. This takes into account the
   * representation, type, charset encoding and alignment-related properties.
   */
  def alignmentValueInBits: JInt

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

  // TODO: if we add recursive types capability to DFDL this will have to change
  // but so will many of these compiler passes up and down through the DSOM objects.

  /**
   * The termChildren are the children that are Terms, i.e., derived from the Term
   * base class. This is to make it clear
   * we're not talking about the XML structures inside the XML parent (which might
   * include annotations, etc.
   *
   * For elements this is Nil for simple types, a single model group for
   * complex types. For model groups there can be more children.
   */
  def termChildren: Seq[Term]

  final val tID = UUID.randomUUID()

  final lazy val isRepresented = this match {
    case eb: ElementBase => {
      val isRep = eb.inputValueCalcOption.isInstanceOf[NotFound]
      if (!isRep) {
        if (isOptional) {
          SDE("inputValueCalc property can not appear on optional elements")
        }
        if (!isScalar) {
          SDE("inputValueCalc property can not appear on array elements")
        }
      }
      isRep
    }
    case _ => true
  }

  /**
   * Answers whether this term appears anywhere inside an unordered sequence.
   *
   * A term can be in both ordered and unordered sequences if it appears in a global
   * sequence def, which has refs to it that specify sequenceKind='ordered' on some
   * and sequenceKind='unordered' on others of the group refs.
   */
  final lazy val isEverInUnorderedSequence: Boolean = {
    optLexicalParent
      .map {
        case s: SequenceTermBase => !s.isOrdered
        case gsd: GlobalSequenceGroupDef => {
          gsd.schemaSet.root.groupRefsTo(gsd).exists { case sgr: SequenceGroupRef =>
            !sgr.isOrdered
          }
        }
        case c: ChoiceDefMixin => false
        case ct: ComplexTypeBase => false
        case x => Assert.invariantFailed("Unexpected lexical parent: " + x)
      }
      .getOrElse(false)
  }

  final lazy val immediatelyEnclosingGroupDef: Option[GroupDefLike] = {
    optLexicalParent.flatMap { lexicalParent =>
      val res: Option[GroupDefLike] = lexicalParent match {
        case c: Choice => Some(c)
        //
        // skip past the implied sequence that is wrapped around choice branches
        // to the actual choice
        //
        case c: ChoiceBranchImpliedSequence => c.immediatelyEnclosingGroupDef
        case s: LocalSequence => Some(s)
        case d: SchemaDocument => {
          // we must be the Root elementRef or a quasi node
          Assert.invariant(this.isInstanceOf[Root] || this.isInstanceOf[QuasiElementDeclBase])
          None
        }
        case gdd: GlobalGroupDef => Some(gdd)
        case ctd: ComplexTypeBase => None
        case rt: RepTypeQuasiElementDecl => rt.immediatelyEnclosingGroupDef
        case std: SimpleTypeBase => None
        case _ =>
          Assert.invariantFailed(
            "immediatelyEnclosingModelGroup called on " + this + " with lexical parent " + lexicalParent
          )
      }
      res
    }
  }

  lazy val immediatelyEnclosingModelGroup: Option[ModelGroup] =
    immediatelyEnclosingGroupDef.flatMap {
      _ match {
        case mg: ModelGroup => Some(mg)
        case _ => None
      }
    }

  /**
   * Prior using one-based position in the enclosing lexical sequence.
   *
   * Nil if enclosed by a choice def, or this is the root.
   */
  final lazy val priorSiblings = {
    optLexicalParent match {
      case Some(stb: SequenceTermBase) => stb.groupMembers.take(position - 1)
      case Some(gsgd: GlobalSequenceGroupDef) => gsgd.groupMembers.take(position - 1)
      case _ => Nil
    }
  }

  /**
   * Siblings after this in the lexically enclosing group.
   *
   * Nil if enclosed by a choice def, or this is the root.
   */
  final lazy val laterSiblings = {
    optLexicalParent match {
      case Some(stb: SequenceTermBase) => stb.groupMembers.drop(position)
      case Some(gsgd: GlobalSequenceGroupDef) => gsgd.groupMembers.drop(position)
      case _ => Nil
    }
  }

  /**
   * Siblings (including self) in the lexically enclosing sequence def.
   *
   * Nil if enclosed by a choice def, or this is the root.
   */
  final lazy val allSiblings = {
    optLexicalParent match {
      case Some(stb: SequenceTermBase) => stb.groupMembers
      case Some(gsgd: GlobalSequenceGroupDef) => gsgd.groupMembers
      case _ => Nil
    }
  }

  // final lazy val laterElementSiblings = laterSiblings.collect { case elt: ElementBase => elt }

  final lazy val priorSibling = priorSiblings.lastOption
  final lazy val nextSibling = laterSiblings.headOption

  /**
   * Does this term have always have statically required instances in the data stream.
   *
   * This excludes elements that have no representation e.g., elements with dfdl:inputValueCalc.
   *
   * Terms that are optional either via element having zero occurrences, or via a choice branch
   * fail this test.
   */
  def hasStaticallyRequiredOccurrencesInDataRepresentation: Boolean

  /**
   * True if the term has some syntax itself or recursively within itself that
   * must appear in the data stream.
   *
   * False only if the term has possibly no representation whatsoever in the
   * data stream.
   */
  def hasKnownRequiredSyntax: Boolean

  /**
   * Can have a varying number of occurrences.
   *
   * Overridden for elements. See [[ParticleMixin.isVariableOccurrences]]
   */
  def isVariableOccurrences: Boolean = false

  /**
   * The concept of potentially trailing is defined in the DFDL specification.
   *
   * This concept applies to terms that are direct children of a sequence only.
   *
   * It is true for terms that may be absent from the representation, but furthermore, may be last
   * in a sequence, so that the notion of whether they are trailing, and so their separator may not be
   * present, is a relevant issue.
   *
   * If an element is an array, and has some required instances, then it is not potentially trailing, as some
   * instances will have to appear, with separators.
   *
   * This concept applies only to elements and model groups that have representation in the data stream.
   *
   * Previously there was a misguided notion that since only DFDL elements can have minOccurs/maxOccurs
   * that this notion of potentially trailing didn't apply to model groups. (Sequences and Choices, the other
   * kind of Term). But this is not the case.
   *
   * A sequence/choice which has no framing, and whose content doesn't exist - no child elements, any contained
   * model groups recursively with no framing and no content - such a model group effectively "dissapears" from
   * the data stream, and in some cases need not have a separator.
   *
   * This is computed by way of couldBePotentiallyTrailing. This value means that the term, in isolation, looking only
   * at its own characteristics, disregarding its following siblings in any given sequence, has the characteristics
   * of being potentially trailing.
   *
   * Then that is combined with information about following siblings in a sequence to determine if a given term, that
   * is a child of a sequence, is in fact potentially trailing within that sequence.
   *
   * These two concepts are mutually recursive, since a sequence that is entirely composed of potentially trailing children
   * satisfies couldBePotentialyTrailing in whatever sequence encloses it.
   */
  final lazy val isPotentiallyTrailing: Boolean = {
    val thisCouldBe = couldBePotentiallyTrailing
    lazy val laterSibilingsAre = laterSiblings.forall { _.isPotentiallyTrailing }
    val res = thisCouldBe && laterSibilingsAre
    res
  }

  final lazy val couldBePotentiallyTrailing: Boolean = {
    import SeparatorSuppressionPolicy._
    this match {
      case e: ElementBase => {
        lazy val allowsZeroOccurs = e.minOccurs == 0
        lazy val minOccursNotZeroButDeclaredLast =
          !e.isScalar && e.minOccurs > 0 && e.isLastDeclaredRepresentedInSequence
        lazy val hasAllowedOCK = (e.occursCountKind eq OccursCountKind.Implicit) ||
          (e.occursCountKind eq OccursCountKind.Parsed)
        lazy val hasAllowedLengthKind = e.lengthKind eq LengthKind.Delimited
        lazy val hasNoDiscriminators = !statements.exists { s =>
          s.isInstanceOf[DFDLDiscriminator]
        }
        val res =
          isRepresented &&
            (allowsZeroOccurs ||
              minOccursNotZeroButDeclaredLast) &&
            hasAllowedOCK &&
            hasAllowedLengthKind &&
            hasNoDiscriminators
        res
      }
      case m: ModelGroup => {
        lazy val seqIsNotSSPNever = m match {
          case s: SequenceTermBase =>
            s.hasSeparator &&
            (s.separatorSuppressionPolicy match {
              case TrailingEmpty => true
              case TrailingEmptyStrict => true
              case AnyEmpty => true
              case Never => false
            })
          case _ => true
        }
        lazy val hasNoStatements = statements.length == 0
        lazy val recursivelyOk =
          m.representedMembers.forall { m =>
            m.couldBePotentiallyTrailing
          }
        val res =
          isRepresented &&
            !m.hasFraming &&
            seqIsNotSSPNever &&
            hasNoStatements &&
            recursivelyOk
        res
      }
    }
  }

  /**
   * Returns a list of sibling terms that could appear before this.
   *
   * Uses only lexically enclosing group/groupDef
   *
   * Nil if enclosed by a choice or this is root.
   */
  lazy val potentialPriorTerms: Seq[Term] = LV(Symbol("potentialPriorTerms")) {
    potentialPriorTermsDef
  }.value

  private def potentialPriorTermsDef = {
    optLexicalParent.toSeq.flatMap { lp =>
      lp match {
        case sq: SequenceTermBase if !sq.isOrdered =>
          sq.groupMembers.filter { _.isRepresented }
        case sq: SequenceDefMixin => {
          val psibs = priorSiblings
          val representedPriorSiblings = psibs.filter { _.isRepresented }
          val (optionalPotentialPriorReversed, requiredPotentialPriorReversed) =
            representedPriorSiblings.reverse.to(LazyList).span { sib =>
              sib match {
                case eb: ElementBase if eb.isScalar =>
                  false
                case _ =>
                  true
              }
            }
          val optionalPotentialPrior = optionalPotentialPriorReversed.reverse
          val firstNonOptional = requiredPotentialPriorReversed.headOption
          optionalPotentialPrior ++ firstNonOptional
        }
        case _ => Nil
      }
    }
  }

  /*
   * This function returns a boolean if the values of the term can be figured
   * out during unparsing or if they don't need to appear in the infoset at all.
   *
   * Usually this means the term/its descendants have a default value (i.e defaultable),
   * have defined dfdl:outputValueCalc, or are optional (minOccurs=0)
   *
   * Note that this currently only requires OVC and Optionality since defaults
   * aren't fully implemented everywhere. This function may need to change when
   * defaults are fully implemented.
   */
  lazy val canUnparseIfHidden: Boolean = {
    val res = this match {
      case s: SequenceTermBase => {
        s.groupMembers.forall { member =>
          val res = member.canUnparseIfHidden
          res
        }
      }
      case c: ChoiceTermBase => {
        c.groupMembers.exists { _.canUnparseIfHidden }
      }
      case e: ElementBase if e.isComplexType => {
        e.complexType.group.canUnparseIfHidden
      }
      case e: ElementBase => {
        !e.isRepresented || e.canBeAbsentFromUnparseInfoset
      }
    }
    res
  }

  /**
   * True if this term is the last one in the enclosing sequence that is represented
   * in the data stream. That is, it is not an element with dfdl:inputValueCalc.
   *
   * This means whether the enclosing sequence's separator (if one is defined) is
   * relevant.
   */
  final lazy val isLastDeclaredRepresentedInSequence = {
    val res = laterSiblings.forall(!_.isRepresented)
    res
  }

  final protected lazy val realChildren: Seq[Term] = {
    this match {
      case mg: ModelGroup => mg.groupMembers.asInstanceOf[Seq[Term]]
      case eb: ElementBase if (eb.isComplexType) => Seq(eb.complexType.group)
      case eb: ElementBase => Seq()
    }
  }

}
