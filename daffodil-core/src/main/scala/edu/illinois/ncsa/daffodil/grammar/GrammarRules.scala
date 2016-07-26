//package edu.illinois.ncsa.daffodil.grammar
//
//import edu.illinois.ncsa.daffodil.dsom._
//import edu.illinois.ncsa.daffodil.processors.UnicodeByteOrderMark
//
///**
// * Consolidate the grammar in one file - that contains only grammar rules, not
// * ad-hoc logic.
// *
// * To the extent possible, these grammar rules should match the DFDL spec
// * section that defines the data grammar.
// *
// * Long lines are ok in this file.
// *
// * Conventions:
// *
// * non-terminals of the grammar start lowercase
// * Terminals of the grammar start uppercase
// * Primitives (which are terminals) take one parameter which is always (this)
// *
// * There are two flavors of combinators:
// *
// * def combinators - these begin lower case
// *
// * A def combinator is just a method that takes Gram arguments and combines
// * them somehow. It should take Gram arguments by name, and should never emit something
// * which uses an argument more than once. A big combinatoric explosion of tree size
// * for the grammar occurs if anything is used more than once.
// *
// * Class combinators - these begin upper case, and are defined as case classes
// * elsewhere. These are generally used when a true parser or unparser combinator
// * is going to be needed, and we're not just recombining things using
// * sequencing and alternatives of the grammar.
// *
// * Grammar principles:
// *
// * Try hard not to need different grammar structures for parsing and unparsing.
// * E.g., for text numbers, we have a string term, and a converter term. This
// * detail doesn't belong in this file, and the fact that these are used in
// * opposite orders parsing and unparsing isn't a detail that should be visible
// * to the grammar at all. A class combinator should deal with that.
// */
//
//trait GlobalElementDeclGrammarRulesMixin
//    extends LocalElementGrammarMixin // can be repeating if not root
//    { self: GlobalElementDecl =>
//
//  final lazy val document = prod("document") {
//    schemaDefinitionUnless(isScalar, "The document element cannot be an array.")
//    UnicodeByteOrderMark(this) ~ documentElement
//  }
//
//  private lazy val documentElement = enclosedElement // simpleElement || complexElement
//
//}
//
///**
// * Matches the grammar in the DFDL Spec v1.0 with the exception of where some
// * combinators are clearly needed to coordinate parsing/unparsing but the
// * original grammar was just sequencing (~) or alternating (|) them.
// *
// * Note below that there are no "|" operators - that was a alternative runtime
// * combinator and really we can't get by with that non-specialized a combinator.
// * The "||" combinator is actually a grammar-time thing. Only one side or the 
// * other survives, so that one is ok to use in these grammar rules. 
// *
// * We do have ordinary sequencing - however it is for when the two grammar terms
// * being sequenced truly have little or nothing to do with each other beyond
// * juxtaposition.
// */
//trait GrammarRulesMixin {
//
//  /*
//   * Problem: all these productions are on one object. Not distributed across
//   * the DSOM objects which are relevant to them. 
//   * 
//   * To work, the combinator defs, and the terminals have to be defined
//   * on the corresponding DSOM classes so that they can inspect the relevant
//   * properties to decide what happens with their argument grammar terms.
//   * 
//   * So, as before, this has to get cut up into:
//   * TermGrammarRulesMixin
//   * ElementGrammarRulesMixin
//   * ModelGroupGrammarRulesMixin
//   * SequenceGrammarRulesMixin
//   * ChoiceGrammarRulesMixin
//   * 
//   * Note that above we already have GlobalElementDeclGrammarRulesMixin. We may
//   * need/want the split into LocalElementDeclGrammarRulesMixin, and 
//   * 
//   * (Separate compilation of types may
//   * require us to have SimpleTypeGrammarRulesMixin and ComplexTypeGrammarRulesMixin
//   * that pick up many of the grammar terms from ElementGrammarRulesMixin.
//   * 
//   * Note that the terminals and combinators being used similarly must be
//   * organized on the right objects so that they have access to what is needed
//   * from those DSOM objects. 
//   */
//
//  import TerminalsAndCombinators._
//
//  protected final lazy val simpleElement = simpleElementDef(simpleLiteralNilElementRep, simpleEmptyElementRep, simpleNormalRep)
//
//  private lazy val simpleEnclosedElement = simpleElement || absentElementRep
//
//  protected final lazy val complexElement = complexElementDef(complexLiteralNilElementRep, complexNormalRep, complexEmptyElementRep)
//
//  private lazy val complexEnclosedElement = complexElement || absentElementRep
//
//  private lazy val enclosedElement = simpleEnclosedElement || complexEnclosedElement
//
//  ////////////////////////////////////////////////////////////////////////////
//
//  private def absentElementRep = Absent
//
//  /////////////////////////////////////////////////////////////////////////////
//
//  private lazy val simpleEmptyElementRep = simpleEmptyElementRepDef(emptyElementLeftFraming, emptyElementRightFraming)
//
//  private lazy val complexEmptyElementRep = complexEmptyElementRepDef(emptyElementLeftFraming, emptyElementRightFraming)
//
//  private lazy val emptyElementLeftFraming = leadingAlignment ~ EmptyElementInitiator ~ prefixLength
//  private lazy val emptyElementRightFraming = EmptyElementTerminator ~ trailingAlignment
//
//  /////////////////////////////////////////////////////////////////////////////
//
//  private lazy val simpleLiteralNilElementRep = simpleLiteralNilElementRepDef(nilElementLeftFraming, (NilLiteralCharacters ||
//    nilElementLiteralContent), nilElementRightFraming)
//
//  private lazy val complexLiteralNilElementRep = complexLiteralNilElementRepDef(nilElementLeftFraming, NilLiteralValue, nilElementRightFraming)
//
//  private lazy val nilElementLeftFraming = leadingAlignment ~ NilElementInitiator ~ prefixLength
//
//  private lazy val nilElementRightFraming = NilElementTerminator ~ trailingAlignment
//
//  private lazy val nilElementLiteralContent = LeftPadding ~ NilLiteralValue ~ rightPadOrFill
//
//  /////////////////////////////////////////////////////////////////////////////
//
//  private lazy val simpleNormalRep = simpleNormalRepDef(leftFraming, prefixLength, simpleContent, rightFraming)
//
//  // Correction has been made to the DFDL Spec 
//  // introduce complexValue, and complexContent encompasses the ElementUnused.
//
//  private lazy val complexNormalRep = complexNormalRepDef(leftFraming, prefixLength, complexContent, rightFraming)
//
//  private lazy val complexContent = complexContentDef(complexValue, ElementUnused)
//
//  private lazy val prefixLength = simpleContent || (prefixPrefixLength ~ simpleContent)
//
//  private lazy val prefixPrefixLength = simpleContent
//
//  private lazy val simpleContent = simpleContentDef(LeftPadding, NilLogicalValue, SimpleValue, rightPadOrFill)
//
//  private lazy val complexValue = sequence || choice
//
//  // Correction to grammar. No reason for the 
//  // alternatives in the grammar rule for rightPadOrFill
//  private lazy val rightPadOrFill = RightPadding ~ RightFill
//
//  //////////////////////////////////////////////////////////////////////////////
//
//  private lazy val sequence = leftFraming ~ sequenceContent ~ rightFraming
//
//  private lazy val sequenceContent = sequenceContentDef(PrefixSeparator, enclosedContent, Separator, PostfixSeparator)
//
//  private lazy val choice = leftFraming ~ choiceContent ~ rightFraming
//
//  private lazy val choiceContent = choiceDef(enclosedContent, ChoiceUnused)
//
//  private lazy val enclosedContent: Gram = enclosedContentDef(enclosedElement, array, sequence, choice)
//
//  private lazy val array = arrayDef(enclosedElement, Separator, stopValue)
//
//  private lazy val stopValue = simpleElement
//
//  /////////////////////////////////////////////////////////////////////////////
//  private lazy val leftFraming = leadingAlignment ~ Initiator
//  private lazy val rightFraming = Terminator ~ trailingAlignment
//
//  private lazy val leadingAlignment = LeadingSkip ~ AlignmentFill
//  private lazy val trailingAlignment = TrailingSkip
//
//  private object TerminalsAndCombinators {
//
//    def Absent: Gram = EmptyGram
//
//    def simpleElementDef(simpleLiteralNilElementRep: => Gram, simpleEmptyElementRep: => Gram, simpleNormalRep: => Gram): Gram = {
//      ???
//    }
//
//    def complexElementDef(complexLiteralNilElementRep: => Gram, complexEmptyElementRep: => Gram, complexNormalRep: => Gram): Gram = {
//      ???
//    }
//
//    /*
//   * Emptyness
//   */
//    def simpleEmptyElementRepDef(emptyElementLeftFraming: => Gram, emptyElementRightFraming: => Gram): Gram = {
//      ???
//    }
//
//    def complexEmptyElementRepDef(emptyElementLeftFraming: => Gram, emptyElementRightFraming: => Gram): Gram = {
//      ???
//    }
//
//    lazy val EmptyElementInitiator: Gram = ???
//
//    lazy val EmptyElementTerminator: Gram = ???
//
//    /*
//   * Nil Rep
//   */
//
//    def simpleLiteralNilElementRepDef(nilElementLeftFraming: => Gram, nilLitCharsOrNilLit: => Gram, nilElementRightFraming: => Gram): Gram = {
//      ???
//    }
//
//    def complexLiteralNilElementRepDef(nilElementLeftFraming: => Gram, nilLit: => Gram, nilElementRightFraming: => Gram): Gram = {
//      ???
//    }
//
//    lazy val NilLiteralCharacters: Gram = ???
//    lazy val NilLiteralValue: Gram = ???
//    lazy val NilElementInitiator: Gram = ???
//    lazy val NilElementTerminator: Gram = ???
//
//    lazy val NilLogicalValue: Gram = ???
//
//    /**
//     * Normal Rep
//     */
//
//    lazy val SimpleValue: Gram = ???
//
//    def simpleContentDef(leftPadding: => Gram, nilLogicalValue: => Gram, simpleValue: => Gram, rightPadOrFill: => Gram): Gram = {
//      ???
//    }
//
//    def simpleNormalRepDef(leftFraming: => Gram, prefixLength: => Gram, simpleContent: => Gram, rightFraming: => Gram): Gram = {
//      ???
//    }
//
//    def complexNormalRepDef(leftFraming: => Gram, prefixLength: => Gram, complexContent: => Gram, rightFraming: => Gram): Gram = {
//      ???
//    }
//
//    def complexContentDef(complexValue: => Gram, elementUnused: => Gram): Gram = {
//      ???
//    }
//
//    lazy val ElementUnused: Gram = ???
//
//    lazy val LeftPadding: Gram = ???
//
//    lazy val RightPadding: Gram = ???
//
//    lazy val RightFill: Gram = ???
//
//    lazy val Initiator: Gram = ???
//
//    lazy val Terminator: Gram = ???
//
//    /*
//     * Model Groups
//     */
//
//    def sequenceContentDef(prefixSeparator: => Gram, enclosedContent: => Gram, separator: => Gram, postfixSeparator: => Gram): Gram = {
//      ???
//    }
//
//    def choiceDef(enclosedContent: => Gram, choiceUnused: => Gram): Gram = {
//      ???
//    }
//
//    def enclosedContentDef(enclosedElement: => Gram, array: => Gram, sequence: => Gram, choice: => Gram): Gram = {
//      ???
//    }
//
//    def arrayDef(enclosedElement: => Gram, separator: => Gram, stopValue: => Gram): Gram = {
//      ???
//    }
//
//    lazy val Separator: Gram = ???
//
//    lazy val PrefixSeparator: Gram = ???
//
//    lazy val PostfixSeparator: Gram = ???
//
//    lazy val ChoiceUnused: Gram = ???
//
//    /*
//   * Other/Shared
//   */
//
//    lazy val LeadingSkip: Gram = ???
//
//    lazy val TrailingSkip: Gram = ???
//
//    lazy val AlignmentFill: Gram = ???
//
//  }
//}
