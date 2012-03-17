package daffodil.dsom

import scala.xml.Node
import daffodil.exceptions.Assert
import daffodil.grammar._
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._

trait ElementBaseGrammarMixin { self: ElementBaseMixin =>
  // 
  // This silly redundancy where the variable name has to also be passed as a string,
  // is, by the way, a good reason Scala needs real Lisp-style macros, that can take an argument and
  // turn it into a type/class, object, def, or val/var name, as well as a string, etc. 
  // 


lazy val parsedNil = Prod("parsedNil", this, NYI && isNillable && nilKind == NilKind.LogicalValue,
      nilElementInitiator ~ LogicalNilValue(this) ~ nilElementTerminator)

  lazy val parsedValue = Prod("parsedValue", this, elementInitiator ~ allowedValue ~ elementTerminator)
  
  def allowedValue : Prod // provided by LocalElementBase for array considerations, and GlobalElementDecl - scalar only

  
  lazy val strangValue = Prod("strangValue", this, lengthKind match {
    case LengthKind.Explicit if isFixedLength => StringFixedLengthPrim(this)
    case _ => Assert.notYetImplemented()
  })
  
  lazy val binaryInt = Prod("binaryInt", this, representation == Representation.Binary,
      regularBinaryRepInt | bcdInt | packedInt)
  
  lazy val regularBinaryRepInt = Prod("regularBinaryRepInt", this, 
      binaryNumberRep == BinaryNumberRep.Binary, RegularBinaryIntPrim(this))
  lazy val bcdInt = Prod("bcdInt", this, 
      binaryNumberRep == BinaryNumberRep.Bcd, BCDIntPrim(this))
  lazy val packedInt = Prod("packedInt", this, 
      binaryNumberRep == BinaryNumberRep.Packed, PackedIntPrim(this))
  
  lazy val textInt = Prod("textInt", this, representation == Representation.Text,
        standardTextInt | zonedTextInt)
  
  // 
  // We could now break it down by lengthKind, and have specialized primitives
  // depending on the length kind.
  // 
  lazy val standardTextInt = Prod("standardTextInt", this, 
      textNumberRep == TextNumberRep.Standard, StandardTextIntPrim(this))
  lazy val zonedTextInt = Prod("zonedTextInt", this, 
      textNumberRep == TextNumberRep.Zoned, ZonedTextIntPrim(this))
 
  //
  // This is clumsy, not to mention slow... every primitive type has to optimize
  // out all but one of these. 
  //
  // TODO: change to a scala match for the expression.
  // 
  lazy val value = Prod("value", this,
      typeDef match {
      case prim : PrimitiveType => {
        val n = prim.name
        n match {
          case "string" => strangValue
          case "int" => binaryInt | textInt
          case _ => Assert.notYetImplemented() // ("only string and int primitive value types are supported")
        }
      }
      case st : SimpleTypeBase => {
        Assert.notYetImplemented() // ("simple type definitions aren't supported yet.")
      }
      case ct : ComplexTypeBase => Assert.invariantFailed("value lazy val for complex type.")
      case _ => Assert.invariantFailed("typeDef was not Primitive, Simple, or Complex")
    }
  )
  
    //  stringValue |
    //  floatValue | doubleValue |
    //  decimalValue | integerValue |
    //  longValue | intValue | shortValue | byteValue |
    //  nonNegativeIntegerValue |
    //  unsignedLongValue | unsignedIntValue | unsignedShortValue | unsignedByteValue |
    //  booleanValue |
    //  dateValue | timeValue | dateTimeValue |
    //  hexBinaryValue
   
    

  
  lazy val empty = Prod("for", this, NYI && emptyIsAnObservableConcept, emptyRepresentation) 
  
  lazy val emptyRepresentation = Prod("emptyRepresentation", this, 
      simpleOrNonImplicitComplexEmpty | complexImplicitEmpty) 
  
  lazy val simpleOrNonImplicitComplexEmpty = Prod("simpleOrNonImplicitComplexEmpty", this, 
      NYI && isSimpleType || isComplexType &&  lengthKind != LengthKind.Implicit,
      emptyElementInitiator ~ emptyElementTerminator)
    
  /**
   * This is about the case where we take an empty, parse a complex type recursively from it
   * and potentially succeed. 
   */
  lazy val complexImplicitEmpty = Prod("complexImplicitEmpty", this, NYI &&  
  	isComplexType &&  lengthKind == LengthKind.Implicit,
  	  SaveInputStream(this) ~ SetEmptyInputStream(this) ~ elementComplexType.grammarExpr ~
  	  RestoreInputStream(this) ~ emptyElementTerminator)
  	  

  
  lazy val emptyDefaulted = Prod("emptyDefaulted", this,
      isDefaultable && emptyIsAnObservableConcept ,
    empty ~ TheDefaultValue(this))
  
  lazy val elementInitiator = Prod("elementInitiator", this, NYI && hasInitiator)
  lazy val elementTerminator = Prod("elementTerminator", this, NYI && hasTerminator)
  lazy val nilElementInitiator = Prod("nilElementInitiator", this, NYI && hasNilValueInitiator)
  lazy val nilElementTerminator = Prod("nilElementTerminator", this, NYI && hasNilValueTerminator)
  
  lazy val emptyElementInitiator = Prod("emptyElementInitiator", this, NYI && hasEmptyValueInitiator)
  lazy val emptyElementTerminator = Prod("emptyElementTerminator", this, NYI && hasEmptyValueTerminator)

  lazy val complexContent = Prod("complexContent", this, isComplexType,
    elementInitiator ~ elementComplexType.grammarExpr ~ elementTerminator)

  lazy val nilLit = Prod("nilLit", this,
    isNillable && nilKind == NilKind.LiteralValue,
    nilElementInitiator ~ LiteralNilValue(this) ~ nilElementTerminator)

  lazy val scalarDefaultableSimpleContent = Prod("scalarDefaultableSimpleContent", this, 
      isSimpleType, nilLit | emptyDefaulted | parsedNil | parsedValue )

  lazy val scalarComplexContent = Prod("scalarComplexContent", this, isComplexType, nilLit | complexContent)

  // Note: there is no such thing as defaultable complex content because you can't have a 
  // default value for a complex type element.
  lazy val scalarDefaultableContent = Prod("scalarDefaultableContent", this, scalarDefaultableSimpleContent | scalarComplexContent)
  lazy val scalarNonDefaultContent = Prod("scalarNonDefaultContent", this, nilLit | parsedNil | parsedValue)
  
  /**
   * the element left framing does not include the initiator nor the element right framing the terminator
   */
  lazy val elementLeftFraming = Prod("elementLeftFraming", this, NYI, 
      LeadingSkipRegion(this) ~ AlignmentFill(this) ~ PrefixLength(this))

  lazy val elementRightFraming = Prod("elementRightFraming", this, NYI, TrailingSkipRegion(this))
  
  /**
   * Placeholders for executing the DFDL 'statement' annotations and doing whatever it is they
   * do to the processor state. This is discriminators, assertions, setVariable, etc.
   * 
   * Also things that care about entry and exit of scope, like newVariableInstance
   */
  lazy val dfdlStatementEvaluations = Prod("dfdlStatementEvaluations", this, NYI)
  lazy val dfdlScopeBegin = Prod("dfdlScopeBegin", this, NYI)
  lazy val dfdlScopeEnd = Prod("dfdlScopeEnd", this, NYI)

  lazy val scalarNonDefault = Prod("scalarNonDefault", this, 
    elementLeftFraming ~ dfdlScopeBegin ~ 
     scalarNonDefaultContent ~ elementRightFraming ~ dfdlStatementEvaluations ~ dfdlScopeEnd)
  
  lazy val scalarDefaultable = Prod("scalarDefaultable", this, 
      elementLeftFraming ~ dfdlScopeBegin ~ scalarDefaultableContent ~ elementRightFraming ~ dfdlStatementEvaluations ~ dfdlScopeEnd)
}

trait LocalElementBaseGrammarMixin { self: LocalElementBase =>
  def separatedForPosition(contentBody: Expr): Expr = {
    val Some(res) = nearestEnclosingSequence.map { _.separatedForPosition(contentBody) }
    res
  }

  def grammarExpr = term

  lazy val allowedValue = Prod("allowedValue", this, (isScalar || !hasStopValue), NotStopValue(this))
  
  lazy val separatedEmpty = Prod("separatedEmpty", this, emptyIsAnObservableConcept, separatedForPosition(empty))
  lazy val separatedScalarDefaultable = Prod("separatedScalar", this, isScalar, separatedForPosition(scalarDefaultable))
  lazy val separatedScalarNonDefault = Prod("separatedScalarNonDefault", this, isScalar, separatedForPosition(scalarNonDefault))
  lazy val recurrance = Prod("recurrance", this, !isScalar, 
      StartArray(this) ~ arrayContents ~ EndArray(this) ~ FinalUnusedRegion(this))

  // FIXME: doesn't allow for an element inside a choice, that is inside a sequence. Or a nest of nothing but choices. (No sequences at all)
  lazy val term = Prod("term", this, separatedScalarDefaultable | recurrance)
  
   /**
     * speculate parsing forward until we get an error
     */
   lazy val separatedContentUnboundedWithoutTrailingEmpties = Prod("separatedContentUnboundedWithoutTrailingEmpties", this, isRecurring,
        RepExactlyN(minOccurs, separatedScalarDefaultable) ~
        RepUnbounded(separatedScalarNonDefault) ~
        StopValue(this) )
  
   lazy val separatedContentUnbounded = Prod("separatedContentUnbounded", this, isRecurring,
        separatedContentUnboundedWithoutTrailingEmpties ~
        RepUnbounded(separatedEmpty))
  
   lazy val separatedContentAtMostNWithoutTrailingEmpties = Prod("separatedContentAtMostNWithoutTrailingEmpties", this, isRecurring,
      RepExactlyN(minOccurs, separatedScalarDefaultable) ~
        RepAtMostTotalN(maxOccurs, separatedScalarNonDefault) ~
        StopValue(this))
 
  // TODO: Do we have to adjust the count to take stopValue into account?
  // Answer: No because the counts are never used when there is a stopValue (at least in current
  // thinking about how occursCountKind='stopValue' works.)
 
  lazy val separatedContentAtMostN = Prod("separatedContentAtMostN", this, isRecurring,
        separatedContentAtMostNWithoutTrailingEmpties ~
        RepAtMostTotalN(maxOccurs, separatedEmpty)) // absorb extra separators, if found.
  
    /**
     *  parse counted number of occurrences exactly.
     */
  lazy val stopValueSize = if (hasStopValue) 1 else 0
  
  def separatedContentExactlyN(count : Long) = { 
      RepExactlyN(minOccurs, separatedScalarDefaultable) ~
        RepAtMostTotalN(count, separatedScalarNonDefault) ~
        StopValue(this) ~
        RepExactlyTotalN(maxOccurs + stopValueSize, separatedEmpty) // absorb reps remaining separators
  }
   
    // keep in mind that anything here that scans for a representation either knows the length it is going after, or knows what the terminating markup is, and
    // our invariant is, that it does NOT consume that markup ever. The parser consumes it with appropriate grammar terminals. 
  
    val UNB = -1 // UNBOUNDED
    val RUNTIME_OCCURS_COUNT = -2
    
    //
    // Silly constants to make the lookup table below more readable without using fragile whitespace
    val Never______ = SeparatorSuppressionPolicy.Never
    val TrailingLax = SeparatorSuppressionPolicy.TrailingLax
    val Trailing___ = SeparatorSuppressionPolicy.Trailing
    val Always_____ = SeparatorSuppressionPolicy.Always
    val StopValue_ = OccursCountKind.StopValue
    val Implicit__ = OccursCountKind.Implicit
    val Parsed____ = OccursCountKind.Parsed
    val Fixed_____ = OccursCountKind.Fixed
    val Expression = OccursCountKind.Expression
    
    lazy val recurring = Prod("stopValueSize", this, isRecurring,
        StartArray(this) ~ arrayContents ~ EndArray(this) // takes care of setting the array index to 1 at the start.
        )
          
    /**
     * Matches the table about separator suppression policy.
     * 
     * TODO: Right now that table is in DFDL WG subgroup working on "Issue 140" which is trying to 
     * rationalize separator suppression among other things. Update this table to match the final spec.
     */
    lazy val arrayContents = Prod("arrayContents", this, isRecurring && hasSep, {
      val triple = (separatorSuppressionPolicy, occursCountKind, maxOccurs)
      val res = triple match {
        case (___________, Expression, ___) => separatedContentExactlyN(RUNTIME_OCCURS_COUNT)
        case (Never______, Fixed_____, UNB) => Assert.SDE("occursCountKind='fixed' not allowed with unbounded maxOccurs")
        case (___________, Fixed_____, max) => separatedContentExactlyN(max)
        case (Never______, Implicit__, UNB) => Assert.SDE("separatorSuppressionPolicy='never' with occursCountKind='implicit' required bounded maxOccurs.")
        case (Never______, Implicit__, max) => separatedContentExactlyN(max)
        case (Never______, ock       , ___) => Assert.SDE("separatorSuppressionPolicy='never' not allowed in combination with occursCountKind='" + ock + "'.")
        case (TrailingLax, Implicit__, UNB) if (!isLastRequiredElementOfSequence) => Assert.SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a sequence")
        case (TrailingLax, Implicit__, UNB) => separatedContentUnbounded
        case (TrailingLax, Implicit__, max) => separatedContentAtMostN // FIXME: have to have all of them - not trailing position 
        case (Trailing___, Implicit__, UNB) if (!isLastRequiredElementOfSequence) => Assert.SDE("occursCountKind='implicit' with unbounded maxOccurs only allowed for last element of a sequence")
        case (Trailing___, Implicit__, UNB) => separatedContentUnboundedWithoutTrailingEmpties // we're depending on optionalEmptyPart failing on empty content.
        case (Trailing___, Implicit__, max) => separatedContentAtMostNWithoutTrailingEmpties
        case (Always_____, Implicit__, UNB) => separatedContentUnbounded
        case (Always_____, Parsed____, ___) => separatedContentUnbounded
        case (Always_____, StopValue_, ___) => separatedContentUnbounded
        case (policy     , ock       , max) => Assert.SDE("separatorSuppressionPolicy='" + policy + "' not allowed with occursCountKind='" + ock + "'.")
      }
      res
    }
    )

}


trait GlobalElementDeclGrammarMixin { self : GlobalElementDecl =>
    
  lazy val allowedValue = Prod("allowedValue", this, value)
  
  lazy val documentElement = Prod("documentElement", this,  scalarDefaultable )
  
  lazy val document = Prod("document", this, UnicodeByteOrderMark(this) ~ documentElement )
}

trait TermGrammarMixin { self : Term =>
  def grammarExpr : Expr  
}

trait ModelGroupGrammarMixin { self : ModelGroup => 
     
  lazy val groupLeftFraming = Prod("groupLeftFraming", this, NYI) // leadingSkipParser ~ alignmentFill ~ groupInitiator)
  lazy val groupRightFraming = Prod("groupRightFraming", this, NYI) // groupTerminator ~ trailingSkipParser)
  
  lazy val grammarExpr = Prod("grammarExpr", this, groupLeftFraming ~ groupContent ~ groupRightFraming )
  
  def mt = EmptyExpr.asInstanceOf[Expr]// cast trick to shut up foldLeft compile error on next line.
  
  lazy val groupContent = Prod("groupContent", this, groupMemberGrammarNodes.foldLeft(mt)(folder) )
   
  def folder(p : Expr, q : Expr) : Expr  
}

trait ChoiceGrammarMixin { self : Choice =>
  
  def folder(p : Expr, q : Expr) = p | q 
  
}

trait SequenceGrammarMixin { self : Sequence =>
  
  def folder(p : Expr, q : Expr) = p ~ q 
  
  def separatedForPosition(contentBody : Expr): Expr = {
    prefixSep ~ infixSepRule ~ contentBody ~ postfixSep
  }
  
  lazy val prefixSep = Delimiter(this, hasPrefixSep)
  lazy val postfixSep = Delimiter(this, hasPostfixSep)
  lazy val infixSep = Delimiter(this, hasInfixSep)
  
  lazy val infixSepWithPriorRequiredSiblings = Prod("prefixSep", this, hasInfixSep && hasPriorRequiredSiblings,
      infixSep)
  lazy val infixSepWithoutPriorRequiredSiblings = Prod("infixSepWithoutPriorRequiredSiblings", this, hasInfixSep && !hasPriorRequiredSiblings,
      // runtime check for group pos such that we need a separator.
     GroupPosGreaterThan(1)(this) ~ infixSep )
  
  lazy val infixSepRule = Prod("infixSepRule", this, hasInfixSep,
      infixSepWithPriorRequiredSiblings | infixSepWithoutPriorRequiredSiblings)
  
     /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */
  lazy val hasPrefixSep = sepExpr(SeparatorPosition.Prefix)
  lazy val hasInfixSep = sepExpr(SeparatorPosition.Infix)
  lazy val hasPostfixSep = sepExpr(SeparatorPosition.Postfix)

  def sepExpr(pos: SeparatorPosition): Boolean = {
    if (separatorExpr.isKnownNonEmpty) if (separatorPosition eq pos) true else false
    else false
  }  
}

trait ComplexTypeBaseGrammarMixin { self : ComplexTypeBase =>
  lazy val startGroup = StartGroup(this, true)
  lazy val endGroup = EndGroup(this, true)
  
  lazy val grammarExpr = Prod("hasPrefixSep", this, startGroup ~ modelGroup.group.grammarExpr ~ endGroup)
  
}
