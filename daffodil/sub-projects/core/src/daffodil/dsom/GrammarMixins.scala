package daffodil.dsom

import scala.xml.Node
import daffodil.exceptions.Assert
import daffodil.grammar._
import daffodil.schema.annotation.props.gen._

trait ElementBaseGrammarMixin { self: ElementBaseMixin =>

  object elementInitiator extends Production(this, NYI)
  object elementTerminator extends Production(this, NYI)
  object nilElementInitiator extends Production(this, NYI)
  object nilElementTerminator extends Production(this, NYI)
  object literalNilValue extends Production(this, NYI)

  object complexContent extends Production(this, isComplexType, elementInitiator ~ elementComplexType.grammarExpr ~ elementTerminator)

  object nilLit extends Production(this,
    isNillable && nilKind == NilKind.LiteralValue,
    nilElementInitiator ~ literalNilValue ~ nilElementTerminator)

  object scalarSimpleContent extends Production(this, NYI) // nilLit | emptyDefaulted | parsedNil | parsedValue )

  object scalarComplexContent extends Production(this, nilLit | complexContent)

  object scalarContent extends Production(this, scalarSimpleContent | scalarComplexContent)
  /**
   * the element left framing does not include the initiator nor the element right framing the terminator
   */
  object elementLeftFraming extends Production(this, NYI, EmptyExpr) // (leadingSkipParser ~ alignmentFill ~ prefixLength)

  object elementRightFraming extends Production(this, NYI, EmptyExpr) // trailingSkipParser

  object scalar extends Production(this, false, elementLeftFraming ~ scalarContent ~ elementRightFraming)
}

trait LocalElementBaseGrammarMixin { self: LocalElementBase =>
  def separatedForPosition(contentBody: Expr): Expr = {
    val Some(res) = nearestEnclosingSequence.map { _.separatedForPosition(contentBody) }
    res
  }

  def grammarExpr = term

  object arrayContents extends Production(this, NYI)
  object finalUnusedRegion extends Production(this, NYI) // probably this is really a primitive
  object separatedScalar extends Production(this, isScalar, separatedForPosition(scalar))
  object recurrance extends Production(this, !isScalar, startArray(this) ~ arrayContents ~ endArray(this) ~ finalUnusedRegion)

  // FIXME: doesn't allow for an element inside a choice, that is inside a sequence. Or a nest of nothing but choices. (No sequences at all)
  object term extends Production(this, separatedScalar | recurrance)
}


trait GlobalElementDeclGrammarMixin { self : GlobalElementDecl =>
  
  object documentElement extends Production(this,  scalar )
  
  object unicodeByteOrderMark extends UnicodeByteOrderMark(this, NYI)
  
  object document extends Production(this, unicodeByteOrderMark ~ documentElement )
}

trait TermGrammarMixin { self : Term =>
  def grammarExpr : Expr  
}

trait ModelGroupGrammarMixin { self : ModelGroup =>
     
  object groupLeftFraming extends Production(this, NYI) // leadingSkipParser ~ alignmentFill ~ groupInitiator)
  object groupRightFraming extends Production(this, NYI) // groupTerminator ~ trailingSkipParser)
  
  object grammarExpr extends Production(this, groupLeftFraming ~ groupContent ~ groupRightFraming )
  
  def mt = EmptyExpr.asInstanceOf[Expr]// cast trick to shut up foldLeft compile error on next line.
  
  object groupContent extends Production(this, groupMemberGrammarNodes.foldLeft(mt)(folder) )
   
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
  
  object prefixSep extends Delimiter(this, hasPrefixSep)
  object postfixSep extends Delimiter(this, hasPostfixSep)
  object infixSep extends Delimiter(this, hasInfixSep)
  
  object infixSepWithPriorRequiredSiblings extends Production(this, hasInfixSep && hasPriorRequiredSiblings,
      infixSep)
  object infixSepWithoutPriorRequiredSiblings extends Production(this, hasInfixSep && !hasPriorRequiredSiblings,
      // runtime check for group pos such that we need a separator.
     groupPosGreaterThan(1)(this) ~ infixSep )
  
  object infixSepRule extends Production(this, hasInfixSep,
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
  
  object grammarExpr extends Production(this, startGroup(this) ~ modelGroup.group.grammarExpr ~ endGroup(this))
  
}
