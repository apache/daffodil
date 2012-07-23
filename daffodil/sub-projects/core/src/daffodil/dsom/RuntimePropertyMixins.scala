package daffodil.dsom

import daffodil.schema.annotation.props.gen._
import daffodil.schema.annotation.props._
import daffodil.exceptions.Assert
import daffodil.dsom.EntityReplacer._
import scala.collection.mutable.ListBuffer

/**
 * These are the DFDL properties which can have their values come
 * from the data stream itself by way of expressions.
 *
 * TODO: EscapeScheme's have a few more of these runtime properties
 * escapeCharacter, and escapeEscapeCharacter.
 */
trait CommonRuntimeValuedPropertiesMixin
  extends DFDLBaseTypeMixin
  with RawCommonRuntimeValuedPropertiesMixin { decl: AnnotatedMixin =>

  lazy val byteOrder = expressionCompiler.compile('String, byteOrderRaw)
  lazy val encoding = expressionCompiler.compile('String, encodingRaw)
  lazy val outputNewLine = {
    // TODO: SDE if characters not in list of allowable NL characters
    this.schemaDefinition(outputNewLineRaw.length() > 0, "outputNewLineRaw may not be empty!")
    expressionCompiler.compile('String, EntityReplacer.replaceAll(outputNewLineRaw))
  }
}

trait DelimitedRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin
  with RawDelimitedRuntimeValuedPropertiesMixin { decl: AnnotatedMixin =>

  // Can be whitespace separated lists, as a result the entity replacement needs to take place elsewhere
  // as it's possible to replace an entity with a whitespace character.
  //  lazy val initiator = expressionCompiler.compile('String, EntityReplacer.replaceAll(initiatorRaw))
  //  lazy val terminator = expressionCompiler.compile('String, EntityReplacer.replaceAll(terminatorRaw))
  lazy val initiator = expressionCompiler.compile('String, initiatorRaw)
  lazy val terminator = expressionCompiler.compile('String, terminatorRaw)

  lazy val hasInitiator = initiator.isKnownNonEmpty
  lazy val hasTerminator = terminator.isKnownNonEmpty

}

trait ElementRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with OccursAGMixin
  with LengthAGMixin
  with SimpleTypeRuntimeValuedPropertiesMixin
  with RawElementRuntimeValuedPropertiesMixin { decl: ElementBase =>

  lazy val length = expressionCompiler.compile('Long, lengthRaw)
  lazy val occursCount = expressionCompiler.compile('Long, occursCountRaw)
}

trait SequenceRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with Sequence_AnnotationMixin
  with RawSequenceRuntimeValuedPropertiesMixin { decl: Sequence =>

  lazy val separator = {
    val replaced = EntityReplacer.replaceAll(separatorRaw)
    // println(replaced)
    expressionCompiler.compile('String, EntityReplacer.replaceAll(separatorRaw))
  }
}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin
  with DFDLSimpleTypeMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin { decl: AnnotatedMixin =>

  // TODO: Implement escape schemes. The escapeCharacter and escapeEscapeCharacter are part of the escapeScheme annotation only.
  // So they're not on the object we're mixing this into.
  // def escapeCharacterExpr = ExpressionCompiler.compile('String, es.getProperty("escapeCharacter"))
  // def escapeEscapeCharacterExpr = ExpressionCompiler.compile('String, es.getProperty("escapeEscapeCharacter"))

  //def textStandardDecimalSeparator = expressionCompiler.compile('String, textStandardDecimalSeparatorRaw)
  def textStandardDecimalSeparator: List[CompiledExpression] = {
    this.schemaDefinition(textStandardDecimalSeparatorRaw.length() > 0, "textStandardDecimalSeparator may not be empty!")
    val l = new ListOfSingleCharacterLiteral(textStandardDecimalSeparatorRaw, this)
    val res: ListBuffer[CompiledExpression] = ListBuffer.empty
    l.cooked.foreach( x => res += expressionCompiler.compile('String, x))
    res.toList
  }

  //def textStandardGroupingSeparator = expressionCompiler.compile('String, EntityReplacer.replaceAll(textStandardGroupingSeparatorRaw))
  def textStandardGroupingSeparator = expressionCompiler.compile('String, {
    this.schemaDefinition(textStandardGroupingSeparatorRaw.length() > 0, "textStandardGroupingSeparator may not be empty!")
    val l = new SingleCharacterLiteral(textStandardGroupingSeparatorRaw, this)
    l.cooked
  })

  // TODO: update when textStandardExponentCharacter is phased out.
  def textStandardExponentRep = {
    this.schemaDefinition(textStandardExponentRepRaw.length() > 0, "textStandardExponentRep may not be empty!")
    expressionCompiler.compile('String, EntityReplacer.replaceAll(textStandardExponentRepRaw)) // Note: name changed to suffix of "...Rep" via Errata
  }
  
  def binaryFloatRep = expressionCompiler.compile('String, binaryFloatRepRaw)
  
  //def textBooleanTrueRep = expressionCompiler.compile('String, EntityReplacer.replaceAll(textBooleanTrueRepRaw))
  def textBooleanTrueRep: List[CompiledExpression] = {
    this.schemaDefinition(textBooleanTrueRepRaw.length > 0, "textBooleanTrueRep may not be empty!")
    val l = new ListOfStringValueAsLiteral(textBooleanTrueRepRaw, this)
    val res: ListBuffer[CompiledExpression] = ListBuffer.empty
    l.cooked.foreach( x => res += expressionCompiler.compile('String, x))
    res.toList
  }
//  def textBooleanFalseRep = expressionCompiler.compile('String, EntityReplacer.replaceAll(textBooleanFalseRepRaw))
  def textBooleanFalseRep: List[CompiledExpression] = {
    this.schemaDefinition(textBooleanFalseRepRaw.length > 0, "textBooleanFalseRep may not be empty!")
    val l = new ListOfStringValueAsLiteral(textBooleanFalseRepRaw, this)
    val res: ListBuffer[CompiledExpression] = ListBuffer.empty
    l.cooked.foreach( x => res += expressionCompiler.compile('String, x))
    res.toList
  }

}

