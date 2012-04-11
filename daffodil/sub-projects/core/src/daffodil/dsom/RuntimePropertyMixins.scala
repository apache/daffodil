package daffodil.dsom

import daffodil.schema.annotation.props.gen._
import daffodil.schema.annotation.props._
import daffodil.exceptions.Assert

/**
 * These are the DFDL properties which can have their values come
 * from the data stream itself by way of expressions.
 * 
 * TODO: EscapeScheme's have a few more of these runtime properties
 * escapeCharacter, and escapeEscapeCharacter.
 */
trait CommonRuntimeValuedPropertiesMixin 
extends DFDLBaseTypeMixin 
{ decl: AnnotatedMixin =>

  lazy val byteOrder = expressionCompiler.compile('String, decl.getProperty("byteOrder"))
  lazy val encodingExpr = expressionCompiler.compile('String, decl.getProperty("encoding"))
  lazy val outputNewLineExpr = expressionCompiler.compile('String, decl.getProperty("outputNewLine"))
}

trait DelimitedRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin { decl : AnnotatedMixin =>
  
  lazy val initiatorExpr = expressionCompiler.compile('String, decl.getProperty("initiator"))
  lazy val terminatorExpr = expressionCompiler.compile('String, decl.getProperty("terminator"))
  
  lazy val hasInitiator = initiatorExpr.isKnownNonEmpty
  lazy val hasTerminator = terminatorExpr.isKnownNonEmpty
  
}

trait ElementRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with OccursAGMixin 
  with LengthAGMixin 
  with SimpleTypeRuntimeValuedPropertiesMixin { decl: ElementBaseMixin =>

  lazy val length = expressionCompiler.compile('Long, decl.getProperty("length"))
  lazy val occursCountExpr = expressionCompiler.compile('Long, decl.getProperty("occursCount"))
}

trait SequenceRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with Sequence_AnnotationMixin { decl: Sequence =>

  lazy val separatorExpr = expressionCompiler.compile('String, decl.getProperty("separator"))
}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin
  with DFDLSimpleTypeMixin { decl: AnnotatedMixin  =>

  // TODO: Implement escape schemes. The escapeCharacter and escapeEscapeCharacter are part of the escapeScheme annotation only.
  // So they're not on the object we're mixing this into.
  // def escapeCharacterExpr = ExpressionCompiler.compile('String, es.getProperty("escapeCharacter"))
  // def escapeEscapeCharacterExpr = ExpressionCompiler.compile('String, es.getProperty("escapeEscapeCharacter"))

  def textStandardDecimalSeparatorExpr = expressionCompiler.compile('String, decl.getProperty("textStandardDecimalSeparator"))
  def textStandardGroupingSeparatorExpr = expressionCompiler.compile('String, decl.getProperty("textStandardGroupingSeparator"))
  // TODO: update when textStandardExponentCharacter is phased out.
  def textStandardExponentRepExpr = expressionCompiler.compile('String, decl.getProperty("textStandardExponentCharacter")) // Note: name changed to suffix of "...Rep" via Errata
  def binaryFloatRepExpr = expressionCompiler.compile('String, decl.getProperty("binaryFloatRep"))
  def textBooleanTrueRepExpr = expressionCompiler.compile('String, decl.getProperty("textBooleanTrueRep"))
  def textBooleanFalseRepExpr = expressionCompiler.compile('String, decl.getProperty("textBooleanFalseRep"))

}

