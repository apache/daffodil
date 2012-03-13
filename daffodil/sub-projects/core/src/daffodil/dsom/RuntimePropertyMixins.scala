package daffodil.dsom

import daffodil.schema.annotation.props.gen._
import daffodil.schema.annotation.props._
import daffodil.exceptions.Assert

/**
 * These are the DFDL properties which can have their values come
 * from the data stream itself by way of expressions.
 *
 * TODO: Dynamic behavior is not yet implemented for most of these properties.
 * 
 * TODO: EscapeScheme's have a couple more of these runtime properties
 * escapeCharacter, and escapeEscapeCharacter.
 */
trait CommonRuntimeValuedPropertiesMixin 
extends DFDLBaseTypeMixin 
{ decl: SchemaComponent =>

  lazy val byteOrderExpr = expressionCompiler.compile('String, decl.byteOrder)
  lazy val encodingExpr = expressionCompiler.compile('String, decl.encoding)
  lazy val outputNewLineExpr = expressionCompiler.compile('String, decl.outputNewLine)
}

trait DelimitedRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin { decl : SchemaComponent =>
  
  lazy val initiatorExpr = expressionCompiler.compile('String, decl.initiator)
  lazy val terminatorExpr = expressionCompiler.compile('String, decl.terminator)
}

trait ElementRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with OccursAGMixin 
  with LengthAGMixin { decl: ElementBaseMixin =>

  lazy val lengthExpr = expressionCompiler.compile('Long, decl.length)
  lazy val occursCountExpr = expressionCompiler.compile('Long, decl.length)
}

trait SequenceRuntimeValuedPropertiesMixin
  extends DelimitedRuntimeValuedPropertiesMixin
  with Sequence_AnnotationMixin { decl: Sequence =>

  lazy val separatorExpr = expressionCompiler.compile('String, decl.separator)
}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin
  with DFDLSimpleTypeMixin { decl: SchemaComponent  =>

  // TODO: Implement escape schemes. The escapeCharacter and escapeEscapeCharacter are part of the escapeScheme annotation only.
  // So they're not on the object we're mixing this into.
  // def escapeCharacterExpr = ExpressionCompiler.compile('String, escapeCharacter)
  // def escapeEscapeCharacterExpr = ExpressionCompiler.compile('String, escapeEscapeCharacter)

  def textStandardDecimalSeparatorExpr = expressionCompiler.compile('String, decl.textStandardDecimalSeparator)
  def textStandardGroupingSeparatorExpr = expressionCompiler.compile('String, decl.textStandardGroupingSeparator)
  // TODO: update when textStandardExponentCharacter is phased out.
  def textStandardExponentRepExpr = expressionCompiler.compile('String, decl.textStandardExponentCharacter) // Note: name changed to suffix of "...Rep" via Errata
  def binaryFloatRepExpr = expressionCompiler.compile('String, decl.binaryFloatRep)
  def textBooleanTrueRepExpr = expressionCompiler.compile('String, decl.textBooleanTrueRep)
  def textBooleanFalseRepExpr = expressionCompiler.compile('String, decl.textBooleanFalseRep)

}

