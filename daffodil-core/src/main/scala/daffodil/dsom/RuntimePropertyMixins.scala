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
    val c = expressionCompiler.compile('String, EntityReplacer.replaceAll(outputNewLineRaw))
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinition(!s.contains("%NL;"), "outputNewLine cannot contain NL")
      this.schemaDefinition(!s.contains("%WSP;"), "outputNewLine cannot contain WSP")
      this.schemaDefinition(!s.contains("%WSP+;"), "outputNewLine cannot contain WSP+")
      this.schemaDefinition(!s.contains("%WSP*;"), "outputNewLine cannot contain WSP*")
      this.schemaDefinition(!s.contains("%ES;"), "outputNewLine cannot contain ES")

      val validNLs: List[Char] = List('\u000A', '\u000D', '\u0085', '\u2028')
      s.foreach(x => {
        this.schemaDefinition(validNLs.contains(x), "'" + x + "' is not a valid new line character for outputNewLine!")
      })
    }
    c
  }
}

trait DelimitedRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin
  with RawDelimitedRuntimeValuedPropertiesMixin { decl: AnnotatedMixin =>

  // Can be whitespace separated lists, as a result the entity replacement needs to take place elsewhere
  // as it's possible to replace an entity with a whitespace character.
  //  lazy val initiator = expressionCompiler.compile('String, EntityReplacer.replaceAll(initiatorRaw))
  //  lazy val terminator = expressionCompiler.compile('String, EntityReplacer.replaceAll(terminatorRaw))
  lazy val initiator = {
    val c = expressionCompiler.compile('String, initiatorRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinition(!s.contains("%ES;"), "Initiator cannot contain ES")
    }
    c
  }

  lazy val terminator = {
    val c = expressionCompiler.compile('String, terminatorRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinition(!s.contains("%ES;"), "Terminator cannot contain ES")
    }
    c
  }

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
    val c = expressionCompiler.compile('String, separatorRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinition(!s.contains("%ES;"), "Separator cannot contain ES")
    }
    c
  }
}

trait SimpleTypeRuntimeValuedPropertiesMixin
  extends CommonRuntimeValuedPropertiesMixin
  with DFDLSimpleTypeMixin
  with RawSimpleTypeRuntimeValuedPropertiesMixin { decl: AnnotatedMixin =>

  // TODO: Will need to 'evaluate' and perform entity replacement on textStandardDecimalSeparator in Parser where it is used.
  def textStandardDecimalSeparator = {
    val c = expressionCompiler.compile('String, textStandardDecimalSeparatorRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinition(!s.contains("%NL;"), "textStandardDecimalSeparator cannot contain NL")
      this.schemaDefinition(!s.contains("%WSP;"), "textStandardDecimalSeparator cannot contain WSP")
      this.schemaDefinition(!s.contains("%WSP*;"), "textStandardDecimalSeparator cannot contain WSP*")
      this.schemaDefinition(!s.contains("%WSP+;"), "textStandardDecimalSeparator cannot contain WSP+")
      this.schemaDefinition(!s.contains("%ES;"), "textStandardDecimalSeparator cannot contain ES")
    }
    c
  }

  //def textStandardGroupingSeparator = expressionCompiler.compile('String, EntityReplacer.replaceAll(textStandardGroupingSeparatorRaw))
  // TODO: Will need to 'evaluate' and perform entity replacement on textStandardGroupingSeparator in Parser where it is used.
  def textStandardGroupingSeparator = {
    val c = expressionCompiler.compile('String, textStandardGroupingSeparatorRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinition(!s.contains("%NL;"), "textStandardGroupingSeparator cannot contain NL")
      this.schemaDefinition(!s.contains("%WSP;"), "textStandardGroupingSeparator cannot contain WSP")
      this.schemaDefinition(!s.contains("%WSP*;"), "textStandardGroupingSeparator cannot contain WSP*")
      this.schemaDefinition(!s.contains("%WSP+;"), "textStandardGroupingSeparator cannot contain WSP+")
      this.schemaDefinition(!s.contains("%ES;"), "textStandardGroupingSeparator cannot contain ES")
    }
    c
  }

  // TODO: update when textStandardExponentCharacter is phased out.
  // Note: name changed to suffix of "...Rep" via Errata
  def textStandardExponentRep = {
    val c = expressionCompiler.compile('String, textStandardExponentRepRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinition(!s.contains("%NL;"), "textStandardExponentRep cannot contain NL")
      this.schemaDefinition(!s.contains("%WSP;"), "textStandardExponentRep cannot contain WSP")
      this.schemaDefinition(!s.contains("%WSP*;"), "textStandardExponentRep cannot contain WSP*")
      this.schemaDefinition(!s.contains("%WSP+;"), "textStandardExponentRep cannot contain WSP+")
      this.schemaDefinition(!s.contains("%ES;"), "textStandardExponentRep cannot contain ES")
    }
    c
  }

  def binaryFloatRep = expressionCompiler.compile('String, binaryFloatRepRaw)

  // TODO: Will need to 'evaluate' and perform entity replacement on textBooleanTrueRep in Parser where it is used.
  def textBooleanTrueRep = {
    val c = expressionCompiler.compile('String, textBooleanTrueRepRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinition(!s.contains("%NL;"), "textBooleanTrueRep cannot contain NL")
      this.schemaDefinition(!s.contains("%WSP;"), "textBooleanTrueRep cannot contain WSP")
      this.schemaDefinition(!s.contains("%WSP*;"), "textBooleanTrueRep cannot contain WSP*")
      this.schemaDefinition(!s.contains("%WSP+;"), "textBooleanTrueRep cannot contain WSP+")
      this.schemaDefinition(!s.contains("%ES;"), "textBooleanTrueRep cannot contain ES")
    }
    c
  }

  // TODO: Will need to 'evaluate' and perform entity replacement on textBooleanFalseRep in Parser where it is used.
  def textBooleanFalseRep = {
    val c = expressionCompiler.compile('String, textBooleanFalseRepRaw)
    if (c.isConstant) {
      val s = c.constantAsString
      this.schemaDefinition(!s.contains("%NL;"), "textBooleanFalseRep cannot contain NL")
      this.schemaDefinition(!s.contains("%WSP;"), "textBooleanFalseRep cannot contain WSP")
      this.schemaDefinition(!s.contains("%WSP*;"), "textBooleanFalseRep cannot contain WSP*")
      this.schemaDefinition(!s.contains("%WSP+;"), "textBooleanFalseRep cannot contain WSP+")
      this.schemaDefinition(!s.contains("%ES;"), "textBooleanFalseRep cannot contain ES")
    }
    c
  }

}

