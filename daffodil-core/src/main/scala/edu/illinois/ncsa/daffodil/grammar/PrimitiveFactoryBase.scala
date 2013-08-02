package edu.illinois.ncsa.daffodil.grammar

import edu.illinois.ncsa.daffodil.dsom.AnnotatedSchemaComponent
import edu.illinois.ncsa.daffodil.dsom.ComplexTypeBase
import edu.illinois.ncsa.daffodil.dsom.DFDLAssert
import edu.illinois.ncsa.daffodil.dsom.DFDLAssertionBase
import edu.illinois.ncsa.daffodil.dsom.DFDLNewVariableInstance
import edu.illinois.ncsa.daffodil.dsom.DFDLSetVariable
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
import edu.illinois.ncsa.daffodil.dsom.LocalElementBase
import edu.illinois.ncsa.daffodil.dsom.LocalElementMixin
import edu.illinois.ncsa.daffodil.dsom.Term

/**
 * There are two kinds of primtives in the grammar. There are terminals
 * which typically implement simple types or framing objects, and there
 * are combinators which combine various grammar clauses in some way.
 */
trait PrimitiveFactoryBase {

  def prod(nameArg: String, sc: Term, guardArg: Boolean, gramArg: => Gram): Gram

  /**
   * Create named productions by applying this object.
   */
  object Prod {
    def apply(nameArg: String, sc: Term, guardArg: Boolean, gramArg: => Gram): Gram = prod(nameArg, sc, guardArg, gramArg)
  }

  /**
   * Interface to grammar terminals
   */

  def ChoiceElementBegin(e: ElementBase): Terminal
  def ElementBegin(e: ElementBase): Terminal
  //def ComplexElementBeginPattern(e: ElementBase) : Terminal
  def ChoiceElementEnd(e: ElementBase): Terminal
  def ElementEnd(e: ElementBase): Terminal
  def ElementEndNoRep(e: ElementBase): Terminal
  //def ComplexElementEndPattern(e: ElementBase): Terminal
  //def StringFixedLengthInBytes(e: ElementBase, nBytes: Long) : Terminal
  def ConvertTextIntegerPrim(e: ElementBase): Terminal
  def ConvertTextDecimalPrim(e: ElementBase): Terminal
  def ConvertTextNonNegativeIntegerPrim(e: ElementBase): Terminal
  def ConvertTextLongPrim(e: ElementBase): Terminal
  def ConvertTextIntPrim(e: ElementBase): Terminal
  def ConvertTextShortPrim(e: ElementBase): Terminal
  def ConvertTextBytePrim(e: ElementBase): Terminal
  def ConvertTextUnsignedLongPrim(e: ElementBase): Terminal
  def ConvertTextUnsignedIntPrim(e: ElementBase): Terminal
  def ConvertTextUnsignedShortPrim(e: ElementBase): Terminal
  def ConvertTextUnsignedBytePrim(e: ElementBase): Terminal
  def ConvertTextDoublePrim(e: ElementBase): Terminal
  def ConvertTextFloatPrim(e: ElementBase): Terminal
  def ZonedTextBytePrim(el: ElementBase): Terminal
  def ZonedTextShortPrim(el: ElementBase): Terminal
  def ZonedTextIntPrim(el: ElementBase): Terminal
  def ZonedTextLongPrim(el: ElementBase): Terminal
  def UnsignedRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase): Terminal
  def UnsignedKnownLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase, len: Long): Terminal
  def SignedRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase): Terminal
  def SignedKnownLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase, len: Long): Terminal
  def HexBinaryKnownLengthBinaryNumber(e: ElementBase, len: Long): Terminal
  def HexBinaryRuntimeLengthBinaryNumber(e: ElementBase): Terminal
  def FloatKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, len: Long): Terminal
  def DoubleKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, len: Long): Terminal
  def DecimalKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, len: Long): Terminal
  def PackedIntPrim(e: ElementBase): Terminal
  def BCDIntPrim(e: ElementBase): Terminal
  def StartChildren(ct: ComplexTypeBase, guard: Boolean = true): Terminal
  def StartSequence(sq: edu.illinois.ncsa.daffodil.dsom.Sequence, guard: Boolean = true): Terminal
  def Nada(sc: Term): Terminal
  def OptionalInfixSep(term: Term, sep: => Gram, guard: Boolean = true): Terminal
  def EndChildren(ct: ComplexTypeBase, guard: Boolean = true): Terminal
  def EndSequence(sq: edu.illinois.ncsa.daffodil.dsom.Sequence, guard: Boolean = true): Terminal
  def StartArray(e: ElementBase, guard: Boolean = true): Terminal
  def EndArray(e: ElementBase, guard: Boolean = true): Terminal
  def NoValue(e: GlobalElementDecl, guard: Boolean = true): Terminal
  def SaveInputStream(e: ElementBase, guard: Boolean = true): Terminal
  def SetEmptyInputStream(e: ElementBase, guard: Boolean = true): Terminal
  def RestoreInputStream(e: ElementBase, guard: Boolean = true): Terminal
  //def Value(e: SchemaComponent, guard: Boolean = true): Terminal
  def NotStopValue(e: ElementBase with LocalElementMixin): Terminal
  def StopValue(e: ElementBase with LocalElementMixin): Terminal
  def TheDefaultValue(e: ElementBase): Terminal
  def LeadingSkipRegion(e: Term): Terminal
  def AlignmentFill(e: Term): Terminal
  def TrailingSkipRegion(e: Term): Terminal
  def PrefixLength(e: ElementBase): Terminal
  def UnicodeByteOrderMark(e: GlobalElementDecl): Terminal
  def FinalUnusedRegion(e: ElementBase): Terminal
  def NewVariableInstanceStart(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance): Terminal
  def NewVariableInstanceEnd(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance): Terminal
  def AssertPatternPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssert): Terminal
  def DiscriminatorPatternPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase): Terminal
  def AssertBooleanPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase): Terminal
  def DiscriminatorBooleanPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase): Terminal
  def InitiatedContent(decl: AnnotatedSchemaComponent): Terminal
  def SetVariable(decl: AnnotatedSchemaComponent, stmt: DFDLSetVariable): Terminal
  def InputValueCalc(e: ElementBase): Terminal

  def StaticInitiator(e: Term): Terminal
  def StaticTerminator(e: Term): Terminal
  def DynamicInitiator(e: Term): Terminal
  def DynamicTerminator(e: Term): Terminal
  def StaticSeparator(s: edu.illinois.ncsa.daffodil.dsom.Sequence, t: Term): Terminal
  def DynamicSeparator(s: edu.illinois.ncsa.daffodil.dsom.Sequence, t: Term): Terminal
  def LiteralNilExplicitLengthInBytes(e: ElementBase): Terminal
  def LiteralNilKnownLengthInBytes(e: ElementBase, lengthInBytes: Long): Terminal
  def LiteralNilExplicitLengthInChars(e: ElementBase): Terminal
  def LiteralNilExplicit(e: ElementBase, nUnits: Long): Terminal
  def LiteralNilPattern(e: ElementBase): Terminal
  def LiteralNilDelimitedEndOfDataStatic(eb: ElementBase): Terminal
  def LiteralNilDelimitedEndOfDataDynamic(eb: ElementBase): Terminal
  def LogicalNilValue(e: ElementBase): Terminal

  def ConvertTextDatePrim(e: ElementBase): Terminal
  def ConvertTextTimePrim(e: ElementBase): Terminal
  def ConvertTextDateTimePrim(e: ElementBase): Terminal

  def HexBinaryFixedLengthInBytes(e: ElementBase, nBytes: Long): Terminal
  def HexBinaryFixedLengthInBits(e: ElementBase, nBits: Long): Terminal
  def HexBinaryVariableLengthInBytes(e: ElementBase): Terminal
  def StringFixedLengthInBytesFixedWidthCharacters(e: ElementBase, nBytes: Long): Terminal
  def StringFixedLengthInBytesVariableWidthCharacters(e: ElementBase, nBytes: Long): Terminal
  def StringFixedLengthInVariableWidthCharacters(e: ElementBase, numChars: Long): Terminal
  def StringVariableLengthInBytes(e: ElementBase): Terminal
  def StringVariableLengthInBytesVariableWidthCharacters(e: ElementBase): Terminal
  def StringVariableLengthInVariableWidthCharacters(e: ElementBase): Terminal
  def StringPatternMatched(e: ElementBase): Terminal
  def StringDelimitedEndOfDataStatic(e: ElementBase): Terminal
  def StringDelimitedEndOfDataDynamic(e: ElementBase): Terminal
  def HexBinaryDelimitedEndOfDataStatic(e: ElementBase): Terminal
  def HexBinaryDelimitedEndOfDataDynamic(e: ElementBase): Terminal

  def SpecifiedLengthPattern(e: ElementBase, eGram: => Gram): Terminal
  def SpecifiedLengthExplicitBitsFixed(e: ElementBase, eGram: => Gram, nBits: Long): Terminal
  def SpecifiedLengthExplicitBits(e: ElementBase, eGram: => Gram): Terminal
  def SpecifiedLengthExplicitBytesFixed(e: ElementBase, eGram: => Gram, nBytes: Long): Terminal
  def SpecifiedLengthExplicitBytes(e: ElementBase, eGram: => Gram): Terminal
  def SpecifiedLengthExplicitCharactersFixed(e: ElementBase, eGram: => Gram, nChars: Long): Terminal
  def SpecifiedLengthExplicitCharacters(e: ElementBase, eGram: => Gram): Terminal

  /**
   * Interface to grammar combinators
   */

  def StmtEval(context: ElementBase, eGram: Gram): Gram
  def UnorderedSequence(context: Term, eGram: Gram): Gram

  def RepExactlyN(context: LocalElementBase, n: Long, r: => Gram): Gram
  def RepAtMostTotalN(context: LocalElementBase, n: Long, r: => Gram): Gram
  def RepExactlyTotalN(context: LocalElementBase, n: Long, r: => Gram): Gram
  def RepUnbounded(context: LocalElementBase, r: => Gram): Gram
  def OccursCountExpression(e: ElementBase): Terminal
  def RepAtMostOccursCount(e: LocalElementBase, n: Long, r: => Gram): Gram
  def RepExactlyTotalOccursCount(e: LocalElementBase, r: => Gram): Gram

}

