package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.unparsers.UState

trait DelimiterEvMixin[+T <: AnyRef] { self: Evaluatable[T] =>

  def converter: Converter[String, List[String]]
  def expr: CompiledExpression[String]
  def trd: TermRuntimeData

  override final def toBriefXML(depth: Int = -1) = if (this.isConstant) this.constValue.toString else expr.toBriefXML(depth)

  protected def evalAndConvert(state: ParseOrUnparseState): List[String] = {
    val expressionResult =
      try {
        expr.evaluate(state)
      } catch {
        case i: InfosetException => toSDE(i, state)
        case v: VariableException => toSDE(v, state)
      }

    val converterResult = state match {
      case cs: CompileState => converter.convertConstant(expressionResult, trd, false)
      case _ => converter.convertRuntime(expressionResult, trd, false)
    }
    converterResult
  }
}

abstract class DelimiterParseEv(override val expr: CompiledExpression[String], override val trd: TermRuntimeData)
  extends Evaluatable[Array[DFADelimiter]](trd)
  with DelimiterEvMixin[Array[DFADelimiter]] {
  
  override lazy val runtimeDependencies = Nil

  def isKnownNonEmpty = !isConstant || optConstant.get.length > 0

  override protected def compute(state: ParseOrUnparseState): Array[DFADelimiter] = {
    if (state.isInstanceOf[UState]) {
       Assert.invariantFailed("State was UState in Parser Evaluatable")
    }

    val converterResult = evalAndConvert(state)
    if (converterResult.length == 1 && converterResult(0) == "") {
      Array()
    } else {
      CreateDelimiterDFA(converterResult)
    }
  }
}

abstract class DelimiterUnparseEv(override val expr: CompiledExpression[String], outputNewLine: OutputNewLineEv, override val trd: TermRuntimeData)
  extends Evaluatable[Option[DFADelimiter]](trd)
  with DelimiterEvMixin[Option[DFADelimiter]] {
  
  override lazy val runtimeDependencies = Seq(outputNewLine)

  override protected def compute(state: ParseOrUnparseState): Option[DFADelimiter] = {
    if (state.isInstanceOf[PState]) {
      Assert.invariantFailed("State was PState in Unparser Evaluatable")
    }

    val converterResult = evalAndConvert(state)
    if (converterResult.length == 1 && converterResult(0) == "") {
      None
    } else {
      val onl = outputNewLine.evaluate(state)
      Some(CreateDelimiterDFA(converterResult(0), onl))
    }
  }
}

class InitiatorParseEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends DelimiterParseEv(expr, trd) {
  
  override val converter = InitiatorCooker
}

class InitiatorUnparseEv(expr: CompiledExpression[String], outputNewLine: OutputNewLineEv, trd: TermRuntimeData)
  extends DelimiterUnparseEv(expr, outputNewLine, trd) {
  
  override val converter = InitiatorCooker
}


class TerminatorParseEv(expr: CompiledExpression[String], isLengthKindDelimited: Boolean, trd: TermRuntimeData)
  extends DelimiterParseEv(expr, trd) {

  override val converter = if (isLengthKindDelimited) TerminatorCookerNoES else TerminatorCooker
}

class TerminatorUnparseEv(expr: CompiledExpression[String], isLengthKindDelimited: Boolean, outputNewLine: OutputNewLineEv, trd: TermRuntimeData)
  extends DelimiterUnparseEv(expr, outputNewLine, trd) {

  override val converter = if (isLengthKindDelimited) TerminatorCookerNoES else TerminatorCooker
}


class SeparatorParseEv(expr: CompiledExpression[String], trd: TermRuntimeData)
  extends DelimiterParseEv(expr, trd) {

  override val converter = SeparatorCooker
}

class SeparatorUnparseEv(expr: CompiledExpression[String], outputNewLine: OutputNewLineEv, trd: TermRuntimeData)
  extends DelimiterUnparseEv(expr, outputNewLine, trd) {

  override val converter = SeparatorCooker
}
