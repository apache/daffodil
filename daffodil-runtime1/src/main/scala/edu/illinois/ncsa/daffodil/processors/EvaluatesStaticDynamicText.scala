package edu.illinois.ncsa.daffodil.processors

import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser

trait EvaluatesStaticDynamicTextCommon { self: Processor =>
  /**
   * Called at compile time in static case, at runtime for dynamic case.
   */
  def errorIfDelimsHaveWSPStar(delims: Seq[String], context: RuntimeData): Unit = {
    if (delims.filter(x => x == "%WSP*;").length > 0) {
      // We cannot detect this error until expressions have been evaluated!
      log(LogLevel.Debug, "%s - Failed due to WSP* detected as a delimiter for lengthKind=delimited.", context.prettyName)
      context.schemaDefinitionError("WSP* cannot be used as a delimiter when lengthKind=delimited.")
    }
  }

  def combineStaticAndDynamic(static: Array[DFADelimiter], dynamic: Array[DFADelimiter]) = {
    val res = static ++ dynamic
    res
  }
}

trait EvaluatesStaticDynamicTextParser
  extends EvaluatesStaticDynamicTextCommon { self: Parser =>

  def getStaticAndDynamicText(ceOpt: Option[CompiledExpression[String]],
    context: RuntimeData,
    errorOnWSPStar: Boolean = false): (Array[DFADelimiter], Option[CompiledExpression[String]]) = {

    val res = if (ceOpt.isDefined) {
      val ce = ceOpt.get
      getStaticAndDynamicText(ce, context, errorOnWSPStar)
    } else {
      val staticDFAs = CreateDelimiterDFA(Queue.empty[String])
      (staticDFAs, None)
    }

    res
  }

  def getStaticAndDynamicText(ce: CompiledExpression[String],
    context: RuntimeData,
    errorOnWSPStar: Boolean): (Array[DFADelimiter], Option[CompiledExpression[String]]) = {

    val (staticRawOpt, dynamicRawOpt) = {
      if (ce.isConstant) {
        if (ce.constant.length() > 0) (Some(ce.constant.split("\\s")), None)
        else { (Some(Array.empty[String]), None) }
      } else (None, Some(ce))
    }
    val staticCooked: Queue[String] = new Queue
    if (staticRawOpt.isDefined) {
      EntityReplacer { er =>
        staticRawOpt.get.foreach(x => staticCooked.enqueue(er.replaceAll(x, Some(context))))
      }
    }
    if (errorOnWSPStar) errorIfDelimsHaveWSPStar(staticCooked, context)
    val staticDFAs = CreateDelimiterDFA(staticCooked)

    (staticDFAs, dynamicRawOpt)
  }

  def evaluateDynamicText(dynamic: Option[CompiledExpression[String]],
    start: PState,
    context: RuntimeData,
    errorOnWSPStar: Boolean): Array[DFADelimiter] = {

    if (dynamic.isDefined) {
      val res = dynamic.get.evaluate(start)
      val finalValueCooked = DelimiterCooker.convertRuntime(res.toString, context, forUnparse = false)

      if (errorOnWSPStar) errorIfDelimsHaveWSPStar(finalValueCooked, context)

      val dfas = CreateDelimiterDFA(finalValueCooked)

      dfas
    } else { Array.empty }
  }
}

trait EvaluatesStaticDynamicTextUnparser
  extends EvaluatesStaticDynamicTextCommon { self: Unparser =>

  def getStaticAndDynamicText(ceOpt: Option[CompiledExpression[String]],
    outputNewLine: OutputNewLineEv,
    context: RuntimeData,
    errorOnWSPStar: Boolean = false): (Option[DFADelimiter], Option[CompiledExpression[String]]) = {

    val res = if (ceOpt.isDefined) {
      val ce = ceOpt.get
      getStaticAndDynamicText(ce, outputNewLine, context, errorOnWSPStar)
    } else {
      (None, None)
    }

    res
  }

  def getStaticAndDynamicText(ce: CompiledExpression[String],
    outputNewLine: OutputNewLineEv,
    context: RuntimeData,
    errorOnWSPStar: Boolean): (Option[DFADelimiter], Option[CompiledExpression[String]]) = {

    val (staticRawOpt, dynamicRawOpt) = {
      if (ce.isConstant && outputNewLine.isConstant) {
        if (ce.constant.length() > 0) (Some(ce.constant.split("\\s+")), None)
        else { return (None, None) }
      } else return (None, Some(ce)) // Dynamic because either CE or outputNewLine was not 'constant'.
    }

    val staticCooked = EntityReplacer { _.replaceAll(staticRawOpt.get.head, Some(context)) }
    if (errorOnWSPStar) errorIfDelimsHaveWSPStar(Seq(staticCooked), context)
    val staticDelim = {
      CreateDelimiterDFA(staticCooked, outputNewLine.optConstant.get)
    }

    (Some(staticDelim), dynamicRawOpt)
  }

  def evaluateDynamicText(dynamic: Option[CompiledExpression[String]],
    outputNewLine: OutputNewLineEv,
    start: UState,
    context: RuntimeData,
    errorOnWSPStar: Boolean): Option[DFADelimiter] = {

    if (dynamic.isDefined) {
      val res = dynamic.get.evaluate(start)

      val outputNL = outputNewLine.evaluate(start)

      val finalValueCooked = DelimiterCooker.convertRuntime(res, context, forUnparse = true)

      if (errorOnWSPStar) errorIfDelimsHaveWSPStar(finalValueCooked, context)

      val dfas = CreateDelimiterDFA(finalValueCooked.head, outputNL)

      Some(dfas)
    } else { None }
  }
}
