/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.processors.parsers

import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.processors.ChoiceDispatchKeyEv
import org.apache.daffodil.processors.DelimiterParseEv
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.EscapeSchemeParseEv
import org.apache.daffodil.processors.RangeBound
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.util.Logger
import org.apache.daffodil.util.Maybe

class ComplexTypeParser(rd: RuntimeData, bodyParser: Parser)
  extends CombinatorParser(rd) {
  override def nom = "ComplexType"

  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector(bodyParser)

  def parse(start: PState): Unit = {
    start.mpstate.childIndexStack.push(1L) // one-based indexing
    bodyParser.parse1(start)
    start.mpstate.childIndexStack.pop()
    ()
  }
}

/**
 * The purpose of this parser is to create/evaluate delimiter DFAs
 * and push them to the delimiter stack (bring them in scope) for
 * subsequent (internal/body) parse steps.  Then on the way out pop
 * the delimiter DFAs (bring them out of scope) after
 * the internal/body parser has completed.
 */
class DelimiterStackParser(
  delimiters: Array[DelimiterParseEv],
  ctxt: RuntimeData, bodyParser: Parser)
  extends CombinatorParser(ctxt) {

  override lazy val childProcessors = Vector(bodyParser)

  override lazy val runtimeDependencies = delimiters.toVector

  def parse(start: PState): Unit = {

    val newLocalIndex = start.mpstate.delimiters.length
    start.mpstate.delimitersLocalIndexStack.push(newLocalIndex)

    try {
      // evaluate and add delimiters to the stack
      var i: Int = 0
      while (i < delimiters.length) {
        start.mpstate.delimiters ++= delimiters(i).evaluate(start)
        i += 1
      }

      // parse
        bodyParser.parse1(start)
    } finally {
      // pop delimiters
      start.mpstate.delimiters.reduceToSize(start.mpstate.delimitersLocalIndexStack.pop)
    }
  }
}

/**
 * *
 * This parser should only ever be called when a dynamic escape scheme exists
 * so the escape scheme is evaluated in the right scope. If a constant
 * escape scheme exists, the Evaluatable should store the constant and this
 * should never be called.
 *
 * Note that the escape scheme evaluatable (and its dependencies) are manually
 * cached, so upon exiting scope the cache must be invalidated.
 */
class DynamicEscapeSchemeParser(
  escapeScheme: EscapeSchemeParseEv,
  ctxt: TermRuntimeData, bodyParser: Parser)
  extends CombinatorParser(ctxt) {

  override lazy val childProcessors = Vector(bodyParser)

  override lazy val runtimeDependencies = Vector(escapeScheme)

  def parse(start: PState): Unit = {
    // evaluate the dynamic escape scheme in the correct scope. the resulting
    // value is cached in the Evaluatable (since it is manually cached) and
    // future parsers that use this escape scheme will use that cached value.
    escapeScheme.newCache(start)
    escapeScheme.evaluate(start)

    // Parse
    bodyParser.parse1(start)

    // invalidate the escape scheme cache
    escapeScheme.invalidateCache(start)
  }
}

/*
 * Sometimes choices will have an empty branch (e.g. an empty <xs:sequence />)
 * that optimizes to a NadaParser. NadaParsers should all optimize out, but the
 * ChoiceCombinatorParsers still expect to have a parser to use in these cases.
 * So we have a special empty branch parser that does nothing and always
 * succeeds, but gives the ChoiceCombinatorParsers something that they can use.
 */
class ChoiceBranchEmptyParser(val context: RuntimeData)
  extends PrimParserNoData {

  override lazy val runtimeDependencies = Vector()

  def parse(state: PState): Unit = {
    //do nothing
  }
}

/*
 * dispatchBranchKeyMap: choiceBranchKey -> (Parser, hasRepresentation)
 */

abstract class ChoiceDispatchCombinatorParserBase(rd: TermRuntimeData, 
                                                  dispatchBranchKeyMap: Map[String, (Parser, Boolean)], 
                                                  dispatchKeyRangeMap: Vector[(RangeBound, RangeBound, Parser, Boolean)])
  extends CombinatorParser(rd) {

  override def nom = "ChoiceDispatch"

  override lazy val runtimeDependencies = Vector()

  override def childProcessors = dispatchBranchKeyMap.values.map(_._1).toVector ++ dispatchKeyRangeMap.map(_._3)

  /*
   * Returns a value if pstate.processorStatus eq Success
   */
  def computeDispatchKey(pstate: PState): Maybe[String]

  /*
   * having choiceDispatchKeyKind=byType introduces some subtle problems.
   * In the basic case a naive implementation would try to parse the repType twice:
   *  once to determine the dispatchKey, and once because the resulting branch would have the same repType
   * However, it is also possible that the branch would have an inputValueCalc, in which case we would
   *  only parse the repType when computing the dispatchKey.
   * The difficulty is that, in both the above cases, the *correct* behavior is to parse the reptype exactly once
   *
   * Ideally, we would actually parse repType once, and pass the result into the branch's parser
   * However, in practice, this would involve a significant amount of reworking of Daffodil parsing subsystem.
   *
   * Instead, we simulate parsing once by saving and restoring state as follows:
   *
   * initialState = pstate.mark()
   * dispatchKey <- repType.parse()
   * if(branch.isRepresented){
   *   pstate.restore(initialState)
   * }else{
   *   pstate.discard(initialState)
   * }
   * branch.parse()
   *
   *
   */

  def parse(pstate: PState): Unit = {
    pstate.withPointOfUncertainty("ChoiceDispatchCombinator", rd) { pou =>

      val maybeKey = computeDispatchKey(pstate)

      if (pstate.processorStatus eq Success) {
        val key = maybeKey.get

        val parserOpt1 = dispatchBranchKeyMap.get(key)
        val parserOpt2 = 
          if (parserOpt1.isDefined) {
            parserOpt1
          } else{
            if (!dispatchKeyRangeMap.isEmpty) {
              try {
                val keyAsBigInt = new JBigInt(key)
                val optAns1= dispatchKeyRangeMap.find { case (min, max, _, _) =>
                  min.testAsLower(keyAsBigInt) && max.testAsUpper(keyAsBigInt)
                }
                optAns1.map { case (_ ,_ ,parser, isRepresented) =>
                  (parser,isRepresented)
                }
              } catch {
                case _: NumberFormatException => None
              }
            } else {
              None
            }
          }
        
        val parserOpt: Option[(Parser,Boolean)] = parserOpt2
        if (parserOpt.isEmpty) {
          val diag = new ChoiceDispatchNoMatch(context.schemaFileLocation, pstate, key)
          pstate.setFailed(diag)
        } else {
          val (parser, isRepresented) = parserOpt.get
          if (isRepresented) {
            pstate.resetToPointOfUncertainty(pou)
          }

          // Note that we are intentionally not pushing/popping a new
          // discriminator here, as is done in the ChoiceCombinatorParser and
          // AltCompParser. This has the effect that if a branch of this direct
          // dispatch choice specifies a discriminator, then it will discriminate a
          // point of uncertainty outside of the choice. If we pushed a new
          // discriminator here if would essentially ignore discriminators on a
          // choice branch.

          Logger.log.debug(s"Dispatching to choice alternative: ${parser}")
          parser.parse1(pstate)

          if (pstate.processorStatus eq Success) {
            Logger.log.debug(s"Choice dispatch success: ${parser}")

            // We usually rely on the sequence parser to set elements as final.
            // But choices with scalar elements do not necessarily have a
            // sequence surrounding them and so they aren't set final. In order
            // to set these elements final, we do it here as well. We will
            // attempt to walk the infoset after the PoU is discarded.
            val newLastChildNode = pstate.infoset.maybeLastChild
            if (newLastChildNode.isDefined) {
              newLastChildNode.get.isFinal = true
            }

          } else {
            Logger.log.debug(s"Choice dispatch failed: ${parser}")
            val diag = new ChoiceDispatchFailed(context.schemaFileLocation, pstate, pstate.diagnostics)
            pstate.setFailed(diag)
          }
        }
      }
    }

  }
}

class ChoiceDispatchCombinatorParser(rd: TermRuntimeData, dispatchKeyEv: ChoiceDispatchKeyEv, 
  dispatchBranchKeyMap: Map[String, (Parser, Boolean)], dispatchKeyRangeMap:Vector[(RangeBound,RangeBound,Parser, Boolean)])
  extends ChoiceDispatchCombinatorParserBase(rd, dispatchBranchKeyMap, dispatchKeyRangeMap) {
  override def computeDispatchKey(pstate: PState): Maybe[String] = Maybe(dispatchKeyEv.evaluate(pstate))
}

class ChoiceDispatchCombinatorKeyByTypeParser(rd: TermRuntimeData, repTypeParser: Parser, repTypeRuntimeData: ElementRuntimeData, 
                                              dispatchBranchKeyMap: Map[String, (Parser, Boolean)], dispatchKeyRangeMap:Vector[(RangeBound,RangeBound,Parser, Boolean)])
  extends ChoiceDispatchCombinatorParserBase(rd, dispatchBranchKeyMap, dispatchKeyRangeMap)
  with WithDetachedParser {

//  override lazy val childProcessors = dispatchBranchKeyMap.values.map(_._1).toVector ++ dispatchKeyRangeMap.map(_._3)
  override lazy val childProcessors = super.childProcessors ++ Vector(repTypeParser)
  
  override def computeDispatchKey(pstate: PState): Maybe[String] = {
    val ans1 = runDetachedParser(pstate, repTypeParser, repTypeRuntimeData)
    if (ans1.isDefined) {
      Maybe(ans1.getAnyRef.toString())
    } else {
      Maybe.Nope
    }
  }

}
