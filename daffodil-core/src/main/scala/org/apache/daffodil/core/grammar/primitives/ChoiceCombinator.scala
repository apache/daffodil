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

package org.apache.daffodil.core.grammar.primitives

import java.math.{ BigInteger => JBigInt }
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.apache.daffodil.core.dsom._
import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.core.grammar.Terminal
import org.apache.daffodil.lib.cookers.ChoiceBranchKeyCooker
import org.apache.daffodil.lib.cookers.IntRangeCooker
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.ChoiceLengthKind
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.ProperlySerializableMap._
import org.apache.daffodil.runtime1.infoset.ChoiceBranchEvent
import org.apache.daffodil.runtime1.processors.RangeBound
import org.apache.daffodil.runtime1.processors.parsers._
import org.apache.daffodil.runtime1.processors.unparsers._
import org.apache.daffodil.unparsers.runtime1._

/*
 * The purpose of the ChoiceCombinator (and the parsers it creates) is to
 * determine which branch to go down. In the parser case, for non-direct
 * dispatch, we just rely on backtracking here.
 *
 * For direct dispatch, we create a disapatch-branch key map
 * which is used to determine which branch to parse at runtime.
 *
 * In the unparser case, we know which element we got from the infoset, but we
 * need to determine which branch of the choice to take at runtime. This
 * unparser uses a Map to make the determination based on the element seen.
 */
case class ChoiceCombinator(ch: ChoiceTermBase, alternatives: Seq[Gram])
  extends Terminal(ch, !alternatives.isEmpty) {

  private lazy val parsers = {
    val res = alternatives.map { alt =>
      val p = alt.parser
      val res =
        if (p.isEmpty)
          new ChoiceBranchEmptyParser(alt.context.runtimeData)
        else p
      res
    }
    res
  }

  override def isEmpty = super.isEmpty || alternatives.isEmpty

  // dfdl:choiceLength is always specified in bytes
  private lazy val choiceLengthInBits: MaybeInt = ch.choiceLengthKind match {
    case ChoiceLengthKind.Explicit => MaybeInt(ch.choiceLength * 8)
    case ChoiceLengthKind.Implicit => MaybeInt.Nope
  }

  private def branchKeyAttribute = "dfdl:choiceBranchKey"
  private def branchKeyRangeAttribute = "dfdlx:choiceBranchKeyRanges"

  lazy val parser: Parser = {
    if (!ch.isDirectDispatch) {
      val cp = new ChoiceParser(ch.termRuntimeData, parsers.toArray)
      ch.choiceLengthKind match {
        case ChoiceLengthKind.Implicit => cp
        case ChoiceLengthKind.Explicit =>
          new SpecifiedLengthChoiceParser(
            cp,
            ch.choiceRuntimeData,
            choiceLengthInBits.get.toLong
          )
      }
    } else {
      // Verify that every alternative has some form of branch key
      alternatives.foreach { alt =>
        val keyTerm = alt.context.asInstanceOf[Term]
        val hasBranchKey = keyTerm.findPropertyOption("choiceBranchKey").isDefined
        val hasBranchKeyRanges = keyTerm.findPropertyOption("choiceBranchKeyRanges").isDefined
        if (!hasBranchKey && !hasBranchKeyRanges) {
          keyTerm.SDE(
            "Neither dfdl:choiceBranchKey nor dfdlx:choiceBranchKeyRanges is defined."
          )
        }
      }
      val dispatchBranchKeyValueTuples: Seq[(String, Gram)] = alternatives.flatMap { alt =>
        val keyTerm = alt.context.asInstanceOf[Term]
        val optUncooked =
          keyTerm.findPropertyOption("choiceBranchKey").toOption
        val cookedBranchKeys: Seq[String] = optUncooked
          .map { uncooked =>
            ChoiceBranchKeyCooker.convertConstant(uncooked, ch.runtimeData, forUnparse = false)
          }
          .toSeq
          .flatten
        val tuples: Seq[(String, Gram)] = cookedBranchKeys.map { (_, alt) }
        tuples
      }

      // [(minKeyValue, maxKeyValue, parser, isRepresented)]
      val dispatchBranchKeyRangeTuples: Seq[(RangeBound, RangeBound, Parser, Boolean)] =
        alternatives.flatMap { alt =>
          val keyTerm = alt.context.asInstanceOf[Term]
          val branchKeyRanges: Seq[(RangeBound, RangeBound)] = {
            val optUncooked = keyTerm.findPropertyOption("choiceBranchKeyRanges").toOption
            val cooked: Seq[(JBigInt, JBigInt)] =
              optUncooked
                .map { uncooked => IntRangeCooker.convertConstant(uncooked, context, false) }
                .toSeq
                .flatten
            val tuples = cooked.map({ case (lowerBound, upperBound) =>
              (new RangeBound(lowerBound, true), new RangeBound(upperBound, true))
            })
            tuples
          }

          //
          // The choice alternative (aka branch) is either an element having
          // the repType, or it is a computed (inputValueCalc) element that uses the
          // repType.
          //
          // This creates a problem where we need to parse the repType but we need
          // to avoid parsing it twice.
          //
          // If the branch alternatives are all elements having the repType as their type,
          // then parsing the repType to compute the dispatch key would result in double
          // parsing the repType when we then select a branch and parse that branch.
          //
          // So we determine if the choice branch is represented. If it is, we parse the repType
          // in order to compute the dispatch key, then we backtrack that, so that when
          // we parse the selected branch it appears to not be parsing twice, just once.
          //
          // For the case where the choice branch is computed (inputValueCalc), we can
          // just parse the repType, and no backtracking is needed. So we pass this boolean
          // down into the choice parser so that it can do the right thing depending on the
          // nature of the branches.
          //
          // In all cases, we're looking for the characteristics here of the lexical
          // object that is the choice branch.
          //
          val isRepresentedTerm = alt.term.isRepresented
          branchKeyRanges.toSeq.map(x => (x._1, x._2, alt.parser, isRepresentedTerm))
        }

      // check for duplicate branch keys
      // Our handling of ranges here is definantly suboptimal, but hopefully
      // we don't see enough distinct ranges on a single element for this to be an issue
      // Additionally, at this point, the keys could be comming from either the choiceBranchKey family of attributes
      // or the repValue family of attributes

      val groupedByKey = dispatchBranchKeyValueTuples.groupBy(_._1)
      groupedByKey.foreach { case (k, kvs) =>
        if (kvs.length > 1) {
          SDE(
            "%s value (%s) is not unique across all branches of a direct dispatch choice. Offending branches are:\n%s",
            branchKeyAttribute,
            k,
            kvs
              .map(_._2.context.runtimeData)
              .map(rd =>
                rd.diagnosticDebugName + " " + rd.schemaFileLocation.locationDescription
              )
              .mkString("- ", "\n- ", "")
          )
        }
        Try(k.toLong) match {
          case Success(v) => {
            val asBigInt = JBigInt.valueOf(v)
            val conflictingRanges = dispatchBranchKeyRangeTuples.filter({
              case (min, max, _, _) => min.testAsLower(asBigInt) && max.testAsUpper(asBigInt)
            })
            if (conflictingRanges.nonEmpty) {
              SDE(
                "%s (%s) conflicts with %s. Offending branches are:\n%s\n%s",
                branchKeyAttribute,
                k,
                branchKeyRangeAttribute,
                kvs
                  .map(_._2.context.runtimeData)
                  .map(rd =>
                    rd.diagnosticDebugName + " " + rd.schemaFileLocation.locationDescription
                  )
                  .mkString("- ", "\n- ", ""),
                conflictingRanges
                  .map(_._3.context)
                  .map(rd =>
                    rd.diagnosticDebugName + " " + rd.schemaFileLocation.locationDescription
                  )
                  .mkString("- ", "\n- ", "")
              )
            }
          }
          /*
           * If k is not a numeric type, it cannot possibly overlap with a range. Since we currently
           * only support up to 64 bit integers as "numeric" keys, this means that if k.toLong
           * fails, we assume that it cannot conflict with a range.
           */
          case Failure(_) => ()
        }
      }
      // check for overlap in choiceBranchKeyRanges
      dispatchBranchKeyRangeTuples.foreach({ case (min, max, alt, isRepresented) =>
        val conflictingRanges1 = dispatchBranchKeyRangeTuples.filter({
          case (min2, max2, _, _) => min.intersectsWithOtherBounds(min2, max2)
        })
        val conflictingRanges2 = dispatchBranchKeyRangeTuples.filter({
          case (min2, max2, _, _) => max.intersectsWithOtherBounds(min2, max2)
        })
        val conflictingRanges = (conflictingRanges1 ++ conflictingRanges2).toSet
        if (conflictingRanges.size > 1) {
          SDE(
            "%s (%s, %s) conflicts with other ranges. Offending branches are:\n%s",
            branchKeyRangeAttribute,
            min,
            max,
            conflictingRanges
              .map(_._3.context)
              .map(rd =>
                rd.diagnosticDebugName + " " + rd.schemaFileLocation.locationDescription
              )
              .mkString("- ", "\n- ", "")
          )
        }
      })

      val dispatchBranchKeyMap = dispatchBranchKeyValueTuples.toMap.view.mapValues { gram =>
        val isRepresented =
          true // choice branches are, currently, always represented (cannot have inputValueCalc).
        val gramParser = gram.parser
        val parser =
          if (gramParser.isEmpty) {
            new ChoiceBranchEmptyParser(gram.context.runtimeData)
          } else {
            gramParser
          }
        (parser, isRepresented)
      }.toMap

      val serializableMap: ProperlySerializableMap[String, (Parser, Boolean)] =
        dispatchBranchKeyMap.toProperlySerializableMap
      val serializableKeyRangeMap: Vector[(RangeBound, RangeBound, Parser, Boolean)] =
        dispatchBranchKeyRangeTuples.toVector.map(identity)

      new ChoiceDispatchCombinatorParser(
        ch.termRuntimeData,
        ch.choiceDispatchKeyEv,
        serializableMap,
        serializableKeyRangeMap
      )
    }
  }

  override lazy val unparser: Unparser = {
    val (eventRDMap, optDefaultBranch) = ch.choiceBranchMap
    /*
     * Since it's impossible to know the hiddenness for terms at this level (unless
     * they're a hiddenGroupRef), we always attempt to find a defaultable unparser.
     * If we find one, and at runtime, we're in a hidden state, we can use that. If we
     * are not in a hidden state, it doesn't get used. If we don't find a defaultable branch
     * for a hidden group ref, we SDE because required elements must have a default value
     * or dfdl:outputValueCalc defined during unparsing,
     */
    val optDefaultUnparser: Option[Unparser] = optDefaultBranch.map { defaultBranch =>
      val defaultBranchGram = defaultBranch.termContentBody
      val defaultBranchUnparser = defaultBranchGram.unparser
      if (defaultBranchUnparser.isEmpty) {
        // This is a NadaUnparser, likely caused by a default choice branch
        // that is just an empty sequence. NadaUnparsers throw an assertion
        // when unparsed, but the ChoiceCombinatorUnparser still expects to
        // have a something to unparse, so we have an unparser that just does
        // nothing for this special case
        new ChoiceBranchEmptyUnparser(defaultBranch.runtimeData)
      } else {
        defaultBranchUnparser
      }
    }

    val branchForUnparse = if (optDefaultUnparser.isEmpty) {
      ch match {
        case cgr: ChoiceGroupRef if cgr.isHidden => {
          /*
           * This is a requirement for at least one term inside a hidden choice, we
           * SDE if there is no descendant that is fully defaultable, optional or OVC.
           */
          cgr.SDE(
            "At least one branch of hidden choice must be fully defaultable or define dfdl:outputValueCalc:\n%s",
            ch.groupMembers.mkString("\n")
          )
        }
        /*
         * empty unparser is fine as we have no expectations of hiddenness during
         * compile time if it's not a hiddenGroupRef
         */
        case _ => optDefaultUnparser
      }
    } else {
      optDefaultUnparser
    }

    val eventUnparserMap = eventRDMap.map { case (cbe, branchTerm) =>
      (cbe, branchTerm.termContentBody.unparser)
    }
    val mapValues = eventUnparserMap.map { case (k, v) => v }.toSeq.filterNot(_.isEmpty)
    if (mapValues.isEmpty) {
      if (branchForUnparse.isEmpty) {
        new NadaUnparser(null)
      } else {
        // just a default branch.
        Assert.invariant(branchForUnparse.isDefined)
        branchForUnparse.get
      }
    } else {
      val serializableMap: ProperlySerializableMap[ChoiceBranchEvent, Unparser] =
        eventUnparserMap.toProperlySerializableMap
      val cbm = ChoiceBranchMap(serializableMap, branchForUnparse)
      new ChoiceCombinatorUnparser(ch.modelGroupRuntimeData, cbm, choiceLengthInBits)
    }
  }
}
