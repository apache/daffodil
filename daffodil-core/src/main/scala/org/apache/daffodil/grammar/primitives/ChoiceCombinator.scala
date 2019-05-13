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

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.dsom._
import org.apache.daffodil.processors.parsers._
import org.apache.daffodil.processors.unparsers._
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.cookers.ChoiceBranchKeyCooker
import org.apache.daffodil.api.WarnID
import org.apache.daffodil.equality._
import org.apache.daffodil.schema.annotation.props.gen.ChoiceLengthKind
import org.apache.daffodil.schema.annotation.props.gen.ChoiceKeyKindType
import org.apache.daffodil.dsom.ChoiceTermBase
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.math.BigInt
import org.apache.daffodil.cookers.RepValueCooker
import org.apache.daffodil.processors.RangeBound
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.MaybeInt

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
          new EmptyChoiceBranchParser(alt.context.runtimeData)
        else p
      res
    }
    res
  }

  override def isEmpty = super.isEmpty || alternatives.isEmpty

  lazy val optChoiceDispatchKeyKind = Some(ch.choiceDispatchKeyKind)

  // dfdl:choiceLength is always specified in bytes
  private lazy val choiceLengthInBits: MaybeInt = ch.choiceLengthKind match {
    case ChoiceLengthKind.Explicit => MaybeInt(ch.choiceLength * 8)
    case ChoiceLengthKind.Implicit => MaybeInt.Nope
  }

  lazy val parser: Parser = {
    if (!ch.isDirectDispatch) {
      val cp = new ChoiceParser(ch.termRuntimeData, parsers.toVector)
      ch.choiceLengthKind match {
        case ChoiceLengthKind.Implicit => cp
        case ChoiceLengthKind.Explicit => new SpecifiedLengthChoiceParser(cp, ch.choiceRuntimeData, choiceLengthInBits.get)
      }
    } else {
      val dispatchBranchKeyValueTuples: Seq[(String, Gram)] = alternatives.flatMap { alt =>
        val keyTerm = alt.context.asInstanceOf[Term]
        val uncookedBranchKeys =
          ch.defaultableChoiceBranchKeyKind match {
            case ChoiceKeyKindType.ByType => {
              val keyTerm_ = keyTerm.asInstanceOf[ElementBase]
              val st = keyTerm_.simpleType
              val aa = st.optRepValueSet
              val repValueSet = keyTerm_.simpleType.optRepValueSet.get
              val ans = repValueSet.valueSet.toSeq.map(_.toString).mkString(" ")
              ans
            }
            case ChoiceKeyKindType.Explicit    => keyTerm.choiceBranchKey
            case ChoiceKeyKindType.Implicit    => keyTerm.choiceBranchKey
            case ChoiceKeyKindType.Speculative => keyTerm.choiceBranchKey
          }
        val cbks = {
          if(uncookedBranchKeys.isEmpty){
            List()
          }else{
            ChoiceBranchKeyCooker.convertConstant(uncookedBranchKeys, ch.runtimeData, forUnparse = false)
          }
        }
        cbks.map { (_, alt) }
      }

      //[(minKeyValue, maxKeyValue, parser, isRepresented)]
      //Since there is not a choiceBranchKeyRanges attribute, this can only currently be populated by repType
      val dispatchBranchKeyRangeTuples: Seq[(RangeBound[BigInt], RangeBound[BigInt], Parser, Boolean)] = alternatives.flatMap { alt =>
        val keyTerm = alt.context.asInstanceOf[Term]
        val branchKeyRanges:Set[(RangeBound[BigInt],RangeBound[BigInt])] = ch.defaultableChoiceBranchKeyKind match {
          case ChoiceKeyKindType.ByType => {
            val keyTerm_ = keyTerm.asInstanceOf[ElementBase]
            keyTerm_.simpleType.optRepValueSet.get.valueRanges.map(x=>{
              val x_ = x.asInstanceOf[(RangeBound[BigInt],RangeBound[BigInt])]
              (x_._1,x_._2)
            })
          }
          case ChoiceKeyKindType.Explicit    => Set()
          case ChoiceKeyKindType.Implicit    => Set()
          case ChoiceKeyKindType.Speculative => Set()
        }
        branchKeyRanges.toSeq.map(x=>(x._1, x._2, alt.parser, alt.context.enclosingTerm.get.isRepresented))
      }

      // check for duplicate dfdl:choiceBranchKeys
      // Our handling of ranges here is definantly suboptimal, but hopefully
      // we don't see enough distinct ranges on a single element for this to be an issue
      val groupedByKey = dispatchBranchKeyValueTuples.groupBy(_._1)
      groupedByKey.foreach {
        case (k, kvs) =>
          if (kvs.length > 1) {
            SDE(
              "dfdl:choiceBranchKey value (%s) is not unique across all branches of a direct dispatch choice. Offending branches are:\n%s",
              k, kvs.map(_._2.context.runtimeData).map(rd => rd.diagnosticDebugName + " " + rd.schemaFileLocation.locationDescription).mkString("- ", "\n- ", ""))
          }
          Try(k.toLong) match {
            case Success(v) => {
              val asBigInt = BigInt(v)
              val conflictingRanges = dispatchBranchKeyRangeTuples.filter({ case (min, max, _, _) => min.testAsLower(asBigInt) && max.testAsUpper(asBigInt) })
              if (conflictingRanges.length > 0) {
                SDE(
                  "dfdl:choiceBranchKey (%s) conflicts with dfdl:choiceBranchKeyRanges. Offending branches are:\n%s\n%s",
                  k,
                  kvs.map(_._2.context.runtimeData).map(rd => rd.diagnosticDebugName + " " + rd.schemaFileLocation.locationDescription).mkString("- ", "\n- ", ""),
                  conflictingRanges.map(_._3.context).map(rd => rd.diagnosticDebugName + " " + rd.schemaFileLocation.locationDescription).mkString("- ", "\n- ", ""))
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
      //check for overlap in choiceBranchKeyRanges
      dispatchBranchKeyRangeTuples.map({
        case (min, max, alt, isRepresented) =>
          val conflictingRanges1 = dispatchBranchKeyRangeTuples.filter({ case (min2, max2, _, _) => min.intersectsWithOtherBounds(min2, max2) })
          val conflictingRanges2 = dispatchBranchKeyRangeTuples.filter({ case (min2, max2, _, _) => max.intersectsWithOtherBounds(min2, max2) })
          val conflictingRanges = (conflictingRanges1 ++ conflictingRanges2).toSet
          if (conflictingRanges.size > 1) {
            SDE(
              "dfdl:choiceBranchKeyRanges (%s, %s) conflicts with other ranges. Offending branches are:\n%s",
              min, max,
              conflictingRanges.map(_._3.context).map(rd => rd.diagnosticDebugName + " " + rd.schemaFileLocation.locationDescription).mkString("- ", "\n- ", ""))
          }
      })

      val dispatchBranchKeyMap = dispatchBranchKeyValueTuples.toMap.mapValues(gram => {
        val isRepresented = gram.context.enclosingTerm.get.isRepresented
        val parser = gram.parser
        (parser, isRepresented)
      })
      val serializableMap: Map[String, (Parser, Boolean)] = dispatchBranchKeyMap.map(identity)
      val serializableKeyRangeMap: Vector[(RangeBound[BigInt], RangeBound[BigInt], Parser, Boolean)] = dispatchBranchKeyRangeTuples.toVector.map(identity)

      ch.defaultableChoiceDispatchKeyKind match {
        case ChoiceKeyKindType.ByType =>
          new ChoiceDispatchCombinatorKeyByTypeParser(ch.termRuntimeData, ch.optRepTypeElement.get.enclosedElement.parser, ch.optRepTypeElement.get.elementRuntimeData, serializableMap, serializableKeyRangeMap)
        case ChoiceKeyKindType.Explicit | ChoiceKeyKindType.Implicit =>
          new ChoiceDispatchCombinatorParser(ch.termRuntimeData, ch.choiceDispatchKeyEv, serializableMap, serializableKeyRangeMap)
        case ChoiceKeyKindType.Speculative => Assert.invariantFailed("ChoiceKeyKindType==speculative while isDirectDispatch==true")
      }
    }
  }

  override lazy val unparser: Unparser = {
    if (!ch.isHidden) {
      val eventRDMap = ch.choiceBranchMap
      val eventUnparserMap = eventRDMap.flatMap {
        case (cbe, rd) =>
          // if we don't find a matching RD for a term that's probably
          // because the term is an empty sequence or empty choice (which do happen
          // and we even have tests for them). Since those can never be chosen by
          // means of an element event, they don't appear in the map.
          val altGram = alternatives.find { alt =>
            val crd = alt.context.runtimeData
            val found = crd =:= rd
            found
          }
          altGram.map { ag => (cbe, ag.unparser) }
      }
      val mapValues = eventUnparserMap.map { case (k, v) => v }.toSeq.filterNot(_.isEmpty)
      if (mapValues.isEmpty)
        new NadaUnparser(null)
      else {
        new ChoiceCombinatorUnparser(ch.modelGroupRuntimeData, eventUnparserMap, choiceLengthInBits)
      }
    } else {
      // Choices inside a hidden group ref are slightly different because we
      // will never see events for any of the branches. Instead, we will just
      // always pick the branch in which every thing is defaultble or OVC. It
      // is a warning if more than one of those branches exist. It is an SDE if
      // such a branch does not exist, which is detected elsewhere

      // this call is necessary since it will throw an SDE if no choice branch
      // was defaultable
      ch.childrenInHiddenGroupNotDefaultableOrOVC
      val defaultableBranches = ch.groupMembers.filter { _.childrenInHiddenGroupNotDefaultableOrOVC.length == 0 }
      Assert.invariant(defaultableBranches.length > 0)
      if (defaultableBranches.length > 1) {
        SDW(WarnID.ChoiceInsideHiddenGroup, "xs:choice inside a hidden group has unparse ambiguity: multiple branches exist with all children either defaulable or have the dfdl:outputValueCalc property set. The first branch will be chosen during unparse. Defaultable branches are:\n%s",
          defaultableBranches.mkString("\n"))
      }
      val defaultableBranchRD = defaultableBranches(0).runtimeData
      val defaultableBranchUnparser = alternatives.find(_.context.runtimeData =:= defaultableBranchRD).get.unparser
      new HiddenChoiceCombinatorUnparser(ch.modelGroupRuntimeData, defaultableBranchUnparser)
    }
  }
}
