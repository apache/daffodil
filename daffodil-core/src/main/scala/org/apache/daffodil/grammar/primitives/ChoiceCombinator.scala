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
case class ChoiceCombinator(ch: ChoiceTermBase, rawAlternatives: Seq[Gram])
  extends Terminal(ch, !rawAlternatives.isEmpty) {

  private lazy val alternatives = rawAlternatives.filterNot(_.isEmpty)

  private lazy val parsers = alternatives.map { _.parser }.filterNot { _.isEmpty }

  override def isEmpty = super.isEmpty || alternatives.isEmpty

  lazy val parser: Parser = {
    if (!ch.isDirectDispatch) {
      new ChoiceParser(ch.termRuntimeData, parsers)
    } else {
      val dispatchBranchKeyValueTuples = alternatives.flatMap { alt =>
        val keyTerm = alt.context.asInstanceOf[Term]
        val cbk = keyTerm.choiceBranchKey
        // Note that this behaves differently than the specification, since
        // this accepts a space separated list of keys
        val cbks = ChoiceBranchKeyCooker.convertConstant(cbk, keyTerm.runtimeData, forUnparse = false)
        cbks.map { (_, alt) }
      }

      // check for duplicate dfdl:choiceBranchKeys
      val groupedByKey = dispatchBranchKeyValueTuples.groupBy(_._1)
      groupedByKey.foreach {
        case (k, kvs) =>
          if (kvs.length > 1) {
            SDE("dfdl:choiceBranchKey value (%s) is not unique across all branches of a direct dispatch choice. Offending branches are:\n%s",
              k, kvs.map(_._2.context.runtimeData).map(rd => rd.diagnosticDebugName + " " + rd.schemaFileLocation.locationDescription).mkString("- ", "\n- ", ""))
          }
      }

      val dispatchBranchKeyMap = dispatchBranchKeyValueTuples.toMap.mapValues(_.parser)
      val serializableMap = dispatchBranchKeyMap.map(identity)

      new ChoiceDispatchCombinatorParser(ch.termRuntimeData, ch.choiceDispatchKeyEv, serializableMap)
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

      new ChoiceCombinatorUnparser(ch.modelGroupRuntimeData, eventUnparserMap)
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

