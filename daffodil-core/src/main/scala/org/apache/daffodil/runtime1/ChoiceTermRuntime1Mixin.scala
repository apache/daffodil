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

package org.apache.daffodil.runtime1

import org.apache.daffodil.dsom.ChoiceTermBase
import org.apache.daffodil.dsom.ModelGroup
import org.apache.daffodil.infoset.ChoiceBranchStartEvent
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.ChoiceRuntimeData
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.infoset.ChoiceBranchEvent
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.ChoiceBranchEndEvent
import org.apache.daffodil.dsom.SequenceTermBase
import org.apache.daffodil.api.WarnID
import org.apache.daffodil.processors.ChoiceDispatchKeyEv
import org.apache.daffodil.dsom.ExpressionCompilers
import org.apache.daffodil.dpath.NodeInfo

trait ChoiceTermRuntime1Mixin { self: ChoiceTermBase =>

  final protected lazy val choiceDispatchKeyExpr = {
    val qn = this.qNameForProperty("choiceDispatchKey")
    ExpressionCompilers.String.compileProperty(qn, NodeInfo.NonEmptyString, choiceDispatchKeyRaw, this, dpathCompileInfo)
  }

  final lazy val choiceDispatchKeyEv = {
    Assert.invariant(isDirectDispatch)
    val ev = new ChoiceDispatchKeyEv(choiceDispatchKeyExpr, ci)
    ev.compile(tunable)
    ev
  }

  final def choiceBranchMap: Map[ChoiceBranchEvent, RuntimeData] = LV('choiceBranchMap) {

    val eventTuples = groupMembers.flatMap {
      case e: ElementBase => Seq((ChoiceBranchStartEvent(e.namedQName), e))
      case mg: ModelGroup => {
        val idEvents = mg.identifyingEventsForChoiceBranch
        Assert.invariant(!idEvents.isEmpty)
        idEvents.map { (_, mg) }
      }
    }

    // converts a sequence of tuples into a multi-map
    val eventMap = eventTuples.groupBy { _._1 }.mapValues { _.map(_._2) }

    val noDupes = eventMap.map {
      case (event, trds) =>
        if (trds.length > 1) {
          if (event.isInstanceOf[ChoiceBranchStartEvent] && trds.exists {
            // any element children in any of the trds?
            // because if so, we have a true ambiguity here.
            case sg: SequenceTermBase => {
              val nonOVCEltChildren = sg.elementChildren.filterNot { _.isOutputValueCalc }
              nonOVCEltChildren.length > 0
            }
            case _ => false
          }) {
            // Possibly due to presence of a element with dfdl:outputValueCalc, XML Schema's
            // UPA check may not catch this ambiguity. However, we need a real element
            // with unique name, to unambiguously identify a branch.
            // So if there is ambiguity at this point, we have to fail.
            SDE(
              "UPA violation. Multiple choice branches begin with %s.\n" +
                "Note that elements with dfdl:outputValueCalc cannot be used to distinguish choice branches.\n" +
                "Note that choice branches with entirely optional content are not allowed.\n" +
                "The offending choice branches are:\n%s",
              event.qname, trds.map { trd => "%s at %s".format(trd.diagnosticDebugName, trd.locationDescription) }.mkString("\n"))
          } else {
            val eventType = event match {
              case _: ChoiceBranchEndEvent => "end"
              case _: ChoiceBranchStartEvent => "start"
            }
            // there are no element children in any of the branches.
            SDW(
              WarnID.MultipleChoiceBranches,
              "Multiple choice branches are associated with the %s of element %s.\n" +
                "Note that elements with dfdl:outputValueCalc cannot be used to distinguish choice branches.\n" +
                "Note that choice branches with entirely optional content are not allowed.\n" +
                "The offending choice branches are:\n%s\n" +
                "The first branch will be used during unparsing when an infoset ambiguity exists.",
              eventType, event.qname, trds.map { trd => "%s at %s".format(trd.diagnosticDebugName, trd.locationDescription) }.mkString("\n"))
          }
        }
        (event, trds(0).runtimeData)
    }

    noDupes
  }.value

  final lazy val modelGroupRuntimeData = choiceRuntimeData

  final lazy val choiceRuntimeData = {
    new ChoiceRuntimeData(
      position,
      partialNextElementResolver,
      schemaSet.variableMap,
      encodingInfo,
      // elementChildren.map { _.elementRuntimeData.dpathElementCompileInfo },
      schemaFileLocation,
      dpathCompileInfo,
      diagnosticDebugName,
      path,
      namespaces,
      defaultBitOrder,
      groupMembersRuntimeData,
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      hasNoSkipRegions,
      optIgnoreCase,
      maybeFillByteEv,
      maybeCheckByteAndBitOrderEv,
      maybeCheckBitOrderAndCharset)
  }
}
