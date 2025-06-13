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

package org.apache.daffodil.core.runtime1

import org.apache.daffodil.core.dsom.ChoiceTermBase
import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.dsom.ExpressionCompilers
import org.apache.daffodil.core.dsom.SequenceTermBase
import org.apache.daffodil.core.dsom.Term
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.util.Delay
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.ChoiceBranchEndEvent
import org.apache.daffodil.runtime1.infoset.ChoiceBranchEvent
import org.apache.daffodil.runtime1.infoset.ChoiceBranchStartEvent
import org.apache.daffodil.runtime1.processors.ChoiceDispatchKeyEv
import org.apache.daffodil.runtime1.processors.ChoiceRuntimeData

trait ChoiceTermRuntime1Mixin { self: ChoiceTermBase =>

  final protected lazy val choiceDispatchKeyExpr = {
    val qn = this.qNameForProperty("choiceDispatchKey")
    ExpressionCompilers.String.compileProperty(
      qn,
      NodeInfo.NonEmptyString,
      getChoiceDispatchKeyRaw,
      this,
      dpathCompileInfo
    )
  }

  final lazy val choiceDispatchKeyEv = {
    Assert.invariant(isDirectDispatch)
    val ev = new ChoiceDispatchKeyEv(choiceDispatchKeyExpr, ci)
    ev.compile(tunable)
    ev
  }

  private lazy val identifyingEventsForAllChoiceBranches =
    groupMembers.map { _.identifyingEventsForChoiceBranch }

  private lazy val allBranchesClosed =
    identifyingEventsForAllChoiceBranches.forall { _.isClosed }

  final lazy val choiceBranchMap: (Map[ChoiceBranchEvent, Term], Option[Term]) = {

    import PossibleNextElements._

    val eventTuples = alternatives.flatMap { alt =>
      alt.context match {
        case t: Term => {
          val poss = t.identifyingEventsForChoiceBranch
          poss.pnes.flatMap { case PNE(e, ovr) =>
            Seq((ChoiceBranchStartEvent(e.namedQName).asInstanceOf[ChoiceBranchEvent], t))
          }
        }
        case _ => Assert.invariantFailed("must be a term")
      }
    }
    //
    // The default branch is the one to be taken if the incoming unparser event isn't one
    // identified with any branch. This can happen even if every branch has required elements
    // if those required elements have dfdl:outputValueCalc because we allow an OVC
    // element event to be in the infoset (for round-tripping from parse). It's value will
    // get recomputed, but it can appear with a stale value (left-over from parse)
    // in the arriving infoset for unparse.
    //
    val optDefaultBranch = {
      val optEmpty: Option[Term] = {
        val emptyBranches = groupMembers.filter { gm =>
          val ies = gm.identifyingEventsForChoiceBranch
          ies.pnes.isEmpty // empty event list makes it the default, not simply isOpen
        }
        if (emptyBranches.length > 1)
          SDW(
            WarnID.MultipleChoiceBranches,
            "Multiple choice branches with no required elements detected and the infoset does not specify a branch, selecting the first branch for unparsing"
          )
        emptyBranches.headOption
      }
      val optOpen: Option[Term] =
        optEmpty.orElse {
          groupMembers.find { gm =>
            val ies = gm.identifyingEventsForChoiceBranch
            ies.isOpen // first open one is used if there is no branch that has empty event list. (test ptg_1u)
          }
        }
      val optDefault: Option[Term] =
        optOpen.orElse {
          groupMembers.find {
            _.canUnparseIfHidden
          } // optional, defaultable, OVC, or IVC
        }
      optDefault
    }

    // converts a sequence of tuples into a multi-map
    val eventMap =
      eventTuples
        .groupBy {
          _._1
        }
        .view
        .mapValues {
          _.map(_._2)
        }
        .toMap

    // Now we examine the event map looking for cases where a given input event corresponds to
    // more than one branch.

    val noDupes = eventMap.map {
      case (event, terms) => {
        Assert.invariant(terms.length > 0)
        if (terms.length > 1) {
          if (
            terms.exists { term =>
              term match {
                // any element children in any of the trds?
                // because if so, we have a true ambiguity here.
                case sg: SequenceTermBase => {
                  val nonOVCEltChildren = sg.groupMembers.filter {
                    case eb: ElementBase => !eb.isOutputValueCalc
                    case _ => false
                  }
                  nonOVCEltChildren.length > 0
                }
                case _ => false
              }
            }
          ) {
            // Possibly due to presence of a element with dfdl:outputValueCalc, XML Schema's
            // UPA check may not catch this ambiguity. However, we need a real element
            // with unique name, to unambiguously identify a branch.
            // So if there is ambiguity at this point, we have to fail.
            SDE(
              "UPA violation. Multiple choice branches begin with %s.\n" +
                "Note that elements with dfdl:outputValueCalc cannot be used to distinguish choice branches.\n" +
                "Note that choice branches with entirely optional content are not allowed.\n" +
                "The offending choice branches are:\n%s",
              event.qname,
              terms
                .map { trd =>
                  "%s at %s".format(trd.diagnosticDebugName, trd.locationDescription)
                }
                .mkString("\n")
            )
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
              eventType,
              event.qname,
              terms
                .map { trd =>
                  "%s at %s".format(trd.diagnosticDebugName, trd.locationDescription)
                }
                .mkString("\n")
            )
          }
        }
        (event, terms(0))
      }
    }
    (noDupes, optDefaultBranch)
  }

  final lazy val modelGroupRuntimeData = choiceRuntimeData

  final lazy val choiceRuntimeData = {
    new ChoiceRuntimeData(
      position,
      Delay(Symbol("ChoicePartialNextElementResolver"), this, partialNextElementResolver),
      schemaSet.variableMap,
      encodingInfo,
      // elementChildren.map { _.elementRuntimeData.dpathElementCompileInfo },
      schemaFileLocation,
      dpathCompileInfo,
      diagnosticDebugName,
      path,
      defaultBitOrder,
      groupMembersRuntimeData,
      isRepresented,
      couldHaveText,
      alignmentValueInBits,
      hasNoSkipRegions,
      optIgnoreCase,
      fillByteEv,
      maybeCheckByteAndBitOrderEv,
      maybeCheckBitOrderAndCharsetEv
    )
  }
}
