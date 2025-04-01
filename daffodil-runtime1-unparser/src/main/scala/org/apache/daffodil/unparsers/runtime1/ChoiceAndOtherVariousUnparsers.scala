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

package org.apache.daffodil.unparsers.runtime1

import scala.jdk.CollectionConverters._

import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.ProperlySerializableMap._
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors._
import org.apache.daffodil.runtime1.processors.unparsers._

case class ChoiceBranchMap(
  lookupTable: ProperlySerializableMap[ChoiceBranchEvent, Unparser],
  unmappedDefault: Option[Unparser]
) extends Serializable {

  def get(cbe: ChoiceBranchEvent): Maybe[Unparser] = {
    val fromTable = lookupTable.get(cbe)
    val res =
      if (fromTable != null) One(fromTable)
      else {
        //
        // There must be an unmapped default in this case
        // because otherwise the map is incomplete.
        //
        if (unmappedDefault.isDefined)
          One(unmappedDefault.get)
        else
          Nope
      }
    res
  }

  def defaultUnparser = unmappedDefault

  def childProcessors = lookupTable.values.iterator.asScala.toVector ++ unmappedDefault

  def keys = lookupTable.keySet.asScala
}

/*
 * Sometimes choices have an empty branch (e.g. a sequence that just has an
 * assert in it) that optimizes to a NadaUnparser. NadaUnparsers should all
 * optimize out, but the ChoiceCombinatorUnparser still expects to have
 * something in these cases. So we have a special empty branch unparser that
 * does nothing, but gives the ChoiceCombinatorUnparser something that it can
 * use.
 */
class ChoiceBranchEmptyUnparser(val context: RuntimeData) extends PrimUnparserNoData {

  override def runtimeDependencies = Vector()

  def unparse(state: UState): Unit = {
    // do nothing
  }
}

class ChoiceCombinatorUnparser(
  mgrd: ModelGroupRuntimeData,
  choiceBranchMap: ChoiceBranchMap,
  choiceLengthInBits: MaybeInt
) extends CombinatorUnparser(mgrd)
  with ToBriefXMLImpl {
  override def nom = "Choice"

  override def runtimeDependencies = Vector()

  override def childProcessors = choiceBranchMap.childProcessors

  def unparse(state: UState): Unit = {
    if (state.withinHiddenNest) {
      val branchForUnparseIfHidden = choiceBranchMap.defaultUnparser
      branchForUnparseIfHidden.get.unparse1(state)
    } else {
      state.pushTRD(mgrd)
      val event: InfosetAccessor = state.inspectOrError
      val key: ChoiceBranchEvent = event match {
        //
        // The ChoiceBranchStartEvent(...) is not a case class constructor. It is a
        // hash-table lookup for a cached value. This avoids constructing these
        // objects over and over again.
        //
        case e if e.isStart && e.isElement => ChoiceBranchStartEvent(e.erd.namedQName)
        case e if e.isEnd && e.isElement => ChoiceBranchEndEvent(e.erd.namedQName)
        case e if e.isStart && e.isArray => ChoiceBranchStartEvent(e.erd.namedQName)
        case e if e.isEnd && e.isArray => ChoiceBranchEndEvent(e.erd.namedQName)
      }

      val maybeChildUnparser = choiceBranchMap.get(key)
      if (maybeChildUnparser.isEmpty) {
        UnparseError(
          One(mgrd.schemaFileLocation),
          One(state.currentLocation),
          "Found next element %s, but expected one of %s.",
          key.qname.toExtendedSyntax,
          choiceBranchMap.keys
            .map {
              _.qname.toExtendedSyntax
            }
            .mkString(", ")
        )
      }
      val childUnparser = maybeChildUnparser.get
      state.popTRD(mgrd)
      state.pushTRD(childUnparser.context.asInstanceOf[TermRuntimeData])
      if (choiceLengthInBits.isDefined) {
        val suspendableOp =
          new ChoiceUnusedUnparserSuspendableOperation(mgrd, choiceLengthInBits.get)
        val choiceUnusedUnparser =
          new ChoiceUnusedUnparser(mgrd, choiceLengthInBits.get, suspendableOp)

        suspendableOp.captureDOSStartForChoiceUnused(state)
        childUnparser.unparse1(state)
        suspendableOp.captureDOSEndForChoiceUnused(state)
        choiceUnusedUnparser.unparse(state)
      } else {
        childUnparser.unparse1(state)
      }
      state.popTRD(childUnparser.context.asInstanceOf[TermRuntimeData])
    }
  }
}

class DelimiterStackUnparser(
  initiatorOpt: Maybe[InitiatorUnparseEv],
  separatorOpt: Maybe[SeparatorUnparseEv],
  terminatorOpt: Maybe[TerminatorUnparseEv],
  ctxt: TermRuntimeData,
  bodyUnparser: Unparser
) extends CombinatorUnparser(ctxt) {
  override def nom = "DelimiterStack"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else
      "<DelimiterStack initiator='" + initiatorOpt +
        "' separator='" + separatorOpt +
        "' terminator='" + terminatorOpt + "'>" +
        bodyUnparser.toBriefXML(depthLimit - 1) +
        "</DelimiterStack>"
  }

  override def childProcessors = Vector(bodyUnparser)

  override def runtimeDependencies =
    (initiatorOpt.toList ++ separatorOpt.toList ++ terminatorOpt.toList).toVector

  def unparse(state: UState): Unit = {
    // Evaluate Delimiters
    val init =
      if (initiatorOpt.isDefined) initiatorOpt.get.evaluate(state)
      else EmptyDelimiterStackUnparseNode.empty
    val sep =
      if (separatorOpt.isDefined) separatorOpt.get.evaluate(state)
      else EmptyDelimiterStackUnparseNode.empty
    val term =
      if (terminatorOpt.isDefined) terminatorOpt.get.evaluate(state)
      else EmptyDelimiterStackUnparseNode.empty

    val node = DelimiterStackUnparseNode(init, sep, term)

    state.pushDelimiters(node)

    bodyUnparser.unparse1(state)

    state.popDelimiters()
  }
}

class DynamicEscapeSchemeUnparser(
  escapeScheme: EscapeSchemeUnparseEv,
  ctxt: TermRuntimeData,
  bodyUnparser: Unparser
) extends CombinatorUnparser(ctxt) {
  override def nom = "EscapeSchemeStack"

  override def childProcessors = Vector(bodyUnparser)

  override def runtimeDependencies = Vector(escapeScheme)

  def unparse(state: UState): Unit = {
    // evaluate the dynamic escape scheme in the correct scope. the resulting
    // value is cached in the Evaluatable (since it is manually cached) and
    // future parsers/evaluatables that use this escape scheme will use that
    // cached value.
    escapeScheme.newCache(state)
    escapeScheme.evaluate(state)

    // Unparse
    bodyUnparser.unparse1(state)

    // invalidate the escape scheme cache
    escapeScheme.invalidateCache(state)
  }
}
