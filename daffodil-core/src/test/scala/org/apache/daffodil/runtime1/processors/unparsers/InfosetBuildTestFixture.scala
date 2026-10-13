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

package org.apache.daffodil.runtime1.processors.unparsers

import java.io.ByteArrayOutputStream
import scala.jdk.CollectionConverters.*
import scala.xml.Node

import org.apache.daffodil.api
import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.runtime1.infoset.InfosetBuildCursor
import org.apache.daffodil.runtime1.infoset.InfosetInputter
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.TermRuntimeData

/**
 * Shared helpers for tests of the infoset build cursor and build state.
 */
object InfosetBuildTestFixture {
  private def throwDiagnostics(ds: java.util.List[api.Diagnostic]): Nothing =
    throw new Exception(ds.asScala.map(_.getMessage()).mkString("\n"))

  /**
   * Compiles testSchema with the given tunables and returns the resulting
   * DataProcessor without a saveAndReload round-trip, since the tests build
   * state directly off the live object.
   */
  def compileForUnparse(
    testSchema: Node,
    tunables: Map[String, String] = Map.empty
  ): DataProcessor = {
    val pf = Compiler().withTunables(tunables).compileNode(testSchema)
    if (pf.isError) throwDiagnostics(pf.getDiagnostics)
    val dp = pf.onPath("/").asInstanceOf[DataProcessor]
    if (dp.isError) throwDiagnostics(dp.getDiagnostics)
    dp
  }

  /**
   * Builds a fresh InfosetInputter walking infosetXML against dp, already
   * initialized with the root TRD pushed.
   */
  def newInitializedInputter(infosetXML: Node, dp: DataProcessor): InfosetInputter = {
    val inputter = new InfosetInputter(new ScalaXMLInfosetInputter(infosetXML))
    inputter.initialize(dp.ssrd.elementRuntimeData, dp.tunables)
    inputter
  }

  /**
   * Unparses infosetXML event driven, then again by building the infoset ahead
   * and unparsing the built tree. Returns (eventDrivenBytes, buildAheadBytes) for
   * the caller to assert equality on.
   */
  def getEventDrivenAndBuildAheadBytes(
    dp: DataProcessor,
    infosetXML: Node
  ): (Array[Byte], Array[Byte]) = {
    val eventDrivenOut = new ByteArrayOutputStream()
    val eventDrivenResult = dp.unparse(new ScalaXMLInfosetInputter(infosetXML), eventDrivenOut)
    if (eventDrivenResult.isError) throwDiagnostics(eventDrivenResult.getDiagnostics)

    val buildAheadOut = new ByteArrayOutputStream()
    val buildAheadInputter = newInitializedInputter(infosetXML, dp)
    val cursor = new InfosetBuildCursor(
      dp.ssrd.builder,
      new InfosetBuildState(buildAheadInputter, dp.tunables)
    )
    val buildAheadState = UState.createInitialUStateForBuildAhead(
      buildAheadOut,
      dp,
      buildAheadInputter,
      false,
      new TreeEventState(cursor, false)
    )
    buildAheadState.getDataOutputStream.setPriorBitOrder(
      dp.ssrd.elementRuntimeData.defaultBitOrder
    )

    val rootUnparser = dp.ssrd.unparser
    buildAheadState.pushTRD(dp.ssrd.elementRuntimeData)
    rootUnparser.unparse1(buildAheadState)
    buildAheadState.popTRD(rootUnparser.context.asInstanceOf[TermRuntimeData])
    cursor.advance(lastAdvance = true)
    buildAheadState.evalSuspensions(isFinal = true)
    buildAheadState.getDataOutputStream.setFinished(buildAheadState)

    (eventDrivenOut.toByteArray, buildAheadOut.toByteArray)
  }
}
