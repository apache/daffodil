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
import java.nio.charset.StandardCharsets
import scala.xml.Node

import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset.DIArray
import org.apache.daffodil.runtime1.infoset.InfosetBuildCursor
import org.apache.daffodil.runtime1.infoset.StreamingInfosetWalker
import org.apache.daffodil.runtime1.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.runtime1.processors.TermRuntimeData

import org.junit.Assert.*
import org.junit.Test

/**
 * Tests the infoset build cursor and build state against the unparse that
 * reads the built tree as events: the event sequence build surfaces, the lead
 * counter, the build ahead window, and the build ahead unparse matching an
 * event-driven unparse for scalar, array and choice content.
 */
class TestInfosetBuildCursor {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  // A separated sequence of three scalars.
  private val separatedRowSchema = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"
      encoding="ascii"
      lengthUnits="bytes"
      outputNewLine="%CR;%LF;"/>,
    <xs:element name="row" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
          <xs:element name="name" type="xs:string" dfdl:lengthKind="delimited"/>
          <xs:element name="age" type="xs:string" dfdl:lengthKind="delimited"/>
          <xs:element name="city" type="xs:string" dfdl:lengthKind="delimited"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>,
    elementFormDefault = "unqualified"
  )

  private val separatedRowInfoset =
    <ex:row xmlns:ex={example}>
      <name>Alice</name>
      <age>30</age>
      <city>Boston</city>
    </ex:row>

  // A scalar, an array and a choice in one separated sequence.
  private val arrayChoiceSchema = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"
      encoding="ascii"
      lengthUnits="bytes"/>,
    <xs:element name="row" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
          <xs:element name="header" type="xs:string" dfdl:lengthKind="delimited"/>
          <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"
            dfdl:lengthKind="delimited"
            dfdl:occursCountKind="implicit"/>
          <xs:choice>
            <xs:element name="typeA" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="typeB" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:choice>
        </xs:sequence>
      </xs:complexType>
    </xs:element>,
    elementFormDefault = "unqualified"
  )

  // Three "item" occurrences exercise the array loop; typeB (not typeA)
  // exercises actual choice resolution. lengthKind=delimited (not explicit)
  // avoids double-firing CaptureStartOfContentLengthUnparser's non-idempotent
  // marker, since a tree reused from a completed unparse runs it again.
  private val arrayChoiceInfoset =
    <ex:row xmlns:ex={example}>
      <header>H</header>
      <item>a</item>
      <item>b</item>
      <item>c</item>
      <typeB>X</typeB>
    </ex:row>

  /**
   * A build side, an InfosetBuildCursor over an InfosetBuildState, and an
   * unparse side that reads the tree the build side makes.
   */
  private final class BuildAheadRun(sch: Node, infoset: Node, buildAheadLimit: Long = 100) {
    val dp = InfosetBuildTestFixture.compileForUnparse(
      sch,
      Map(
        "releaseUnneededInfoset" -> "false",
        "infosetBuilderMode" -> "buildAhead",
        "unparseBuildAheadWindowNodes" -> buildAheadLimit.toString
      )
    )
    val buildInputter = InfosetBuildTestFixture.newInitializedInputter(infoset, dp)
    val buildState = new InfosetBuildState(buildInputter, dp.tunables)
    val cursor = new InfosetBuildCursor(dp.ssrd.builder, buildState)

    def buildAll(): Unit = cursor.advance(lastAdvance = true)

    def rootNode = buildInputter.documentElement.child(0).asComplex

    // Unparses the tree build produced, pulling build forward as the unparse
    // needs it, and returns the output.
    def unparseBuiltTree(): String = {
      val out = new ByteArrayOutputStream()
      val state = UState.createInitialUStateForBuildAhead(
        out,
        dp,
        buildInputter,
        false,
        new TreeEventState(cursor, false)
      )
      state.getDataOutputStream.setPriorBitOrder(dp.ssrd.elementRuntimeData.defaultBitOrder)

      val rootUnparser = dp.ssrd.unparser
      state.pushTRD(dp.ssrd.elementRuntimeData)
      rootUnparser.unparse1(state)
      state.popTRD(rootUnparser.context.asInstanceOf[TermRuntimeData])
      // Build may still have trailing end events to consume, and a speculative
      // separator is written via a suspension that must drain before the DOS
      // is finalized.
      buildAll()
      state.evalSuspensions(isFinal = true)
      state.getDataOutputStream.setFinished(state)
      new String(out.toByteArray, StandardCharsets.US_ASCII)
    }
  }

  // A tree built outside an event-driven unparse, compared against that unparse.
  private def assertBuildAheadMatchesEventDriven(sch: Node, infoset: Node): Array[Byte] = {
    // Event-driven on purpose: the tunable would otherwise replace it.
    val dp = InfosetBuildTestFixture.compileForUnparse(
      sch,
      Map("releaseUnneededInfoset" -> "false", "infosetBuilderMode" -> "eventDriven")
    )
    val (eventDrivenBytes, buildAheadBytes) =
      InfosetBuildTestFixture.getEventDrivenAndBuildAheadBytes(dp, infoset)
    assertArrayEquals(eventDrivenBytes, buildAheadBytes)
    eventDrivenBytes
  }

  @Test def testBuildStateSurfacesCorrectEventSequence(): Unit = {
    val run = new BuildAheadRun(separatedRowSchema, separatedRowInfoset)

    // Drives through the actual InfosetBuilder frames rather than hand-driven
    // advance() calls, since next-element resolution depends on the TRD
    // push/pop the element frame performs. This schema's separator never
    // reaches InfosetBuildState: the InfosetBuilder tree skips the
    // delimiter-stack wrapper.
    run.buildAll()

    assertEquals(4L, run.buildState.currentLead) // row, name, age, city

    assertEquals(3, run.rootNode.numChildren)
    assertEquals("name", run.rootNode.child(0).erd.name)
    assertEquals("age", run.rootNode.child(1).erd.name)
    assertEquals("city", run.rootNode.child(2).erd.name)
  }

  @Test def testLeadCounterIncrementsOnBuildAndDecrementsOnUnparse(): Unit = {
    // Fixed dfdl:length is safe here because this tree comes from
    // InfosetBuildState, which never runs content-unparsing (including
    // CaptureStartOfContentLengthUnparser).
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="name" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="age" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="2"/>
            <xs:element name="city" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="6"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    val run = new BuildAheadRun(sch, separatedRowInfoset)

    assertEquals(0L, run.buildState.currentLead)
    run.buildAll()
    // row itself, name, age, city = 4 elements total, each incrementing once
    // via unparseBegin's actual hookup.
    assertEquals(4L, run.buildState.currentLead)

    // The unparse reads the same already-built tree, decrementing the lead
    // counter as it goes.
    assertEquals("Alice30Boston", run.unparseBuiltTree())

    // The unparse decremented once per element too, so the counter is back to
    // 0: build and the unparse agree on how many nodes exist.
    assertEquals(0L, run.buildState.currentLead)
  }

  // With a small buildAheadLimit, one advance() leaves the lead counter just
  // past it, and the unparse is still correct across the refills the unparse
  // triggers while it runs.
  @Test def testBuildStopsAtBuildAheadLimit(): Unit = {
    val numItems = 40
    val buildAheadLimit = 3L

    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"
              dfdl:lengthKind="delimited"
              dfdl:occursCountKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    val items = (0 until numItems).map(i => <item>{s"i$i"}</item>)
    val infoset =
      <ex:row xmlns:ex={example}>
        {items}
      </ex:row>
    val expectedBytes = (0 until numItems).map(i => s"i$i").mkString(",")

    val run = new BuildAheadRun(sch, infoset, buildAheadLimit)

    run.cursor.advance()

    // numItems + 1 (row + all items) is what currentLead would equal here if
    // advance() ignored the build ahead limit. It counts one node per element, so
    // it stops at the first node past the limit.
    assertFalse("expected build to stop with more left to build", run.cursor.isFinished)
    assertEquals(
      s"expected build to stop as soon as the lead passed buildAheadLimit=$buildAheadLimit " +
        s"(numItems=$numItems)",
      buildAheadLimit + 1,
      run.buildState.currentLead
    )

    // The unparse pulls the rest of the tree forward as it needs it; the
    // output must be byte-for-byte correct despite having been built across
    // many separate advance() calls rather than a single one-shot build pass.
    assertEquals(expectedBytes, run.unparseBuiltTree())
  }

  @Test def testBuildAheadMatchesEventDriven(): Unit = {
    assertBuildAheadMatchesEventDriven(separatedRowSchema, separatedRowInfoset)
  }

  @Test def testArrayAndChoiceBuildAheadMatchesEventDriven(): Unit = {
    val eventDrivenBytes =
      assertBuildAheadMatchesEventDriven(arrayChoiceSchema, arrayChoiceInfoset)
    assertEquals("H,a,b,c,X", new String(eventDrivenBytes, StandardCharsets.US_ASCII))
  }

  // Drives InfosetBuildState directly, then feeds its tree to the unparse
  // (end-to-end build-then-unparse).
  @Test def testStandaloneBuildStateNavigatesArrayChoiceSeparator(): Unit = {
    val run = new BuildAheadRun(arrayChoiceSchema, arrayChoiceInfoset)

    // The cursor builds the whole tree from the inputter, including the array
    // and choice content.
    run.buildAll()

    // row, header, item x3, typeB = 6 elements total.
    assertEquals(6L, run.buildState.currentLead)

    assertEquals(3, run.rootNode.numChildren)
    assertEquals("header", run.rootNode.child(0).erd.name)
    assertEquals("item", run.rootNode.child(1).erd.name)
    assertEquals(3, run.rootNode.child(1).asInstanceOf[DIArray].numChildren)
    assertEquals("typeB", run.rootNode.child(2).erd.name)

    // Unparsing the tree InfosetBuildState just constructed confirms it's a
    // usable, fully-built tree, not just a navigation exercise.
    assertEquals("H,a,b,c,X", run.unparseBuiltTree())
  }

  // Each event as "start element name", "end array name" and so on.
  private def eventDescriptions(events: InfosetEventState): List[String] = {
    val descriptions = List.newBuilder[String]
    while (events.advance) {
      val event = events.advanceAccessor
      val position = if (event.isStart) {
        "start"
      } else {
        "end"
      }
      val kind = if (event.isElement) {
        "element"
      } else {
        "array"
      }
      descriptions += position + " " + kind + " " + event.erd.name
    }
    descriptions.result()
  }

  @Test def testTreeEventsForScalars(): Unit = {
    val run = new BuildAheadRun(separatedRowSchema, separatedRowInfoset)
    val treeEvents = new TreeEventState(run.cursor, true)
    assertEquals(
      List(
        "start element row",
        "start element name",
        "end element name",
        "start element age",
        "end element age",
        "start element city",
        "end element city",
        "end element row"
      ),
      eventDescriptions(treeEvents)
    )
  }

  private val arrayChoiceEvents = List(
    "start element row",
    "start element header",
    "end element header",
    "start array item",
    "start element item",
    "end element item",
    "start element item",
    "end element item",
    "start element item",
    "end element item",
    "end array item",
    "start element typeB",
    "end element typeB",
    "end element row"
  )

  @Test def testTreeEventsForArrayAndChoice(): Unit = {
    val run = new BuildAheadRun(arrayChoiceSchema, arrayChoiceInfoset)
    val treeEvents = new TreeEventState(run.cursor, true)
    assertEquals(arrayChoiceEvents, eventDescriptions(treeEvents))
  }

  @Test def testTreeEventsPullBuildOnlyAsFarAsNeeded(): Unit = {
    val run = new BuildAheadRun(arrayChoiceSchema, arrayChoiceInfoset, buildAheadLimit = 1)
    val treeEvents = new TreeEventState(run.cursor, true)
    assertTrue(treeEvents.advance)
    assertEquals("row", treeEvents.advanceAccessor.erd.name)
    // Only the root has been needed so far, so build has not run to the end.
    assertFalse(run.cursor.isFinished)
    assertEquals(arrayChoiceEvents.tail, eventDescriptions(treeEvents))
    assertTrue(run.cursor.isFinished)
  }

  // The infoset as a debugger shows it: the whole built tree, limited to what
  // the unparse has reached.
  private def reachedInfoset(run: BuildAheadRun, treeEvents: TreeEventState): String = {
    val out = new ByteArrayOutputStream()
    val xml = new XMLTextInfosetOutputter(out, pretty = false, minimal = true)
    StreamingInfosetWalker(
      run.buildInputter.documentElement,
      xml,
      walkHidden = false,
      ignoreBlocks = true,
      releaseUnneededInfoset = false,
      visibleChildCounts = treeEvents.reachedChildCounts()
    ).walk(lastWalk = true)
    out.toString("UTF-8")
  }

  @Test def testReachedChildCountsLimitTheDebuggerInfoset(): Unit = {
    val run = new BuildAheadRun(separatedRowSchema, separatedRowInfoset)
    run.buildAll()
    val treeEvents = new TreeEventState(run.cursor, false)

    // start row, start name, end name
    assertTrue(treeEvents.advance)
    assertTrue(treeEvents.advance)
    assertTrue(treeEvents.advance)
    val afterName = reachedInfoset(run, treeEvents)
    assertTrue(afterName, afterName.contains("Alice"))
    assertFalse(afterName, afterName.contains("30"))

    // The start of age is computed but not consumed, so age is not reached.
    assertTrue(treeEvents.inspect)
    assertFalse(reachedInfoset(run, treeEvents).contains("30"))

    assertTrue(treeEvents.advance)
    val afterAgeStart = reachedInfoset(run, treeEvents)
    assertTrue(afterAgeStart, afterAgeStart.contains("30"))
    assertFalse(afterAgeStart, afterAgeStart.contains("Boston"))
  }
}
