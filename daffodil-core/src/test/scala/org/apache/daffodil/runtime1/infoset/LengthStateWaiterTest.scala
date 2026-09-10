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

package org.apache.daffodil.runtime1.infoset

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.unparsers.UState

import org.junit.Assert.*
import org.junit.Test
import passera.unsigned.ULong

/**
 * Regression guard for block()/Suspension.registerWaiter/SuspensionWaiter.
 * removeSuspension: a suspension whose length dependency shifts from
 * element A to element B between retries must be deregistered from A's
 * waiter once it registers with B's; otherwise A's eventual
 * notifySuspensions() would retry it pointlessly even though it now
 * depends on B, not A. block() always precedes registerWaiter on a real
 * retry, so this drives that same sequence directly.
 *
 * Drives LengthState/Suspension directly with a minimal Suspension double
 * (registerSuspension/removeSuspension only compare by reference
 * identity, never call any method on it) rather than through a full DFDL
 * schema: reliably forcing a real dependency shift via schema+tunable
 * timing isn't deterministic, while this exercises the exact bookkeeping
 * that changed.
 */
class LengthStateWaiterTest {

  private def compileTrivialSchema(): DataProcessor = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="b" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val pf = Compiler().compileNode(sch)
    assertFalse(pf.getDiagnostics.toString, pf.isError)
    pf.onPath("/").asInstanceOf[DataProcessor]
  }

  @Test def testDeregistersFromPriorLengthStateOnShift(): Unit = {
    val dp = compileTrivialSchema()

    // Two distinct DIElement instances is all this needs; the
    // register/remove bookkeeping never inspects element content, so
    // there's no need to drive a real parse/unparse to populate one.
    val elementA = new DISimple(dp.ssrd.elementRuntimeData)
    val elementB = new DISimple(dp.ssrd.elementRuntimeData)

    val lengthStateA = new ContentLengthState(elementA)
    val lengthStateB = new ContentLengthState(elementB)

    // registerSuspension/removeSuspension only ever compare by reference
    // identity; neither doTask nor rd is ever actually invoked by the
    // code under test here.
    val suspension = new Suspension {
      override def rd = elementA.erd
      override protected def doTask(ustate: UState): Unit = ()
    }

    val exc = new RuntimeException("test")

    suspension.block(elementA, elementA.erd, 0, exc)
    suspension.registerWaiter(lengthStateA.suspensionWaiter)
    assertTrue(
      "expected the suspension to be registered on A right after registering",
      lengthStateA.isRegisteredWaiter(suspension)
    )
    assertTrue(
      "expected isParked to be true - this is the field SuspensionTracker " +
        "actually reads to decide whether to park the suspension",
      suspension.isParked
    )

    // A subsequent re-block on the same reason must still deregister from
    // whatever it was previously registered on before it re-registers;
    // here, that happens to be the same LengthState (A again).
    suspension.block(elementA, elementA.erd, 0, exc)
    suspension.registerWaiter(lengthStateA.suspensionWaiter)
    assertTrue(
      "expected the suspension to remain registered on A after a re-block/re-register cycle",
      lengthStateA.isRegisteredWaiter(suspension)
    )

    // The suspension's dependency shifts to B on a later retry, without A
    // ever having resolved (A.suspensionWaiter.notifySuspensions() was
    // never called).
    suspension.block(elementB, elementB.erd, 0, exc)
    suspension.registerWaiter(lengthStateB.suspensionWaiter)

    assertFalse(
      "expected the suspension to be deregistered from A once it registered with B instead",
      lengthStateA.isRegisteredWaiter(suspension)
    )
    assertTrue(
      "expected the suspension to be registered on B",
      lengthStateB.isRegisteredWaiter(suspension)
    )
    assertTrue(
      "expected isParked to still be true, now for B",
      suspension.isParked
    )
  }

  /**
   * Regression guard for a non-InfosetLengthUnknownException block(),
   * as SuspendableOperation passes: registerWaiter's isEmpty precondition
   * must still hold on repeated block()+registerWaiter cycles, mirroring
   * how maybeRegisterWaiterOnBlock calls it directly right after block().
   */
  @Test def testRepeatedBlockAndExternalRegisterWaiterDoesNotTripInvariant(): Unit = {
    val dp = compileTrivialSchema()
    val elementA = new DISimple(dp.ssrd.elementRuntimeData)
    val lengthStateA = new ContentLengthState(elementA)

    val suspension = new Suspension {
      override def rd = elementA.erd
      override protected def doTask(ustate: UState): Unit = ()
    }

    // Not an InfosetLengthUnknownException, so block() itself registers
    // nothing here, mirroring SuspendableOperation's own exc = this.
    val exc = new RuntimeException("test")

    for (i <- 1 to 3) {
      suspension.block(elementA, elementA.erd, 0, exc)
      // Throws (registerWaiter's isEmpty invariant) if block() left a
      // stale registration in place from the previous iteration.
      suspension.registerWaiter(lengthStateA.suspensionWaiter)
      assertTrue(
        s"expected the suspension to be registered on iteration $i",
        lengthStateA.isRegisteredWaiter(suspension)
      )
      assertTrue(s"expected isParked to be true on iteration $i", suspension.isParked)
    }
  }

  @Test def testSetDoneDeregistersFromWaiter(): Unit = {
    val dp = compileTrivialSchema()
    val elementA = new DISimple(dp.ssrd.elementRuntimeData)
    val lengthStateA = new ContentLengthState(elementA)

    val suspension = new Suspension {
      override def rd = elementA.erd
      override protected def doTask(ustate: UState): Unit = ()
    }

    val exc = new RuntimeException("test")
    suspension.block(elementA, elementA.erd, 0, exc)
    suspension.registerWaiter(lengthStateA.suspensionWaiter)

    // A suspension can resolve via the periodic fallback retry rather than
    // via its registered waiter firing. setDone() must deregister it in
    // that case too, or it stays reachable from a waiter that will never
    // notify it again.
    suspension.setDone()

    assertFalse(
      "expected setDone() to deregister the suspension from its waiter",
      lengthStateA.isRegisteredWaiter(suspension)
    )
    assertFalse(
      "expected isParked to be false after setDone()",
      suspension.isParked
    )
  }

  @Test def testClearDeregistersAllSuspensions(): Unit = {
    val dp = compileTrivialSchema()
    val elementA = new DISimple(dp.ssrd.elementRuntimeData)
    val lengthStateA = new ContentLengthState(elementA)

    val suspension = new Suspension {
      override def rd = elementA.erd
      override protected def doTask(ustate: UState): Unit = ()
    }

    val exc = new RuntimeException("test")
    suspension.block(elementA, elementA.erd, 0, exc)
    suspension.registerWaiter(lengthStateA.suspensionWaiter)

    // clear() (e.g. from LengthState.copyFrom/clear on reuse) drops every
    // suspension without notifying them. It must also reset their own
    // isParked, or a dropped suspension is left believing a
    // wake-up is still armed for a waiter that no longer tracks it.
    lengthStateA.suspensionWaiter.clear()

    assertFalse(
      "expected isParked to be false after the waiter was cleared",
      suspension.isParked
    )
  }

  /**
   * Regression guard for SuspensionWaiter's single-registrant fast path:
   * cond() reentrantly notifying this same waiter (mirroring LengthState
   * migrating a position mid-evaluation) must not corrupt the
   * registration when neither the outer nor inner evaluation resolves.
   */
  @Test def testReentrantNotifyDuringSoleCondDoesNotCorruptState(): Unit = {
    val dp = compileTrivialSchema()
    val elementA = new DISimple(dp.ssrd.elementRuntimeData)
    val lengthStateA = new ContentLengthState(elementA)
    val waiter = lengthStateA.suspensionWaiter

    val suspension = new Suspension {
      override def rd = elementA.erd
      override protected def doTask(ustate: UState): Unit = ()
    }

    val exc = new RuntimeException("test")
    suspension.block(elementA, elementA.erd, 0, exc)

    var condCalls = 0
    suspension.registerWaiter(
      waiter,
      () => {
        condCalls += 1
        if (condCalls == 1) {
          waiter.notifySuspensions()
        }
        false
      }
    )

    waiter.notifySuspensions()

    assertEquals("expected the outer call and one reentrant call", 2, condCalls)
    assertTrue(
      "expected the suspension to remain registered: neither call resolved",
      waiter.isRegisteredSuspension(suspension)
    )
    assertTrue("expected isParked to remain true", suspension.isParked)
  }

  /**
   * Regression guard for SuspensionWaiter's per-registration condition:
   * a registrant whose condition doesn't hold yet must stay registered
   * through a notify instead of being promoted (or silently dropped)
   * just because some notify fired. This is what let LengthState's own
   * bespoke notifySuspensions() override retire in favor of passing its
   * re-verify check as a condition at registration time instead.
   */
  @Test def testConditionGatesNotify(): Unit = {
    val dp = compileTrivialSchema()
    val elementA = new DISimple(dp.ssrd.elementRuntimeData)
    val lengthStateA = new ContentLengthState(elementA)

    val suspension = new Suspension {
      override def rd = elementA.erd
      override protected def doTask(ustate: UState): Unit = ()
    }

    var conditionHolds = false
    val exc = new RuntimeException("test")
    suspension.block(elementA, elementA.erd, 0, exc)
    suspension.registerWaiter(lengthStateA.suspensionWaiter, () => conditionHolds)

    lengthStateA.suspensionWaiter.notifySuspensions()
    assertTrue(
      "expected the suspension to remain registered: its condition doesn't hold yet",
      lengthStateA.isRegisteredWaiter(suspension)
    )
    assertTrue("expected isParked to remain true", suspension.isParked)
  }

  /**
   * Regression guard for the broad-registration fix: maybeLengthInBits
   * can resolve via several different paths (same-DOS, chain-walk,
   * mixed absolute/relative), so it must register on every DOS whose
   * state could make any of those paths succeed, not just whichever
   * single fact the branch taken on one particular call happened to
   * depend on, which could miss the length becoming computable via a
   * different path entirely.
   */
  @Test def testRegistersBroadlyAcrossStartAndEndAndNotifiesOnEither(): Unit = {
    val dp = compileTrivialSchema()
    val element = new DISimple(dp.ssrd.elementRuntimeData)
    val lengthState = new ContentLengthState(element)

    // A root DOS with two chained buffered children: dosA and dosB are
    // distinct DOS instances, both starting with no absolute position
    // (a freshly buffered DOS resets its bit pos on creation).
    val root = DirectOrBufferedDataOutputStream(
      new java.io.ByteArrayOutputStream(),
      null,
      false,
      4096,
      2000 * (1 << 20),
      new java.io.File("."),
      Maybe.Nope
    )
    val dosA = root.addBuffered()
    val dosB = dosA.addBuffered()

    lengthState.maybeStartDataOutputStream = Maybe(dosA)
    lengthState.maybeStartPos0bInBits = MaybeULong(0)
    lengthState.maybeEndDataOutputStream = Maybe(dosB)
    lengthState.maybeEndPos0bInBits = MaybeULong(5)

    assertTrue(
      "neither DOS has an absolute position yet, so the length can't be computed",
      lengthState.maybeLengthInBits().isEmpty
    )
    assertEquals(
      "expected broad registration on both start and end DOSs for their " +
        "absolute position becoming known, not just whichever one an " +
        "older, single-DOS design would have picked",
      Set(dosA, dosB),
      lengthState.maybeAbsBitPosDoses
    )

    // Resolving only dosA's absolute position (directly, with no
    // Suspension actually registered to be notified: LengthState
    // itself never listens for this event, only a Suspension that
    // registered via dosListeners would) isn't enough by
    // itself to compute the length. A subsequent retry (standing in
    // for the real one a properly-registered Suspension's notification
    // would trigger) must find it still unresolved, and re-register,
    // now only on dosB, since start is no longer relative.
    dosA.setAbsStartingBitPos0b(ULong(100))
    assertTrue(
      "expected the length to still be unknown - only start resolved, not end",
      lengthState.maybeLengthInBits().isEmpty
    )
    assertEquals(
      "expected registration to have narrowed to just the remaining unresolved DOS",
      Set(dosB),
      lengthState.maybeAbsBitPosDoses
    )

    // Resolving dosB's absolute position now supplies the last missing
    // fact.
    dosB.setAbsStartingBitPos0b(ULong(200))
    val len = lengthState.maybeLengthInBits()
    assertTrue(
      "expected the length to be computable now that both endpoints are absolute",
      len.isDefined
    )
    assertEquals(105L, len.get)
  }
}
