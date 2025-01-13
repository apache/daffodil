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

package org.apache.daffodil

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Ignore
import org.junit.Test

object TestAF extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AF.tdml"
}

object TestAG extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AG.tdml"
}

object TestAP extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AP.tdml"
}

object TestAV000 extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AV000.tdml"
}

object TestAV001 extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AV001.tdml"
}

object TestAV002 extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AV002.tdml"
}

object TestAV003 extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AV003.tdml"
}

object TestAW extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AW.tdml"
}

object TestAX extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AX.tdml"
}

object TestAY extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AY.tdml"
}

object TestAZ extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/AZ.tdml"
}

object TestBA extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/BA.tdml"
}

object TestBB extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/BB.tdml"
}

object TestBC extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/BC.tdml"
}

object TestBD extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/BD.tdml"
}

object TestBE extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/BE.tdml"
}

object TestBF extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/BF.tdml"
}

object TestBG extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/BG.tdml"
}

object TestDelimited extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/delimTests.tdml"
}

object TestMixedBinary extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/mixed-binary-text.tdml"
}

object TestMultipleDiagnostics extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/multiple-diagnostics.tdml"
  override def createRunner() = Runner(tdmlDir, tdmlFile, compileAllTopLevel = true)
}

object TestMultipleDiagnosticsNoValidate extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/multiple-diagnostics.tdml"
  override def createRunner() =
    Runner(tdmlDir, tdmlFile, compileAllTopLevel = true, validateDFDLSchemas = false)
}

object TestNestedGroupRef extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/nested_group_ref.tdml"
}

object TestNestedSeparatorDelimited extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/nested-separator-delimited.tdml"
}

object TestRuntimeDiagnostics extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/runtime-diagnostics.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, compileAllTopLevel = true, validateTDMLFile = false)
}

object TestSequence extends TdmlSuite {
  val tdmlResource = "/test-suite/tresys-contributed/sequence.tdml"
}

class TestAF extends TdmlTests {
  val tdmlSuite = TestAF

  @Test def AF000 = test
  @Test def AF001 = test
  @Test def AF002 = test
}

class TestAG extends TdmlTests {
  val tdmlSuite = TestAG

  @Test def AG000 = test
  @Test def AG001 = test
  @Test def AG002 = test
}

class TestAP extends TdmlTests {
  val tdmlSuite = TestAP

  @Ignore @Test def AP000 = test
}

class TestAV000 extends TdmlTests {
  val tdmlSuite = TestAV000

  @Test def AV000 = test
}

class TestAV001 extends TdmlTests {
  val tdmlSuite = TestAV001

  @Test def AV001 = test
}

class TestAV002 extends TdmlTests {
  val tdmlSuite = TestAV002

  @Test def AV002 = test
}

class TestAV003 extends TdmlTests {
  val tdmlSuite = TestAV003

  @Test def AV003 = test
}

class TestAW extends TdmlTests {
  val tdmlSuite = TestAW

  @Test def AW000 = test
  @Test def AW001 = test
}

class TestAX extends TdmlTests {
  val tdmlSuite = TestAX

  @Test def AX000 = test
}

class TestAY extends TdmlTests {
  val tdmlSuite = TestAY

  @Test def AY000 = test
}

class TestAZ extends TdmlTests {
  val tdmlSuite = TestAZ

  @Test def AZ000 = test
}

class TestBA extends TdmlTests {
  val tdmlSuite = TestBA

  // Jira DFDL-1392 - Issue with escapeEscape character that is first and precedes an escape-block start.
  // Is being removed, but should be preserved as it does not precede an escape character, nor an escape block end.
  @Ignore @Test def BA000 = test
}

class TestBB extends TdmlTests {
  val tdmlSuite = TestBB

  // Jira DFDL-1392 - Issue with escapeEscape character that is first and precedes an escape-block start.
  // Is being removed, but should be preserved as it does not precede an escape character, nor an escape block end.
  @Ignore @Test def BB000 = test
}

class TestBC extends TdmlTests {
  val tdmlSuite = TestBC

  @Test def BC000 = test
}

class TestBD extends TdmlTests {
  val tdmlSuite = TestBD

  @Test def BD000 = test
}

class TestBE extends TdmlTests {
  val tdmlSuite = TestBE

  // DFDL-1010
  @Test def BE000 = test
  @Test def BE001 = test
}

class TestBF extends TdmlTests {
  val tdmlSuite = TestBF

  // DFDL-1010
  @Test def BF000 = test
  @Test def BF001 = test
}

class TestBG extends TdmlTests {
  val tdmlSuite = TestBG

  @Test def BG000 = test
}

class TestDelimited extends TdmlTests {
  val tdmlSuite = TestDelimited

  @Test def length_delimited_12_03_controversial = test
}

class TestMixedBinary extends TdmlTests {
  val tdmlSuite = TestMixedBinary

  // DFDL-935
  @Ignore @Test def encodingErrorPolicy_error = test
  @Ignore @Test def t2 = test
  @Ignore @Test def t3 = test
  @Test def t1 = test
}

class TestMultipleDiagnostics extends TdmlTests {
  val tdmlSuite = TestMultipleDiagnostics

  @Test def twoMissingTypeDefErrors = test
  @Test def manyErrors1 = test
}

class TestMultipleDiagnosticsNoValidate extends TdmlTests {
  val tdmlSuite = TestMultipleDiagnosticsNoValidate

  @Test def manyErrors2 = test
}

class TestNestedGroupRef extends TdmlTests {
  val tdmlSuite = TestNestedGroupRef

  @Test def nestedGroupRefs1 = test
}

class TestNestedSeparatorDelimited extends TdmlTests {
  val tdmlSuite = TestNestedSeparatorDelimited

  @Test def baseline = test

  // Fails in IBM DFDL - ambiguous separator/terminator not accepted.
  @Test def baseline_ibm = test

  @Test def basicNest = test
  @Test def basicNest2 = test

  @Test def nest1 = test
  @Test def nest2 = test
  @Test def nest3 = test
}

class TestRuntimeDiagnostics extends TdmlTests {
  val tdmlSuite = TestRuntimeDiagnostics

  @Test def PE1 = test
}

class TestSequence extends TdmlTests {
  val tdmlSuite = TestSequence

  @Test def seq1 = test
}
