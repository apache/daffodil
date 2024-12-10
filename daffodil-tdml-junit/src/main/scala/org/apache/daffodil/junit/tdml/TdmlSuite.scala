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

package org.apache.daffodil.junit.tdml

import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.tdml.TDMLTestNotCompatibleException

import org.junit.AfterClass
import org.junit.AssumptionViolatedException
import org.junit.Rule
import org.junit.rules.TestName

/**
 * Mixin for a JUnit test suite companion object
 */
trait TdmlSuite {

  /**
   * Resource path to the TDML file to use for this JUnit suite
   */
  val tdmlResource: String

  /**
   * Function to get the directory containing tdmlResource. Useful if createRunner is overridden
   * to use the Runner(dir, file, ...) factory method
   */
  def tdmlDir: String = tdmlResource.substring(0, tdmlResource.lastIndexOf("/") + 1)

  /**
   * Function to get basename of tdmlResource. Useful if createRunner is overridden to use the
   * Runner(dir, file, ...) factory method
   */
  def tdmlFile: String = tdmlResource.substring(tdmlResource.lastIndexOf("/") + 1)

  /**
   * Default implementation to create a Runner using the tdmlResource. This function can be
   * overridden if additional options need to be passed to the Runner factory object, for
   * example to disable TDML validation. In most cases this should not be needed, since most
   * parameters should not be changed by normal schema tests, or can be defined in the TDML
   * file.
   */
  def createRunner(): Runner = Runner(tdmlResource)

  /**
   * Lazily build the runner when needed
   */
  final lazy val runner = createRunner()

  /**
   * Ensure all resources associated with the Runner (e.g. cached compiled schemas, parsed TDML
   * XML) to be freed once the test suite has completed
   */
  @AfterClass
  final def shutDown(): Unit = runner.reset
}

/**
 * Mixin for a JUnit test suite companion class
 */
trait TdmlTests {

  /**
   * The companion object that contains the runner to be used for tests in this class
   */
  val tdmlSuite: TdmlSuite

  /**
   * Run a test from the runner with the same name as the JUnit test
   *
   * If a tdml test is not compatible with the DFDL implementation used for the test, we mark
   * test test as having violated an assumption. This prevents the test from generating a test
   * case failure, and instead it is marked as "skipped".
   */
  final def test: Unit = {
    try {
      tdmlSuite.runner.runOneTest(testName)
    } catch {
      case e: TDMLTestNotCompatibleException =>
        throw new AssumptionViolatedException(e.getMessage, e)
    }
  }

  /**
   * Helper function to easily trace a test. This should only be used for debugging and running
   * a single test since "trace" modifies the state of the Runner causing all later tests in the
   * suite to run with trace enabled
   */
  final def trace: Unit = {
    tdmlSuite.runner.trace
    test
  }

  /**
   * Helper function to get the current JUnit test name. Useful for some tests that might want
   * to provide special diagnostics about the test name
   */
  final def testName = _name.getMethodName

  /**
   * JUnit magic for setting the test name. Because the way scala turns vals into class members
   * + methods, the Rule annotation must be on a def
   */
  private val _name: TestName = new TestName()
  @Rule final def name = _name
}
