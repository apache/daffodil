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

package org.apache.daffodil.tdml

import org.apache.daffodil.util.Misc
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.HasSetDebugger
import scala.xml.Node

/**
 * A wrapper to contain the optional parameters that can be used by the Runner
 * class, so that they don't need to be redefined in multiple constructors,
 * and can be easily defaulted by calling functions
 */

object RunnerOpts {
  def apply(validateTDMLFile: Boolean = true,
    validateDFDLSchemas: Boolean = true,
    compileAllTopLevel: Boolean = false,
    defaultRoundTripDefault: RoundTrip = RunnerOpts.defaultRoundTripDefaultDefault,
    defaultValidationDefault: String = RunnerOpts.defaultValidationDefaultDefault,
    defaultImplementationsDefault: Seq[String] = RunnerOpts.defaultImplementationsDefaultDefault,
    shouldDoErrorComparisonOnCrossTests: Boolean = RunnerOpts.defaultShouldDoErrorComparisonOnCrossTests,
    shouldDoWarningComparisonOnCrossTests: Boolean = RunnerOpts.defaultShouldDoWarningComparisonOnCrossTests): RunnerOpts = 
      new RunnerOpts(validateTDMLFile, validateDFDLSchemas, compileAllTopLevel, 
        defaultRoundTripDefault, defaultValidationDefault, defaultImplementationsDefault, 
        shouldDoErrorComparisonOnCrossTests, shouldDoWarningComparisonOnCrossTests)

  def defaultRoundTripDefaultDefault: RoundTrip = NoRoundTrip
  def defaultValidationDefaultDefault: String = "off"

  /**
   * Default for what DFDL implementations to run tests against.
   *
   * A test or test suite can override this to specify more or different implementations
   * that the test should pass for.
   */
  def defaultImplementationsDefaultDefault: Seq[String] = Seq("daffodil", "ibm")

  /**
   * By default we don't run Daffodil negative TDML tests against cross-testers.
   * The error messages are simply too varied.
   *
   * Negative tests must fail, but error messages aren't compared.
   */
   def defaultShouldDoErrorComparisonOnCrossTests: Boolean = false

  /**
   * By default we don't cross test warning messages because they are too varied.
   */
  val defaultShouldDoWarningComparisonOnCrossTests: Boolean = false
}

class RunnerOpts(
  var validateTDMLFile: Boolean = true,
  var validateDFDLSchemas: Boolean = true,
  var compileAllTopLevel: Boolean = false,
  var defaultRoundTripDefault: RoundTrip,
  var defaultValidationDefault: String,
  var defaultImplementationsDefault: Seq[String],
  var shouldDoErrorComparisonOnCrossTests: Boolean,
  var shouldDoWarningComparisonOnCrossTests: Boolean) {
  // Yes, that's a lot of defaults.....
  // but really it is 3-tiers deep:
  // roundTrip - on test case
  // defaultRoundTrip - on test suite
  // defaultRoundTripDefault - on runner aka test suite factory
  // defaultRoundTripDefaultDefault - on runner factory
}

/**
 * Creates the DFDLTestSuite object lazily, so the file isn't read into memory
 * and parsed unless you actually try to run a test using it.
 *
 * Creates the DFDLTestSuite only once.
 *
 * Provides a reset method to be called from @AfterClass to drop
 * the test suite object (and avoid memory leak).
 *
 * Note: I have verified that this does get called after each test suite has been run.
 *
 * defaultRoundTripDefault if true the round trip default for the test suite will be
 * this value, if the test suite does not specify defaultRoundTrip attribute.
 *
 * defaultRoundTripDefaultDefault
 */
object Runner {
  def apply(dir: String, file: String): Runner =
    new Runner(dir, file, RunnerOpts())

  def apply(dir: String, file: String, options: RunnerOpts): Runner =
    new Runner(dir, file, options)

  def apply(elem: scala.xml.Elem): Runner =
    new Runner(elem, RunnerOpts())

  def apply(elem: scala.xml.Elem, options: RunnerOpts): Runner =
    new Runner(elem, options)
}

/**
 * Needs to be thread-safe (i.e., use all thread-local state) so that
 * test can be run in parallel.
 *
 * Note however, that each thread will get its own copy of the DFDLTestSuite
 */
class Runner private (elem: scala.xml.Elem, dir: String, file: String, options: RunnerOpts) 
extends HasSetDebugger {

  /*
   * these constructors are for use by Java programs
   */
  def this(dir: String, file:String) =
    this(null, dir, file, RunnerOpts())

  def this(dir: String, file:String, options: RunnerOpts) =
    this(null, dir, file, options)

  def this(elem: scala.xml.Elem) =
    this(elem, null, null, RunnerOpts())

  def this(elem: scala.xml.Elem, options: RunnerOpts) =
    this(elem, null, null, options)

  if (elem ne null)
    Assert.usage((dir eq null) && (file eq null))
  else
    Assert.usage((dir ne null) && (file ne null))

  private lazy val resource = {
    // This is ok to be a hard-wired "/" because these are resource identifiers, which
    // are not file-system paths that have to be made platform-specific.
    // In other words, we don't need to use "\\" for windows here. "/" works there as well.
    val d = if (dir.endsWith("/")) dir else dir + "/"
    Misc.getRequiredResource(d + file)
  }

  private def getTS = {
    if (ts == null) {
      if (elem eq null) {
        tl_ts.set(new DFDLTestSuite(null, resource,
        options.validateTDMLFile,
        options.validateDFDLSchemas,
        options.compileAllTopLevel,
        options.defaultRoundTripDefault,
        options.defaultValidationDefault,
        options.defaultImplementationsDefault,
        options.shouldDoErrorComparisonOnCrossTests,
        options.shouldDoWarningComparisonOnCrossTests))
      } else {
        tl_ts.set(new DFDLTestSuite(null, elem,
        options.validateTDMLFile,
        options.validateDFDLSchemas,
        options.compileAllTopLevel,
        options.defaultRoundTripDefault,
        options.defaultValidationDefault,
        options.defaultImplementationsDefault,
        options.shouldDoErrorComparisonOnCrossTests,
        options.shouldDoWarningComparisonOnCrossTests))
      }
    }
    ts
  }

  private object tl_ts extends ThreadLocal[DFDLTestSuite]

  private def ts = tl_ts.get

  def testCases = tl_ts.get.testCases

  def unparserTestCases = tl_ts.get.unparserTestCases

  def parserTestCases = tl_ts.get.parserTestCases

  def isTDMLFileValid = tl_ts.get.isTDMLFileValid

  def runOneTest(testName: String, schema: Option[scala.xml.Node] = None, leakCheck: Boolean = false) =
    try {
      getTS.runOneTest(testName, schema, leakCheck)
    } finally {
      getTS.setDebugging(false)
    }

  def runOneTest(testName: String): Unit = {
    runOneTest(testName, None, false)
  }

  /**
   *  Call this from an @AfterClass method
   *  to drop any state (like the test suite object) so we don't leak
   */
  def reset: Unit = {
    try {
      tl_ts.set(null)
    } catch {
      case io: java.io.FileNotFoundException => //ok
    }
  }

  def trace = {
    getTS.trace
    this
  }

  def runAllTests(schema: Option[Node] = None) : Unit = {
    getTS.runAllTests(schema)
  }

  def setDebugger(db: AnyRef) = {
    getTS.setDebugger(db)
    debug
  }

  def setDebugging(flag: Boolean) = {
    getTS.setDebugging(flag)
  }

  def debug = {
    getTS.setDebugging(true)
  }

}
