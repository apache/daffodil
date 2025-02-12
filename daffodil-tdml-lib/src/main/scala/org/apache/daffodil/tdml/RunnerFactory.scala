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

import java.nio.file.Paths
import scala.collection.compat.immutable.ArraySeq

import org.apache.daffodil.lib.api.TDMLImplementation
import org.apache.daffodil.lib.api.URISchemaSource
import org.apache.daffodil.lib.util.Misc

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
  def apply(
    dir: String,
    file: String,
    tdmlImplementation: TDMLImplementation = TDMLImplementation.Daffodil,
    validateTDMLFile: Boolean = true,
    validateDFDLSchemas: Boolean = true,
    compileAllTopLevel: Boolean = false,
    defaultRoundTripDefault: RoundTrip = defaultRoundTripDefaultDefault,
    defaultValidationDefault: String = defaultValidationDefaultDefault,
    defaultImplementationsDefault: Seq[String] =
      ArraySeq.unsafeWrapArray(defaultImplementationsDefaultDefault)
  ): Runner = {

    // Prepend forward slash to turn dir/file into classpath resource
    val resourceDir =
      if (dir.startsWith("/"))
        dir
      else
        "/" + dir
    val resourcePath =
      if (resourceDir.endsWith("/"))
        resourceDir + file
      else
        resourceDir + "/" + file

    new Runner(
      Right(resourcePath),
      Some(tdmlImplementation),
      validateTDMLFile,
      validateDFDLSchemas,
      compileAllTopLevel,
      defaultRoundTripDefault,
      defaultValidationDefault,
      defaultImplementationsDefault
    )
  }

  // Scala 2 allows only one apply method to have default arguments,
  // so we must overload the next pair of apply methods both with and
  // without optTDMLImplementation

  def apply(path: String): Runner =
    new Runner(path)

  def apply(path: String, optTDMLImplementation: Option[TDMLImplementation]): Runner =
    // Don't turn path into classpath resource; we want to open a real file
    new Runner(Right(Paths.get(path).toUri.toString), optTDMLImplementation)

  // Scala 2 allows only one apply method to have default arguments,
  // so we must overload the next pair of apply methods (generally
  // used only for testing) both w/ and w/o validateTDMLFile

  def apply(elem: scala.xml.Elem): Runner =
    new Runner(elem)

  def apply(elem: scala.xml.Elem, validateTDMLFile: Boolean): Runner =
    new Runner(Left(elem), validateTDMLFile = validateTDMLFile)

  // Yes, that's a lot of defaults.....
  // but really it is 3-tiers deep:
  // roundTrip - on test case
  // defaultRoundTrip - on test suite
  // defaultRoundTripDefault - on runner aka test suite factory
  // defaultRoundTripDefaultDefault - on runner factory
  //
  def defaultRoundTripDefaultDefault: RoundTrip = NoRoundTrip
  def defaultValidationDefaultDefault = "off"

  /**
   * Default for what DFDL implementations to run tests against.
   *
   * A test or test suite can override this to specify more or different implementations
   * that the test should pass for.
   */
  def defaultImplementationsDefaultDefault = TDMLImplementation.values.map(_.toString)

  /**
   * By default we don't run Daffodil negative TDML tests against cross-testers.
   * The error messages are simply too varied.
   *
   * Negative tests must fail, but error messages aren't compared.
   */
  def defaultShouldDoErrorComparisonOnCrossTests = false

  /**
   * By default we don't cross test warning messages because they are too varied.
   */
  def defaultShouldDoWarningComparisonOnCrossTests = false

}

/**
 * A Runner is responsible for creating and running tests in a DFDLTestSuite. A
 * Runner will lazily create a DFDLTestSuite associated with the parameters
 * only when a test is run. Tests can be run in parallel, and synchronization
 * will be used to ensure only a single DFDLTestSuite is created and used.
 *
 * source is either a scala.xml.Elem, generally used for unit testing or a
 * String. If the string starts with a forward slash, it is treated is if it
 * were a resource on the classpath. Otherwise it is treated as if it were a
 * URI.
 */
final class Runner private (
  source: Either[scala.xml.Elem, String],
  optTDMLImplementation: Option[TDMLImplementation] = None,
  validateTDMLFile: Boolean = true,
  validateDFDLSchemas: Boolean = true,
  compileAllTopLevel: Boolean = false,
  defaultRoundTripDefault: RoundTrip = Runner.defaultRoundTripDefaultDefault,
  defaultValidationDefault: String = Runner.defaultValidationDefaultDefault,
  defaultImplementationsDefault: Seq[String] =
    ArraySeq.unsafeWrapArray(Runner.defaultImplementationsDefaultDefault),
  defaultIgnoreUnexpectedWarningsDefault: Boolean = true,
  defaultIgnoreUnexpectedValidationErrorsDefault: Boolean = true
) {

  /**
   * Create a runner for the path. This constructor requires this path be on
   * the classpath. A forward slash is prepended to ensure this path always
   * works as a classpath resource.
   */
  def this(path: String) =
    this(
      if (path.startsWith("/")) Right(path)
      else Right("/" + path)
    )

  /**
   * Create a runner for the dir + file combination. This constructor requires
   * this combined path be on the classpath. A forward slash is appended
   * between the dir and file if needed, and then we call the "path: String"
   * constructor which will prepend a forward slash if needed. This ensures
   * this dir + file path always works as a classpath resource.
   */
  def this(dir: String, file: String) =
    this(
      if (dir.endsWith("/")) dir + file
      else dir + "/" + file
    )

  /**
   * Create a runner for a URI.
   */
  def this(uri: java.net.URI) =
    this(Right(uri.toString))

  /**
   * Create a runner for a File
   */
  def this(file: java.io.File) =
    this(file.toURI)

  /**
   * Create a runner for a scala XML Elem. This is generally only used for testing
   */
  def this(elem: scala.xml.Elem) =
    this(Left(elem))

  private var ts: DFDLTestSuite = null

  // This Runner should only ever have a single DFDLTestSuite associated with
  // it. But different tests using this runner could be run in parallel. We
  // also do not want to create the runner until a test actually uses it to
  // avoid unnecessary allocations/computations. So all access of the
  // underlying test suite should occur through this function, which is
  // synchronized to ensure we only ever create one per Runner.
  def getTS = this.synchronized {
    if (ts == null) {
      val elemOrURISchemaSource: Any = source match {
        case Left(l) => l
        case Right(r) =>
          val uri = if (r.startsWith("/")) Misc.getRequiredResource(r) else new java.net.URI(r)
          URISchemaSource(new java.io.File(r), uri)
      }
      ts = new DFDLTestSuite(
        elemOrURISchemaSource,
        optTDMLImplementation,
        validateTDMLFile,
        validateDFDLSchemas,
        compileAllTopLevel,
        defaultRoundTripDefault,
        defaultValidationDefault,
        defaultImplementationsDefault,
        Runner.defaultShouldDoErrorComparisonOnCrossTests,
        Runner.defaultShouldDoWarningComparisonOnCrossTests,
        defaultIgnoreUnexpectedWarningsDefault,
        defaultIgnoreUnexpectedValidationErrorsDefault
      )
    }
    ts
  }

  def runOneTest(testName: String, leakCheck: Boolean = false) =
    try {
      getTS.runOneTest(testName, leakCheck)
    } finally {
      getTS.setDebugging(false)
    }

  def runOneTest(testName: String): Unit = {
    runOneTest(testName, false)
  }

  def runAllTests(): Unit = {
    getTS.runAllTests()
  }

  def testCases(): Seq[TestCase] = {
    getTS.testCases
  }

  /**
   *  Call this from an @AfterClass method
   *  to drop any state (like the test suite object) so we don't leak
   */
  def reset(): Unit = {
    // Note that we intentionally do not use getTS here for two reasons:
    //
    // 1) the DFDLTestSuite is lazily created only when a test is run--if no
    //    tests are ever run, then there is no need to create the
    //    DFDLTestSuite, which getTS will force.
    // 2) If creating the DFDLTestSuite leads to an exception, then ts will be
    //    null and calling getTS here will just cause that same exception to
    //    occur in the reset method, which can be confusing.
    if (ts != null) {
      ts.cleanUp()
    }
    ts = null
  }

  def trace = {
    getTS.trace
    this
  }

  def setDebugger(db: AnyRef) = {
    getTS.setDebugger(db)
    debug()
  }

  def debug() = {
    getTS.setDebugging(true)
  }

}
