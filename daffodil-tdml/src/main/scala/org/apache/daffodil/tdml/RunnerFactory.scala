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
  def apply(dir: String, file: String,
    validateTDMLFile: Boolean = true,
    validateDFDLSchemas: Boolean = true,
    compileAllTopLevel: Boolean = false,
    defaultRoundTripDefault: RoundTrip = defaultRoundTripDefaultDefault,
    defaultValidationDefault: String = defaultValidationDefaultDefault): Runner =
    new Runner(null, dir, file, validateTDMLFile, validateDFDLSchemas, compileAllTopLevel,
      defaultRoundTripDefault, defaultValidationDefault)

  def apply(elem: scala.xml.Elem): Runner =
    new Runner(elem, null, null)

  // Yes, that's a lot of defaults.....
  // but really it is 3-tiers deep:
  // roundTrip - on test case
  // defaultRoundTrip - on test suite
  // defaultRoundTripDefault - on runner aka test suite factory
  // defaultRoundTripDefaultDefault - on runner factory
  //
  def defaultRoundTripDefaultDefault: RoundTrip = NoRoundTrip
  def defaultValidationDefaultDefault = "off"
}

/**
 * Needs to be thread-safe (i.e., use all thread-local state) so that
 * test can be run in parallel.
 *
 * Note however, that each thread will get its own copy of the DFDLTestSuite
 */
class Runner private (elem: scala.xml.Elem, dir: String, file: String,
  validateTDMLFile: Boolean = true,
  validateDFDLSchemas: Boolean = true,
  compileAllTopLevel: Boolean = false,
  defaultRoundTripDefault: RoundTrip = Runner.defaultRoundTripDefaultDefault,
  defaultValidationDefault: String = Runner.defaultValidationDefaultDefault) {


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
        tl_ts.set(new DFDLTestSuite(resource, validateTDMLFile, validateDFDLSchemas, compileAllTopLevel,
          defaultRoundTripDefault, defaultValidationDefault))
      } else {
        tl_ts.set(new DFDLTestSuite(elem, validateTDMLFile, validateDFDLSchemas, compileAllTopLevel,
          defaultRoundTripDefault, defaultValidationDefault))
      }
    }
    ts
  }

  private object tl_ts extends ThreadLocal[DFDLTestSuite]

  private def ts = tl_ts.get

  def runOneTest(testName: String, schema: Option[scala.xml.Node] = None, leakCheck: Boolean = false) =
    getTS.runOneTest(testName, schema, leakCheck)

  /**
   *  Call this from an @AfterClass method
   *  to drop any state (like the test suite object) so we don't leak
   */
  def reset {
    try {
      //      if (file ne null)
      //        System.err.println("Reset runner for " + resource)
      //      else
      //        System.err.println("Resetting runner")
      tl_ts.set(null)
    } catch {
      case io: java.io.FileNotFoundException => //ok
    }
  }

  def trace = {
    getTS.trace
  }
}
