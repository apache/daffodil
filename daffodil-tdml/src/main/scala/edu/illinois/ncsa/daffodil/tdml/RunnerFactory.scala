/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.tdml

import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.exceptions.Assert

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
    defaultRoundTripDefault: Boolean = defaultRoundTripDefaultDefault,
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
  def defaultRoundTripDefaultDefault = false
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
  defaultRoundTripDefault: Boolean = Runner.defaultRoundTripDefaultDefault,
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
      if (file ne null)
        System.err.println("Reset runner for " + resource)
      else
        System.err.println("Resetting runner")
      tl_ts.set(null)
    } catch {
      case io: java.io.FileNotFoundException => //ok
    }
  }

  def trace = {
    getTS.trace
  }
}
