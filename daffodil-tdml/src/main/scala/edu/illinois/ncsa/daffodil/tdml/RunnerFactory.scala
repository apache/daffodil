package edu.illinois.ncsa.daffodil.tdml

import edu.illinois.ncsa.daffodil.util.Misc

/**
 * Creates the DFDLTestSuite object lazily, so the file isn't read into memory
 * and parsed unless you actually try to run a test using it.
 *
 * Creates the DFDLTestSuite only once.
 *
 * Provides a reset method to be called from @AfterClass to drop
 * the test suite object (and avoid memory leak).
 *
 * defaultRoundTripDefault if true the round trip default for the test suite will be
 * this value, if the test suite does not specify defaultRoundTrip attribute.
 */
object Runner {
  def apply(dir: String, file: String,
    validateTDMLFile: Boolean = true,
    validateDFDLSchemas: Boolean = true,
    compileAllTopLevel: Boolean = false,
    defaultRoundTripDefault: Boolean = defaultRoundTripDefaultDefault): Runner = new Runner(dir, file, validateTDMLFile, validateDFDLSchemas, compileAllTopLevel,
    defaultRoundTripDefault)

  // Yes, that's a lot of defaults.....
  // but really it is 3-tiers deep:
  // roundTrip - on test case
  // defaultRoundTrip - on test suite
  // defaultRoundTripDefault - on runner aka test suite factory
  // defaultRoundTripDefaultDefault - on runner factory
  //
  def defaultRoundTripDefaultDefault = false
}

/**
 * Needs to be thread-safe (i.e., use all thread-local state) so that
 * test can be run in parallel.
 *
 * Note however, that each thread will get its own copy of the DFDLTestSuite
 */
class Runner private (dir: String, file: String,
  validateTDMLFile: Boolean,
  validateDFDLSchemas: Boolean,
  compileAllTopLevel: Boolean,
  defaultRoundTripDefault: Boolean) {

  private def getTS = {
    if (ts == null) {
      // This is ok to be a hard-wired "/" because these are resource identifiers, which
      // are not file-system paths that have to be made platform-specific.
      // In other words, we don't need to use "\\" for windows here. "/" works there as well.
      val d = if (dir.endsWith("/")) dir else dir + "/"
      tl_ts.set(new DFDLTestSuite(Misc.getRequiredResource(d + file), validateTDMLFile, validateDFDLSchemas, compileAllTopLevel,
        defaultRoundTripDefault))
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
    tl_ts.set(null)
  }

  def trace = {
    getTS.trace
  }
}
