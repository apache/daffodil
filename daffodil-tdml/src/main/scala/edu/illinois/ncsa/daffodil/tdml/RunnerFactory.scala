package edu.illinois.ncsa.daffodil.tdml

import edu.illinois.ncsa.daffodil.util.Misc

object Runner {
  def apply(dir: String, file: String): Runner = new Runner(dir, file)
}

class Runner private (dir: String, file: String) {

  private lazy val ts = {
    // This is ok to be a hard-wired "/" because these are resource identifiers, which
    // are not file-system paths that have to be made platform-specific. 
    // In other words, we don't need to use "\\" for windows here. "/" works there as well.
    val d = if (dir.endsWith("/")) dir else dir + "/"
    new DFDLTestSuite(Misc.getRequiredResource(d + file))
  }

  def runOneTest(testName: String, schema: Option[scala.xml.Node] = None, leakCheck: Boolean = false) =
    ts.runOneTest(testName, schema, leakCheck)
}