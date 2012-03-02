package daffodil

import java.io.File

import org.scalatest.junit.JUnit3Suite

import tdml.DFDLTestSuite

class TresysTests extends JUnit3Suite {
  val testDir = "./test-suite/tresys-contributed/"
  val tdml1 = testDir + "AA.tdml"
  val runner1 = new DFDLTestSuite(new File(tdml1))

  def test_AA000() { runner1.runOneTest("AA000") }
}