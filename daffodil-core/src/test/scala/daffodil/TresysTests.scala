package daffodil

import java.io.File
import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import scala.xml._
import daffodil.compiler.Compiler
import tdml.DFDLTestSuite
import daffodil.util.LogLevel
import daffodil.util.LoggingDefaults
import daffodil.util.Misc
import org.junit.Test
import daffodil.debugger.Debugger

class TresysTests extends JUnitSuite {
  val testDir = "/test-suite/tresys-contributed/"

  val delimited = testDir + "dpaext1.tdml"
  lazy val runnerDelimited = new DFDLTestSuite(Misc.getRequiredResource(delimited))

  @Test def test_length_delimited_12_03_controversial() { runnerDelimited.runOneTest("length_delimited_12_03_controversial") }

  val td = testDir + "multiple-diagnostics.tdml"
  lazy val runnerMD = new DFDLTestSuite(Misc.getRequiredResource(td))
  runnerMD.setCheckAllTopLevel(true)

  @Test def test_multiple_diagnostics1() {
    runnerMD.runOneTest("twoMissingTypeDefErrors")
  }
  @Test def test_multiple_diagnostics2() { runnerMD.runOneTest("manyErrors1") }
  @Test def test_multiple_diagnostics3() { // LoggingDefaults.setLoggingLevel(LogLevel.Compile)
    runnerMD.runOneTest("manyErrors2")
  }

  val sv = testDir + "dfdl-schema-validation-diagnostics.tdml"
  lazy val runnerSV = new DFDLTestSuite(Misc.getRequiredResource(sv),
    validateTDMLFile = false)
  runnerSV.setCheckAllTopLevel(true) // check every top level construct. Not just the one under specific test.
  //
  // These must all be run without TDML validation because at least one part of the TDML file
  // contains DFDL schema validation errors, and TDML validation normally would check that
  //
  @Test def test_multiple_diagnostics4() { runnerSV.runOneTest("twoDFDLSchemaValidationErrors") }

  val nsd = testDir + "nested-separator-delimited.tdml"
  lazy val runnerNSD = new DFDLTestSuite(Misc.getRequiredResource(nsd))

  @Test def test_nested_separator_delimited_baseline() { runnerNSD.runOneTest("baseline") }
  @Test def test_nested_separator_delimited_basicNest() { runnerNSD.runOneTest("basicNest") }
  // Fails infinite loop
  // @Test def test_nested_separator_delimited_basicNest2() { runnerNSD.runOneTest("basicNest2")}

  // Fails, index out of bounds
  // @Test def test_nested_separator_delimited_nest1() { runnerNSD.runOneTest("nest1")}
  // Fails infinite loop
  // @Test def test_nested_separator_delimited_nest2() { runnerNSD.runOneTest("nest2")}    
  // Fails infinite loop
  // @Test def test_nested_separator_delimited_nest3() { runnerNSD.runOneTest("nest3")}

  /* Very big test data files, so each is in its own TDML file */

  //  val ab7 = testDir + "AB007.tdml"
  //  lazy val runnerAB7 = new DFDLTestSuite(Misc.getRequiredResource(ab7))
  //  @Test def test_AB007() { runnerAB7.runOneTest("AB007") }
  //  val ab8 = testDir + "AB008.tdml"
  //  lazy val runnerAB8 = new DFDLTestSuite(Misc.getRequiredResource(ab8))
  //  @Test def test_AB008() { runnerAB8.runOneTest("AB008") }
  //  val ab9 = testDir + "AB009.tdml"
  //  lazy val runnerAB9 = new DFDLTestSuite(Misc.getRequiredResource(ab9))
  //  @Test def test_AB009() { runnerAB9.runOneTest("AB009") }

  val st = testDir + "simple-type-bases.tdml"
  lazy val runnerST = new DFDLTestSuite(Misc.getRequiredResource(st))
  @Test def test_simpleTypeDerivedFromPrimitiveType() { runnerST.runOneTest("st-prim") }
  @Test def test_simpleTypeChainedDerivations() { runnerST.runOneTest("st-derived") }
  @Test def test_simpleTypeOverlapPrimError() { runnerST.runOneTest("st-prim-err1") }
  @Test def test_simpleTypeOverlapSimpleTypeError() { runnerST.runOneTest("st-st-err1") }

  val rd = testDir + "runtime-diagnostics.tdml"
  lazy val runnerRD = new DFDLTestSuite(Misc.getRequiredResource(rd),
    validateTDMLFile = false)
  runnerRD.setCheckAllTopLevel(true)

  @Test def test_runtime_diagnostics1() { runnerRD.runOneTest("PE1") }

  val sq = testDir + "sequence.tdml"
  lazy val runnerSQ = new DFDLTestSuite(Misc.getRequiredResource(sq))
  @Test def test_seq1() { runnerSQ.runOneTest("seq1") }

  lazy val runnerMB = new DFDLTestSuite(Misc.getRequiredResource(testDir + "mixed-binary-text.tdml"))

  @Test def test_t1() { runnerMB.runOneTest("t1") }
  @Test def test_t2() { runnerMB.runOneTest("t2") }
  @Test def test_t3() { runnerMB.runOneTest("t3") }

  @Test def test_codingErrorAction() { runnerMB.runOneTest("codingErrorAction") }

}
