package daffodil

import java.io.File
import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import scala.xml._
import daffodil.compiler.Compiler
import tdml.DFDLTestSuite
import daffodil.util.LogLevel
import daffodil.util.LoggingDefaults
import daffodil.util.Misc

class TresysTests extends JUnit3Suite {
  val testDir = "/test-suite/tresys-contributed/"
  val aa = testDir + "AA.tdml"

  def runnerAA = {
    new DFDLTestSuite(Misc.getRequiredResource(aa))
  }

  // Needs InputValueCalc to work before this has a chance.
  def test_AA000() { runnerAA.runOneTest("AA000") }
  def test_inputValueCalcErrorDiagnostic1() { runnerAA.runOneTest("inputValueCalcErrorDiagnostic1")}
  def test_inputValueCalcErrorDiagnostic2() { runnerAA.runOneTest("")}
  def test_inputValueCalcAbsolutePath() { runnerAA.runOneTest("inputValueCalcAbsolutePath")}
  
  val delimited = testDir + "dpaext1.tdml"
  lazy val runnerDelimited = new DFDLTestSuite(Misc.getRequiredResource(delimited))
  
  def test_length_delimited_12_03_controversial() { runnerDelimited.runOneTest("length_delimited_12_03_controversial") }

  val td = testDir + "multiple-diagnostics.tdml"
  lazy val runnerMD = new DFDLTestSuite(Misc.getRequiredResource(td))
  runnerMD.setCheckAllTopLevel(true)

  def test_multiple_diagnostics1() {
    runnerMD.runOneTest("twoMissingTypeDefErrors")}
  def test_multiple_diagnostics2() { runnerMD.runOneTest("manyErrors1")}
  def test_multiple_diagnostics3() {  // LoggingDefaults.setLoggingLevel(LogLevel.Compile)
    runnerMD.runOneTest("manyErrors2")}

  val sv = testDir + "dfdl-schema-validation-diagnostics.tdml"
  lazy val runnerSV = new DFDLTestSuite(Misc.getRequiredResource(sv))
  runnerSV.setCheckAllTopLevel(true)// check every top level construct. Not just the one under specific test.
    //
  // These must all be run without TDML validation because at least one part of the TDML file
  // contains DFDL schema validation errors, and TDML validation normally would check that
  //
  def test_multiple_diagnostics4() { runnerSV.runOneTestNoTDMLValidation("twoDFDLSchemaValidationErrors")}

  val nsd = testDir + "nested-separator-delimited.tdml"
  lazy val runnerNSD = new DFDLTestSuite(Misc.getRequiredResource(nsd))
  
  def test_nested_separator_delimited_baseline() { runnerNSD.runOneTest("baseline")}
  def test_nested_separator_delimited_basicNest() { runnerNSD.runOneTest("basicNest")}
  // Fails infinite loop
  // def test_nested_separator_delimited_basicNest2() { runnerNSD.runOneTest("basicNest2")}
  
  // Fails, index out of bounds
  // def test_nested_separator_delimited_nest1() { runnerNSD.runOneTest("nest1")}
  // Fails infinite loop
  // def test_nested_separator_delimited_nest2() { runnerNSD.runOneTest("nest2")}    
  // Fails infinite loop
  // def test_nested_separator_delimited_nest3() { runnerNSD.runOneTest("nest3")}

  val escapeScheme = testDir + "escapeScheme.tdml"
  lazy val runnerEscapeScheme = new DFDLTestSuite(Misc.getRequiredResource(escapeScheme))
  
  def test_escape_scheme() { runnerEscapeScheme.runOneTest("escapeSchemeSimple") }

  /* Very big test data files, so each is in its own TDML file */
//  val ab6 = testDir + "AB006.tdml"
//  lazy val runnerAB6 = new DFDLTestSuite(Misc.getRequiredResource(ab6))
//  def test_AB006() { runnerAB6.runOneTest("AB006") }
//  val ab7 = testDir + "AB007.tdml"
//  lazy val runnerAB7 = new DFDLTestSuite(Misc.getRequiredResource(ab7))
//  def test_AB007() { runnerAB7.runOneTest("AB007") }
//  val ab8 = testDir + "AB008.tdml"
//  lazy val runnerAB8 = new DFDLTestSuite(Misc.getRequiredResource(ab8))
//  def test_AB008() { runnerAB8.runOneTest("AB008") }
//  val ab9 = testDir + "AB009.tdml"
//  lazy val runnerAB9 = new DFDLTestSuite(Misc.getRequiredResource(ab9))
//  def test_AB009() { runnerAB9.runOneTest("AB009") }
  
  val aj = testDir + "AJ.tdml"
  lazy val runnerAJ = new DFDLTestSuite(Misc.getRequiredResource(aj))
  
  def test_AJ000() { runnerAJ.runOneTest("AJ000") }
  def test_AJ001() { runnerAJ.runOneTest("AJ001") }
  
  val st = testDir + "simple-type-bases.tdml"
  lazy val runnerST = new DFDLTestSuite(Misc.getRequiredResource(st))
  def test_simpleTypeDerivedFromPrimitiveType() { runnerST.runOneTest("st-prim")}
  def test_simpleTypeChainedDerivations() { runnerST.runOneTest("st-derived")}
  def test_simpleTypeOverlapPrimError() { runnerST.runOneTest("st-prim-err1")}
  def test_simpleTypeOverlapSimpleTypeError() { runnerST.runOneTest("st-st-err1")}
  
  val ch = testDir + "choice.tdml"
  lazy val runnerCH= new DFDLTestSuite(Misc.getRequiredResource(ch))
  def test_basicChoice() { runnerCH.runOneTest("basic")}
  def test_choice2() { runnerCH.runOneTest("choice2")}
  def test_choice3() { runnerCH.runOneTest("choice3")}
  def test_choice4() { runnerCH.runOneTest("choice4") }
  
  def test_choice5() { runnerCH.runOneTest("choice5")}
  def test_choice6() { runnerCH.runOneTest("choice6")}
  def test_choiceFail1() { runnerCH.runOneTest("choiceFail1")}
  def test_choiceDelim1() { 
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    runnerCH.runOneTest("choiceDelim1")
    }
  def test_nestedChoice1() { 
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    runnerCH.runOneTest("nestedChoice1")
    }
  
  val rd = testDir + "runtime-diagnostics.tdml"
  lazy val runnerRD = new DFDLTestSuite(Misc.getRequiredResource(rd))
  runnerRD.setCheckAllTopLevel(true)

  def test_runtime_diagnostics1() { runnerRD.runOneTest("PE1")}
  
  val sq = testDir + "sequence.tdml"
  lazy val runnerSQ = new DFDLTestSuite(Misc.getRequiredResource(sq))
  def test_seq1() { runnerSQ.runOneTest("seq1")}
  
  val ln = testDir + "literal-value-nils.tdml"
  lazy val runnerLN = new DFDLTestSuite(Misc.getRequiredResource(ln))
  def test_text_01() { runnerLN.runOneTest("text_01")}
  def test_text_02() { runnerLN.runOneTest("text_02")}
  def test_text_03() { runnerLN.runOneTest("text_03")}
  def test_text_04() { runnerLN.runOneTest("text_04")}
  def test_text_05() { runnerLN.runOneTest("text_05")}
  def test_text_06() { runnerLN.runOneTest("text_06")}
  def test_binary_01() { runnerLN.runOneTest("binary_01")}
  
  val entity = testDir + "entities.tdml"
  lazy val runnerEntity = new DFDLTestSuite(Misc.getRequiredResource(entity))
  def test_entity_fail_01() { // LoggingDefaults.setLoggingLevel(LogLevel.Compile)
    runnerEntity.runOneTest("entity_fail_01") }
  def test_entity_fail_02() { runnerEntity.runOneTest("entity_fail_02") }
  def test_entity_fail_03() { runnerEntity.runOneTest("entity_fail_03") }
  def test_entity_fail_04() { runnerEntity.runOneTest("entity_fail_04") }
  def test_entity_fail_05() { runnerEntity.runOneTest("entity_fail_05") }
  def test_entity_fail_06() { runnerEntity.runOneTest("entity_fail_06") }
  
  val ai = testDir + "AI.tdml"
  lazy val runnerAI = new DFDLTestSuite(Misc.getRequiredResource(ai))

  def test_AI000() { runnerAI.runOneTest("AI000") }
  
  val aq = testDir + "AQ.tdml"
  lazy val runnerAQ = new DFDLTestSuite(Misc.getRequiredResource(aq))

  def test_AQ000() { runnerAQ.runOneTest("AQ000") }

  
  lazy val runnerEX = new DFDLTestSuite(Misc.getRequiredResource(testDir + "expressions.tdml"))

  def test_expressions_lke1() { runnerEX.runOneTest("lke1") }
  def test_expressions_ocke1() { runnerEX.runOneTest("ocke1") }
    
  val ar = testDir + "AR.tdml"
  lazy val runnerAR = new DFDLTestSuite(Misc.getRequiredResource(ar))

  def test_AR000() { runnerAR.runOneTest("AR000") }
}
