package daffodil

import java.io.File

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import scala.xml._
import daffodil.dsom.Compiler

import tdml.DFDLTestSuite

class TresysTests extends JUnit3Suite {
  val testDir = "test-suite/tresys-contributed/"
  val aa = testDir + "AA.tdml"
  val runnerAA = new DFDLTestSuite(new File(aa))

  def test_AA000() { runnerAA.runOneTest("AA000") }
  
  val ab = testDir + "AB.tdml"
  val runnerAB = new DFDLTestSuite(new File(ab))
    
  def test_AB000() { runnerAB.runOneTest("AB000") }
  def test_AB001() { runnerAB.runOneTest("AB001") }
  def test_AB002() { runnerAB.runOneTest("AB002") }
  def test_AB003() { runnerAB.runOneTest("AB003") }
//  def test_AB004() { runnerAB.runOneTest("AB004") }
//  def test_AB005() { runnerAB.runOneTest("AB005") }
  
  val an = testDir + "AN.tdml"
  val runnerAN = new DFDLTestSuite(new File(an))
  
  def test_AN000() { runnerAN.runOneTest("AN000") }
  def test_AN001() { runnerAN.runOneTest("AN001") }
  
  val delimited = testDir + "dpaext1.tdml"
  val runnerDelimited = new DFDLTestSuite(new File(delimited))
  
  def test_length_delimited_12_02() { runnerDelimited.runOneTest("length_delimited_12_02") }
  def test_length_delimited_12_03_controversial() { runnerDelimited.runOneTest("length_delimited_12_03_controversial") }
  def test_length_delimited_12_03() { runnerDelimited.runOneTest("length_delimited_12_03") }
//  def test_length_delimited_12_05() { runnerDelimited.runOneTest("length_delimited_12_05") }
//  def test_length_delimited_12_06() { runnerDelimited.runOneTest("length_delimited_12_06") }
  def test_multiple_delimiters() { runnerDelimited.runOneTest("multiple_delimiters") }
  def test_multiple_delimiters2() { runnerDelimited.runOneTest("multiple_delimiters2") }

  val td = testDir + "multiple-diagnostics.tdml"
  val runnerMD = new DFDLTestSuite(new File(td))
  runnerMD.setCheckEverything(true)

  def test_multiple_diagnostics1() { runnerMD.runOneTest("twoMissingTypeDefErrors")}
  def test_multiple_diagnostics2() { runnerMD.runOneTest("manyErrors1")}
  def test_multiple_diagnostics3() { runnerMD.runOneTest("manyErrors2")}

  val sv = testDir + "dfdl-schema-validation-diagnostics.tdml"
  val runnerSV = new DFDLTestSuite(new File(sv))
  runnerSV.setCheckEverything(true)// check every top level construct. Not just the one under specific test.
    //
  // These must all be run without TDML validation because at least one part of the TDML file
  // contains DFDL schema validation errors, and TDML validation normally would check that
  //
  def test_multiple_diagnostics4() { runnerSV.runOneTestNoTDMLValidation("twoDFDLSchemaValidationErrors")}

  val nsd = testDir + "nested-separator-delimited.tdml"
  val runnerNSD = new DFDLTestSuite(new File(nsd))
  
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
  val runnerEscapeScheme = new DFDLTestSuite(new File(escapeScheme))
  
  def test_escape_scheme() { runnerEscapeScheme.runOneTest("escapeSchemeSimple") }

  /* Very big test data files, so each is in its own TDML file */
//  val ab6 = testDir + "AB006.tdml"
//  val runnerAB6 = new DFDLTestSuite(new File(ab6))
//  def test_AB006() { runnerAB6.runOneTest("AB006") }
//  val ab7 = testDir + "AB007.tdml"
//  val runnerAB7 = new DFDLTestSuite(new File(ab7))
//  def test_AB007() { runnerAB7.runOneTest("AB007") }
//  val ab8 = testDir + "AB008.tdml"
//  val runnerAB8 = new DFDLTestSuite(new File(ab8))
//  def test_AB008() { runnerAB8.runOneTest("AB008") }
//  val ab9 = testDir + "AB009.tdml"
//  val runnerAB9 = new DFDLTestSuite(new File(ab9))
//  def test_AB009() { runnerAB9.runOneTest("AB009") }
  
  val aj = testDir + "AJ.tdml"
  val runnerAJ = new DFDLTestSuite(new File(aj))
    
  def test_AJ000() { runnerAJ.runOneTest("AJ000") }
  def test_AJ001() { runnerAJ.runOneTest("AJ001") }
  
  val st = testDir + "simple-type-bases.tdml"
  val runnerST = new DFDLTestSuite(new File(st))
  def test_simpleTypeDerivedFromPrimitiveType() { runnerST.runOneTest("st-prim")}
  def test_simpleTypeChainedDerivations() { runnerST.runOneTest("st-derived")}
  def test_simpleTypeOverlapPrimError() { runnerST.runOneTest("st-prim-err1")}
  def test_simpleTypeOverlapSimpleTypeError() { runnerST.runOneTest("st-st-err1")}
  
}