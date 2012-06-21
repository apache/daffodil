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
  def test_AB004() { runnerAB.runOneTest("AB004") }
  def test_AB005() { runnerAB.runOneTest("AB005") }
  
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
  
}