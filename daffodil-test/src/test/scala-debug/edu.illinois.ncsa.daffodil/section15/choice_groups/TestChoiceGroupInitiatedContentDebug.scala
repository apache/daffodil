package edu.illinois.ncsa.daffodil.section15.choice_groups

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger

//class TestChoiceGroupInitiatedContentDebug extends JUnitSuite {
//
//  val testDir_01 = "/edu.illinois.ncsa.daffodil/section15/choice_groups/"
//  val tdml_01 = testDir_01 + "ChoiceGroupInitiatedContent.tdml"
//  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))
//
//  //  @Test def test_5() { runner_01.runOneTest("initiatedContentChoice5") }
//  //  @Test def test_6() { runner_01.runOneTest("initiatedContentChoice6") }
//  //  @Test def test_8() { runner_01.runOneTest("initiatedContentChoice8") }
//  //  @Test def test_9() { runner_01.runOneTest("initiatedContentChoice9") }
//  //  @Test def test_discriminatorNesting2() = Debugger.withDebugger { runner_01.runOneTest("discriminatorNesting2") }
//}
