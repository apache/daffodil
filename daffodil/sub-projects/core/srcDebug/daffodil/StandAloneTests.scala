package daffodil

import scala.xml.Utility
import org.scalatest.junit.JUnit3Suite
import daffodil.debugger.DebugUtil
import daffodil.compiler.Compiler
import daffodil.xml.XMLUtils
import junit.framework.Assert._
import java.io.File
import java.io.FileNotFoundException
import daffodil.util.TestUtils

/**
 * TODO: this test rig should go away and all these tests should be rewritten into TDML.
 */
object TestRig {
    var isDebug = false
    def doTest(schemaFileName : String, rootName : String, inputFileName : String, expectedFileName : String) {
    val compiler = daffodil.compiler.Compiler()

    if (isDebug)
      compiler.setDebugging(true)
    compiler.setDistinguishedRootNode(rootName)
    val testDir = "test/"
    val schemaPath = testDir + schemaFileName
    val schemaFile = TestUtils.findFile(schemaPath)
    if (schemaFile == null || !schemaFile.exists) {
      throw new FileNotFoundException(schemaPath)
    }
    System.err.println("\nTest " + inputFileName)
    val parserFactory = DebugUtil.time("Compiling schema", compiler.compile(schemaFile.getAbsolutePath))
    if (parserFactory.canProceed) {
      val parser = parserFactory.onPath("/")
      if (parser.canProceed) {
        val data = Compiler.fileToReadableByteChannel(TestUtils.findFile(testDir + inputFileName))
        //println("|" + data + "|")
        val presult = parser.parse(data)
        val actual = Utility.trim(presult.result)
        val expectedXML = Utility.trim(scala.xml.XML.loadFile(TestUtils.findFile(testDir + expectedFileName)))
        val expectedNoAttrs = XMLUtils.removeAttributes(expectedXML)
        val actualNoAttrs = XMLUtils.removeAttributes(actual)
        assertEquals(expectedNoAttrs, actualNoAttrs) // Need to compare in a canonicalized manner.
      } else {

        val diags = parser.getDiagnostics.map{ _.toString }
        val msgs = "Parser cannot proceed." +: diags
        val msg = msgs.mkString("\n")
        isDebug = false
        println(msg)
        fail(msg)
      }
    } else {
        val diags = parserFactory.getDiagnostics.map{ _.toString }
        val msgs = "ParserFactory cannot proceed." +: diags
        val msg = msgs.mkString("\n")
        isDebug = false
        println(msg)
        fail(msg)
    }
  }
}

class PassingTests extends JUnit3Suite {
  val doTest = TestRig.doTest _
  // 
  def testAF000() { doTest("AF.dfdl.xsd", "allZones", "AF000.in", "AF000.xml") }
  def testAF001() { doTest("AF.dfdl.xsd", "allZones", "AF001.in", "AF001.xml") }
 
  def testAI000() { doTest("AI.dfdl.xsd", "list", "AI000.in", "AI000.xml") }
  def testAK000() { doTest("AK.dfdl.xsd", "list", "AK000.in", "AK000.xml") }
  def testAK001() { doTest("AK.dfdl.xsd", "list", "AK001.in", "AK001.xml") }
  def testAL000() { doTest("AL.dfdl.xsd", "list", "AL000.in", "AL000.xml") }
  def testAP000() { doTest("AP.dfdl.xsd", "parent", "AP000.in", "AP000.xml") }

  def testAW000() { doTest("AW.dfdl.xsd", "list", "AW000.in", "AW000.xml") }
  def testAW001() { doTest("AW.dfdl.xsd", "list", "AW001.in", "AW001.xml") }
  def testAX000() { doTest("AX.dfdl.xsd", "list", "AX000.in", "AX000.xml") }
  def testAZ000() { doTest("AZ.dfdl.xsd", "list", "AZ000.in", "AZ000.xml") }
  def testBA000() { doTest("BA.dfdl.xsd", "list", "BA000.in", "BA000.xml") }
  def testBC000() { doTest("BC.dfdl.xsd", "list", "BC000.in", "BC000.xml") }
  def testBD000() { doTest("BD.dfdl.xsd", "list", "BD000.in", "BD000.xml") }
  def testBE000() { doTest("BE.dfdl.xsd", "seq", "BE000.in", "BE000.xml") }
  def testBE001() { doTest("BE.dfdl.xsd", "seq", "BE001.in", "BE001.xml") }
 

}

class TestsThatFailComparison extends JUnit3Suite {
  val doTest = TestRig.doTest _
  // 
}

class TestsThatAbend extends JUnit3Suite {
  val doTest = TestRig.doTest _
  def testAB000() { doTest("AB.dfdl.xsd", "matrix", "AB000.in", "AB000.xml") }
  def testAB001() { doTest("AB.dfdl.xsd", "matrix", "AB001.in", "AB000.xml") }
  def testAB002() { doTest("AB.dfdl.xsd", "matrix", "AB002.in", "AB000.xml") }
  def testAB003() { doTest("AB.dfdl.xsd", "matrix", "AB003.in", "AB003.xml") }
  def testAB004() { doTest("AB.dfdl.xsd", "matrix", "AB004.in", "AB004.xml") }
  def testAB005() { doTest("AB.dfdl.xsd", "matrix", "AB005.in", "AB005.xml") }
  def testAB010() { doTest("AB.dfdl.xsd", "matrix", "AB010.in", "AB010.xml") }
  def testAC000() { doTest("AC.dfdl.xsd", "table", "AC000.in", "AC000.xml") }
  def testAD000() { doTest("AD.dfdl.xsd", "list", "AD000.in", "AD000.xml") }
  def testAE000() { doTest("AE.dfdl.xsd", "transposedMatrix", "AE000.in", "AE000.xml") } // non-standard multi-assignment
  def testAF002() { doTest("AF.dfdl.xsd", "allZones", "AF002.in", "AF002.xml") }
  def testAG000() { doTest("AG.dfdl.xsd", "allZones", "AG000.in", "AG000.xml") }
  def testAG001() { doTest("AG.dfdl.xsd", "allZones", "AG001.in", "AG001.xml") }
  def testAG002() { doTest("AG.dfdl.xsd", "allZones", "AG002.in", "AG002.xml") }
 // AH tests work in the old backend. The new front-end won't let them run
  def testAH000() { doTest("AH.dfdl.xsd", "allZones", "AH000.in", "AH000.xml") }
  def testAH001() { doTest("AH.dfdl.xsd", "allZones", "AH001.in", "AH001.xml") }
  def testAH002() { doTest("AH.dfdl.xsd", "allZones", "AH002.in", "AH002.xml") }
  
  def testAM000() { doTest("AM.dfdl.xsd", "mimeType", "AM000.in", "AM000.xml") }
  def testAM001() { doTest("AM.dfdl.xsd", "mimeType", "AM001.in", "AM001.xml") }
  def testAO000() { doTest("AO.dfdl.xsd", "element", "AO000.in", "AO000.xml") }
  def testAO001() { doTest("AO.dfdl.xsd", "element", "AO001.in", "AO001.xml") }
  def testAO002() { doTest("AO.dfdl.xsd", "element", "AO002.in", "AO002.xml") }
  def testAO003() { doTest("AO.dfdl.xsd", "element", "AO003.in", "AO003.xml") }
  def testAO004() { doTest("AO.dfdl.xsd", "element", "AO004.in", "AO004.xml") }

  def testAQ000() { doTest("AQ.dfdl.xsd", "ROOT", "AQ000.in", "AQ000.xml") }
  def testAR000() { doTest("AR.dfdl.xsd", "DFDL", "AR000.in", "AR000.xml") }
  def testAS000() { doTest("AS.dfdl.xsd", "table", "AS000.in", "AS000.xml") }
  def testAT000() { doTest("AT.dfdl.xsd", "PRP", "AT000.in", "AT000.xml") }
  def testAU000() { doTest("AU.dfdl.xsd", "list", "AU000.in", "AU000.xml") }
  def testAV000() { doTest("AV.dfdl.xsd", "wholeFile", "AV000.in", "AV000.xml") }
  def testAV001() { doTest("AV.dfdl.xsd", "wholeFile", "AV001.in", "AV001.xml") }
  def testAV002() { doTest("AV.dfdl.xsd", "wholeFile", "AV002.in", "AV002.xml") }
  def testAV003() { doTest("AV.dfdl.xsd", "wholeFile", "AV003.in", "AV003.xml") }

  def testBB000() { doTest("BB.dfdl.xsd", "list", "BB000.in", "BB000.xml") }
  // BF tests work in the old backend. New front-end breaks them.
  def testBF000() { doTest("BF.dfdl.xsd", "root", "BF000.in", "BF000.xml") }
  def testBF001() { doTest("BF.dfdl.xsd", "root", "BF001.in", "BF001.xml") }
  
  def testBG000() { doTest("BG.dfdl.xsd", "list", "BG000.in", "BG000.xml") }

}

//class StandAloneTests extends JUnit3Suite {
//  val doTest = TestRig.doTest _
//
//  //  def testStandAloneTests() {
//  //    val allLines = Source.fromFile("test/testSuite.txt").getLines.toList
//  //    val testLines = allLines.filter { !_.startsWith("#") } //strip comments
//  //    for (line <- testLines) {
//  //      val p = line.split(",")
//  //      val testDir = "test/"
//  //      val (schemaFileName, root, inputFileName, expectedFileName) = (testDir+p(0), p(1), testDir+p(2), testDir+p(3))
//  //      val schemaParser = new SchemaParser
//  //      if (isDebug)
//  //        schemaParser setDebugging (true)
//  //      System.err.println("Test "+ schemaFileName)
//  //      DebugUtil.time("Parsing schema", schemaParser parse (schemaFileName))
//  //      val result = schemaParser eval (inputFileName, root)
//  //      val res = trim(XMLUtils.element2Elem(result))
//  //
//  //      DebugUtil log ("Total nodes:" + XMLUtils.getTotalNodes)
//  //      val expectedXML = trim(scala.xml.XML.loadFile(expectedFileName))
//  //      assertEquals(expectedXML, res) // Need to compare in a canonicalized manner.
//  //
//  //    }
//
//  def testAA000() { doTest("AA.dfdl.xsd", "list", "AA000.in", "AA000.xml") }
//  def testAB000() { doTest("AB.dfdl.xsd", "matrix", "AB000.in", "AB000.xml") }
//  def testAB001() { doTest("AB.dfdl.xsd", "matrix", "AB001.in", "AB000.xml") }
//  def testAB002() { doTest("AB.dfdl.xsd", "matrix", "AB002.in", "AB000.xml") }
//  def testAB003() { doTest("AB.dfdl.xsd", "matrix", "AB003.in", "AB003.xml") }
//  def testAB004() { doTest("AB.dfdl.xsd", "matrix", "AB004.in", "AB004.xml") }
//  def testAB005() { doTest("AB.dfdl.xsd", "matrix", "AB005.in", "AB005.xml") }
//  def testAB010() { doTest("AB.dfdl.xsd", "matrix", "AB010.in", "AB010.xml") }
//  def testAC000() { doTest("AC.dfdl.xsd", "table", "AC000.in", "AC000.xml") }
//  def testAD000() { doTest("AD.dfdl.xsd", "list", "AD000.in", "AD000.xml") }
//  def testAE000() { doTest("AE.dfdl.xsd", "transposedMatrix", "AE000.in", "AE000.xml") } // non-standard multi-assignment
//  def testAF000() { doTest("AF.dfdl.xsd", "allZones", "AF000.in", "AF000.xml") }
//  def testAF001() { doTest("AF.dfdl.xsd", "allZones", "AF001.in", "AF001.xml") }
//  def testAF002() { doTest("AF.dfdl.xsd", "allZones", "AF002.in", "AF002.xml") }
//  def testAG000() { doTest("AG.dfdl.xsd", "allZones", "AG000.in", "AG000.xml") }
//  def testAG001() { doTest("AG.dfdl.xsd", "allZones", "AG001.in", "AG001.xml") }
//  def testAG002() { doTest("AG.dfdl.xsd", "allZones", "AG002.in", "AG002.xml") }
//  def testAH000() { doTest("AH.dfdl.xsd", "allZones", "AH000.in", "AH000.xml") }
//  def testAH001() { doTest("AH.dfdl.xsd", "allZones", "AH001.in", "AH001.xml") }
//  def testAH002() { doTest("AH.dfdl.xsd", "allZones", "AH002.in", "AH002.xml") }
//  def testAI000() { doTest("AI.dfdl.xsd", "list", "AI000.in", "AI000.xml") }
//  def testAJ000() { doTest("AJ.dfdl.xsd", "list", "AJ000.in", "AJ000.xml") }
//  def testAJ001() { doTest("AJ.dfdl.xsd", "list", "AJ001.in", "AJ001.xml") }
//  def testAK000() { doTest("AK.dfdl.xsd", "list", "AK000.in", "AK000.xml") }
//  def testAK001() { doTest("AK.dfdl.xsd", "list", "AK001.in", "AK001.xml") }
//  def testAL000() { doTest("AL.dfdl.xsd", "list", "AL000.in", "AL000.xml") }
//  def testAM000() { doTest("AM.dfdl.xsd", "mimeType", "AM000.in", "AM000.xml") }
//  def testAM001() { doTest("AM.dfdl.xsd", "mimeType", "AM001.in", "AM001.xml") }
//  def testAN000() { doTest("AN.dfdl.xsd", "path", "AN000.in", "AN000.xml") }
//  def testAN001() { doTest("AN.dfdl.xsd", "path", "AN001.in", "AN001.xml") }
//  def testAO000() { doTest("AO.dfdl.xsd", "element", "AO000.in", "AO000.xml") }
//  def testAO001() { doTest("AO.dfdl.xsd", "element", "AO001.in", "AO001.xml") }
//  def testAO002() { doTest("AO.dfdl.xsd", "element", "AO002.in", "AO002.xml") }
//  def testAO003() { doTest("AO.dfdl.xsd", "element", "AO003.in", "AO003.xml") }
//  def testAO004() { doTest("AO.dfdl.xsd", "element", "AO004.in", "AO004.xml") }
//  def testAP000() { doTest("AP.dfdl.xsd", "parent", "AP000.in", "AP000.xml") }
//  def testAQ000() { doTest("AQ.dfdl.xsd", "ROOT", "AQ000.in", "AQ000.xml") }
//  def testAR000() { doTest("AR.dfdl.xsd", "DFDL", "AR000.in", "AR000.xml") }
//  def testAS000() { doTest("AS.dfdl.xsd", "table", "AS000.in", "AS000.xml") }
//  def testAT000() { doTest("AT.dfdl.xsd", "PRP", "AT000.in", "AT000.xml") }
//  def testAU000() { doTest("AU.dfdl.xsd", "list", "AU000.in", "AU000.xml") }
//  def testAV000() { doTest("AV.dfdl.xsd", "wholeFile", "AV000.in", "AV000.xml") }
//  def testAV001() { doTest("AV.dfdl.xsd", "wholeFile", "AV001.in", "AV001.xml") }
//  def testAV002() { doTest("AV.dfdl.xsd", "wholeFile", "AV002.in", "AV002.xml") }
//  def testAV003() { doTest("AV.dfdl.xsd", "wholeFile", "AV003.in", "AV003.xml") }
//  def testAW000() { doTest("AW.dfdl.xsd", "list", "AW000.in", "AW000.xml") }
//  def testAW001() { doTest("AW.dfdl.xsd", "list", "AW001.in", "AW001.xml") }
//  def testAX000() { doTest("AX.dfdl.xsd", "list", "AX000.in", "AX000.xml") }
//  def testAZ000() { doTest("AZ.dfdl.xsd", "list", "AZ000.in", "AZ000.xml") }
//  def testBA000() { doTest("BA.dfdl.xsd", "list", "BA000.in", "BA000.xml") }
//  def testBB000() { doTest("BB.dfdl.xsd", "list", "BB000.in", "BB000.xml") }
//  def testBC000() { doTest("BC.dfdl.xsd", "list", "BC000.in", "BC000.xml") }
//  def testBD000() { doTest("BD.dfdl.xsd", "list", "BD000.in", "BD000.xml") }
//  def testBE000() { doTest("BE.dfdl.xsd", "seq", "BE000.in", "BE000.xml") }
//  def testBE001() { doTest("BE.dfdl.xsd", "seq", "BE001.in", "BE001.xml") }
//  def testBF000() { doTest("BF.dfdl.xsd", "root", "BF000.in", "BF000.xml") }
//  def testBF001() { doTest("BF.dfdl.xsd", "root", "BF001.in", "BF001.xml") }
//  def testBG000() { doTest("BG.dfdl.xsd", "list", "BG000.in", "BG000.xml") }
//
//}
