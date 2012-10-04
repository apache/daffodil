package daffodil.section05.simple_types

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestSimpleTypes extends JUnitSuite {
  val testDir = "/daffodil/section05/simple_types/"
  val aa = testDir + "SimpleTypes.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  @Test def test_Long1() { runner.runOneTest("Long1") }
  @Test def test_BigInteger1() { runner.runOneTest("BigInteger1") }
  @Test def test_Integer01() { runner.runOneTest("Integer01") }
  @Test def test_Int01() { runner.runOneTest("Int01") }
  @Test def test_int_error() { runner.runOneTest("int_error") }
 
  // Test warning_exercise moved to scala-debug until warnings are implemented.
  
  @Test def test_UnsignedNumbers1() { runner.runOneTest("UnsignedNumbers1") }
  @Test def test_unsignedLong_01() {runner.runOneTest("unsignedLong_01")}
  @Test def test_unsignedLong_02() {runner.runOneTest("unsignedLong_02")}
  @Test def test_unsignedLong_03() {runner.runOneTest("unsignedLong_03")}
  @Test def test_Long2() {runner.runOneTest("Long2")}
  @Test def test_Long3() {runner.runOneTest("Long3")}
  @Test def test_Long4() {runner.runOneTest("Long4")}
  @Test def test_int_error_02() { runner.runOneTest("int_error_02") }
  @Test def test_int_error_03() { runner.runOneTest("int_error_03") }
  @Test def test_short_01() { runner.runOneTest("short_01") }
  @Test def test_short_02() { runner.runOneTest("short_02") }
  @Test def test_unsignedInt_01() { runner.runOneTest("unsignedInt_01") }
  @Test def test_unsignedInt_02() { runner.runOneTest("unsignedInt_02") }
  
  @Test def test_characterDuringValidInt() { runner.runOneTest("characterDuringValidInt") }
  @Test def test_whiteSpaceAfterLengthExceededInt() { runner.runOneTest("whiteSpaceAfterLengthExceededInt") }
  @Test def test_whiteSpaceBeforeLengthExceededInt() { runner.runOneTest("whiteSpaceBeforeLengthExceededInt") }
  @Test def test_whiteSpaceDuringLengthExceededInt() { runner.runOneTest("whiteSpaceDuringLengthExceededInt") }
  @Test def test_whiteSpaceAfterValidInt() { runner.runOneTest("whiteSpaceAfterValidInt") }    
  
  @Test def test_characterDuringValidInteger() { runner.runOneTest("characterDuringValidInteger") }
  @Test def test_whiteSpaceAfterLengthExceededInteger() { runner.runOneTest("whiteSpaceAfterLengthExceededInteger") }
  @Test def test_whiteSpaceBeforeLengthExceededInteger() { runner.runOneTest("whiteSpaceBeforeLengthExceededInteger") }
  @Test def test_whiteSpaceDuringLengthExceededInteger() { runner.runOneTest("whiteSpaceDuringLengthExceededInteger") }
  @Test def test_whiteSpaceBeforeValidInteger() { runner.runOneTest("whiteSpaceBeforeValidInteger") }
  <!-- TODO: Find out why these test generate unexpected errors -->
  @Test def test_whiteSpaceDuringValidInteger() { runner.runOneTest("whiteSpaceDuringValidInteger") }
  @Test def test_whiteSpaceAfterValidInteger() { runner.runOneTest("whiteSpaceAfterValidInteger") }  
  
  @Test def test_characterDuringValidLong() { runner.runOneTest("characterDuringValidLong") }
  @Test def test_whiteSpaceAfterLengthExceededLong() { runner.runOneTest("whiteSpaceAfterLengthExceededLong") }
  @Test def test_whiteSpaceBeforeLengthExceededLong() { runner.runOneTest("whiteSpaceBeforeLengthExceededLong") }
  @Test def test_whiteSpaceDuringLengthExceededLong() { runner.runOneTest("whiteSpaceDuringLengthExceededLong") }
  @Test def test_whiteSpaceAfterValidLong() { runner.runOneTest("whiteSpaceAfterValidLong") }
  
  @Test def test_characterDuringValidShort() { runner.runOneTest("characterDuringValidShort") }
  @Test def test_whiteSpaceAfterLengthExceededShort() { runner.runOneTest("whiteSpaceAfterLengthExceededShort") }
  @Test def test_whiteSpaceBeforeLengthExceededShort() { runner.runOneTest("whiteSpaceBeforeLengthExceededShort") }
  @Test def test_whiteSpaceDuringLengthExceededShort() { runner.runOneTest("whiteSpaceDuringLengthExceededShort") }
  @Test def test_whiteSpaceAfterValidShort() { runner.runOneTest("whiteSpaceAfterValidShort") }
  
  @Test def test_characterDuringValidByte() { runner.runOneTest("characterDuringValidByte") }
  @Test def test_whiteSpaceAfterLengthExceededByte() { runner.runOneTest("whiteSpaceAfterLengthExceededByte") }
  @Test def test_whiteSpaceBeforeLengthExceededByte() { runner.runOneTest("whiteSpaceBeforeLengthExceededByte") }
  @Test def test_whiteSpaceDuringLengthExceededByte() { runner.runOneTest("whiteSpaceDuringLengthExceededByte") }
  @Test def test_whiteSpaceBeforeValidByte() { runner.runOneTest("whiteSpaceBeforeValidByte") }
  <!-- TODO: Find out why these test generate unexpected errors -->
  @Test def test_whiteSpaceDuringValidByte() { runner.runOneTest("whiteSpaceDuringValidByte") }
  @Test def test_whiteSpaceAfterValidByte() { runner.runOneTest("whiteSpaceAfterValidByte") }  
  
  @Test def test_characterDuringValidUnsignedInt() { runner.runOneTest("characterDuringValidUnsignedInt") } 
  @Test def test_negativeUnsignedInt() { runner.runOneTest("negativeUnsignedInt") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedInt() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedInt") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedInt() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedInt") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedInt() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedInt") }
  @Test def test_whiteSpaceAfterValidUnsignedInt() { runner.runOneTest("whiteSpaceAfterValidUnsignedInt") }
  
  @Test def test_characterDuringValidUnsignedByte() { runner.runOneTest("characterDuringValidUnsignedByte") }  
  @Test def test_negativeUnsignedByte() { runner.runOneTest("negativeUnsignedByte") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedByte() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedByte") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedByte() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedByte") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedByte() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedByte") }
  @Test def test_whiteSpaceAfterValidUnsignedByte() { runner.runOneTest("whiteSpaceAfterValidUnsignedByte") }
  
  @Test def test_characterDuringValidUnsignedLong() { runner.runOneTest("characterDuringValidUnsignedLong") }  
  @Test def test_negativeUnsignedLong() { runner.runOneTest("negativeUnsignedLong") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedLong() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedLong") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedLong() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedLong") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedLong() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedLong") }
  @Test def test_whiteSpaceBeforeValidUnsignedLong() { runner.runOneTest("whiteSpaceBeforeValidUnsignedLong") }
<!-- TODO: Find out why these test generate unexpected errors -->
  @Test def test_whiteSpaceDuringValidUnsignedLong() { runner.runOneTest("whiteSpaceDuringValidUnsignedLong") }
  @Test def test_whiteSpaceAfterValidUnsignedLong() { runner.runOneTest("whiteSpaceAfterValidUnsignedLong") }
  
  @Test def test_characterDuringValidUnsignedShort() { runner.runOneTest("characterDuringValidUnsignedShort") }  
  @Test def test_negativeUnsignedShort() { runner.runOneTest("negativeUnsignedShort") }
  @Test def test_whiteSpaceAfterLengthExceededUnsignedShort() { runner.runOneTest("whiteSpaceAfterLengthExceededUnsignedShort") }
  @Test def test_whiteSpaceBeforeLengthExceededUnsignedShort() { runner.runOneTest("whiteSpaceBeforeLengthExceededUnsignedShort") }
  @Test def test_whiteSpaceDuringLengthExceededUnsignedShort() { runner.runOneTest("whiteSpaceDuringLengthExceededUnsignedShort") }
  @Test def test_whiteSpaceAfterValidUnsignedShort() { runner.runOneTest("whiteSpaceAfterValidUnsignedShort") }
  
  @Test def test_unsignedShort_01() { runner.runOneTest("unsignedShort_01") }
  @Test def test_unsignedByte_01() { runner.runOneTest("unsignedByte_01") }
  @Test def test_unsignedByte_02() { runner.runOneTest("unsignedByte_02") }
  
  // Test range checking for signed integers too!
  @Test def test_byte_01() { runner.runOneTest("byte_01") }
  @Test def test_byte_02() { runner.runOneTest("byte_02") }

  
  val aj = testDir + "AJ.tdml"
  lazy val runnerAJ = new DFDLTestSuite(Misc.getRequiredResource(aj))
  
  @Test def test_AJ000() { runnerAJ.runOneTest("AJ000") }
  @Test def test_AJ001() { runnerAJ.runOneTest("AJ001") }
  
  val testDir_01 = "/daffodil/ibm-tests/"
  val aa_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(aa_01))
  @Test def test_schema_types_5_01() { runner_01.runOneTest("schema_types_5_01") }
  @Test def test_schema_types_5_02() { runner_01.runOneTest("schema_types_5_02") }
  @Test def test_schema_types_5_03() { runner_01.runOneTest("schema_types_5_03") }
  @Test def test_schema_types_5_04() { runner_01.runOneTest("schema_types_5_04") }
  @Test def test_schema_types_5_05() { runner_01.runOneTest("schema_types_5_05") }
  }
