package daffodil.dsom

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import daffodil.processors.InStreamFromByteChannel
import daffodil.processors._
import daffodil.compiler._

class TestBinaryInput_01 extends JUnitSuite {

  var runner = {
    val testDir = "/test-suite/tresys-contributed/"
    val aa = testDir + "BinaryInput_01.tdml"
    lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
    runner
  }

  /*** DFDL-307 ***/
  // Verify Bit Extraction
  @Test
  def testBufferBitExtraction() {
    var in = Compiler.stringToReadableByteChannel("3")
    val inStream = new InStreamFromByteChannel(null, in, 3)
    assert(inStream.getPartialByte(1, 3) == 3)
  }

  @Test
  def test_one_octet() {
    runner.runOneTest("OneOctetBinaryParse")
  }
}
