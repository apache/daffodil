package edu.illinois.ncsa.daffodil.usertests
import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.Implicits._

class TestLeaks {
  val testDir = "/edu/illinois/ncsa/daffodil/usertests/"
  val aa = testDir + "leaks.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  def volumeRunner(tc: => Unit) {
    1 to 100 foreach { _ =>
      println("")
      1 to 100 foreach { _ =>
        print(".")
        1 to 100 foreach { _ =>
          System.gc()
          Thread.sleep(100)
          tc
        }
      }
    }
  }

  @Test def test_leak1() { runner.runOneTest("leak1") }
  @Test def test_leak2() { runner.runOneTest("leak2") }
  @Test def test_leak3() { runner.runOneTest("leak3") }
  @Test def test_leak4() { runner.runOneTest("leak4") }

  @Test def testLeak1() { volumeRunner(test_leak1()) } // no leak - correct, but no parsing (input value calc)

  @Test def testLeak2() { volumeRunner(test_leak2()) } // no leak - runtime error

  @Test def testLeak3() { volumeRunner(test_leak3()) } // no leak - compilation error
  @Test def testLeak4() { volumeRunner(test_leak4()) } // leaks horribly 

}