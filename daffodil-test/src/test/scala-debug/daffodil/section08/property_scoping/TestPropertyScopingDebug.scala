package daffodil.section08.property_scoping

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

class TestPropertyScopingDebug extends JUnitSuite {
  val testDir = "/daffodil/section08/property_scoping/"
  val aa = testDir + "PropertyScoping.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  
  val tdml = testDir + "PropertyScoping_01.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
}
