package daffodil.tdml

import java.io.File
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.Utility
import scala.xml.XML
import org.scalatest.junit.JUnitSuite
import daffodil.Implicits.using
import daffodil.compiler.Compiler
import daffodil.xml.XMLUtils
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import daffodil.util._
import org.junit.Test
import daffodil.debugger.Debugger
import daffodil.Implicits._

class TestTDMLRunner2 extends JUnitSuite {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  val sub = XMLUtils.DFDL_XMLSCHEMASUBSET_NAMESPACE

  @Test def testTDMLUnparse() {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } xmlns:tns={ tns } xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:xsi={ xsi } suiteName="theSuiteName">
                      <ts:defineSchema name="unparseTestSchema1">
                        <dfdl:format ref="tns:daffodilTest1"/>
                        <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 9 }"/>
                      </ts:defineSchema>
                      <ts:serializerTestCase ID="some identifier" name="testTDMLUnparse" root="data" model="unparseTestSchema1">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example }>123456789</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>123456789</ts:document>
                      </ts:serializerTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTDMLUnparse")
  }

}
