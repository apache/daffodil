package edu.illinois.ncsa.daffodil.tdml

import java.io.File
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.Utility
import scala.xml.XML
import edu.illinois.ncsa.daffodil.Implicits.using
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import junit.framework.Assert.assertFalse
import junit.framework.Assert.fail
import edu.illinois.ncsa.daffodil.util._
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.Implicits._

class TestTDMLUnparseCases {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example

  @Test def testUnparseSuite1() {

    val testSuite = <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:defineSchema name="s">
                        <xs:include schemaLocation="xsd/built-in-formats.xsd"/>
                        <dfdl:format ref="ex:daffodilTest1"/>
                        <xs:element name="bar" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
                      </ts:defineSchema>
                      <ts:unparserTestCase ID="test1" name="test1" root="bar" model="s">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <ex:bar>Hello</ex:bar>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>Hello</ts:document>
                      </ts:unparserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    val tc: UnparserTestCase = ts.unparserTestCases.find { utc => utc.name == "test1" }.get
    println(tc)
    val doc = tc.document
    val is = tc.inputInfoset
    println(is)
    val bar = is.dfdlInfoset.rawContents
    val scala.xml.Elem(pre, label, _, _, child) = bar
    assertEquals("ex", pre)
    assertEquals("bar", label)
    ts.runOneTest("test1")
  }

}