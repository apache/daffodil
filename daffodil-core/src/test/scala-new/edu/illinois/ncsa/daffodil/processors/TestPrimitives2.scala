package edu.illinois.ncsa.daffodil.processors

import org.junit.Test
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.TestUtils

class TestPrimitives2 {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testUnparseNilValueEntities() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" nillable="true" dfdl:nilKind="literalValue" dfdl:lengthKind="delimited" type="xs:string" dfdl:nilValue="%WSP;nil%NL; foobar" dfdl:outputNewline="%LF;"/>,
      elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example } xsi:nil="true"/>
    TestUtils.testUnparsing(sch, infoset, " nil\u000a")
  }

  @Test def testUnparseNilValueEntities2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:nilValue="start%WSP;bla%%WSP;;;;foo%WSP*;bar%WSP+;baz%ES;quux%NL;boo%%baz%%NL;end" dfdl:outputNewline="%LF;" nillable="true" dfdl:nilKind="literalValue" dfdl:lengthKind="delimited" type="xs:string"/>,
      elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example } xsi:nil="true"/>
    TestUtils.testUnparsing(sch, infoset, "start bla%WSP;;;;foobar bazquux\u000aboo%baz%NL;end")
  }

}