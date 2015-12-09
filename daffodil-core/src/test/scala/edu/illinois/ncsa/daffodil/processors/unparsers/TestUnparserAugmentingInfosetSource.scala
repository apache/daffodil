//package edu.illinois.ncsa.daffodil.processors.unparsers
//
//// import junit.framework.Assert._
//import org.junit.Test
//import edu.illinois.ncsa.daffodil.xml._
//import edu.illinois.ncsa.daffodil.util._
//import edu.illinois.ncsa.daffodil.compiler._
//import junit.framework.Assert.assertEquals
//import edu.illinois.ncsa.daffodil.equality._; object ENoWarnU2 { EqualitySuppressUnusedImportWarning() }
//import edu.illinois.ncsa.daffodil.Implicits._
//import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
//import edu.illinois.ncsa.daffodil.processors.DataProcessor
//import edu.illinois.ncsa.daffodil.processors.DIComplex
//import edu.illinois.ncsa.daffodil.processors.DISimple
//import edu.illinois.ncsa.daffodil.processors.OutputValueCalcEvaluationException
//import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils
//
//class TestUnparserAugmentingInfosetSource {
//
//  def infosetRootERDAndSource(testSchema: scala.xml.Node, infosetXML: scala.xml.Node) = {
//    val compiler = Compiler()
//    val pf = compiler.compileNode(testSchema)
//    if (pf.isError) {
//      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
//      throw new Exception(msgs)
//    }
//    val u = pf.onPath("/").asInstanceOf[DataProcessor]
//    if (u.isError) {
//      val msgs = u.getDiagnostics.map(_.getMessage).mkString("\n")
//      throw new Exception(msgs)
//    }
//    val xmlEventCursor = XMLUtils.nodeToXMLEventCursor(infosetXML)
//    val rootERD = u.ssrd.elementRuntimeData
//    val infosetSource = InfosetSource.fromXMLSource(xmlEventCursor, rootERD)
//    val augmentingSource = new UnparserAugmentingInfosetSource(rootERD, infosetSource)
//    (rootERD, augmentingSource)
//  }
//
//  @Test def testUnparsingOVCWithLookAhead3 {
//    val sch = SchemaUtils.dfdlTestSchema(
//      <dfdl:format ref="tns:daffodilTest1"/>,
//      <xs:element name="root" xmlns:ex={ XMLUtils.EXAMPLE_NAMESPACE }>
//        <xs:complexType>
//          <xs:sequence>
//            <xs:element name="ovc1" dfdl:outputValueCalc='{ ../ex:after1 }' dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
//            <xs:element name="after1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="5"/>
//          </xs:sequence>
//        </xs:complexType>
//      </xs:element>)
//    val infosetXML = <root xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><after1>Hello</after1></root>
//    TestUtils.testUnparsing(sch, infosetXML, "HelloHello")
//  }
//
//}
