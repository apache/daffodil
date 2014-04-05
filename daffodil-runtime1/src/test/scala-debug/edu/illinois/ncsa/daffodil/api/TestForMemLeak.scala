package edu.illinois.ncsa.daffodil.api
import junit.framework.Assert._
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.util._
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.xml._

//object Fakes {
//  lazy val sch = <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
 //                  <xs:element name="e1" type="xs:int" dfdl:representation="binary" dfdl:inputValueCalc="{ 42 }" dfdl:encodingErrorPolicy="replace" dfdl:encoding="ascii"/>
 //                </xs:schema>
//  lazy val xsd_sset = new SchemaSet(null, sch, "", "fake")
//  lazy val xsd_schema = xsd_sset.getSchema(NS("")).get
//  lazy val fakeSD = xsd_schema.schemaDocuments(0)
//}

//class H extends OOLAGHost() {
//     def rethrowAsDiagnostic(th: Throwable) = ???
//      oolagContextViaSet == None
//      override lazy val oolagRoot = this
//  }
//
//object Fakes {
//  lazy val fakeSD = new H
//}




  
class TestForMemLeak {

  @Test def testParseSimpleLeakChecking() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    val actual = TestUtils.testString(sch, "5678").result
    val expected: Node = <e1>5678</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }


  @Test def testLeak() {
    1 to 100 foreach { _ =>
      println("")
      1 to 100 foreach { _ =>
        print(".")
        1 to 100 foreach { _ =>
          System.gc()
          Thread.sleep(100)
          testParseSimpleLeakChecking()
        }
      }
    }
  }

}