package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors.xpath._
import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.SchemaUtils

class TextCompileExpressionExecution {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testCompiledRelativePathEvaluation6() {

    // Note: removed targetNamespace={ example }. 
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format xmlns:tns={ example } ref="tns:daffodilTest1"/>,
      <xs:element name="data" dfdl:lengthKind="implicit" dfdl:initiator="" dfdl:terminator="">
        <xs:complexType>
          <xs:sequence dfdl:separator="" dfdl:initiator="" dfdl:terminator="" dfdl:initiatedContent="no">
            <xs:element name="e1" type="xs:string" dfdl:encoding="ascii" dfdl:lengthUnits="bytes" dfdl:lengthKind="explicit" dfdl:length="2" dfdl:initiator="" dfdl:terminator=""/>
            <xs:element name="e2" type="xs:string" dfdl:inputValueCalc="{ ../tns:e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val edecl = sset.getGlobalElementDecl(example, "data").get.forRoot() // removed namespace example => ""
    val ct = edecl.typeDef.asInstanceOf[ComplexTypeBase]
    val d = Misc.stringToReadableByteChannel("42")
    val compiler = Compiler()
    val pf = compiler.compile(Seq.empty, testSchema)
    val dp = pf.onPath("/")
    val resState = dp.parse(d)
    val resNode = resState.result
    //println(resNode)
    val Seq(e2) = (resNode \\ "e2")
    val dataNode = e2.text
    //println(dataNode)

    assertEquals("42", dataNode)

  }
}