package edu.illinois.ncsa.daffodil.infoset

import org.junit.Assert.assertTrue
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits.intercept
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError

class TestInfosetCursor1 {

  def infosetCursor(testSchema: scala.xml.Node, infosetRdr: java.io.Reader) = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val rootERD = u.ssrd.elementRuntimeData
    val ic = InfosetCursor.fromXMLReader(infosetRdr, rootERD)
    ic
  }

  @Test def testInfosetCursorOnBadData() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)

    val rdr = new java.io.StringReader("this is not XML");
    val ic = infosetCursor(sch, rdr)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get
    assertTrue(msg.contains("prolog")) // content not allowed in prolog of an XML document.
  }

  @Test def testInfosetCursorOnBadData2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)

    val rdr = new java.io.StringReader("""<this pretends to be xml""");
    val ic = infosetCursor(sch, rdr)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get

    assertTrue(msg.contains("Unexpected character")) // expects an equal sign for an attribute
  }

  @Test def testInfosetCursorOnBadData3() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)

    val rdr = new java.io.StringReader("\u0000\u0000\uFFFF\uFFFF");
    val ic = infosetCursor(sch, rdr)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get
    assertTrue(msg.contains("Illegal character")) // content not allowed in prolog.
  }
}
