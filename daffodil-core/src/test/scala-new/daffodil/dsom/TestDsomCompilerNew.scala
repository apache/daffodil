package daffodil.dsom

import daffodil.xml.XMLUtils
import daffodil.util._
import scala.xml._
import daffodil.compiler._
import org.scalatest.junit.JUnitSuite
import daffodil.schema.annotation.props.gen._
import daffodil.schema.annotation.props._
import daffodil.util.Misc
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import java.io.FileOutputStream
import java.nio.channels.WritableByteChannel
import java.io.FileWriter
import java.io.File
import java.nio.ByteBuffer
import org.junit.Test
import daffodil.debugger.Debugger

class TestDsomCompilerNew extends JUnitSuite with Logging {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val dummyGroupRef = null // just because otherwise we have to construct too many things.

  def FindValue(collection: Map[String, String], key: String, value: String): Boolean = {
    val found: Boolean = Option(collection.find(x => x._1 == key && x._2 == value)) match {
      case Some(_) => true
      case None => false
    }
    found
  }

  @Test def testHasPatternFacets() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="tns:st1"/>
      <xs:simpleType name="st1">
        <xs:restriction base="xs:string">
          <xs:pattern value="1"/>
          <xs:pattern value="2"/>
          <xs:pattern value="3"/>
        </xs:restriction>
      </xs:simpleType>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()

    assertEquals(1, decl.patternValues.length)
    val facets = decl.patternValues(0)
    assertEquals(1, facets.length)
    val facet = facets(0)
    val patterns = facet._2
    assertEquals(3, patterns.size)
    assertEquals("1", patterns(0).toString())
    assertEquals("2", patterns(1).toString())
    assertEquals("3", patterns(2).toString())
  }

  @Test def testPatternFacetsInheritance() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="tns:st1"/>
      <xs:simpleType name="st1">
        <xs:restriction base="tns:st2">
          <xs:pattern value="1"/>
          <xs:pattern value="2"/>
          <xs:pattern value="3"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:simpleType name="st2">
        <xs:restriction base="tns:st3">
          <xs:pattern value="4"/>
          <xs:pattern value="5"/>
          <xs:pattern value="6"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:simpleType name="st3">
        <xs:restriction base="xs:string">
          <xs:pattern value="7"/>
          <xs:pattern value="8"/>
          <xs:pattern value="9"/>
        </xs:restriction>
      </xs:simpleType>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    
    assertEquals(3, decl.patternValues.length)
    val facets1 = decl.patternValues(0)
    val facets2 = decl.patternValues(1)
    val facets3 = decl.patternValues(2)
    
    assertEquals(1, facets1.length)
    val facet1_1 = facets1(0)
    val patterns1 = facet1_1._2
    assertEquals(3, patterns1.size)
    assertEquals("1", patterns1(0).toString())
    assertEquals("2", patterns1(1).toString())
    assertEquals("3", patterns1(2).toString())
    
    assertEquals(1, facets2.length)
    val facet2_1 = facets2(0)
    val patterns2 = facet2_1._2
    assertEquals(3, patterns2.size)
    assertEquals("4", patterns2(0).toString())
    assertEquals("5", patterns2(1).toString())
    assertEquals("6", patterns2(2).toString())
    
    assertEquals(1, facets3.length)
    val facet3_1 = facets3(0)
    val patterns3 = facet3_1._2
    assertEquals(3, patterns3.size)
    assertEquals("7", patterns3(0).toString())
    assertEquals("8", patterns3(1).toString())
    assertEquals("9", patterns3(2).toString())
  }
}