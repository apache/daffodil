package edu.illinois.ncsa.daffodil.xml

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.Implicits._
import java.net.URL
import java.io.File

class TestXMLLoaderWithLocation extends JUnitSuite {

  @Test def testBasic() {
    val node = (new DaffodilXMLLoader(BasicStderrErrorHandler)).loadString("<a><b/></a>")
    println(node)
    assertTrue(node.toString.toLowerCase.contains("dafint:line"))
  }

  @Test def testFile1() {
    val tmpXMLFileName = getClass.getName() + ".xml"
    // Our loader looks for xs:schema node, and appends a file attribute
    // if it can.
    val testXML = <xs:schema xmlns:xs={ XMLUtils.XSD_NAMESPACE }><b/></xs:schema>
    try {
      using(new java.io.FileWriter(tmpXMLFileName)) {
        fw =>
          fw.write(testXML.toString())
      }
      val res = new File(tmpXMLFileName)
      val node = (new DaffodilXMLLoader(BasicStderrErrorHandler)).loadFile(res)
      assertTrue(node.toString.toLowerCase.contains("dafint:file"))
    } finally {
      val t = new java.io.File(tmpXMLFileName)
      t.delete()
    }
  }

  @Test def testCatalogResolver() {
    val baseURI: String = new File(".").toURI().toString
    val ldr = new DaffodilXMLLoader(BasicStderrErrorHandler)
    val pId: String = null
    val sId: String = null
    val resolved = ldr.resolver.resolveResource(XMLUtils.XSD_NAMESPACE, XMLUtils.XSD_NAMESPACE, pId, sId, baseURI)
    println(resolved)
  }

  @Test def testFileValidation() {
    val tmpXMLFileName = getClass.getName() + ".xml"
    // Our loader looks for xs:schema node, and appends a file attribute
    // if it can.
    val testXML = <xs:schema xmlns:xs={ XMLUtils.XSD_NAMESPACE }><xs:illegal/></xs:schema>
    try {
      using(new java.io.FileWriter(tmpXMLFileName)) {
        fw =>
          fw.write(testXML.toString())
      }
      val res = new File(tmpXMLFileName)
      val node = (new DaffodilXMLLoader(BasicStderrErrorHandler)).loadFile(res)
      assertTrue(node.toString.toLowerCase.contains("dafint:file"))
    } finally {
      val t = new java.io.File(tmpXMLFileName)
      t.delete()
    }
  }
}