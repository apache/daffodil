package daffodil.xml

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.Test
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import daffodil.processors._
import daffodil.compiler._
import daffodil.debugger.Debugger
import daffodil.Implicits.using
import java.net.URL
import java.io.File

class TestXMLLoaderWithLocation extends JUnitSuite {

  @Test def testBasic() {
    val node = XMLLoaderWithLocator.loadString("<a><b/></a>")
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
      val node = XMLLoaderWithLocator.loadFile(res)
      println(node)
      assertTrue(node.toString.toLowerCase.contains("dafint:file"))
    } finally {
      val t = new java.io.File(tmpXMLFileName)
      t.delete()
    }
  }
}