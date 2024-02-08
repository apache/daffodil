package org.apache.daffodil.tdml

import org.apache.daffodil.tdml.scalaxb.TestSuite

import org.junit.Assert._
import org.junit.Test

class ScalaxbTests {

  @Test def testReading(): Unit = {
    val testSuite =
      _root_.scalaxb.fromXML[TestSuite](
        scala.xml.XML.load(
          getClass
            .getClassLoader()
            .getResourceAsStream("test-suite/ibm-contributed/dpaext1-2.tdml"),
        ),
      )

    assertNotNull(testSuite)
    assertEquals(Some("dpaext"), testSuite.suiteName)
  }
}
