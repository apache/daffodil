package daffodil.dsom



import junit.framework.Assert._

import org.scalatest.junit.JUnit3Suite
import daffodil.schema.annotation.props.gen._


/**
 * Scala Unit Testing Notes:
 *
 * It is important that the Eclipse IDE make it convenient to run the unit tests, step the user directly to the point
 * of failure, etc.
 *
 * Scalatest doesn't do this directly, but using it driven by JUnit3 does. 
 *
 * So I'm advocating that a much more vanilla approach be taken to unit tests. Straight use of Junit3.
 *
 * Here is an example. Some simple tests, some that intercept exceptions, and demonstrate that the intercept
 * device works properly.
 */
class TestDsomCompiler extends JUnit3Suite {

  // @Test
  def test() {
    val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <!-- Basic variable definition and inputValueCalc -->
        <annotation>
          <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
            <dfdl:format byteOrder="bigEndian"/>
          </appinfo>
        </annotation>
        <element name="list" type="tns:example1"/>
        <complexType name="example1">
          <sequence>
            <element name="w" type="xsd:int" dfdl:inputValueCalc="{ $x + 1 }"/>
          </sequence>
        </complexType>
      </schema>

    val sd = DsomCompiler.compile(testSchema)
    val ge = sd.globalElementDecls
    val df = sd.defaultFormat
   // val Some(bo) = df.xml.attribute("byteOrder")
    assertEquals(1, ge.length)
   // assertEquals("bigEndian", bo.text)
  }
  
  def test2() {
        val testSchema =
      <schema xmlns="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" xmlns:tns="http://example.com" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        <!-- Basic variable definition and inputValueCalc -->
        <annotation>
          <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
            <dfdl:format byteOrder="bigEndian" alignmentUnits="byte"/>
          </appinfo>
        </annotation>
        <element name="list" type="tns:example1">
          <annotation>
            <appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
             <dfdl:element encoding="ASCII" alignmentUnits="bytes"/>
            </appinfo>
          </annotation>
        </element>
        <complexType name="example1">
          <sequence>
            <element name="w" type="xsd:int" dfdl:inputValueCalc="{ $x + 1 }"/>
          </sequence>
        </complexType>
      </schema>

    val sd = DsomCompiler.compile(testSchema)
    val ge = sd.globalElementDecls.toList
    val Some(le) = ge.find{ed=>ed.name == "list"}
    val fa = le.formatAnnotation.asInstanceOf[DFDLElement]
    assertEquals(AlignmentUnits.Bytes, fa.alignmentUnits)
    fa.alignmentUnits match {
      case AlignmentUnits.Bits => println("was bits")
      case AlignmentUnits.Bytes => println("was bytes")
    }
    //fa.occursCountKind
    println(fa.toString())
    
    //val Some(bo) = df.xml.attribute("byteOrder")
    assertEquals(1, ge.length)
    //assertEquals("bigEndian", bo.text)
  }
  
}