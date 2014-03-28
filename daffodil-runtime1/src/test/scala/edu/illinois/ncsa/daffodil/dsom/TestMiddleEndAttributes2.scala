package edu.illinois.ncsa.daffodil.dsom

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import org.junit.Test
import edu.illinois.ncsa.daffodil.processors.PrimitiveFactory
import edu.illinois.ncsa.daffodil.util.TestUtils

class TestMiddleEndAttributes2 {
  
  @Test def testNestedSequencePrefixSep() = {
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    val testSchema = SchemaUtils.dfdlTestSchema(

      <dfdl:format ref="tns:daffodilTest1" lengthKind="delimited" encoding="US-ASCII"/>,

      <xs:element name="e1">
        <xs:complexType>
          <xs:sequence dfdl:separator="/" dfdl:separatorPosition="prefix">
            <xs:sequence>
              <xs:element name="x" type="xs:int"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val sset = new SchemaSet(PrimitiveFactory, testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // Explore global element decl
    val Seq(e1f) = sd.globalElementDecls
    val e1 = e1f.forRoot()
    val e1ct = e1.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val seq1 = e1ct.modelGroup.asInstanceOf[Sequence]
    val mems = seq1.groupMembers
    val Seq(t1: Term) = mems
    val seq2 = t1.asInstanceOf[Sequence]
    val actual = TestUtils.testString(testSchema, "/5").result
    val actualString = actual.toString
    val expected = <e1><x>5</x></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }
  
}