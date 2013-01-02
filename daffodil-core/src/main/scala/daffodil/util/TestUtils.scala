package daffodil.util

import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import junit.framework.Assert.assertEquals
import java.io.File
import java.io.FileNotFoundException

/*
 * This is not a file of tests.
 * 
 * These are utilities to support unit testing schemas
 */
object TestUtils {

  /**
   * utility to create test schemas without having to repeat all the namespace definitions,
   * and the appinfo boilerplate. This makes tests more uniform.
   *
   * Defines standard named formats that can be re-used by tests. This helps with the issue
   * of having to add properties everywhere when a new property starts being expected.
   */

  val test1 = <dfdl:defineFormat name="daffodilTest1">
                <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" alignment='1' alignmentUnits='bytes' textStandardBase='10' binaryFloatRep='ieee' binaryNumberRep='binary' byteOrder='bigEndian' calendarPatternKind='implicit' escapeSchemeRef='' documentFinalTerminatorCanBeMissing='no' ignoreCase='no' initiatedContent='no' leadingSkip='0' lengthKind='implicit' occursCountKind='parsed' separatorPolicy='suppressed' separatorPosition='infix' sequenceKind='ordered' textNumberRep='standard' textNumberCheckPolicy='strict' textStringJustification='left' trailingSkip='0' initiator="" terminator="" separator="" emptyValueDelimiterPolicy="both" utf16Width="fixed" textTrimKind="none"/>
              </dfdl:defineFormat>

  def dfdlTestSchema(topLevelAnnotations: Seq[Node], contentElements: Seq[Node], fileName: String = "") = {
    val fileAttrib = (if (fileName == "") Null else Attribute(XMLUtils.INT_PREFIX, "file", Text(fileName), Null))
    val realSchema = <xs:schema xmlns:xs={ xsdURI } xmlns:xsd={ xsdURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI } xmlns:fn={ fnURI } xmlns={ targetNS } xmlns:tns={ targetNS } xmlns:dafint={ dafintURI } targetNamespace={ targetNS }>
                       <xs:annotation>
                         <xs:appinfo source={ dfdlURI }>
                           { test1 }
                           { topLevelAnnotations }
                         </xs:appinfo>
                       </xs:annotation>
                       <xsd:import namespace={ DFDLSubsetURI } schemaLocation="DFDLSubsetOfXMLSchema_v1_036.xsd"/>
                       <xsd:import namespace={ xsdURI } schemaLocation="XMLSchema.xsd"/>
                       <xsd:import namespace={ dfdlURI } schemaLocation="DFDL_part3_model.xsd"/>
                       { contentElements }
                     </xs:schema> % fileAttrib
    //
    // It is essential to stringify and then reload the above schema because the
    // pieces being spliced in don't have the namespace definitions for the prefixes.
    // This massively reduces clutter for creation of test schemas in tests.
    //
    // Writing it out to a string, and reloading
    // forces reinterpretation of all the prefixes as new nodes are created. The 
    // enclosing nodes created above have the namespace definitions.
    // 
    val realSchemaText = realSchema.toString()
    val real = XML.loadString(realSchemaText)
    real
  }

  /**
   * Just like dfdlTestSchema, but without any namespace or targetNamespace definitions, which is
   * the way many self-contained one-file schemas are written.
   */
  def dfdlTestSchemaNoNamespace(topLevelAnnotations: Seq[Node], contentElements: Seq[Node]) = {
    val realSchema = <xs:schema xmlns:xs={ xsdURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI }>
                       <xs:annotation>
                         <xs:appinfo source={ dfdlURI }>
                           { test1 }
                           { topLevelAnnotations }
                         </xs:appinfo>
                       </xs:annotation>
                       { contentElements }
                     </xs:schema>
    val realSchemaText = realSchema.toString() // see comment in dfdlTestSchema for why we do this
    val real = XML.loadString(realSchemaText)
    real
  }

  /**
   * Compares two XML Elements, after having stripped off all attributes.
   *
   * TODO: we might start using xsi:type attributes at some point. If so fix this to
   * save that attribute.
   *
   * NOTE: Has Side Effects: strips off attributes
   */
  def assertEqualsXMLElements(expected: Node, actual: Node) {
    val exp = XMLUtils.removeAttributes(expected)
    val act = XMLUtils.removeAttributes(actual)
    assertEquals(exp, act)
  }

  /**
   * We want to be able to run tests from Eclipse or from batch builds that
   * are rooted in a different directory, so, since Java/JVMs don't have a notion
   * of setting the current directory to a specific value for interpreting things,
   * we have to do that ourselves manually like this.
   *
   * When you specify a file for use in a test, you want to specify it
   * relative to the root of the sub-project of which it is part. I.e., within core,
   * the file you specify should be relative to daffodil/sub-projects/core.
   *
   * Returns null if the file cannot be found.
   */
  def findFile(fn: String): File = findFile(new File(fn))
  def findFile(f: File): File = {
    if (f.exists()) return f
    val cwd = new File("").getAbsolutePath
    throw new FileNotFoundException("Couldn't find file " + f + " relative to " + cwd + ".")
  }

}