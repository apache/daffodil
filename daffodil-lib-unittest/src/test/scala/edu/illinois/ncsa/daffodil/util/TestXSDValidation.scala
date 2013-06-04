package edu.illinois.ncsa.daffodil.util

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */


import scala.xml._;
import junit.framework.Assert._;
import org.junit.Test;
import java.io.StringReader;
import java.io.InputStreamReader; //class TestXSDValidation {
//
//  val xmlnsURI = "http://www.w3.org/2001/XMLSchema";
//  val xsdSubsetURI = "http://www.w3.org/2001/XMLSchema";
//  val dfdlURI = "http://www.ogf.org/dfdl/dfdl-1.0/";
//  val xsiURI = "http://www.w3.org/2001/XMLSchema-instance";
//  val targetNS = "http://example.com";
//
//  @Test def testValidationNoTargetNamespace() {
//    val schema =
//      <xs:schema xmlns:xs={ xmlnsURI }>
//        <xs:element name="foo" type="xs:string"/>
//      </xs:schema>; // xml literals confuse Eclipse's scala plug in. Put them in to make it happy.
//    val document =
//      <foo>bar</foo>;
//    Validator.validateXMLNodes(schema, document)
//  };
//  @Test def testValidationWithTargetNamespace() {
//    val schema =
//      <xs:schema xmlns:xs={ xmlnsURI } targetNamespace={ targetNS }>
//        <xs:element name="foo" type="xs:string"/>
//      </xs:schema>; // xml literals confuse Eclipse's scala plug in. Put semicolons in to make it happy.
//    val document =
//      <foo xmlns={ targetNS }>bar</foo>;
//    Validator.validateXMLNodes(schema, document)
//  };
//  @Test def testSchemaValidationQualifiedXSPrefixes() {
//    ; // xml literals confuse Eclipse's scala plug in. Put semicolons in to make it happy.
//    val document = <xs:schema xmlns:xs={ xmlnsURI } targetNamespace={ targetNS }>
//                     <xs:element name="foo" type="xs:string"/>
//                   </xs:schema>;
//    val documentReader = new StringReader(document.toString());
//    val schemaResource = Misc.getRequiredResource("/xsd/XMLSchema.xsd");
//    assertTrue(schemaResource != null);
//    Validator.validateXMLStream(schemaResource.toURI(), documentReader)
//  };
//  @Test def testSchemaValidationNoQualifiedXSPrefixes() {
//    ; // xml literals confuse Eclipse's scala plug in. Put semicolons in to make it happy.
//    val document =
//      <schema xmlns={ xmlnsURI } xmlns:xs={ xmlnsURI } targetNamespace={ targetNS }>
//        <element name="foo" type="xs:string"/>
//      </schema>;
//    val documentReader = new StringReader(document.toString());
//    val schemaResource = Misc.getRequiredResource("/xsd/XMLSchema.xsd").toURI();
//    Validator.validateXMLStream(schemaResource, documentReader)
//  };
//  @Test def testSchemaValidationNoTargetNamespaceAndNoQualifiedXSPrefixes() {
//    ; // xml literals confuse Eclipse's scala plug in. Put semicolons in to make it happy.
//    val document =
//      <schema xmlns={ xmlnsURI } xmlns:xs={ xmlnsURI }>
//        <element name="foo" type="xs:string"/>
//      </schema>;
//    val documentReader = new StringReader(document.toString());
//    val schemaResource = Misc.getRequiredResource("/xsd/XMLSchema.xsd").toURI();
//    Validator.validateXMLStream(schemaResource, documentReader)
//  };
//  @Test def testInvalidSchemaValidationNoTargetNamespaceAndNoQualifiedXSPrefixes() {
//    ; // xml literals confuse Eclipse's scala plug in. Put semicolons in to make it happy.
//    val document =
//      <schema xmlns={ xmlnsURI } xmlns:xs={ xmlnsURI }>
//        <element notValidAttribute="foo" type="xs:string"/>
//      </schema>;
//    val documentReader = new StringReader(document.toString());
//    val schemaResource = Misc.getRequiredResource("/xsd/XMLSchema.xsd").toURI();
//    val ex = intercept[Exception] {
//      Validator.validateXMLStream(schemaResource, documentReader)
//    };
//    val msg = ex.getMessage();
//    val hasMsgInfo = msg.contains("notValidAttribute");
//    assertTrue(hasMsgInfo);
//  };
//  /**
//   * constructs a DFDL schema using our defined namespaces including for the subset of XSD we support in DFDL.
//   */
//  def xsdUsingSubset(topLevelAnnotations: Seq[Node], contentElements: Seq[Node]) = {
//    val sch =
//      <xs:schema xmlns:xs={ xsdSubsetURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI } targetNamespace={ targetNS }>
//        <xs:annotation>
//          <xs:appinfo source={ dfdlURI }>
//            { topLevelAnnotations }
//          </xs:appinfo>
//        </xs:annotation>
//        { contentElements }
//      </xs:schema>; // xml literals confuse Eclipse's scala plug in. Put them in to make it happy.
//
//    val schTxt = sch.toString();
//    // we do this to get the namespace prefixes in the argument nodes re-interpreted according to our definitions.
//    // this is a function of scala's parsing, so things have to be lexically enclosed, which is painful at best. 
//    // I'd really prefer to be able to do some sort of withMyNSByDefault { <foo:bar baz:quux="value"/> }
//    // but I don't know how to achieve that.
//    val sch2 = XML.loadString(schTxt);
//    sch2
//  };
//
//  /**
//   * constructs a DFDL schema using our defined namespaces including for the subset of XSD we support in DFDL.
//   * But this variation does not xs: qualify all the XML Schema elements. Many DFDL schemas will get written
//   * this way, even though it is a sort of frowned-upon style.
//   */
//  def xsdUsingSubsetNoPrefix(topLevelAnnotations: Seq[Node], contentElements: Seq[Node]) = {
//    val sch =
//      <schema xmlns={ xsdSubsetURI } xmlns:xs={ xsdSubsetURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI } targetNamespace={ targetNS }>
//        <annotation>
//          <appinfo source={ dfdlURI }>
//            { topLevelAnnotations }
//          </appinfo>
//        </annotation>
//        { contentElements }
//      </schema>; // xml literals confuse Eclipse's scala plug in. Put them in to make it happy.
//
//    val schTxt = sch.toString();
//    // we do this to get the namespace prefixes in the argument nodes re-interpreted according to our definitions.
//    // this is a function of scala's parsing, so things have to be lexically enclosed, which is painful at best. 
//    // I'd really prefer to be able to do some sort of withMyNSByDefault { <foo:bar baz:quux="value"/> }
//    // but I don't know how to achieve that.
//    val sch2 = XML.loadString(schTxt);
//    sch2
//  }; ;
//  def validateDFDLSchema(xsSchema: NodeSeq) {
//    val schemaForDFDLSchema = Misc.getRequiredResource(Validator.dfdlSchemaFileName()).toURI();
//    val xsReader = new StringReader(xsSchema.toString());
//    Validator.validateXMLStream(schemaForDFDLSchema, xsReader)
//  };
//  @Test def testBasicSchemaProcessing() {
//    val schema =
//      xsdUsingSubset(Nil,
//        <xs:element name="foobar" type="xs:int"/>);
//    validateDFDLSchema(schema)
//  };
//  @Test def testBasicInvalidSchemaProcessing() {
//    val schema =
//      xsdUsingSubset(Nil,
//        <xs:element notValidAttribute="foobar" type="xs:int"/>);
//    val ex = intercept[Exception] {
//      validateDFDLSchema(schema)
//    };
//    val msg = ex.getMessage();
//    val hasMsgInfo = msg.contains("notValidAttribute");
//    assertTrue(hasMsgInfo);
//  };
//  @Test def testBasicSchemaProcessingNoPrefix() {
//    val schema =
//      xsdUsingSubsetNoPrefix(Nil,
//        <element name="foobar" type="xs:int"/>);
//    validateDFDLSchema(schema)
//  };
//  @Test def testValidationOfXSDSchemaSubsetErrors() {
//    val schema1 = xsdUsingSubset(Nil,
//      <xs:element1 name="foo" type="xs:int"/> // <!-- error. No such element -->
//      )
//    val ex = intercept[Exception] {
//      validateDFDLSchema(schema1)
//    }
//    // println(ex)
//    val msg = ex.getMessage()
//    val hasElement1InMsg = msg.contains("element1");
//    assertTrue(hasElement1InMsg);
//  }
//
//  @Test def testValidationOfDFDLLongFormAnnotationPropertyNameErrors() {
//    val schema2 = xsdUsingSubset(
//      <dfdl:defineFormat name="foo">
//        <dfdl:format byteOrder1="bigEndian"/><!-- error: incorrect property name -->
//      </dfdl:defineFormat>,
//      Nil)
//    val ex = intercept[Exception] {
//      validateDFDLSchema(schema2);
//    }
//    // should throw a validation error. 
//    // println(ex)
//    val msg = ex.getMessage()
//    val hasErrorText = msg.contains("byteOrder1")
//    assertTrue(hasErrorText)
//  };
//  @Test def testValidationOfDFDLShortFormPropertyValueError() {
//    val schema2 = xsdUsingSubset(Nil,
//      <xs:element name="foo" dfdl:byteOrder="invalidValue" type="xs:int"/>);
//    val ex = intercept[Exception] {
//      validateDFDLSchema(schema2);
//    };
//    // should throw a validation error. 
//    // println(ex) ;
//    val msg = ex.getMessage();
//    val hasErrorText = msg.contains("invalidValue");
//    assertTrue(hasErrorText)
//  }
//
//  // Test related to Jira task DFDL-76
//  // Schema below should error out, because name 'bar' isn't a valid internal reference to the type. It should
//  // be caught as in the xsd namespace, which won't allow it to match the targetNS.
//  //  @Test def testSchemaValidationReferentialIntegrityChecking() {
//  //    //
//  //    // Something about this style of XML Literal breaks Eclipse's scala compilation
//  //    // and it needs you to put semi-colons all over the place.
//  //    //
//  //    val schema =
//  //      xsdUsingSubsetNoPrefix(Nil,
//  //                       <element name="foo" type="bar"/>
//  //                       <complexType name="bar">
//  //                         <sequence/>
//  //                       </complexType>
//  //      ) ;
//  //     println("THIS TEST SHOULD PASS, BUT VALIDATION ISNT CHECKING FOR THIS COMMON ERROR, SO THE TEST FAILS");
//  //     val exc = intercept[Exception] {
//  //    	 validateDFDLSchema(schema)
//  //     } ;
//  //     assertTrue(exc.getMessage().contains("bar"))
//  //  }
//
//} // end class
