/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.dsom

import scala.xml.Elem

import org.apache.daffodil.core.compiler._
import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert._
import org.junit.Test

/**
 * Tests that verify that Daffodil works properly with other
 * appinfo and non-native attribute annotations.
 */
class TestAppinfoSyntax {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE

  /**
   * Test that non-native attributes can be placed on DFDL appinfo annotations.
   */
  @Test def testAppinfoWithNonNativeAttributes(): Unit = {
    val expected = "this is a non-native attribute"
    val nnURI = "urn:nonNativeAttributeNamespaceURN"
    val sc = <xs:schema xmlns:xs={
      xsd
    } xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:foo={nnURI}>
               <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
               <xs:annotation>
                 <xs:appinfo source={dfdl} foo:nonNativeAttribute1={expected}>
                   <dfdl:format ref="GeneralFormat"/>
                 </xs:appinfo>
               </xs:annotation>
               <xs:element name="root" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1">
                 <xs:annotation>
                   <xs:appinfo source={dfdl}>
                     <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
                   </xs:appinfo>
                 </xs:annotation>
               </xs:element>
             </xs:schema>
    val compiler = Compiler()
    val sset = compiler.compileNode(sc).sset
    val Seq(sch) = sset.schemas
    val a = sch.schemaDocuments.head
    val Seq(ao: Elem) = a.dfdlAppInfos
    val Some(ns) = ao.attribute(nnURI, "nonNativeAttribute1")
    assertEquals(expected, ns.toString())
    val (_, actual) = TestUtils.testString(sc, "5")
    val expectedParseInfoset = <root>5</root>
    XMLUtils.compareAndReport(expectedParseInfoset, actual)
  }

  /**
   * Test that more than one DFDL appinfo at the same point causes
   * an exception.
   */
  @Test def testMultipleAppinfos(): Unit = {
    val expected = "this is a non-native attribute"
    val nnURI = "urn:nonNativeAttributeNamespaceURN"
    val sc = <xs:schema xmlns:xs={
      xsd
    } xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:foo={nnURI}>
               <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
               <xs:annotation>
                 <xs:appinfo source={dfdl} foo:nonNativeAttribute1={expected}>
                   <dfdl:format ref="GeneralFormat"/>
                 </xs:appinfo>
               </xs:annotation>
               <xs:element name="root" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1">
                 <xs:annotation>
                   <xs:appinfo source={dfdl}>
                     <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
                   </xs:appinfo>
                 </xs:annotation>
               </xs:element>
               <xs:annotation>
                 <xs:appinfo source={dfdl} foo:nonNativeAttribute2={expected}>
                   <dfdl:format ref="GeneralFormat"/>
                 </xs:appinfo>
               </xs:annotation>
             </xs:schema>
    val compiler = Compiler()
    val sset = compiler.compileNode(sc).sset
    val Seq(sch) = sset.schemas
    val a = sch.schemaDocuments.head
    val Seq(ao: Elem, bo: Elem) = a.dfdlAppInfos
    val Some(anna) = ao.attribute(nnURI, "nonNativeAttribute1")
    val Some(bnna) = bo.attribute(nnURI, "nonNativeAttribute2")
    assertEquals(expected, anna.toString())
    assertEquals(expected, bnna.toString())
    val e = intercept[Exception] {
      TestUtils.testString(sc, "5")
    }
    assertTrue(e.getMessage().contains("Only one format annotation"))
  }

  /**
   * Test that tries to  mix other annotation elements from other namespaces
   * inside an appinfo that uses the DFDL source.
   *
   * This is not allowed as it would impact our ability to provide good
   * diagnostics around the DFDL annotation elements.
   */
  @Test def testMixedAnnotationElementsInsideDFDLAppinfo(): Unit = {
    val expected = "this is a non-native attribute"
    val nnURI = "urn:nonNativeAttributeNamespaceURN"
    val sc = <xs:schema xmlns:xs={
      xsd
    } xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:foo={nnURI}>
               <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
               <xs:annotation>
                 <xs:appinfo source={dfdl} foo:nonNativeAttribute1={expected}>
                   <dfdl:format ref="GeneralFormat"/>
                   <foo:anotherAnnotationElementAdjacentToDFDLAnnotation/>
                 </xs:appinfo>
               </xs:annotation>
               <xs:element name="root" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1">
                 <xs:annotation>
                   <xs:appinfo source={dfdl}>
                     <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
                   </xs:appinfo>
                 </xs:annotation>
               </xs:element>
             </xs:schema>
    val compiler = Compiler()
    val sset = compiler.compileNode(sc).sset
    val Seq(sch) = sset.schemas
    val a = sch.schemaDocuments.head
    val Seq(ao: Elem) = a.dfdlAppInfos
    val Some(anna) = ao.attribute(nnURI, "nonNativeAttribute1")
    assertEquals(expected, anna.toString())
    val e = intercept[Exception] {
      TestUtils.testString(sc, "5")
    }
    assertTrue(e.getMessage().contains("Invalid dfdl annotation found: foo:another"))
  }

  /**
   * Test that non-DFDL appinfos do not interact with DFDL.
   */
  @Test def testNonDFDLAppinfos(): Unit = {
    val expected = "this is a non-native attribute"
    val nnURI = "urn:nonNativeAttributeNamespaceURN"
    val nnURI2 = "urn:anotherNonNativeURI"
    val sc = <xs:schema xmlns:xs={
      xsd
    } xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:foo={nnURI} xmlns:bar={nnURI2}>
               <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
               <xs:annotation>
                 <xs:appinfo source={nnURI} foo:nonNativeAttribute1={expected}>
                   <!-- since the source is not DFDL's, this should be completely ignored. -->
                   <foo:thisIsAnAnnotationElement/>
                 </xs:appinfo>
                 <xs:appinfo source={dfdl} foo:nonNativeAttribute1={expected}>
                   <dfdl:format ref="GeneralFormat"/>
                 </xs:appinfo>
               </xs:annotation>
               <xs:element name="root" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1">
                 <xs:annotation>
                   <xs:appinfo source={dfdl}>
                     <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
                   </xs:appinfo>
                 </xs:annotation>
               </xs:element>
               <xs:annotation>
                 <xs:appinfo source={nnURI2} bar:nonNativeAttribute2={expected}>
                   <bar:anotherAppinfoAnnotationElement/>
                 </xs:appinfo>
               </xs:annotation>
             </xs:schema>
    val compiler = Compiler()
    val sset = compiler.compileNode(sc).sset
    val Seq(sch) = sset.schemas
    val a = sch.schemaDocuments.head
    val Seq(ao: Elem) = a.dfdlAppInfos
    val Some(anna) = ao.attribute(nnURI, "nonNativeAttribute1")
    assertEquals(expected, anna.toString())
    val (_, actual) = TestUtils.testString(sc, "5")
    val expectedParseInfoset = <root>5</root>
    XMLUtils.compareAndReport(expectedParseInfoset, actual)
  }

  /**
   * Test that XML comments are allowed at all places in and around appinfo
   * elements.
   */
  @Test def testAppinfoWithComments(): Unit = {
    val expected = "this is a non-native attribute"
    val nnURI = "urn:nonNativeAttributeNamespaceURN"
    val sc = <xs:schema xmlns:xs={
      xsd
    } xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:foo={nnURI}>
               <!-- This is a comment -->
               <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
               <!-- This is a comment -->
               <xs:annotation>
                 <!-- This is a comment -->
                 <xs:appinfo source={dfdl} foo:nonNativeAttribute1={expected}>
                   <!-- This is a comment -->
                   <dfdl:format ref="GeneralFormat"/>
                   <!-- This is a comment -->
                 </xs:appinfo>
                 <!-- This is a comment -->
               </xs:annotation>
               <!-- This is a comment -->
               <xs:element name="root" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1">
                 <!-- This is a comment -->
                 <xs:annotation>
                   <!-- This is a comment -->
                   <xs:appinfo source={dfdl}>
                     <!-- This is a comment -->
                     <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
                     <!-- This is a comment -->
                   </xs:appinfo>
                   <!-- This is a comment -->
                 </xs:annotation>
                 <!-- This is a comment -->
               </xs:element>
               <!-- This is a comment -->
             </xs:schema>
    val compiler = Compiler()
    val sset = compiler.compileNode(sc).sset
    val Seq(sch) = sset.schemas
    val a = sch.schemaDocuments.head
    val Seq(ao: Elem) = a.dfdlAppInfos
    val Some(anna) = ao.attribute(nnURI, "nonNativeAttribute1")
    assertEquals(expected, anna.toString())
    val (_, actual) = TestUtils.testString(sc, "5")
    val expectedParseInfoset = <root>5</root>
    XMLUtils.compareAndReport(expectedParseInfoset, actual)
  }

}
