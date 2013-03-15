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

import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import junit.framework.Assert.assertEquals
import java.io.File
import java.io.FileNotFoundException
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.Implicits._

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
                <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" alignment='1' alignmentUnits='bytes' textStandardBase='10' binaryFloatRep='ieee' binaryNumberRep='binary' byteOrder='bigEndian' calendarPatternKind='implicit' calendarFirstDayOfWeek='Sunday' calendarDaysInFirstWeek='4' calendarTimeZone='UTC' calendarCheckPolicy='strict' calendarLanguage='en' escapeSchemeRef='' documentFinalTerminatorCanBeMissing='no' ignoreCase='no' initiatedContent='no' leadingSkip='0' lengthKind='implicit' occursCountKind='parsed' separatorPolicy='suppressed' separatorPosition='infix' sequenceKind='ordered' textNumberRep='standard' textNumberCheckPolicy='strict' textStringJustification='left' trailingSkip='0' initiator="" terminator="" separator="" emptyValueDelimiterPolicy="both" utf16Width="fixed" textTrimKind="none"/>
              </dfdl:defineFormat>

  //  def appendScopes(ns1: NamespaceBinding, ns2: NamespaceBinding): NamespaceBinding = {
  //    if (ns1 == TopScope) ns2
  //    else if (ns2 == TopScope) TopScope
  //    else {
  //      val ns2URI = ns2.getURI(ns1.prefix)
  //      if (ns2URI == null) ns1.copy(parent = appendScopes(ns1.parent, ns2))
  //      else if (ns2URI == ns1.uri) {
  //        // same prefix is bound to the same uri.
  //        // so don't copy.
  //        appendScopes(ns1.parent, ns2)
  //      } else {
  //        // same prefix bound to different URIs
  //        // first one wins
  //        appendScopes(ns1, ns2.parent)
  //      }
  //    }
  //  }

  /**
   * Constructs a DFDL schema more conveniently than having to specify all those xmlns attributes.
   */
  def dfdlTestSchema(topLevelAnnotations: Seq[Node], contentElements: Seq[Node], fileName: String = "") = {
    val fileAttrib = (if (fileName == "") Null else Attribute(XMLUtils.INT_PREFIX, "file", Text(fileName), Null))
    val nodeWithScope = (topLevelAnnotations ++ contentElements).head.asInstanceOf[Elem]
    val scopeToInherit = nodeWithScope.scope
    val schemaNode = {
      (<surroundingElement xmlns={ targetNS } xmlns:xs={ xsdURI } xmlns:xsd={ xsdURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI } xmlns:fn={ fnURI } xmlns:tns={ targetNS } xmlns:ex={ targetNS } xmlns:dafint={ dafintURI }>
         {
           nodeWithScope.copy(child = {
             <xs:schema targetNamespace={ targetNS } elementFormDefault="qualified" attributeFormDefault="unqualified">
               <xs:annotation>
                 <xs:appinfo source={ dfdlURI }>
                   { test1 }
                   { topLevelAnnotations }
                 </xs:appinfo>
               </xs:annotation>
               <!-- No imports needed: XML Catalog gets them 
                       <xsd:import namespace={ xsdURI } schemaLocation="XMLSchema.xsd"/>
                       <xsd:import namespace={ dfdlURI } schemaLocation="DFDL_part3_model.xsd"/> 
                        -->
               { contentElements }
             </xs:schema> % fileAttrib
           })
         }
       </surroundingElement>) \\ "schema"
    }.head.asInstanceOf[Elem]
    val realSchema = schemaNode
    //
    // It is essential to stringify and then reload the above schema because the
    // pieces being spliced in don't have the namespace definitions for the prefixes.
    // This massively reduces clutter for creation of test schemas in tests.
    //
    // Writing it out to a string, and reloading
    // forces reinterpretation of all the prefixes as new nodes are created. The 
    // enclosing nodes created above have the namespace definitions.
    // 
    // TODO: Consider: we may not need to do this anymore, given that all schemas
    // are placed into files (tmp files if they started out as Node objects)
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
