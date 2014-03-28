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
import java.io.File
import java.io.FileNotFoundException
import edu.illinois.ncsa.daffodil.Implicits._

/*
 * This is not a file of tests.
 * 
 * These are utilities to support unit testing schemas
 */
object SchemaUtils {

  /**
   * utility to create test schemas without having to repeat all the namespace definitions,
   * and the appinfo boilerplate. This makes tests more uniform.
   *
   * Defines standard named formats that can be re-used by tests. This helps with the issue
   * of having to add properties everywhere when a new property starts being expected.
   */

  /**
   * Constructs a DFDL schema more conveniently than having to specify all those xmlns attributes.
   */
  def dfdlTestSchema(topLevelAnnotations: Seq[Node], contentElements: Seq[Node], fileName: String = "") = {
    val fileAttrib = (if (fileName == "") Null else Attribute(XMLUtils.INT_PREFIX, "file", Text(fileName), Null))
    val nodeWithScope = (topLevelAnnotations ++ contentElements).head.asInstanceOf[Elem]
    val scopeToInherit = nodeWithScope.scope
    val schemaNode = {
      (<surroundingElement xmlns={ targetNS } xmlns:xs={ xsdURI } xmlns:xsd={ xsdURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI } xmlns:fn={ fnURI } xmlns:tns={ targetNS } xmlns:ex={ targetNS } xmlns:dafint={ dafintURI } >
         {
           nodeWithScope.copy(child = {
             <xs:schema targetNamespace={ targetNS } elementFormDefault="qualified" attributeFormDefault="unqualified">
               <xs:include schemaLocation="xsd/built-in-formats.xsd"/>
               <xs:annotation>
                 <xs:appinfo source={ dfdlAppinfoSource }>
                   { topLevelAnnotations }
                 </xs:appinfo>
               </xs:annotation>
               { contentElements }
             </xs:schema> % fileAttrib
           })
         }
       </surroundingElement>) \\ "schema"
    }.head.asInstanceOf[Elem]
    val realSchema = schemaNode
    //
    // Note: no longer needs to write out and re-load to get namespaces 
    // right due to the surroundingElement trick above
    //
    schemaNode
  }
  
  /**
   * Constructs a DFDL schema more conveniently than having to specify all those xmlns attributes.
   */
  def dfdlTestSchemaWithTarget(topLevelAnnotations: Seq[Node], contentElements: Seq[Node], theTargetNS: String) = {
    val fileAttrib = Null
    val nodeWithScope = (topLevelAnnotations ++ contentElements).head.asInstanceOf[Elem]
    val scopeToInherit = nodeWithScope.scope
    val schemaNode = {
      (<surroundingElement xmlns={ targetNS } xmlns:xs={ xsdURI } xmlns:xsd={ xsdURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI } xmlns:fn={ fnURI } xmlns:tns={ targetNS } xmlns:ex={ targetNS } xmlns:dafint={ dafintURI } >
         {
           nodeWithScope.copy(child = {
             <xs:schema targetNamespace={ theTargetNS } elementFormDefault="qualified" attributeFormDefault="unqualified">
                <xs:include schemaLocation="xsd/built-in-formats.xsd"/>
                <xs:annotation>
                 <xs:appinfo source={ dfdlAppinfoSource }>
                   { topLevelAnnotations }
                 </xs:appinfo>
               </xs:annotation>
               { contentElements }
             </xs:schema> % fileAttrib
           })
         }
       </surroundingElement>) \\ "schema"
    }.head.asInstanceOf[Elem]
    val realSchema = schemaNode
    //
    // Note: no longer needs to write out and re-load to get namespaces 
    // right due to the surroundingElement trick above
    //
    schemaNode
  }

}
