package edu.illinois.ncsa.daffodil.dsom

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

import edu.illinois.ncsa.daffodil.util.Misc
import java.net.URL
import scala.xml.Node
import scala.xml.Elem
import scala.xml.Attribute
import scala.xml.Null
import scala.collection.immutable.ListMap
import org.apache.xerces.util.XMLResourceIdentifierImpl
import java.io.IOException
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.Info
import IIUtils._
import java.io.File
import java.net.URI
import scala.xml.NodeSeq
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG
import edu.illinois.ncsa.daffodil.util.Delay
import java.net.URLDecoder
import java.net.URLEncoder

/**
 * Mixin for all SchemaComponents
 */

trait SchemaComponentIncludesAndImportsMixin { self: SchemaComponent =>

  lazy val targetNamespace: NS = targetNamespace_.value
  private val targetNamespace_ = LV('targetNamespace) {
    val res = xmlSchemaDocument.targetNamespace
    res
  }

  lazy val targetNamespacePrefix = xml.scope.getPrefix(targetNamespace.toString)
  
  val orElseURL: String = "file:??"

  /**
   * Used in diagnostic messages; hence, valueOrElse to avoid
   * problems when this can't get a value due to an error.
   */
  lazy val fileName: String = fileName_.valueOrElse(orElseURL)
  private val fileName_ = LV('fileName) {
    xmlSchemaDocument.fileName
  }

}

