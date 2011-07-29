package daffodil.xml

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies 
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its 
 *     contributors may be used to endorse or promote products derived from this 
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/*
 * Copied and modified from original bash GRDDL extractor by
 * Sean B. Palmer at http://inamidst.com/proj/grddl/grddler.sh
 */

/* 
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu > 
 * Date: 2010
 */


import java.io.InputStream
import java.io.OutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.net.URL
import java.util.Scanner

import javax.xml.transform.TransformerFactory
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.stream.StreamResult

import org.jdom.Element
import org.jdom.Namespace

/**
 * Helper object for GRDDL processing
 *
 * @version 1
 * @author Alejandro Rodriguez
 *
 */
object GRDDLUtil {

  /**
   * Injects links to GRDDL transformations into a DOM tree
   *
   * @param root the root of the DOM tree
   * @param transformations the urls of the transformations
   */
  def inject(root:Element,transformations:List[String]) = {
    val ns = Namespace getNamespace("grddl","http://www.w3.org/2003/g/data-view#")
    root addNamespaceDeclaration (ns)
    root setAttribute ("transformation",transformations reduceLeft { _ + " " + _ },ns)
  }

  /**
   * Gleans RDF from a GRDDL-marked XML document
   *
   * @param input the URL of the XML document
   * @param output the output stream where to print the rdf-xml
   */
  def glean(input:URL,output:OutputStream):Unit = {
    val getter = "http://inamidst.com/proj/grddl/getTransforms.xsl"

    val buffer = new ByteArrayOutputStream

    transform(input.openStream,buffer,new URL(getter).openStream)

    val s = new String(buffer .toByteArray)

    val scanner = new Scanner(s)

    var transformations = List[String]()

    while(scanner.hasNext) {
      val url = scanner.nextLine
      if (url.startsWith("T "))
        transformations = url.substring(2) :: transformations
    }

    println(transformations)
    
    glean(input,transformations reverse,output)
  }

  /**
   * Gleans RDF from an XML document, with or without GRDDL annotations
   *
   * @param input the URL of the XML document
   * @param transformations the GRDDL transformations to run on the document
   * @param output the output stream where to print the rdf-xml
   */
  def glean(input:URL,transformations:List[String],output:OutputStream):Unit = {

    var outputs = List[File]()

    for(trans <- transformations){
      val out = File.createTempFile("Daffodil","GRDDLOut")
      val transformURL = new URL(input,trans)

      transform(input.openStream,new FileOutputStream(out),transformURL.openStream)
      outputs = out :: outputs
    }

    outputs = outputs reverse
    val sb = new StringBuffer

    sb append("""<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
          <xsl:template match="/">
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">""")

    for(file <- outputs){
      sb append("<xsl:for-each select=\"document('"+file.getAbsolutePath+"')/rdf:RDF/*\">\n")
      sb append("<xsl:copy-of select=\".\"/>\n")
      sb append("</xsl:for-each>\n")
    }

    sb append("\n</rdf:RDF></xsl:template></xsl:stylesheet>")

    transform(new ByteArrayInputStream(sb.toString.getBytes),
                       output,
                       new ByteArrayInputStream(sb.toString.getBytes))

    for(file <- outputs)
      file delete
  }

  /**
   * Runs an XSLT transformation on an XML document
   *
   * @param input the XML document
   * @param output the stream where to print the result
   * @param xslt the XSLT transformation to run
   */
  def transform(input:InputStream,output:OutputStream,xslt:InputStream) = {
    val xmlSource = new StreamSource(input)
    val xsltSource = new StreamSource(xslt)

    val transFact = TransformerFactory.newInstance()
    val trans = transFact.newTransformer(xsltSource)

    trans.transform(xmlSource, new StreamResult(output))
  }

}
