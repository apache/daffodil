package edu.illinois.ncsa.daffodil.processors.input

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

import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.xpath.XPathEvaluator
import net.sf.saxon.option.jdom2.JDOM2ObjectModel
import org.xml.sax.InputSource
import javax.xml.namespace.QName
import javax.xml.namespace.NamespaceContext
import javax.xml.transform.sax.SAXSource
import javax.xml.xpath._
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.util.Iterator
import java.util.List
import java.io.ByteArrayInputStream
import java.util.ArrayList
import java.math.BigInteger
import org.jdom2._
import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._

class SaxonWorkingTest extends XPathVariableResolver with NamespaceContext {

  @Test def testTrivialExpression1() {
    val xpf = new net.sf.saxon.xpath.XPathFactoryImpl
    val config = xpf.getConfiguration
    config.registerExternalObjectModel(new JDOM2ObjectModel)
    val xpe = xpf.newXPath();
    // System.err.println("Loaded XPath Provider " + xpe.getClass().getName());

    val document = """<?xml version="1.0"?><FOOBAR/>"""
    val is = new InputSource(new ByteArrayInputStream(document.toString().getBytes("utf-8")));
    val ss = new SAXSource(is);
    val doc = xpe.asInstanceOf[XPathEvaluator].setSource(ss);

    // Declare a variable resolver to return the value of variables used in XPath expressions
    xpe.setXPathVariableResolver(this);

    //xpe.setNamespaceContext(this);

    val compiled = xpe.compile("16 + 26");
    val matchedLines = compiled.evaluate(doc, XPathConstants.NODESET);
    val firstRes = matchedLines.asInstanceOf[ArrayList[BigInteger]].get(0)
    val asInt = firstRes.intValue()
    assertEquals(42, asInt)

  }

  @Test def testTrivialExpression2() {
    val xpf = new net.sf.saxon.xpath.XPathFactoryImpl
    val config = xpf.getConfiguration
    config.registerExternalObjectModel(new JDOM2ObjectModel)
    val xpe = xpf.newXPath();

    val document = new Document() // A JDOM object
    val root = new Element("FOOBAR")
    document.addContent(root)

    xpe.setXPathVariableResolver(this);

    //xpe.setNamespaceContext(this);

    val compiled = xpe.compile("16 + 26");
    // val compiled = xpe.compile("//FOOBAR")
    val matchedLines = compiled.evaluate(document, XPathConstants.NODESET);
    // System.err.println(matchedLines)
    val firstRes = matchedLines.asInstanceOf[ArrayList[BigInteger]].get(0)
    val asInt = firstRes.intValue()
    assertEquals(42, asInt)

  }

  // @Test
  //
  // This test is an attempt to get SAXON/XPath to take heed of xsi:type attributes on JDOM objects
  //
  // It is incomplete. Doesn't show this working. You just get an error because a clearly non-numeric string
  // can't be turned into a double, but this check is just because the addition requires the values to be coerced 
  // to double to do arithmetic.
  //
  //    @Test def testXSITypeCheck1() {
  //        System.setProperty("javax.xml.xpath.XPathFactory:"+NamespaceConstant.OBJECT_MODEL_JDOM,"net.sf.saxon.xpath.XPathFactoryImpl")
  //        val xpf = XPathFactory.newInstance(NamespaceConstant.OBJECT_MODEL_JDOM)
  //        val xpe = xpf.newXPath();          xpe.setXPathVariableResolver(this);
  //
  //         val document = new Document()
  //         val g = XMLUtils.elem2Element(<g  xmlns:xsi={ XMLUtils.XSI_NAMESPACE } xmlns:xs={ XMLUtils.XSD_NAMESPACE }><e1 xsi:type="xs:string">notAnInt</e1><e2 xsi:type="xs:int">2</e2></g>)
  //         document.setRootElement(g)
  //
  //
  //        //xpe.setNamespaceContext(this);
  //
  //        val compiled =  xpe.compile(" /g/e1 + /g/e2 ");
  //        val e = intercept[Exception] {
  //         compiled.evaluate(document, XPathConstants.NODESET);
  //        }
  //        assertNotNull(e)
  //        val msg = e.getMessage()
  //        val m = if (msg == null) e.getCause().getMessage() else msg
  //        println(m)
  //        assertTrue(m.contains("notAnInt"))
  //
  //    }

  /**
   * This class serves as a variable resolver. The only variable used is $word.
   * @param qName the name of the variable required
   * @return the current value of the variable
   */

  def resolveVariable(qName: QName): Object = {
    return null;
  }

  /**
   * This class serves as a namespace context.
   */

  /**
   *Get Namespace URI bound to a prefix in the current scope.</p>
   * @param prefix prefix to look up
   * @return Namespace URI bound to prefix in the current scope
   */

  def getNamespaceURI(prefix: String): String = {
    // System.err.println("Looking up: " + prefix);
    if (prefix.equals("saxon")) {
      return "http://saxon.sf.net/";
    } else {
      return null;
    }
  }

  /**
   *Get prefix bound to Namespace URI in the current scope.</p>
   * @param namespaceURI URI of Namespace to lookup
   * @return prefix bound to Namespace URI in current context
   */

  def getPrefix(namespaceURI: String): String = {
    return null; // not used by Saxon
  }

  def getPrefixes(namespaceURI: String): Iterator[Any] = {
    return null;
  }

}
