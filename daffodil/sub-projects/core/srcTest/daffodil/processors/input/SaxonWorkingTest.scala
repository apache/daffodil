package daffodil.processors.input
import net.sf.saxon.om.NamespaceConstant
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.xpath.XPathEvaluator
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
import org.jdom._


/**
 * Test that Saxon XPath is working the way we need it to. Good test to have in case we upgrade to a new version of Saxon.
 */

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

class SaxonWorkingTest extends JUnit3Suite with XPathVariableResolver with NamespaceContext {
 
   // @Test
    def testTrivialExpression1() {
        val xpf = XPathFactory.newInstance(NamespaceConstant.OBJECT_MODEL_SAXON);
        val xpe = xpf.newXPath();
        System.err.println("Loaded XPath Provider " + xpe.getClass().getName());

        val document = """<?xml version="1.0"?><FOOBAR/>"""
        val is = new InputSource(new ByteArrayInputStream(document.toString().getBytes("utf-8")));
        val ss = new SAXSource(is);
        val doc = xpe.asInstanceOf[XPathEvaluator].setSource(ss);

        // Declare a variable resolver to return the value of variables used in XPath expressions
        xpe.setXPathVariableResolver(this);

        //xpe.setNamespaceContext(this);

        val compiled =  xpe.compile("16 + 26");
        val matchedLines = compiled.evaluate(doc, XPathConstants.NODESET);
        val firstRes = matchedLines.asInstanceOf[ArrayList[BigInteger]].get(0)
        val asInt = firstRes.intValue()
        assertEquals(42, asInt)
  
    }

    // @Test
    def testTrivialExpression2() {
        System.setProperty("javax.xml.xpath.XPathFactory:"+NamespaceConstant.OBJECT_MODEL_JDOM,"net.sf.saxon.xpath.XPathFactoryImpl")
        val xpf = XPathFactory.newInstance(NamespaceConstant.OBJECT_MODEL_JDOM)
        val xpe = xpf.newXPath();

        val document = new Document()   // A JDOM object
        val root = new Element("FOOBAR")
        document.addContent(root)

        xpe.setXPathVariableResolver(this);

        //xpe.setNamespaceContext(this);

        val compiled =  xpe.compile("16 + 26");
        // val compiled = xpe.compile("//FOOBAR")
        val matchedLines = compiled.evaluate(document, XPathConstants.NODESET);
        System.err.println(matchedLines)
        val firstRes = matchedLines.asInstanceOf[ArrayList[BigInteger]].get(0)
        val asInt = firstRes.intValue()
        assertEquals(42, asInt)
  
    }

    /**
     * This class serves as a variable resolver. The only variable used is $word.
     * @param qName the name of the variable required
     * @return the current value of the variable
     */

    def resolveVariable(qName : QName) :Object = {
            return null;
    }

    /**
     * This class serves as a namespace context.
     */

    /**
     * <p>Get Namespace URI bound to a prefix in the current scope.</p>
     * @param prefix prefix to look up
     * @return Namespace URI bound to prefix in the current scope
     */

    def getNamespaceURI(prefix : String) : String = {
        System.err.println("Looking up: " + prefix);
        if (prefix.equals("saxon")) {
            return "http://saxon.sf.net/";
        } else {
            return null;
        }
    }

    /**
     * <p>Get prefix bound to Namespace URI in the current scope.</p>
     * @param namespaceURI URI of Namespace to lookup
     * @return prefix bound to Namespace URI in current context
     */

    def getPrefix(namespaceURI : String) :String = {
        return null;  // not used by Saxon
    }

    def getPrefixes(namespaceURI : String) : Iterator[Any] = {
      return null;
    }

}