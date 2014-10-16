package edu.illinois.ncsa.daffodil.xml

/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

import java.io.FileInputStream
import java.io.File
import java.io.InputStream
import org.jdom2.input.SAXBuilder
import scala.collection.JavaConversions._
import scala.xml._
import java.io.{ OutputStream, PrintWriter, StringWriter }
import java.lang.management._
import java.util.regex.Pattern
import scala.collection.mutable.LinkedList
import scala.xml.MetaData
import edu.illinois.ncsa.daffodil.exceptions._
import java.io.StringReader
import edu.illinois.ncsa.daffodil.util.Misc
import javax.xml.namespace.{ QName => JQName }
import javax.xml.XMLConstants
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGHost
import edu.illinois.ncsa.daffodil.dsom._
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuilder

/**
 * Utilities for handling XML
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
object XMLUtils {

  /**
   * We must have xsi prefix bound to the right namespace.
   * That gets enforced elsewhere.
   */
  val xmlNilAttribute = new PrefixedAttribute("xsi", "nil", "true", scala.xml.Null)

  val PositiveInfinity = Double.PositiveInfinity
  val NegativeInfinity = Double.NegativeInfinity
  val NaN = Double.NaN

  /**
   * Legal XML v1.0 chars are #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
   */
  def remapXMLIllegalCharToPUA(c: Char, checkForExistingPUA: Boolean = true): Char = {
    val cInt = c.toInt
    // println("%x".format(cInt))
    val res = cInt match {
      case 0x9 => c
      case 0xA => c
      case 0xD => c
      case _ if (c < 0x20) => (c + 0xE000).toChar
      case _ if (c > 0xD7FF && c < 0xE000) => (c + 0x1000).toChar
      case _ if (c >= 0xE000 && c <= 0xF8FF) => {
        if (checkForExistingPUA)
          Assert.usageError("Pre-existing Private Use Area (PUA) character found in data: '%s'".format(c))
        else c
      }
      case 0xFFFE => 0xF0FE.toChar
      case 0xFFFF => 0xF0FF.toChar
      case _ if (c > 0x10FFFF) => {
        Assert.invariantFailed("Character code beyond U+10FFFF found in data. Codepoint: %s".format(c.toInt))
      }
      case _ => c

    }
    res
  }

  def isLeadingSurrogate(c: Char) = {
    c >= 0xD800 && c <= 0xDBFF
  }

  def isTrailingSurrogate(c: Char) = {
    c >= 0xDC00 && c <= 0xDFFF
  }

  /**
   * Length where a surrogate pair counts as 1 character, not two.
   */
  def uncodeLength(s: String) = {
    // performance note: this might get called a lot. So needs to be fast.
    // it needs to scan the string once, examine each character.
    // using getBytes utf-32 isn't necessarily slow. It might be fine.
    val res = s.getBytes("UTF-32BE").length / 4
    res
  }

  /**
   * Because of surrogate pairs, and the difference between 16-bit string codepoints
   * and real character codes, lots of things that traverse strings need
   * to consider either the codepoint after (if current is a leading surrogate)
   * or codepoint before (if current is a trailing surrogate).
   *
   * This calls a body function with prev, current, next bound to those.
   * For first codepoint prev will be 0. For last codepoint next will be 0.
   *
   * NOTE: This function contains the same algorithm as
   * remapXMLIllegalCharactersToPUA, but is more general and is a bit slower.
   * Any changes made to this function probably need to be incorporated into
   * the other.
   */
  def walkUnicodeString[T](str: String)(bodyFunc: (Char, Char, Char) => T): Seq[T] = {
    val len = str.length
    if (len == 0) return Nil

    val list = new scala.collection.mutable.ListBuffer[T]

    var pos = 0;
    var prev = 0.toChar
    var curr = str(0)
    var next = 0.toChar

    while (pos < len) {
      next = if (pos + 1 < len) str(pos + 1) else 0.toChar
      list += bodyFunc(prev, curr, next)
      prev = curr
      curr = next

      pos += 1
    }
    list
  }

  /*
   * This function contains the same string traversal algorithm as
   * walkUnicodeString. The only difference is that it uses a StringBuilder
   * rather than a ListBuffer[T] that would be used in walkUnicodeString. Note
   * that since StringBuilder is not synchronized it is noticably faster than
   * StringBuffer, and since the StringBuilder is local to the function, we
   * don't have to worry about any threading issues. This specificity makes for
   * a noticable speed increase, so much so that the code duplication is worth
   * it. Any changes made to this function probably need to be incorporated
   * into the other.
   */
  def remapXMLIllegalCharactersToPUA(dfdlString: String): String = {
    // we want to remap XML-illegal characters
    // but leave legal surrogate-pair character pairs alone.
    def remapOneChar(previous: Char, current: Char, next: Char): Char = {
      if (isLeadingSurrogate(current) && isTrailingSurrogate(next)) return current
      if (isTrailingSurrogate(current) && isLeadingSurrogate(previous)) return current
      remapXMLIllegalCharToPUA(current, false)
    }

    val len = dfdlString.length
    if (len == 0) return dfdlString

    val sb = new StringBuilder()

    var pos = 0;
    var prev = 0.toChar
    var curr = dfdlString(0)
    var next = 0.toChar

    while (pos < len) {
      next = if (pos + 1 < len) dfdlString(pos + 1) else 0.toChar
      sb.append(remapOneChar(prev, curr, next))
      prev = curr
      curr = next

      pos += 1
    }

    sb.toString
  }

  def coalesceAdjacentTextNodes(seq: Seq[Node]): Seq[Node] = {
    type Atom = scala.xml.Atom[String]
    if (seq.length == 0) return seq
    if (seq.length == 1) {
      seq(0) match {
        case p: PCData => {
          val txt = new Text(p.text)
          return Seq(txt)
        }
        case t: Text => return seq
        case u: Unparsed => return seq // TODO: are these needed or possible?
        case _ => // fall through to code below. (We need to process children)
      }
    }
    val ab = ArrayBuilder.make[Node]
    var i = 0
    // 
    // invariant: either the tn node is null
    // or the stringbuilder is null or empty
    //
    // They never both have content.
    // 
    var tn: Node = null
    var sb: StringBuilder = null
    def processText = {
      if (tn == null) {
        if (sb != null && sb.length > 0) {
          // we have accumulated text
          // let's output a text node
          // Note that a Text constructor
          // will escapify the text again.
          // We unescaped it
          // when we used .text to get data
          // out of the nodes.
          ab += new Text(sb.toString)
          sb.clear()
        }
      } else {
        // tn not null
        Assert.invariant(sb == null || sb.length == 0)
        ab += tn
        tn = null
      }
    }
    while (i < seq.length) {
      val current = seq(i)
      i = i + 1
      if (current.isInstanceOf[Atom]) {
        if (tn == null) {
          if (sb == null || sb.length == 0) {
            // hold onto this text node. It might be isolated
            tn = current
          } else {
            // accumulate this text
            sb.append(current.text)
          }
        } else {
          if (sb == null) sb = new StringBuilder
          // accumulate both the pending tn text node
          // and this new one we just encountered.
          //
          // Note we use .text here - that unescapifies
          // Which is important since we're putting together
          // things that might be PCData (aka <![CDATA[...]]>
          // We want that stuff gone.
          //
          sb.append(tn.text)
          sb.append(current.text)
          //
          // set tn to null to indicate we're accumulating
          // into the string buffer
          //
          tn = null
        }
      } else {
        // not an atom
        processText // if there is pending text output that first
        ab += current // then the current non-atom node.
      }
    }
    // we fell out of the loop. So
    processText // in case there is text left pending when we hit the end
    ab.result
  }

  val XSD_NAMESPACE = NS("http://www.w3.org/2001/XMLSchema") // removed trailing slash (namespaces care)
  val XSI_NAMESPACE = NS("http://www.w3.org/2001/XMLSchema-instance")
  val XPATH_FUNCTION_NAMESPACE = NS("http://www.w3.org/2005/xpath-functions")
  val DFDL_NAMESPACE = NS("http://www.ogf.org/dfdl/dfdl-1.0/") // dfdl ns does have a trailing slash
  val TDML_NAMESPACE = NS("http://www.ibm.com/xmlns/dfdl/testData")
  val EXAMPLE_NAMESPACE = NS("http://example.com")

  // must manufacture these because in JDOM, attributes have parent pointers up
  // to their enclosing elements.
  // JDOM def nilAttribute() = new org.jdom2.Attribute("nil", "true", XMLUtils.xsiNS)

  //  def wasCheckConstraintsRan(e: Element) = {
  //    ???
  //    //    val checkAttr = e.getAttribute("checkConstraintsRan", XMLUtils.INT_NS_OBJECT)
  //    //    val res =
  //    //      if (checkAttr == null) false
  //    //      else checkAttr.getValue() == "true"
  //    //    res
  //  }

  /**
   * Added to support extensions and proposed future features as part of daffodil.
   *
   * The DFDL standard requires us to keep these out of the primary DFDL namespace, and
   * we really should be using URN-style notation, not http URLs for these.
   * (for why http URLs are a bad idea for these, see:
   * http://www.w3.org/blog/systeam/2008/02/08/w3c_s_excessive_dtd_traffic/ )
   *
   * These definitions must match their XSD counterparts in dafint.xsd and dafext.xsd
   */
  private val DAFFODIL_EXTENSIONS_NAMESPACE_ROOT = "urn:ogf:dfdl:2013:imp:opensource.ncsa.illinois.edu:2012" // TODO: finalize syntax of this URN
  val DAFFODIL_EXTENSION_NAMESPACE = NS(DAFFODIL_EXTENSIONS_NAMESPACE_ROOT + ":ext")
  val DAFFODIL_INTERNAL_NAMESPACE = NS(DAFFODIL_EXTENSIONS_NAMESPACE_ROOT + ":int")
  val EXT_NS = DAFFODIL_EXTENSION_NAMESPACE.toString
  val EXT_PREFIX = "daf"
  val EXT_NS_OBJECT = NS(EXT_PREFIX, EXT_NS)
  val INT_NS = DAFFODIL_INTERNAL_NAMESPACE.toString
  val INT_PREFIX = "dafint"
  val INT_NS_OBJECT = NS(INT_PREFIX, INT_NS)

  val FILE_ATTRIBUTE_NAME = "file"
  val LINE_ATTRIBUTE_NAME = "line"
  val COLUMN_ATTRIBUTE_NAME = "col"

  val CONFIG_NAMESPACE = EXT_NS

  // shorter forms, to make constructing XML literals,... make the lines shorter.
  //  val DFDLSubsetURI = DFDL_SUBSET_NAMESPACE
  val xsdURI = XSD_NAMESPACE
  val dfdlURI = DFDL_NAMESPACE
  val dfdlAppinfoSource = "http://www.ogf.org/dfdl/"
  val targetNS = EXAMPLE_NAMESPACE // we use this for tests.
  val xsiURI = XSI_NAMESPACE
  val fnURI = XPATH_FUNCTION_NAMESPACE
  val dafintURI = DAFFODIL_INTERNAL_NAMESPACE

  //  val PCDATA = "#PCDATA"
  //  val REM = "#REM"

  //  val SCHEMA = XSD_NAMESPACE+"/"+"schema"
  //  val COMPLEX_TYPE = XSD_NAMESPACE+"/"+"complexType"
  //  val SIMPLE_TYPE = XSD_NAMESPACE+"/"+"simpleType"
  //  val GROUP = XSD_NAMESPACE+"/"+"group"
  //  val SEQUENCE = XSD_NAMESPACE+"/"+"sequence"
  //  val ALL = XSD_NAMESPACE+"/"+"all"
  //  val XSD_CHOICE = XSD_NAMESPACE+"/"+"choice"
  //  val ELEMENT = XSD_NAMESPACE+"/"+"element"
  //  val ATTRIBUTE = XSD_NAMESPACE+"/"+"attribute"
  //  val ATTRIBUTE_GROUP = XSD_NAMESPACE+"/"+"attributeGroup"
  //  val ANNOTATION = XSD_NAMESPACE+"/"+"annotation"
  //  val APP_INFO = XSD_NAMESPACE+"/"+"appinfo"
  //
  //  val DFDL_ASSERT = DFDL_NAMESPACE+"assert"
  //  val DFDL_CALENDAR_FORMAT = DFDL_NAMESPACE+"calendarFormat"
  //  val DFDL_CHOICE = DFDL_NAMESPACE+"choice"
  //  val DFDL_DISCRIMINATOR = DFDL_NAMESPACE+"discriminator"
  //  val DFDL_DEFINE_CALENDAR_FORMAT = DFDL_NAMESPACE+"defineCalendarFormat"
  //  val DFDL_DEFINE_FORMAT = DFDL_NAMESPACE+"defineFormat"
  //  val DFDL_DEFINE_ESCAPE_SCHEME = DFDL_NAMESPACE+"defineEscapeScheme"
  //  val DFDL_DEFINE_TEXT_NUMBER_FORMAT = DFDL_NAMESPACE+"defineTextNumberFormat"
  //  val DFDL_DEFINE_VARIABLE = DFDL_NAMESPACE+"defineVariable"
  //  val DFDL_ELEMENT = DFDL_NAMESPACE+"element"
  //  val DFDL_ESCAPE_SCHEMA = DFDL_NAMESPACE+"escapeScheme"
  //  val DFDL_FORMAT = DFDL_NAMESPACE+"format"
  //  val DFDL_GROUP = DFDL_NAMESPACE+"group"
  //  val DFDL_HIDDEN = DFDL_NAMESPACE+"hidden"
  //  val DFDL_NEW_VARIABLE_INSTANCE = DFDL_NAMESPACE+"newVariableInstance"
  //  val DFDL_PROPERTY = DFDL_NAMESPACE+"property"
  //  val DFDL_SET_VARIABLE = DFDL_NAMESPACE+"setVariable"
  //  val DFDL_SEQUENCE = DFDL_NAMESPACE+"sequence"
  //  val DFDL_SIMPLE_TYPE = DFDL_NAMESPACE+"simpleType"
  //  val DFDL_TEXT_NUMBER_FORMAT = DFDL_NAMESPACE+"textNumberFormat"
  //  val DFDL_RECURSIVE = DFDL_NAMESPACE+"recursive"
  //
  //
  //  //XSD data types
  //
  val XSD_STRING = expandedQName(XSD_NAMESPACE, "string")
  val XSD_FLOAT = expandedQName(XSD_NAMESPACE, "float")
  val XSD_DOUBLE = expandedQName(XSD_NAMESPACE, "double")
  val XSD_DECIMAL = expandedQName(XSD_NAMESPACE, "decimal")
  val XSD_INTEGER = expandedQName(XSD_NAMESPACE, "integer")
  val XSD_LONG = expandedQName(XSD_NAMESPACE, "long")
  val XSD_INT = expandedQName(XSD_NAMESPACE, "int")
  val XSD_SHORT = expandedQName(XSD_NAMESPACE, "short")
  val XSD_BYTE = expandedQName(XSD_NAMESPACE, "byte")
  val XSD_UNSIGNED_LONG = expandedQName(XSD_NAMESPACE, "unsignedLong")
  val XSD_UNSIGNED_INT = expandedQName(XSD_NAMESPACE, "unsignedInt")
  val XSD_NON_NEGATIVE_INTEGER = expandedQName(XSD_NAMESPACE, "nonNegativeInteger")
  val XSD_UNSIGNED_SHORT = expandedQName(XSD_NAMESPACE, "unsignedShort")
  val XSD_UNSIGNED_BYTE = expandedQName(XSD_NAMESPACE, "unsignedByte")
  val XSD_BOOLEAN = expandedQName(XSD_NAMESPACE, "boolean")
  val XSD_DATE = expandedQName(XSD_NAMESPACE, "date")
  val XSD_TIME = expandedQName(XSD_NAMESPACE, "time")
  val XSD_DATE_TIME = expandedQName(XSD_NAMESPACE, "dateTime")
  val XSD_HEX_BINARY = expandedQName(XSD_NAMESPACE, "hexBinary")
  //
  val DFDL_SIMPLE_BUILT_IN_TYPES =
    List("string",
      "float",
      "double",
      "decimal",
      "integer",
      "long",
      "int",
      "short",
      "byte",
      "unsignedLong",
      "unsignedInt",
      "nonNegativeInteger",
      "unsignedShort",
      "unsignedByte",
      "boolean",
      "date",
      "time",
      "dateTime",
      "hexBinary")
  //
  //  private val listPattern = Pattern.compile("'([^']*)'|([^'\\s]+)")
  //
  //  private var shouldCompress = false
  //  private var hasCompressed = false

  def slashify(s: String): String = if (s == "" || s.endsWith("/")) s else s + "/"

  //  // TODO: Somehow the term "Full" name has come to mean the short name (no namespace qualification, nor prefix.
  //  def getFullName(e:Element) = e.getName()
  // 
  //  def getFullNameWithNamespace(e:Element) = slashify(e.getNamespaceURI()) + e.getName()
  //  
  //  def getUnqualifiedName(e:Element) = e.getName()
  //  
  //  def getUnqualifiedName(a:Attribute) = a.getName()

  //  def getFullNameWithNamespace(a:Attribute) =
  //    slashify(a.getNamespaceURI()) + a.getName()
  //    
  //  def getFullName(a:Attribute) = a.getName()
  //
  //  def getAttribute(e:Element,name:String):Option[String] =
  //    e getAttributeValue(name) match {
  //      case null => None
  //      case s:String => Some(s)
  //    }
  //
  //  def getFullName(name:String,namespaces:Namespaces) = {
  //    var namespace:String = ""
  //    var localName:String = ""
  //
  //    if (name contains(":")){
  //      val parts = name split(":")
  //      namespace = namespaces getNamespaceURI(parts(0))
  //      localName = parts(1)
  //    }else{
  //      namespace = namespaces getNamespaceURI("")
  //      localName = name
  //    }
  //    slashify(namespace)+localName
  //  }
  //
  //  def getChildren(e:Element):Iterable[Element] = {
  //    val children = e.getChildren
  //    val res = children.asInstanceOf[java.util.List[Element]]
  //    res
  //  }
  //
  //  def getAttributes(e:Element):Iterable[Attribute] = {
  //    e.getAttributes.asInstanceOf[java.util.List[Attribute]]
  //  }
  //
  //  /**
  //   * given a name, finds the named child
  //   * 
  //   * TODO: QName/namespace support - right now it just tries with and without the prefix.
  //   */
  //  def getChildByName(parent:Element,name:String):Element = {
  //    for(child <- getChildren(parent))
  //      getAttribute(child,"name") match {
  //        case Some(s) => {
  //          if (s==name) return child
  //          else {
  //            // try stripping any prefix
  //            val tokens = name.split(":").toList
  //            if (tokens.length == 2) {
  //              val withoutPrefix = tokens(1)
  //              if (s==withoutPrefix) return child
  //            }
  //          }
  //        }
  //        case _ =>
  //      }
  //    throw new DFDLSchemaDefinitionException("Reference not found:"+name,schemaContext = parent)
  //  }
  //
  //  def getChildByTag(parent:Element,tag:String):Option[Element] = {
  //    for(child <- getChildren(parent))
  //      if (getFullNameWithNamespace(child) == tag)
  //        return Some(child)
  //    None
  //  }
  //
  //  def compare(x:Element,y:Element):Boolean = { // FIXME: give good diagnostics. This is used in unit tests.
  //    val children1 = x getChildren
  //    val children2 = y getChildren;
  //    getFullName(x) == getFullName(y) && x.getText() == y.getText() &&
  //            children1.size == children2.size &&
  //            (0 until children1.size).forall { i:Int => compare(children1.get(i).asInstanceOf[Element],
  //              children2.get(i).asInstanceOf[Element]) }
  //  }
  //
  //  def addNewChild(parent:Parent,name:String,target:String,namespaces:Namespaces) = {
  //
  //    // checkMemory(parent)
  //
  //    val namespace = getTargetNamespace(name,target,namespaces)
  //    nodeCount += 1
  //    // val ele = new CompressableElement(name,namespace)
  //    val ele = new Element(name,namespace)
  //
  //    for( ns <- namespaces getNotNullNamespaces)
  //      ele addNamespaceDeclaration(ns)
  //
  //    parent match {
  //      case null => ele
  //      case p:Document => p addContent(ele)
  //      case e:Element => e addContent(ele)
  //    }
  //    ele
  //  }
  //
  //  def addNewChild(parent:Parent,name:String,value:String,target:String,namespaces:Namespaces) = {
  //    val namespace = getTargetNamespace(name,target,namespaces)
  //
  //    // checkMemory(parent)
  //
  //    val ele = element(name,namespace,value)
  //    for( ns <- namespaces getNotNullNamespaces)
  //      ele addNamespaceDeclaration(ns)
  //
  //    parent match {
  //      case null => ele
  //      case p:Document => p addContent(ele)
  //      case e:Element => e addContent(ele)
  //    }
  //    assert (ele.getDocument != null)
  //    ele
  //  }

  //  /*
  //   * public because it is used by tests
  //   */
  //  def element(name:String,text:String) = {
  //    nodeCount += 1
  //    // val ele = new CompressableElement(name)
  //    val ele = new Element(name)
  //    ele addContent(text)
  //    ele
  //  }

  //  private def element(name:String,namespace:Namespace,text:String) = {
  //    nodeCount += 1
  //    // val ele = new CompressableElement(name,namespace)
  //    val ele = new Element(name,namespace)
  //    ele addContent(text)
  //    ele
  //  }
  //
  //  def element(name:String,children:List[Element]):Element = {    
  //    nodeCount += 1
  //    // val node = new CompressableElement(name)
  //    val node = new Element(name)
  //    children foreach { node addContent (_) }
  //    node
  //  }
  //
  //  def removeChild(parent:Parent,child:Element):Unit = {
  //    if (child != null){
  //      val root = getRoot(child)
  //      if (parent != null)
  //        parent removeContent(child)
  //    }
  //  }

  //  def getRoot(child:Parent):Parent =
  //    if (child.getParent != null)
  //      getRoot(child getParent)
  //    else
  //      child
  //
  //
  //  def removeChildren(node:Parent,children:LinkedList[Element]):Unit =
  //    if (node != null && children !=null )
  //      children foreach { node removeContent (_) }
  //
  //
  //  def addChildren(parent:Parent,children:LinkedList[Element]):Unit =
  //    if (parent != null && children !=null )
  //     parent match {
  //      case p:Document => children foreach { p addContent(_) }
  //      case e:Element => children foreach { e addContent(_) }
  //    }
  //  
  //  def parse(is:InputStream):Document = {
  //	val builder = new SAXBuilder()
  //	builder.build(is)
  //  }
  //  
  //  def parse(f:File):Document = {
  //	  parse(new FileInputStream(f))
  //  }
  //
  //  def compactXml(xml: scala.xml.NodeSeq): scala.xml.NodeSeq = {
  //    xml flatMap {
  //      case scala.xml.Elem(prefix, label, attributes, scope, children @ _*) => {
  //        scala.xml.Elem(prefix, label, attributes, scope, children.flatMap(compactXml(_)): _*)
  //      }
  //      case scala.xml.Text(data) => {
  //        if (data.matches("""\s*"""))
  //          scala.xml.NodeSeq.Empty
  //        else
  //          xml
  //      }
  //      case x => x
  //    }
  //  }
  //    
  //  def serialize(parent:Parent) = serializeFormatted(parent, Format.getPrettyFormat())
  //  def serializeCompact(parent : Parent) = serializeFormatted(parent, Format.getCompactFormat())
  //  
  //  def serializeFormatted(parent:Parent, format : Format) = {
  //    val sw = new StringWriter()
  //    format setLineSeparator(System.getProperty("line.separator"))
  //    parent match {
  //      case d:Document => new XMLOutputter(format).output(d,sw)
  //      case e:Element =>  new XMLOutputter(format).output(e,sw)
  //    }
  //    sw write(System.getProperty("line.separator"));
  //    sw toString
  //  }
  //  
  //    def serialize(out: OutputStream, parent: Parent) = {
  //      val format = Format.getPrettyFormat()
  //      format.setLineSeparator(System.getProperty("line.separator"))
  //      parent match {
  //        case d:Document => new XMLOutputter(format).output(d,out)
  //        case e:Element => new XMLOutputter(format).output(e,out)
  //      }
  //      out.write(System.getProperty("line.separator").getBytes())
  //      //new PrintWriter(out).println()
  //    }
  //  
  //  def getTotalNodes = nodeCount
  //
  //  def printContext(element:Element,insert:String,context:Int):String = {
  //    try {
  //      val sw = new StringWriter()
  //      val separator = System.getProperty("line.separator")
  //      val writer = new daffodil.parser.xml.Writer(sw)
  //      val format = Format getPrettyFormat;
  //      format setLineSeparator(separator)
  //      getRoot(element) match {
  //        case document:Document => new daffodil.parser.xml.XMLOutputter(format).output(document,writer)
  //        case child:Element => new daffodil.parser.xml.XMLOutputter(format).output(child,writer)
  //      }
  //      val position = writer getPosition(element)
  //      val level = writer getLevel(element)
  //      val string = sw toString
  //      val indent = format getIndent
  //      var start = string lastIndexOf(separator,position-context)
  //      val stop = string indexOf(separator,position)
  //      var end = string indexOf(separator,position+context)
  //
  //      start = if (start<0) 0 else start
  //      end = if (end<0) string.length else end
  //
  //      val sb = new StringBuilder()
  //      sb append(string.substring(start,stop))
  //      sb append(separator)
  //      for( i <- 0 until level)
  //        sb append(indent)
  //      sb append(insert)
  //      sb append(string.substring(stop,end))
  //
  //      sb toString
  //    }catch {
  //      case e:java.util.NoSuchElementException => "Unknown"
  //      case e:StringIndexOutOfBoundsException => "Unknown"
  //    }
  //  }
  @deprecated("QName functionality is being centralized in daffodil.xml.QName", "2014-08-27")
  def expandedQName(qName: JQName): String = {
    val uri = NS(qName.getNamespaceURI)
    val localName = qName.getLocalPart
    expandedQName(uri, localName)
  }

  @deprecated("QName functionality is being centralized in daffodil.xml.QName", "2014-08-27")
  def expandedQName(uri: NS, localName: String): String = {
    Assert.usage(uri != null)
    Assert.usage(localName != null)
    val nsPart =
      if (uri == NoNamespace) ""
      else "{" + uri + "}"
    val expName = nsPart + localName
    expName
  }

  /**
   * returns null to indicate no prefix mapping that
   * we can use to qualify this name
   */
  @deprecated("QName functionality is being centralized in daffodil.xml.QName", "2014-08-27")
  def expandNCNameToQName(qName: String, xml: Node): String = {
    val pair @ (ns, local) = getQName(qName, xml)
    if (ns != null) expandedQName(ns, local)
    else null
  }

  /**
   * If there is no default namespace (that is, no xmlns="..." at all), then getQName("foo") should return ("", "foo").
   * If there is a default namespace with URI "defNS", then getQName("foo") should return ("defNS", "foo")
   * If there is no namespace definition for prefix bar, then getQName("bar:foo") should return (null, "foo")
   * which indicates that the prefix was unmapped. (Caller will likely issue a error.)
   * If there is a namespace definition for prefix bar of "barNS", then getQName("bar:foo") should return ("barNS", "foo")
   */
  @deprecated("QName functionality is being centralized in daffodil.xml.QName", "2014-08-27")
  def getQName(qName: String, xml: Node): (NS, String) = getQName(qName, xml.scope)
  @deprecated("QName functionality is being centralized in daffodil.xml.QName", "2014-08-27")
  def getQName(qName: String, scope: NamespaceBinding): (NS, String) = {
    val parts = qName.split(":").toList
    val (prefix, localName) = parts match {
      case List(local) => (null, local)
      case List(pre, local) => (pre, local)
      case _ => Assert.impossibleCase()
    }
    val nsURI = scope.getURI(prefix) // should work even when there is no namespace prefix.

    if (nsURI == null && prefix == null)
      (NoNamespace, localName)
    else if (nsURI == null)
      (null, localName) // indicates prefix was unmapped.
    else
      (NS(nsURI), localName)
  }

  val NSFormat = """\{([^\{\}]*)\}(.+)""".r

  /**
   * Specialized getQName function for handling
   * manually specified variables via the CLI.
   *
   * Variables will be of the format:
   *
   * 1. {nsURI}varName=value
   * 2. {}varName=value
   * 3. varName=value
   */
  @deprecated("QName functionality is being centralized in daffodil.xml.QName", "2014-08-27")
  def getQName(name: String): (Option[NS], String) = {
    name match {
      case NSFormat(nsURI, varName) => (Some(NS(nsURI)), varName)
      case _ => (None, name)
    }
  }

  /**
   * super inefficient, but useful for unit tests
   */
  //  def element2Elem(jElem: Element): scala.xml.Node = {
  //    return XML.loadString(new org.jdom2.output.XMLOutputter().outputString(jElem))
  //  }
  //
  //  def element2ElemTDML(jElem: Element): scala.xml.Node = {
  //    val format = org.jdom2.output.Format.getRawFormat().setTextMode(Format.TextMode.PRESERVE) // Literal text preservation
  //    val w = new java.io.StringWriter
  //    val out = new org.jdom2.output.XMLOutputter(format)
  //    out.output(jElem, w)
  //    val res = XML.loadString(w.toString())
  //    return res
  //  }

  /**
   * Annoying, but namespace bindings are never a collection you can process like a normal collection.
   * Instead they are linked by these parent chains.
   */
  def namespaceBindings(nsBinding: NamespaceBinding): Seq[NamespaceBinding] = {
    if (nsBinding == null) Nil
    else {
      val thisOne =
        if (nsBinding.uri != null) List(nsBinding)
        else Nil
      val others = namespaceBindings(nsBinding.parent)
      thisOne ++ others
    }
  }

  //
  //  def namespaceBindings(element: org.jdom2.Element): Seq[org.jdom2.Namespace] = {
  //    if (element == null) Nil
  //    else {
  //      val ans = element.getAdditionalNamespaces.toSeq.asInstanceOf[Seq[org.jdom2.Namespace]]
  //      val thisOne = element.getNamespace()
  //      val parentContribution = element.getParent match {
  //        case parentElem: org.jdom2.Element => namespaceBindings(parentElem)
  //        case _ => Nil
  //      }
  //      val res = thisOne +: (ans ++ parentContribution)
  //      res
  //    }
  //  }

  val CDATAPattern = {
    """(?s)^<!\[CDATA\[(.*?)\]\]>$""".r
  }

  /**
   * Specially interprets text that matches the CDATAPattern so as to preserve
   * it
   *
   * TODO: refactor this and elem2Element(node) since they are only different
   * in a few lines.
   */
  //  def elem2ElementTDML(node: scala.xml.Node): Element = {
  //    val jdomNode = new Element(node.label, node.prefix, node.namespace)
  //    var Elem(_, _, _, nsBinding: NamespaceBinding, _*) = node.asInstanceOf[scala.xml.Elem]
  //
  //    namespaceBindings(nsBinding).foreach { ns =>
  //      {
  //        val prefix = ns.prefix
  //        if (prefix != null & prefix != ""
  //          && jdomNode.getNamespace(prefix) == null)
  //          jdomNode.addNamespaceDeclaration(org.jdom2.Namespace.getNamespace(ns.prefix, ns.uri))
  //      }
  //    }
  //
  //    val attribsList = if (node.attributes == null) Nil else node.attributes
  //
  //    val attribs = attribsList.map { (attribute: MetaData) =>
  //      {
  //        // for(attribute <- attribs) {
  //        val attrNS = attribute.getNamespace(node)
  //        val name = attribute.key
  //        val value = attribute.value.text
  //        val prefixedKey = attribute.prefixedKey
  //        val prefix = if (prefixedKey.contains(":")) prefixedKey.split(":")(0) else ""
  //        val ns = (prefix, attrNS) match {
  //          //
  //          // to make our test cases less cluttered and more compact visually, we're 
  //          // going to specifically allow for an attribute named xsi:nil where xsi prefix
  //          // is NOT defined.
  //          //
  //          case ("xsi", null) | ("xsi", "") => xsiNS
  //          case (_, null) | (_, "") => {
  //            Assert.invariantFailed("attribute with prefix '%s', but no associated namespace".format(prefix))
  //          }
  //          case ("", uri) => Namespace.getNamespace(uri)
  //          case (pre, uri) => Namespace.getNamespace(pre, uri)
  //        }
  //
  //        if (attribute.isPrefixed && attrNS != "") {
  //          //          println("THE ATTRIBUTE IS: " + name)
  //          //          println("THE NAMESPACE SHOULD BE: " + attrNS)
  //          //          println("IT ACTUALLY IS:" + Namespace.getNamespace(name, attrNS))
  //
  //          // jdomNode setAttribute (name, value, ns)
  //          new Attribute(name, value, ns)
  //        } else
  //          // jdomNode setAttribute (name, value)
  //          new Attribute(name, value)
  //      }
  //    }
  //    jdomNode.setAttributes(attribs)
  //    for (child <- node.child) {
  //      child.toString match {
  //        case CDATAPattern(text) => {
  //          jdomNode.addContent(new org.jdom2.CDATA(text))
  //        }
  //        case _ => child.label match {
  //          case "#PCDATA" => jdomNode.addContent(child.toString)
  //          case "#CDATA" => jdomNode.addContent(new org.jdom2.CDATA(child.toString))
  //          case "#REM" =>
  //          case _ => jdomNode.addContent(elem2ElementTDML(child))
  //        }
  //      }
  //
  //    }
  //    jdomNode
  //  }

  /**
   * We don't want to be sensitive to which prefix people bind
   */
  def attributesInNamespace(ns: String, n: Node) = n.attributes filter { _.getNamespace(n) == ns }

  def dfdlAttributes(n: Node) = attributesInNamespace(DFDL_NAMESPACE.toString, n)

  /**
   * Removes nodes marked as hidden
   */
  def removeHiddenElements(ns: NodeSeq): NodeSeq = {
    ns.filter { !isHidden(_) }.map {
      n =>
        n match {
          case e @ Elem(prefix, label, attributes, scope, children @ _*) => {
            val removedChildren = removeHiddenElements(children)
            val newElem = Elem(prefix, label, attributes, scope, true, removedChildren: _*)
            newElem
          }
          case other => other
        }
    }
  }

  def isHidden(n: Node): Boolean = {
    val attr = n.attribute(INT_NS, "hidden")
    val res = attr match {
      case Some(Text(s)) => {
        Assert.usage(s == "true", "hidden attribute should have value true or not be present at all.")
        true
      }
      case None => false
    }
    res
  }

  // private lazy val hiddenNS = org.jdom2.Namespace.getNamespace(INT_NS)
  //
  //  def isHiddenElement(e: org.jdom2.Element): Boolean = {
  //    e.getAttribute("hidden", hiddenNS) match {
  //      case null => false
  //      case attr: Attribute => {
  //        Assert.usage(attr.getValue() == "true", "hidden attribute should have value true or not be present at all.")
  //        true
  //      }
  //    }
  //  }

  /**
   * Copies a JDOM Element and removes all hidden
   * elements from the copy and its children.
   *
   * @param e the Element to copy and remove hidden elements from.
   * @return a copy of Element without hidden elements.
   */
  //  def copyWithoutHiddenElements(e: org.jdom2.Element): org.jdom2.Element = {
  //    val newNode: org.jdom2.Element = e.clone.asInstanceOf[org.jdom2.Element]
  //    removeHiddenElements(newNode)
  //    newNode
  //  }

  /**
   * Removes hidden elements from a JDOM Element and
   * all its children.
   *
   * @param e the Element to remove hidden elements from.
   */
  //  def removeHiddenElements(e: Element): Unit = {
  //    val children = e.getChildren().toList.asInstanceOf[List[Element]]
  //    for (child <- children) {
  //      if (isHiddenElement(child)) {
  //        if (!e.removeContent(child)) Assert.abort("removeHiddenElements - JDOM failed to remove hidden element.")
  //      } else {
  //        removeHiddenElements(child)
  //      }
  //    }
  //  }

  /**
   * Used to collapse the excessive xmlns proliferation.
   *
   * If a local scope has bindings in it that are not in the outer scope
   * then a new local scope is created which extends the outer scope.
   *
   * This algorithm is n^2 (or worse) in the length of the outer binding chain (worst case).
   */
  def combineScopes(local: NamespaceBinding, outer: NamespaceBinding): NamespaceBinding = {
    if (local == TopScope) outer
    else {
      val NamespaceBinding(pre, uri, moreBindings) = local
      val outerURI = outer.getURI(pre)
      if (outerURI == uri) {
        // same binding for this prefix in the outer, so we don't need 
        // this binding from the local scope.
        combineScopes(moreBindings, outer)
      } else if (outerURI == null) {
        // outer lacks a binding for this prefix
        NamespaceBinding(pre, uri, combineScopes(moreBindings, outer))
      } else {
        // outer has a different binding for this prefix.
        // one would hope that we can just put our superceding binding on the
        // front, but you end up with two bindings for the same prefix
        // in the chain ... and things fail
        //
        // The problem this creates is that it un-shares all the sub-structure
        // of the scopes, and so we no longer have contained elements 
        // that share scopes with enclosing parents. That may mean that 
        // lots of xmlns:pre="ns" proliferate again even though they're 
        // unnecessary.
        //
        val outerWithoutDuplicate = removeBindings(NamespaceBinding(pre, uri, TopScope), outer)
        NamespaceBinding(pre, uri, combineScopes(moreBindings, outerWithoutDuplicate))
      }
    }
  }

  /**
   * remove all the binding s
   */
  def removeBindings(nb: NamespaceBinding, scope: NamespaceBinding): NamespaceBinding = {
    if (nb == TopScope) scope
    else if (scope == TopScope) scope
    else {
      val NamespaceBinding(pre, uri, more) = scope
      if (nb.getURI(pre) != null) {
        // the scope has a binding for this prefix
        // so irrespective of the uri, we remove it.
        removeBindings(nb, more)
      } else {
        // no binding, so keep it
        scope.copy(parent = removeBindings(nb, more))
      }
    }
  }

  def combineScopes(prefix: String, ns: NS, outer: NamespaceBinding): NamespaceBinding = {
    if (ns == NoNamespace) {
      outer
    } else {
      val inner = NamespaceBinding(prefix, ns.uri.toString, TopScope)
      combineScopes(inner, outer)
    }
  }

  def collapseScopes(x: Node, outer: NamespaceBinding): Node = {
    x match {
      case Elem(pre, lab, md, scp, child @ _*) => {
        val newScope = combineScopes(scp, outer)
        Elem(pre, lab, md, newScope, true, (child flatMap { ch => collapseScopes(ch, newScope) }): _*)
      }
      case _ => x
    }
  }

  /**
   * Removes NamespaceBindings from a scope containing specified namespaces
   */
  def filterScope(nsb: NamespaceBinding, nss: Seq[NS]): NamespaceBinding = {
    val newHead =
      if (nsb == xml.TopScope) {
        xml.TopScope
      } else {
        val parentCopy = filterScope(nsb.parent, nss)
        if (nss.contains(NS(nsb.uri))) {
          parentCopy
        } else {
          nsb.copy(parent = parentCopy)
        }
      }
    newHead
  }

  /**
   * Determines if a prefix is defined inside a scope
   */
  def prefixInScope(prefix: String, scope: NamespaceBinding): Boolean = {
    val ret =
      if (scope == null) {
        false
      } else if (prefix == scope.prefix) {
        true
      } else {
        prefixInScope(prefix, scope.parent)
      }
    ret
  }

  /**
   * Remove Comments
   */

  def removeComments(e: Node): Node = {
    e match {
      case Elem(prefix, label, attribs, scope, child @ _*) => {
        val newChildren = child.filterNot { _.isInstanceOf[Comment] }.map { removeComments(_) }
        Elem(prefix, label, attribs, scope, true, newChildren: _*)
      }
      case x => x
    }
  }

  /**
   * Removes attributes associated xmlns quasi-attributes.
   *
   * If a sequence of namespaces are given, only those attributes and scopes in
   * those namepsaces are removed. Otherwise, all attributes and scopes (aside
   * from special ones like xsi:nil) are removed. Additionally, if a scope is
   * filtered, the prefixes of elements prefixed with filtered scopes are also
   * removed.
   *
   * If a scope is given, it will be used for a child element if the
   * childs filtered scope is the same as the scope.
   *
   * Also strips out comments.
   */
  def removeAttributes(n: Node, ns: Seq[NS] = Seq[NS](), parentScope: Option[NamespaceBinding] = None): Node =
    removeAttributes1(n, ns, parentScope).asInstanceOf[scala.xml.Node]

  private def removeAttributes1(n: Node, ns: Seq[NS] = Seq[NS](), parentScope: Option[NamespaceBinding] = None): NodeSeq = {
    Utility.escape("")
    val res = n match {
      case e @ Elem(prefix, label, attributes, scope, children @ _*) => {

        val filteredScope = if (ns.length > 0) filterScope(scope, ns) else xml.TopScope

        // If the filtered scope is logically the same as the parent scope, use
        // the parent scope. Scala uses references to determine if scopes are
        // the same during pretty printing. However, scopes are immutable, so
        // the filter algorithm creates new scopes. Because of this, we need to
        // ignore the newly filtered scope if it is logically the same as the
        // parent so that the scala pretty printer doesn't see them as
        // different scopes.
        val newScope = parentScope match {
          case Some(ps) => if (ps == filteredScope) ps else filteredScope
          case None => filteredScope
        }

        val newChildren: NodeSeq = children.flatMap { removeAttributes1(_, ns, Some(newScope)) }

        // Important to merge adjacent text. Otherwise when comparing 
        // two structuers that print out the same, they might not be equal
        // because they have different length lists of text nodes 
        //
        // Ex: <foo>A&#xE000;</foo> creates an element containing TWO
        // text nodes. But coming from the Daffodil Infoset, a string like
        // that would be just one text node. 
        // Similarly <foo>abc<![CDATA[def]]>ghi</foo> has 3 child nodes.
        // The middle one is PCData. The two around it are Text.
        // Both Text and PCData are Atom[String].
        val textMergedChildren = coalesceAdjacentTextNodes(newChildren)

        val newPrefix = if (prefixInScope(prefix, newScope)) prefix else null

        val newAttributes = attributes.filter { m =>
          m match {
            case xsiNilAttr @ PrefixedAttribute(_, "nil", Text("true"), _) if (NS(xsiNilAttr.getNamespace(e)) == XMLUtils.XSI_NAMESPACE) => {
              true
            }
            //
            // This tolerates xsi:nil='true' when xsi has no definition at all.
            case xsiNilAttr @ PrefixedAttribute("xsi", "nil", Text("true"), _) if (xsiNilAttr.getNamespace(e) == null) => {
              true
            }
            case attr => {
              if (ns.length > 0) {
                !ns.contains(NS(attr.getNamespace(e)))
              } else {
                false
              }
            }
          }
        }

        Elem(newPrefix, label, newAttributes, newScope, true, textMergedChildren: _*)
      }
      case c: scala.xml.Comment => NodeSeq.Empty // remove comments
      case other => other
    }
    res
  }

  def compareAndReport(trimmedExpected: Node, actualNoAttrs: Node) = {
    if (trimmedExpected != actualNoAttrs) {
      val diffs = XMLUtils.computeDiff(trimmedExpected, actualNoAttrs)
      if (diffs.length > 0) {
        //throw new Exception("Comparison failed. Expected: " + expected + " but got " + actualNoAttrs)
        throw new Exception("""
Comparison failed.
Expected 
          %s
Actual 
          %s
Differences were (path, expected, actual):
 %s""".format(
          trimmedExpected.toString, actualNoAttrs.toString, diffs.map { _.toString }.mkString("\n")))
      }
    }
  }

  /**
   * computes a precise difference list which is a sequence of triples.
   * Each triple is the path (an x-path-like string), followed by expected, and actual values.
   */
  def computeDiff(a: Node, b: Node) = {
    computeDiffOne(Seq(a), Seq(b), Map.empty, Nil)
  }

  def childArrayCounters(e: Elem) = {
    val Elem(_, _, _, _, children @ _*) = e
    val labels = children.map { _.label }
    val groups = labels.groupBy { x => x }
    val counts = groups.map { case (label, labelList) => (label, labelList.length) }
    val arrayCounts = counts.filter { case (label, 1) => false; case _ => true } // remove counters for scalars
    val arrayCounters = arrayCounts.map { case (label, _) => (label, 1.toLong) } // 1 based like XPath!
    arrayCounters
  }

  def computeDiffOne(as: Seq[Node], bs: Seq[Node],
    aCounters: Map[String, Long],
    path: Seq[String]): Seq[(String, String, String)] = {
    lazy val zPath = path.reverse.mkString("/")
    (as, bs) match {
      case (a1 :: ars, b1 :: brs) if (a1.isInstanceOf[Elem] && b1.isInstanceOf[Elem]) => {
        val (a: Elem, b: Elem) = (a1, b1)
        val Elem(_, labelA, attribsA, _, childrenA @ _*) = a
        val Elem(_, labelB, attribsB, _, childrenB @ _*) = b
        if (labelA != labelB) List((zPath, a.toString, b.toString))
        else if (attribsA != attribsB
          && !((attribsA == null && (attribsB == null || attribsB.length == 0))
            || (attribsB == null) && attribsA.length == 0)) {

          // println("attributes are different")

          val aA = if (attribsA == null || attribsA == "") "null" else attribsA.toString
          val aB = if (attribsB == null || attribsB == "") "null" else attribsB.toString
          List((zPath, aA, aB))
        } else {
          val aIndex = aCounters.get(labelA)
          val aIndexExpr = aIndex.map { n => labelA + "[" + n + "]" }
          val newAIndex = aIndex.map { n => (labelA, n + 1) }
          val newACounters = aCounters ++ newAIndex.toList
          val pathStep = aIndexExpr.getOrElse(labelA)
          val aChildArrayCounters = childArrayCounters(a)
          //
          // Tricky induction here. For the rest of our peers, we must use newACounters
          // But as we move across our children, we're using a new map, aChildArrayCounters.
          //
          val newPath = pathStep +: path
          val childrenAList = childrenA.toList
          val childrenBList = childrenB.toList
          val childrenDiffs =
            computeDiffOne(childrenAList, childrenBList, aChildArrayCounters, newPath)
          val subsequentPeerDiffs = computeDiffOne(ars, brs, newACounters, path)
          val res = childrenDiffs ++ subsequentPeerDiffs
          res
        }
      }
      case (tA1 :: ars, tB1 :: brs) if (tA1.isInstanceOf[Text] && tB1.isInstanceOf[Text]) => {
        val (tA: Text, tB: Text) = (tA1, tB1)
        val thisDiff = computeTextDiff(zPath, tA, tB)
        val restDiffs = computeDiffOne(ars, brs, aCounters, path)
        val res = thisDiff ++ restDiffs
        res
      }
      case (Nil, Nil) => Nil
      case _ => {
        List((zPath, as.toString, bs.toString))
      }
    }
  }

  def computeTextDiff(zPath: String, tA: Text, tB: Text) = {
    val dataA = tA.toString
    val dataB = tB.toString
    def quoteIt(str: String) = "'" + str + "'"
    if (dataA == dataB) Nil
    else if (dataA.length != dataB.length) {
      List((zPath, quoteIt(dataA), quoteIt(dataB)))
    } else {
      val ints = Stream.from(1).map { _.toString }
      val z = dataA zip dataB zip ints
      val res = z.flatMap {
        case ((a1, b1), index) =>
          if (a1 == b1) Nil
          else {
            val indexPath = zPath + ".charAt(" + index + ")"
            List((indexPath, a1.toString, b1.toString))
          }
      }
      res
    }
  }

  /**
   * Translates a qualified name into a pair of a namespace uri, and a local name part.
   *
   * Currently makes an effort to take unqualified names into the targetNamespace of the schema,
   */
  @deprecated("use daffodil.xml.QName object", "2014-08-28")
  def QName(nsBindings: NamespaceBinding, nom: String, loc: LookupLocation): (NS, String) = {
    val parts = nom.split(":").toList
    val (prefix, localName) = parts match {
      case List(local) => (null, local) // use null not "" for no prefix
      case List(pre, local) => (pre, local)
      case _ => Assert.impossibleCase()
    }
    val nsURI = nsBindings.getURI(prefix) // should work even when there is no namespace prefix.
    if (nsURI == null && prefix != null) {
      // no resolution to this non-null prefix
      throw new QNamePrefixNotInScopeException(prefix, loc)
    }
    val finalURI = NS(nsURI)
    (finalURI, localName)
  }

  /**
   * for quick tests, we use literal scala nodes. However, the underlying
   * infrastructure wants to be all file centric for diagnostic-message
   * reasons (line numbers for errors)
   */
  def convertNodeToTempFile(xml: Node) = {
    // Create temp file
    val tmpSchemaFile = File.createTempFile("daffodil_tmp_", ".dfdl.xsd")
    // Delete temp file when program exits
    tmpSchemaFile.deleteOnExit
    val fw = new java.io.FileWriter(tmpSchemaFile)
    fw.write(xml.toString())
    fw.close()
    tmpSchemaFile
  }
}

trait GetAttributesMixin extends ThrowsSDE {
  def xml: Node

  /**
   * Use to retrieve things that are not format properties.
   */
  def getAttributeRequired(name: String) = {
    getAttributeOption(name) match {
      case None => schemaDefinitionError("The attribute '" + name + "' is required.")
      case Some(s) => s
    }
  }

  /**
   * Use to retrieve things that are not format properties.
   */
  def getAttributeOption(name: String): Option[String] = {
    val attrString = xml.attribute(name).map { _.text }
    attrString
  }

  def getAttributeOption(ns: NS, name: String): Option[String] = {
    //
    // Most annoying, but this doesn't work....
    // val res = xml.attribute(ns.toString, name).map{ _.text }
    val attr = (xml \ ("@{" + ns.toString + "}" + name))
    if (attr.length == 0) None
    else Some(attr.text)
  }

  /**
   * For picking off the short-form annotations.
   */
  def attributesInNamespace(ns: String, n: Node) = n.attributes.filter { _.getNamespace(n) == ns }
  def dfdlAttributes(n: Node) = attributesInNamespace(XMLUtils.DFDL_NAMESPACE.toString, n)

}

class QNamePrefixNotInScopeException(pre: String, loc: LookupLocation)
  extends Exception("Prefix " + pre + " not found in scope. Location: " + loc.toString)

// Commented out for now, but we may reactivate this to 
// do more validation stuff in the TDMLRunner. So keeping in the 
// source like this. 
//
//object XMLSchemaUtils {
//  /**
//   * validate a DFDL schema.
//   *
//   * This validates the XML Schema language subset that DFDL uses, and also all the annotations
//   * hung off of it.
//   */
//  def validateDFDLSchema(doc: Node) = {
//    // TODO: should this do something other than throw an exception on a validation error?
//    //
//    // Users will write DFDL Schemas, using the xs or xsd prefix (usually) bound to the XML Schema namespace,
//    // and the dfdl prefix (usually) bound to the DFDL namespace.
//    //
//    // However, we don't want to validate using the XML Schema for XML Schema (which would be the usual interpretation
//    // of validating an XML Schema), instead we want to use the schema for the DFDL Subset of XML Schema.
//    //
//    // So, the hack here, is we're going to textually substitute the URIs, so that the validator doesn't have to be 
//    // modified to do this switch, and we don't have to lie in the DFDL Subset schema, and claim it is realizing the
//    // XML Schema URI.
//    //
//    // However, we should consider whether there is a better way to do this involving either (a) lying and having the
//    // DFDL Subset Schema pretend it is the XSD schema, or we can play some catalog tricks perhaps.
//    //
//    // Also, the way this whole thing finds the necessary schemas is a bit daft. It should look in the jar or files,
//    // but it should be using an XML Catalog.
//    //
//    val docstring = doc.toString()
//    val xmlnsURI = "http://www.w3.org/2001/XMLSchema";
//    val xsdSubsetURI = "http://www.w3.org/2001/XMLSchema";
//    val docReplaced = docstring.replaceAll(xmlnsURI, xsdSubsetURI)
//    val docReader = new StringReader(docReplaced)
//    val schemaResource = Misc.getRequiredResource(Validator.dfdlSchemaFileName()).toURI()
//    val res =
//      try {
//        Validator.validateXMLStream(schemaResource, docReader)
//      } catch {
//        case e => {
//          val exc = e
//          // System.err.println(exc.getMessage())
//          // Really useful place for a breakpoint.
//          throw e
//        }
//      }
//    res
//  }
//}
