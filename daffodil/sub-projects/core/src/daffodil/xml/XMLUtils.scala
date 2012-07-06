package daffodil.xml

import java.io.FileInputStream
import java.io.File
import java.io.InputStream
import org.jdom.input.SAXBuilder
import scala.collection.JavaConversions._
import scala.xml._
import java.io.{OutputStream, PrintWriter, StringWriter}
import java.lang.management._
import java.util.regex.Pattern
import org.jdom.Attribute
import org.jdom.Element
import org.jdom.Parent
import org.jdom.Document
import org.jdom.Namespace
import org.jdom.output.XMLOutputter
import org.jdom.output.Format
import daffodil.processors.VariableMap
import daffodil.processors.xpath.NodeResult
import daffodil.processors.xpath.StringResult
import daffodil.processors.xpath.XPathUtil
import daffodil.schema.annotation._
import daffodil.debugger.DebugUtil
import scala.collection.mutable.LinkedList
import scala.xml.MetaData
import daffodil.exceptions._
import daffodil.dsom._
import java.io.StringReader
import daffodil.util.Misc
import daffodil.util.Validator

/**
 * Utilities for handling XML 
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
object XMLUtils {

//  val MAX_MEMORY_PERCENTAGE = 0.90
//  var WARNING_MEMORY_PERCENTAGE = 0.8
//  var nodeCount:Long = 0

  val XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema" // removed trailing slash (namespaces care)
  val XSI_NAMESPACE = "http://www.w3.org/2001/XMLSchema-instance"
  val DFDL_NAMESPACE = "http://www.ogf.org/dfdl/dfdl-1.0/" // dfdl ns does have a trailing slash
  val DFDL_SUBSET_NAMESPACE = "http://www.ogf.org/dfdl/dfdl-1.0/XMLSchemaSubset" 
  val TDML_NAMESPACE = "http://www.ibm.com/xmlns/dfdl/testData"
  val DFDL_XMLSCHEMASUBSET_NAMESPACE = "http://www.ogf.org/dfdl/dfdl-1.0/XMLSchemaSubset"
  val EXAMPLE_NAMESPACE = "http://example.com"
  
  /**
   * This namespace for extensions above and beyond DFDL v1.0  
   */
  val DFDL_EXTENSIONS_NAMESPACE = "http://www.dataiti.com/dfdl/dfdl-1.0/extensions"
  
  // shorter forms, to make constructing XML literals,... make the lines shorter.
  val DFDLSubsetURI = DFDL_SUBSET_NAMESPACE 
  val xsdURI = XSD_NAMESPACE
  val dfdlURI = DFDL_NAMESPACE
  val targetNS = EXAMPLE_NAMESPACE // we use this for tests.
  val xsiURI = XSI_NAMESPACE
  
  
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
//  val XSD_STRING = XSD_NAMESPACE+"/"+"string"
//  val XSD_FLOAT = XSD_NAMESPACE+"/"+"float"
//  val XSD_DOUBLE = XSD_NAMESPACE+"/"+"double"
//  val XSD_DECIMAL = XSD_NAMESPACE+"/"+"decimal"
//  val XSD_INTEGER = XSD_NAMESPACE+"/"+"integer"
//  val XSD_LONG = XSD_NAMESPACE+"/"+"long"
//  val XSD_INT = XSD_NAMESPACE+"/"+"int"
//  val XSD_SHORT = XSD_NAMESPACE+"/"+"short"
//  val XSD_BYTE = XSD_NAMESPACE+"/"+"byte"
//  val XSD_UNSIGNED_LONG = XSD_NAMESPACE+"/"+"unsignedLong"
//  val XSD_UNSIGNED_INT = XSD_NAMESPACE+"/"+"unsignedInt"
//  val XSD_NON_NEGATIVE_INTEGER = XSD_NAMESPACE+"/"+"nonNegativeInteger"
//  val XSD_UNSIGNED_SHORT = XSD_NAMESPACE+"/"+"unsignedShort"
//  val XSD_UNSIGNED_BYTE = XSD_NAMESPACE+"/"+"unsignedByte"
//  val XSD_BOOLEAN = XSD_NAMESPACE+"/"+"boolean"
//  val XSD_DATE = XSD_NAMESPACE+"/"+"date"
//  val XSD_TIME = XSD_NAMESPACE+"/"+"time"
//  val XSD_DATE_TIME = XSD_NAMESPACE+"/"+"dateTime"
//  val XSD_HEX_BINARY = XSD_NAMESPACE+"/"+"hexBinary"
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

  def slashify (s : String) : String = if (s == "" || s.endsWith("/")) s else s + "/"
  
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
//  def serialize(out:OutputStream,parent:Parent) = {
//    val format = Format getPrettyFormat;
//    format setLineSeparator(System.getProperty("line.separator"))
//    parent match {
//      case d:Document => new XMLOutputter(format).output(d,out)
//      case e:Element =>  new XMLOutputter(format).output(e,out)
//    }
//    out.write(System.getProperty("line.separator").getBytes())
//    //new PrintWriter(out).println()
//  }
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

  /**
   * super inefficient, but useful for unit tests
   */
  def element2Elem(jElem: Element): scala.xml.Node = {
    return scala.xml.XML.loadString(new org.jdom.output.XMLOutputter().outputString(jElem))
  }

  def elem2Element(nodes:scala.xml.NodeSeq):Seq[Element] = nodes.map{elem=>elem2Element(elem)}

  def elem2Element(node: scala.xml.Node): Element = {
    // val jdomNode = new CompressableElement(node label,node namespace)
    val jdomNode = new Element(node.label, node.prefix, node.namespace)

    val attribs = node.attributes.map { (attribute: MetaData) =>
      {
        // for(attribute <- attribs) {
        val attrNS = attribute getNamespace (node)
        val name = attribute key
        val value = attribute.value.apply(0).text
        val prefixedKey = attribute.prefixedKey
        val prefix = if (prefixedKey.contains(":")) prefixedKey.split(":")(0) else ""
        val ns = Namespace getNamespace (prefix, attrNS)
        if (attribute.isPrefixed && attrNS != "") {
          println("THE ATTRIBUTE IS: " + name)
          println("THE NAMESPACE SHOULD BE: " + attrNS)
          println("IT ACTUALLY IS:" + Namespace.getNamespace(name, attrNS))

          // jdomNode setAttribute (name, value, ns)
          new Attribute(name, value, ns)
        } else
          // jdomNode setAttribute (name, value)
          new Attribute(name, value)
      }
    }
    jdomNode.setAttributes(attribs)
    for (child <- node child) {
      child label match {
        case "#PCDATA" => jdomNode addContent (child toString)
        case "#REM" =>
        case _ => jdomNode addContent (elem2Element(child))
      }
    }
    jdomNode
  }
 

//  private def map(c:Char) = c match {
//    case 's' => ' '
//    case 't' => '\t'
//    case 'b' => '\b'
//    case 'n' => '\n'
//    case 'r' => '\r'
//    case 'f' => '\f'
//    case '\'' => '\''
//    case '"' => '\"'
//    case '\\' => '\\'
//  }

//  def getTargetNamespace(name:String,target:String,namespaces:Namespaces):Namespace = {
//    if (name contains(":")){
//      val parts = name split(":")
//      namespaces getNamespaceByPrefix(parts(0)) match {
//        case Some(n) => n
//        case None => throw new DFDLSchemaDefinitionException("undefined prefix "+parts(0))
//      }
//    }else{
//      namespaces addNamespace(target,"")
//      namespaces.getNamespaceByPrefix("").get
//    }
//  }

  def getNamespaces(node:Element) = {
    val namespaces = new Namespaces
    namespaces addNamespaces (node)
    namespaces
  }

  def getNamespaces(node:Element,targetNamespace:String) = {
    val namespaces = new Namespaces
    namespaces addNamespaces (node)
    namespaces addNamespace (targetNamespace,null)
    namespaces
  }
  
//  def getListFromValue(value:String):AttributeValue =
//    if (XPathUtil isExpression(value))
//      new ExpressionValue(value)
//    else
//      new ListLiteralValue(AnnotationParser.separate(value).map { AnnotationParser.unescape(_)})
//
//  def getListFromExpression(expression:ExpressionValue,variables:VariableMap,
//                            element:Parent,namespaces:Namespaces):List[Regex] =
//    expression match {
//      case ExpressionValue(e) =>
//        XPathUtil.evalExpression(XPathUtil.getExpression(e),variables,element,namespaces) match {
//          case StringResult(s) => AnnotationParser.separate(s).filter { _.length > 0 }. map { AnnotationParser unescape(_) }
//          case NodeResult(n) => AnnotationParser.separate(n toString) .filter { _.length > 0 } map { AnnotationParser unescape(_) }
//        }
//    }
//

//  private def getCompressablePath(node:Parent,path:List[CompressableNode]):List[CompressableNode] = {
//    node getParent match {
//      case e:CompressableNode => getCompressablePath(e,e::path)
//      case _ => path
//    }
//  }
//
//  private def getCompressableRoot(parent:Parent):CompressableNode =
//    parent getParent match {
//      case e:CompressableNode => getCompressableRoot(e)
//      case _ => parent match {
//        case e:CompressableNode => e
//        case _ => throw new IllegalArgumentException("No compressable root for this node")
//      }
//    }
//
//
//  private def compress(path:List[CompressableNode]) = {
//    for(node <- path)
//      node match {
//        case p:CompressableElement =>
//          if (! p.isCompressed)
//            for(child <- p.getChildren)
//               if (! path.contains(child))
//                 child.asInstanceOf[CompressableNode].compress
//        case _ => 
//      }
//  }

//  def checkMemory(parent:Parent) = {
//    if (nodeCount%100000 == 0 )
//      DebugUtil log("Node count = "+nodeCount)
//
//    if(shouldCompress){
//      DebugUtil time ("Compressing",{
//        val path = getCompressablePath(parent,Nil)
//        compress(path)
//        if (path.length == 0){
//	        DebugUtil.log("Empty path "+path)
//        }else{
//          shouldCompress = false
//          hasCompressed = true
//        }
//      })
//
//      MemoryWarningSystem gc
//      
//      val mem = MemoryWarningSystem getMemoryUsage
//
//      DebugUtil log("Serialization completed ("+MemoryWarningSystem.toMb(mem.getUsed)+"/"+
//              MemoryWarningSystem.toMb(mem.getMax)+")")
//
//      WARNING_MEMORY_PERCENTAGE += 0.025
//      if (WARNING_MEMORY_PERCENTAGE>MAX_MEMORY_PERCENTAGE)
//      	WARNING_MEMORY_PERCENTAGE = MAX_MEMORY_PERCENTAGE
//      MemoryWarningSystem setPercentageUsageThreshold(WARNING_MEMORY_PERCENTAGE)
//    }
//  }
//
//  MemoryWarningSystem setPercentageUsageThreshold(WARNING_MEMORY_PERCENTAGE)
//  MemoryWarningSystem addListener { (used:Long,max:Long) =>
//    shouldCompress = true;
//   	DebugUtil log("Low memory condititon dectected ("+MemoryWarningSystem.toMb(used)+"/"+
//             MemoryWarningSystem.toMb(max)+").") }

    
//  def getListFromExpression(expression:String,variables:VariableMap,
//                            element:Parent,namespaces:Namespaces):List[String] =
//    if (expression!=null)
//      if (XPathUtil isExpression(expression))
//        XPathUtil.evalExpression(expression,variables,element,namespaces) match {
//          case StringResult(s) => separate(s) filter { _.length > 0 }
//          case NodeResult(n) => separate(n toString) .filter { _.length > 0 }
//        }
//      else
//        separate(expression)
//    else
//      List()
  
import xml.transform.{RuleTransformer, RewriteRule}
import xml.{NodeSeq, Node, Elem}
import xml.Utility.trim

/**
 * Removes attributes, and also element prefixes.
 */
 private class RemoveAttributes extends RewriteRule {
      override def transform(n: Node) = n match {
          case e @ Elem(prefix, label, attributes, scope, children @ _*) => {
            val childrenWithoutAttributes : NodeSeq = children.map{myRule.transform(_)(0)}
            Elem(null, label, noAttributes, scope, childrenWithoutAttributes : _*)
          }
          case other => other
      }
  }

  private val noAttributes = Null
  private val myRule = new RuleTransformer(new RemoveAttributes)

  def removeAttributes(node : Node) : Node = {
      val nseq = myRule.transform(trim(node))
      val res = nseq(0)
      res
  }
  
  
   /**
   * computes a precise difference list
   */
  def computeDiff(a : Node, b : Node) = {
    computeDiffPath(a, b, Nil)
  }
  def computeDiffSeqPath(as : Seq[Node], bs : Seq[Node], path : Seq[String]) : Seq[(String, String, String)] = {
    lazy val zPath = path.reverse.mkString("/")
    (as, bs) match {
      case (a :: ars, b :: brs) => {
        val diffAB = computeDiffPath(a, b, path)
        val diffRest = computeDiffSeqPath(ars, brs, path)
        val res = diffAB ++ diffRest
        res
      }
      case (Nil, Nil) => Nil
      // case (_, Nil) | (Nil, _) => List((zPath, as.toString, bs.toString))
      case _ => List((zPath, as.toString, bs.toString))
    }
  }
  
  def computeDiffPath(a : Node, b : Node, path : Seq[String]) : Seq[(String, String, String)] = {
    lazy val zPath = path.reverse.mkString("/")
    (a, b) match {
      case (Elem(_, labelA, _, _, childrenA @ _*), Elem(_, labelB, _, _, childrenB @ _*)) => {
        if (labelA != labelB) List((zPath, a.toString, b.toString))
        else {
          if (childrenA.length == childrenB.length) {
          // recurse into children of Elem
          val z = childrenA zip childrenB
          val res = z.flatMap{ case (a, b) => {
            computeDiffPath(a, b, labelA +: path)
            }
          }
          res
          }
          else {
            // different lengths, so they're different, but where?
            computeDiffSeqPath(childrenA, childrenB, labelA +: path)
          }
        }
      }
      case (tA : Text, tB : Text) => {
        val dataA = tA.toString
        val dataB = tB.toString
        def quoteIt(str : String) = "'" + str + "'"
        if (dataA == dataB) Nil
        else if (dataA.length != dataB.length) {
            List((zPath, quoteIt(dataA), quoteIt(dataB)))
        }
        else {
          val ints = Stream.from(0).map{_.toString}
          val z = dataA zip dataB zip ints
          val res = z.flatMap{
            case ((a1, b1), index) =>
              if (a1 == b1) Nil 
              else {
                val indexPath = zPath + "[" + index + "]"
                List((indexPath, a1.toString, b1.toString))
              }
          }
          res
        }
      }
      case _ => {
        List((zPath, a.toString, b.toString))
      }
    }
  }

  
    /**
   * Translates a qualified name into a pair of a namespace uri, and a local name part.
   * 
   * Currently makes an effort to take unqualified names into the targetNamespace of the schema,
   */
  def QName(xml : Node, nom: String, sd : daffodil.dsom.SchemaDocument): (String, String) = {
    val parts = nom.split(":").toList
    val (prefix, localName) = parts match {
      case List(local) => ("", local)
      case List(pre, local) => (pre, local)
      case _ => Assert.impossibleCase()
    }
    val nsURI = xml.getNamespace(prefix) // should work even when there is no namespace prefix.
    // Assert.schemaDefinition(nsURI != null, "In QName " + typeName + ", the prefix " + prefix + " was not defined.")
    // TODO: accumulate errors, don't just throw on one.
    // TODO: error location for diagnostic purposes. 
    // see: http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
    
    // TODO: Clarify whether we should be tolerant this way, or strict
    val finalURI = if (nsURI == null || nsURI == "") sd.targetNamespace else nsURI
    (finalURI, localName)
  }
}



trait GetAttributesMixin {
  def xml : Node
  
 /**
   * Use to retrieve things that are not format properties.
   */
  def getAttributeRequired(name: String) = {
    getAttributeOption(name) match {
      case None => Assert.schemaDefinitionError("The attribute " + name + " is required.")
      case Some(s) => s
    }
  }

  /**
   * Use to retrieve things that are not format properties.
   */
  def getAttributeOption(name: String): Option[String] = {
    val attrString = (xml \ ("@" + name)).text
    val res = if (attrString == "") None else Some(attrString)
    res
  }
}

object XMLSchemaUtils {
   /**
   * validate a DFDL schema.
   *
   * This validates the XML Schema language subset that DFDL uses, and also all the annotations
   * hung off of it.
   */
  def validateDFDLSchema(doc : Node) = {
    // TODO: should this do something other than throw an exception on a validation error?
    //
    // Users will write DFDL Schemas, using the xs or xsd prefix (usually) bound to the XML Schema namespace,
    // and the dfdl prefix (usually) bound to the DFDL namespace.
    //
    // However, we don't want to validate using the XML Schema for XML Schema (which would be the usual interpretation
    // of validating an XML Schema), instead we want to use the schema for the DFDL Subset of XML Schema.
    //
    // So, the hack here, is we're going to textually substitute the URIs, so that the validator doesn't have to be 
    // modified to do this switch, and we don't have to lie in the DFDL Subset schema, and claim it is realizing the
    // XML Schema URI.
    //
    // However, we should consider whether there is a better way to do this involving either (a) lying and having the
    // DFDL Subset Schema pretend it is the XSD schema, or we can play some catalog tricks perhaps.
    //
    // Also, the way this whole thing finds the necessary schemas is a bit daft. It should look in the jar or files,
    // but it should be using an XML Catalog.
    //
    val docstring = doc.toString()
    val xmlnsURI = "http://www.w3.org/2001/XMLSchema";
    val xsdSubsetURI = "http://www.ogf.org/dfdl/dfdl-1.0/XMLSchemaSubset";
    val docReplaced = docstring.replaceAll(xmlnsURI, xsdSubsetURI)
    val docReader = new StringReader(docReplaced)
    val schemaResource = Misc.getRequiredResource(Validator.dfdlSchemaFileName()).toURI()
    val res = 
      try {
        Validator.validateXMLStream(schemaResource, docReader)
      }
      catch {
        case e : Exception => {
          // System.err.println(e.getMessage())
          // Really useful place for a breakpoint.
          throw e
        }
      }
    res
  }
}
