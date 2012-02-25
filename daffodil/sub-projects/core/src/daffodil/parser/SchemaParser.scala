package daffodil.parser

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
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 */

import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.nio.ByteBuffer

import scala.collection.mutable.Map

import org.jdom.input.JDOMParseException
import org.jdom.input.SAXBuilder
import org.jdom.Document
import org.jdom.Element

import daffodil.debugger.BasicTextDebugger
import daffodil.exceptions.DFDLDisallowConstructException
import daffodil.exceptions.DFDLReservedKeywordException
import daffodil.exceptions.DFDLSchemaDefinitionException
import daffodil.exceptions.MalformedXMLException
import daffodil.exceptions.UnimplementedException
import daffodil.processors.ProcessorFactory
import daffodil.processors.VariableMap
import daffodil.schema.annotation.Annotation
import daffodil.schema.annotation.Format
import daffodil.schema.annotation.Hidden
import daffodil.schema.BasicNode
import daffodil.schema.BasicNodeImpl
import daffodil.schema.Choice
import daffodil.schema.ComplexElement
import daffodil.schema.ComplexType
import daffodil.schema.DebuggingBasicNode
import daffodil.schema.ReferenceNode
import daffodil.schema.Sequence
import daffodil.schema.SimpleElement
import daffodil.schema.SimpleExtendedElement
import daffodil.schema.SimpleType
import daffodil.util.GrowableByteBuffer
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil

/**
 * Parses a schema and creates a set of document parsers that will parse a conforming document.
 * Objects of these class take a DFDL schema and produce a parsing function for each top level element in the schema.
 *
 * The parsers created are of type BasicNode.
 *
 * @see daffodil.schema.BasicNode
 *
 * @author Alejandro Rodriguez
 * @version 1 
 */
@SerialVersionUID(1)
class SchemaParser extends Serializable {

  private var enableDebugger = false

  private val debugger = new BasicTextDebugger

  private val DFDL_SIMPLE_TYPES = Set(
    XMLUtil.XSD_STRING,XMLUtil.XSD_FLOAT,XMLUtil.XSD_DOUBLE,
    XMLUtil.XSD_DECIMAL,XMLUtil.XSD_INTEGER,XMLUtil.XSD_LONG,
    XMLUtil.XSD_INT,XMLUtil.XSD_SHORT,XMLUtil.XSD_BYTE,
    XMLUtil.XSD_UNSIGNED_LONG,XMLUtil.XSD_UNSIGNED_INT,
    XMLUtil.XSD_UNSIGNED_SHORT,XMLUtil.XSD_UNSIGNED_BYTE,XMLUtil.XSD_BOOLEAN,
    XMLUtil.XSD_DATE,XMLUtil.XSD_TIME,XMLUtil.XSD_DATE_TIME,XMLUtil.XSD_HEX_BINARY)

  private val DFDL_RESERVED_TYPES = Set(
    XMLUtil.XSD_NAMESPACE+"/"+"normalizedString",XMLUtil.XSD_NAMESPACE+"/"+"token",XMLUtil.XSD_NAMESPACE+"/"+"Name",
    XMLUtil.XSD_NAMESPACE+"/"+"NCName",XMLUtil.XSD_NAMESPACE+"/"+"QName",XMLUtil.XSD_NAMESPACE+"/"+"language",
    XMLUtil.XSD_NAMESPACE+"/"+"positiveInteger",XMLUtil.XSD_NAMESPACE+"/"+"nonPositiveInteger",
    XMLUtil.XSD_NAMESPACE+"/"+"negativeInteger",XMLUtil.XSD_NAMESPACE+"/"+"nonNegativeInteger",
    XMLUtil.XSD_NAMESPACE+"/"+"gYear",XMLUtil.XSD_NAMESPACE+"/"+"gYearMonth",XMLUtil.XSD_NAMESPACE+"/"+"gMonthDay",
    XMLUtil.XSD_NAMESPACE+"/"+"gDay",XMLUtil.XSD_NAMESPACE+"/"+"ID",XMLUtil.XSD_NAMESPACE+"/"+"IDREF",
    XMLUtil.XSD_NAMESPACE+"/"+"IDREFS",XMLUtil.XSD_NAMESPACE+"/"+"ENTITIES",XMLUtil.XSD_NAMESPACE+"/"+"ENTITY",
    XMLUtil.XSD_NAMESPACE+"/"+"NMTOKEN",XMLUtil.XSD_NAMESPACE+"/"+"NMTOKENS",XMLUtil.XSD_NAMESPACE+"/"+"NOTATION",
    XMLUtil.XSD_NAMESPACE+"/"+"anyURI",XMLUtil.XSD_NAMESPACE+"/"+"base64Binary")

  private val DFDL_PREDEFINED_VARIABLES = List(
   ("encoding",XMLUtil.XSD_NAMESPACE+"/"+"string","UTF-8"),
   ("byteOrder",XMLUtil.XSD_NAMESPACE+"/"+"string","bigEndian"),
   ("binaryFloatRep",XMLUtil.XSD_NAMESPACE+"/"+"string","ieee"),
   ("outputNewLine",XMLUtil.XSD_NAMESPACE+"/"+"string","\u000A"))

  private var targetNamespace:String = _

  private var definedTypes:Map[String,BasicNode] = Map()

  private var definedElements:Map[String,BasicNode] = Map()

  private var definedFormats:Map[String,Format] = Map()

  private var topLevelAnnotations:Annotation = _

  private var root:Element = _

  def getTypes = definedTypes

  /** Sets whether the debugger should be engaged when parsing a document with the document parser produced.
   */
  def setDebugging(debugging:Boolean) = enableDebugger = debugging

  /** Parses a schema in the given file */
  def parse(fileName:String):Unit =
    parse(new FileInputStream(fileName))

  /** Parses a schema in the given InputStream */
  def parse(input: InputStream): Unit = {
    val dfdlSubsetNS = XMLUtil.DFDL_NAMESPACE + "XMLSchemaSubset"
    val dfdlSchemaNode = scala.xml.XML.load(input)
    val tns = dfdlSchemaNode.namespace.toString
    val inputStream = {
        // because Eclipse gives us nice electric XML mode for editing DFDL schemas
        // and will check our usage of the XML Schema subset, as well as proper use of
        // short form annotations, etc., for these reasons we really want to use the 
        // DFDL Subset-of-XML-Schema URI in our DFDL schemas, not the basic XML Schema URI.
        //
        // Unfortunately, that means all the symbols are in the "wrong" namespace per-se.
        // So we force-switch the namespace via this total awful hack.
        //
        // FIXME: this won't work if schemas include other DFDL Schemas. We'd have to 
        // intercept all of them and fix them up the same way. This is doable... not sure
        // it's worth it. Also, I worry about performance, but that may be silly. Cost of
        // stringing a whole schema is tiny compared with the processing of it.
        //
        val dfdlSchemaString = dfdlSchemaNode.toString
        val xmlSchemaString = dfdlSchemaString.replaceAll(dfdlSubsetNS, XMLUtil.XSD_NAMESPACE)
        val inStream = new java.io.ByteArrayInputStream(xmlSchemaString.getBytes());
        inStream
      }
    try {
      val builder = new SAXBuilder()
      val document = builder.build(inputStream)
      parse(document getRootElement)
    } catch {
      case e: JDOMParseException => throw new MalformedXMLException("Parsing the schema. " + e.getMessage, e)
    }
  }

  /** Parses a schema in the given DOM tree
   * @param root the root of the schema
   */
  def parse(root:Element):Unit = {
    val nsroot = XMLUtil.getFullNameWithNamespace(root)
    if (nsroot!=XMLUtil.SCHEMA)
      throw new DFDLSchemaDefinitionException("Top element is not xsd:schema",null,root,null,None)

    this root = root

    XMLUtil.getAttribute(root,"targetNamespace") match {
      case Some(s) => targetNamespace = s
      case None => targetNamespace = ""
    }

    topLevelAnnotations = AnnotationParser(root,definedFormats)

    for(child <- XMLUtil.getChildren(root))
      XMLUtil.getFullNameWithNamespace(child) match {
        case XMLUtil.COMPLEX_TYPE => topLevelComplexType(child)
        case XMLUtil.SIMPLE_TYPE => throw
        		new UnimplementedException("Top level simple type unimplemented: optional",schemaContext = child)
        case XMLUtil.ELEMENT => topLevelElement(child)
        case XMLUtil.GROUP => topLevelGroup(child)
        case XMLUtil.ATTRIBUTE | XMLUtil.ATTRIBUTE_GROUP => throw
                new DFDLDisallowConstructException("attribute/attributeGroup not allowed in DFDL",schemaContext = child)
        case XMLUtil.PCDATA | XMLUtil.REM =>
        case XMLUtil.ANNOTATION =>
      }
  }

  /** Parses a top level xsd:complexType definition */
  def topLevelComplexType(node:Element) = {
    XMLUtil.getAttribute(node,"name") match {
      case Some(s) => definedTypes += ((s,parseComplexType(node,List(s))))
      case None => throw new DFDLSchemaDefinitionException("Missing name for top level type",null,node,null,None)
    }
  }

  /** Parses a top level xsd:simpleType definition */
  // TODO determine where this might be used, other than in defining a prefixLengthType
  def topLevelSimpleType(node:Element) = { 
	  XMLUtil.getAttribute(node,"name") match {
	 	  case Some(s) => definedTypes += ((s,parseTopLevelSimpleType(node,List(s))))
	 	  case None => throw new DFDLSchemaDefinitionException("Missing name for top level simple type",null,node,null,None)
	  }
  }

  /** Parses a top level xsd:group definition */
  def topLevelGroup(node:Element) = {
    XMLUtil.getAttribute(node,"name") match {
      case Some(s) => definedTypes += ((s,parseGroup(node,List(s))))
      case None => throw new DFDLSchemaDefinitionException("Missing name for top level group",null,node,null,None)
    }
  }

  /** Parses a top level xsd:element definition */
  def topLevelElement(node:Element) =
    XMLUtil.getAttribute(node,"name") match {
      case Some(s) => definedTypes += ((s,parseElement(node,List(s))));
                      definedElements += ((s,parseElement(node,List(s))))
      case None => throw new DFDLSchemaDefinitionException("Missing name for top level element",null,node,null,None)
    }

  /** Parses a element referred by the "ref" attribute in the schema */
  def parseReferredElement(node:Element,name:String,topReferred:List[String]):BasicNode = {
    val annotations = AnnotationParser(node,definedFormats)

    val basicNode =
      if (topReferred.contains(name)) //recursion, create a by-name reference
        new ReferenceNode(definedTypes,name,annotations)
      else
        parseElementNode(XMLUtil.getChildByName(root,name),name::topReferred)
    
    basicNode.annotation = basicNode.annotation.combine(annotations)
    basicNode
  }

  /** Parses an xsd:element definition */
  //could be an element, or anything that could go where elements go
  //(sequence, group)
  def parseElementNode(node:Element,topReferred:List[String]):BasicNode =
    XMLUtil.getAttribute(node,"ref") match {
      case Some(name) => parseReferredElement(node,name,topReferred)
      case None => XMLUtil.getFullNameWithNamespace(node) match {
        case XMLUtil.ELEMENT => parseElement(node,topReferred)
        case XMLUtil.GROUP => parseGroup(node,topReferred)
        case XMLUtil.SEQUENCE => parseSequence(node,topReferred)
        case XMLUtil.XSD_CHOICE => parseChoice(node,topReferred)
        case _ => throw new DFDLSchemaDefinitionException("I'm not sure what to do with this :"+XMLUtil.getFullName(node),
          null,node,null,None)
      }
    }

  /** Top-level simple types */
  def parseTopLevelSimpleType(node:Element,topReferred:List[String]):BasicNode = {
	  val annotations = AnnotationParser(node,definedFormats)
	  var basicNode:BasicNode = null
	  basicNode.annotation = basicNode.annotation.combine(annotations)
	  basicNode
  }
  
  /** Parses an xsd:complexType definition */
  def parseComplexType(node:Element,topReferred:List[String] = Nil):BasicNode = {
    val annotations = AnnotationParser(node,definedFormats)

    var basicNode:BasicNode = null
    for(child <- XMLUtil.getChildren(node))
      XMLUtil.getFullNameWithNamespace(child) match {
        case XMLUtil.SEQUENCE | XMLUtil.XSD_CHOICE | XMLUtil.GROUP => basicNode = parseElementNode(child,topReferred)
        case XMLUtil.ANNOTATION => if (AnnotationParser.getHiddenElement(child,root)!=null)
          throw new DFDLSchemaDefinitionException("Element not allowed here",null,node,null,None)
        case XMLUtil.ELEMENT => throw new DFDLSchemaDefinitionException("Element not allowed here",null,node,null,None)
        case XMLUtil.PCDATA | XMLUtil.REM =>
        case XMLUtil.ATTRIBUTE | XMLUtil.ATTRIBUTE_GROUP | XMLUtil.ALL =>
          throw new DFDLDisallowConstructException("attribute/attributeGroup/all not allowed in DFDL",schemaContext = node)
        case s:String => throw new DFDLSchemaDefinitionException("Invalid tag "+s,null,node,null,None)
      }

    if (basicNode==null)
      throw new DFDLSchemaDefinitionException("Invalid complexType definition",null,node,null,None)

    basicNode.annotation = basicNode.annotation.combine(annotations)
    basicNode
  }
  
  def parseElement(node:Element,topReferred:List[String]):BasicNode = {
    var element:BasicNode = null
    val annotations =  AnnotationParser(node,definedFormats)

    val name = XMLUtil.getAttribute(node,"name") match {
      case Some(s) => s
      case None => {
        XMLUtil.getAttribute(node,"ref") match {
          case Some(s2) => return parseElement(XMLUtil.getChildByName(root,s2),topReferred)
          case None => throw new DFDLSchemaDefinitionException("Element with no name",null,node,null,None)
        }
      }
    }
    element = XMLUtil.getAttribute(node,"type") match {
      case Some(s) => {
        if (DFDL_SIMPLE_TYPES.contains(XMLUtil.getFullName(s,XMLUtil.getNamespaces(node)))){
          wrap(new SimpleElement(name,annotations,targetNamespace,XMLUtil.getNamespaces(node,targetNamespace)),
            annotations,node)
        }else if (DFDL_RESERVED_TYPES.contains(XMLUtil.getFullName(s,XMLUtil.getNamespaces(node)))){
          throw new DFDLReservedKeywordException("Reserved DFDL keyword '"+s+"'",node)
        }else {
          parseComplexType(XMLUtil.getChildByName(root,s),topReferred) match {
            case simpleType:SimpleType =>
              wrap(new SimpleExtendedElement(name,annotations,targetNamespace,
                XMLUtil.getNamespaces(node,targetNamespace),simpleType),annotations,node)
            case  complexType:ComplexType =>
              wrap(new ComplexElement(name,annotations,complexType,targetNamespace,
                XMLUtil getNamespaces(node,targetNamespace)),annotations,node)
            case debuggingBasicNode:DebuggingBasicNode =>
              wrap(new ComplexElement(name,annotations,debuggingBasicNode,targetNamespace,
                XMLUtil getNamespaces(node,targetNamespace)),annotations,node)
          }
        }
      }
      case None => XMLUtil.getChildByTag(node,XMLUtil.COMPLEX_TYPE) match {
        case Some(complexType) =>
          wrap(new ComplexElement(name,annotations,parseComplexType(complexType,topReferred),
            targetNamespace,XMLUtil getNamespaces(node,targetNamespace)),annotations,node)
        case None => XMLUtil.getChildByTag(node,XMLUtil.SIMPLE_TYPE) match {
          case Some(simpleType) =>
            wrap(new SimpleExtendedElement(name,annotations,targetNamespace,
              XMLUtil getNamespaces(node),parseComplexType(simpleType,topReferred)),annotations,node)
          case None => throw new DFDLSchemaDefinitionException("Element without type",null,node,null,None)
        }
      }
    }

    element
  }

  /** Parses an xsd:sequence definition */
  def parseSequence(node:Element,topReferred:List[String]):BasicNode = {
    var children:List[BasicNode] = Nil
    for(child <- XMLUtil.getChildren(node))
      XMLUtil.getFullNameWithNamespace(child) match {
        case XMLUtil.PCDATA | XMLUtil.REM =>
        case XMLUtil.ELEMENT | XMLUtil.GROUP | XMLUtil.SEQUENCE | XMLUtil.XSD_CHOICE =>
          children = parseElementNode(child,topReferred) :: children
        case XMLUtil.ANNOTATION => { val n = AnnotationParser.getHiddenElement(child,root);
          if (n!=null) children = createHiddenNode(n,topReferred) :: children }
        case XMLUtil.ATTRIBUTE | XMLUtil.ATTRIBUTE_GROUP | XMLUtil.ALL => throw
                new DFDLDisallowConstructException("attribute/attributeGroup/all not allowed in DFDL",schemaContext = child)
        case s:String => throw new DFDLSchemaDefinitionException("Invalid tag "+s,null,child,null,None)
      }
    val annotations = AnnotationParser(node,definedFormats)
    wrap(new Sequence(annotations,targetNamespace,XMLUtil getNamespaces(node,targetNamespace),children.reverse),
      annotations,node)
  }

  /** Parses an xsd:choice definition */
  def parseChoice(node:Element,topReferred:List[String]):BasicNode = {
    var children:List[BasicNode] = Nil
    for(child <- XMLUtil.getChildren(node))
      XMLUtil.getFullNameWithNamespace(child) match {
        case XMLUtil.PCDATA | XMLUtil.REM =>
        case XMLUtil.ELEMENT | XMLUtil.GROUP | XMLUtil.SEQUENCE | XMLUtil.XSD_CHOICE =>
          children = parseElementNode(child,topReferred) :: children
        case XMLUtil.ANNOTATION =>  { val n = AnnotationParser.getHiddenElement(child,root);
          if (n!=null) children = createHiddenNode(n,topReferred) :: children }
        case XMLUtil.ATTRIBUTE | XMLUtil.ATTRIBUTE_GROUP | XMLUtil.ALL => throw
                new DFDLDisallowConstructException("attribute/attributeGroup/all not allowed in DFDL",schemaContext = child)
        case s:String => throw new DFDLSchemaDefinitionException("Invalid tag "+s,schemaContext = child)
      }
    val annotations = AnnotationParser(node,definedFormats)
    wrap(new Choice(annotations,targetNamespace,XMLUtil getNamespaces(node,targetNamespace),children.reverse),
      annotations,node)
  }

  /** Creates a dfdl:hidden node */
  def createHiddenNode(node:Element,topReferred:List[String]):BasicNode = {
    val element = parseElementNode(node,topReferred)
    element.annotation hidden = new Hidden()
    element
  }

  /** Parses an xsd:group definition */
  def parseGroup(node:Element,topReferred:List[String]):BasicNode = {
    parseComplexType(node,topReferred)
  }

  /** Returns the names of the top-level elements defined in the schema */
  def getTopElements:List[String] = {
    definedElements.toList map { _._1 }
  }

  /**
   * Evaluates an infoset against the schema to produce a representation of the original data file
   * @param infoset name of the XML format infoset file
   * @param root name of the top-level element of the schema to be run against infoset
   * @returns a java.nio.ByteBuffer representation of the original data file
   */
  def unparse(infoset:String, root:String):ByteBuffer =
    unparse(new BufferedInputStream(new FileInputStream(infoset)),root)
  
  /** Evaluates a document parser built from the schema in dataFile provided
   * @param dataFile name of the data file to be translated
   * @param root name of the top-level element of the schema to be the root of the translated document
   * @returns the root of the DOM tree produced
   */
  def eval(dataFile:String,root:String):Element =
    eval(new BufferedInputStream(new FileInputStream(dataFile)),root)

  /**
   * Evaluates an infoset against the schema to produce a representation of the original data file
   * @param inputStream for an infoset in XML format
   * @param root name of the top-level element of the schema to be run against infoset
   * @returns a java.nio.ByteBuffer representation of the original data file
   */
  def unparse(inputStream:BufferedInputStream, root:String):ByteBuffer = {
    val builder = new SAXBuilder()
    val infoDoc = builder.build(inputStream)
    unparse(infoDoc, root)
  }
  
  def unparse(infoDoc : Document, root:String):ByteBuffer = {
    var variables = getPredefinedVariables
    var bb:GrowableByteBuffer = new GrowableByteBuffer(ByteBuffer.allocate(1024))
    var schemaDoc = new Document()
    
    // Sets up the processors and variables the same way as the eval method
    // Only difference will be invoking the 'outProcess' method for processors instead of 'apply'
    // May not actually need this step. Need to check on this further
    for ( processor <- ProcessorFactory.getBeforeProcessor(topLevelAnnotations))
      variables = processor(schemaDoc, variables, targetNamespace, XMLUtil.getNamespaces(this.root, targetNamespace))
    

    val rootEle:org.jdom.Element = infoDoc.getRootElement;
    
    definedTypes.get(root) match {
      case Some(n) => {
          n match {
            // For now, only BasicNodeImpl seems to have things that matter
            case bni:BasicNodeImpl => {
                processChildren(bni, rootEle, bb)
            }
              
            case x =>
              println("Not bni: " + x.getClass)
          }
      }
      case None =>
    }

    return bb.byteBuffer
  }
  
  /**
   * Recurses the node tree calling the processes' outProcess method where applicable
   * @param node to be output processed
   * @param root to the XML infoset
   * @param output GrowableByteBuffer where results will be written to by processors
   */
  def  processChildren(node:BasicNodeImpl, root:org.jdom.Element, output:GrowableByteBuffer):Unit = {
    for ( c <- node.getChildren) {
      c match {
        case s: SimpleElement => {
            val name = s.getName(null)
            val e:org.jdom.Element = root.getChild(name)
            val v = e.getTextTrim
            ProcessorFactory.getInputProcessor(s.annotation).outProcess(output, v)
        }
        case b: BasicNodeImpl => {
            processChildren(b, root, output)
        }
        case x => {
            println("Can't Handle: " + x.getClass)
        }
      }
    }
  }
  
  def getFullElementName(e:org.jdom.Element): String ={
    e.getParentElement match {
      case ele:org.jdom.Element =>
        return getFullElementName(ele) + "." + e.getName
      case _ =>
        return e.getName
    }
  }
  
  /** Evaluates a document parser built from the schema in dataFile provided
   * @param inputStream input stream to be translated
   * @param root name of the top-level element of the schema to be the root of the translated document
   * @returns the root of the DOM tree produced
   */
  def eval(inputStream:BufferedInputStream,root:String):Element = {
    val reader = new RollbackStream(inputStream)
    val doc = new Document()
    var variables = getPredefinedVariables

    if (enableDebugger)
      ProcessorFactory setDebugger(debugger)

    for (processor <- ProcessorFactory getBeforeProcessor(topLevelAnnotations))
      variables = processor(doc,variables,targetNamespace,XMLUtil getNamespaces(this.root,targetNamespace))

    definedTypes.get(root) match {
      case Some(t) =>
        val s = t(reader,topLevelAnnotations,variables,doc,-1,Nil)
        variables removeHidden;
        s head
      case None => throw new DFDLSchemaDefinitionException("No top level type named "+root,null,null,null,None)
    }
  }

  private def wrap(basicNode:BasicNode,annotations:Annotation,node:org.jdom.Element) =
    if (enableDebugger)
       new DebuggingBasicNode(annotations,node,debugger,XMLUtil.getNamespaces(node,targetNamespace),
              basicNode)
    else
      basicNode

  private def getPredefinedVariables : VariableMap = {
    val namespaces = new Namespaces()
    var variables = new VariableMap
    namespaces.addNamespace(XMLUtil.DFDL_NAMESPACE,"dfdl")
    for((name,typeName,value) <- DFDL_PREDEFINED_VARIABLES)
      variables = variables.defineVariable("dfdl:"+name,typeName,namespaces,value)
    variables
  }
}

object SchemaParser {
  
  /** Deserializaes a DFDL generated parser from a file */
  def readParser(fileName:String):SchemaParser = {
    val is = new ObjectInputStream(new FileInputStream(fileName))
    is.readObject.asInstanceOf[SchemaParser]
  }

  /** Serializes the DFDL generated parser to a file */
  def writeParser(schemaParser:SchemaParser,fileName:String) = {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    os writeObject(schemaParser)
    os.close    
  }
}
