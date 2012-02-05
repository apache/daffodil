//package daffodil.dsom
//
//import scala.xml._
//import scala.xml.parsing._
//import daffodil.exceptions._
//import daffodil.schema.annotation.props.gen._
//import com.sun.xml.xsom.parser.{SchemaDocument=>XSSchemaDocument, _}
//import com.sun.xml.xsom._
//import java.io.ByteArrayInputStream
//import java.io.InputStream
//import scala.collection.JavaConversions._
//import parser.AnnotationParser
//
//abstract class SchemaComponent(xml : Any, parent : Option[SchemaComponent]) {
//  lazy val schemaDocument : SchemaDocument = {
//    parent match {
//      case None => this.asInstanceOf[SchemaDocument]
//      case Some(par) => par.schemaDocument
//    }
//  }
//  val annotationNode : NodeSeq
//  lazy val dais = {
//    val ais = (annotationNode \ "appinfo")
//    val dais = ais.filter{ai=>{
//      ai.attribute("source") match {
//        case None => false
//        case Some(n) => {
//          val str = n.text
//          str == "http://www.ogf.org/dfdl/dfdl-1.0/"
//        }
//      }
//    }}
//    dais
//  }
//
//  def annotationFactory(node : Node) : DFDLAnnotation
//  
//  lazy val annotationObjs = {
//    dais.flatMap { dai =>
//      {
//        val children = dai.child
//        val res = children.filterNot { _.isInstanceOf[Text] }.map { child =>
//          {
//            annotationFactory(child)
//          }
//        }
//        res
//      }
//    }
//  }
//}
//
//
//
//abstract class Annotated(xml : XSComponent, parent : Option[SchemaComponent]) extends SchemaComponent(xml, parent) {
//
//}
//
///**
// * Base class for any DFDL annotation
// */
//abstract class DFDLAnnotation
//
///**
// * Base class for annotations that carry format properties
// */
//abstract class DFDLFormatAnnotation(xml : Node) extends DFDLAnnotation {
//  def getPropertyOption(name : String) = {
//    val a = xml.attribute(name)
//    a match {
//      case None => None
//      case Some(ns) => Some(ns.text)
//    }
//  }
//}
//
//
//
///**
// * Base class for assertions, variable assignments, etc
// */
//abstract class DFDLStatement
//
//abstract class ElementBase(xml: XSElementDecl, parent: SchemaComponent) extends Annotated(xml, Some(parent)) {
//  lazy val annotationNode = {
//    val ann = xml.getAnnotation()
//    val annObj = ann.getAnnotation().asInstanceOf[NodeSeq]
//    annObj
//  }
//  
//  lazy val formatAnnotation = {
//    val format = annotationObjs.find { _.isInstanceOf[DFDLElement] }
//    val res = format match {
//      case None => new DFDLElement(<dfdl:format/>, this)
//      case Some(x) => x
//    }
//    res.asInstanceOf[DFDLElement]
//  }
//  
//  
//
//}
//
//class SchemaDocument(xml: Node, sset: XSSchemaSet) extends SchemaComponent(sset, None){
//  lazy val annotationNode = Assert.notYetImplemented()
//  lazy val schemaSet = sset
//  lazy val globalElementDecls = geIterator.map{new GlobalElementDecl(_, this)}
//  lazy val geIterator : Iterator[XSElementDecl]= sset.iterateElementDecls()
//  lazy val globalSimpleTypeDefs: Iterator[XSSimpleType] = sset.iterateSimpleTypes()
//
//  //lazy val globalElementDecls = (xml \ "element").map{new GlobalElementDecl(_, this)}
//  //lazy val globalSimpleTypeDefs = (xml \ "simpleType").map{new GlobalSimpleTypeDef(_)}
//  lazy val defaultFormat = {
//    val format = annotationObjs.find { _.isInstanceOf[DFDLFormat] }
//    val res = format match {
//      case None => new DFDLFormat(<dfdl:format/>, this)
//      case Some(x) => x.asInstanceOf[DFDLFormat]
//    }
//    res
//  }
//
//  def annotationFactory(node: Node): DFDLAnnotation = {
//    Assert.notYetImplemented(node.label != "format")
//    new DFDLFormat(xml, this)
//  }
//}
//
//class DFDLFormat(xml : Node, sd : SchemaDocument)
//extends DFDLFormatAnnotation(xml) with Format_AnnotationMixin {
//  
//}
//
//class DFDLElement(xml : Node, decl : ElementBase)
//extends DFDLFormatAnnotation(xml) with Element_AnnotationMixin {
//  
//}
//
////class DFDLSimpleType(xml : Node, stDef : SimpleTypeBase) {
////}
//
//class GlobalElementDecl(xml : XSElementDecl, sd : SchemaDocument) extends ElementBase(xml, sd) {
//  lazy val name = xml.getName()
//  def annotationFactory(node : Node) : DFDLAnnotation = {
//    Assert.notYetImplemented(node.label != "element")
//    new DFDLElement(node, this)
//  } 
//
//  
//  //lazy val byteOrder = 
//}
//
//class GlobalSimpleTypeDef(xml : Node) {
//  
//}
//
//object DsomCompiler extends App {
//  def rip(schema : String) = {
//	val parser = new XSOMParser()
//	val apf = new AnnotationParserFactory() {
//	  def create() = {
//	    new AnnotationParser() {
//	      def getContentHandler(context : AnnotationContext , parentElementName : String, errorHandler : org.xml.sax.ErrorHandler, entityResolver : org.xml.sax.EntityResolver) : org.xml.sax.ContentHandler = {
//	        new NoBindingFactoryAdapter()
//	      }
//	      def getResult(existing : Object) = {
//	        Assert.notYetImplemented()
//	      }
//	    }
//	  }
//	}
//	parser.setAnnotationParser(apf)
//	//parser.setErrorHandler(...);
//	//parser.setEntityResolver(...);
//
//	val instream = new ByteArrayInputStream(schema.getBytes());
//	
//	parser.parse(instream)
//
//	val sset = parser.getResult()
//	sset
//}
//  /*
//  val root = """C:\cygwin\home\Jeremy\workspace\ngf\daffodil\sub-projects\"""
//  val path = root + """daffodil-lib\src\xsd\DFDL_part3_model.xsd"""
//  val xml = XML.loadFile(path)
//  val sd = new SchemaDocument(xml)
//  println(xml.label)
//  */
//  def compile(xml : Node) = {
//    val sset = rip(xml.toString())
//    new SchemaDocument(xml, sset)
//  }
//  
//  /*
//  //println(elements)
//  val lastElement = elements.last
//
//  //val lastType = lastElement.attribute("type")
//  val typeattrs = (elements \\ "@type").toList
//  println(typeattrs)
//  //println(lastType)
//  
//  val (globalElementDecls, nonElements) = elements.partition{n=>{
//    val typeattribute = n.attribute("type")
//    val res = typeattribute match {
//      case None => false
//      case Some(m) => !(m.text.endsWith("Format"))
//    }
//    res
//  }}
//  */
//}
//
//
///*
// * new org.xml.sax.helpers.DefaultHandler() {
//	          var buf : String = ""
//	          def startElement (uri : String, localName : String, qName : String, atts : Attributes) = {  
//	        	buf += "<" +  qName + ">"
//	          }
//
//	          def endElement (uri : String, localName : String, qName : String) = {
//	        	buf += "</" +  qName + ">"
//	          }
//
//	          def characters (ch : Array[Char], start : Int, length : Int) = {  
//	        	
//	          }
//	        }
// */
