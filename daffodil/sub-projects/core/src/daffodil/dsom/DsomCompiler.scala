package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.exceptions._
import daffodil.schema.annotation.props.gen._
import com.sun.xml.xsom.parser.{SchemaDocument=>XSSchemaDocument, _}
import com.sun.xml.xsom._
import com.sun.xml.xsom.util._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
import parser.AnnotationParser

abstract class SchemaComponent(xml : Any, parent : Option[SchemaComponent]) {
  lazy val schemaDocument : SchemaDocument = {
    parent match {
      case None => this.asInstanceOf[SchemaDocument]
      case Some(par) => par.schemaDocument
    }
  }
  val annotationNode : NodeSeq
  lazy val dais = {
    val ais = (annotationNode \ "appinfo")
    val dais = ais.filter{ai=>{
      ai.attribute("source") match {
        case None => false
        case Some(n) => {
          val str = n.text
          str == "http://www.ogf.org/dfdl/dfdl-1.0/"
        }
      }
    }}
    dais
  }

  def annotationFactory(node : Node) : DFDLAnnotation
  
  lazy val annotationObjs = {
    dais.flatMap { dai =>
      {
        val children = dai.child
        val res = children.filterNot { _.isInstanceOf[Text] }.map { child =>
          {
            annotationFactory(child)
          }
        }
        res
      }
    }
  }
}



abstract class Annotated(xml : XSComponent, parent : Option[SchemaComponent]) extends SchemaComponent(xml, parent) {

}

/**
 * Base class for any DFDL annotation
 */
abstract class DFDLAnnotation

/**
 * Base class for annotations that carry format properties
 */
abstract class DFDLFormatAnnotation(xml : Node) extends DFDLAnnotation {
  def getPropertyOption(name : String) = {
    val a = xml.attribute(name)
    a match {
      case None => None
      case Some(ns) => Some(ns.text)
    }
  }
}



/**
 * Base class for assertions, variable assignments, etc
 */
abstract class DFDLStatement

abstract class ElementBase(xml: XSElementDecl, parent: SchemaComponent) extends Annotated(xml, Some(parent)) {

def asXml(dom: org.w3c.dom.Node): Node = {
    val dom2sax = new org.apache.xalan.xsltc.trax.DOM2SAX(dom) 
    val adapter = new NoBindingFactoryAdapter
    dom2sax.setContentHandler(adapter)
    dom2sax.parse()
    return adapter.rootElem
  }

  lazy val annotationNode = {
    val ann = xml.getAnnotation()
    val annObj = asXml(ann.getAnnotation().asInstanceOf[org.w3c.dom.Element])
      annObj
  }
  
  lazy val formatAnnotation = {
    val format = annotationObjs.find { _.isInstanceOf[DFDLElement] }
    val res = format match {
      case None => new DFDLElement(<dfdl:format/>, this)
      case Some(x) => x
    }
    res.asInstanceOf[DFDLElement]
  }
}

class SchemaDocument(xml: Node, sset: XSSchemaSet) extends SchemaComponent(sset, None){
  lazy val annotationNode = Assert.notYetImplemented()
  lazy val schemaSet = sset
  lazy val globalElementDecls = geIterator.map{new GlobalElementDecl(_, this)}
  lazy val geIterator : Iterator[XSElementDecl]= sset.iterateElementDecls()
  lazy val globalSimpleTypeDefs: Iterator[XSSimpleType] = sset.iterateSimpleTypes()

  //lazy val globalElementDecls = (xml \ "element").map{new GlobalElementDecl(_, this)}
  //lazy val globalSimpleTypeDefs = (xml \ "simpleType").map{new GlobalSimpleTypeDef(_)}
  lazy val defaultFormat = {
    val format = annotationObjs.find { _.isInstanceOf[DFDLFormat] }
    val res = format match {
      case None => new DFDLFormat(<dfdl:format/>, this)
      case Some(x) => x.asInstanceOf[DFDLFormat]
    }
    res
  }

  def annotationFactory(node: Node): DFDLAnnotation = {
    Assert.notYetImplemented(node.label != "format")
    new DFDLFormat(xml, this)
  }
}

class DFDLFormat(xml : Node, sd : SchemaDocument)
extends DFDLFormatAnnotation(xml) with Format_AnnotationMixin {
  
}

class DFDLElement(xml : Node, decl : ElementBase)
extends DFDLFormatAnnotation(xml) with Element_AnnotationMixin {
  
}

class GlobalElementDecl(xml : XSElementDecl, sd : SchemaDocument) extends ElementBase(xml, sd) {
  lazy val name = xml.getName()
  def annotationFactory(node : Node) : DFDLAnnotation = {
    Assert.notYetImplemented(node.label != "element")
    new DFDLElement(node, this)
  }
}

class GlobalSimpleTypeDef(xml : Node) {
  
}

object DsomCompiler extends App {
  def rip(schema : String) = {
	val parser = new XSOMParser()
	val apf = new DomAnnotationParserFactory()
	parser.setAnnotationParser(apf)
	
	val instream = new ByteArrayInputStream(schema.getBytes());
	
	parser.parse(instream)

	val sset = parser.getResult()
	sset
}

def compile(xml : Node) = {
    val sset = rip(xml.toString())
    new SchemaDocument(xml, sset)
  }
}
