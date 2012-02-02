package daffodil.dsom

import scala.xml._
import daffodil.exceptions._

abstract class SchemaComponent(xml : Node, parent : Option[SchemaComponent]) {
  
}

abstract class Annotated(xml : Node, parent : Option[SchemaComponent]) extends SchemaComponent(xml, parent) {
  lazy val dais = {
    val ais = (xml \ "annotation" \ "appinfo")
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
  lazy val annotationObjs = {
	dais.flatMap {dai=>{
	    val children = dai.child
	    val res = children.filterNot{_.isInstanceOf[Text]}.map{child=>{
	      annotationFactory(child)
	    }}
	    res
	    }}
	}
    
  def annotationFactory(node : Node) : DFDLAnnotation
}

/**
 * Base class for any DFDL annotation
 */
abstract class DFDLAnnotation

/**
 * Base class for annotations that carry format properties
 */
abstract class DFDLFormatAnnotation(xml : Node) extends DFDLAnnotation {
  def getProperty(name : String) = {
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

abstract class ElementBase(xml: Node, parent: SchemaComponent) extends Annotated(xml, Some(parent)) {
  lazy val formatAnnotation = {
    val format = annotationObjs.find { _.isInstanceOf[DFDLElement] }
    val res = format match {
      case None => new DFDLElement(<dfdl:format/>, this)
      case Some(x) => x
    }
    res.asInstanceOf[DFDLElement]
  }
}

class SchemaDocument(xml : Node) extends Annotated(xml, None) {
  lazy val globalElementDecls = (xml \ "element").map{new GlobalElementDecl(_, this)}
  lazy val globalSimpleTypeDefs = (xml \ "simpleType").map{new GlobalSimpleTypeDef(_)}
  lazy val defaultFormat = {
    val format = annotationObjs.find{_.isInstanceOf[DFDLFormat]}
    format match { 
      case None => new DFDLFormat(<dfdl:format />, this)
      case Some(x) => x
    }
  }
  def annotationFactory(node : Node) : DFDLAnnotation = {
    Assert.notYetImplemented(node.label != "format")
    new DFDLFormat(xml, this)
  }
}

class DFDLFormat(xml : Node, sd : SchemaDocument) extends DFDLFormatAnnotation(xml) {
  
}

class DFDLElement(xml : Node, decl : ElementBase) extends DFDLFormatAnnotation(xml) {
  
}

class GlobalElementDecl(xml : Node, sd : SchemaDocument) extends ElementBase(xml, sd) {
  lazy val name = xml.attribute("name").get.text
  def annotationFactory(node : Node) : DFDLAnnotation = {
    Assert.notYetImplemented(node.label != "element")
    new DFDLElement(node, this)
  } 

  
  //lazy val byteOrder = 
}

class GlobalSimpleTypeDef(xml : Node) {
  
}

object DsomCompiler extends App {
  /*
  val root = """C:\cygwin\home\Jeremy\workspace\ngf\daffodil\sub-projects\"""
  val path = root + """daffodil-lib\src\xsd\DFDL_part3_model.xsd"""
  val xml = XML.loadFile(path)
  val sd = new SchemaDocument(xml)
  println(xml.label)
  */
  def compile(xml : Node) = {
    new SchemaDocument(xml)
  }
  
  /*
  //println(elements)
  val lastElement = elements.last

  //val lastType = lastElement.attribute("type")
  val typeattrs = (elements \\ "@type").toList
  println(typeattrs)
  //println(lastType)
  
  val (globalElementDecls, nonElements) = elements.partition{n=>{
    val typeattribute = n.attribute("type")
    val res = typeattribute match {
      case None => false
      case Some(m) => !(m.text.endsWith("Format"))
    }
    res
  }}
  */
}