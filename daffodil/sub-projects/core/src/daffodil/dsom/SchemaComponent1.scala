//package daffodil.dsom
//
//import scala.xml._
//import scala.xml.parsing._
//import daffodil.exceptions._
//import daffodil.schema.annotation.props.gen._
//import com.sun.xml.xsom.parser.{ SchemaDocument => XSSchemaDocument, _ }
//import com.sun.xml.xsom._
////import com.sun.xml.xsom.
//import com.sun.xml.xsom.util._
//import java.io.ByteArrayInputStream
//import java.io.InputStream
//import scala.collection.JavaConversions._
//import parser.AnnotationParser
//
//abstract class SchemaComponent(xml_component_arg: XSComponent, parent: Option[SchemaComponent]) {
//  
//  // def this(xml_doc_arg: XSSchemaDocument) = this(null, None)
//  
////  lazy val xmlDoc = xml_doc_arg
////  lazy val xmlComp = xml_component_arg
//  
//  lazy val schemaDocument: SchemaDocument = {
//    parent match {
//      case None => this.asInstanceOf[SchemaDocument]
//      case Some(par) => par.schemaDocument
//    }
//  }
//
//}
//
//trait AnnotatedMixin { 
// lazy val dais = {
//    val ais = (annotationNode \ "appinfo")
//    val dais = ais.filter { ai =>
//      {
//        ai.attribute("source") match {
//          case None => false
//          case Some(n) => {
//            val str = n.text
//            str == "http://www.ogf.org/dfdl/dfdl-1.0/"
//          }
//        }
//      }
//    }
//    dais
//  }
//  
//  val annotationNode: NodeSeq
//  def annotationFactory(node: Node): DFDLAnnotation
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
//abstract class Annotated(xml: XSComponent, parent: Option[SchemaComponent]) 
//  extends SchemaComponent(xml, parent) with AnnotatedMixin {
// 
//
//}
//
//// trait MyXSSchemaDocument extends XSComponent with XSSchemaDocument
//
//class SchemaDocument(xml: XSSchemaDocument, sset: XSSchemaSet) extends SchemaComponent(null, None) with AnnotatedMixin {
//  lazy val annotationNode = Assert.notYetImplemented()
//  lazy val schemaSet = sset
//  lazy val globalElementDecls = geIterator.map { new GlobalElementDecl(_, this) }
//  lazy val geIterator: Iterator[XSElementDecl] = sset.iterateElementDecls()
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
//abstract class ElementBase(xml: XSElementDecl, parent: SchemaComponent) extends Term(xml, parent) {
//
//  def asXml(dom: org.w3c.dom.Node): Node = {
//    val dom2sax = new org.apache.xalan.xsltc.trax.DOM2SAX(dom)
//    val adapter = new NoBindingFactoryAdapter
//    dom2sax.setContentHandler(adapter)
//    dom2sax.parse()
//    return adapter.rootElem
//  }
//
//  lazy val annotationNode = {
//    val ann = xml.getAnnotation()
//    val annObj = asXml(ann.getAnnotation().asInstanceOf[org.w3c.dom.Element])
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
//  def annotationFactory(node: Node): DFDLAnnotation = {
//    Assert.notYetImplemented(node.label != "element")
//    new DFDLElement(this)
//  }
//  
//}
//
//// A term is content
//abstract class Term(xml: XSTerm, parent: SchemaComponent) extends Annotated(xml, Some(parent)) {
//  
//}
//
//abstract class GroupBase(xml: XSModelGroup, parent: SchemaComponent) extends Term(xml, parent) {
//  
//}
//
//abstract class ModelGroup(xml: XSModelGroup, parent: SchemaComponent) extends GroupBase(xml, parent) {
//  
//}
//
//abstract class ElementDeclBase(xml: XSElementDecl, parent: SchemaComponent) extends ElementBase(xml, parent) {
//  
//}
//
//// A Particle is something that can be repeating.
//trait Particle
//
//trait TypeBase
//
//trait Named { 
//	val name : String
//}
//
//trait NamedType extends Named with TypeBase
//
//abstract class SimpleTypeBase() extends TypeBase {
//  
//}
//
//abstract class ComplexTypeBase(xml: XSComplexType, parent: SchemaComponent) extends SchemaComponent(xml, Some(parent)) with TypeBase {
//  
//}
//
//abstract class NamedSimpleTypeBase() extends SimpleTypeBase with NamedType {
//  
//}
//
//class GlobalElementDecl(xml: XSElementDecl, sd: SchemaDocument) extends ElementDeclBase(xml, sd) {
//
//}
//
//class LocalElementDecl(xml: XSElementDecl, parent: SchemaComponent) extends ElementDeclBase(xml, parent) with Particle {
//  
//}
//
//class ElementRef(xml: XSElementDecl, parent: SchemaComponent) extends ElementBase(xml, parent) with Particle {
//  
//}
//
//class Choice(xml: XSModelGroup, parent: SchemaComponent) extends ModelGroup(xml, parent){
//  
//}
//
//class Sequence(xml: XSModelGroup, parent: SchemaComponent) extends ModelGroup(xml, parent) {
//  
//}
//
//class GroupRef (xml: XSModelGroup, parent: SchemaComponent) extends GroupBase(xml, parent) {
//  
//}
//
//class SchemaSet () {
//  
//}
//
//class Schema (parent: SchemaSet) extends SchemaComponent(null, None) {
//  
//}
//
//class GlobalGroupDef () extends SchemaDocument with Named {
//  
//}
//
//class LocalSimpleTypeDef () extends SimpleTypeBase {
//  
//}
//
//class PrimitiveType () extends NamedSimpleTypeBase {
//  
//}
//
//class GlobalSimpleTypeDef() extends NamedSimpleTypeBase {
//  
//}
//
//class GlobalComplexTypeDef(xml: XSComplexType, parent: SchemaComponent) extends ComplexTypeBase(xml, parent) {
//  
//}
//
//class LocalComplexTypeDef(xml: XSComplexType, parent: SchemaComponent) extends ComplexTypeBase(xml, parent) {
//  
//}
