package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.exceptions._
import daffodil.schema.annotation.props.gen._
import com.sun.xml.xsom.parser.{ SchemaDocument => XSSchemaDocument, _ }
import com.sun.xml.xsom._
import com.sun.xml.xsom.util._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
import parser.AnnotationParser

trait SchemaComponent {
  val schemaDocument : SchemaDocument
  
//  lazy val schemaDocument: SchemaDocument = {
//    parent match {
//      case None => this.asInstanceOf[SchemaDocument]
//      case Some(par) => par.schemaDocument
//    }
//  }

}

trait LexicallyEnclosedMixin {
 	val parent : SchemaComponent
}

trait LocalComponentMixin 
  extends SchemaComponent
  with LexicallyEnclosedMixin {
  
  lazy val schemaDocument = parent.schemaDocument
}

trait NamedMixin { 
	val xsNamed : { def getName() : String }
	val name = xsNamed.getName()
}

abstract class GlobalComponentMixin(val xsNamed : { def getName() : String })
  extends SchemaComponent
  with NamedMixin 

trait AnnotatedMixin { 
 
 /**
  * Anything annotated has to have an associated XSOM class with a getAnnotation() method.
  */
 val xsAnnotated : { def getAnnotation() : XSAnnotation } // this is called a type refinement or structural typing.
 
 /**
  * Anything annotated must be able to construct the
  * appropriate DFDLAnnotation object from the xml.
  */
 def annotationFactory(node: Node): DFDLAnnotation
  
 lazy val annotationNode = {
    val ann = xsAnnotated.getAnnotation()
    val annObj = asXml(ann.getAnnotation().asInstanceOf[org.w3c.dom.Element])
    annObj
  }
  
  /**
   * Converts DOM XML Node to Scala XML NodeSeq
   * 
   * We need this because XSOM only contains a predefined thing
   * to give back the annotation contents as a DOM tree.
   */
  def asXml(dom: org.w3c.dom.Node): Node = {
    val dom2sax = new org.apache.xalan.xsltc.trax.DOM2SAX(dom)
    val adapter = new NoBindingFactoryAdapter
    dom2sax.setContentHandler(adapter)
    dom2sax.parse()
    return adapter.rootElem
  }
  
  /**
   * dais = DFDL AppInfo NodeSeq
   */
 lazy val dais = {
    val ais = (annotationNode \ "appinfo")
    val dais = ais.filter { ai =>
      {
        ai.attribute("source") match {
          case None => false
          case Some(n) => {
            val str = n.text
            val hasRightSource = (str == "http://www.ogf.org/dfdl/dfdl-1.0/")
            val isAcceptable = str.startsWith("http://www.ogf.org/dfdl")
            (hasRightSource || isAcceptable) //TODO: remove lax check once examples & tests are updated.
          }
        }
      }
    }
    dais
  }
  
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

abstract class Annotated(val xso: XSComponent) 
  extends SchemaComponent with AnnotatedMixin {
}

/**
 * A schema set is exactly that, a set of schemas. Each schema has
 * a target namespace, so a schema set is conceptually a mapping from 
 * namespace URI onto schema. 
 * 
 * Constructing these from a list of schema Nodes is a unit-test 
 * interface.
 * 
 * schemaNodeList is a list of scala xml Node, each expected to be an <xs:schema...>
 * node. These may have include/import statements in them, and the schemas 
 * being included/imported don't have to be within the list.
 * 
 * xsoSSet is an XSOM XSSchemaSet object created from the same schemas. Note that
 * due to include/import, the xsoSSet may contain more Schemas than were part of
 * the schemaNodeList.
 */
class SchemaSet (schemaNodeList : Seq[Node], xsds : Set[XSSchemaDocument], xsoSSet : XSSchemaSet) {
  // TODO Constructor(s) or companion-object methods to create a SchemaSet from files.
   
   lazy val schemas = xsoSSet.getSchemas().map { new Schema(_, this) }
   
   lazy val schemaDocuments = xsds.map{ xsd=>{
       val Some(schema) = schemas.find{sch=>sch.targetNamespace == xsd.getTargetNamespace()}
       new SchemaDocument(xsd, schema)
     }
   } 
   
   /**
    * For use by SchemaDocument 
    */
   lazy val sdToEdecl = {
     val res = xsoSSet.iterateElementDecls().toSeq.groupBy{_.getSourceDocument()}
     res
   }
   lazy val sdToSTDef = xsoSSet.iterateSimpleTypes().toSeq.groupBy{_.getSourceDocument()}
   lazy val sdToCTDef = xsoSSet.iterateComplexTypes().toSeq.groupBy{_.getSourceDocument()}
   lazy val sdToGDef = xsoSSet.iterateModelGroupDecls().toSeq.groupBy{_.getSourceDocument()}
}

/**
 * A schema is all the schema documents sharing a single target namespace.
 * 
 * That is, one can write several schema documents which all have the 
 * same target namespace, and in that case all those schema documents make up
 * the 'schema'.
 */
class Schema (val xs : XSSchema, val schemaSet : SchemaSet) {
   lazy val targetNamespace = xs.getTargetNamespace()
   lazy val schemaDocuments = schemaSet.schemaDocuments.filter{_.targetNamespace == targetNamespace }
}

/**
 * A schema document corresponds to one file usually named with an ".xsd" extension.
 * The root element of a schema document is xsd:schema where xsd is the namespace for
 * XML Schema. 
 * 
 * A schema document is important because it is the unit of lexical scoping of format
 * properties in DFDL. 
 * 
 * Specifically, note that two schema documents may have the same target namespace, but
 * different default formats scoped over their contents. 
 * 
 * When dealing with plain XML and XSD (not DFDL), the concept of Schema with a single
 * target namespace is more used than the concept of schema document, which is usually
 * only needed to issue diagnostic error messages ("In file foo.xsd, line 938...")
 * 
 * Conversely, for DFDL, the concept of schema document is more used because schema documents
 * are where default formats are specified, so it is very important what schema document
 * a schema component was defined within.
 */
class SchemaDocument(xsd: XSSchemaDocument, val schema : Schema) extends AnnotatedMixin {
  
  lazy val xsAnnotated = Assert.notYetImplemented() // xso needs to be a thing with a getAnnotation() method
  // TODO: there is no way to get the annotations on a XSSchemaDocument in XSOM.
  // So we either have to pass down to here, the scala xml nodes, and we parse them from there, or we 
  // have to figure out the actual implementation class in XSOM, and start depending on that....
  // 
    
  lazy val targetNamespace = xsd.getTargetNamespace()
  
  def annotationFactory(node: Node): DFDLAnnotation = {
    Assert.notYetImplemented(node.label != "format") // nothing but a default format right now.
    new DFDLFormat(node, this)
  }
  
  private lazy val sset = schema.schemaSet
  
  lazy val globalElementDecls = {
    val eDeclMap = sset.sdToEdecl
    val optEdecls = eDeclMap.get(xsd)
    val eDecls = optEdecls.get
    val res = eDecls.map{ new GlobalElementDecl(_, this) }
    res
  }
  lazy val globalSimpleTypeDefs = sset.sdToSTDef.get(xsd).get.map{ new GlobalSimpleTypeDef(_, this) }
  lazy val globalComplexTypeDefs = sset.sdToCTDef.get(xsd).get.map{ new GlobalComplexTypeDef(_, this) }
  lazy val globalGroupDefs = sset.sdToGDef.get(xsd).get.map{ new GlobalGroupDef(_, this) }
  
  //lazy val globalElementDecls = (xml \ "element").map{new GlobalElementDecl(_, this)}
  //lazy val globalSimpleTypeDefs = (xml \ "simpleType").map{new GlobalSimpleTypeDef(_)}
 
  lazy val defaultFormat = {
   // val format = annotationObjs.find { _.isInstanceOf[DFDLFormat] }
   // val res = format match {
   //   case None => new DFDLFormat(<dfdl:format/>, this)
//      case Some(x) => x.asInstanceOf[DFDLFormat]
//    }
//    res
    new DFDLFormat(<dfdl:format/>, this) // return empty format for SchemaDocument
  }

}

 /////////////////////////////////////////////////////////////////
 // Elements System
 /////////////////////////////////////////////////////////////////

/**
 * provides element-specific implementation of requirements from AnnotatedMixin
 */
trait AnnotatedElementMixin extends AnnotatedMixin {

  lazy val formatAnnotation = {
    val format = annotationObjs.find { _.isInstanceOf[DFDLElement] }
    val res = format match {
      case None => new DFDLElement(<dfdl:element />, this)
      case Some(x) => x
    }
    res.asInstanceOf[DFDLElement]
  }
  
  def annotationFactory(node: Node): DFDLAnnotation = {
    new DFDLElement(node, this)
  }
  
}

// A Particle is something that can be repeating.
trait Particle { self : LocalElementBase =>
  
  lazy val minOccurs = self.xsParticle.getMinOccurs()
  
  lazy val maxOccurs = self.xsParticle.getMaxOccurs()
  
}

abstract class LocalElementBase(val xml : XSElementDecl, val xsParticle : XSParticle, parent : ModelGroup) 
  extends Term(xml, parent)
  with AnnotatedElementMixin
  with Particle

class ElementRef(xml: XSElementDecl, xsParticle : XSParticle, parent: ModelGroup) 
  extends LocalElementBase(xml, xsParticle, parent) {
  lazy val xsAnnotated = xml
}

trait ElementDeclBase extends AnnotatedElementMixin 

class LocalElementDecl(xml: XSElementDecl, xsParticle : XSParticle, parent: ModelGroup) 
  extends LocalElementBase(xml, xsParticle, parent) 
  with ElementDeclBase {
  lazy val xsAnnotated = xml
}
  
class GlobalElementDecl(xml: XSElementDecl, val schemaDocument: SchemaDocument) 
  extends GlobalComponentMixin(xml)
  with ElementDeclBase {
  val xsAnnotated = xml
}
  
 /////////////////////////////////////////////////////////////////
 // Groups System
 /////////////////////////////////////////////////////////////////

// A term is content of a group
abstract class Term(xml: XSTerm, val parent: SchemaComponent) 
  extends Annotated(xml) 
  with LocalComponentMixin

abstract class GroupBase(xml: XSModelGroup, parent: SchemaComponent) 
  extends Term(xml, parent) {
  lazy val xsAnnotated = xml
}

abstract class ModelGroup(xml: XSModelGroup, parent: SchemaComponent) extends GroupBase(xml, parent) 

class Choice(xml: XSModelGroup, parent: SchemaComponent) extends ModelGroup(xml, parent) {
  def annotationFactory(node: Node): DFDLAnnotation = new DFDLChoice(node, this)
}

class Sequence(xml: XSModelGroup, parent: SchemaComponent) extends ModelGroup(xml, parent) {
  def annotationFactory(node: Node): DFDLAnnotation = new DFDLSequence(node, this)
}

class GroupRef (xml: XSModelGroup, parent: SchemaComponent) extends GroupBase(xml, parent) {
  def annotationFactory(node: Node): DFDLAnnotation = new DFDLGroup(node, this)
}

class GlobalGroupDef(val xml: XSModelGroupDecl, val schemaDocument : SchemaDocument) 
 extends GlobalComponentMixin(xml) 

 /////////////////////////////////////////////////////////////////
 // Type System
 /////////////////////////////////////////////////////////////////
 
trait TypeBase

trait NamedType extends NamedMixin with TypeBase

trait SimpleTypeBase extends TypeBase 

trait NamedSimpleTypeBase extends SimpleTypeBase with NamedType 

trait ComplexTypeBase extends SchemaComponent with TypeBase 
 
class LocalSimpleTypeDef(xml : XSSimpleType, val parent : SchemaComponent) 
  extends SimpleTypeBase 
  with LocalComponentMixin 

//TBD: are Primitives "global", or do they just have names like globals do?
class PrimitiveType(override val name : String) extends NamedSimpleTypeBase {
  lazy val xsNamed = null // unused. we have to provide the definition in order to compile.
}

class GlobalSimpleTypeDef(val xml : XSSimpleType, val schemaDocument : SchemaDocument) 
  extends GlobalComponentMixin(xml)
  with NamedSimpleTypeBase 

class GlobalComplexTypeDef(val xml: XSComplexType, val schemaDocument : SchemaDocument) 
  extends GlobalComponentMixin(xml)
  with ComplexTypeBase  

class LocalComplexTypeDef(val xml: XSComplexType, val parent: SchemaComponent) 
  extends ComplexTypeBase 
  with LocalComponentMixin 
