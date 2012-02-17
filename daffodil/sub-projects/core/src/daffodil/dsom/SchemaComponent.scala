package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.exceptions._
import daffodil.schema.annotation.props.gen._
/*import com.sun.xml.xsom.parser.{ SchemaDocument => XSSchemaDocument, _ }
import com.sun.xml.xsom._
import com.sun.xml.xsom.util._*/
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
//import parser.AnnotationParser

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
	val xsNamed : Node
	val name = (xsNamed\"@name").text
}

abstract class GlobalComponentMixin(val xsNamed : Node)
  extends SchemaComponent
  with NamedMixin 

trait AnnotatedMixin { 
 
 /**
  * Anything annotated has to have an associated XSOM class with a getAnnotation() method.
  */
 val xsAnnotated : Node // this is called a type refinement or structural typing.
 
 /**
  * Anything annotated must be able to construct the
  * appropriate DFDLAnnotation object from the xml.
  */
 def annotationFactory(node: Node): DFDLAnnotation
  
 lazy val annotationNode = {
    val ann = xsAnnotated\"annotation"
    Assert.invariant(ann.length == 1)
    ann
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

abstract class Annotated(val xso: Node) 
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
class SchemaSet (schemaNodeList : Seq[Node]) {
  // TODO Constructor(s) or companion-object methods to create a SchemaSet from files.
   
	lazy val schemaPairs = schemaNodeList.map{ s => {
	  val ns = (s\"@targetNamespace").text
	  (ns,s)
	  }
	}
	
	lazy val schemaGroups = schemaPairs.groupBy{
	  case (ns,s) => ns 
	}.toList
	
   lazy val schemas = schemaGroups.map { 
     case (ns, pairs) => {
       val sds = pairs.map(_._2) // Grabs second of 'pairs', list of schema Document
       val res = new Schema(ns, sds, this) 
       res
     	}
     }
   
//   lazy val schemaDocuments = xsds.map{ xsd=>{
//       val Some(schema) = schemas.find{sch=>sch.targetNamespace == xsd.getTargetNamespace()}
//       new SchemaDocument(xsd, schema)
//     }
//   } 
   
//   /**
//    * For use by SchemaDocument 
//    */
//   lazy val sdToEdecl = {
//     val res = xsoSSet.iterateElementDecls().toSeq.groupBy{_.getSourceDocument()}
//     res
//   }
//   lazy val sdToSTDef = xsoSSet.iterateSimpleTypes().toSeq.groupBy{_.getSourceDocument()}
//   lazy val sdToCTDef = xsoSSet.iterateComplexTypes().toSeq.groupBy{_.getSourceDocument()}
//   lazy val sdToGDef = xsoSSet.iterateModelGroupDecls().toSeq.groupBy{_.getSourceDocument()}
}

/**
 * A schema is all the schema documents sharing a single target namespace.
 * 
 * That is, one can write several schema documents which all have the 
 * same target namespace, and in that case all those schema documents make up
 * the 'schema'.
 */
class Schema (val ns: String, val sds : NodeSeq, val schemaSet : SchemaSet) {
   lazy val targetNamespace = ns
   lazy val schemaDocuments = sds.map{
     new SchemaDocument(_, this)
   }
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
class SchemaDocument(xsd: Node, val schema : Schema) extends AnnotatedMixin {
  
  lazy val xsAnnotated = xsd
  lazy val targetNamespace = schema.targetNamespace
  
  def annotationFactory(node: Node): DFDLAnnotation = {
    Assert.notYetImplemented(node.label != "format") // nothing but a default format right now.
    new DFDLFormat(node, this)
  }
  
  private lazy val sset = schema.schemaSet
  
  lazy val globalElementDecls = {
    val eDecls = xsd\"element"
    val res = eDecls.map{ new GlobalElementDecl(_, this) }
    res
  }
  
  lazy val globalSimpleTypeDefs = (xsd\"simpleType").map{ new GlobalSimpleTypeDef(_, this) }
  lazy val globalComplexTypeDefs = (xsd\"complexType").map{ new GlobalComplexTypeDef(_, this) }
  lazy val globalGroupDefs = (xsd\"group").map{ new GlobalGroupDef(_, this) }
 
  lazy val defaultFormat = {
    val format = annotationObjs.find { _.isInstanceOf[DFDLFormat] }
    val res = format match {
      case None => new DFDLFormat(<dfdl:format/>, this)
      case Some(x) => x.asInstanceOf[DFDLFormat]
    }
    res
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
  
  lazy val minOccurs = {
    val min = (self.xml\"@minOccurs").text.toString
    min match {
      case "" => 1
      case _ => min.toInt
    }
  }
  
  lazy val maxOccurs = {
    val max = (self.xml\"@maxOccurs").text.toString 
    max match {
      case "unbounded" => -1
      case "" => 1
      case _ => max.toInt 
    }
  }
}

abstract class LocalElementBase(val xml : Node, parent : ModelGroup) 
  extends Term(xml, parent)
  with AnnotatedElementMixin
  with Particle

class ElementRef(xml: Node, parent: ModelGroup) 
  extends LocalElementBase(xml, parent) {
  lazy val xsAnnotated = xml
}

trait ElementDeclBase extends AnnotatedElementMixin 

class LocalElementDecl(xml: Node, parent: ModelGroup) 
  extends LocalElementBase(xml, parent) 
  with ElementDeclBase {
  lazy val xsAnnotated = xml
}
  
class GlobalElementDecl(xml: Node, val schemaDocument: SchemaDocument) 
  extends GlobalComponentMixin(xml)
  with ElementDeclBase {
  val xsAnnotated = xml
}
  
 /////////////////////////////////////////////////////////////////
 // Groups System
 /////////////////////////////////////////////////////////////////

// A term is content of a group
abstract class Term(xml: Node, val parent: SchemaComponent) 
  extends Annotated(xml) 
  with LocalComponentMixin

abstract class GroupBase(xml: Node, parent: SchemaComponent) 
  extends Term(xml, parent) {
  lazy val xsAnnotated = xml
}

abstract class ModelGroup(xml: Node, parent: SchemaComponent) extends GroupBase(xml, parent) 

class Choice(xml: Node, parent: SchemaComponent) extends ModelGroup(xml, parent) {
  def annotationFactory(node: Node): DFDLAnnotation = new DFDLChoice(node, this)
}

class Sequence(xml: Node, parent: SchemaComponent) extends ModelGroup(xml, parent) {
  def annotationFactory(node: Node): DFDLAnnotation = new DFDLSequence(node, this)

  lazy val <sequence>{ xmlChildren @ _* }</sequence> = xml
  lazy val children = xmlChildren.flatMap { child =>
    child match {
      case <sequence>{ _* }</sequence> => List(new Sequence(xml, this))
      case <choice>{ _* }</choice> => List(new Choice(xml, this))
      case <group>{ _* }</group> => List(new GroupRef(xml, this))
      case <element>{ _* }</element> => {
        val refProp = (xml \ "@ref").text
        if (refProp == "") List(new LocalElementDecl(xml, this))
        else List(new ElementRef(xml, this))
      }
      case textNode : Text => Nil
      case _ => Assert.impossibleCase()
    }
  }
}

class GroupRef (xml: Node, parent: SchemaComponent) extends GroupBase(xml, parent) {
  def annotationFactory(node: Node): DFDLAnnotation = new DFDLGroup(node, this)
}

class GlobalGroupDef(val xml: Node, val schemaDocument : SchemaDocument) 
 extends GlobalComponentMixin(xml) 

 /////////////////////////////////////////////////////////////////
 // Type System
 /////////////////////////////////////////////////////////////////
 
trait TypeBase

trait NamedType extends NamedMixin with TypeBase

trait SimpleTypeBase extends TypeBase 

trait NamedSimpleTypeBase extends SimpleTypeBase with NamedType

trait ComplexTypeBase extends SchemaComponent with TypeBase {
  val xml: Node

  lazy val <complexType>{ xmlChildren @ _* }</complexType> = xml
  lazy val Seq(modelGroup) = xmlChildren.flatMap { child =>
    child match {
      case <sequence>{ _* }</sequence> => List(new Sequence(child, this))
      case <choice>{ _* }</choice> => List(new Choice(child, this))
      case <group>{ _* }</group> => List(new GroupRef(child, this))
      case <annotation>{ _* }</annotation> => Nil
      case textNode : Text => Nil
      case _ => Assert.impossibleCase()
    }
  }
}
 
class LocalSimpleTypeDef(xml : Node, val parent : SchemaComponent) 
  extends SimpleTypeBase 
  with LocalComponentMixin 

//TBD: are Primitives "global", or do they just have names like globals do?
class PrimitiveType(override val name : String) extends NamedSimpleTypeBase {
  lazy val xsNamed = null // unused. we have to provide the definition in order to compile.
}

class GlobalSimpleTypeDef(val xml : Node, val schemaDocument : SchemaDocument) 
  extends GlobalComponentMixin(xml)
  with NamedSimpleTypeBase 

class GlobalComplexTypeDef(val xml: Node, val schemaDocument : SchemaDocument) 
  extends GlobalComponentMixin(xml)
  with ComplexTypeBase  

class LocalComplexTypeDef(val xml: Node, val parent: SchemaComponent) 
  extends ComplexTypeBase 
  with LocalComponentMixin 
