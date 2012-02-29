package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.exceptions._
import daffodil.schema.annotation.props.gen._
/*
 * Not using XSOM - too many issues. Schema Documents aren't first class objects, and we need
 * them to implement lexical scoping of default formats over the contents of a schema document.
 * 
 * import com.sun.xml.xsom.parser.{ SchemaDocument => XSSchemaDocument, _ }
 * import com.sun.xml.xsom._
 * import com.sun.xml.xsom.util._
 */
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
	lazy val name = (xsNamed\"@name").text
}

trait GlobalComponentMixin
  extends SchemaComponent
  with NamedMixin

trait AnnotatedMixin { 
 
 /**
  * Anything annotated has to have an associated xml object and getAnnotation() method.
  */
 val xsAnnotated : Node 
 
 /**
  * Anything annotated must be able to construct the
  * appropriate DFDLAnnotation object from the xml.
  */
 def annotationFactory(node: Node): DFDLAnnotation
  
 lazy val annotationNode = {
    val ann = xsAnnotated\"annotation"
    //Assert.invariant(ann.length == 1)
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
 
  def emptyFormatFactory : DFDLAnnotation
  def isMyAnnotation(a : DFDLAnnotation) : Boolean
  
  lazy val formatAnnotation = {
    val format = annotationObjs.find { isMyAnnotation(_) }
    val res = format match {
      case None => emptyFormatFactory
      case Some(x) => x
    }
    res
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
class SchemaDocument(xsd: Node, val schema : Schema) extends AnnotatedMixin with SchemaComponent {
  
  lazy val xsAnnotated = xsd
  lazy val targetNamespace = schema.targetNamespace
  lazy val schemaDocument = this
  
  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:format>{ content @ _* }</dfdl:format> => new DFDLFormat(node, this)
      case <dfdl:defineFormat>{ content @ _* }</dfdl:defineFormat> => new DFDLDefineFormat(node, this)
      case <dfdl:defineEscapeScheme>{ content @ _* }</dfdl:defineEscapeScheme> => new DFDLDefineEscapeScheme(node, this)
      case <dfdl:defineVariable>{ content @ _* }</dfdl:defineVariable> => new DFDLDefineVariable(node, this)
      case _ => Assert.impossible("Invalid dfdl annotation found!")
    }
  }
  
  def emptyFormatFactory = new DFDLFormat(<dfdl:format />, this)
  def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLFormat]
  
  private lazy val sset = schema.schemaSet
  
  lazy val globalElementDecls = {
    val eDecls = xsd\"element"
    val res = eDecls.map{ new GlobalElementDecl(_, this) }
    res
  }
  
  lazy val globalSimpleTypeDefs = (xsd\"simpleType").map{ new GlobalSimpleTypeDef(_, this) }
  lazy val globalComplexTypeDefs = (xsd\"complexType").map{ new GlobalComplexTypeDef(_, this) }
  lazy val globalGroupDefs = (xsd\"group").map{ new GlobalGroupDef(_, this) }
 
  /**
   * Here we establish an invariant. Every annotatable schema component has, definitely, has an
   * annotation object. It may have no properties on it, but it will be there. Hence, we can 
   * delegate various property-related attribute calculations to it.
   */
  lazy val defaultFormat = {
    val format = annotationObjs.find { _.isInstanceOf[DFDLFormat] }
    val res = format match {
      case None => new DFDLFormat(<dfdl:format/>, this)
      case Some(x) => x.asInstanceOf[DFDLFormat]
    }
    res
  }
  
  lazy val defineFormats = {
    val df = annotationObjs.filter { _.isInstanceOf[DFDLDefineFormat] }
    df
  }

  lazy val defineEscapeSchemes = {
    val df = annotationObjs.filter { _.isInstanceOf[DFDLDefineEscapeScheme] }
    df
  }
  
  lazy val defineVariables = {
    val df = annotationObjs.filter { _.isInstanceOf[DFDLDefineVariable] }
    df
  }
}

 /////////////////////////////////////////////////////////////////
 // Elements System
 /////////////////////////////////////////////////////////////////

/**
 * provides element-specific implementation of requirements from AnnotatedMixin
 */
trait AnnotatedElementMixin extends AnnotatedMixin {
	def emptyFormatFactory = new DFDLElement(<dfdl:element />, this)
	def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLElement]
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

/**
 * Some XSD models have a single Element class, and distinguish local/global and element references
 * based on attributes of the instances. 
 * 
 * Our approach is to provide common behaviors on base classes or traits/mixins, and to have distinct
 * classes for each instance type.
 */
abstract class LocalElementBase(val xml : Node, parent : ModelGroup) 
  extends Term(xml, parent)
  with AnnotatedElementMixin
  with Particle {
  lazy val xsNamed = xml
  lazy val xsAnnotated = xml
  
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => super.annotationFactory(node)
    }
  }
}

class ElementRef(xml: Node, parent: ModelGroup) 
  extends LocalElementBase(xml, parent) {
  
}

trait ElementDeclBase extends AnnotatedElementMixin with SchemaComponent {
  val decl : Node
  lazy val immediateType : Option[TypeBase] = {
    val st = decl\"simpleType"
    val ct = decl\"complexType"
    val nt = (decl\"@type").text
    if (st.length == 1)
      Some(new LocalSimpleTypeDef(st(0), this))
    else if (ct.length == 1)
      Some(new LocalComplexTypeDef(ct(0), this))
    else
      None
  }
} 

class LocalElementDecl(xml: Node, parent: ModelGroup) 
  extends LocalElementBase(xml, parent) 
  with ElementDeclBase {
  lazy val decl = xml
}
  
 trait DFDLStatementMixin {
  def annotationFactory(node: Node, self: AnnotatedMixin): DFDLAnnotation = {
    node match {
      case <dfdl:assert>{ content @ _* }</dfdl:assert> => new DFDLAssert(node, self)
      case <dfdl:discriminator>{ content @ _* }</dfdl:discriminator> => new DFDLDiscriminator(node, self)
      case <dfdl:setVariable>{ content @ _* }</dfdl:setVariable> => new DFDLSetVariable(node, self)
      case <dfdl:newVariableInstance>{ content @ _* }</dfdl:newVariableInstance> => new DFDLNewVariableInstance(node, self)
      case _ => Assert.impossible("Invalid dfdl annotation found!")
    }
 }
}
  
class GlobalElementDecl(xml: Node, val schemaDocument: SchemaDocument) 
  extends GlobalComponentMixin
  with ElementDeclBase
  with DFDLStatementMixin {
  lazy val decl = xml
  val xsAnnotated = xml
  val xsNamed = xml
  
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => annotationFactory(node, this)
    }
  }
}
  
 /////////////////////////////////////////////////////////////////
 // Groups System
 /////////////////////////////////////////////////////////////////

// A term is content of a group
abstract class Term(xml: Node, val parent: SchemaComponent) 
  extends Annotated(xml) 
  with LocalComponentMixin 
  with DFDLStatementMixin {
  def annotationFactory(node: Node): DFDLAnnotation = annotationFactory(node, this)
}

abstract class GroupBase(xml: Node, parent: SchemaComponent) 
  extends Term(xml, parent) {
  lazy val xsAnnotated = xml
}

abstract class ModelGroup(xml: Node, parent: SchemaComponent) extends GroupBase(xml, parent) 

class Choice(xml: Node, parent: SchemaComponent) extends ModelGroup(xml, parent) {
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:choice>{ contents @ _* }</dfdl:choice> => new DFDLChoice(node, this)
      case _ => super.annotationFactory(node)
    }
  }
  
  def emptyFormatFactory = new DFDLChoice(<dfdl:choice />, this)
  def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLChoice]
}

class Sequence(xml: Node, parent: SchemaComponent) extends ModelGroup(xml, parent) {
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:sequence>{ contents @ _* }</dfdl:sequence> => new DFDLSequence(node, this)
      case _ => super.annotationFactory(node)
    }
  }
  
  def emptyFormatFactory = new DFDLSequence(<dfdl:sequence />, this)
  def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLSequence]

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
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:group>{ contents @ _* }</dfdl:group> => new DFDLGroup(node, this)
      case _ => super.annotationFactory(node)
    }
  }
  
  def emptyFormatFactory = new DFDLGroup(<dfdl:group />, this)
  def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLGroup]
  
}

class GlobalGroupDef(val xml: Node, val schemaDocument : SchemaDocument) 
 extends GlobalComponentMixin {
  val xsNamed = xml
}

 /////////////////////////////////////////////////////////////////
 // Type System
 /////////////////////////////////////////////////////////////////
 
trait TypeBase

trait NamedType extends NamedMixin with TypeBase

abstract class SimpleTypeBase(xml : Node, val parent : SchemaComponent) 
extends TypeBase with AnnotatedMixin with DFDLStatementMixin {
  val xsAnnotated = xml

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:simpleType>{ contents @ _* }</dfdl:simpleType> => new DFDLSimpleType(node, this)
      case _ => annotationFactory(node, this)
    }
  }
}

abstract class NamedSimpleTypeBase(xml : Node, parent : SchemaComponent)
extends SimpleTypeBase(xml, parent) with NamedType {
	lazy val xsNamed = xml
}

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
 
class LocalSimpleTypeDef(xml : Node, parent : SchemaComponent) 
  extends SimpleTypeBase(xml, parent)
  with LocalComponentMixin {
  
  def emptyFormatFactory = new DFDLSimpleType(<dfdl:simpleType />, this)
  def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]
}

//TBD: are Primitives "global", or do they just have names like globals do?
class PrimitiveType(name_ : String) extends NamedSimpleTypeBase (null, null) {
  override lazy val xsNamed = <fake_primitive name={name_}/> // unused. we have to provide the definition in order to compile.
  
  def emptyFormatFactory = Assert.invariantFailed()
  def isMyAnnotation(a : DFDLAnnotation) = Assert.invariantFailed()
}

class GlobalSimpleTypeDef(val xml : Node, val schemaDocument : SchemaDocument) 
extends NamedSimpleTypeBase (xml, schemaDocument) with GlobalComponentMixin {
  def emptyFormatFactory = new DFDLSimpleType(<dfdl:simpleType />, this)
	def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]
}

class GlobalComplexTypeDef(val xml: Node, val schemaDocument : SchemaDocument) 
  extends GlobalComponentMixin
  with ComplexTypeBase {
  lazy val xsNamed = xml
}

class LocalComplexTypeDef(val xml: Node, val parent: SchemaComponent) 
  extends ComplexTypeBase 
  with LocalComponentMixin 
