package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.xml.XMLUtil
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
  val schemaDocument: SchemaDocument
  val xml : Node

}

trait LexicallyEnclosedMixin {
  val parent: SchemaComponent
}

trait LocalComponentMixin
  extends SchemaComponent
  with LexicallyEnclosedMixin {

  lazy val schemaDocument = parent.schemaDocument
}

/**
 * Anything named has a name from its name attribute and a namespace from the
 * schema document it is part of.
 */
trait NamedMixin { self: SchemaComponent =>
  lazy val name = (xml \ "@name").text
  lazy val namespace = self.schemaDocument.targetNamespace
}

trait GlobalComponentMixin
  extends SchemaComponent
  with NamedMixin

trait AnnotatedMixin extends SchemaComponent {
  /**
   * Anything annotated must be able to construct the
   * appropriate DFDLAnnotation object from the xml.
   */
  def annotationFactory(node: Node): DFDLAnnotation

  lazy val annotationNode = {
    val ann = xml \ "annotation"
    ann
  }

  /**
   * dais = Dfdl App Info nodeSeq
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

  /**
   * Here we establish an invariant which is that every annotatable schema component has, definitely, has an
   * annotation object. It may have no properties on it, but it will be there. Hence, we can
   * delegate various property-related attribute calculations to it.
   * 
   * To realize this, every concrete class must implement (or inherit) an implementation of 
   * emptyFormatFactory, which constructs an empty format annotation,
   * and isMyAnnotation which tests if an annotation is the corresponding kind.
   * 
   * Given that, formatAnnotation then either finds the right annotation, or constructs one, but our invariant
   * is imposed. There *is* a formatAnnotation.
   */
  def emptyFormatFactory: DFDLAnnotation
  def isMyAnnotation(a: DFDLAnnotation): Boolean

  lazy val formatAnnotation = {
    val format = annotationObjs.find { isMyAnnotation(_) }
    val res = format match {
      case None => emptyFormatFactory
      case Some(x) => x
    }
    res
  }
}

abstract class Annotated(xmlArg: Node)
  extends SchemaComponent with AnnotatedMixin {
  lazy val xml = xmlArg
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
class SchemaSet(schemaNodeList: Seq[Node]) {
  // TODO Constructor(s) or companion-object methods to create a SchemaSet from files.

  lazy val schemaPairs = schemaNodeList.map { s =>
    {
      val ns = (s \ "@targetNamespace").text
      (ns, s)
    }
  }

  lazy val schemaGroups = schemaPairs.groupBy {
    case (ns, s) => ns
  }.toList

  lazy val schemas = schemaGroups.map {
    case (ns, pairs) => {
      val sds = pairs.map(_._2) // Grabs second of 'pairs', list of schema Document
      val res = new Schema(ns, sds, this)
      res
    }
  }

  /**
   * Retrieve schema by namespace name.
   */
  def getSchema(namespace: String) = {
    // TODO: what about when there is no namespace. Can we pass "" ??
    val schemaForNamespace = schemas.find { s => s.targetNamespace == namespace }
    schemaForNamespace
  }
  
  /**
   * Given a namespace and name, try to retrieve the named object
   */
  def getGlobalElementDecl(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalElementDecl(name) }
  def getGlobalSimpleTypeDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalSimpleTypeDef(name) }
  def getGlobalComplexTypeDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalComplexTypeDef(name) }
  def getGlobalGroupDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalGroupDef(name) }
  def getDefineFormat(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineFormat(name) }
  def getDefineVariable(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineVariable(name) }
  def getDefineEscapeScheme(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineEscapeScheme(name) }

}

/**
 * A schema is all the schema documents sharing a single target namespace.
 *
 * That is, one can write several schema documents which all have the
 * same target namespace, and in that case all those schema documents make up
 * the 'schema'.
 */
class Schema(val namespace: String, val schemaDocs: NodeSeq, val schemaSet: SchemaSet) {

  lazy val targetNamespace = namespace

  lazy val schemaDocuments = schemaDocs.map { new SchemaDocument(_, this) }

  
  private def noneOrOne[T](scs: Seq[T], name: String) : Option[T] = {
    scs match {
      case Nil => None
      case Seq(sc) => Some(sc)
      case _ => throw new DFDLSchemaDefinitionException("more than one definition for name: " + name)
    }
  }

  /**
   * Given a name, try to retrieve the appropriate object.
   */
  def getGlobalElementDecl(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalElementDecl(name) }, name)
  def getGlobalSimpleTypeDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalSimpleTypeDef(name) }, name)
  def getGlobalComplexTypeDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalComplexTypeDef(name) }, name)
  def getGlobalGroupDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalGroupDef(name) }, name)
  def getDefineFormat(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineFormat(name) }, name)
  def getDefineVariable(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineVariable(name) }, name)
  def getDefineEscapeScheme(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineEscapeScheme(name) }, name)

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
class SchemaDocument(xmlArg: Node, val schema: Schema) extends AnnotatedMixin with SchemaComponent {

  lazy val xml = xmlArg
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

  def emptyFormatFactory = new DFDLFormat(<dfdl:format/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLFormat]

  private lazy val sset = schema.schemaSet

  lazy val globalElementDecls = (xml \ "element").map { new GlobalElementDecl(_, this) }
  lazy val globalSimpleTypeDefs = (xml \ "simpleType").map { new GlobalSimpleTypeDef(_, this) }
  lazy val globalComplexTypeDefs = (xml \ "complexType").map { new GlobalComplexTypeDef(_, this) }
  lazy val globalGroupDefs = (xml \ "group").map { new GlobalGroupDef(_, this) }


  lazy val defaultFormat = formatAnnotation.asInstanceOf[DFDLFormat]
  
  lazy val defaultProperties = defaultFormat.combinedLocalProperties

  //
  // There's some scala way of avoiding all this downcasting, using some clever type parameterization scheme
  // Have to ask a Scala types expert.
  //
  lazy val defineFormats = {
    val df = annotationObjs.filter { _.isInstanceOf[DFDLDefineFormat] }.map{_.asInstanceOf[DFDLDefineFormat]}
    df
  }

  lazy val defineEscapeSchemes = {
    val desc = annotationObjs.filter { _.isInstanceOf[DFDLDefineEscapeScheme] }.map{_.asInstanceOf[DFDLDefineEscapeScheme]}
    desc
  }
  
  lazy val defineVariables = {
    val dv = annotationObjs.filter { _.isInstanceOf[DFDLDefineVariable] }.map{_.asInstanceOf[DFDLDefineVariable]}
    dv
  }

  /**
   * Issue: cloning, so that each referer gets a copy of the object
   * TBD: don't design so as to rule-out recursion some day.
   */
  def getGlobalElementDecl(name: String) = globalElementDecls.find { _.name == name }
  def getGlobalSimpleTypeDef(name: String) = globalSimpleTypeDefs.find { _.name == name }
  def getGlobalComplexTypeDef(name: String) = globalComplexTypeDefs.find { _.name == name }
  def getGlobalGroupDef(name: String) = globalGroupDefs.find { _.name == name }
  def getDefineFormat(name: String) = defineFormats.find { _.name == name }
  def getDefineVariable(name: String) = defineVariables.find { _.name == name }
  def getDefineEscapeScheme(name: String) = defineEscapeSchemes.find { _.name == name }

}

/////////////////////////////////////////////////////////////////
// Elements System
/////////////////////////////////////////////////////////////////

/**
 * provides element-specific implementation of requirements from AnnotatedMixin
 */
trait AnnotatedElementMixin extends AnnotatedMixin {
  def emptyFormatFactory = new DFDLElement(<dfdl:element/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLElement]
}

// A Particle is something that can be repeating.
trait Particle { self: LocalElementBase =>

  lazy val minOccurs = {
    val min = (self.xml \ "@minOccurs").text.toString
    min match {
      case "" => 1
      case _ => min.toInt
    }
  }

  lazy val maxOccurs = {
    val max = (self.xml \ "@maxOccurs").text.toString
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
abstract class LocalElementBase(xmlArg: Node, parent: ModelGroup)
  extends Term(xmlArg, parent)
  with AnnotatedElementMixin
  with Particle {

  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:element>{ contents @ _* }</dfdl:element> => new DFDLElement(node, this)
      case _ => super.annotationFactory(node)
    }
  }
}

class ElementRef(xmlArg: Node, parent: ModelGroup)
  extends LocalElementBase(xmlArg, parent) with HasRef {
}

trait HasRef { self : SchemaComponent =>
  lazy val ref = (xml \ "@ref").text
}

trait ElementDeclBase extends AnnotatedElementMixin with SchemaComponent {
  lazy val immediateType: Option[TypeBase] = {
    val st = xml \ "simpleType"
    val ct = xml \ "complexType"
    val nt = (xml \ "@type").text
    if (st.length == 1)
      Some(new LocalSimpleTypeDef(st(0), this))
    else if (ct.length == 1)
      Some(new LocalComplexTypeDef(ct(0), this))
    else {
      Assert.invariant(nt != "")
      None
    }
  }
}

class LocalElementDecl(xmlArg: Node, parent: ModelGroup)
  extends LocalElementBase(xmlArg, parent)
  with ElementDeclBase {
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

class GlobalElementDecl(xmlArg: Node, val schemaDocument: SchemaDocument)
  extends GlobalComponentMixin
  with ElementDeclBase
  with DFDLStatementMixin {
  lazy val xml = xmlArg
  
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
abstract class Term(xmlArg: Node, val parent: SchemaComponent)
  extends Annotated(xmlArg)
  with LocalComponentMixin
  with DFDLStatementMixin {
  def annotationFactory(node: Node): DFDLAnnotation = annotationFactory(node, this)
}

abstract class GroupBase(xmlArg: Node, parent: SchemaComponent)
  extends Term(xmlArg, parent) {
}

/**
 * Base class for all model groups, which are term containers.
 */
abstract class ModelGroup(xmlArg: Node, parent: SchemaComponent) extends GroupBase(xmlArg, parent) {
  
  /**
   * Synthesize a term - i.e., a model group has a term factory to construct its terms.
   * 
   * Because of the context where this is used, this returns a list. Nil for non-terms, non-Nil for 
   * an actual term. There should be only one non-Nil.
   */
   def term(child : Node) = {
     val childList : List[Term] = child match {
      case <element>{ _* }</element> => {
        val refProp = (child \ "@ref").text
        if (refProp == "") List(new LocalElementDecl(child, this))
        else List(new ElementRef(child, this))
      }
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => ModelGroup(child, this)
    }
     childList
   }
   
   val xmlChildren : Seq[Node]
     
   lazy val children = xmlChildren.flatMap { term(_) }
}

/**
 * A factory for model groups.
 */
object ModelGroup{
  
  /**
   * Because of the contexts where this is used, we return a list. That lets users
   * flatmap it to get a collection of model groups. Nil for non-model groups, non-Nil for the model group
   * object. There should be only one non-Nil.
   */
    def apply(child : Node, self : SchemaComponent) = {
      val childList : List[GroupBase] = child match {
      case <sequence>{ _* }</sequence> => List(new Sequence(child, self))
      case <choice>{ _* }</choice> => List(new Choice(child, self))
      case <group>{ _* }</group> => List(new GroupRef(child, self))
      case <annotation>{ _* }</annotation> => Nil
      case textNode: Text => Nil
      case _ => Assert.impossibleCase()
    }
    childList
    }
}

class Choice(xmlArg: Node, parent: SchemaComponent) extends ModelGroup(xmlArg, parent) {
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:choice>{ contents @ _* }</dfdl:choice> => new DFDLChoice(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  def emptyFormatFactory = new DFDLChoice(<dfdl:choice/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLChoice]
  
  lazy val <choice>{ xmlChildren @ _* }</choice> = xml
}

class Sequence(xmlArg: Node, parent: SchemaComponent) extends ModelGroup(xmlArg, parent) {
  
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:sequence>{ contents @ _* }</dfdl:sequence> => new DFDLSequence(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  def emptyFormatFactory = new DFDLSequence(<dfdl:sequence/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSequence]

  lazy val <sequence>{ xmlChildren @ _* }</sequence> = xml

}

class GroupRef(xmlArg : Node, parent: SchemaComponent) extends GroupBase(xmlArg, parent) {
  override def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:group>{ contents @ _* }</dfdl:group> => new DFDLGroup(node, this)
      case _ => super.annotationFactory(node)
    }
  }

  def emptyFormatFactory = new DFDLGroup(<dfdl:group/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLGroup]

}

class GlobalGroupDef(val xmlArg : Node, val schemaDocument: SchemaDocument)
  extends GlobalComponentMixin {
  lazy val xml = xmlArg
  //
  // Note: Dealing with XML can be fragile. It's easy to forget some of these children
  // might be annotations and Text nodes. Even if you trim the text nodes out, there are
  // places where annotations can be.
  //
  lazy val <group>{ xmlChildren @ _* }</group> = xml
  //
  // So we have to map, so that we can tolerate annotation objects.
  // and our ModelGroup factory has to return Nil for annotations and Text nodes.
  //
  lazy val Seq(modelGroup) = xmlChildren.flatMap{ ModelGroup(_, this) }
  
}

/////////////////////////////////////////////////////////////////
// Type System
/////////////////////////////////////////////////////////////////

trait TypeBase

trait NamedType extends NamedMixin with TypeBase with SchemaComponent

abstract class SimpleTypeBase(xmlArg: Node, val parent: SchemaComponent)
  extends TypeBase with AnnotatedMixin with DFDLStatementMixin {
  lazy val xml = xmlArg
  
  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:simpleType>{ contents @ _* }</dfdl:simpleType> => new DFDLSimpleType(node, this)
      case _ => annotationFactory(node, this)
    }
  }
}

abstract class NamedSimpleTypeBase(xmlArg : Node, parent: SchemaComponent)
  extends SimpleTypeBase(xmlArg, parent) with NamedType {
}

trait ComplexTypeBase extends SchemaComponent with TypeBase {
  lazy val <complexType>{ xmlChildren @ _* }</complexType> = xml
  lazy val Seq(modelGroup) = xmlChildren.flatMap{ ModelGroup(_, this) }
}

class LocalSimpleTypeDef(xmlArg : Node, parent: SchemaComponent)
  extends SimpleTypeBase(xmlArg, parent)
  with LocalComponentMixin {

  def emptyFormatFactory = new DFDLSimpleType(<dfdl:simpleType/>, this)
  def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]
  
  lazy val base = (xml \ "restriction" \ "@base").text
  
}



//TBD: are Primitives "global", or do they just have names like globals do?
class PrimitiveType(name_ : String) extends NamedSimpleTypeBase(null, null) {
  //
  // Lots of faking & dummy objects here
  //
  override lazy val xml = <fake_primitive name={ name_ }/> // unused. we have to provide the definition in order to compile.
  lazy val xsdNamespace = XMLUtil.XSD_NAMESPACE
  lazy val dummySchemaSet = new SchemaSet(NodeSeq.Empty)
  lazy val xsdSchema = new Schema(xsdNamespace, NodeSeq.Empty, dummySchemaSet)
  lazy val schemaDocument = new SchemaDocument(<schema/>, xsdSchema)
  def emptyFormatFactory = Assert.invariantFailed()
  def isMyAnnotation(a : DFDLAnnotation) = Assert.invariantFailed()
}

class GlobalSimpleTypeDef(xmlArg : Node, val schemaDocument: SchemaDocument)
  extends NamedSimpleTypeBase(xmlArg, schemaDocument) with GlobalComponentMixin {
  
  def emptyFormatFactory = new DFDLSimpleType(<dfdl:simpleType/>, this)
  
  def isMyAnnotation(a : DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]
  
}

class GlobalComplexTypeDef(xmlArg : Node, val schemaDocument: SchemaDocument)
  extends GlobalComponentMixin
  with ComplexTypeBase {
  lazy val xml = xmlArg
}

class LocalComplexTypeDef(val xmlArg : Node, val parent: SchemaComponent)
  extends ComplexTypeBase
  with LocalComponentMixin {
  lazy val xml = xmlArg
}
