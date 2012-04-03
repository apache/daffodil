package daffodil.dsom

import scala.xml._
import daffodil.exceptions._
import daffodil.grammar._
import daffodil.xml._

/////////////////////////////////////////////////////////////////
// Type System
/////////////////////////////////////////////////////////////////

trait TypeBase {
  // use def, can be overriden by lazy val or def
  def localAndRefProperties: Map[String, String]
}

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

  lazy val localAndRefProperties: Map[String, String] = {
    this.formatAnnotation.getFormatPropertiesNonDefault()
  }
  
  // Returns name of base class in the form of
  // ex:myType
  //
  lazy val restrictionBase: String = {
    val rsb = xml \\ "restriction" \ "@base"
    rsb.head.text
  }
  
  lazy val myBaseType = {
    Assert.invariant(restrictionBase.length() != 0)
    val sd = this.schemaDocument
    val (nsURI, localName) = XMLUtil.QName(xml, restrictionBase, sd)
    
    sd.globalSimpleTypeDefs.find{ x => x.detailName == localName}
  }

  // Need to go and grab a list of GlobalSimpleTypes from the schema document
  // execute a find using the name obtained from restrictionBase.
  lazy val simpleTypeBaseProperties: Map[String,String] = {
    
    val baseProps = {
      myBaseType match {
        case Some(bt) => bt.forDerivedType(this).localAndRefProperties
        case None => Map.empty[String,String]
      }
    }
    
    baseProps
  }
  
  lazy val overlappingLocalProperties = {
    val localAndRef = localAndRefProperties.map{ x => x._1 }.toSet
    val baseProps = simpleTypeBaseProperties.map{ x => x._1 }.toSet
    val intersect = localAndRef.intersect(baseProps)
    intersect
  }
  
  lazy val hasOverlap: Boolean = {
    if (overlappingLocalProperties.size > 0){
      true
    }
    else {
      false
    }
  }
  
  lazy val allProperties = {
    Assert.schemaDefinition(!hasOverlap, "Overlap detected between simpleType (" + this.detailName + ") and its base.")
    
    val theLocalUnion = {
      localAndRefProperties.toSeq.union(simpleTypeBaseProperties.toSeq).toMap
    }
    val filteredDefaultProps = this.defaultProperties.filter(x => x._1 != "ref")
    val combined = this.formatAnnotation.combinePropertiesWithOverriding(theLocalUnion, filteredDefaultProps)
   
    combined
  }
}

abstract class NamedSimpleTypeBase(xmlArg: => Node, parent: => SchemaComponent)
  extends SimpleTypeBase(xmlArg, parent) with NamedType {
}

class LocalSimpleTypeDef(xmlArg: Node, parent: ElementDeclBase)
  extends SimpleTypeBase(xmlArg, parent)
  with LocalComponentMixin {

  lazy val detailName = "inside " + parent.detailName
  def emptyFormatFactory = new DFDLSimpleType(<dfdl:simpleType/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

  lazy val baseName = (xml \ "restriction" \ "@base").text
  lazy val baseType = {
    val res = if (baseName == "") None
    else Assert.notYetImplemented() // should go find the global simple type here
  }
}

//TBD: are Primitives "global", or do they just have names like globals do?
class PrimitiveType(name_ : String) extends NamedType {
  //
  // Lots of faking & dummy objects here
  //
  override lazy val name = name_
  override lazy val namespace = XMLUtil.XSD_NAMESPACE
  lazy val xml = Assert.invariantFailed("Primitives don't have xml definitions.")
  lazy val dummySchemaSet = new SchemaSet(NodeSeq.Empty)
  lazy val xsdSchema = new Schema(namespace, NodeSeq.Empty, dummySchemaSet)
  lazy val schemaDocument = new SchemaDocument(<schema/>, xsdSchema)
  lazy val localAndRefProperties: Map[String, String] = {
    Map.empty[String, String]
  }
}

/**
 * The factory is sharable even though the global object it creates cannot
 * be shared.
 *
 * Call forElement(element) and supply the element referring
 * to the global type, then you get back an instance that is one-to-one with the
 * element.
 *
 * This then allows attributes of the type to refer to the element in deciding things.
 * I.e., the context is clear and kept separate for each place a global type is used.
 */

class GlobalSimpleTypeDefFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends GlobalComponentMixin {
  def xml = xmlArg
  def schemaDocument = schemaDocumentArg
  
  def forRoot() = new GlobalSimpleTypeDef(xmlArg, schemaDocument, None)

  /**
   * Create a private instance for this element's use.
   */
  def forElement(element: ElementDeclBase) = new GlobalSimpleTypeDef(xmlArg, schemaDocumentArg, Some(element))
  def forDerivedType(derivedType: SimpleTypeBase) = new GlobalSimpleTypeDef(xmlArg, schemaDocument, None)
}
/**
 * The instance type for global simple type definitions.
 */
class GlobalSimpleTypeDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, val element: Option[AnnotatedMixin])
  extends NamedSimpleTypeBase(xmlArg, schemaDocumentArg)
  with GlobalComponentMixin {

  def schemaDocument = schemaDocumentArg
  def emptyFormatFactory = new DFDLSimpleType(<dfdl:simpleType/>, this)

  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

}

abstract class ComplexTypeBase(xmlArg: Node, val parent: SchemaComponent)
  extends SchemaComponent
  with TypeBase
  with ComplexTypeBaseGrammarMixin {

  lazy val xml = xmlArg
  lazy val <complexType>{ xmlChildren @ _* }</complexType> = xml
  lazy val Seq(modelGroup) = xmlChildren.flatMap { GroupFactory(_, this, 1) }
  lazy val localAndRefProperties: Map[String, String] = {
    Map.empty[String, String]
  }
}

class GlobalComplexTypeDefFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends GlobalComponentMixin {
  def xml = xmlArg
  def schemaDocument = schemaDocumentArg

  def forElement(element: ElementDeclBase) = new GlobalComplexTypeDef(xmlArg, schemaDocumentArg, element)
}

class GlobalComplexTypeDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, val element: ElementDeclBase)
  extends ComplexTypeBase(xmlArg, schemaDocumentArg)
  with GlobalComponentMixin {
  def schemaDocument = schemaDocumentArg
}

class LocalComplexTypeDef(xmlArg: Node, parent: ElementDeclBase)
  extends ComplexTypeBase(xmlArg, parent)
  with LocalComponentMixin {
}
