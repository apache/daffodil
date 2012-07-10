package daffodil.dsom

import scala.xml._
import daffodil.exceptions._
import daffodil.grammar._
import daffodil.xml._

/////////////////////////////////////////////////////////////////
// Type System
/////////////////////////////////////////////////////////////////

abstract class TypeBase(xmlArg : Node, context : SchemaComponent) 
extends SchemaComponent(xmlArg) {
  // use def, can be overriden by lazy val or def
  def localAndFormatRefProperties: Map[String, String]
  def allNonDefaultProperties = localAndFormatRefProperties
 
}

abstract class SimpleTypeBase(xmlArg : Node, context : SchemaComponent) 
extends TypeBase(xmlArg, context) {
  def primitiveType : PrimitiveType
}

abstract class SimpleTypeDefBase(xmlArg: Node, val parent: SchemaComponent)
  extends SimpleTypeBase(xmlArg, parent) 
with AnnotatedMixin 
with DFDLStatementMixin {

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:simpleType>{ contents @ _* }</dfdl:simpleType> => new DFDLSimpleType(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  lazy val localAndFormatRefProperties: Map[String, String] = {
    this.formatAnnotation.getFormatPropertiesNonDefault()
  }
  
  lazy val localProperties = {
    this.formatAnnotation.combinedLocalProperties
  }
  
  lazy val formatRefProperties = {
    this.formatAnnotation.formatRefProperties
  }
  
  lazy val combinedSimpleTypeAndBaseProperties = {
    schemaDefinition(overlappingLocalProperties.size == 0, 
        "Overlap detected between the local SimpleType (" 
        + this.detailName + ") properties and its base.")
        
    val props = this.localAndFormatRefProperties ++ this.simpleTypeBaseProperties
    props
  }
  
  // Returns name of base class in the form of
  // ex:myType
  //
  lazy val restrictionBase: String = {
    val rsb = xml \\ "restriction" \ "@base"
    rsb.head.text
  }
  
  
  lazy val myPrimitiveType = {
    val (nsURI, localName) = baseTypeQName
    if (nsURI == XMLUtils.XSD_NAMESPACE
        ||
        nsURI == XMLUtils.DFDL_SUBSET_NAMESPACE) {// tolerate use of this subset.
      // XSD namespace
      val prim = schemaDocument.schema.schemaSet.getPrimitiveType(localName)
      schemaDefinition(prim != None, 
          "Type " + localName + " is not an XSD primitive type.")
      prim
    }
    else None
  }
         
      
  lazy val myBaseTypeFactory = {
    Assert.invariant(restrictionBase.length() != 0)
    val (nsURI, localName) = baseTypeQName
    Assert.invariant(myPrimitiveType == None)
    val factory = schemaDocument.schema.schemaSet.getGlobalSimpleTypeDef(nsURI, localName)
    factory
  }

  /**
   * Follows all indirections to get you the ultimate primitive 
   * built-in simple type that must underlie all simple types 
   * eventually.
   */
  lazy val primitiveType = {
    myBaseType.primitiveType
  }
    
  lazy val baseTypeQName = XMLUtils.QName(xml, restrictionBase, schemaDocument)
  
  lazy val myBaseType : SimpleTypeBase = {
    myPrimitiveType match {
      case Some(pt) => pt
      case None => {
    	val bt = myBaseTypeFactory.map{ _.forDerivedType(this) }
    	bt match {
    	  case None => schemaDefinitionError("No type found for base: " + baseTypeQName)
    	  case Some(bt) => bt
    	}
      }
    }
  }
  
  lazy val myBaseTypeList = List(myBaseType)
  
  lazy val diagnosticChildren = annotationObjs ++ myBaseTypeList
  
  // TODO: Is the comment below still relevant? If not remove.
  // Need to go and grab a list of GlobalSimpleTypes from the schema document
  // execute a find using the name obtained from restrictionBase.
  
  lazy val simpleTypeBaseProperties: Map[String,String] = {
    val baseProps = {
      myBaseType match {
        case st : SimpleTypeDefBase => st.localAndFormatRefProperties
        case _ => Map.empty[String,String]
      }
    }
    baseProps
  }
  
  lazy val overlappingLocalProperties = {
    val localAndFormatRef = localAndFormatRefProperties.map{ x => x._1 }.toSet
    val baseProps = simpleTypeBaseProperties.map{ x => x._1 }.toSet
    val intersect = localAndFormatRef.intersect(baseProps)
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
  
  override lazy val allNonDefaultProperties = {
    schemaDefinition(!hasOverlap, "Overlap detected between simpleType (" + this.detailName + ") and its base.")
    
    val theLocalUnion = this.combinedSimpleTypeAndBaseProperties
    theLocalUnion
  }
}

class LocalSimpleTypeDef(xmlArg: Node, parent: ElementBase)
  extends SimpleTypeDefBase(xmlArg, parent)
  with LocalComponentMixin {

  lazy val detailName = "inside " + parent.detailName
  def emptyFormatFactory = new DFDLSimpleType(newDFDLAnnotationXML("simpleType"), this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

  lazy val baseName = (xml \ "restriction" \ "@base").text
  lazy val baseType = {
    val res = if (baseName == "") None
    else Assert.notYetImplemented() // should go find the global simple type here
  }
  
  lazy val prettyName = "complexType in " + parent.name
}

/**
 * We need a schema document and such for our primitives
 * so that our invariant, that *everything* has a schema document, schema, and schema set
 * holds true.
 */
object Fakes {
  lazy val xsd_sd = new SchemaDocument(<xs:schema xmlns:xs={ XMLUtils.XSD_NAMESPACE } />, xsd_schema)
  lazy val xsd_schema = new Schema(XMLUtils.XSD_NAMESPACE, NodeSeq.Empty, xsd_sset)
  lazy val xsd_sset = new SchemaSet(NodeSeq.Empty)
}

//TBD: are Primitives "global", or do they just have names like globals do?
class PrimitiveType(name_ : String) 
extends SimpleTypeBase(<primitive/>, Fakes.xsd_sd) // use fake schema document
with NamedMixin { 
    
  def primitiveType = this
  
  override def toString = "PrimitiveType(" + prettyName + ")"

  override lazy val name = name_
  override lazy val prettyName = name_
  override lazy val scPath = ""
  lazy val diagnosticChildren = Nil
  
  // override val xml = Assert.invariantFailed("Primitives don't have xml definitions.")
  
  override lazy val schemaDocument = Fakes.xsd_sd
  
  lazy val localAndFormatRefProperties = Map.empty[String, String]
  
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

class GlobalSimpleTypeDefFactory(val xml: Node, schemaDocument: SchemaDocument)
extends NamedMixin
{
  def forRoot() = new GlobalSimpleTypeDef(xml, schemaDocument, None)

  /**
   * Create a private instance for this element's use.
   */
  def forElement(element: ElementBase) = new GlobalSimpleTypeDef(xml, schemaDocument, Some(element))
  def forDerivedType(derivedType: SimpleTypeDefBase) = new GlobalSimpleTypeDef(xml, schemaDocument, None)
}
/**
 * The instance type for global simple type definitions.
 */
class GlobalSimpleTypeDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, val element: Option[AnnotatedMixin])
  extends SimpleTypeDefBase(xmlArg, schemaDocumentArg) with NamedMixin 
  with GlobalComponentMixin {

  def schemaDocument = schemaDocumentArg
  def emptyFormatFactory = new DFDLSimpleType(newDFDLAnnotationXML("simpleType"), this)

  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

}

abstract class ComplexTypeBase(xmlArg: Node, val parent: SchemaComponent)
  extends TypeBase(xmlArg, parent)
  with ComplexTypeBaseGrammarMixin {
  
  lazy val <complexType>{ xmlChildren @ _* }</complexType> = xml
  lazy val Seq(modelGroup) = xmlChildren.flatMap { GroupFactory(_, this, 1) }
  lazy val localAndFormatRefProperties: Map[String, String] = {
    Map.empty[String, String]
  }
  
  lazy val diagnosticChildren = List(modelGroup)
}

class GlobalComplexTypeDefFactory(val xml: Node, schemaDocument: SchemaDocument)
extends NamedMixin
{
  def forElement(element: ElementBase) = new GlobalComplexTypeDef(xml, schemaDocument, element)
}

class GlobalComplexTypeDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, val element: ElementBase)
  extends ComplexTypeBase(xmlArg, schemaDocumentArg)
  with GlobalComponentMixin {
  def schemaDocument = schemaDocumentArg
 
}

class LocalComplexTypeDef(xmlArg: Node, parent: ElementBase)
  extends ComplexTypeBase(xmlArg, parent)
  with LocalComponentMixin {
  
  lazy val prettyName = "complexType in " + parent.name
}
